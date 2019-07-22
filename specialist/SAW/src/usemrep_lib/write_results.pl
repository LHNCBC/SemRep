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

% File:	    write_results.pl
% Module:   write_results
% Author:   tcr
% Purpose:  write results of linguistic analysis in SAW


% ----- Module declaration and exported predicates

:- module(write_results, [
	entrezgene_entity_not_used/4,
	format_one_entity/9,
	format_one_predication/9,
	mca_output_with_label/3,
	phrasex_output/3,
	% NOT USED
%	timing/7,
%	write_all_predications/10,
	write_sorted/11
%	write_special/3
   ]).

:- load_files( usemrep_lib(module_version),
	       [ when(compile_time) ] ).

% ----- Imported predicates

:- use_module( usemrep_lib(portray_minimal_syntax), [
		portray_minimal_syntax_structure/1
   ]).

:- use_module( usemrep_lib(ssuppserv), [
		add_indexes_to_all_MSUs/3,
		add_msu_index/3,
		atomize/2,
		assemble_format_control_string/2,
		convert_dash_structure_to_list/2,
%	        convert_genphenom_function_list/2,
		get_all_concept_cuis/2,
%		get_augmented_concept_cui/2,
		get_entity_atom/5,
		get_index_start_and_end_pos/4,
		get_last_indexes/3,
%		get_print_semtype_list/2,
		get_sem_info_and_type/5,
		is_adjectival_indicator/1,
		is_nominal_indicator/1,
		locate_npu_head/3,
		lower_list/2,
		min_max/4,
		remove_variables/3,
%		set_genphenom_or_disorder_fields/8,
		split_input_label/4,
		update_entity_fields/15
   ]).


:- use_module( usemrep_lib(full_fielded_output), [
                get_genphenoms_from_tree/3,
		get_metaconcs_from_tree/4
   ]).

:- use_module( usemrep_lib(sem_xml), [
		generate_and_print_utterance_xml/9
   ]).

% :- use_module( usemrep_lib(wrappers), [
%                 format_one_entity_wrapper/11,
% 		format_one_predication_wrapper/11,
% 		syntax_tree_charpos_wrapper/14,	
% 		semantics_tree_charpos_wrapper/10
%    ]).

:- use_module( usemrep_main(usemrep), [ errorFlag/1 ]).

:- use_module( skr_lib(nls_strings), [
		split_string_completely/3,
		trim_whitespace_right/2
   ]).

:- use_module( skr_lib(nls_system), [
		control_option/1
   ]).

:- use_module( skr_lib(nls_lists), [
		get_from_list/3
   ]).


:- use_module( library(samsort), [
                samsort/2      
			      ]).

:- use_module( library(sets), [
		intersect/2,
		list_to_set/2
   ]).

:- use_module( skr_lib(sicstus_utils), [
		concat_atom/2,
		concat_atom/3,
%		index/3,
		midstring/5,
		substring/4,
		upper/2
   ]).

:- use_module(metamap(metamap_tokenization), [
	get_phrase_item_feature/3,
	get_phrase_item_name/2
   ]).

:- use_module( library(lists), [
                append/2,
		keys_and_values/3,
		is_list/1,
		last/2,
		rev/2,		
		reverse/2,
		select/3,
		selectchk/3
   ]).

:- use_module( skr_lib(ctypes), [
		is_ascii/1
   ]).

write_sorted(OutputStream, Position, minimal_syntax(SyntacticAnalysis), PrecedingAnalysis, minimal_syntax(SemanticAnalysis), 
	     CitationTextAtom, InputLabel, UNExpandedInputText,UtteranceType, PredicationLen, Predications) :-
	write_syntax(minimal_syntax(SyntacticAnalysis), OutputStream),
	append(PrecedingAnalysis,AllPrecedingAnalysis),
	add_msu_index(SemanticAnalysis, 0, SemanticAnalysisWithMSUIndex),
%	SemanticAnalysisWithMSUIndex = SemanticAnalysis0,
	append(AllPrecedingAnalysis,SemanticAnalysis,AllSemanticAnalysis),
	( ( control_option(full_fielded_output_format), 
	    FullFielded = 1,
	    XML = 0,
	    update_predications(Predications,Predications,SemanticAnalysisWithMSUIndex,
				AllSemanticAnalysis,UpdatedPredications)
	  ; control_option(xml_output_format),
	    FullFielded = 1,
	    XML = 1,
	    update_predications(Predications,Predications,SemanticAnalysisWithMSUIndex,
				AllSemanticAnalysis,UpdatedPredications)
	  ; FullFielded = 0,
	    XML = 0,
	    UpdatedPredications = Predications
          ),
	  get_genphenoms_from_tree(SemanticAnalysis, GenPhenomList, []),
	  get_metaconcs_from_tree(SemanticAnalysis, GenPhenomList, MetaConcList, []),
	  get_genphenoms_from_tree(AllSemanticAnalysis, AllGenPhenomList, []),
	  get_metaconcs_from_tree(AllSemanticAnalysis, AllGenPhenomList, AllMetaConcList, []),
%	  append(PrecedingAnalysis,AllPrecedingAnalysis),
%	  append(AllPrecedingAnalysis,SemanticAnalysis,AllSemanticAnalysis),
	  get_semantic_poslist(AllSemanticAnalysis,IndexCharPositionList),
%	    format(user_output,'PrecedingAnalysis    : ~q~n', [PrecedingAnalysis]),
%	    format(user_output,'AllSemanticAnalysis  : ~q~n', [AllSemanticAnalysis]),
%	    format(user_output,'IndexCharPositionList: ~q~n', [IndexCharPositionList]),
%	    format(user_output,'MetaConcList         : ~q~n', [MetaConcList]),
				%	    format(user_output,'GenPhenomList        : ~q~n', [GenPhenomList]),
	   format_all_predications(FullFielded, UpdatedPredications, CitationTextAtom, 
	                          UNExpandedInputText, OutputStream,
				  AllMetaConcList,AllGenPhenomList, IndexCharPositionList,
				  InputLabel, UtteranceType, FormattedPredications0),
%	    sort(FormattedPredications0, FormattedPredications),
	    samsort(FormattedPredications0, FormattedPredications),
          format_all_entities(FullFielded, CitationTextAtom, UNExpandedInputText, OutputStream,
	                      MetaConcList, MetaConcList, GenPhenomList,
	                      IndexCharPositionList, InputLabel, UtteranceType, FormattedEntities),
	  ( XML == 1 ->
	      % NOT GREAT TO DO FORMAT_ALL_ENTITIES TWICE, Ideally we should get FormattedEntities
	      % from AllFormattedEntities
	      format_all_entities(FullFielded, CitationTextAtom, UNExpandedInputText, OutputStream,
				  AllMetaConcList, AllMetaConcList, AllGenPhenomList,
				  IndexCharPositionList, InputLabel, UtteranceType, AllFormattedEntities),
	      generate_and_print_utterance_xml(OutputStream, Position, UtteranceType, InputLabel, UNExpandedInputText,
					       AllFormattedEntities, FormattedEntities,
					       PredicationLen, FormattedPredications)
	  ; 
            write_text(FullFielded, OutputStream, Position, InputLabel, UtteranceType, UNExpandedInputText),
	    write_all_entities(FullFielded, OutputStream, InputLabel, UtteranceType, FormattedEntities),
            write_all_predications(FullFielded, OutputStream, InputLabel, UtteranceType, 
				   FormattedPredications)
	 )
      ),
      flush_output(OutputStream).

get_semantic_poslist(SemanticAnalysis,IndexCharPosList) :-
	get_sentence_index(SemanticAnalysis,Index,999999),
	get_semantic_poslist_1(SemanticAnalysis,Index,IndexCharPosList0),
	append(IndexCharPosList0,IndexCharPosList),
	!.

get_semantic_poslist_1([],_Index,[]) :- !.
get_semantic_poslist_1([ThisMSU|RestMSUs],Index, 
	             [PosList|RestPosList]) :-
	get_semantic_poslist_aux(ThisMSU,Index,PosList),
	get_semantic_poslist_1(RestMSUs,Index,RestPosList).

get_semantic_poslist_aux([],_Index,[]) :- !.
get_semantic_poslist_aux([MSUItem|RestMSU],Index0,
	                  [Index-InputMatch-Start-End|RestIndexCharPosList]) :-
	functor(MSUItem,_Type,1),
	arg(1,MSUItem,ArgList),
	get_from_list(inputmatch,ArgList,InputMatch),
	get_from_list(index,ArgList,Index),
	memberchk(position(Start,End),ArgList),
%        ( Start >= Index0 ->
%	  Start0 is Start - Index0 +1,
%	  End0 is End - Index0 + 1
        % temporary fix for cases where positional information is wrong
%        ; Start0 is 1,
%          End0 is End - Start + 1
%        ),
	get_semantic_poslist_aux(RestMSU,Index0,RestIndexCharPosList).
get_semantic_poslist_aux([_MSUItem|RestMSU],Index,IndexCharPosList) :-
	get_semantic_poslist_aux(RestMSU,Index,IndexCharPosList).

get_sentence_index([],Index,Index) :- !.
get_sentence_index([MSU|_Rest],IndexOut,IndexIn) :-
	get_sentence_index_aux(MSU,IndexOut,IndexIn),
	!.
% To cope with weirdness caused by token lists. See Dongwook's mail dated 01/05/10.
%	get_sentence_index(Rest,IndexOut,IndexNext).

get_sentence_index_aux([],Index,Index) :- !.
get_sentence_index_aux([Item|Rest],IndexOut,IndexIn) :-
	functor(Item,_Type,1),
	arg(1,Item,ArgList),
	memberchk(position(Index,_End),ArgList),
	IndexIn >= Index,
	!,
	get_sentence_index_aux(Rest,IndexOut,Index).
get_sentence_index_aux([_MSU|Rest],IndexOut,IndexIn) :-
	get_sentence_index_aux(Rest,IndexOut,IndexIn).


format_all_predications(_FullFielded, [], _CitationTextAtom, _UNExpandedInputText,
	                _OutputStream, _MetaConcList, _GenPhenomList, _IndexCharPositionList,
			_InputLabel, _UtteranceType, []) :- !.
format_all_predications(FullFielded, [FirstPredication|RestPredications], CitationTextAtom,
	                UNExpandedInputText, OutputStream, MetaConcList, GenPhenomList, 
			IndexCharPositionList, InputLabel, UtteranceType,
			[FormattedFirstPredication|FormattedRestPredications]) :-
	format_one_predication(FullFielded, FirstPredication, CitationTextAtom,
			       UNExpandedInputText, MetaConcList, 
			       GenPhenomList, IndexCharPositionList, InputLabel,
			       UtteranceType, FormattedFirstPredication),
        !,				       
	format_all_predications(FullFielded, RestPredications, CitationTextAtom,  
	                        UNExpandedInputText, OutputStream, MetaConcList, GenPhenomList, 
				IndexCharPositionList, InputLabel, UtteranceType, 
				FormattedRestPredications).
format_all_predications(FullFielded, [_FirstPredication|RestPredications], CitationTextAtom,
	                UNExpandedInputText, OutputStream, MetaConcList, GenPhenomList, 
			IndexCharPositionList, InputLabel, UtteranceType, 
			FormattedRestPredications) :-
	format_all_predications(FullFielded, RestPredications, CitationTextAtom, 
	                        UNExpandedInputText, OutputStream, MetaConcList, 
				GenPhenomList, IndexCharPositionList, InputLabel, UtteranceType, 
				FormattedRestPredications).

format_one_predication(0, scale-ScaleName-_Index, _CitationTextAtom, _UNExpandedInputText, _MetaConcList,
	               _GenPhenomList, _IndexCharPositionList, InputLabel,
		       _UtteranceType,
		       '2entity'-(InputLabel-relation-scale-ScaleName)).
format_one_predication(1, scale-ScaleName-Index, CitationTextAtom, UNExpandedInputText, _MetaConcList, 
	               _GenPhenomList, IndexCharPositionList, InputLabel, UtteranceType,
		       '2entity'-('SE'-PMID-UtteranceType-TiOrAb-SentenceID-entity-
	               CUI-MetaConc-SemTypeListAtom-
	               NormGeneID-NormGeneName-InputText-
	               Change-Degree-Negation-
	               Confidence-StartPos-EndPos)) :-
	!,
	set_empty_semrep_value(_, SemTypeListAtom),
	set_empty_semrep_value(_, NormGeneID),
	set_empty_semrep_value(_, NormGeneName),
	get_all_concept_cuis(ScaleName, CUI),
	MetaConc = FakeMetaConc,
        % The next three fields are Carol-Friedman-specific
	set_empty_semrep_value(change, Change),
	set_empty_semrep_value(degree, Degree),
	set_empty_semrep_value(negation,    Negation),
	set_empty_semrep_value(confidence,  Confidence),
	split_input_label(InputLabel, PMID, TiOrAb, SentenceID),
	get_index_start_and_end_pos(Index, IndexCharPositionList, StartPos, EndPos),
	get_entity_atom(Index, IndexCharPositionList, 
	                CitationTextAtom, UNExpandedInputText, InputText),
	concat_atom(['<<', ScaleName, '>>'], FakeMetaConc).
format_one_predication(0,Anaphor-'COREF'-Antecedent,
	               CitationTextAtom, UNExpandedInputText, MetaConcList, GenPhenomList, 
		       IndexCharPositionList, InputLabel, _UtteranceType,
		       '3relation'-(InputLabel-coreference-
		       RealSubjectCUI-RealSubjectMetaConc-SubjectSemTypeListAtom-
                       SubjectNormGeneID-SubjectNormGeneName-
		       'COREF'-
		       RealObjectCUI-RealObjectMetaConc-ObjectSemTypeListAtom-
	               ObjectNormGeneID-ObjectNormGeneName)) :-
	locate_npu_head([Anaphor],_Type,AnaphorHead),
	get_sem_info_and_type(AnaphorHead, [SubjectMetaConc], SubjectCUI,
			      SubjectSemTypeList, _SubjectSemType),
	get_sem_info_and_type(Antecedent, [ObjectMetaConc], ObjectCUI,
			      ObjectSemTypeList, _ObjectSemType),
	get_from_list(index,Antecedent,ObjectIndex),
	memberchk((SubjectMetaConc:SubjectCUI:_SubjectSemTypeList)-SubjectIndex-_SubjectConfidenceScore-_SubjectNegation,
		   MetaConcList),
        memberchk((ObjectMetaConc:ObjectCUI:_ObjectSemTypeList)-ObjectIndex-_ObjectConfidenceScore-_ObjectNegation,
	           MetaConcList),
	update_entity_fields(IndexCharPositionList, GenPhenomList,
	                     CitationTextAtom, UNExpandedInputText,
	                     SubjectCUI, SubjectMetaConc, SubjectIndex, SubjectSemTypeList, 
			     RealSubjectCUI, RealSubjectMetaConc, 
			     SubjectNormGeneID, SubjectNormGeneName, _SubjectAtom,
			     _SubjectStartPos, _SubjectEndPos),
	update_entity_fields(IndexCharPositionList, GenPhenomList,
                             CitationTextAtom, UNExpandedInputText,
	                     ObjectCUI, ObjectMetaConc, ObjectIndex, ObjectSemTypeList,
			     RealObjectCUI, RealObjectMetaConc,
			     ObjectNormGeneID, ObjectNormGeneName, _ObjectAtom,
			     _ObjectStartPos, _ObjectEndPos),
	concat_atom(SubjectSemTypeList, ',', SubjectSemTypeListAtom),
	concat_atom(ObjectSemTypeList, ',',  ObjectSemTypeListAtom).
format_one_predication(1,Anaphor-'COREF'-Antecedent,CitationTextAtom,UNExpandedInputText,MetaConcList,
	               GenPhenomList,IndexCharPositionList, InputLabel,
		       UtteranceType, FormattedCoreference) :-
	FormattedCoreference = '3relation'-('SE'-PMID-UtteranceType-TiOrAb-SentenceID-coreference-
					    RealSubjectCUI-RealSubjectMetaConc-SubjectSemTypeListAtom-
	                                    SubjectNormGeneID-SubjectNormGeneName-SubjectAtom-
					    SubjectChange-SubjectDegree-SubjectNegation-
					    SubjectConfidenceScore-SubjectStartPos-SubjectEndPos-
					    'COREF'-
					    RealObjectCUI-RealObjectMetaConc-ObjectSemTypeListAtom-
	                                    ObjectNormGeneID-ObjectNormGeneName-ObjectAtom-
					    ObjectChange-ObjectDegree-ObjectNegation-
					    ObjectConfidenceScore-ObjectStartPos-ObjectEndPos),
	set_empty_semrep_value(change,   SubjectChange),
	set_empty_semrep_value(degree,   SubjectDegree),
	% set_empty_semrep_value(negation, SubjectNegation),
	set_empty_semrep_value(change,   ObjectChange),
	set_empty_semrep_value(degree,   ObjectDegree),
	% set_empty_semrep_value(negation, ObjectNegation),

	split_input_label(InputLabel, PMID, TiOrAb, SentenceID),

	locate_npu_head([Anaphor],_Type,AnaphorHead),
	
	Anaphor = [AnaphorFirst|_],
	arg(1,AnaphorFirst,AnaphorFirstList),
	memberchk(position(SubjectStartPos,_LastPos),AnaphorFirstList),
	last(Anaphor,AnaphorLast),
	arg(1,AnaphorLast,AnaphorLastList),
	memberchk(position(_FirstPos,SubjectEndPos),AnaphorLastList),
	SubjectLen is SubjectEndPos - SubjectStartPos,
%	atom_codes(CitationTextAtom1,UNExpandedInputText),
	substring(CitationTextAtom,SubjectAtom,SubjectStartPos,SubjectLen),
	
	get_sem_info_and_type(AnaphorHead, [SubjectMetaConc], SubjectCUI,
			      SubjectSemTypeList, _SubjectSemType),
	get_sem_info_and_type(Antecedent, [ObjectMetaConc], ObjectCUI,
			      ObjectSemTypeList, _ObjectSemType),
	get_from_list(index,Antecedent,ObjectIndex),
	concat_atom(SubjectSemTypeList, ',', SubjectSemTypeListAtom),
	concat_atom(ObjectSemTypeList, ',', ObjectSemTypeListAtom),

	memberchk((SubjectMetaConc:SubjectCUI:_SubjectSemTypeList)-SubjectIndex-SubjectConfidenceScore-SubjectNegation,
		   MetaConcList),
        memberchk((ObjectMetaConc:ObjectCUI:_ObjectSemTypeList)-ObjectIndex-ObjectConfidenceScore-ObjectNegation,
	           MetaConcList),
	update_entity_fields(IndexCharPositionList, GenPhenomList,
	                     CitationTextAtom, UNExpandedInputText,
	                     SubjectCUI, SubjectMetaConc, SubjectIndex, SubjectSemTypeList, 
			     RealSubjectCUI, RealSubjectMetaConc, 
			     SubjectNormGeneID, SubjectNormGeneName, _SubjectAtom,
			     _StartPos, _EndPos),
	update_entity_fields(IndexCharPositionList, GenPhenomList,
                             CitationTextAtom, UNExpandedInputText,
	                     ObjectCUI, ObjectMetaConc, ObjectIndex, ObjectSemTypeList,
			     RealObjectCUI, RealObjectMetaConc,
			     ObjectNormGeneID, ObjectNormGeneName, ObjectAtom,
			     ObjectStartPos, ObjectEndPos).
format_one_predication(0,
 	               _SubjectMaxDist-_SubjectDist-SubjectIndex-
	               SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
	               _IndicatorType-Relation-_IndicatorIndex-
	               _ObjectMaxDist-_ObjectDist-ObjectIndex-
	               ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType, 
	               CitationTextAtom, UNExpandedInputText, _MetaConcList, GenPhenomList, 
		       IndexCharPositionList, InputLabel, _UtteranceType,
		       '3relation'-(InputLabel-relation-
		       RealSubjectCUI-RealSubjectMetaConc-SubjectSemTypeListAtom-SubjectSemType-
                       SubjectNormGeneID-SubjectNormGeneName-
		       Relation-
		       RealObjectCUI-RealObjectMetaConc-ObjectSemTypeListAtom-ObjectSemType-
	               ObjectNormGeneID-ObjectNormGeneName)) :-
	update_entity_fields(IndexCharPositionList, GenPhenomList,
	                     CitationTextAtom, UNExpandedInputText,
	                     SubjectCUI, SubjectMetaConc, SubjectIndex, SubjectSemTypeList, 
			     RealSubjectCUI, RealSubjectMetaConc, 
			     SubjectNormGeneID, SubjectNormGeneName, _SubjectAtom,
			     _SubjectStartPos, _SubjectEndPos),
	update_entity_fields(IndexCharPositionList, GenPhenomList,
                             CitationTextAtom, UNExpandedInputText,
	                     ObjectCUI, ObjectMetaConc, ObjectIndex, ObjectSemTypeList,
			     RealObjectCUI, RealObjectMetaConc,
			     ObjectNormGeneID, ObjectNormGeneName, _ObjectAtom,
			     _ObjectStartPos, _ObjectEndPos),
	concat_atom(SubjectSemTypeList, ',', SubjectSemTypeListAtom),
	concat_atom(ObjectSemTypeList, ',',  ObjectSemTypeListAtom).
format_one_predication(1,
 	               SubjectMaxDist-SubjectDist-SubjectIndex-
	               SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
	               IndicatorType-Relation-IndicatorIndex-
	               ObjectMaxDist-ObjectDist-ObjectIndex-
	               ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType, 
	               CitationTextAtom, UNExpandedInputText, MetaConcList,
	               GenPhenomList, IndexCharPositionList, InputLabel, UtteranceType,
		       FormattedPredication) :-
	!,
	FormattedPredication = '3relation'-('SE'-PMID-UtteranceType-TiOrAb-SentenceID-relation-
					    SubjectMaxDist-SubjectDist-
					    RealSubjectCUI-RealSubjectMetaConc-
					    SubjectSemTypeListAtom-SubjectSemType-
	                                    SubjectNormGeneID-SubjectNormGeneName-SubjectAtom-
					    SubjectChange-SubjectDegree-SubjectNegation-
					    SubjectConfidenceScore-SubjectStartPos-SubjectEndPos-
					    IndicatorType-NewRelation-RelationNegation-
					    RelationStartPos-RelationEndPos-
					    ObjectMaxDist-ObjectDist-
					    RealObjectCUI-RealObjectMetaConc-
					    ObjectSemTypeListAtom-ObjectSemType-
	                                    ObjectNormGeneID-ObjectNormGeneName-ObjectAtom-
					    ObjectChange-ObjectDegree-ObjectNegation-
					    ObjectConfidenceScore-ObjectStartPos-ObjectEndPos),

 	set_empty_semrep_value(change,   SubjectChange),
	set_empty_semrep_value(degree,   SubjectDegree),
% 	set_empty_semrep_value(negation, SubjectNegation),
	set_empty_semrep_value(change,   ObjectChange),
	set_empty_semrep_value(degree,   ObjectDegree),
% 	set_empty_semrep_value(negation, ObjectNegation),

	split_input_label(InputLabel, PMID, TiOrAb, SentenceID),
	concat_atom(SubjectSemTypeList, ',', SubjectSemTypeListAtom),
	concat_atom(ObjectSemTypeList,  ',', ObjectSemTypeListAtom),

	update_entity_fields(IndexCharPositionList, GenPhenomList,
	                     CitationTextAtom, UNExpandedInputText,
	                     SubjectCUI, SubjectMetaConc, SubjectIndex, SubjectSemTypeList, 
			     RealSubjectCUI, RealSubjectMetaConc, 
			     SubjectNormGeneID, SubjectNormGeneName, SubjectAtom,
			     SubjectStartPos, SubjectEndPos),
	update_entity_fields(IndexCharPositionList, GenPhenomList,
                             CitationTextAtom, UNExpandedInputText,
	                     ObjectCUI, ObjectMetaConc, ObjectIndex, ObjectSemTypeList,
			     RealObjectCUI, RealObjectMetaConc,
			     ObjectNormGeneID, ObjectNormGeneName, ObjectAtom,
			     ObjectStartPos, ObjectEndPos),

	get_index_start_and_end_pos(IndicatorIndex, IndexCharPositionList,
				    RelationStartPos, RelationEndPos),

	memberchk((SubjectMetaConc:SubjectCUI:_SubjectSemTypeList)-SubjectIndex-SubjectConfidenceScore-SubjectNegation,
		   MetaConcList),
        memberchk((ObjectMetaConc:ObjectCUI:_ObjectSemTypeList)-ObjectIndex-ObjectConfidenceScore-ObjectNegation,
	           MetaConcList),
	negate_relation_if_necessary(Relation, NewRelation, RelationNegation).

format_all_entities(0, _CitationTextAtom, _UNExpandedInputText, _OutputStream,
	            _MetaConcList, _MetaConcList, _GenPhenomList,
 	            _IndexCharPositionList,  _InputLabel, _UtteranceType, []) :- !.
format_all_entities(1, _CitationTextAtom, _UNExpandedInputText, _OutputStream, 
	            [], _MetaConcList, _GenPhenomList,
	            _IndexCharPositionList, _InputLabel, _UtteranceType, []) :- !.
format_all_entities(1, CitationTextAtom, UNExpandedInputText, OutputStream,
	            [FirstMetaConc|RestMetaConcs], MetaConcList, GenPhenomList,
	            IndexCharPositionList, InputLabel, UtteranceType,
	            [FirstConceptStructure|RestConceptStructure]) :-
	format_one_entity(1, CitationTextAtom, UNExpandedInputText, 
			  FirstMetaConc, MetaConcList, GenPhenomList,
			  IndexCharPositionList, InputLabel, UtteranceType,
			  FirstConceptStructure),
        !,				  
        format_all_entities(1, CitationTextAtom, UNExpandedInputText, OutputStream,
	                    RestMetaConcs, MetaConcList, GenPhenomList,
			    IndexCharPositionList, InputLabel, UtteranceType,
			    RestConceptStructure).
format_all_entities(FullFielded, CitationTextAtom, UNExpandedInputText, OutputStream,
	            [_FirstMetaConc|RestMetaConcs], MetaConcList, GenPhenomList,
		    IndexCharPositionList, InputLabel, UtteranceType, 
		    RestConceptStructure) :-
	format_all_entities(FullFielded, CitationTextAtom, UNExpandedInputText, OutputStream,
	                    RestMetaConcs, MetaConcList, GenPhenomList,
		            IndexCharPositionList, InputLabel, UtteranceType, 
		            RestConceptStructure).
		    

format_one_entity(1, CitationTextAtom, UNExpandedInputText, 
	          (MetaConc:CUI:SemTypeList)-Index-Confidence-Negation,
		  MetaConcList, GenPhenomList, IndexCharPositionList,
		  InputLabel, UtteranceType, 
		  '2entity'-('SE'-PMID-UtteranceType-TiOrAb-SentenceID-entity-
 	          RealCUI-RealMetaConc-SemTypeListAtom-
	          NormGeneID-NormGeneName-InputText-
	          Change-Degree-Negation-
	          Confidence-StartPos-EndPos)) :-
       % The next three fields are Carol-Friedman-specific
	set_empty_semrep_value(change, Change),
	set_empty_semrep_value(degree, Degree),
	% set_empty_semrep_value(negation,    Negation),
	concat_atom(SemTypeList, ',', SemTypeListAtom),
	split_input_label(InputLabel, PMID, TiOrAb, SentenceID),
	update_entity_fields(IndexCharPositionList, GenPhenomList,
	                     CitationTextAtom, UNExpandedInputText, 
			     CUI, MetaConc, Index, SemTypeList, 
			     RealCUI, RealMetaConc, 
			     NormGeneID, NormGeneName, InputText,
			     StartPos, EndPos),	
        % ensure that the concept created from Entrezgene is not repeated 
        % if it is already used in addition to a MetaConc.
        entrezgene_entity_not_used(MetaConcList, Index, CUI, NormGeneID).

entrezgene_entity_not_used(MetaConcList, Index, CUI, NormGeneID) :-
	( midstring(CUI,'C',_Front,0,1)
        ; NormGeneID == '' % ctrial concept
        ; count_metaconcs_in_index(MetaConcList, Index, MetaConcLen, 0),
	  MetaConcLen == 1
        ).

% count the metaconcs in the MSU item with the given index.
count_metaconcs_in_index([], _Index, Len, Len) :- !.
count_metaconcs_in_index([(_MetaConc:_CUI:SemTypeList)-Index-_Confidence-_Negation|RestMetaConc], Index,
	                LenOut, LenIn) :-
	intersect([aapp,gngm], SemTypeList),
	!,
	LenNext is LenIn + 1,
	count_metaconcs_in_index(RestMetaConc, Index, LenOut,LenNext).
count_metaconcs_in_index([_|RestMetaConc],Index, LenOut, LenIn) :-
	count_metaconcs_in_index(RestMetaConc, Index, LenOut, LenIn).

set_empty_semrep_value(_, Field) :-
	( var(Field) ->
	  Field = ''
	; true
	).


write_text(0, _OutputStream, _Position, _InputLabel, _UtteranceType, _UNExpandedInputText) :- !.
write_text(1, OutputStream, Position, InputLabel, UtteranceType, UNExpandedInputText) :-
	trim_whitespace_right(UNExpandedInputText,UNExpandedInputTextTrim),
	atom_codes(InputTextAtom, UNExpandedInputTextTrim),
	Position = StartIndex/Length,
	EndIndex is StartIndex + Length,
	split_input_label(InputLabel, PMID, TiOrAb, SentenceID),
	!,
	write_dash_structure('SE'-PMID-UtteranceType-TiOrAb-SentenceID-text-StartIndex-EndIndex-InputTextAtom,
	                      OutputStream).

write_all_entities(FullFielded, OutputStream, InputLabel, UtteranceType, Entities) :-
	keys_and_values(Entities, _Keys, NewEntities),	
	!,
	write_all_entities_aux(FullFielded, OutputStream, InputLabel, UtteranceType, NewEntities).

write_all_entities_aux(0, _OutputStream, _InputLabel, _UtteranceType, _Entities) :- !.
write_all_entities_aux(1, _OutputStream, _InputLabel, _UtteranceType, []) :- !.
write_all_entities_aux(1, OutputStream, _InputLabel, _UtteranceType, [Entity|RestEntities]) :-
	write_dash_structure(Entity, OutputStream),
	!,
	write_all_entities_aux(1, OutputStream, _InputLabel, _UtteranceType, RestEntities).

write_all_predications(FullFielded, OutputStream, _InputLabel, _UtteranceType,
	               Predications) :-
	keys_and_values(Predications, _Keys, NewPredications0),
	!,
%	list_to_set(NewPredications0,NewPredications),
	write_all_predications_aux(FullFielded, OutputStream, _InputLabel, _UtteranceType, 
	                       NewPredications0),
        format(OutputStream, '~n',[]).

write_all_predications_aux(_FullFielded, _OutputStream, _InputLabel, _UtteranceType, []) :- !.
write_all_predications_aux(FullFielded, OutputStream, InputLabel, UtteranceType, 
	               [Predication|RestPredications]) :-
	write_dash_structure(Predication, OutputStream),
	!,
	write_all_predications_aux(FullFielded, OutputStream, InputLabel, UtteranceType, RestPredications).


write_dash_structure(Structure, OutputStream) :-
	convert_dash_structure_to_list(Structure, List),
	remove_variables(List, ListWithNoVariables, Length),
	assemble_format_control_string(Length, FormatControlString),
	format(OutputStream, FormatControlString, ListWithNoVariables).


get_argument_index_from_coreference([Anaphor-'COREF'-Antecedent|_Rest],SubjectIndex, ObjectIndex,
				    SubjectIndexOut, ObjectIndexOut) :-
	get_from_list(index,Antecedent,AntecedentIndex),
	locate_npu_head([Anaphor],_Type,AnaphorHead),
	get_from_list(index,AnaphorHead,AnaphorIndex),
	( AntecedentIndex == SubjectIndex ->
	  SubjectIndexOut = AnaphorIndex,
	  ObjectIndexOut = ObjectIndex
	; AntecedentIndex == ObjectIndex,
	  ObjectIndexOut = AnaphorIndex,
	  SubjectIndexOut = SubjectIndex
	),
	!.
get_argument_index_from_coreference([_Predication|Rest],SubjectIndex, ObjectIndex,
				    SubjectIndexOut, ObjectIndexOut) :-
	get_argument_index_from_coreference(Rest,SubjectIndex, ObjectIndex,
					    SubjectIndexOut,ObjectIndexOut).
	
update_predications([],_Predications,_SemanticAnalysis,_FullSemanticAnalysis,[]) :- !.
update_predications([scale-ScaleName-Index|Rest],Predications,SemanticAnalysis,
		    FullSemanticAnalysis, [scale-ScaleName-Index|RestUpdated]) :-
	update_predications(Rest, Predications, SemanticAnalysis, FullSemanticAnalysis, RestUpdated).
update_predications([Anaphor-'COREF'-Antecedent|Rest], Predications, SemanticAnalysis,
		    FullSemanticAnalysis, [Anaphor-'COREF'-Antecedent|RestUpdated]) :-
	update_predications(Rest, Predications, SemanticAnalysis, FullSemanticAnalysis, RestUpdated).
update_predications([Predication|Rest], Predications, SemanticAnalysis,
		    FullSemanticAnalysis,[PredicationUpdated|RestUpdated]) :-
	Predication = _SubjectMaxDist-_SubjectDist-SubjectIndex-
	              SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
	              IndicatorType-Relation0-IndicatorIndex-
	              _ObjectMaxDist-_ObjectDist-ObjectIndex-
	              ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
        % This means distance fields need to be instantiated		      
        ( var(IndicatorType)  ->
          get_indicator(SemanticAnalysis, IndicatorIndex, _Indicator, IndType),
	  ( compute_distances(SemanticAnalysis, IndicatorIndex,
	                    SubjectIndex, ObjectIndex, 
	                    SubjMaxDist, SubjDist, ObjMaxDist, ObjDist),
	    Relation = Relation0
	  ; get_argument_index_from_coreference(Predications, SubjectIndex, ObjectIndex,
					  SubjectIndex0, ObjectIndex0),
	    compute_distances(SemanticAnalysis, IndicatorIndex,
			      SubjectIndex0, ObjectIndex0,
			      SubjMaxDist, SubjDist, ObjMaxDist,ObjDist),
	    concat_atom([Relation0,'(COREF)'],Relation)
	  ),
	  PredicationUpdated = SubjMaxDist-SubjDist-SubjectIndex-
	                       SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
	                       IndType-Relation-IndicatorIndex-
	                       ObjMaxDist-ObjDist-ObjectIndex-
	                       ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
          update_coordinated_predications(Rest, PredicationUpdated, RestUpdated0),	
	  update_predications(RestUpdated0, Predications, SemanticAnalysis, FullSemanticAnalysis,
			      RestUpdated)
	; PredicationUpdated = Predication,
	  update_predications(Rest, Predications, SemanticAnalysis, FullSemanticAnalysis, RestUpdated)
        ).
update_predications([_Predication|Rest], Predications, SemanticAnalysis,
		    FullSemanticAnalysis,RestUpdated) :-
	  update_predications(Rest, Predications, SemanticAnalysis,
			      FullSemanticAnalysis, RestUpdated).


% ---------- indicator_type ---------------------
get_indicator(_SemanticAnalysis,_FirstIndex:_LastIndex,_,'COMPLEX').
get_indicator([ThisMSU|_Rest], Index, MSUElement, Type) :-
	get_indicator_1(ThisMSU, Index, MSUElement, Type),
	!.
get_indicator([_ThisMSU|Rest], Index, MSUElement, Type) :-
	get_indicator(Rest, Index, MSUElement, Type).
        

get_indicator_1([MSUElement|_Rest], Index, MSUElement, Type) :-
	functor(MSUElement, _ElementType, 1),
	arg(1,MSUElement, ItemList),	
	memberchk(index(Index), ItemList),
	!,
	indicator_type(MSUElement, Type).
get_indicator_1([_MSUElement|Rest], Index, MSUElement, Type) :-
	get_indicator_1(Rest, Index, MSUElement, Type).
	
indicator_type(verb(_),     'VERB').
indicator_type(prep(_),     'PREP').
indicator_type(aux(_),      'AUX').
indicator_type(pastpart(_), 'PART').
% vs is a conj
indicator_type(conj(_),     'CONJ').
indicator_type(Indicator,   'NOM') :-
	is_nominal_indicator(Indicator).
indicator_type(Indicator,   'ADJ') :-
	is_adjectival_indicator(Indicator).
% This should not happen but instead of failing, returning empty string seems better.
indicator_type(_Indicator,   '').


% ---------- compute_distances ------------------
% moved over from semgeninterp.pl and modified
% can be simplified possibly
compute_distances(SemanticAnalysis, IndicatorIndex, 
	          SubjectIndex, ObjectIndex, 
		  SubjectMaxDist, SubjectDist, ObjectMaxDist, ObjectDist) :-
	rev(SemanticAnalysis, RevAnalysis),
	get_msu_index(RevAnalysis, IndicatorIndex, IndicatorPos, Indicator),
	get_msu_index(RevAnalysis, SubjectIndex, SubjectPos, _Subject),
	get_msu_index(RevAnalysis, ObjectIndex, ObjectPos, _Object),
	compute_distances_1(Indicator, 
	                  (IndicatorPos, SemanticAnalysis, RevAnalysis, SubjectPos, ObjectPos),
			  (SubjectMaxDist, SubjectDist, ObjectMaxDist, ObjectDist)).
	
% for multi-phrase indicators
compute_distances_1(IndicatorList, (E0:E1,F,G,H,I), (A,B,C,D)) :-
	is_list(IndicatorList),
	!,
	IndicatorList=[First|_],
	last(IndicatorList,Last),
	compute_distances_1(First,(E0,F,G,H,I),(A0,B0,C0,D0)),
	compute_distances_1(Last,(E1,F,G,H,I),(A1,B1,C1,D1)),
	A is min(A0,A1),
	B is min(B0,B1),
	C is min(C0,C1),
	D is min(D0,D1).

compute_distances_1(verb(_), InputArgs, OutputArgs) :-
	!,
	compute_distances_2(InputArgs, OutputArgs).
compute_distances_1(aux(_), InputArgs, OutputArgs) :-
	!,
	compute_distances_2(InputArgs, OutputArgs).
compute_distances_1(pastpart(_), InputArgs, OutputArgs) :-
	!,
	compute_distances_2(InputArgs, OutputArgs).
compute_distances_1(head(_), InputArgs, OutputArgs) :-
	!,
	compute_distances_2(InputArgs, OutputArgs).
compute_distances_1(mod(_), InputArgs, OutputArgs) :-
	!,
	compute_distances_2(InputArgs, OutputArgs).
compute_distances_1(prep(_), InputArgs, OutputArgs) :-
	!,
	compute_distances_3(InputArgs, OutputArgs).
compute_distances_1(conj(_), InputArgs, OutputArgs) :-
	!,
	compute_distances_2(InputArgs, OutputArgs).
compute_distances_1(_,      _InputArgs, (0,0,0,0) ).

compute_distances_2( (IndicatorPos, Analysis, RevAnalysis, FirstPos, SecondPos),
		     (SubjectMaxDist, SubjectDist, ObjectMaxDist, ObjectDist ) ) :-
	% FirstPos and SecondPos are the msu_index values of the MSU containing
	% SubjectHeadList and ObjectHeadList.
	% The msu_index(_) terms are added (by domain_add_msu_index/3 in usemrep.pl)
	% iff the domain is genetics
	( FirstPos > IndicatorPos ->
	  get_intervening_npu_counts(RevAnalysis, FirstPos, IndicatorPos,
				     0, SubjectMaxDist, 0, SubjectDist1),
	  SubjectDist is SubjectMaxDist - SubjectDist1 + 1
	; get_intervening_npu_counts(Analysis, FirstPos, IndicatorPos,
				     0, SubjectMaxDist, 0, SubjectDist)
	),
	( SecondPos > IndicatorPos ->
	  get_intervening_npu_counts(RevAnalysis, SecondPos, IndicatorPos,
				     0, ObjectMaxDist, 0, ObjectDist1),
	  ObjectDist is ObjectMaxDist - ObjectDist1 + 1
	; get_intervening_npu_counts(Analysis, SecondPos, IndicatorPos,
				     0, ObjectMaxDist, 0, ObjectDist)
	).

compute_distances_3( (IndicatorPos, Analysis, RevAnalysis, FirstPos, SecondPos),
		     (SubjectMaxDist, SubjectDist, ObjectMaxDist, ObjectDist) ) :-
	  % Argument is in the same MSU as the indicator
	( FirstPos =:= IndicatorPos ->
	  SubjectDist is 1,
	  get_intervening_npu_counts(RevAnalysis, FirstPos, IndicatorPos,
				     0, SubjectMaxDist1, 0, _),
	  SubjectMaxDist is SubjectMaxDist1 + 1
	  % Argument's MSU follows the indicator's MSU
        ; IndicatorPos > FirstPos ->
	  get_intervening_npu_counts(Analysis, FirstPos, IndicatorPos, 
	                             0, SubjectMaxDist, 0, SubjectDist)
%	; FirstPos > IndicatorPos ->
	;  get_intervening_npu_counts(RevAnalysis, FirstPos, IndicatorPos,
				     0, SubjectMaxDist1, 0, SubjectDist1),
	  SubjectMaxDist is SubjectMaxDist1 + 1,
	  SubjectDist is SubjectMaxDist - SubjectDist1 + 1
	  % Argument's MSU precedes the indicator's MSU
%	; get_intervening_npu_counts(Analysis, FirstPos, IndicatorPos,
%				     0, SubjectMaxDist, 0, SubjectDist)
        ),
	( SecondPos =:= IndicatorPos ->
	  ObjectDist is 1,
	  get_intervening_npu_counts(RevAnalysis, SecondPos, IndicatorPos,
				     0, ObjectMaxDist1, 0, _),
	  ObjectMaxDist is ObjectMaxDist1 + 1
        ; IndicatorPos > SecondPos ->
	  get_intervening_npu_counts(Analysis, SecondPos, IndicatorPos,
	                             0, ObjectMaxDist, 0, ObjectDist)
%	; SecondPos > IndicatorPos ->
	;  get_intervening_npu_counts(RevAnalysis, SecondPos, IndicatorPos,
				     0, ObjectMaxDist1, 0, ObjectDist1),
	  ObjectMaxDist is ObjectMaxDist1 + 1,
	  ObjectDist is ObjectMaxDist - ObjectDist1 + 1
%	; get_intervening_npu_counts(Analysis, SecondPos, IndicatorPos,
%				     0, ObjectMaxDist, 0, ObjectDist)
        ).

% ---------- Get Argument Position ---------------
% Get the msu_index of ThisMSU
% We should consider including the msu_index(_) term in the headlist returned by get_arguments
get_msu_index(MSUs,FirstIndex:LastIndex, FirstMSUIndex:LastMSUIndex, Items) :-
	get_msu_index(MSUs, FirstIndex, FirstMSUIndex, FirstItem),
	get_msu_index(MSUs, LastIndex, LastMSUIndex, LastItem),
	append([FirstItem],[LastItem],Items).
get_msu_index([ThisMSU|_], ItemIndex, MSUIndex, Item) :-
	match_msu_element(ThisMSU, ItemIndex, Item), 
	!,
	get_from_list(msu_index, ThisMSU, MSUIndex).
get_msu_index([_|MoreMSU], Index, MSUIndex, Item) :-
	get_msu_index(MoreMSU, Index, MSUIndex, Item).

match_msu_element([MSUElement|_RestMSU], Index, MSUElement) :-
	functor(MSUElement, _ElementType, 1),
	arg(1,MSUElement, ItemList),	
	memberchk(index(Index), ItemList).
match_msu_element([_|RestMSU], Index, MSUElement) :-
	match_msu_element(RestMSU, Index, MSUElement).

% ---------- Get Intervening NPU counts ----------
% compute the # of NPUs
% (1) on this side of the argument ThisMSU (TargetMaxDistOut), and
% (2) between the given argument ThisMSU and indicator (TargetDistOut)

% ask Halil: isn't TargetMaxDist the # of NPUs on this side of INDICATOR, and not ARGUMENT?

% If we get to the indicator position, then just return current distance counts.
get_intervening_npu_counts([ThisMSU|RestMSUx], ArgPos, IndicatorPos,
                           TargetMaxDistIn, TargetMaxDistOut,
                           TargetDistIn, TargetDistOut) :-
	get_from_list(msu_index, ThisMSU, MSUIndex),
	get_intervening_npu_counts_1([ThisMSU|RestMSUx], ArgPos, IndicatorPos, MSUIndex,
                           TargetMaxDistIn, TargetMaxDistOut,
                           TargetDistIn, TargetDistOut).

get_intervening_npu_counts_1([ThisMSU|RestMSUs], ArgPos, IndicatorPos, MSUIndex,
                           TargetMaxDistIn, TargetMaxDistOut,
                           TargetDistIn, TargetDistOut) :-
          % If we are at the indicator position, then just return current distance counts.
	( MSUIndex =:= IndicatorPos ->
	  TargetMaxDistIn = TargetMaxDistOut,
	  TargetDistIn = TargetDistOut
	  % Otherwise increment counts if we're at an MSU
	; increment_distances(ThisMSU, ArgPos, MSUIndex,
			    TargetMaxDistIn, TargetMaxDistNext,
			    TargetDistIn, TargetDistNext),
	  get_intervening_npu_counts(RestMSUs, ArgPos, IndicatorPos,
				       TargetMaxDistNext, TargetMaxDistOut,
				       TargetDistNext, TargetDistOut)
	).

increment_distances(ThisMSU, ArgPos, MSUIndex,
		    TargetMaxDistIn, TargetMaxDistNext,
		    TargetDistIn, TargetDistNext) :-
	% If ThisMSU is an NPU, then increment TargetMaxDist (# of NPUs before argument)
	( get_from_list(head, ThisMSU, _) ->
          TargetMaxDistNext is TargetMaxDistIn + 1,
	    % If we're reached the Argument's position,
	    % then start incrementing TargetDist (# of NPUs between argument and indicator)
	  ( MSUIndex >= ArgPos ->
            TargetDistNext is TargetDistIn + 1
	  ; TargetDistNext is TargetDistIn
          )
	  % If ThisMSU is NOT an NPU, then don't increase either count
        ; TargetMaxDistNext is TargetMaxDistIn,
	  TargetDistNext is TargetDistIn
	).

% Instantiate the distance fields of the  coordinated predications
% Assumes that coordinated predications will appear consecutively. Is this always right?
% Coordinated subjects
update_coordinated_predications([FirstPredication|Rest], Predication,
	                        [FirstPredicationUpdated|RestUpdated]) :-
	Predication = SubjMaxDist-SubjDist-_SubjectIndex-
	              _SubjectMetaConc-_SubjectCUI-_SubjectSemTypeList-_SubjectSemType-
	              IndType-Relation-IndicatorIndex-
	              ObjMaxDist-ObjDist-ObjectIndex-
	              ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
        FirstPredication = _SubjMaxDist1-_SubjDist1-SubjectIndex1-
	                   SubjectMetaConc1-SubjectCUI1-SubjectSemTypeList1-SubjectSemType1-
	                   _IndType1-Relation-IndicatorIndex-
	                   _ObjMaxDist1-_ObjDist1-ObjectIndex-
	                   ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
        FirstPredicationUpdated = SubjMaxDist-SubjDist-SubjectIndex1-
	                          SubjectMetaConc1-SubjectCUI1-SubjectSemTypeList1-SubjectSemType1-
	                          IndType-Relation-IndicatorIndex-
	                          ObjMaxDist-ObjDist-ObjectIndex-
	                          ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
	update_coordinated_predications(Rest, Predication, RestUpdated). 

%Coordinated objects
update_coordinated_predications([FirstPredication|Rest], Predication,
	                        [FirstPredicationUpdated|RestUpdated]) :-
	Predication = SubjMaxDist-SubjDist-SubjectIndex-
	              SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
	              IndType-Relation-IndicatorIndex-
	              ObjMaxDist-ObjDist-_ObjectIndex-
	              _ObjectMetaConc-__ObjectCUI-_ObjectSemTypeList-_ObjectSemType,
        FirstPredication = _SubjMaxDist1-_SubjDist1-SubjectIndex-
	                   SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
	                   _IndType1-Relation-IndicatorIndex-
	                   _ObjMaxDist1-_ObjDist1-ObjectIndex1-
	                   ObjectMetaConc1-ObjectCUI1-ObjectSemTypeList1-ObjectSemType1,
        FirstPredicationUpdated = SubjMaxDist-SubjDist-SubjectIndex-
	                          SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
	                          IndType-Relation-IndicatorIndex-
	                          ObjMaxDist-ObjDist-ObjectIndex1-
	                          ObjectMetaConc1-ObjectCUI1-ObjectSemTypeList1-ObjectSemType1,
	update_coordinated_predications(Rest, Predication, RestUpdated). 
% End of coordinated predications
update_coordinated_predications(Rest, _Predication, Rest):- !.

negate_relation_if_necessary(Relation, NewRelation, RelationNegation) :-
	atom_codes(Relation, RelationChars),
	%                   N  E  G  _
	( RelationChars = [78,69,71,95|NewRelationChars] ->
	  atom_codes(NewRelation, NewRelationChars),
	  RelationNegation = 'negation'
	; NewRelation = Relation,
	  set_empty_semrep_value(_, RelationNegation)
	).



% ***************************WRITE_ALL_PREDICATIONS***********************************

/*
Real program from tom. St are printed.
The delimiters have been changed to | instead of - to facilitate output reading in perl.
If we print the semtypes we must be aware that in the coordination algorithm
since in the past Tom was using semtypes instead of semgroups the sentence
"antibiotics and aspirin for pneumonia"
becomes instantiated to antibiotics-phsu and aspirin-phsu.
The build_predications predicate inside of deal_with_coordination
must be changed to refelect that.
For now we could  print the Semgroups as specified in write_all_predications below.
*/
 
% FML 02/10/2004 Tuesday @ 10:38:05
% Rewrote predicate to allow any number of a-b-c-d in list elements.
% Necessary because CorTex calls write_sorted/3 (which calls write_all_predications/3)
% with this list
% [_533-_535-'Right coronary artery'-'BRANCH_OF'-'Aorta']
% which has two problems:
% (1) format/2 must be called with atoms in the list argument,
% otherwise Prolog raises a consistency error, and
% (2) the number of elements in the list must equal
% the number of '~w's in the control atom/string

% This new version analyzes the a-b-c-d term and does four things:
% (1) convert to a-b-c-d term to a list,
% (2) remove any variables from the list,
% (3) count how many elements are left in the list, and
% (4) assemble a control string with that number of '~w's, concluding with a '~n'

% write_all_predications(Predications, Domain, FullFielded, 
% 	               GenPhenomList, IndexCharPositionList,
% 		       CitationTextAtom, UNExpandedInputText,
% 	               OutputStream, InputLabel, RelationLabel) :-
% 	re_format_all_predications(Predications, Domain, FullFielded, 
%  	                           GenPhenomList, IndexCharPositionList,
% 				   CitationTextAtom,UNExpandedInputText,
% 	                           ReFormattedPredications),
% 	write_all_predications_1(ReFormattedPredications, OutputStream, InputLabel, RelationLabel).
% 
% re_format_all_predications([], _Domain, _FullFielded, 
% 	                   _GenPhenomList, _IndexCharPositionList,
% 			   _CitationTextAtom, _UNExpandedInputText, []).
% re_format_all_predications([FirstPredication|RestPredications], Domain, FullFielded,
% 	                   GenPhenomList, IndexCharPositionList, CitationTextAtom, UNExpandedInputText,
% 			   [ReFormattedFirstPredication|ReFormattedRestPredications]) :-
% 	re_format_one_predication(FullFielded, Domain, 
% 	                          GenPhenomList, IndexCharPositionList,
% 				  CitationTextAtom, UNExpandedInputText,
% 	                          FirstPredication, ReFormattedFirstPredication),
% 	re_format_all_predications(RestPredications, Domain, FullFielded, 
% 	                           GenPhenomList, IndexCharPositionList,
% 				   CitationTextAtom, UNExpandedInputText,
% 	                           ReFormattedRestPredications).

% write_all_predications_1([], _OutputStream, _InputLabel, _RelationLabel).
% write_all_predications_1([FirstPredication|MorePredications],
% 		         OutputStream, InputLabel, RelationLabel) :-
% 	write_one_predication(FirstPredication, OutputStream, InputLabel, RelationLabel),
% 	write_all_predications_1(MorePredications, OutputStream, InputLabel, RelationLabel).

% The first two clauses handle useless genphenom predications
% write_one_predication('', _OutputStream, _InputLabel, _RelationLabel) :- !.
% write_one_predication([], _OutputStream, _InputLabel, _RelationLabel) :- !.
% write_one_predication(FirstPredication, OutputStream, InputLabel, RelationLabel) :-
% 	convert_dash_structure_to_list(FirstPredication, List),
% 	remove_variables(List, ListWithNoVariables, Length),
% 	assemble_format_control_string(Length, FormatControlString),
% 	% the leading "126,119,124,126,119,124" (which is "~w|~w|")
% 	% is for the InputLabel and "relation"
% 	format(OutputStream, [126,119,124,126,119,124|FormatControlString],
% 	       [InputLabel,RelationLabel|ListWithNoVariables]).
% 	% format(user_output, [126,119,124,126,119,124|FormatControlString],
% 	%        [InputLabel,RelationLabel|ListWithNoVariables]).
% 	
% % *************************** WRITE_SPECIAL ************************

% write_special(minimal_syntax(ListOfSyntUnits), Selector, Stream) :-
% 	write_special(ListOfSyntUnits, Selector, Stream).
% 
% write_special([],  _, Stream) :-
% 	format(Stream, '~n~n',[]).
% 
% write_special([ ThisSyntUnit | MoreSyntUnits ], semtype, Stream) :-
% 	get_from_list(verb, ThisSyntUnit, VerbList),
% 	is_list(VerbList),
% 	!,
% 	format(Stream, '[', []),
% 	get_from_list(lexmatch, VerbList, [ LexMatch ]),
% 	upper(LexMatch, UpperLexMatch),
% 	format(Stream, ' ~w ', [UpperLexMatch]),
% 	format(Stream, '] ',[]),
% 	write_special(MoreSyntUnits, semtype, Stream).
% 
% write_special([ ThisSyntUnit | MoreSyntUnits ],  Selector, Stream) :-
% 	write_bracket_if_semtype(Selector, Stream, '['),
% 	write_head_or_mod(ThisSyntUnit, Selector, Stream),
% 	!,
% 	write_bracket_if_semtype(Selector, Stream, ']'),
% 	write_special(MoreSyntUnits, Selector, Stream).
% 
% write_special([ _ThisSyntUnit | MoreSyntUnits ], Selector, Stream) :-
% 	write_special(MoreSyntUnits, Selector, Stream).
% 
% write_bracket_if_semtype(Selector, Stream, Bracket) :-
% 	( Selector = semtype ->
% 	  format(Stream, Bracket, [])
% 	; true
% 	).

% ----- WRITE HEAD OR MOD

% write_head_or_mod([], _, _).
% write_head_or_mod([ThisItem|More], Selector, Stream) :-
% 	functor(ThisItem, Label, _),
% 	label_is_head_or_mod(Label),
% 	arg(1, ThisItem, HeadOrModList),
% 	write_selector(Selector, Stream, HeadOrModList),
% 	!,
% 	write_head_or_mod(More, Selector, Stream).
% write_head_or_mod([_ThisItem|More], Selector, Stream) :-
% 	write_head_or_mod(More, Selector, Stream).
% 
% write_selector(metaconc, Stream, HeadOrModList) :-
% 	write_metaconc_and_semtype(Stream, HeadOrModList).
% write_selector(semtype, Stream, HeadOrModList) :-
% 	write_semtype_pattern(Stream, HeadOrModList).


% label_is_head_or_mod(head).
% label_is_head_or_mod(mod).

% ----- WRITE METACONC AND SEMTYPE

% write_metaconc_and_semtype(Stream, HeadOrModList) :-
% 	get_from_list(metaconc, HeadOrModList, MetaConcList),
% 	is_list(MetaConcList),
% 	!,
% 	write_metaconc_list(MetaConcList, Stream).

% ----- WRITE METACONC LIST

% write_metaconc_list([], _Stream).
% write_metaconc_list([MetaConc:_CUI:USemtypeList|MoreMetaConcs], Stream) :-
% 	format(Stream, '     ~a', [MetaConc]),
% 	format(Stream, ' (',[]),
% 	write_semtype_list(USemtypeList, Stream),
% 	format(Stream, ')~n',[]),
% 	write_metaconc_list(MoreMetaConcs, Stream).

% ----- WRITE SEMTYPE PATTERN 

% write_semtype_pattern(Stream, HeadOrModList) :-
% 	get_from_list(ausemtype, HeadOrModList, AbbrevSemtypeList),
% 	is_list(AbbrevSemtypeList),
% 	format(Stream, '[', []),
% 	write_semtype_list(AbbrevSemtypeList, Stream),
% 	format(Stream, '] ', []).

% ----- WRITE SEMTYPE LIST 

% write_semtype_list([], _).
% write_semtype_list([Semtype1|RestSemtypes], Stream) :-
% 	choose_blank_space(RestSemtypes, Blank),
% 	format(Stream, '~a~a', [Semtype1, Blank]),
% 	write_semtype_list(RestSemtypes, Stream).

% choose_blank_space([],    '').
% choose_blank_space([_|_], ' ').

% ******************************* TIMING ***************************************

% timing( Time0, Time1, Time2, Time3, Time4, Time5, Time6 ) :-
%     LUTime  is ( Time1 - Time0 ),
%     GVTime  is ( Time2 - Time1 ),
%     CatTime is ( Time3 - Time2 ),
%     RefTime is ( Time4 - Time3 ),
%     RelTime is ( Time5 - Time4 ),
%     InfTime is ( Time6 - Time5 ),
%     TTime   is ( Time6 - Time0 ),
% 
%     format( '~2nTokenize & Lookup time: ~0d msecs ~n',        [ LUTime  ] ),
%     format( 'Generate variant info time: ~0d msecs ~n',       [ GVTime  ] ), 
%     format( 'Gategorial Analysis time: ~0d msecs ~n',         [ CatTime ] ), 
%     format( 'Referential analysis time: ~0d msecs ~n',        [ RefTime ] ),
%     format( 'Relational analysis time: ~0d msecs ~n',         [ RelTime ] ),
%     format( 'Inference analysis time: ~0d msecs ~n',          [ InfTime ] ), 
%     format( 'Total time: ~0d msecs ~3n',                      [ TTime   ] ).

% remove the index arguments unless running in full fielded output mode,
% in which case they've already been removed.
% Keep the distance arguments;
% they will be vars in SemRep and nonvars in SemGen,
% and removed, if they are vars, by remove_variables/3.
% re_format_one_predication(FullFielded, Domain, FirstPredication, RevisedFirstPredication)
% re_format_one_predication(1, _Domain, _GenPhenomList, _IndexCharPositionList,
% 			  _CitationTextAtom, _UNExpandedInputText,
%                           Predication, Predication) :- !.
% re_format_one_predication(0, Domain, 
% 	                  GenPhenomList, IndexCharPositionList,
% 			  CitationTextAtom, UNExpandedInputText,
%                           Predication, PredicationWithoutIndexArguments) :-
% 	( Predication = scale-Scale-_IndicatorIndex ->
% 	  PredicationWithoutIndexArguments = scale-Scale
% 	; re_format_one_predication_domain(Domain,
% 	                                   GenPhenomList, IndexCharPositionList,
% 					   CitationTextAtom, UNExpandedInputText, 
%                                            Predication, PredicationWithoutIndexArguments)
% 	).
% re_format_one_predication(0, Domain, _GenPhenomList, _IndexCharPositionList,
% 			  _CitationTextAtom, _UNExpandedInputText, 
%                           Predication, _) :- !,
% 	( format('~nERROR: re_format_one_predication(0, ~w, ~w, _) failed.  It should never fail!~n', [Domain, Predication])
% 	),
% 	assert(errorFlag(false)).
% 
% re_format_one_predication_domain(_Domain, GenPhenomList, IndexCharPositionList,
% 		       CitationTextAtom, UNExpandedInputText,
% 		       _SubjectMaxDist-_SubjectDist-SubjectIndex-
% 		       SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
% 	               _IndicatorType-Relation-_IndicatorIndex-
% 		       _ObjectMaxDist-_ObjectDist-ObjectIndex-
% 		       ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType,
% 		       RealSubjectCUI-RealSubjectMetaConc-SubjectSemTypeListAtom-SubjectSemType-
%                        SubjectNormGeneID-SubjectNormGeneName-
% 		       Relation-
% 		       RealObjectCUI-RealObjectMetaConc-ObjectSemTypeListAtom-ObjectSemType-
% 	               ObjectNormGeneID-ObjectNormGeneName) :-
% 
% 	update_entity_fields(IndexCharPositionList, GenPhenomList,
% 	                     CitationTextAtom, UNExpandedInputText,
% 	                     SubjectCUI, SubjectMetaConc, SubjectIndex, SubjectSemTypeList, 
% 			     RealSubjectCUI, RealSubjectMetaConc, 
% 			     SubjectNormGeneID, SubjectNormGeneName, _SubjectAtom,
% 			     _SubjectStartPos, _SubjectEndPos),
% 	update_entity_fields(IndexCharPositionList, GenPhenomList,
%                              CitationTextAtom, UNExpandedInputText,
% 	                     ObjectCUI, ObjectMetaConc, ObjectIndex, ObjectSemTypeList,
% 			     RealObjectCUI, RealObjectMetaConc,
% 			     ObjectNormGeneID, ObjectNormGeneName, _ObjectAtom,
% 			     _ObjectStartPos, _ObjectEndPos),
% 	concat_atom(SubjectSemTypeList, ',', SubjectSemTypeListAtom),
% 	concat_atom(ObjectSemTypeList, ',',  ObjectSemTypeListAtom).
% re_format_one_predication_domain(_Domain, _GenPhenomList, _IndexCharPositionList, 
% 	                         _UNExpandedInputText,
% 	                         _Predication, '').


% get_real_metaconc(MetaConc, RealMetaConc) :-
% 	( MetaConc = NormGeneID-NormGeneName-GeneName-
% 		     GenPhenomOrDisorderAtom-_GeneFunctionList-TempMetaConc ->
% 	  concat_atom(['______', TempMetaConc, '______'], DisplayMetaConc),
% 	  concat_atom([NormGeneID,NormGeneName,GeneName,GenPhenomOrDisorderAtom,DisplayMetaConc],
% 		      '^',
% 		      RealMetaConc)
%         ; RealMetaConc = MetaConc
%         ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% R and r control options 
% write_syntax(minimal_syntax(SyntListWithTokens), _Label, _InputString, OutputStream) :-
% 	( control_option(write_syntax)
% 	; control_option(write_syntax_only)
% 	),
% 	!,
% 	write_syntax_1(SyntListWithTokens, OutputStream).
% write_syntax(_, _, _, _).

write_syntax(minimal_syntax(SyntListWithTokens),OutputStream) :-
	( control_option(write_syntax)
	; control_option(write_syntax_only)
	),
	!,
	write_syntax_1(SyntListWithTokens, OutputStream).
write_syntax(_, _).

write_syntax_1([], OutputStream) :-
	!,
	format(OutputStream, '~n~n',[]).
write_syntax_1([ThisMSU|MoreMSU], OutputStream) :-
	get_simplified_msu(ThisMSU, SimpleMSU),
	format(OutputStream, '[', []),
	write_term_list(SimpleMSU, OutputStream),
	format(OutputStream, ']      ', []),
	write_syntax_1(MoreMSU, OutputStream).

get_simplified_msu([],[]).
get_simplified_msu([confid(_)|MoreTokens], MoreSimpleTokens) :-
	get_simplified_msu(MoreTokens, MoreSimpleTokens).		  
get_simplified_msu([Token|MoreTokens], [SimpleToken|MoreSimpleTokens]) :-
	arg(1,Token,ItemList),
	get_from_list(inputmatch, ItemList, InputList),
	functor(Token,TokenName,_),
	upper(TokenName, NewTokenName),
	functor(SimpleToken,NewTokenName,1),
	arg(1,SimpleToken,InputList),
	get_simplified_msu(MoreTokens, MoreSimpleTokens).

write_term_list([], _Stream).
write_term_list([ThisTerm|MoreTerms], Stream) :-
	write_term(Stream, ThisTerm, [quoted(false),portrayed(true)]),
	( MoreTerms = [] ->
	  true
	; format(Stream, ',', [])
	),
	write_term_list(MoreTerms, Stream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mca_output_with_label(Label,Syntax,OutputStream) :-
	format(OutputStream, '~a~n',[Label]),
	!,
	mca_output(Syntax,OutputStream).

mca_output([ThisMSU|[]], OutputStream) :-
	!,
	format(OutputStream, '   [~n', []),
	write_items(ThisMSU, OutputStream),
	format(OutputStream, ' ]~n~n', []).
mca_output([ThisMSU|More], OutputStream) :-
	format(OutputStream, '   [~n', []),
	write_items(ThisMSU, OutputStream),
	format(OutputStream, ' ],~n~n', []),
	mca_output(More,OutputStream).

% ----- Write Items

write_items([ThisItem|[]], OutputStream) :-
	!,
	format(OutputStream, '     ', []),
	write_term(OutputStream, ThisItem, [quoted(true),portrayed(true)]).
write_items([ThisItem|More], OutputStream) :-
	format(OutputStream, '     ', []),
	write_term(OutputStream, ThisItem, [quoted(true),portrayed(true)]),
	format(OutputStream, ',~n', []),
	write_items(More,OutputStream).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% ------------ Phrasex ----------
% +Arg1 is e.g., [utterance('00000000.tx.1',"the dog is in the house")]
% +Arg2 is e.g., [parse('00000000.tx.1',minimal_syntax([[mod([inputmatch([the]),tag(noun)])...
phrasex_output([],[],_) :- !.
phrasex_output(AbstInpUttList,AbstParseList,OutputStream) :-
    AbstInpUttList = [utterance(Label,_)|_],
    assess_label(Label,medline,_AbstrID,_UttNumb),!,
    interpret_all_utterances(AbstInpUttList,medline,AbstParseList,OutputStream).
phrasex_output(AbstInpUttList,AbstParseList,OutputStream) :-
    AbstInpUttList = [utterance(Label,_)|_],
    assess_label(Label,other,Label,_UttNumb),!,
    interpret_all_utterances(AbstInpUttList,other,AbstParseList,OutputStream).

% ----------

assess_label( InpLabel, medline, AbstrID, Numb ) :-
    midstring( InpLabel, AbstrID, Back, 0, 8 ), % number of chars preceding first period
    midstring( Back, _TiAb, Numb, 0, 4 ), !.
assess_label( InpLabel, other, InpLabel, _Numb ).

% ---------- Interpret all Utterance ----------

interpret_all_utterances([],_,[],_) :- !.

% MEDLINE abstract
interpret_all_utterances([utterance(Label,_InputText)|MoreUtt],
                         medline,
                         [parse(Label,minimal_syntax(Phrases),_Definitions)
                         |MoreSynt],OutputStream) :-
%    format('~a ~s~n',[Label,InputText]),
    midstring( Label, _AbstrID, Back, 0, 8 ), 
    (midstring( Back,'.ti.' , _Numb, 0, 4 )
     -> TiAb = 'TI'
     ;  TiAb = 'AB'
    ),!,
    interpret_an_utterance(Phrases,Label,TiAb,OutputStream),
    interpret_all_utterances(MoreUtt,medline,MoreSynt,OutputStream).

% other text
interpret_all_utterances([utterance(Label,_InputText)|MoreUtt],
                         other,
                         [parse(Label,minimal_syntax(Phrases),_Definitions)
                         |MoreSynt],OutputStream) :-
%    format('~a ~s~n',[Label,InputText]),
    TiAb = 'TX',
    interpret_an_utterance(Phrases,Label,TiAb,OutputStream),
    interpret_all_utterances(MoreUtt,other,MoreSynt,OutputStream).

% ---------- Interpret an Utterance

interpret_an_utterance(Phrases,AbstrID,_TiAb,OutputStream) :-
    get_simple_noun_phrases(Phrases,TokenLists),
    write_noun_phrases(TokenLists,AbstrID,simp,OutputStream),
    get_macro_noun_phrases(Phrases,MacroTokenLists),
    write_noun_phrases(MacroTokenLists,AbstrID,macro,OutputStream),
    get_mega_phrases(Phrases,MegaTokenLists),
    write_noun_phrases(MegaTokenLists,AbstrID,mega,OutputStream).

% ---------- Get Simple Noun Phrases

get_simple_noun_phrases([],[]) :- !.
get_simple_noun_phrases([ThisPhrase|MorePhrases],[NewTokens|Gap]) :-
    get_np_tokens(ThisPhrase,[],TheseTokens,[head,mod,not_in_lex]),
    TheseTokens \== [],!,
    cull_hyphens(TheseTokens,NewTokens),
    get_simple_noun_phrases(MorePhrases,Gap).
get_simple_noun_phrases([ThisPhrase|MorePhrases],Gap) :-
    get_np_tokens(ThisPhrase,[],TheseTokens,[head,mod,not_in_lex]),
    TheseTokens == [],
    get_simple_noun_phrases(MorePhrases,Gap).

% ----- Get NP Tokens
% each np becomes one token list

get_np_tokens([],Tokens,Tokens,_TargetList) :- !.
get_np_tokens([ThisItem|More],TokensIn,TokensOut,TargetList) :-
    functor(ThisItem,Label,_),
    memberchk(Label,TargetList),
    Label == shapes,!,
    arg(1,ThisItem,ArgList),
    get_from_list(inputmatch,ArgList,InputMatchList),
    concat_atom(InputMatchList,InputAtom),
    LowList = [InputAtom],
    append(TokensIn,LowList,TokensTemp),
    get_np_tokens(More,TokensTemp,TokensOut,TargetList).
get_np_tokens([ThisItem|More],TokensIn,TokensOut,TargetList) :-
    functor(ThisItem,Label,_),
    memberchk(Label,TargetList),!,
    arg(1,ThisItem,ArgList),
    get_from_list(inputmatch,ArgList,InputMatchList),
    lower_list(InputMatchList,LowList),
    append(TokensIn,LowList,TokensTemp),
    get_np_tokens(More,TokensTemp,TokensOut,TargetList).
get_np_tokens([_No|More],TokensIn,TokensOut,TargetList) :-
    get_np_tokens(More,TokensIn,TokensOut,TargetList).

% -----

cull_hyphens([],[]) :- !.
cull_hyphens(['-'|More],Gap) :-
    cull_hyphens(More,Gap),!.
cull_hyphens([Other|More],[Other|Gap]) :-
    cull_hyphens(More,Gap).

/*
% ---------- Get Macro Noun Phrases

macro-noun phrases here are of the form: <prep> NP (of NP)*, where the
"of" phrase is repeated zero or more times.

Based on Lan's is_syntactically_simple/2

*/

get_macro_noun_phrases([_SinglePhrase|[]],[]) :- !.
get_macro_noun_phrases([],[]) :- !.
get_macro_noun_phrases(Phrases,[FirstTokenList|MoreTokenLists]) :-
    is_macro_noun_phrase(Phrases,FirstTokenList,Remainder),
    get_macro_noun_phrases(Remainder,MoreTokenLists).

/*
% ----- Is Macro Noun Phrase

-First can be any noun phrase
-Second can be any prep phrase; fails otherwise
-Rest is allowed to be empty; it always returns the
remainder of the sentence

*/
is_macro_noun_phrase([First,Second|Rest],MacroTokens,Remainder) :-
    get_from_list(head,First,_HeadList),
    get_np_tokens(First,[],FirstTokens,[head,mod,not_in_lex]),
    is_prep_phrase(Second,SecondTokens),
    are_of_phrases(Rest,[],OfPhraseTokens,Remainder),!,
    append(FirstTokens,SecondTokens,InterTokens),
    append(InterTokens,OfPhraseTokens,MacroTokens).
is_macro_noun_phrase([_First,Second|Rest],[],[Second|Rest]).
% ---
is_prep_phrase(Phrase,Tokens) :-
    Phrase = [FirstItem,_NextItem|_],
    get_phrase_item_name(FirstItem,prep),!,
    get_np_tokens(Phrase,[],Tokens,[head,mod,not_in_lex,prep]).

% ---
are_of_phrases([First|Rest],OfTokensIn,OfTokensOut,Remainder) :-
    is_of_phrase(First,ThisOfTokens),!,
    append(OfTokensIn,ThisOfTokens,TempOfTokens),
    are_of_phrases(Rest,TempOfTokens,OfTokensOut,Remainder).
are_of_phrases(Rest,Tokens,Tokens,Rest).

% --
is_of_phrase(Phrase,Tokens) :-
    Phrase = [FirstItem,_NextItem|_],
    get_phrase_item_name(FirstItem,prep),
    get_phrase_item_feature(FirstItem,inputmatch,InputMatch),
    InputMatch == [of],!,
    get_np_tokens(Phrase,[],Tokens,[head,mod,not_in_lex,prep]).

/*
% ---------- Get Mega Phrases

Mega phrases encompass everything up to a verb or modal. Initial
prepositions are removed. They aren't necessarily noun phrases, but
are needed by the UMLS group for identifying potential Metathesaurus
strings in MEDLINE abstracts.

*/
get_mega_phrases([],[]) :- !.
get_mega_phrases(Phrases,[FirstTokenList|MoreTokenLists]) :-
    remove_initial_item(Phrases,prep,NewPhrases),
    is_a_mega_phrase(NewPhrases,[],ThisMegaPhrase,Remainder),
    TargetList = [adv,conj,head,mod,not_in_lex,prep,shapes],
    get_np_tokens(ThisMegaPhrase,[],FirstTokenList,TargetList),
    get_mega_phrases(Remainder,MoreTokenLists).

% ----- Remove Initial Item
remove_initial_item([ThisPhrase|More], Label, [NewPhrase|More]) :-
	remove_initial_item_aux(ThisPhrase, Label, NewPhrase).

remove_initial_item_aux([FirstItem|More], Label, More) :-
	functor(FirstItem, Label, _), !.
remove_initial_item_aux(Phrases, _Label, Phrases).

% ----- Is a Mega Phrase
is_a_mega_phrase([], NewPhrase, NewPhrase, []) :- !.
is_a_mega_phrase([ThisPhrase|More], NewPhrase, NewPhrase, More) :-
	(	get_from_list(verb, ThisPhrase, _VerbInfo)
	;	get_from_list(modal, ThisPhrase, _ModalInfo)
	),
	!.
is_a_mega_phrase([ThisPhrase|More], PhraseIn, PhraseOut, Remainder) :-
	append(PhraseIn,ThisPhrase,TempPhrase),
	is_a_mega_phrase(More,TempPhrase,PhraseOut,Remainder).

% ---------- Write Noun Phrases
write_noun_phrases([],_,_,_) :- !.
write_noun_phrases([[]|More], AbstrID, Tag, OutputStream) :-
	write_noun_phrases(More,AbstrID,Tag,OutputStream),!.
write_noun_phrases([ThisTokenList|More], AbstrID, Tag, OutputStream) :-
	format(OutputStream,'~a|~a|', [AbstrID,Tag]),
	write_this_list(ThisTokenList, OutputStream),
	write_noun_phrases(More, AbstrID, Tag, OutputStream).

% ---
write_this_list([], OutputStream) :-
	!, format(OutputStream,'~n',[]).
write_this_list([Token|More],OutputStream) :-
	format(OutputStream,'~a ', [Token]),
	write_this_list(More, OutputStream).

%filter_genetic_predications([],[]) :- !.
%filter_genetic_predications([Predication|RestPredications],[Predication|RestPredicationsOut]) :-
%	Predication = _SubjectMaxDist-_SubjectDist-_SubjectIndex-
%	              _SubjectMetaConc-_SubjectCUI-_SubjectSemTypeList-_SubjectSemType-
%	              _IndicatorType-Relation-_IndicatorIndex-
%	              _ObjectMaxDist-_ObjectDist-_ObjectIndex-
%	              _ObjectMetaConc-_ObjectCUI-_ObjectSemTypeList-_ObjectSemType,
%        genetic_relation(Relation),
%!,
%	filter_genetic_predications(RestPredications,RestPredicationsOut).
%filter_genetic_predications([_Predications|RestPredications],RestPredicationsOut) :-
%	filter_genetic_predications(RestPredications,RestPredicationsOut).

