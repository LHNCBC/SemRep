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

% File:	    seminterp.pl
% Module:   seminterp
% Author:   tcr, modified by MF in the coordination predicate.
% Purpose:  Semantic interpretation in SPECIALIST, with neg processing


% ----- Module declaration and exported predicates

:- module( seminterp, [
		       semantic_interpretation/12,
		       check_for_neg/5
   ]).

:- load_files( usemrep_lib(module_version), [
		when(compile_time)
   ]).

% ----- Imported predicates

:- use_module( library(lists), [
		append/2,
		last/2,
		nextto/3,
	        rev/2,
		segment/2,
		select/3
   ]).

:- use_module( library( sets ), [
		intersect/2,
		setproduct/3
		]).

% :- use_module( skr(skr_utilities), [
% 		member_var/2,
% 		memberchk_var/2
%    ]).

:- use_module( skr_lib(nls_system), [
		control_value/2
   ]).

:- use_module( skr_lib( sicstus_utils ), [
		concat_atom/2,
		midstring/5,
		lower/2,			  
	        upper/2
   ]).

:- use_module( usemrep_lib( portray_minimal_syntax ), [
		portray_minimal_syntax_structure/1
   ]).

:- use_module( usemrep_lib( locsemnet_GEN ), [
		local_preferred_relation_GEN/1,
		local_relation_inverse_GEN/2
   ]).

:- use_module( usemrep_domain( locsemnet_DOM ), [
		local_preferred_relation_DOM/1,
		local_relation_inverse_DOM/2
   ]).

:- use_module( usemrep_lib( anaphora ), [
	get_npu_number/4,
	resolve_sortal_anaphora_if_necessary/12
	% resolve_sortal_anaphora_if_necessary_baseline/12
   ]).

:- use_module( usemrep_lib(ssuppserv), [
		announce_check_relation/7,
%		announce_conjlist/1,
%		announce_predication/5,
	        add_roles_to_conjuncts/7,
		check_for_passive/6,
		check_for_relativizer/1,
	        check_relation/3,
	        determine_left_domain/4,
	        determine_right_domain/4,
		find_the_next/6,
		find_next_non_adverb/2,			
%		get_all_semtypes/2,
		get_arguments/25,
%		get_arguments/19,
		get_base/4,
		get_conjunct_data/9,
		get_from_list_if_possible/3,
		get_left_partition/3,
		get_lexical_category/3,
		get_list_indexes/6,
		is_adjectival_indicator/1,
		is_next_msu_head/3,
		is_nominal_indicator/1,
		is_verb_indicator/1,
		get_sem_info_and_type/5,
%		get_term_with_metaconc/3,
		head_lists_are_not_conjuncts/3,
                get_transitive_cue/4,
		locate_npu_head/3,
		locate_npu_from_head/3,
		locate_npu_and_head/5,
		min_max/4,
		%no_conj_between_head_lists/3,
		no_conj_between_arguments/3,
		no_crossing_lines/5,
		npu_has_conjunct/6,
		potential_indicator/3,
		surface_role/3
   ]).

:- use_module( usemrep_lib(semrules_GEN), [
		word_corresponds_to_semnet_relation_GEN/4,
		multiphrase_corresponds_to_semnet_relation_GEN/6,
		phrase_corresponds_to_semnet_relation_GEN/6
   ]).

:- use_module( usemrep_domain(semrules_DOM), [
		word_corresponds_to_semnet_relation_DOM/4,
		multiphrase_corresponds_to_semnet_relation_DOM/6,
		phrase_corresponds_to_semnet_relation_DOM/6
   ]).

:- use_module( skr_lib(nls_lists), [
		get_from_list/3
   ]).

:- use_module( skr_lib( nls_system ), [
	        control_option/1
   ]).

% ****************************************************************************
% **************************  SEMANTIC_INTERPRETATION ************************
% ****************************************************************************

/*

NOTE: See seminterp_orig.pl for the original version
edit comments from that file

*/

% +ConjList
semantic_interpretation(DebugBitVector, AnalysisWithSemtypes,
			Definitions, VarInfoList,
			PrecedingVarInfoList, PrecedingAnalysis, PrecedingConjList,
			ConjList,SubordinatorPredicateList,PredicationsTail,
			SemInterpHead, SemInterpTail) :-
	% portray_minimal_syntax_structure(AnalysisWithSemtypes),
	rev(AnalysisWithSemtypes, RevAnalysis),
	rev(PrecedingVarInfoList, RevPrecedingVarInfoList),
	rev(PrecedingAnalysis, RevPrecedingAnalysis),
	rev(PrecedingConjList, RevPrecedingConjList),
	semantic_interpretation_1(AnalysisWithSemtypes, DebugBitVector, RevAnalysis,
				  Definitions, VarInfoList,
				  RevPrecedingVarInfoList,RevPrecedingAnalysis,RevPrecedingConjList,
				  ConjList,SubordinatorPredicateList,PredicationsTail,
				  % the two [] terms are accumulators for UsedHeads and UsedCues
				  % the fourth one keeps track of predication element indexes for "no crossing line"
				  [], [], [], pos_env,
				  SemInterpHead, SemInterpTail).

% ************************** SEMANTIC_INTERPRETATION/9 *************************

semantic_interpretation_1([], _DebugBitVector, _RevAnalysis, _Definitions, _VarInfoList,
			  _PrecedingVarInfoList,_PrecedingAnalysis, _PrecedingConjList,
			  _ConjList, _SubordinatorPredicateList,_PredicationsTail,
			  _UsedHeads, _UsedCues,_PredicationIndexes, _NegEncountered,
			  Predications, Predications).
semantic_interpretation_1([ThisMSU|RestMSUs], DebugBitVector, RevAnalysis,
			  Definitions, VarInfoList,
			  PrecedingVarInfoList, PrecedingAnalysis,PrecedingConjList,
			  ConjList, SubordinatorPredicateList,PredicationsTail,
			  UsedHeadsIn, UsedCuesIn, PredicationIndexIn, NegEncounteredIn,
			  PredicationsIn, PredicationsOut) :-

 	% format('~n~ncalling intRA_npu_relations on ~w~n', [ThisMSU]),
	intra_npu_relations(DebugBitVector, ThisMSU, ConjList,
			    PredicationsIn, PredicationsIntraTail),
 	% format('~n~ncalling intER_npu_relations on ~w~n', [ThisMSU]),
 	% format('~nfound ~w~n~n', [PredicationsIn]),
	inter_npu_relations(ThisMSU, RestMSUs, DebugBitVector, RevAnalysis,
			    Definitions, VarInfoList,
			    PrecedingVarInfoList, PrecedingAnalysis, PrecedingConjList,
			    ConjList,SubordinatorPredicateList,PredicationsTail,
			    UsedHeadsIn, UsedHeadsOut,
			    UsedCuesIn,  UsedCuesOut,
			    PredicationIndexIn, PredicationIndexOut,
			    NegEncounteredIn, NegEncounteredOut,
			    PredicationsIntraTail, PredicationsInterTail),
	!, % FML added cut 03/31/2015
 	% format('~nRestMSUs~w~n', [RestMSUs]),
 	% format('~nConjList~w~n', [ConjList]),
        % format('~nfound ~q~n~n', [PredicationsIntraTail]),
 	% format('~nUsedHeadsIn: ~q~nUsedHeadsOut: ~q~n', [UsedHeadsIn, UsedHeadsOut]),
	semantic_interpretation_1(RestMSUs, DebugBitVector, RevAnalysis,
				  Definitions, VarInfoList,
				  PrecedingVarInfoList, PrecedingAnalysis, PrecedingConjList,
				  ConjList, SubordinatorPredicateList,PredicationsTail,
				  UsedHeadsOut, UsedCuesOut, 
				  PredicationIndexOut, NegEncounteredOut,
				  PredicationsInterTail, PredicationsOut).

% ************************** INTRA_MSU_RELATIONS ******************************
/* 
*/

intra_npu_relations(DebugBitVector, ThisMSU, ConjList, [Predication|Rest], Rest)  :- 
	% look for mod-head relations only
	get_types_and_relation(DebugBitVector,
			       ThisMSU,
			       SubjectHeadList,
			       SubjectSemTypeList, SubjectSemType,
			       SubjectIndex,       SubjectMetaConcList,
			       SubjectCUI,
			       Relation,
			       ObjectHeadList,
			       ObjectSemTypeList, ObjectSemType,
			       ObjectIndex,       ObjectMetaConcList, 
			       ObjectCUI),
	%%%  format('~n~n######### get_types_and_relation~nThisMSU: ~w~nSubjectHeadList: ~w~nObjectHeadList: ~w~nSubjectSemType: ~w~nRelation: ~w~nObjectSemType: ~w~nSubjectMetaConcList: ~w~nObjectMetaConcList: ~w~n~n',
	%%%          [ThisMSU, SubjectHeadList, ObjectHeadList,
	%%% 	  SubjectSemType, Relation, ObjectSemType,
	%%% 	  SubjectMetaConcList, ObjectMetaConcList]),
				% format('~nConjList~q~n', [ConjList]),
	head_lists_are_not_conjuncts(ConjList, SubjectHeadList, ObjectHeadList),
       % no_conj_between_head_lists(ConjList,SubjectIndex,ObjectIndex),
	%%% LowerIndex/HigherIndex is the index of the Indicator
	Predication =  0-0-NormalizedSubjectIndex-
		       NormalizedSubjectMetaConc-NormalizedSubjectCUI-
		       NormalizedSubjectSemTypeList-NormalizedSubjectSemType-
		       'MOD/HEAD'-UpRelation-(LowerIndicatorIndex/HigherIndicatorIndex)-
		       0-0-NormalizedObjectIndex-
		       NormalizedObjectMetaConc-NormalizedObjectCUI-
		       NormalizedObjectSemTypeList-NormalizedObjectSemType,

	% reverse the order of the two arguments, if necessary
	normalize_relationship(Relation,
			       SubjectHeadList,               ObjectHeadList,
			       SubjectSemTypeList,            ObjectSemTypeList,
			       SubjectSemType,                ObjectSemType,
			       SubjectMetaConcList,           ObjectMetaConcList,
			       SubjectCUI,                    ObjectCUI,
			       SubjectIndex,                  ObjectIndex,

			       NormalizedRelation,
			       _NormalizedSubjectHeadList,    _NormalizedObjectHeadList,
			       NormalizedSubjectSemTypeList,  NormalizedObjectSemTypeList,
			       NormalizedSubjectSemType,      NormalizedObjectSemType,
			       NormalizedSubjectMetaConcList, NormalizedObjectMetaConcList,
			       NormalizedSubjectCUI,          NormalizedObjectCUI,
			       NormalizedSubjectIndex,        NormalizedObjectIndex),
%	!,
	\+ ( NormalizedSubjectCUI = NormalizedObjectCUI ),
	!,
	%%%%%% need to get all metaconcs
	min_max(SubjectIndex, ObjectIndex,
		LowerIndicatorIndex, HigherIndicatorIndex),
	NormalizedSubjectMetaConcList  = [NormalizedSubjectMetaConc|_],
	NormalizedObjectMetaConcList =   [NormalizedObjectMetaConc|_],
	% check for negation with 'non' 
	( has_non_modification(ThisMSU,SubjectHeadList) ->
	  negate_relation_0(NormalizedRelation,neg_env,NormalizedRelation0)
	; NormalizedRelation0 = NormalizedRelation
	),
	upper( NormalizedRelation0, UpRelation ).
intra_npu_relations(_DebugBitVector, _ThisMSU, _ConjList, Predications, Predications). %none


get_types_and_relation(DebugBitVector, ThisMSU,
		       SubjectHeadList,
		       SubjectSemTypeList, SubjectSemType,
		       SubjectIndex, SubjectMetaConcList,
		       SubjectCUI,
		       Relation,
		       ObjectHeadList,
		       ObjectSemTypeList, ObjectSemType,
		       ObjectIndex, ObjectMetaConcList,
		       ObjectCUI) :-
	get_types_and_relation_1(ThisMSU, SubjectHeadList, Relation, ObjectHeadList),
	% get the metaconc and semtype from the HeadList, e.g., from
	% metaconc([Reduced (qualifier value):[qlco]]),
	% extract ['Reduced (qualifier value)'] and qlco
	get_sem_info_and_type(SubjectHeadList, SubjectMetaConcList, SubjectCUI,
			      SubjectSemTypeList, SubjectSemType),
	get_sem_info_and_type(ObjectHeadList,  ObjectMetaConcList, ObjectCUI,
			      ObjectSemTypeList, ObjectSemType),
	% check to see if Semtype1-Relation-Semtype2 is a valid relation
	check_relation_plus('INTRA', DebugBitVector,
			    SubjectHeadList, SubjectSemType, SubjectMetaConcList,
			    _SubjectLexMatch, SubjectIndex,
			    mod_head, _IndicatorIndex, Relation,
			    ObjectHeadList, ObjectSemType, ObjectMetaConcList,
			    _ObjectLexMatch, ObjectIndex).

get_types_and_relation_1(ThisMSU, SubjectHeadList, Relation, ObjectHeadList) :-
	% Part I is for structures like
	% "pregnancy-induced hypertension" and
	% "lithium-induced headache"
	% assuming the hyphen is specified
	% ( segment(ThisMSU,
	%	  [mod(SubjectHeadList), punc(_), mod(IndicatorHeadList), head(ObjectHeadList)]),
	( append([_Prefix,
		  [mod(SubjectHeadList), punc(Punc), mod(IndicatorHeadList)],
		  Suffix],
		 ThisMSU),
	  get_from_list(inputmatch,Punc,[-]),
	  Suffix = [H|T],
	  first_head_after_mods(H, T, head(ObjectHeadList)) ->
	  get_corresponding_relation(mod(IndicatorHeadList), ThisMSU, ThisMSU, Relation,
				     'NO PREP!!', [], mod(IndicatorHeadList),
				     mod(IndicatorHeadList),_)
	% Part II is for standard mod-head structures.
	; nextto(mod(SubjectHeadList), head(ObjectHeadList), ThisMSU),
	  get_corresponding_relation(mod_head, ThisMSU, ThisMSU, Relation, _Cue, [],
				     mod_head,mod_head,_)
	).

first_head_after_mods(mod(_), [Next|Rest], Head) :-
	first_head_after_mods(Next, Rest, Head).
first_head_after_mods(head(HeadList), _Rest, head(HeadList)).


/*
% ************************** INTER_NPU_RELATIONS ******************************

This predicate (inter_npu_relations/13) proceeds through the sentence
from right to left, stopping at each potential indicator.  

Indicators are syntactic items which "indicate" a semantic relationships.
Currently these are nominalizations (and other relational nouns),
verbs, present and past participles, auxiliaries, and prepositions.
(The treatment of present participles/gerunds needs work.)

inter_npu_relations/13 first determines the "left partition" of the sentence,
which is the list of structures (reversed) preceding the current indicator.
Example: The Left partition of "abcX...", where X is the current indicator, is "cba".

The notion of Left and Right Domain is used in order to determine where to look for
the left and right arguments, which are figured differently for different indicators.

The left domain of the preposition "of" is the immediately preceding MSU.
The left domain of a past participle is the current MSU (since the PastPart
    may be the last item of this MSU) and all MSUs to the left.
The left domain for any other indicator is the list of MSUs to the left
    (i.e., the left partition).

The right domain for indicator prep is inside the current MSU.
The right domain for gerunds (may not be properly implemented) is the
    current MSU and all following MSUs.
The right domain for any other indicator is anyplace to the right of the current indicator.

get_arguments/22 get potential arguments for the various indicators [expand]

Reusable MSU

[describe the rest of the predicate]

----- Neg processing

When a negation trigger is encountered (see check_for_neg),
NegEncountered is set to neg_env. This is the only clause that
succeeds at an inter_npu semantic relationship. When this clause is
finished, NegEncountered is set to pos_env.  The consequence of this
is that negation only has scope over a single syntactic predication.

Note that "without" as a negation trigger is handled in
get_corresponding_relation. Also see check_for_neg. When "without" is
encountered: "without" is changed to "with" and the current relation
is negated.

>> this hasn't been completely worked out yet <<
"rule out" is considered to be a negation trigger. A special mechanism
is used to handle the fact that the two words of this trigger occur in
separate NPU's.  When check_for_neg encounters "rule" it returns the
value 'pot_neg_env' (potential negatation environment). It also
returns this value for "out". negate_relation then negates the current
relation under several conditions determined by two variables:
Previous and Current. if either current or previous is neg_env, it
negates the current relation. If either Previous or Current is
pot_neg_env, then the other must also be pot_neg env for the current
relation to be negated.

*/

inter_npu_relations(ThisMSU, RestMSUs, DebugBitVector, RevAnalysis,
		    Definitions, VarInfoList,
		    PrecedingVarInfoList,PrecedingAnalysis, PrecedingConjList,
		    ConjList,SubordinatorPredicateList,PredicationsTail,
		    UsedHeadsIn, UsedHeadsOut,
		    UsedCuesIn,  UsedCuesOut,
		    PredicationIndexIn, PredicationIndexOut,
		    NegEncounteredIn, NegEncounteredOut,
		    % [Predication|AllPredications],
		    AllPredications,
		    Gap) :-
	% for "aspirin and iron for headache and cancer",
	% the predication in the head of the clause is iron-TREATS-headache.

	% Indicator is a term of the form
	% head(_), mod(_), verb(_), aux(_), pastpart(_), prep(_), or ing(_)
	% format(user_error, 'PI0: ~w~n', [ThisMSU]),
	potential_indicator(ThisMSU, UsedCuesIn, Indicator),
	% format(user_error, 'PI1: ~w~n', [Indicator]),
	% LeftPartition is all MSUs between (and including) first MSU and ThisMSU
	get_left_partition(ThisMSU, RevAnalysis, LeftPartition),
	get_corresponding_relation(Indicator, ThisMSU, RestMSUs, Relation, Cue, 
	                           RemainingBases, UpdatedIndicator, IndicatorHead, NewRestMSUs),
	% format(user_error, 'IND:~w~nREL:~w~n~n', [Indicator,Relation]),
	% RestMSUs is essentially RightPartition
	determine_left_domain(Indicator, ThisMSU, LeftPartition, LeftDomain),
	determine_right_domain(Indicator, ThisMSU, NewRestMSUs, RightDomain),

%	get_arguments(IndicatorHead, UsedHeadsIn,
%                     ConjList, SubordinatorPredicateList, ThisMSU, 
%		      LeftPartition, LeftDomain, RightDomain, Definitions, VarInfoList, CueList,
%		      SubjectHeadList, _SubjectSemTypeList, SubjectSemType, 
%		      SubjectMetaConcList, _SubjectCUI,
%		      ObjectHeadList,  _ObjectSemTypeList,  ObjectSemType,  
%		      ObjectMetaConcList, _ObjectCUI),
	get_arguments(IndicatorHead, RemainingBases, Cue, Relation, UsedHeadsIn,
                      ConjList, SubordinatorPredicateList, ThisMSU, 
		      LeftPartition, NewRestMSUs,
		      LeftDomain, RightDomain, Definitions, VarInfoList, CueList,
		      SubjectHeadList, _SubjectSemTypeList, SubjectSemType, 
		      SubjectMetaConcList, _SubjectCUI,
		      ObjectHeadList,  _ObjectSemTypeList,  ObjectSemType,  
		      ObjectMetaConcList, _ObjectCUI),	
	% format(user_error, 'GAI: ~w~n',   [IndicatorHead]),
	% format(user_error, 'GAS: ~w~n',   [SubjectHeadList]),
	% format(user_error, 'GAO: ~w~n~n', [ObjectHeadList]),
        % format(user_error, '1 SubCUI|ObjCUI = ~w|~w~n', [SubjectCUI,ObjectCUI]),
	
	% format('~nSUBJECT:~n~w~nINDICATOR:~n~w~nOBJECT:~n~w~n',
	%       [SubjectHeadList, Indicator, ObjectHeadList]),
	arg(1, IndicatorHead, IndicatorList),
	% format(user_error, '~nIND  :~w~nSUBHL:~w~nOBJHL:~w~n~n',
	%        [IndicatorList, SubjectHeadList, ObjectHeadList]),
	
	\+ ( SubjectHeadList = ObjectHeadList),
	\+ ( SubjectHeadList = IndicatorList),
	\+ ( ObjectHeadList  = IndicatorList),

	% announce_predication('~nPREDICATION 2? ~q-~q-~q (~q) -~q-~q~n',
	% 		        Relation, Indicator, SubjectHeadList, ObjectHeadList),
	% announce_conjlist(ConjList),
	% format(user_output, 'CRP: ~w~n', [SubjectHeadList]),
	% format(user_output, 'CRP: ~w~n~n', [ObjectHeadList]),
	check_relation_plus('INTER', DebugBitVector,
			    SubjectHeadList, SubjectSemType, SubjectMetaConcList,
			    _SubjectLexMatch, SubjectIndex,
			    UpdatedIndicator, IndicatorIndex, Relation,
			    ObjectHeadList, ObjectSemType,   ObjectMetaConcList,
			    _ObjectLexMatch, ObjectIndex),
	% check_relation(SubjectSemType, Relation, ObjectSemType),
	( Cue == between-and ->
	    true
	; head_lists_are_not_conjuncts(ConjList, SubjectHeadList, ObjectHeadList)
	),
        % no_conj_between_head_lists(RevAnalysis,SubjectIndex,ObjectIndex),
	no_crossing_lines(SubjectIndex,IndicatorIndex,ObjectIndex,PredicationIndexIn,PredicationIndexOut),
%	not_hypernymic(SubjectIndex,ObjectIndex,PredicationsTail),
	% From HeadList, should go get the entire MSU to check for anaphoricity.
	% We might change get_arguments to carry over this information,
	% or we can get it from the full analysis in resolve_sortal_anaphora
	% tried locate_npu_head, but does not work.
	% I think the cleanest is to return this from get_arguments

	% format(user_output, 'SubjectHeadList = ~w~n', [SubjectHeadList]),
	% format(user_output, 'ObjectHeadList = ~w~n',  [ObjectHeadList]),

	get_anaphor_antecedents(SubjectHeadList, ObjectHeadList,
				IndicatorList,  PrecedingVarInfoList, PrecedingAnalysis,
				PrecedingConjList, VarInfoList, RevAnalysis,
				ConjList, PredicationsTail,
				SubjectNumbersAndAntecedents,
				ObjectNumbersAndAntecedents),
% Francois has implemented this setproduct call, but I think it has a side effect, 
% as we still do deal_with_coordination afterwards, which does something similar
% all adding up to 4 X(COREF) relations instead of 2, when an anaphor like 'both drugs'	
% refer to two drugs. See PMID 21397483. 
% This of course would not work, if we recognized multiple antecedents without using
% conjLists; however, since that is all we are doing right now, this should be sufficient. HK 10/26/2017
	
%	setproduct(SubjectNumbersAndAntecedents,
%		   ObjectNumbersAndAntecedents,
%		   CartesianProduct),
	SubjectNumbersAndAntecedents = [FirstSubjectNumbersAndAntecedents|_],
	ObjectNumbersAndAntecedents = [FirstObjectNumbersAndAntecedents|_],
	Product = [FirstSubjectNumbersAndAntecedents-FirstObjectNumbersAndAntecedents],
%	format(user_output,'Indicator : ~q~n~nCuesIn: ~q~n', [Indicator,UsedCuesIn]),
	create_all_predications(Product, SubjectSemType, ObjectSemType,
				Cue, CueList, ThisMSU, Definitions, RevAnalysis, LeftPartition, 
				IndicatorHead, IndicatorIndex,
				PrecedingConjList, ConjList, Relation,
				UsedCuesIn, UsedHeadsIn, NegEncounteredIn,
				UsedCuesOut, UsedHeadsOut, NegEncounteredOut,
				AllPredications, Gap).

inter_npu_relations(ThisMSU, RestMSUs, _DebugBitVector, RevAnalysis,
		    Definitions, VarInfoList,
		    _PrecedingVarInfoList,_PrecedingAnalysis, _PrecedingConjList,
		    _ConjList, _SubordinatorPredicateList, _PredicationsTail,
		    UsedHeadsIn, UsedHeadsOut,
		    UsedCuesIn,  UsedCuesIn,
		    PredicationIndexIn,PredicationIndexIn,
		    NegEncounteredIn, NegEncounteredOut,
		    Diff, Diff) :-
	potential_indicator(ThisMSU, UsedCuesIn, Indicator),
	get_left_partition(ThisMSU, RevAnalysis, LeftPartition),
	determine_left_domain(Indicator, ThisMSU, LeftPartition, _LeftDomain),
	determine_right_domain(Indicator, ThisMSU, RestMSUs, RightDomain),
	get_transitive_cue(uninterpreted,Indicator,Definitions,TranCue),
	\+ check_for_passive(Indicator,LeftPartition,VarInfoList,
	                     RestMSUs,Definitions,_),
	is_next_msu_head(MSUHeadList,TranCue,RightDomain),
	append(UsedHeadsIn,[role(obj),srole(obj),indicator(Indicator)|MSUHeadList],UsedHeadsOut),
	check_for_neg(NegEncounteredIn,ThisMSU, Definitions, RevAnalysis, NegEncounteredOut),
	!.
	

% No semantic interpretation returned, but neg processing
inter_npu_relations( ThisMSU, _RestMSUs, _DebugBitVector, RevAnalysis, 
                     Definitions, _VarInfoList,
		     _PrecedingVarInfoList, _PrecedingAnalysis, _PrecedingConjList,
		     _ConjList, _SubordinatorPredicateList, _PredicationsTail,
                     UsedHeads, UsedHeads,
                     UsedCues, UsedCues,
		     PredicationIndex,PredicationIndex,
                     NegEncounteredIn, NegEncounteredOut,
                     Diff, Diff) :-
	check_for_neg(NegEncounteredIn,ThisMSU, Definitions, RevAnalysis, NegEncounteredOut),
	!.

% no interp ?? is this really needed
% turn off neg if this is an indicator which wasn't used
inter_npu_relations( ThisMSU, _RestMSUs, _DebugBitVector, _RevAnalysis, 
                     _Definitions, _VarInfoList,
		     _PrecedingVarInfoList, _PrecedingAnalysis, _PrecedingConjList,
		     _ConjList,_SubordinatorPredicateList,_PredicationsTail,
                     UsedHeads, UsedHeads,
                     UsedCues, UsedCues,
		     PredicationIndex,PredicationIndex,
                     neg_env, pos_env,
                     Diff, Diff) :-
	potential_indicator(ThisMSU, [], Indicator),
	functor(Indicator, Label, _),
	memberchk(Label, [verb,aux,pastpart,prep]),
	!.

% no interp
inter_npu_relations( _ThisMSU, _RestMSUs, _DebugBitVector, _RevAnalysis, 
                     _Definitions, _VarInfoList,
		     _PrecedingVarInfoList, _PrecedingAnalysis, _PrecedingConjList,
		     _ConjList,_SubordinatorPredicateList,_PredicationsTail,
                     UsedHeads, UsedHeads, 
                     UsedCues, UsedCues,
		     PredicationIndex, PredicationIndex,
                     NegEnvIn, NegEnvIn,
                     Diff, Diff).   

get_anaphor_antecedents(SubjectHeadList,  ObjectHeadList,
			_IndicatorList,  _PrecedingVarInfoList, _PrecedingAnalysis,
			_PrecedingConjList, VarInfoList, RevAnalysis,
			_ConjList, PredicationsTail,
			AllSubjectNumbersAndAntecedents, AllObjectNumbersAndAntecedents) :-
	( control_option(anaphora_resolution) ->
%  	  resolve_sortal_anaphora_if_necessary(SubjectHeadList, [IndicatorList,ObjectHeadList],
%  					       PrecedingVarInfoList, PrecedingAnalysis,
%  					       PrecedingConjList, VarInfoList, RevAnalysis,
%  					       ConjList, PredicationsTail,
%  					       subject, OrigSubjectNumber, OrigSubjectHeadList0),
%	  find_anaphor_antecedent(SubjectHeadList, RevAnalysis, VarInfoList, PredicationsTail,
%				  NewSubjectNumber, NewSubjectHeadList0),
	  nonvar_corefs(PredicationsTail, NonVarCorefs),
	  find_all_anaphor_antecedents(NonVarCorefs, SubjectHeadList, RevAnalysis, VarInfoList,
				       AllSubjectNumbersAndAntecedents),
%	  test_new_anaphora_logic(subject, PredicationsTail, SubjectHeadList,
%				  OrigSubjectNumber,    NewSubjectNumber,
%			          OrigSubjectHeadList0, NewSubjectHeadList0),
%  	  resolve_sortal_anaphora_if_necessary(ObjectHeadList, [IndicatorList,SubjectHeadList],
%  					       PrecedingVarInfoList, PrecedingAnalysis,
%  					       PrecedingConjList, VarInfoList, RevAnalysis,
%  					       ConjList, PredicationsTail,
%  					       object, OrigObjectNumber, OrigObjectHeadList0),
%	  find_anaphor_antecedent(ObjectHeadList, RevAnalysis, VarInfoList, PredicationsTail,
%		       		  NewObjectNumber,   NewObjectHeadList0),
	  find_all_anaphor_antecedents(NonVarCorefs, ObjectHeadList, RevAnalysis, VarInfoList,
				       AllObjectNumbersAndAntecedents)
%	  test_new_anaphora_logic(object, PredicationsTail, ObjectHeadList,
%				  OrigObjectNumber,    NewObjectNumber,
%			          OrigObjectHeadList0, NewObjectHeadList0)

%	  control_value(a_choice, CHOICE),
%	  choose(CHOICE, SubjectHeadList, ObjectHeadList,
%		 OrigSubjectNumber, OrigSubjectHeadList0, OrigObjectNumber,  OrigObjectHeadList0,
%		 NewSubjectNumber,  NewSubjectHeadList0,  NewObjectNumber,   NewObjectHeadList0,
%		 SubjectNumber,     SubjectHeadList0,     ObjectNumber,      ObjectHeadList0)
	; AllSubjectNumbersAndAntecedents = [_/SubjectHeadList],
	  AllObjectNumbersAndAntecedents = [_/ObjectHeadList]
	).

nonvar_corefs(List, NonVarElements) :-
	( var(List) ->
	  NonVarElements = []
	; List = [H|T],
	  H = _Antecedent-'COREF'-_Anaphor ->
	  NonVarElements = [H|RestNonVarElements],
	  nonvar_corefs(T, RestNonVarElements)
	; nonvar_corefs(T, NonVarElements)
	).

not_hypernymic(_SubjectIndex,_ObjectIndex,PredicationsTail) :-
	var(PredicationsTail),
	!.
not_hypernymic(SubjectIndex,ObjectIndex,
	       [_Anaphor-'COREF'-_Antecedent|RestPredicationsTail]) :- 
	not_hypernymic(SubjectIndex,ObjectIndex,RestPredicationsTail).
not_hypernymic(SubjectIndex,ObjectIndex,
	       [_SubjectMaxDist-_SubjectDist-SubjectIndex0-
	       _SubjectConc-_SubjectCUI-_SubjectSemTypeList-_SubjectSemType-
	       _IndicatorType-Relation-_IndicatorIndex-
	       _ObjectMaxDist-_ObjectDist-ObjectIndex0-
	       _ObjectConc-_ObjectCUI-_ObjectSemTypeList-_ObjectSemType|RestPredicationsTail]) :-
	( Relation == 'ISA'
	; Relation == 'NEG_ISA'
	),
	( \+ SubjectIndex = SubjectIndex0
	; \+ ObjectIndex = ObjectIndex0
	),
	not_hypernymic(SubjectIndex,ObjectIndex,RestPredicationsTail).

find_all_anaphor_antecedents([], AnaphorHeadList, _Analysis, _VarInfoList, [_Number/AnaphorHeadList]).
find_all_anaphor_antecedents([FirstCoref|RestCorefs],
			     AnaphorHeadList, Analysis, VarInfoList, NumbersAndAntecedents) :-
	faaa([FirstCoref|RestCorefs],
	     AnaphorHeadList, Analysis, VarInfoList, NumbersAndAntecedents0),
	( NumbersAndAntecedents0 == [] ->
	  NumbersAndAntecedents = [_/AnaphorHeadList]
	; NumbersAndAntecedents = NumbersAndAntecedents0
	).

faaa([], _AnaphorHeadList, _Analysis, _VarInfoList, []).
faaa([FirstCoref|RestCorefs], AnaphorHeadList, Analysis, VarInfoList, NumbersAndAntecedents) :-
	FirstCoref = Anaphor-'COREF'-AntecedentHead,
	( memberchk(head(AnaphorHeadList), Anaphor) ->
	  locate_npu_from_head(Analysis, AnaphorHeadList, HeadNP),
	  get_npu_number(HeadNP, AnaphorHeadList, VarInfoList, Number),
	  NumbersAndAntecedents  = [Number/AntecedentHead|NumbersAndAntecedentsNext]
	; NumbersAndAntecedents  = NumbersAndAntecedentsNext
	),
	faaa(RestCorefs, AnaphorHeadList, Analysis, VarInfoList, NumbersAndAntecedentsNext).



create_all_predications([], _SubjectSemType, _ObjectSemType,
			_Cue, _CueList, _ThisMSU, _Definitions, _RevAnalysis, _LeftPartition, 
			_IndicatorHead, _IndicatorIndex,
			_PrecedingConjList, _ConjList, _Relation,
			UsedCues, UsedHeads, NegEncountered,
			UsedCues, UsedHeads, NegEncountered,
			AllPredications, AllPredications).
create_all_predications([SubjectNumber/SubjectHeadList0-ObjectNumber/ObjectHeadList0|Rest],
			SubjectSemType, ObjectSemType,
			Cue, CueList, ThisMSU, Definitions, RevAnalysis, LeftPartition,
			IndicatorHead, IndicatorIndex,
			PrecedingConjList, ConjList, Relation,
			UsedCuesIn, UsedHeadsIn, NegEncounteredIn,
			UsedCuesOut, UsedHeadsOut, NegEncounteredOut,
			AllPredicationsIn, AllPredicationsOut) :-
	create_all_predications_aux(SubjectNumber, SubjectHeadList0, SubjectSemType,
				    ObjectNumber, ObjectHeadList0, ObjectSemType,
				    Cue, CueList, ThisMSU, Definitions, RevAnalysis, LeftPartition,
				    IndicatorHead, IndicatorIndex,
				    PrecedingConjList, ConjList, Relation,
				    UsedCuesIn, UsedHeadsIn, NegEncounteredIn,
				    UsedCuesNext, UsedHeadsNext, NegEncounteredNext,
				    AllPredicationsIn, AllPredicationsNext),
	create_all_predications(Rest, SubjectSemType, ObjectSemType,
				Cue, CueList, ThisMSU, Definitions, RevAnalysis, LeftPartition,
				IndicatorHead, IndicatorIndex,
				PrecedingConjList, ConjList, Relation,
				UsedCuesNext, UsedHeadsNext, NegEncounteredNext,
				UsedCuesOut, UsedHeadsOut, NegEncounteredOut,
				AllPredicationsNext, AllPredicationsOut).

% need to create Cartesian Product, and call the rest of this code on each product!
% Had to implement memberchk mechanism to disallow inconsistent semantic types
% in predications. Bugs #52 and #54.  HK
create_all_predications_aux(SubjectNumber, SubjectHeadList0, SubjectSemType,
			    ObjectNumber, ObjectHeadList0, ObjectSemType,
			    Cue, CueList, ThisMSU, Definitions, RevAnalysis, _LeftPartition,
			    IndicatorHead, IndicatorIndex,
			    PrecedingConjList, ConjList, Relation,
			    UsedCuesIn, UsedHeadsIn, NegEncounteredIn,
			    UsedCuesOut, UsedHeadsOut, NegEncounteredOut,
			    AllPredications, Gap) :-
	( get_sem_info_and_type(SubjectHeadList0, SubjectMetaConcList0, SubjectCUI0,
				SubjectSemTypeList0, SubjectSemType0),
	  get_sem_info_and_type(ObjectHeadList0,  ObjectMetaConcList0, ObjectCUI0,
			      ObjectSemTypeList0, ObjectSemType0),
	  memberchk(SubjectSemType,SubjectSemTypeList0),
	  memberchk(ObjectSemType,ObjectSemTypeList0),
	  SubjectSemType2 = SubjectSemType,
	  ObjectSemType2 = ObjectSemType
	; get_sem_info_and_type(SubjectHeadList0, SubjectMetaConcList0, SubjectCUI0,
				SubjectSemTypeList0, SubjectSemType0),
	  get_sem_info_and_type(ObjectHeadList0,  ObjectMetaConcList0, ObjectCUI0,
			      ObjectSemTypeList0, ObjectSemType0),
	  ( memberchk(SubjectSemType,SubjectSemTypeList0) ->
	    SubjectSemType2 = SubjectSemType
	  ; SubjectSemType2 = SubjectSemType0
	  ),
	  ( memberchk(ObjectSemType,ObjectSemTypeList0) ->
	    ObjectSemType2 = ObjectSemType
	  ; ObjectSemType2 = ObjectSemType0
	  )
	),
	% format(user_error, '2 SubCUI|ObjCUI = ~w|~w~n', [SubjectCUI0,ObjectCUI0]),
	get_from_list(index, SubjectHeadList0, SubjectIndex0),
	get_from_list(index, ObjectHeadList0, ObjectIndex0),
	% format('~n###### PREDICATION SUCCEEDED!~n', []),
	% format('~nPREDICATION OK~n', []),
	add_cue_to_used_cues(CueList, UsedCuesIn, UsedCuesOut),
	normalize_relationship(Relation,
			       SubjectHeadList0,               ObjectHeadList0,
			       SubjectSemTypeList0,            ObjectSemTypeList0, 
			       SubjectSemType2,                 ObjectSemType2,
			       SubjectMetaConcList0,           ObjectMetaConcList0,
			       SubjectCUI0,                    ObjectCUI0,
			       SubjectIndex0,                  ObjectIndex0,

			       NormalizedRelation,
			       NormalizedSubjectHeadList,     NormalizedObjectHeadList,
			       NormalizedSubjectSemTypeList,  NormalizedObjectSemTypeList, 
			       NormalizedSubjectSemType,      NormalizedObjectSemType,
			       NormalizedSubjectMetaConcList, NormalizedObjectMetaConcList,
			       NormalizedSubjectCUI,          NormalizedObjectCUI,
			       NormalizedSubjectIndex,        NormalizedObjectIndex),
        % format(user_error, '3 SubCUI|ObjCUI = ~w|~w~n~n', [NormalizedSubjectCUI,NormalizedObjectCUI]),
	!,
	\+ ( NormalizedSubjectCUI = NormalizedObjectCUI ),
	NormalizedSubjectMetaConcList = [NormalizedSubjectMetaConc|_],
	NormalizedObjectMetaConcList  = [NormalizedObjectMetaConc|_],
	upper(NormalizedRelation, UpRelation),
	check_for_neg(NegEncounteredIn, ThisMSU, Definitions, RevAnalysis, NegEncounteredHere),
	check_for_argument_neg(SubjectHeadList0,subject,SubjectEnv),
	check_for_argument_neg(ObjectHeadList0,object,ObjectEnv),
	( same_np_relation(ThisMSU,IndicatorHead,SubjectHeadList0,ObjectHeadList0) ->
	  negate_relation_0(UpRelation, NegEncounteredHere,NegatedRelation)
	; negate_relation_1(UpRelation, NegEncounteredIn, NegEncounteredHere, SubjectEnv, ObjectEnv, NegatedRelation)
	),
	NegEncounteredOut = pos_env, % ??????
	% format('~n~n### deal_with_coordination:~n', []),
	% format('NormalizedSubjectHeadList: ~q~n', [NormalizedSubjectHeadList]),
	% format('NormalizedObjectHeadList: ~q~n', [NormalizedObjectHeadList]),
	% format('ConjList: ~q~n', [ConjList]),
	% format('~nPREDICATION:~q~n', [Predication]),
%	arg(1, Indicator, IndicatorList),
%	get_from_list(index, IndicatorList, IndicatorIndex),
	deal_with_coordination(Cue,NegatedRelation, NormalizedRelation, 
	                       IndicatorHead,IndicatorIndex,
			       NormalizedSubjectHeadList,
			       NormalizedSubjectSemTypeList,
			       NormalizedSubjectSemType,
			       NormalizedSubjectMetaConc,
			       NormalizedSubjectCUI,
			       NormalizedSubjectIndex,
			       SubjectNumber,

			       NormalizedObjectHeadList,
			       NormalizedObjectSemTypeList,
			       NormalizedObjectSemType,
			       NormalizedObjectMetaConc,
			       NormalizedObjectCUI,
			       NormalizedObjectIndex,
			       ObjectNumber,

			       RevAnalysis,PrecedingConjList,ConjList,
			       UsedHeadListWithRoles, RoleGap,
			       AllPredications, Gap),
	!,
	% format_predications(AllPredications),
%	UsedHeadsOut = UsedHeadListWithRoles,
	append(UsedHeadsIn,UsedHeadListWithRoles,UsedHeadsOut),		       
	RoleGap = [].
create_all_predications_aux(_SubjectNumber, _SubjectHeadList0, _SubjectSemType,
			    _ObjectNumber, _ObjectHeadList0, _ObjectSemType,
			    _Cue, _CueList, _ThisMSU, _Definitions, _RevAnalysis, _LeftPartition, 
			    _IndicatorHead, _IndicatorIndex,
			    _PrecedingConjList, _ConjList, _Relation,
			    UsedCues, UsedHeads, NegEncountered,
			    UsedCues, UsedHeads, NegEncountered,
			    AllPredications, AllPredications).


% find_anaphor_antecedent(AnaphorHeadList, Analysis, VarInfoList, Predications, Number, AntecedentHead) :-	
% 	( memberchk_var(Anaphor-'COREF'-AntecedentHead, Predications),
% 	  memberchk(head(AnaphorHeadList), Anaphor) ->
% 	  locate_npu_from_head(Analysis, AnaphorHeadList, HeadNP),
% 	  get_npu_number(HeadNP, AnaphorHeadList, VarInfoList, Number)  
% 	; AntecedentHead = AnaphorHeadList
% 	).
	
% format_predications(P) :- var(P), !.
% format_predications([H|T]) :-
% 	H = _-_-SC-ST-R-_-_-OC-OT,
% 	format('~nCOORD: ~q-~q-~q-~q-~q~n', [SC,ST,R,OC,OT]),
% 	format_predications(T).

%add_cue_to_used_cues(Cue, UsedCuesIn, UsedCuesOut) :-
%	( nonvar(Cue) ->
%	  ( Cue = Prep1-Prep2 ->
%	    UsedCuesOut = [Prep1,Prep2|UsedCuesIn]
%	  ; UsedCuesOut = [Cue|UsedCuesIn]
%	  )
%	; UsedCuesOut = UsedCuesIn
%	).
add_cue_to_used_cues(CueList, UsedCuesIn, UsedCuesOut) :-
    ( nonvar(CueList) ->
      UsedCuesOut = [CueList|UsedCuesIn]
    ; UsedCuesOut = UsedCuesIn
    ).


same_np_relation(MSU,IndicatorHead,SubjectHead,ObjectHead) :-
    member(IndicatorHead,MSU),
    check_for_member(MSU,SubjectHead),
    check_for_member(MSU,ObjectHead).

check_for_member([Element|_Rest],HeadList) :-
    arg(1,Element,HeadList),
    !.
check_for_member([_Element|Rest],HeadList) :-
    check_for_member(Rest,HeadList).
	
	

% ----- CHECK RELATION

check_relation_plus(_Caller, DebugBitVector,
		    SubjectHeadList, SubjectSemType, _SubjectMetaConcList,
		    SubjectInputMatch, SubjectIndex,
		    UpdatedIndicator, IndicatorIndex, Relation,
		    ObjectHeadList, ObjectSemType, _ObjectMetaConcList,
		    ObjectInputMatch, ObjectIndex) :-
	get_from_list_if_possible(inputmatch,  SubjectHeadList, SubjectInputMatch),
	get_from_list_if_possible(index,       SubjectHeadList, SubjectIndex),
	( atom(UpdatedIndicator) ->
	  IndicatorIndex = '<NONE>',
	  IndicatorInputMatch = UpdatedIndicator
	; get_indicator_index_inputmatch(UpdatedIndicator, IndicatorIndex, IndicatorInputMatch)
%	  arg(1, IndicatorHead, IndicatorList),
%	  get_from_list_if_possible(index,       IndicatorList,   IndicatorIndex),
%	  get_from_list_if_possible(inputmatch,  IndicatorList,   IndicatorInputMatch)
	),
	get_from_list_if_possible(inputmatch,  ObjectHeadList,  ObjectInputMatch),
	get_from_list_if_possible(index,       ObjectHeadList,  ObjectIndex),
	announce_check_relation(DebugBitVector, SubjectInputMatch,    SubjectSemType,
				IndicatorInputMatch, Relation,
				ObjectInputMatch,    ObjectSemType).
	% check_relation(SubjectSemType, Relation, ObjectSemType).

get_indicator_index_inputmatch(indicator([[Indicator]]),Index,IndicatorInputMatch) :-
	!,
	arg(1,Indicator,IndicatorList),
	get_from_list_if_possible(index,IndicatorList,Index),
	get_from_list(inputmatch,IndicatorList,IndicatorInputMatch).
% For cases like up-regulate, to handle the weirdness of what MetaMap returns.
get_indicator_index_inputmatch(indicator([IndicatorMSUList]),FirstIndex:LastIndex,IndicatorInputMatch) :-
	IndicatorMSUList = [FirstTerm|_],
	arg(1,FirstTerm,FirstTermList),
	last(IndicatorMSUList,LastTerm),
	arg(1,LastTerm,LastTermList),
	!,
	get_from_list_if_possible(index,FirstTermList,FirstIndex),
	get_from_list_if_possible(index,LastTermList,LastIndex),
	get_indicator_inputmatch([IndicatorMSUList],IndicatorInputMatch0),
	append(IndicatorInputMatch0,IndicatorInputMatch).
get_indicator_index_inputmatch(indicator(IndicatorMSUList),FirstIndex:LastIndex,IndicatorInputMatch) :-
	IndicatorMSUList = [FirstMSU|_],
	last(IndicatorMSUList,LastMSU),
	FirstMSU = [FirstTerm|_],
	arg(1,FirstTerm,FirstTermList),
	( locate_npu_head([LastMSU],_Type,LastTermList)
	; last(LastMSU,LastTerm),
	  arg(1,LastTerm,LastTermList)
	),
	!,
	get_from_list_if_possible(index,FirstTermList,FirstIndex),
	get_from_list_if_possible(index,LastTermList,LastIndex),
	get_indicator_inputmatch(IndicatorMSUList,IndicatorInputMatch0),
	append(IndicatorInputMatch0,IndicatorInputMatch).

get_indicator_inputmatch([],[]) :- !.
get_indicator_inputmatch([MSU|RestMSUList],[InputMatches|RestInputMatchList]) :-
	get_indicator_inputmatch_aux(MSU,InputMatches),
	!,
	get_indicator_inputmatch(RestMSUList,RestInputMatchList).

get_indicator_inputmatch_aux([],[]) :- !.
get_indicator_inputmatch_aux([MSUTerm|RestMSU],[InputMatch|RestInputMatches]) :-
	arg(1,MSUTerm,MSUTermList),
	get_from_list(inputmatch,MSUTermList,InputMatch),
	!,
	get_indicator_inputmatch_aux(RestMSU,RestInputMatches).
get_indicator_inputmatch_aux([_MSUTerm|RestMSU],RestInputMatches) :-
	get_indicator_inputmatch_aux(RestMSU,RestInputMatches).

% ---------- Get Head Lists

% get_head_lists([], []).
% get_head_lists([ThisMSU|Rest], [HeadList|Gap]) :-
% 	get_from_list(head, ThisMSU, HeadList),
% 	!,
% 	get_head_lists(Rest, Gap).
% get_head_lists([_NoHead|Rest], Gap) :-
% 	get_head_lists(Rest,Gap).

/*
% ---------- Negate Relation ----------

See discussion of neg processing under inter_npu_relations

*/

negate_relation_1(PosRelation, Previous, Current, Subject, Object, NegRelation) :-
	( ( Previous == neg_env -> true
	  ; Current == neg_env  -> true
	  ; ( Previous == pot_neg_env, Current == pot_neg_env )
	  ; Subject == subject_neg_env -> true
	  ; Object == object_neg_env -> true
	  ) ->
	  ( midstring(PosRelation,'NEG_',PosRelationNegRemoved, 0,4) ->
	    NegRelation = PosRelationNegRemoved	
	  ; concat_atom(['NEG_',PosRelation], NegRelation)
	  )
	; NegRelation = PosRelation
	).

negate_relation_0(PosRelation, Current, NegRelation) :-
	( Current == neg_env  ->
	  ( midstring(PosRelation,'NEG_',PosRelationNegRemoved, 0,4) ->
	    NegRelation = PosRelationNegRemoved	
	  ; concat_atom(['NEG_',PosRelation], NegRelation)
	  )
	; NegRelation = PosRelation
	).

/*
%% negate_relation(PosRelation, neg_env, _Current, NegRelation) :-
%% 	!,
%% 	concat_atom(['NEG_',PosRelation], NegRelation).
%% negate_relation(PosRelation,_Previous, neg_env, NegRelation) :-
%% 	!,
%% 	concat_atom(['NEG_',PosRelation], NegRelation).
%% negate_relation(PosRelation, pot_neg_env, pot_neg_env, NegRelation) :-
%% 	!,
%% 	concat_atom(['NEG_',PosRelation], NegRelation).
%% negate_relation(PosRelation, _Previous, _Current, PosRelation).
*/

/*
% ---------- Deal With Coordination ----------

%%%%ConjList = a list of: coord( Conj, LeftConjunct, RightConjunct, [] ) 

In the case of coordinated NPs in both subject and object, e.g.,
"aspirin and iron for headache and cancer",

NSubjectHeadList  = headlist for "iron"
NObjectHeadList = headlist for "headache"

LConjList1 = headlist for "aspirin"
RConjHeadList1 = NSubjectHeadList
CoordConcList1 = ['Aspirin']

LConjList2 = headlist for "headaches"
RConjHeadList2 = headlist for "cancer"
CoordConcList2 = ['Cancer Genus']

ConjList        = [ coord(_,X1,Y1,_), coord(_,X2,Y2,_) ]
             X1 = LConjList1, Y1 = RConjHeadList1
             X2 = LConjList2, Y2 = RConjHeadList2

deal_with_coordination now handles both subject and object coordination.
The main predication and the coordination-generated predications
are all created in deal_with_coordination.

*/

deal_with_coordination(Cue,NegatedRelation, _NormalizedRelation, Indicator, IndicatorIndex,
		       Subject, SubjectSemTypeList, SubjectSemType,
		       SubjectMetaConc, SubjectCUI, SubjectIndex, _SubjectNumber,
		       Object,  ObjectSemTypeList,  ObjectSemType,
		       ObjectMetaConc,  ObjectCUI, ObjectIndex, _ObjectNumber,
		       RevAnalysis,_PrecedingConjList, ConjList,
		       [SubjectListWithRoles,ObjectListWithRoles], _ObjGap,
		       AllPredications, Gap) :-
        Cue == between-and,
	surface_role(Indicator,Subject,SubjectRole),
	SubjectListWithRoles  = [role(subj),srole(SubjectRole),indicator(Indicator)|Subject],
	surface_role(Indicator,Object,ObjectRole),
	ObjectListWithRoles  = [role(obj),srole(ObjectRole),indicator(Indicator)|Object],
	build_all_predications([SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex-1],
			       NegatedRelation, IndicatorIndex, RevAnalysis, ConjList,
			       [ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType-ObjectIndex-1],
			       1,1,AllPredications, Gap).	
deal_with_coordination(_Cue,NegatedRelation, NormalizedRelation, Indicator, IndicatorIndex,
		       Subject, SubjectSemTypeList, SubjectSemType,
		       SubjectMetaConc, SubjectCUI, SubjectIndex, SubjectNumber,
		       Object,  ObjectSemTypeList,  ObjectSemType,
		       ObjectMetaConc,  ObjectCUI, ObjectIndex, ObjectNumber,
		       RevAnalysis,PrecedingConjList, ConjList,
		       UsedHeadListWithRoles, ObjGap,
		       AllPredications, Gap) :-
	append(PrecedingConjList,AllPreceding),
	append(ConjList,AllPreceding,AllConjList),
	( get_from_list(negated,Subject,SubjectNeg)
	; SubjectNeg = 0
	),
	( get_from_list(negated,Object,ObjectNeg)
	; ObjectNeg = 0
	),
	create_coord_list(subj, Subject, SubjectNumber,
			  SubjectSemTypeList,
			  Indicator,
			  NormalizedRelation,
			  ObjectSemTypeList,
			  AllConjList,
			  SubjectCoordList,
			  SubjectListWithRoles, SubjGap),
	create_coord_list(obj, Object, ObjectNumber,
			  ObjectSemTypeList,
			  Indicator,
			  NormalizedRelation,
			  SubjectSemTypeList,
			  AllConjList,
			  ObjectCoordList,
			  ObjectListWithRoles, ObjGap),
	UsedHeadListWithRoles = SubjectListWithRoles,
	SubjGap = ObjectListWithRoles,
	% format('~nSubjects:~n', []),
	% write_list_with_format(SubjectMetaConcsAndSemTypes, '*'),
	% format('~nObjects:~n', []),
	% write_list_with_format(ObjectMetaConcsAndSemTypes, '*'),
	% format('~nPREDICATION: ~q-~q-~q-~q-~q~n',
	%        [SubjectMetaConc,SubjectSemType,NegatedRelation,ObjectMetaConc,ObjectSemType]),
	% format('~nConjList:~n~q~n', [ConjList]),
	% format('~nSubjectList: ~q', [SubjectMetaConcAndSemTypeList]),
	% format('~nObjectList: ~q',  [ObjectMetaConcAndSemTypeList]),
	% arg(1, Indicator, IndicatorList),
	% get_from_list(index, IndicatorList, IndicatorIndex),
	build_all_predications([SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex-SubjectNeg
			         |SubjectCoordList],
			       NegatedRelation, IndicatorIndex, ConjList, RevAnalysis,
			       [ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType-ObjectIndex-ObjectNeg
			         |ObjectCoordList], SubjectNeg,ObjectNeg,
			       AllPredications, Gap).

% HeadList is a list such as
% [ index([12), bases([imipramine]), ausemtype([orch,phsu]), ... ]
% HeadSemType is the semtype of the HeadList that was used
% in the predication created in inter_npu_relations.
% IMPORTANT: We *must* pass in the semtype. Why?
% We cannot simply extract it from the ausemtype(_) term in HeadList
% because there could be several semtypes in the ausemtype(_) term,
% and we want to ensure that the predications created in deal_with_coordination
% use the same semtypes that appeared in the original predication
% (assuming the conjuncts have the original predication's semtype).
% ConjList is a list of coord(_,_,_,_,_) terms
% HeadAtom is either 'subj' or 'obj'

% create_coord_list/8 creates the following:
% CoordList: a list of terms of the form MetaConc-SemType-Index, e.g.,
% ['Sertraline'-phsu-15,'Paroxetine'-phsu-16]
% The list will contain one such term for each Head in [RConj|LConjList],
% i.e., one term for RConj, and one term for each Head in LConjList
% HeadListsWithRoles: a difference list containing a copy of RConj and each Head in LConjList,
% with with a role(subj) or role(obj) term, as appropriate, at the head of each list.
% Gap: The tail of the HeadListsWithRoles difference list

% If HeadAtom == subj
% then HeadList and HeadSemType are the Subject, and
% OtherSemType is the semtype of the Object
% and vice versa
create_coord_list(HeadAtom, HeadList, HeadNumber, HeadSemTypeList,
		  Indicator,Relation, OtherSemTypeList,
		  ConjList,
		  CoordList, HeadListsWithRoles, Gap) :-
	( var(HeadNumber)
	; HeadNumber == plural
	),
	surface_role(Indicator,HeadList,SurfaceRole),
	( npu_has_conjunct(HeadList, ConjList,
			   Conj, LeftConjList, RightConjunct, _) ->
	  get_conjunct_data(HeadList, HeadSemTypeList, Relation,
			    HeadAtom, OtherSemTypeList,
			    Conj, LeftConjList, RightConjunct, CoordList),
	  add_roles_to_conjuncts(Indicator,LeftConjList, RightConjunct,
				 HeadAtom, SurfaceRole,HeadListsWithRoles, Gap)
	; CoordList = [],
	  HeadListsWithRoles = [[role(HeadAtom),srole(SurfaceRole),indicator(Indicator)|HeadList]|Gap]
	).
create_coord_list(HeadAtom, HeadList, _HeadNumber, _HeadSemTypeList,
		  Indicator,_Relation, _OtherSemTypeList, _ConjList, [],
		  [[role(HeadAtom),srole(SurfaceRole),indicator(Indicator)|HeadList]|Gap],Gap) :-
	surface_role(Indicator,HeadList,SurfaceRole).


% ---------- Check for Neg ----------

% New clause added by FML 08/24/2016: Check for a negated(1) term
% in the feature list of the final element of the MSU, e.g.,
% head([index(4),usemtype(['Sign or Symptom']),ausemtype([sosy]),semgroup([diso]),
%       lexmatch([headache]),inputmatch([headache]),tag(noun),tokens([headache]),
%       metaconc(['Headache':'C0018681':[sosy]]),negated(1),position(15,23),bases([headache])])

%check_for_neg(neg_env, ThisMSU, _Definitions, RevAnalysis, neg_env) :- 
%	get_from_list(adv, ThisMSU, _AdvList),
%	rev(RevAnalysis,Analysis),
%	get_left_partition(ThisMSU, Analysis, RestAnalysis),
%	find_next_non_adverb(RestAnalysis,NextMSU),
%	is_verb_indicator(NextMSU),
%	!.
%check_for_neg(neg_env, ThisMSU, _Definitions, RevAnalysis, neg_env) :- 
%        get_from_list(aux, ThisMSU, _AuxList),
%	rev(RevAnalysis,Analysis),
%	get_left_partition(ThisMSU, Analysis, RestAnalysis),
%	find_next_non_adverb(RestAnalysis,NextMSU),
%	is_verb_indicator(NextMSU),
%	!.
%check_for_neg(neg_env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
%	( get_from_list(verb, ThisMSU, _VerbList)
%	; get_from_list(pastpart, ThisMSU, _PartList)
%	),
%	get_left_partition(ThisMSU, RevAnalysis, LeftPartition),
%	LeftPartition = [PrevMSU|_Rest],
%	( get_from_list(aux, PrevMSU, _AuxList)
%	; get_from_list(modal, PrevMSU, _ModalList)
%	; get_from_list(adv, PrevMSU, _AdvList)
%	),
%	!.
% This first clause aims to address negation occurring within verb complexes.
% The way verb complexes are chunked means simply relying on MSU distance
% does not work well.
check_for_neg(neg_env, ThisMSU, _Definitions, _RevAnalysis, neg_env) :- 
	( get_from_list(adv, ThisMSU, _AdvList)
	; get_from_list(aux, ThisMSU, _AuxList)
	; get_from_list(modal, ThisMSU, _ModList)
	; get_from_list(verb, ThisMSU, _VerbList)
	; get_from_list(pastpart, ThisMSU, _PartList)
	),
	!.
%check_for_neg(neg_env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
%	( get_from_list(verb, ThisMSU, _VerbList)
%	; get_from_list(pastpart, ThisMSU, _PartList)
%	),
%	get_left_partition(ThisMSU, RevAnalysis, LeftPartition),
%	LeftPartition = [PrevMSU|_Rest],
%	( get_from_list(aux, PrevMSU, _AuxList)
%	; get_from_list(modal, PrevMSU, _ModalList)
%	; get_from_list(adv, PrevMSU, _AdvList)
%	),
%	!.
check_for_neg(_Env, ThisMSU, _Definitions, _RevAnalysis, neg_env) :-
%	last(ThisMSU, LastMSUElement),
	locate_npu_head([ThisMSU], _Type, MSUHeadList),
	memberchk(negated(1),MSUHeadList),
%	arg(1, LastMSUElement, LastMSUElementFeatureList),
%	get_from_list(negated, LastMSUElementFeatureList, Negated),
%	Negated == 1,
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(adv, ThisMSU, AdvList),
	get_from_list(lexmatch, AdvList, [not]),
	\+ pseudo_negation(not, AdvList, ThisMSU, RevAnalysis),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, _RevAnalysis, neg_env) :-
	get_from_list(det, ThisMSU, AdvList),
	get_from_list(lexmatch, AdvList, [neither]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, _RevAnalysis, neg_env) :-
	get_from_list(conj, ThisMSU, AdvList),
	get_from_list(lexmatch, AdvList, [nor]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(det, ThisMSU, DetList),
	get_from_list(lexmatch, DetList, [no]),
	\+ pseudo_negation(no, DetList, ThisMSU, RevAnalysis),
	!.
check_for_neg(_Env, ThisMSU, Definitions, _RevAnalysis, neg_env) :-
	get_from_list(verb, ThisMSU, VerbList),
	get_from_list(lexmatch, VerbList, [Deny]),
	% memberchk(Deny,[deny,denies,denied,denying]),
	get_base( Deny, _LexicalCategory, Definitions, Base ),
	Base == deny,
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(prep, ThisMSU, PrepList),
	get_from_list(lexmatch, PrepList, [without]),
	\+ pseudo_negation(without,PrepList,ThisMSU,RevAnalysis),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(verb, ThisMSU, VerbList),
	get_from_list(bases, VerbList, [fail]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(adv,NextMSU,NextList),
	get_from_list(lexmatch,NextList, [to]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(verb, ThisMSU, VerbList),
	get_from_list(bases, VerbList, [fail]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(prep,NextMSU,NextList),
	get_from_list(lexmatch,NextList, [in]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(head, ThisMSU, VerbList),
	get_from_list(bases, VerbList, [failure]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(adv,NextMSU,NextList),
	get_from_list(lexmatch,NextList, [to]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(head, ThisMSU, VerbList),
	get_from_list(bases, VerbList, [failure]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(adv,NextMSU,NextList),
	get_from_list(lexmatch,NextList, [in]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, _RevAnalysis, neg_env) :-
	get_from_list(modal, ThisMSU, VerbList),
	get_from_list(lexmatch, VerbList, [cannot]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, _RevAnalysis, neg_env) :-
	get_from_list(adv, ThisMSU, VerbList),
	get_from_list(lexmatch, VerbList, [never]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(head, ThisMSU, VerbList),
	get_from_list(bases, VerbList, [incapable]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(adv,NextMSU,NextList),
	get_from_list(lexmatch,NextList, [of]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(head, ThisMSU, VerbList),
	get_from_list(bases, VerbList, [insufficient]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(adv,NextMSU,NextList),
	get_from_list(lexmatch,NextList, [to]),
	!.
check_for_neg(_Env, ThisMSU, _Definitions, RevAnalysis, neg_env) :-
	get_from_list(head, ThisMSU, VerbList),
	get_from_list(bases, VerbList, [unable]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(adv,NextMSU,NextList),
	get_from_list(lexmatch,NextList, [to]),
	!.
% Added by MF for loosely "negating" predisposes IR risk factors [as mods: lower, decreased, reduced]
% *** Commented out by Halil because it needs to be better thought out ***
% *** Currently finds stuff like NEG_DISRUPTS etc ***
%check_for_neg(ThisMSU, _Definitions, neg_env) :-
%	get_from_list(mod, ThisMSU, ModList),
%	get_from_list(lexmatch, ModList, LexList),
%	intersect(LexList, [lower, decreased, reduced]),
%	!.
check_for_neg(_Env, _ThisMSU, _Definitions, _RevAnalysis, pos_env).

%check_for_argument_neg(Relation,ArgList,Type,Env) :-
%	\+ midstring(Relation,'NEG_',_RelationNegRemoved, 0,4),
%	!,
%	check_for_argument_neg_aux(ArgList,Type,Env).
%check_for_argument_neg(_Relation,_ArgList,Type,Env) :-
%	concat_atom([Type,'_pos_env'], Env).

check_for_argument_neg(ArgList,Type,Env) :-
	memberchk(negated(1),ArgList),
	!,
	concat_atom([Type,'_neg_env'], Env).
check_for_argument_neg(_ArgList,Type,Env) :-
	concat_atom([Type,'_pos_env'], Env).

pseudo_negation(not,ItemList,ThisMSU,_RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[not]),
	nextto(adv(ItemList), adv(NextList), ThisMSU),
	get_from_list(lexmatch,NextList, [only]),
	!.
pseudo_negation(not,ItemList,ThisMSU,_RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[not]),
	nextto(adv(ItemList), adv(NextList), ThisMSU),
	get_from_list(lexmatch,NextList,[LexMatch]),
	memberchk(LexMatch,[significantly,necessarily,solely]),
	!.
pseudo_negation(not,ItemList,ThisMSU,RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[not]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(adv,NextMSU,AdvList),
	get_from_list(lexmatch,AdvList,[LexMatch]),
	memberchk(LexMatch,[significantly,necessarily,solely]),
	!.
pseudo_negation(not,ItemList,ThisMSU,_RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[not]),
	get_from_list(mod,ThisMSU,ModList),
	get_from_list(lexmatch,ModList,[LexMatch]),
	memberchk(LexMatch,[common,uncommon,important,unimportant,frequent,infrequent,rare,significant,insignificant]),
	!.
pseudo_negation(not,ItemList,ThisMSU,_RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[not]),
	get_from_list(head,ThisMSU,HeadList),
	get_from_list(lexmatch,HeadList,[LexMatch]),
	memberchk(LexMatch,[common,uncommon,important,unimportant,frequent,infrequent,rare,significant,insignificant]),
	!.
pseudo_negation(not,ItemList,ThisMSU,RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[not]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(mod,NextMSU,ModList),
	get_from_list(lexmatch,ModList,[LexMatch]),
	memberchk(LexMatch,[common,uncommon,important,unimportant,frequent,infrequent,rare,significant,insignificant]),
	!.
pseudo_negation(without,ItemList,ThisMSU,RevAnalysis) :-
	get_left_partition(ThisMSU, RevAnalysis, LeftPartition),
	get_from_list(lexmatch,ItemList,[without]),
	LeftPartition = [ConjMSU,WithMSU|_Rest],
	get_from_list(conj,ConjMSU,ConjList),
	get_from_list(prep,WithMSU,PrepList),
	get_from_list(lexmatch,ConjList,[LexMatch]),
	memberchk(LexMatch,[and,or]),
	get_from_list(lexmatch,PrepList,[with]),
	!.
pseudo_negation(without,ItemList,ThisMSU,_RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[without]),
	get_from_list(head,ThisMSU,HeadList),
	get_from_list(lexmatch,HeadList,[LexMatch]),
	memberchk(LexMatch,[doubt,question,delay]),
	!.
pseudo_negation(without,ItemList,ThisMSU,_RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[without]),
	get_from_list(mod,ThisMSU,HeadList),
	get_from_list(lexmatch,HeadList,[LexMatch]),
	memberchk(LexMatch,[further]),
	!.
pseudo_negation(without,ItemList,ThisMSU,RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[without]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(verb,NextMSU,VerbList),
	get_from_list(lexmatch,VerbList,[LexMatch]),
	memberchk(LexMatch,[doubt,question,delay]),
	!.
pseudo_negation(no,ItemList,ThisMSU,RevAnalysis) :-
	nextto(det(ItemList), adv(MoreList), ThisMSU),
	get_from_list(lexmatch,MoreList, [more]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(prep,NextMSU,PrepList),
	get_from_list(lexmatch,PrepList,[than]),
	!.
pseudo_negation(no,ItemList,ThisMSU,RevAnalysis) :-
	last(ThisMSU,det(ItemList)),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(det,NextMSU,DetList),
	get_from_list(lexmatch,DetList,[other]),
	!.
pseudo_negation(without,ItemList,ThisMSU,_RevAnalysis) :-
	get_from_list(lexmatch,ItemList,[no]),
	get_from_list(mod,ThisMSU,HeadList),
	get_from_list(lexmatch,HeadList,[LexMatch]),
	memberchk(LexMatch,[further]),
	!.
pseudo_negation(nothing,ItemList,ThisMSU,RevAnalysis) :-
	nextto(pron(ItemList), adv(MoreList), ThisMSU),
	get_from_list(lexmatch,MoreList, [more]),
	nextto(NextMSU,ThisMSU,RevAnalysis),
	get_from_list(prep,NextMSU,PrepList),
	get_from_list(lexmatch,PrepList,[than]),
	!.
		     
has_non_modification(ThisMSU,SubjectHeadList) :-
	get_from_list(mod,ThisMSU,SubjectHeadList),
	get_from_list(lexmatch,SubjectHeadList,LexMatchList),
	length(LexMatchList,Len),
	( Len == 1 ->
	  LexMatchList = [FirstLex|_],
	  lower(FirstLex,FirstLexLower),  
	  midstring(FirstLexLower,'non-',_,0,4)
	; LexMatchList = [FirstLex,SecondLex|_Rest],
	  lower(FirstLex,FirstLexLower),
	  midstring(FirstLexLower,non,-,0,3),
	  SecondLex == '-'
	).

	

% Given a structure such as 
% head(_), mod(_), verb(_), aux(_), pastpart(_), prep(_), or ing(_),
% determine the semnet relation encoded by the structure.
% (1) Use the functor of the structure to determine the lexical category,
% e.g., head --> noun; mod --> adj; verb --> verb, etc.
% (2) Get the lexical item in the lexmatch(_) element
% (3) Determine its base, if possible; otherwise just use the lexical item itself
% (4) Determine the semnet relation encoded by the Base/LexicalCategory combination

% clauses 2 and 3 now handle phrase and multi-phrase indicators
get_corresponding_relation( mod_head, _ThisMSU, RightMSUs, Relation, Cue,
			    [], mod_head, mod_head, RightMSUs ) :-
	!,
	word_corresponds_to_semnet_relation_BOTH(mod_head, _LexicalCategory, Cue, Relation ).

% handle in-phrase indicators, does not apply to multi-word lexical items like 
% "risk factor" or "due to", but to constructions like "reduced risk".
get_corresponding_relation(IndicatorStructure, ThisMSU, RightMSUs, Relation, Cue,
			   RemainingBases, UpdatedIndicator, IndicatorStructure, RightMSUs) :-
	functor(IndicatorStructure, Label, _),
	arg(1, IndicatorStructure, ArgList),
	get_lexical_category(ArgList, Label, LexicalCategory),
	get_from_list(bases, ArgList, Bases),
	( phrase_corresponds_to_semnet_relation_BOTH(Bases, RemainingBases, LexicalCategory,
						     RemainingCategories, Cue, Relation)
	; Bases = [Base],
	  phrase_corresponds_to_semnet_relation_BOTH(Base, RemainingBases, LexicalCategory,
						     RemainingCategories, Cue, Relation)
	),
        check_left_indicator(IndicatorStructure, ThisMSU, RemainingBases,
			     RemainingCategories, LeftIndicatorList),
	append(LeftIndicatorList, [IndicatorStructure], UpdatedIndicatorList),
	UpdatedIndicator = indicator([UpdatedIndicatorList]).
% handle multi-phrase indicators, such as "reduce risk of"
get_corresponding_relation(IndicatorStructure, _ThisMSU,RightMSUs, Relation, Cue,
			   RemainingBases, UpdatedIndicator, IndicatorHead, RestMSUs) :-
	functor(IndicatorStructure, Label, _),
	arg(1, IndicatorStructure, ArgList),
	get_lexical_category(ArgList, Label, LexicalCategory),
	get_from_list(bases, ArgList, Bases),
	( multiphrase_corresponds_to_semnet_relation_BOTH(Bases, RemainingBases, LexicalCategory,
							  RemainingCategories, Cue, Relation)
  	; Bases = [Base],					 
	  multiphrase_corresponds_to_semnet_relation_BOTH(Base, RemainingBases, LexicalCategory,
							  RemainingCategories, Cue, Relation)
	),
        check_right_indicator(RightMSUs, RemainingBases, RemainingCategories,
			      [], RightIndicatorMSUs, [], RightIndicatorList),
	append(RightIndicatorMSUs,RestMSUs,RightMSUs),
	append([[IndicatorStructure]],RightIndicatorList,UpdatedIndicatorList),
	get_indicator_head(UpdatedIndicatorList,IndicatorHead),
        UpdatedIndicator = indicator(UpdatedIndicatorList).
% handle simple indicators as well as the multi-word lexical items like "risk factor"
get_corresponding_relation(IndicatorStructure, _ThisMSU,RightMSUs, Relation, Cue,
			   [], indicator([[IndicatorStructure]]),IndicatorStructure,  RightMSUs) :-
        functor(IndicatorStructure, Label, _),
        arg(1, IndicatorStructure, ArgList),
        get_lexical_category(ArgList, Label, LexicalCategory),
        !,
        get_from_list(bases, ArgList, Bases),
	% multiword lexical items
        ( word_corresponds_to_semnet_relation_BOTH(Bases, LexicalCategory, Cue, Relation)
        % others
        ; Bases = [Base],
	  word_corresponds_to_semnet_relation_BOTH(Base, LexicalCategory, Cue, Relation)
        ).

%-----
% get_right_most_mod([mod(HeadList)|_], HeadList) :- !.
% get_right_most_mod([_|Rest], HeadList) :-
% 	get_right_most_mod(Rest, HeadList).

% This assumes that the indicator head is the last item in the IndicatorList
% that can have arguments. Such expressions are limited to verbs, nouns, adjectives for now.
% Another more precise mechanism might be to represent the expression in the semrule itself.
get_indicator_head(IndicatorList,IndicatorHead) :-
	rev(IndicatorList,RevIndicatorList),
	get_indicator_head_aux(RevIndicatorList,IndicatorHead),
	!.
get_indicator_head(IndicatorList,IndicatorHead) :-
	last(IndicatorList,HeadMSU),
	( locate_npu_head([HeadMSU],Type,Head),
	  functor(IndicatorHead,Type,1),
	  arg(1,IndicatorHead,Head)
        ; last(HeadMSU,IndicatorHead)
        ).

get_indicator_head_aux([MSU|_RestMSU],IndicatorHead) :-
	rev(MSU,RevMSU),
	get_MSU_indicator_head(RevMSU,IndicatorHead).
get_indicator_head_aux([_MSU|RestMSU],IndicatorHead) :-
	get_indicator_head_aux(RestMSU,IndicatorHead).

get_MSU_indicator_head([MSUElement|_RestMSUElements],MSUElement) :-
	( is_verb_indicator(MSUElement)
	; is_nominal_indicator(MSUElement)
	; is_adjectival_indicator(MSUElement)
	; MSUElement = prep(_)
	),
	!.
get_MSU_indicator_head([_MSUElement|RestMSUElements],IndicatorHead) :-
	get_MSU_indicator_head(RestMSUElements,IndicatorHead).

check_left_indicator(IndicatorStructure,ThisMSU,Bases,Categories,LeftIndicator) :- 
	ThisMSU = [msu_index(_Index)|MSU],
	append(PrecedingStructure,[IndicatorStructure|_],MSU),
	scan_for_indicator(PrecedingStructure,Bases,[],Categories,[],[],LeftIndicator).
	%check_left_indicator_aux(PrecedingStructure,Bases,Categories,LeftIndicatorIn,LeftIndicatorOut).

%check_left_indicator_aux(_MSUItems,[],[],LeftIndicator,LeftIndicator) :- !.
%check_left_indicator_aux([MSUItem|RestItems],[Bases|RestBases],[Category|RestCategories],LeftIndicatorIn,LeftIndicatorOut) :-
%	functor(MSUItem,Type,_),
%	arg(1,MSUItem,ItemList),
%	get_from_list(bases,ItemList,ItemBase),
%	( ItemBase = Bases
%        ; ItemBase = [Bases]
%        ),
%	get_lexical_category(ItemList,Type,Category),
%	!,
%	LeftIndicatorOut = [MSUItem|LeftIndicatorNext],
%	check_left_indicator_aux(RestItems, RestBases, RestCategories, LeftIndicatorIn,LeftIndicatorNext).
%check_left_indicator_aux([_MSUItem|RestItems],Bases,Categories,LeftIndicatorIn,LeftIndicatorOut) :-
%	check_left_indicator_aux(RestItems,Bases,Categories,LeftIndicatorIn,LeftIndicatorOut).

% check MSUs on the right to see if it matches elements of the multiphrase indicator.
% something has to match in the MSU. otherwise will fail.
% may need to allow intervening adverbials here, as well.
check_right_indicator(_RestMSUs,[],[],RightIndicatorMSUs,RightIndicatorMSUs,RightIndicator,RightIndicator) :-!.
check_right_indicator([MSU|RestMSU],Bases,Categories,
	              RightIndicatorMSUIn,RightIndicatorMSUOut,
		      RightIndicatorIn,RightIndicatorOut) :-
        MSU=[msu_index(_Index)|MSUIn],
	scan_for_indicator(MSUIn,Bases,RightBases,Categories,RightCategories,[],MatchingItems),
	\+ Bases = RightBases,
	\+ Categories = RightCategories,
	!,
	RightIndicatorMSUOut=[MSU|RightIndicatorMSUNext],
	RightIndicatorOut = [MatchingItems|RightIndicatorNext],
	check_right_indicator(RestMSU,RightBases,RightCategories,RightIndicatorMSUIn, 
	                      RightIndicatorMSUNext, RightIndicatorIn, RightIndicatorNext).

% scan an MSU for indicator elements, and 
% return unmatched bases and categories of the indicator for the next recursion
% as well as the matching MSUItems
scan_for_indicator([],Bases,Bases,Categories,Categories,
	           MatchingItems,MatchingItems) :- !.
scan_for_indicator([MSUItem|RestItems],[Base|RestBases],BasesOut,
	           [Category|RestCategories],CategoriesOut,
		   MatchingItemsIn,MatchingItemsOut) :-
	functor(MSUItem,Type,_),
	arg(1,MSUItem,ArgList),
	get_from_list(bases,ArgList,Bases),
	( Bases = Base
        ; Bases = [Base]
        ),
	get_lexical_category(ArgList,Type,Category),
	!,
	MatchingItemsOut=[MSUItem|MatchingItemsNext],
	scan_for_indicator(RestItems,RestBases,BasesOut,RestCategories,CategoriesOut,
	                   MatchingItemsIn,MatchingItemsNext).
scan_for_indicator([_MSUItem|RestItems],Bases,BasesOut,Categories,CategoriesOut,
	           MatchingItemsIn,MatchingItemsOut) :-
	scan_for_indicator(RestItems,Bases,BasesOut,Categories,CategoriesOut,
	                   MatchingItemsIn,MatchingItemsOut).
/*

% ---------- NORMALIZE_RELATIONSHIP ----------

relation_inverse/2 lists reverse of both the direct and the inverse semantic type;
preferred_relation/1 identifies the direct relation
*/

% If Relation is a preferred_relation or unspecified_relation,
% then keep the arguments in their original order
normalize_relationship( Relation,
                        SubjectHeadList,        ObjectHeadList,
			SubjectSemTypeList,     ObjectSemTypeList, 
			SubjectSemType,	        ObjectSemType, 
                        SubjectMetaConcList,    ObjectMetaConcList,
			SubjectCUI,             ObjectCUI,
			SubjectIndex,           ObjectIndex,

                        Relation,
                        SubjectHeadList,        ObjectHeadList,
			SubjectSemTypeList,     ObjectSemTypeList,
			SubjectSemType,         ObjectSemType, 
                        SubjectMetaConcList,    ObjectMetaConcList,
			SubjectCUI,             ObjectCUI,
			SubjectIndex,           ObjectIndex) :-

	( local_preferred_relation_BOTH(Relation)  ->
	  true
	; midstring(Relation,'neg_',RelationNegRemoved, 0,4),
	  local_preferred_relation_BOTH(RelationNegRemoved)  ->
	  true
	 % FML 03/28/2005 Monday @ 16:28:02 per Tom:
	 % the preferred relation of unspecified_relation is unspecified_relation
	; Relation == unspecified_relation
	),
	!.

% Otherwise, reverse the order of the arguments
normalize_relationship( Relation,
                        SubjectHeadList,       ObjectHeadList,
			SubjectSemTypeList,    ObjectSemTypeList,
			SubjectSemType,        ObjectSemType, 
                        SubjectMetaConcList,   ObjectMetaConcList,
			SubjectCUI,            ObjectCUI,
			SubjectIndex,          ObjectIndex,

                        NormalizedRelation,
                        ObjectHeadList,        SubjectHeadList,       % reverse order here!
			ObjectSemTypeList,     SubjectSemTypeList,    % reverse order here!
			ObjectSemType,         SubjectSemType,        % reverse order here!
                        ObjectMetaConcList,    SubjectMetaConcList,   % reverse order here!
			ObjectCUI,             SubjectCUI,
			ObjectIndex,           SubjectIndex) :-       % reverse order here!

	( local_relation_inverse_BOTH( Relation, NormalizedRelation )
	; midstring(Relation,'neg_',RelationNegRemoved, 0,4),
	  local_relation_inverse_BOTH(RelationNegRemoved,NormalizedRelationNegRemoved),
	  concat_atom(['neg_',NormalizedRelationNegRemoved], NormalizedRelation)
	),
	!.
% This last clause for normalize_relationship/14 is an error check
% to catch relations that are neither preferred, unspecified, or inverse.
% This should never happen.
normalize_relationship( Relation,
                        SubjectHeadList,      ObjectHeadList,
			SubjectSemTypeList,   ObjectSemTypeList, 
			SubjectSemType,       ObjectSemType, 
                        SubjectMetaConcList,  ObjectMetaConcList,
			SubjectCUI,           ObjectCUI,
			SubjectIndex,         ObjectIndex,

                        Relation,
                        SubjectHeadList,      ObjectHeadList,
			SubjectSemTypeList,   ObjectSemTypeList,
			SubjectSemType,       ObjectSemType, 
                        SubjectMetaConcList,  ObjectMetaConcList,
			SubjectCUI,           ObjectCUI,
			SubjectIndex,         ObjectIndex) :-
	format('~n~n~w ~w~n~nOther arguments are~n~w~n~w~n~w~n~w~n~w~n~w~n~w~n~w~n',
	       ['##### ERROR: Unknown relation: ', Relation,
		SubjectSemTypeList, SubjectSemType,
		ObjectSemTypeList,  ObjectSemType,
		SubjectMetaConcList, ObjectMetaConcList,
		SubjectCUI, ObjectCUI,
		SubjectHeadList, ObjectHeadList]).


% The new version of build_all_predications/6 takes
% SubjectList: a list of MetaConc-Semtype terms, one for each conjoined Subject;
%              if the subject is not conjoined, this list has only one element.
% Relation:    the relation, e.g., 'TREATS', 'OCCURS_IN', etc.
% ObjectList: same structure for objects

% It simply creates a predication using Relation for each subject/object pair
% in the Cartesian Product of the SubjectList and ObjectList

build_all_predications([],_, _, _, _, _, _,_,Predications, Predications).
build_all_predications([FirstSubject|RestSubjects], Relation, IndicatorIndex, ConjList,RevAnalysis, ObjectList,
		       SubjectNeg, ObjectNeg,
		       PredicationsIn, PredicationsOut) :-
	build_all_predications_1(ObjectList, Relation, IndicatorIndex, ConjList, RevAnalysis, FirstSubject, SubjectNeg, ObjectNeg,
				 PredicationsIn, PredicationsNext),
	build_all_predications(RestSubjects, Relation, IndicatorIndex, ConjList, RevAnalysis, ObjectList, SubjectNeg, ObjectNeg,
			       PredicationsNext, PredicationsOut).
build_all_predications([_FirstSubject|RestSubjects], Relation, IndicatorIndex, ConjList, RevAnalysis, ObjectList, SubjectNeg, ObjectNeg,
		       PredicationsIn, PredicationsOut) :-
	build_all_predications(RestSubjects, Relation, IndicatorIndex, ConjList, RevAnalysis, ObjectList, SubjectNeg, ObjectNeg,
			       PredicationsIn, PredicationsOut).

build_all_predications_1([], _, _, _, _, _, _,_,Predications, Predications).
build_all_predications_1([ObjectConc-ObjectCUI-ObjectSemTypeList-ObjectSemType-ObjectIndex-ObjectNeg|RestObjects],
			 Relation, IndicatorIndex, ConjList, RevAnalysis,
			 SubjectConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex-SubjectNeg,OrigSubjNeg,OrigObjNeg,
			 [_SubjectMaxDist-_SubjectDist-SubjectIndex-
			 SubjectConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
			 _IndicatorType-RelationOut-IndicatorIndex-
			 _ObjectMaxDist-_ObjectDist-ObjectIndex-
			 ObjectConc-ObjectCUI-ObjectSemTypeList-ObjectSemType
			 |PredicationsIn],
			 PredicationsOut) :-
	\+ ( SubjectCUI = ObjectCUI ),
	\+ ( IndicatorIndex = SubjectIndex ),
	\+ ( IndicatorIndex = ObjectIndex ),
        no_conj_between_arguments(ConjList,SubjectIndex,ObjectIndex),
	(( \+ ObjectNeg == OrigObjNeg 
	  ; \+ SubjectNeg == OrigSubjNeg
	  ) ->
	  ( midstring(Relation,'NEG_',RelationNegRemoved,0,4) -> 
	   RelationOut = RelationNegRemoved
	  ; concat_atom(['NEG_',Relation],RelationOut)
	  )
	; RelationOut = Relation
	),
	!,
	build_all_predications_1(RestObjects, Relation, IndicatorIndex, ConjList, RevAnalysis,
				 SubjectConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex-SubjectNeg,
				 OrigSubjNeg, OrigObjNeg, PredicationsIn, PredicationsOut).
build_all_predications_1([_ObjectList|RestObjects],
			 Relation, IndicatorIndex, ConjList, RevAnalysis,
			 SubjectList, SubjectNeg, ObjectNeg, PredicationsIn,PredicationsOut) :-
	build_all_predications_1(RestObjects,Relation, IndicatorIndex, ConjList, RevAnalysis, SubjectList, SubjectNeg, ObjectNeg,
	                         PredicationsIn,PredicationsOut).

%%% DOMAIN-CHOICE LOGIC begins here
% There are three files that exist in both GENeric and DOMain flavors:
% exceptions, locsemnet, and semrules.

% (1) The logic for locsemnet and semrules is:
%  * If running DOMain SemRep, call the DOM version first,
%    and if that fails, call the GEN version.
%  * If running GENeric SemRep, call the GEN version only.

% (2) The logic for exceptions is
%  * If running DOMain SemRep,  call the DOM version only.
%  * If running GENeric SemRep, call the GEN version only.

% word_corresponds_to_semnet_relation is semrules, so the first set of rules applies
word_corresponds_to_semnet_relation_BOTH( Bases, LexicalCategory, Cue, Relation) :-
	( control_value(domain, _Domain),
	  word_corresponds_to_semnet_relation_DOM( Bases, LexicalCategory, Cue, Relation)
 	; word_corresponds_to_semnet_relation_GEN( Bases, LexicalCategory, Cue, Relation)
	).

% phrase_corresponds_to_semnet_relation is semrules, so the first set of rules applies
phrase_corresponds_to_semnet_relation_BOTH(Bases, RemainingBases, LexicalCategory,
					   RemainingCategories, Cue, Relation) :-
	( control_value(domain, _Domain),
	  phrase_corresponds_to_semnet_relation_DOM(Bases, RemainingBases, LexicalCategory,
						    RemainingCategories, Cue, Relation)
	; phrase_corresponds_to_semnet_relation_GEN(Bases, RemainingBases, LexicalCategory,
							RemainingCategories, Cue, Relation)
	).

% multiphrase_corresponds_to_semnet_relation is semrules, so the first set of rules applies
multiphrase_corresponds_to_semnet_relation_BOTH(Bases, RemainingBases, LexicalCategory,
						RemainingCategories, Cue, Relation) :-
	( control_value(domain, _Domain),
	  multiphrase_corresponds_to_semnet_relation_DOM(Bases, RemainingBases, LexicalCategory,
							 RemainingCategories, Cue, Relation)
	; multiphrase_corresponds_to_semnet_relation_GEN(Bases, RemainingBases, LexicalCategory,
							     RemainingCategories, Cue, Relation)
	).

% local_preferred_relation is semrules, so the first set of rules applies
local_preferred_relation_BOTH(Relation) :-
	( control_value(domain, _Domain),
	  local_preferred_relation_DOM(Relation)
	; local_preferred_relation_GEN(Relation)
	).

% local_relation_inverse is semrules, so the first set of rules applies
local_relation_inverse_BOTH( Relation, NormalizedRelation) :-
	( control_value(domain, _Domain),
	  local_relation_inverse_DOM( Relation, NormalizedRelation)
	; local_relation_inverse_GEN( Relation, NormalizedRelation)
	).

	
% test_new_anaphora_logic(_, _, _, _, _, _, _) :- !.
% test_new_anaphora_logic(SubjectOrObject, PredicationsTail, TargetHeadList,
% 			OrigNumber,    NewNumber,
% 		        OrigHeadList0, NewHeadList0) :- 
% 	( OrigNumber = NewNumber,
% 	  OrigHeadList0 = NewHeadList0 ->
% 	  true
% 	; foo,
% 	  format('### ANAPHORA inconsistency between ~w/~w/~w~n~w~n~w~n~w~n~n',
% 		 [SubjectOrObject, OrigNumber, NewNumber, TargetHeadList, OrigHeadList0, NewHeadList0]),
% 	  display_COREF_predications(PredicationsTail)
% 	).
% 
% display_COREF_predications(PredicationsTail) :-
% 	( nonvar(PredicationsTail) ->
% 	    PredicationsTail = [H|T],
% 	      ( H = X-'COREF'-Y ->
% 		format('~w-~nCOREF-~n~w~n~n', [X,Y])
% 	      ; true
% 	      ),
% 	    display_COREF_predications(T)
% 	; true
% 	).     
% 
% 
% choose(orig, _SubjectHeadList, _ObjectHeadList,
%        OrigSubjectNumber, OrigSubjectHeadList0, OrigObjectNumber,  OrigObjectHeadList0,
%        _NewSubjectNumber, _NewSubjectHeadList0, _NewObjectNumber,  _NewObjectHeadList0,
%        SubjectNumber,     SubjectHeadList0,     ObjectNumber,      ObjectHeadList0) :-
% 	SubjectNumber    = OrigSubjectNumber,
% 	SubjectHeadList0 = OrigSubjectHeadList0,
% 	ObjectNumber     = OrigObjectNumber,
% 	ObjectHeadList0  = OrigObjectHeadList0.
% 
% 
% choose(new, _SubjectHeadList, _ObjectHeadList,
%        _OrigSubjectNumber, _OrigSubjectHeadList0, _OrigObjectNumber,  _OrigObjectHeadList0,
%        NewSubjectNumber,   NewSubjectHeadList0,   NewObjectNumber,    NewObjectHeadList0,
%        SubjectNumber,      SubjectHeadList0,      ObjectNumber,       ObjectHeadList0) :-
% 	SubjectNumber    = NewSubjectNumber,
% 	SubjectHeadList0 = NewSubjectHeadList0,
% 	ObjectNumber     = NewObjectNumber,
% 	ObjectHeadList0  = NewObjectHeadList0.
