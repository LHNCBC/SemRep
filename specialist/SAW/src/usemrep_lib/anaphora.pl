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

% File:	    anaphora.pl
% Module    anaphora 
% Author    halil
% Purpose   identify anaphora and find their antecedents


% ----- Module declaration and exported predicates

:- module( anaphora, [
	anaphora_resolution/9,
	get_npu_number/4,
	identify_sortal_anaphora/3,
	identify_sortal_anaphora_baseline/3,
	resolve_sortal_anaphora_if_necessary/12
	% resolve_sortal_anaphora_if_necessary_baseline/12
    ]).

:- use_module( library(sets), [
		intersect/2,
		intersection/3,
		list_to_set/2
   ]).

:- use_module( library(lists), [
		append/2,
		delete/3,
		last/2,
		nth0/4,
		rev/2
   ]).

:- use_module( skr_lib(nls_lists), [
                get_from_list/3
   ]).

:- use_module( skr_lib(sicstus_utils), [
	        midstring/5
   ]).

:- use_module( usemrep_lib(get_semtypes), [
		look_up_semantic_groups/3
  ]).

:- use_module( usemrep_lib(interanalysis), [
		find_subordinator/3
  ]).

:- use_module( usemrep_lib(mancmod), [
		meta_ancestors/3
  ]).

:- use_module( usemrep_lib(semspec), [
		must_be_appositives/2
  ]).

:- use_module( usemrep_lib(ssuppserv), [
		check_for_relativizer/1,
		clean_index/2,
		collect_bases/2,
		get_intervening_structure/3,
		get_left_partition_1/3,
		get_term_with_metaconc/3,
		is_adjectival_indicator/1,
		is_nominal_indicator/1,
		is_verb_indicator/1,
		locate_npu_from_head/3,
		locate_npu_head/3,
		lower_list/2,
		no_verbal/1,
		npu_has_conjunct/6,
		preceding_mod/3
    ]).

% SemSpecAnalysis not used. Remove probably.
anaphora_resolution(Analysis, PrecedingAnalysis, PrecedingVarInfoList,
		    PrecedingConjList, VarInfoList, ConjList,
		   SemSpecAnalysis, CoreferenceIn, CoreferenceOut) :-
	anaphora_resolution(Analysis, PrecedingAnalysis, PrecedingVarInfoList,
			    PrecedingConjList, Analysis, VarInfoList, ConjList,
			    SemSpecAnalysis, CoreferenceIn, CoreferenceOut).

anaphora_resolution([], _PrecedingAnalysis, _PrecedingVarInfoList,
		    _PrecedingConjList, _Analysis, _VarInfoList, _ConjList,
		    _SemSpecAnalysis, CoreferenceList, CoreferenceList).
anaphora_resolution([ThisMSU|RestMSUs], PrecedingAnalysis, PrecedingVarInfoList,
		    PrecedingConjList, Analysis, VarInfoList, ConjList,
		    SemSpecAnalysis, CoreferenceIn, CoreferenceOut) :-
	anaphoric(ThisMSU),
        locate_npu_head([ThisMSU],_Type,MSUHead),
%	get_rightmost_npu_element([ThisMSU],_Type,MSUHead),
	get_role(ThisMSU,Analysis,MSURole),
	rev(Analysis,RevAnalysis),
	rev(PrecedingAnalysis,RevPrecedingAnalysis),
	rev(PrecedingConjList,RevPrecedingConjList),
	rev(PrecedingVarInfoList,RevPrecedingVarInfoList),
%	format(user_output,'Resolve anaphora for: ~q~n~n',[ThisMSU]),
	resolve_sortal_anaphora_if_necessary(MSUHead,[],RevPrecedingVarInfoList,
					     RevPrecedingAnalysis,RevPrecedingConjList,
					     VarInfoList,RevAnalysis,ConjList,SemSpecAnalysis,
					     MSURole,Number,AntecedentHead),
	AntecedentHead \== MSUHead,
	!,
	get_anaphor_elements(ThisMSU,AnaphorElements),
	deal_with_coordination(Number,AnaphorElements,AntecedentHead,PrecedingConjList,ConjList,Corefs),
%	form_corefs(AnaphorElements,AntecedentHead,Corefs),
	append(Corefs,CoreferenceNext,CoreferenceOut),
	anaphora_resolution(RestMSUs, PrecedingAnalysis,PrecedingVarInfoList,
			    PrecedingConjList, Analysis, VarInfoList, ConjList,
			    SemSpecAnalysis, CoreferenceIn, CoreferenceNext).
anaphora_resolution([_ThisMSU|RestMSUs], PrecedingAnalysis, PrecedingVarInfoList,
		    PrecedingConjList, Analysis, VarInfoList, ConjList,
		    SemSpecAnalysis, CoreferenceIn, CoreferenceOut) :-
	anaphora_resolution(RestMSUs, PrecedingAnalysis, PrecedingVarInfoList,
			    PrecedingConjList, Analysis, VarInfoList, ConjList,
			    SemSpecAnalysis, CoreferenceIn, CoreferenceOut).

anaphora_resolution_baseline([], _PrecedingAnalysis, _PrecedingVarInfoList,
			     _PrecedingConjList, _Analysis, _VarInfoList, _ConjList,
			     _SemSpecAnalysis, CoreferenceList, CoreferenceList).
anaphora_resolution_baseline([ThisMSU|RestMSUs], PrecedingAnalysis, PrecedingVarInfoList,
			     PrecedingConjList, Analysis, VarInfoList, ConjList,
			     SemSpecAnalysis, CoreferenceIn, CoreferenceOut) :-
        anaphoric(ThisMSU),
        locate_npu_head([ThisMSU],_Type,MSUHead),
%	get_rightmost_npu_element([ThisMSU],_Type,MSUHead),
	get_role(ThisMSU,Analysis,MSURole),
	rev(Analysis,RevAnalysis),
	rev(PrecedingAnalysis,RevPrecedingAnalysis),
	rev(PrecedingConjList,RevPrecedingConjList),
	rev(PrecedingVarInfoList,RevPrecedingVarInfoList),
	resolve_sortal_anaphora_if_necessary_baseline(MSUHead,[],RevPrecedingVarInfoList,
					     RevPrecedingAnalysis,RevPrecedingConjList,
					     VarInfoList,RevAnalysis,ConjList,SemSpecAnalysis,
					     MSURole,_Number,AntecedentHead),
	AntecedentHead \== MSUHead,
	!,
	get_anaphor_elements(ThisMSU,AnaphorElements),
	deal_with_coordination(_Number,AnaphorElements,AntecedentHead,PrecedingConjList,ConjList,Corefs),
%	form_corefs(AnaphorElements,AntecedentHead,Corefs),
	append(Corefs,CoreferenceNext,CoreferenceOut),
	anaphora_resolution_baseline(RestMSUs, PrecedingAnalysis,PrecedingVarInfoList,
				     PrecedingConjList, Analysis, VarInfoList, ConjList,
				     SemSpecAnalysis, CoreferenceIn, CoreferenceNext).
anaphora_resolution_baseline([_ThisMSU|RestMSUs], PrecedingAnalysis, PrecedingVarInfoList,
			     PrecedingConjList, Analysis, VarInfoList, ConjList,
			     SemSpecAnalysis, CoreferenceIn, CoreferenceOut) :-
	anaphora_resolution_baseline(RestMSUs, PrecedingAnalysis, PrecedingVarInfoList,
				     PrecedingConjList, Analysis, VarInfoList, ConjList,
				     SemSpecAnalysis, CoreferenceIn, CoreferenceOut).
	
deal_with_coordination(plural,AnaphorElements,AntecedentHead,PrecedingConjList,ConjList,Corefs) :-
	append(PrecedingConjList,[ConjList],AllConjList0),
	append(AllConjList0,AllConjList),
	combine_conj_elements_aux(AllConjList,CombinedConjList),
	get_other_conjuncts(CombinedConjList,AntecedentHead,OtherHeadList),
	!,
	locate_npu_head([AnaphorElements],_Type,AnaphorHead),
	( all_conj_consonant(AnaphorHead,OtherHeadList) ->
	  form_corefs(AnaphorElements,OtherHeadList,Corefs0),
	  append(Corefs0,[AnaphorElements-'COREF'-AntecedentHead],Corefs)
	; Corefs = [AnaphorElements-'COREF'-AntecedentHead]
	).
deal_with_coordination(_Number,AnaphorElements,AntecedentHead,_PrecedingConjList,_ConjList,Corefs) :-
	form_corefs(AnaphorElements,[AntecedentHead],Corefs).

form_corefs(_AnaphorElements,[],[]) :- !.
form_corefs(AnaphorElements,[AntecedentHead|Rest],
	    [AnaphorElements-'COREF'-AntecedentHead|RestOut]) :-
	!,
	form_corefs(AnaphorElements,Rest,RestOut).

form_corefs_some(_AnaphorHead,_AnaphorElements,[],[]) :- !.
form_corefs_some(AnaphorHead,AnaphorElements,[AntecedentHead|Rest],
	    [AnaphorElements-'COREF'-AntecedentHead|RestOut]) :-
	consonance([semantic],AnaphorHead,AntecedentHead,_,_,_,_),
	!,
	form_corefs_some(AnaphorHead,AnaphorElements,Rest,RestOut).
form_corefs_some(AnaphorHead,AnaphorElements,[_AntecedentHead|Rest],RestOut) :-
	form_corefs_some(AnaphorHead,AnaphorElements,Rest,RestOut).

identify_sortal_anaphora(minimal_syntax(AnalysisIn),VarInfoList,minimal_syntax(AnalysisOut)) :-
 	 identify_sortal_anaphora(AnalysisIn,VarInfoList,AnalysisOut).

identify_sortal_anaphora([],_VarInfoList,[]) :- !.
identify_sortal_anaphora([ThisMSU|RestMSUs],VarInfoList,[ThisMSUOut|RestMSUsOut]) :-
	 ( RestMSUs == [] ->
	   NextMSU = []
	 ; RestMSUs = [NextMSU|_]
	 ),
	 % Other hypernym structures should probably also be blocked.
	 ( must_be_appositives(ThisMSU,NextMSU) ->
	   ThisMSUOut = ThisMSU
	 ; identify_sortal_anaphora_aux(ThisMSU,NextMSU,0,VarInfoList,ThisMSUOut)
	 ),
	!,
	identify_sortal_anaphora(RestMSUs,VarInfoList,RestMSUsOut).
identify_sortal_anaphora([_ThisMSU|RestMSUs],VarInfoList,RestMSUsOut) :-
	identify_sortal_anaphora(RestMSUs,VarInfoList,RestMSUsOut).

identify_sortal_anaphora_aux([],_,_,_,[]) :- !.
identify_sortal_anaphora_aux([MSUElement|Rest],NextMSU,SortalAnaphorFound,VarInfoList,
			     [MSUElementOut|RestOut]) :-
	( SortalAnaphorFound == 0 ->
	  sortal_anaphor(MSUElement,Rest,VarInfoList)
	; \+ functor(MSUElement,punc,1)
	),
	%% This is not sufficient, since NextMSU can be coordinated with the current MSU.
	%% In that case, we need to look at other subsequent MSUs.
	\+ prepositional_object(NextMSU),
	\+ self_referencing(MSUElement,Rest),
	!,
	functor(MSUElement, Functor, 1),
	arg(1,MSUElement,MSUElementList),
	append(MSUElementList,[sortalAnaphor],MSUElementListOut),
	functor(MSUElementOut,Functor,1),
	arg(1,MSUElementOut,MSUElementListOut),
%	format(user_output,'Sortal Anaphor: ~q~n',[MSUElementOut]),
	identify_sortal_anaphora_aux(Rest,NextMSU,1,VarInfoList,RestOut).
identify_sortal_anaphora_aux([MSUElement|Rest],NextMSU,SortalAnaphorFound,VarInfoList,
			     [MSUElement|RestOut]) :-
	identify_sortal_anaphora_aux(Rest,NextMSU,SortalAnaphorFound,VarInfoList,RestOut).

sortal_anaphor(Candidate,Rest,VarInfoList) :-
	Candidate = det(CandidateList),
	!,
	get_from_list(lexmatch, CandidateList, LexList),
	lower_list(LexList,LLexList),
	intersection(LLexList, [this, that, the, these, those, such, either, neither, both, each],Lex),
	\+ Lex == [],
	required_number(Lex,Number),
	locate_npu_head([Rest],_Type,Head),
	number(Head,VarInfoList,Number),
	% Not sure about this, but it seems like we need to block
	% expressions like 'the transcriptional level', where the semantics
	% is attached to transcriptional but not to level (essentially an empty head)
	% but we do not have that information at this point.
	% Needs thorough testing
	% This didn't work with cases like 'the interleukin-2 gene', where
	% 'interleukin-2 gene' 
	get_term_with_metaconc(Rest,head,_MetaConcList),
	\+ rigid_designator(Rest),
	\+ cataphoric(Rest).

required_number([],1).
required_number([the|_Rest],_Number) :- !.
required_number([Det|_Rest],plural) :-
%	intersect([Det],[these,those,such,both,either,neither,each]),
	intersect([Det],[these,those,such,both]),
	!.
required_number([_Det|Rest],Number) :-
	required_number(Rest,Number).

rigid_designator([mod(ModifierList)|_Rest]) :-
	memberchk(metaconc(_),ModifierList),
	get_from_list(tag,ModifierList,noun),
	!.
rigid_designator([shapes(ModifierList)|_Rest]) :-
	memberchk(metaconc(_),ModifierList),
	!.
rigid_designator([_Element|Rest]) :-
	rigid_designator(Rest).

cataphoric([MSUElement|_Rest]) :-
	arg(1,MSUElement,MSUList),
	get_from_list(bases,MSUList,[following]),
	!.
cataphoric([_MSUElement|Rest]) :-
	cataphoric(Rest).

anaphoric([MSUElement|_Rest]) :-
	arg(1,MSUElement,MSUList),
	intersect(MSUList,[sortalAnaphor]),
	!.
anaphoric([_MSUElement|Rest]) :-
	anaphoric(Rest).

get_anaphor_elements([],[]) :- !.
get_anaphor_elements([MSUElement|Rest],[MSUElement|RestOut]) :-
	anaphoric([MSUElement]),
	!,
	get_anaphor_elements(Rest,RestOut).
get_anaphor_elements([_MSUElement|Rest],RestOut) :-
	get_anaphor_elements(Rest,RestOut).


get_npu_number(AnaphorNP, AnaphorHeadList, VarInfoList, Number) :-
	( distributive_np(AnaphorNP) ->
	  Number = plural
	; number(AnaphorHeadList, VarInfoList, Number)
	).

% How can we handle exemplifications?
resolve_sortal_anaphora_if_necessary(AnaphorHeadList,ExcludeCandidates,PrecedingVarInfoList,
				     PrecedingAnalysis,PrecedingConjList,
				     VarInfoList,Analysis,ConjList,SemSpecAnalysis,
				     Role,Number,AntecedentHead) :-
	locate_npu_from_head(Analysis,AnaphorHeadList,AnaphorNP),
	anaphoric(AnaphorNP),
	% otherwise, 'each amine' is thought of as singular
	get_npu_number(AnaphorNP, AnaphorHeadList, VarInfoList, Number),
%	( distributive_np(AnaphorNP) ->
%	  Number = plural
%	; number(AnaphorHeadList,VarInfoList,Number)
%	),
	% Calling get_left_partition_1/3 instead of get_left_partition/3
	% becauseXF I think we don't want to check for seminterp barriers, but I might be
	% wrong. With semicolons, I can see get_left_partition/3 being a problem,
	% but section headers may present another issue that needs resolving.
	get_left_partition_1(AnaphorNP,Analysis,LeftPartition),
	( LeftPartition == [] ->
	  append([[]],PrecedingAnalysis,FullPrecedingAnalysis),
	  append([[]],PrecedingVarInfoList,FullVarInfoList),
	  append([[]],PrecedingConjList,FullConjList)
	; append([LeftPartition],PrecedingAnalysis,FullPrecedingAnalysis),
	  %% SHOULD REALLY BE LEFT PARTITION VARINFO ONLY, DOES IT MATTER?
	  append([VarInfoList],PrecedingVarInfoList,FullVarInfoList),
	  get_from_list(index,AnaphorHeadList,Index),
	  get_left_conj_list(ConjList,Index,LeftConjList),
	  append([LeftConjList],PrecedingConjList,FullConjList)
	),
	combine_conj_elements(FullConjList,CombinedConjList),
	combine_conj_elements([ConjList],[CSentenceConjList]),
	\+ conjunct_with_prepositional_object(Analysis,AnaphorNP,CSentenceConjList),
	get_consonant_NPs(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,FullVarInfoList,
			  FullPrecedingAnalysis,CombinedConjList,
			  SemSpecAnalysis,VarInfoList,SentenceCandidateNPs),
	append(SentenceCandidateNPs,CandidateNPs),
%	format(user_output,'CandidateNPs: ~q~n',[CandidateNPs]),
	find_antecedent(AnaphorNP,CandidateNPs,FullPrecedingAnalysis,Role,0,_SentenceDistanceOut,AntecedentNP),
	locate_npu_head([AntecedentNP],_Type,AntecedentHead).
resolve_sortal_anaphora_if_necessary(AnaphorHeadList,_Exclude,_PrecedingVarInfoList,
				     _PrecedingAnalysis,_PrecedingConjList,
				     _VarInfoList,_Analysis,_ConjList,_SemSpecAnalysis,
				     _Role,_Number,AnaphorHeadList).

get_consonant_NPs(_Number,_AnaphorNP,_Analysis,_ConjList,_Exclude,_,[],_CConjList,
		  _SemSpecAnalysis,_VarInfoList,[]) :- !.
get_consonant_NPs(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,[SentenceVarInfoList|RestVarInfoList],
		  [SentenceAnalysis|RestAnalysis],[SentenceCConjList|RestCConjList],
		  SemSpecAnalysis,VarInfoList,[SentenceConsonantNPs|RestConsonantNPs]) :-
%	append(SentenceCConjList,SentenceConjunctList),
	get_consonant_NPs_aux(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,
			      SentenceVarInfoList,SentenceAnalysis,SentenceCConjList,
			      SemSpecAnalysis,VarInfoList,SentenceConsonantNPs),
	!,
	get_consonant_NPs(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,RestVarInfoList,
			  RestAnalysis,RestCConjList,SemSpecAnalysis,VarInfoList,RestConsonantNPs).

get_consonant_NPs_aux(_Number,_AnaphorNP,_Analysis,_ConjList,_Exclude,
		      _SentenceVarInfoList,[],_SentenceCConjList,
		      _SemSpecAnalysis,_VarInfoList,[]) :- !.
get_consonant_NPs_aux(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,SentenceVarInfoList,
		      [CandidateNP|RestAnalysis],SentenceCConjList,
		      SemSpecAnalysis,VarInfoList,[CandidateNP|RestConsonantNPs]) :-
	locate_npu_head([AnaphorNP],_AnaType,AnaphorHeadList),
	locate_npu_head([CandidateNP],_CandType,CandidateHeadList),
	\+ has_semspec_relation(SemSpecAnalysis,AnaphorHeadList,CandidateHeadList),
	( Number == plural,
	  get_other_conjuncts(SentenceCConjList,CandidateHeadList,OtherHeadLists),
	  consonance([semantic],AnaphorHeadList,CandidateHeadList,AnaphorNP,CandidateNP,VarInfoList,SentenceVarInfoList),
	  all_conj_consonant(AnaphorHeadList,OtherHeadLists)
	; \+distributive_np(AnaphorNP),
	  consonance([number,semantic],AnaphorHeadList,CandidateHeadList,AnaphorNP,CandidateNP,VarInfoList,SentenceVarInfoList)
	),
	% If coming from seminterp, only make sure candidate is NOT the other argument.
	% Otherwise, use the *inadequate* syntactic consonance which tries to make sure
	% that the candidate and anaphor are not syntactic argument to the same predicate
	( ExcludeCandidates == [] ->
	  syntactic_consonance(AnaphorNP,CandidateNP,Analysis,ConjList)
	; \+ memberchk(CandidateHeadList,ExcludeCandidates)
	),
	!,
	get_consonant_NPs_aux(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,
			      SentenceVarInfoList,RestAnalysis,SentenceCConjList,
			      SemSpecAnalysis,VarInfoList,RestConsonantNPs).
get_consonant_NPs_aux(ConsonanceList,AnaphorNP,ConjList,Analysis,ExcludeCandidates,
		      SentenceVarInfoList,[_CandidateNP|RestAnalysis],SentenceCConjList,
		      SemSpecAnalysis,VarInfoList,RestConsonantNPs) :-
	get_consonant_NPs_aux(ConsonanceList,AnaphorNP,ConjList,Analysis,ExcludeCandidates,
			      SentenceVarInfoList,RestAnalysis,SentenceCConjList,
			      SemSpecAnalysis,VarInfoList,RestConsonantNPs).

%has_semspec_relation([SemSpecRelation|RestRelations],AnaphorNP,CandidateNP) :-
%	% At this point, semspec list is not complete.
%	nonvar(SemSpecRelation),
%	( SemSpecRelation = _SubjMaxDist-_SubjDist-_SubjIndex-_SubjMeta-_SubjCUI-
%	                    _SubjSemTypes-_SubjSemType-
%	                    _SpecRel-_SpecRel2-_IndicatorIndex-
%			    _ObjMaxDist-_ObjDist-ObjIndex-_ObjMeta-_ObjCUI-
%	                   _ObjSemTypes-_ObjSemType,
%	    get_from_list(index,AnaphorNP,ObjIndex)
%%	    get_from_list(msu_index,CandidateNP,SubjIndex)
%	; has_semspec_relation(RestRelations,AnaphorNP,CandidateNP)
%	).
has_semspec_relation([SemSpecRelation|RestRelations],AnaphorNP,CandidateNP) :-
	% At this point, semspec list is not complete.
	nonvar(SemSpecRelation),
	SemSpecRelation = _SubjMaxDist-_SubjDist-SubjIndex-_SubjMeta-_SubjCUI-
	                    _SubjSemTypes-_SubjSemType-
	                    _SpecRel-_SpecRel2-_IndicatorIndex-
			    _ObjMaxDist-_ObjDist-ObjIndex-_ObjMeta-_ObjCUI-
			    _ObjSemTypes-_ObjSemType,

	( get_from_list(index,AnaphorNP,ObjIndex)
	% this is meant to handle exemplifications, per Bug 14. Might cause problems?
	; get_from_list(index,CandidateNP,SubjIndex)
	; has_semspec_relation(RestRelations,AnaphorNP,CandidateNP)
	).


	
% When the antecedent is found to be a conjunction list,
% ensure that all conjuncts satistfy semantic constraints.
all_conj_consonant(AnaphorHead,LConjList) :-
	% If the conjunct list is empty, we should not have
	% come here!
	length(LConjList,LConjLen),
	LConjLen > 0,
	all_conj_consonant_aux(AnaphorHead,LConjList).

all_conj_consonant_aux(_AnaphorHead,[]) :- !.
all_conj_consonant_aux(AnaphorHead,[LConj|Rest]) :-
	consonance([semantic],AnaphorHead,LConj,_,_,_,_),
	!,
	all_conj_consonant_aux(AnaphorHead,Rest).

get_other_conjuncts([CConj|_Rest],Conjunct,OtherConjuncts) :-
	memberchk(Conjunct,CConj),
	delete(CConj,Conjunct,OtherConjuncts),
	!.
get_other_conjuncts([_CConj|Rest],Conjunct,OtherConjuncts) :-
	get_other_conjuncts(Rest,Conjunct,OtherConjuncts).

syntactic_consonance(AnaphorNP,CandidateNP,Analysis,ConjList) :-
	( \+ memberchk(CandidateNP,Analysis)
	; \+ potential_subject_object_pair(CandidateNP,AnaphorNP,Analysis,ConjList),
	  \+ non_distributive_conjoined_items(AnaphorNP,CandidateNP,ConjList)
	).

% This is almost certainly inadequate.
potential_subject_object_pair(CandidateNP,AnaphorNP,Analysis,ConjList) :-
	get_left_partition_1(AnaphorNP,Analysis,LeftPartition0),
	rev(LeftPartition0,RevLeftPartition0),
	get_left_partition_1(CandidateNP,RevLeftPartition0,InterveningStructure),
	( InterveningStructure == [] ->
%	  prepositional_object(CandidateNP),
	  memberchk(prep(_),AnaphorNP)
	; rev(Analysis, RevAnalysis),
	  get_role(CandidateNP,RevAnalysis,subject),
	  get_role(AnaphorNP,RevAnalysis,object),
	  potential_predicates(CandidateNP,InterveningStructure,ConjList,Types),
	  list_to_set(Types,TypeSet),
	  length(TypeSet,1)
	).

potential_predicates(_CandidateNP,[],_ConjList,[]) :- !.
potential_predicates(CandidateNP,[MSU|Rest],ConjList,[noun|RestOut]) :-
	\+ must_be_appositives(CandidateNP,MSU),
	\+ npu_has_conjunct(CandidateNP,MSU,ConjList),
	locate_npu_head([MSU],Type,MSUHeadList),
	functor(MSUHead,Type,1),
	arg(1,MSUHead,MSUHeadList),
	is_nominal_indicator(MSUHead),
	!,
	potential_predicates(CandidateNP,Rest,ConjList,RestOut).
% Probably need to take care of subordination??
% X .. suggest that Y 
potential_predicates(CandidateNP,[MSU|Rest],ConjList,[verb|RestOut]) :-
	last(MSU,MSUHead),
	is_verb_indicator(MSUHead),
	!,
	potential_predicates(CandidateNP,Rest,ConjList,RestOut).
potential_predicates(CandidateNP,[MSU|Rest],ConjList,[adj|RestOut]) :-
	locate_npu_head([MSU],Type,MSUHeadList),
	functor(MSUHead,Type,1),
	arg(1,MSUHead,MSUHeadList),
	is_adjectival_indicator(MSUHead),
	!,
	potential_predicates(CandidateNP,Rest,ConjList,RestOut).
potential_predicates(CandidateNP,[_MSU|Rest],ConjList,RestOut) :-
	potential_predicates(CandidateNP,Rest,ConjList,RestOut).

non_distributive_conjoined_items(AnaphorNP,CandidateNP,ConjList) :-
	get_from_list(det,AnaphorNP,_Determiner),
%	get_from_list(bases,Determiner,DeterminerBase),
	\+ distributive_np(AnaphorNP),
%	\+ intersect(DeterminerBase,[both,either,each,neither]),
	npu_has_conjunct(AnaphorNP,CandidateNP,ConjList).

distributive_np(NP) :-
	get_from_list(det,NP,Determiner),
	get_from_list(bases,Determiner,DeterminerBase),
	intersect(DeterminerBase,[both,either,each,neither]),
	!.

npu_has_conjunct(NP,ConjunctNP,ConjList) :-
	locate_npu_head([NP],_Type,NPHead),
	locate_npu_head([ConjunctNP],_ConjType,ConjunctHead),
	npu_has_conjunct(NPHead,ConjList,_Conj,LConjList,RConj,_ModList),
	( memberchk(ConjunctHead,LConjList)
	; RConj == ConjunctHead
	).

consonance(ConsonanceList,AnaphorHeadList,CandidateHeadList,AnaphorNP,CandidateNP,VarInfoListA,VarInfoListB) :-
	( memberchk(semantic,ConsonanceList) ->
	  \+ hypernymic_anaphor(CandidateHeadList,AnaphorHeadList,CandidateNP,AnaphorNP),
	  hypernymic_anaphor(AnaphorHeadList,CandidateHeadList,AnaphorNP,CandidateNP),
	  semantic_consonance(AnaphorHeadList,CandidateHeadList,AnaphorNP,CandidateNP)
	; true
	),
	( memberchk(number,ConsonanceList) ->
	  number_agreement(AnaphorHeadList,CandidateHeadList,VarInfoListA,VarInfoListB)
	; true
	).

% Almost the same code exists in interanalysis.pl. Consider merging.
semantic_consonance(HeadListA, HeadListB,_ANP,_BNP) :-
	get_from_list(ausemtype, HeadListA, ASTList),
	get_from_list(ausemtype, HeadListB, BSTList),
	intersection(ASTList, BSTList, CommonSTs),
	CommonSTs \== [],
	\+ look_up_semantic_groups(CommonSTs,[conc],generic),
	!.
semantic_consonance(HeadListA, HeadListB,_ANP,_BNP) :-
	get_from_list(semgroup, HeadListA, ASGList),
	get_from_list(semgroup, HeadListB, BSGList),
	intersection(ASGList, BSGList, CommonSGs),
	CommonSGs \== [],
	\+ memberchk(conc,CommonSGs).
semantic_consonance(HeadListA, HeadListB, ANP, BNP) :-
	headword_consonance(HeadListA, HeadListB, ANP, BNP).

headword_consonance(HeadListA,HeadListB,_ANP,_BNP) :-
	get_from_list(semgroup, HeadListB, BSGList),
	get_from_list(bases,HeadListA,Bases),
	semgroup_hypernym_match(BSGList,Bases).
headword_consonance(HeadListA,HeadListB,_ANP,_BNP) :-
	get_from_list(ausemtype, HeadListB, BSTList),
	get_from_list(bases,HeadListA,Bases),
	semtype_hypernym_match(BSTList,Bases).
headword_consonance(HeadListA,HeadListB,ANP,BNP) :-
	get_from_list(bases,HeadListA,BasesA),
	get_from_list(bases,HeadListB,BasesB),
	last(BasesA,BaseA),
	last(BasesB,BaseA),
	get_element_count(ANP,0,ALen),
	ALen =< 2,
	get_element_count(BNP,0,BLen),
	BLen > ALen,
	get_from_list(metaconc,HeadListA,AMetaconc),
	get_from_list(metaconc,HeadListB,BMetaconc),
	AMetaconc \== BMetaconc.

get_element_count([],In,In).
get_element_count([Element|Rest],In,Out) :-
	( Element = mod(ElementList)
	; Element = empty_head(ElementList)
	; Element = head(ElementList)
	; Element = empty_mod(ElementList)
	; Element = shapes(ElementList)
	; Element = not_in_lex(ElementList)
	; Element = unknown(ElementList)
	),
	!,
	get_from_list(inputmatch,ElementList,EIMList),
	length(EIMList,ELen),
	LenNext is In + ELen,
	get_element_count(Rest,LenNext,Out).
get_element_count([_Element|Rest],In,Out) :-
	get_element_count(Rest,In,Out).

hypernymic_anaphor(AnaphorHeadList, CandidateHeadList, _AnaphorNP,_CandidateNP) :-
	get_from_list(metaconc,AnaphorHeadList,AMetaConcList),
	get_from_list(metaconc,CandidateHeadList,CMetaConcList),
	hypernymic_anaphor_aux(AMetaConcList,CMetaConcList),
	\+ possibly_meronymic(AnaphorHeadList,CandidateHeadList).
hypernymic_anaphor(AnaphorHeadList, CandidateHeadList,_AnaphorNP,_CandidateNP) :-
	get_from_list(bases,AnaphorHeadList,Bases),
	has_genphenom(CandidateHeadList),
	gene_protein_hypernym_match(Bases).
hypernymic_anaphor(AnaphorHeadList, CandidateHeadList,AnaphorNP,CandidateNP) :-
	headword_consonance(AnaphorHeadList,CandidateHeadList,AnaphorNP,CandidateNP).

has_genphenom(CandidateHeadList) :-
	get_from_list(genphenom,CandidateHeadList,CGList),
	\+ CGList == [],
	CGList = [_,EntrezName,_],
	\+ EntrezName == ['None'].

hypernymic_anaphor_aux([_Concept:CUI:_SemTypes|_RestAMetaConcList], CMetaConcList) :-
	hypernymic(CUI,CMetaConcList),
	!.
hypernymic_anaphor_aux([_MetaConc|RestAMetaConcList],CMetaConcList) :-
	hypernymic_anaphor_aux(RestAMetaConcList,CMetaConcList).

hypernymic(ACUI,[_Concet:CUI:_SemTypes|_RestCMetaConcList]) :-
	meta_ancestors(ACUI,CUI,SpecGenAtom),
	midstring(SpecGenAtom,CUI,_,0,8),
	!.
hypernymic(ACUI,[_MetaConc|RestCMetaConcList]) :-
	hypernymic(ACUI,RestCMetaConcList).

possibly_meronymic(AnaphorHeadList,CandidateHeadList) :-
	get_from_list(semgroup,AnaphorHeadList,ASGList),
	get_from_list(semgroup,CandidateHeadList,CSGList),
	intersection(CSGList,ASGList,CommonSG),
	memberchk(anat,CommonSG),
	get_from_list(ausemtype,AnaphorHeadList,ASTList),
	get_from_list(ausemtype,CandidateHeadList,CSTList),
	intersection(CSTList,ASTList,CommonST),
	\+ memberchk(cell,CommonST).

semgroup_hypernym_match([SemGroup|_Rest],Base) :-
	semgroup_hypernym(SemGroup,Base),
	!.
semgroup_hypernym_match([_SemGroup|Rest],Base) :-
	semgroup_hypernym_match(Rest,Base).

semtype_hypernym_match([SemType|_Rest],Base) :-
	semtype_hypernym(SemType,Base),
	!.
semtype_hypernym_match([_SemType|Rest],Base) :-
	semtype_hypernym_match(Rest,Base).

% Should we have semtype_hypernym, as well?
% As it  turns out, we do not want here words that already map to the said semantic group.
% e.g., 'procedure' to 'proc'.
semgroup_hypernym(diso,[condition]).
semgroup_hypernym(diso,[disease]).
semgroup_hypernym(diso,[disorder]).
semgroup_hypernym(diso,[syndrome]).
semgroup_hypernym(diso,[illness]).
semgroup_hypernym(diso,[problem]).
semgroup_hypernym(diso,[abnormality]).
semgroup_hypernym(diso,[ailment]).
semgroup_hypernym(tmod,[procedure]).
%semgroup_hypernym(tmod,[therapy]).
%semgroup_hypernym(tmod,[treatment]).
semgroup_hypernym(tmod,[medication]).
semgroup_hypernym(tmod,[medicine]).
%semgroup_hypernym(tmod,[drug]).
%semgroup_hypernym(tmod,[enzyme]).
semgroup_hypernym(tmod,[intervention]).
semgroup_hypernym(tmod,[product]).
semgroup_hypernym(tmod,[remedy]).
semgroup_hypernym(tmod,[solution]).
semgroup_hypernym(tmod,[agent]).
semgroup_hypernym(tmod,[preparation]).
%semgroup_hypernym(proc,[procedure]).
%semgroup_hypernym(gene,[gene]).
%semgroup_hypernym(gene,[protein]).
%semgroup_hypernym(livb,[bacteria]).
semgroup_hypernym(livb,[group]).

semtype_hypernym(bact,[pathogen]).
semtype_hypernym(virs,[pathogen]).
semtype_hypernym(fngs,[pathogen]).
semtype_hypernym(euka,[pathogen]).
semtype_hypernym(bact,[microorganism]).
semtype_hypernym(virs,[microorganism]).
semtype_hypernym(fngs,[microorganism]).
semtype_hypernym(euka,[microorganism]).
%semtype_hypernym(grup,[group]).

gene_protein_hypernym_match([gene]).
gene_protein_hypernym_match([protein]).

number_agreement(HeadListA,HeadListB,VarInfoListA,VarInfoListB):-
	number(HeadListA,VarInfoListA,ANumber),
	number(HeadListB,VarInfoListB,BNumber),
	ANumber == BNumber.

% NOTE: This will identify all unknown lexmatches as singular.
% Probably not what we want, but not sure there is a general way of handling it.
number(HeadList, VarInfoList, Number) :-
	get_from_list(lexmatch,HeadList,LexMatchList),
	last(LexMatchList,LexMatch),
	get_from_list(LexMatch,VarInfoList,LexVarList),
	( intersect(LexVarList,[noun:[base]]) ->
	  Number = 1
	; intersect(LexVarList,[noun:[plural]]),
	  Number = plural
	),
	!.
% number(HeadList,VarInfoList,plural) :-
% 	get_from_list(lexmatch,HeadList,LexMatchList),
% 	last(LexMatchList,LexMatch),
% 	get_from_list(LexMatch,VarInfoList,LexVarList),
% 	intersect(LexVarList,[noun:[plural]]),
% 	!.
%number(HeadList,VarInfoList,plural) :-
%	get_from_list(lexmatch,HeadList,LexMatchList),
%	last(LexMatchList,LexMatch),
%	collective_nouns(CollectiveNouns),
%	memberchk(LexMatch,CollectiveNouns),
%	!.
number(_HeadList,_VarInfoList,1).

%collective_nouns([family,group,population]).

% Only one candidate
% Set the sentence distance and be done with it.
find_antecedent(_AnaphorNP,[AntecedentNP],Analysis,_Role,SentenceDist,SentenceDistOut,AntecedentNP) :-
	!,
	sentence_distance_from_anaphor(AntecedentNP,Analysis,SentenceDist,SentenceDistOut).
% Multiple candidates, at least one same sentence antecedent
% Take the first one in the list, should be the closest to the anaphor
find_antecedent(_AnaphorNP,[AntecedentNP|_RestCandidates],Analysis,_Role,0,0,AntecedentNP) :-
	sentence_distance_from_anaphor(AntecedentNP,Analysis,0,DistanceOut),
	DistanceOut == 0,
	!.
% No antecedent in the same sentence
% Continue with previous sentences
find_antecedent(AnaphorNP,CandidateNPs,Analysis,Role,0,SentenceDistOut,AntecedentNP) :-
	!,
	find_antecedent(AnaphorNP,CandidateNPs,Analysis,Role,1,SentenceDistOut,AntecedentNP).
find_antecedent(AnaphorNP, CandidateNPs, Analysis, _Role, SentenceDistIn, SentenceDistOut,AntecedentNP) :-
	SentenceDistIn > 0,
	filter_by_sentence(Analysis,CandidateNPs,SentenceDistIn,SentenceAnalysis,FilteredNPs),
	( FilteredNPs = [] ->
	  SentenceDistNext is SentenceDistIn + 1,
	  find_antecedent(AnaphorNP,CandidateNPs,Analysis,_Role,SentenceDistNext, SentenceDistOut,AntecedentNP)
	; length(FilteredNPs,1),
	  FilteredNPs = [AntecedentNP]
	; Analysis = [AnaphorAnalysis|_],
	  get_role(AnaphorNP,AnaphorAnalysis,AnaphorRole),
%	  format(user_output,'Anaphor Role: ~a~n', [AnaphorRole]),
	  find_antecedent_aux(AnaphorNP,AnaphorRole,SentenceAnalysis,FilteredNPs,AntecedentNP),
	  SentenceDistOut = SentenceDistIn
	).

% This predicate is useless now, because we don't need to look at the argument role anymore.
find_antecedent_aux(_AnaphorNP,object,_SentenceAnalysis,FilteredNPs, AntecedentNP) :-
%	last(FilteredNPs,AntecedentNP).
	FilteredNPs = [AntecedentNP|_],
	!.
find_antecedent_aux(_AnaphorNP,subject,_SentenceAnalysis,FilteredNPs,AntecedentNP) :-
	FilteredNPs = [AntecedentNP|_].
%	filter_by_role(object,FilteredNPs,SentenceAnalysis,ObjectNPs),
%	( ObjectNPs == [] ->
%	  FilteredNPs = [AntecedentNP|_]
%	; ObjectNPs = [AntecedentNP|_]
%	).

filter_by_sentence(Analysis,CandidateNPs,Distance,SentenceAnalysis,FilteredNPs) :-
	nth0(Distance,Analysis,SentenceAnalysis,_),
	filter_by_sentence_aux(SentenceAnalysis, CandidateNPs,FilteredNPs).

filter_by_sentence_aux(_Analysis,[],[]) :- !.
filter_by_sentence_aux(Analysis,[CandidateNP|RestCandidates],[CandidateNP|RestFilteredNPs]) :-
	memberchk(CandidateNP,Analysis),
	!,
	filter_by_sentence_aux(Analysis,RestCandidates,RestFilteredNPs).
filter_by_sentence_aux(Analysis,[_CandidateNP|RestCandidates],RestFilteredNPs) :-
	filter_by_sentence_aux(Analysis,RestCandidates,RestFilteredNPs).


sentence_distance_from_anaphor(NP,[SentenceAnalysis|_RestAnalysis],Distance,Distance) :-
	memberchk(NP,SentenceAnalysis),
	!.
sentence_distance_from_anaphor(NP,[_SentenceAnalysis|RestAnalysis],DistanceIn,DistanceOut) :-
	DistanceNext is DistanceIn + 1,
	sentence_distance_from_anaphor(NP,RestAnalysis,DistanceNext,DistanceOut).

get_left_conj_list([],_,[]) :- !.
get_left_conj_list([CoordStruct|RestCoord],
		    AnaphorIndex,[CoordStruct|RestCoordOut]) :-
	CoordStruct = coord(_Conj,_Type,CoordIndex,LConjList,RConj,_ModList),
	all_indexes_to_left(AnaphorIndex,CoordIndex,LConjList,RConj),
	!,
	get_left_conj_list(RestCoord,AnaphorIndex,RestCoordOut).
get_left_conj_list([_CoordStruct|RestCoord],AnaphorIndex,RestOut) :-
	get_left_conj_list(RestCoord,AnaphorIndex,RestOut).

all_indexes_to_left(AnaphorIndex,CoordIndex,LConjList,RConj) :-
	clean_index(AnaphorIndex,CleanAnaphorIndex),
	clean_index(CoordIndex,CleanCoordIndex),
	CleanCoordIndex < CleanAnaphorIndex,
	LConjList \== [],
	RConj \== [],
	indexes_to_left(CleanAnaphorIndex, LConjList),
	indexes_to_left(CleanAnaphorIndex, [RConj]).

indexes_to_left(_Index,[]) :- !.
indexes_to_left(Index,[MSU|Rest]) :-
	get_from_list(index,MSU,MSUIndex0),
	clean_index(MSUIndex0,MSUIndex),
	MSUIndex < Index,
	!,
	indexes_to_left(Index,Rest).

combine_conj_elements([],[]) :- !.
combine_conj_elements([SentenceCoord|Rest],[SentenceCoordOut|RestOut]) :-
	combine_conj_elements_aux(SentenceCoord,SentenceCoordOut),
	!,
	combine_conj_elements(Rest,RestOut).
combine_conj_elements([_SentenceCoord|Rest],[[]|RestOut]) :-
	combine_conj_elements(Rest,RestOut).

combine_conj_elements_aux([],[]) :- !.
combine_conj_elements_aux([coord(_Conj,_Type,_CoordIndex,LConjList,RConj,_ModList)|Rest],
			  [ConjOut|RestOut]) :-
	append(LConjList,[RConj],ConjOut),
	!,
	combine_conj_elements_aux(Rest,RestOut).

filter_by_role(_Role,[],_Analysis,[]) :- !.
filter_by_role(Role,[MSU|RestMSUs],Analysis,[MSU|RestMSUsOut]) :-
	get_role(MSU,Analysis,MSURole),
	MSURole == Role,
	!,
	filter_by_role(Role,RestMSUs,Analysis,RestMSUsOut).
filter_by_role(Role,[_MSU|RestMSUs],Analysis,MSUsOut) :-
	filter_by_role(Role,RestMSUs,Analysis,MSUsOut).

get_role(MSU,Analysis,subject) :-
	no_verbs_to_the_left(MSU,Analysis),
	!.
get_role(MSU,Analysis,subject) :-
	dependent_clause_subject(MSU,Analysis),
	!.
get_role(MSU,Analysis,subject) :-
	relative_clause_head(MSU,Analysis),
	!.
get_role(MSU,_Analysis,object) :-
	prepositional_object(MSU),
	!.
get_role(MSU,Analysis,object) :-
	verbal_object(MSU,Analysis),
	!.
get_role(_MSU,_Analysis,object).

no_verbs_to_the_left(_MSU,[]) :- !.
no_verbs_to_the_left(MSU,Analysis) :-
	rev(Analysis,RevAnalysis),
	get_left_partition_1(MSU,RevAnalysis,LeftPartition),
	no_verbal(LeftPartition),
	!.

dependent_clause_subject(MSU,Analysis) :-
	rev(Analysis,RevAnalysis),
	get_left_partition_1(MSU,RevAnalysis,LeftPartition),
	find_subordinator(LeftPartition,_Subordinator,RestPartition),
	append(InterveningStructure0,RestPartition,LeftPartition),
	last(InterveningStructure0,SubordinatorMSU),
	append(InterveningStructure,[SubordinatorMSU],InterveningStructure0),
	no_verbal(InterveningStructure),	
	!.

relative_clause_head(MSU,Analysis) :-
%	rev(Analysis,RevAnalysis),
	get_left_partition_1(MSU,Analysis,Rest),
	Rest =[First|_RestMSUs],
	check_for_relativizer([First]),
	!.

conjunct_with_prepositional_object(Analysis,MSU,ConjList) :-
	locate_npu_head([MSU],_Type,MSUHead),
	rev(Analysis,RevAnalysis),
	get_left_partition_1(MSU,RevAnalysis,Rest),
	get_other_conjuncts(ConjList,MSUHead,ConjunctHeadList),
	conjunct_with_prepositional_object_aux(Rest,ConjunctHeadList).

conjunct_with_prepositional_object_aux([MSU|RestMSUs],ConjunctHeadList) :-
	locate_npu_head([MSU],_Type,MSUHead),
	memberchk(MSUHead,ConjunctHeadList),
	RestMSUs = [NextMSU|_Rest],
	prepositional_object(NextMSU).
conjunct_with_prepositional_object_aux([_MSU|RestMSUs],ConjunctHeadList) :-
	conjunct_with_prepositional_object_aux(RestMSUs,ConjunctHeadList).
	
prepositional_object(MSU) :-
	memberchk(prep(PrepList),MSU),
	get_from_list(lexmatch,PrepList,[Prep]),
	% May consider with and probably others as well, 'the treatment with'
	memberchk(Prep,[of]),
	!.

verbal_object(MSU,Analysis) :-
	rev(Analysis,RevAnalysis),
	get_left_partition_1(MSU,RevAnalysis,LeftPartition),
	last(LeftPartition,LeftMSU),
	\+ no_verbal([LeftMSU]),
	!.

get_rightmost_npu_element([ThisNPU|_MoreNPUs], Type, RightMost) :-
	rev(ThisNPU, RevThisNPU),
	RevThisNPU = [RightMost|_],
	functor(RightMost,Type,1).


self_referencing(First,RestNP) :-
	append([First],RestNP,All),
	collect_bases(All,AllBasesList),
	append(AllBasesList,AllBases),
	( AllBases = [this,study]
	; AllBases = [the,present,study]
	; AllBases = [the,current,study]
	; AllBases = [this,paper]
	; AllBases = [this,article]
	).
	


%%%%% BASELINE METHODS %%%%
%% Can remove it eventually
identify_sortal_anaphora_baseline(minimal_syntax(AnalysisIn),VarInfoList,minimal_syntax(AnalysisOut)) :-
	identify_sortal_anaphora_baseline(AnalysisIn,VarInfoList,AnalysisOut).

identify_sortal_anaphora_baseline([],_VarInfoList,[]) :- !.
identify_sortal_anaphora_baseline([ThisMSU|RestMSUs],VarInfoList,[ThisMSUOut|RestMSUsOut]) :-
	identify_sortal_anaphora_aux_baseline(ThisMSU,0,VarInfoList,ThisMSUOut),
	!,
	identify_sortal_anaphora_baseline(RestMSUs,VarInfoList,RestMSUsOut).
identify_sortal_anaphora_baseline([_ThisMSU|RestMSUs],VarInfoList,RestMSUsOut) :-
	identify_sortal_anaphora_baseline(RestMSUs,VarInfoList,RestMSUsOut).

identify_sortal_anaphora_aux_baseline([],_,_,[]) :- !.
identify_sortal_anaphora_aux_baseline([MSUElement|Rest],SortalAnaphorFound,VarInfoList,
			     [MSUElementOut|RestOut]) :-
	( SortalAnaphorFound == 0 ->
	  sortal_anaphor_baseline(MSUElement)
	; \+ functor(MSUElement,punc,1)
	),
	!,
	functor(MSUElement, Functor, 1),
	arg(1,MSUElement,MSUElementList),
	append(MSUElementList,[sortalAnaphor],MSUElementListOut),
	functor(MSUElementOut,Functor,1),
	arg(1,MSUElementOut,MSUElementListOut),
	identify_sortal_anaphora_aux_baseline(Rest,1,VarInfoList,RestOut).
identify_sortal_anaphora_aux_baseline([MSUElement|Rest],SortalAnaphorFound,VarInfoList,
			     [MSUElement|RestOut]) :-
	identify_sortal_anaphora_aux_baseline(Rest,SortalAnaphorFound,VarInfoList,RestOut).

sortal_anaphor_baseline(Candidate) :-
	Candidate = det(CandidateList),
	!,
	get_from_list(lexmatch, CandidateList, LexList),
	lower_list(LexList,LLexList),
	intersection(LLexList, [this, that, the, these, those, such, either, neither, both, each],Lex),
	\+ Lex == [].


resolve_sortal_anaphora_if_necessary_baseline(AnaphorHead,ExcludeCandidates,PrecedingVarInfoList,
					      PrecedingAnalysis,PrecedingConjList,
					      VarInfoList,Analysis,ConjList,SemSpecAnalysis,
					      _Role,Number,AntecedentHead) :-
	locate_npu_from_head(Analysis,AnaphorHead,AnaphorNP),
	anaphoric(AnaphorNP),
	number(AnaphorHead,VarInfoList,Number),
	% Calling get_left_partition_1/3 instead of get_left_partition/3
	% because I think we don't want to check for seminterp barriers, but I might be
	% wrong. With semicolons, I can see get_left_partition/3 being a problem,
	% but section headers may present another issue that needs resolving.
	get_left_partition_1(AnaphorNP,Analysis,LeftPartition),
	( LeftPartition == [] ->
	  append([[]],PrecedingAnalysis,FullPrecedingAnalysis),
	  append([[]],PrecedingVarInfoList,FullVarInfoList),
	  append([[]],PrecedingConjList,FullConjList)
	; append([LeftPartition],PrecedingAnalysis,FullPrecedingAnalysis),
	  %% SHOULD REALLY BE LEFT PARTITION VARINFO ONLY, DOES IT MATTER?
	  append([VarInfoList],PrecedingVarInfoList,FullVarInfoList),
	  get_from_list(index,AnaphorHead,Index),
	  get_left_conj_list(ConjList,Index,LeftConjList),
	  append([LeftConjList],PrecedingConjList,FullConjList)
	),
	combine_conj_elements(FullConjList,CombinedConjList),
	combine_conj_elements([ConjList],[_CSentenceConjList]),
%	\+ conjunct_with_prepositional_object(Analysis,AnaphorNP,CSentenceConjList),
	get_consonant_NP_baseline(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,FullVarInfoList,
			  FullPrecedingAnalysis,CombinedConjList,
			  SemSpecAnalysis,VarInfoList,AntecedentNP),
	format(user_output,'Consonant NP: ~q~n',[AntecedentNP]),
	locate_npu_head([AntecedentNP],_Type,AntecedentHead).
resolve_sortal_anaphora_if_necessary_baseline(AnaphorHead,_Exclude,_PrecedingVarInfoList,
				     _PrecedingAnalysis,_PrecedingConjList,
				     _VarInfoList,_Analysis,_ConjList,_SemSpecAnalysis,
				     _Role,_Number,AnaphorHead).


get_consonant_NP_baseline(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,[SentenceVarInfoList|_RestVarInfoList],
		  [SentenceAnalysis|_RestAnalysis],[SentenceCConjList|_RestCConjList],
		  SemSpecAnalysis,VarInfoList,ConsonantNP) :-
	get_consonant_NP_aux_baseline(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,
			      SentenceVarInfoList,SentenceAnalysis,SentenceCConjList,
			      SemSpecAnalysis,VarInfoList,ConsonantNP),
	!.
get_consonant_NP_baseline(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,[_SentenceVarInfoList|RestVarInfoList],
		  [_SentenceAnalysis|RestAnalysis],[_SentenceCConjList|RestCConjList],
		  SemSpecAnalysis,VarInfoList,ConsonantNP) :-	
	get_consonant_NP_baseline(Number,AnaphorNP,Analysis,ConjList,ExcludeCandidates,RestVarInfoList,
			  RestAnalysis,RestCConjList,SemSpecAnalysis,VarInfoList,ConsonantNP).

get_consonant_NP_aux_baseline(_Number,AnaphorNP,_Analysis,_ConjList,_ExcludeCandidates,SentenceVarInfoList,
		      [CandidateNP|_RestAnalysis],_SentenceCConjList,
		      _SemSpecAnalysis,VarInfoList,CandidateNP) :-
	locate_npu_head([AnaphorNP],_AnaType,AnaphorHeadList),
	locate_npu_head([CandidateNP],_CandType,CandidateHeadList),
	consonance_baseline(AnaphorHeadList,CandidateHeadList,VarInfoList,SentenceVarInfoList),
	!.
get_consonant_NP_aux_baseline(Number,AnaphorNP,ConjList,Analysis,ExcludeCandidates,
		      SentenceVarInfoList,[_CandidateNP|RestAnalysis],SentenceCConjList,
		      SemSpecAnalysis,VarInfoList,RestConsonantNPs) :-
	get_consonant_NP_aux_baseline(Number,AnaphorNP,ConjList,Analysis,ExcludeCandidates,
			      SentenceVarInfoList,RestAnalysis,SentenceCConjList,
			      SemSpecAnalysis,VarInfoList,RestConsonantNPs).

consonance_baseline(HeadListA, HeadListB,VarInfoListA,VarInfoListB) :-
	number_agreement(HeadListA,HeadListB,VarInfoListA,VarInfoListB),
	get_from_list(bases,HeadListA,BasesA),
	get_from_list(bases,HeadListB,BasesB),
	last(BasesA,BaseA),
	last(BasesB,BaseA),
	get_from_list(inputmatch,HeadListA,AIMList),
	get_from_list(inputmatch,HeadListB,BIMList),
	length(AIMList,AIMLen),
	length(BIMList,BIMLen),
	BIMLen > AIMLen,
	!.
