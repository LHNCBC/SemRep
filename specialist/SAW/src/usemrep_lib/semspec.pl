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

% File:     semspec.pl
% Module:   semspec
% Author:   MF, NLS
% Purpose:  Purpose:  semantic specification of medical concepts.
% It now includes all semantic groups. For that, it uses a module
% called semgroups that can be generated automatically from semantic group files. 


% ----- Module declaration and exported predicates

:- module(semspec, [
	count_parens_in_one_MSU/3,
	count_parens_in_MSU_list/3,
	must_be_appositives/2,
	semantic_specification/8
   ]).

:- load_files( usemrep_lib(module_version), [
	when(compile_time)
   ]).

% ----- Imported predicates

:- use_module( library(lists), [
	last/2,
	nextto/3,
	rev/2
   ]).

:- use_module( library(sets), [
	intersection/3
   ]).

:- use_module( skr_lib(sicstus_utils), [
	concat_atom/2,
	lower/2,
	midstring/5
   ]).

:- use_module( usemrep_lib(mancmod), [
	meta_ancestors/3
   ]).

:- use_module( usemrep_domain(domain_processing), [
	domain_specific_semgroup_member/3
   ]).

:- use_module( usemrep_lib(seminterp), [
	check_for_neg/5
   ]).

:- use_module( usemrep_lib(ssuppserv), [
	get_base/4,
	get_left_partition/3,
	get_sem_info/4,
	get_conjunct_data/9,
	left_parenthesis/1,
	npu_has_conjunct/6,
	right_parenthesis/1
   ]).
                                               
:- use_module( skr_lib(nls_lists), [
		get_from_list/3,
		get_from_list_nd/3
   ]).

:- use_module( skr_lib( nls_strings ), [
	split_string_completely/3
   ]).

:- use_module( skr_db(db_access), [
        db_get_concept_cui/2
   ]).

% See section H-1-6-1 of the Quintus Prolog manual about the load_files/2 directive below.
% This was complicated and unexpected, at least to FML. 01/27/2004 Tuesday @ 12:55:14

% Whenever qpc is used to compile a file, such as the current file isemtype.pl,
% any code called by that file that helps compile the application,
% such as compile_semgroups/0, must be separated out into its own file,
% such as semgroup_member.pl. This separated file (semgroup_member.pl)
% must then be loaded into the file that needs it (isemtype.pl)
% via a load_files/2 directive.

% The directive after this comment will
% (1) create the file compiled_semgroups.pl containing unit clauses of the form
% 
% semgroup_member(acty,acti).
% semgroup_member(bhvr,acti).
% semgroup_member(dora,acti).
% semgroup_member(evnt,acti).
% semgroup_member(gora,acti).
% semgroup_member(inbe,acti).
% semgroup_member(mcha,acti).
% semgroup_member(ocac,acti).
% semgroup_member(socb,acti)
% 
% to allow reverse access to Semantic Group names, and
% (2) compile that file.

% :- load_files(usemrep_lib(compile_semgroup_member), [
% 	when(compile_time)
%    ]).

/*
Semantic_Specification called from Semrep

Returns the first ISA relationship found in a sentence in a list form.
If no ISA relationship is found returns the empty list.

To determine ISA relationship Semantic_Specification looks for appropriate patterns 
that are searched by find_pattern and then submits then to the metatsaus to check consistency 
of hierarchical relationship.

Needs to be expanded to include more ISA rels in the list and more Tst types

*/

semantic_specification(generic,
		       minimal_syntax(MSUList),Definitions,
		       VarInfoList, ConjList,
		       IsaPredListIn, IsaPredListOut, Domain) :-
	!,
	% To have difference lists, unify IsaPredList and IntraIsaPredList
	% and pass IntraIraPredListTail into semantic_specification_inter
	semantic_specification_INTRA(MSUList, IsaPredListIn, IsaPredListNext, Domain),
	% IntraIsaPredListTail = [], % for the time being, don't use the fact that it's a diff list
	semantic_specification_INTER(MSUList, Definitions, VarInfoList,
				     ConjList, IsaPredListNext, IsaPredListOut, Domain),
	!.
semantic_specification(_Domain, _Syntax, _Definitions,
		       _VarInfoList, _ConjList,
		       IsaPredList, IsaPredList, _Domain).

% -----Intra NP
% catches appositives, e.g., "treatment with the analgesic aspirin"
semantic_specification_INTRA([], PredList, PredList, _Domain).
semantic_specification_INTRA([MSU|More], IsaPredListIn, IsaPredListOut, Domain) :-
	% "the analgesic aspirin" will generate [ ... mod(analgesic), head(aspirin) ... ]
	nextto(mod(ModifierList), head(HeadList), MSU),
	% According to Marcelo, the idea here is to ensure that the SemType lists of
	% Modlist and Headlist each contain an element of the same semantic group,
	% e.g., one list contains dsyn, and the other sosy, because both are
	% part of the Semantic Group diso.
	msus_share_common_semantic_group(Domain,
					 ModifierList,        HeadList,
					 ModifierSemTypeList, HeadSemTypeList,
					 ModifierSemType,     HeadSemType,
					 GroupName),
	GroupName \== anat,  % Must exclude meronomies
	GroupName \== conc, % Mostly FPs
	
	get_mod_and_head_metaconc_names(ModifierList,    ModifierSemTypeList,
					HeadList,        HeadSemTypeList,
					ModifierConcept, HeadConcept,
					ModifierCUI,     HeadCUI),
%	get_concept_cui(ModifierConcept, ModifierCUI),
%	get_concept_cui(HeadConcept,     HeadCUI),
	ModifierCUI \== HeadCUI,
	meta_ancestors(ModifierCUI, HeadCUI, SpecGenAtom),
	SpecGenAtom \== 'None.',
	\+ symptoms_as_head(HeadCUI,HeadConcept,HeadSemTypeList),
	!,
	get_from_list(index, ModifierList, ModifierIndex),
	get_from_list(index, HeadList,     HeadIndex),
	get_genspec_concepts_ModHead(SpecGenAtom, 
				     ModifierIndex,ModifierCUI,ModifierConcept,
				     ModifierSemTypeList, ModifierSemType,
				     HeadIndex, HeadCUI, HeadConcept,
				     HeadSemTypeList,     HeadSemType,
				     SpecificIndex, SpecificCUI,SpecificConcept,
				     SpecificSemTypeList, SpecificSemType,
				     GeneralIndex, GeneralCUI, GeneralConcept,
				     GeneralSemTypeList,  GeneralSemType),
	IsaPredListIn = [0-0-SpecificIndex-
			SpecificConcept-SpecificCUI-SpecificSemTypeList-SpecificSemType-
	                'MOD/HEAD'-'ISA'-(ModifierIndex/HeadIndex)-
			0-0-GeneralIndex-
			GeneralConcept-GeneralCUI-GeneralSemTypeList-GeneralSemType
			|IsaPredListNext],
	semantic_specification_INTRA(More, IsaPredListNext, IsaPredListOut, Domain).

semantic_specification_INTRA([_NoIsaPred|More], IsaPredListIn, IsaPredListOut, Domain) :-
	semantic_specification_INTRA(More, IsaPredListIn, IsaPredListOut, Domain).


symptoms_as_head('C1457887','Symptoms',[sosy]).

% -----Inter NP
% catches explicit ISAs, e.g., "aspirin is an analgesic"
% semantic_specification_INTER([], _, _, _, PredList, PredList, _Domain).
semantic_specification_INTER([], _, _, _, PredList, PredList, _Domain).
semantic_specification_INTER([MSU|RestMSUs],  Definitions, 
	                     VarInfoList, ConjList, 
			     IsaPredListIn, IsaPredListOut, Domain) :-
	find_next_matching_MSU(RestMSUs,
			       MSU,                 MatchingMSU,
			       SemTypeList,         SemType,
			       MatchingSemTypeList, MatchingSemType,
			       SemGroup, MatchingSemGroup,
			       Distance, IntSentList, Remainder, Domain),
	\+ mismatched_parenthesis(Distance, MatchingMSU, RestMSUs),
	% The distance must be =< 5.
	Distance =< 5,
	validate_MSU_distance(Distance, MSU, MatchingMSU, IntSentList, Definitions, VarInfoList),
	nps_are_not_coordinate(MSU,MatchingMSU,ConjList),
	get_metaconcs_matching_semtypes(MSU,         SemTypeList,         SemType,
					MatchingMSU, MatchingSemTypeList, MatchingSemType,
					MetaConc, MatchingMetaConc,
					CUI, MatchingCUI),
%	get_concept_cui(MetaConc, CUI),
%	get_concept_cui(MatchingMetaConc, MatchingCUI),
	
	CUI \== MatchingCUI,
	meta_ancestors(CUI, MatchingCUI, SpecGenAtom),
	SpecGenAtom \== 'None.',
%	!,
%	( must_be_appositives(MSU, MatchingMSU) ->
%	  true
%	; concat_atom([CUI, ' ', MatchingCUI], ConcatenatedCUIs),
%	  SpecGenAtom == ConcatenatedCUIs
%	),
	get_from_list(head,  MSU,             MSUHead),
	get_from_list(index, MSUHead,         Index),
	get_from_list(head,  MatchingMSU,     MatchingMSUHead),
	get_from_list(index, MatchingMSUHead, MatchingIndex),
	get_genspec_concepts(SpecGenAtom,

			     MSU, Index, MetaConc, CUI,
			     SemTypeList, SemType, SemGroup,

			     MatchingMSU, MatchingIndex, MatchingMetaConc,MatchingCUI,
			     MatchingSemTypeList, MatchingSemType, MatchingSemGroup,

			     SpecificMSU, SpecificIndex, SpecificMetaConc, SpecificCUI,
			     SpecificSemTypeList, SpecificSemType, _SpecificSemGroup,

			     GeneralMSU, GeneralIndex, GeneralMetaConc, GeneralCUI,
			     GeneralSemTypeList, GeneralSemType, _GeneralSemGroup
			    ),
	allowed_geoa(SpecificSemType,GeneralSemType,GeneralMetaConc),
	!,

	% format('~n~ninter: ~w~n~n', [IsaPredListIn]),
	get_from_list(head, SpecificMSU, SpecificHeadList),
	get_from_list(head, GeneralMSU,  GeneralHeadList),

%%%	deal_with_coordination(SpecificHeadList, GeneralHeadList, ConjList,
%%%			       'ISA', Index/MatchingIndex,
%%%                               SpecificMetaConc, SpecificSemType, SpecificIndex,
%%%			       GeneralMetaConc,  GeneralSemType,  GeneralIndex,
%%%                               CoordPreds0, _LConjList, _RConjHeadList, Gap),

	intervening_negation(IntSentList,IntSentList,Definitions,Env),
	( Env == neg_env ->
	  Relation = 'NEG_ISA'
	; Relation = 'ISA'
	),
	deal_with_coordination(Relation, Relation, Index/MatchingIndex, Domain,

			       SpecificHeadList,
			       SpecificSemTypeList, SpecificSemType,
			       SpecificMetaConc,
			       SpecificCUI,
			       SpecificIndex,

			       GeneralHeadList,
			       GeneralSemTypeList, GeneralSemType,
			       GeneralMetaConc,
			       GeneralCUI,
			       GeneralIndex,

			       ConjList,
			       [], [],
			       IsaPredListIn, IsaPredListNext),
	% append(TempList1, CoordPreds, TempList2),
	semantic_specification_INTER(Remainder,Definitions, VarInfoList,
				     ConjList, IsaPredListNext, IsaPredListOut, Domain).
semantic_specification_INTER([_MSU|RestMSUs],Definitions, VarInfoList,
			     ConjList, IsaPredListIn, IsaPredListOut, Domain) :-
    semantic_specification_INTER(RestMSUs, Definitions, VarInfoList,
				 ConjList, IsaPredListIn, IsaPredListOut, Domain).

	
% From Bugzilla #25.
% Simplified example: "visual acuity was (perception)".
% We want to block the predication
% relation|C0042812|Visual Acuity|clna|clna|||ISA|C0030971|Perception|menp|menp||
% if the two arguments are separated by an unmatched left/right parenthesis, i.e.,
% punc([inputmatch(['(']),tokens([]),position(18,19),bases(['('])])
% or
% punc([inputmatch([')']),tokens([]),position(18,19),bases([')'])]).

% How to actually do this? We must count parens, starting from 0.
% Each left paren does ++Count and each right paren does --Count.
% If Count ever goes negative, fail immediately, because that means
% that the first MSU was itself inside parentheses.
% When we're done, the final Count must be zero.

% Case 1: Ensure that no unmatched left/right parenthesis element
% appears in the MatchingMSU structure before its head.
% count_parens_in_one_MSU(MatchingMSU, StartingParenCount, FinalParenCount1)
% This handles the example above ("visual acuity was (perception)")
% in which a paren(_) structure is actually in the MatchingMSU:
% MatchingMSU = [confid(1000),
% 		 punc([inputmatch(['(']),tokens([]),position(18,19),bases(['('])]),
% 		 head([index(4),usemtype(['Mental Process']),ausemtype([menp]),
% 		 semgroup([phys]),lexmatch([perception]),
% 		 inputmatch([perception]),tag(noun),tokens([perception]),
% 		 metaconc(['Perception':'C0030971':[menp]]),
% 		 position(19,29),bases([perception])])]

% no_mismatched_parenthesis(MatchingMSU, RestMSUs) :-
% 	StartingParenCount is 0,
% 	count_parens_in_one_MSU(MatchingMSU, StartingParenCount, FinalParenCount1),
% 	FinalParenCount1 is 0,
% 	% Split RestMSUs into FrontMSUs and BackMSUs such that
%         % FrontMSUs contains all MSUs before MatchingMSU
%         append(FrontMSUs, [MatchingMSU|_RestBackMSUs], RestMSUs),
% 	!,
% 	count_parens_in_MSU_list(FrontMSUs, StartingParenCount, FinalParenCount2),
% 	FinalParenCount2 is 0.

mismatched_parenthesis(Distance, MatchingMSU, RestMSUs) :-
	Distance > 1,
	StartingParenCount is 0,
	% Does MatchingMSU contains an unbalanced paren?
	count_parens_in_one_MSU(MatchingMSU, StartingParenCount, FinalParenCount1),
	( FinalParenCount1 =\= 0 ->
	  true
	; % Split RestMSUs into FrontMSUs and BackMSUs such that
	  % FrontMSUs contains all MSUs before MatchingMSU
	  append(FrontMSUs, [MatchingMSU|_RestBackMSUs], RestMSUs),
	  !,
	  count_parens_in_MSU_list(FrontMSUs, StartingParenCount, FinalParenCount2),
	  FinalParenCount2 =\= 0
	).

% Case 2: Ensure that no unmatched left/right parenthesis element appears in RestMSUs before MatchingMSU.
% count_parens_in_MSU_list(FrontMSUs, StartingParenCount, FinalParenCount2),
% This handles a similar case "visual acuity was (the perception)"
% in which the paren structure is NOT part of the MatchingMSU,
% but does appear before MatchingMSU in RestMSUs:
% RestMSUs  = [[aux([index(3),lexmatch([was]),inputmatch([was]),tag(aux),
% 		   tokens([was]),position(14,17),bases([be])])],
% 	       [punc([inputmatch(['(']),tokens([]),position(18,19),bases(['('])])],
% 	       [confid(1000),det([index(4),lexmatch([the]),inputmatch([the]),tag(det),
% 				  tokens([the]),position(19,22),bases([the]),sortalAnaphor]),
% 	        head([index(5),usemtype(['Mental Process']),ausemtype([menp]),
% 		      semgroup([phys]),lexmatch([perception]),inputmatch([perception]),
% 		      tag(noun),tokens([perception]),metaconc(['Perception':'C0030971':[menp]]),
% 		      position(23,33),bases([perception]),sortalAnaphor])],
% 	       [punc([inputmatch([')']),tokens([]),position(33,34),bases([')'])])]]

% count_parens_in_MSU_list/3 counts the number of parentheses occurring in the MSU list:
% Increase the count by 1 for each left paren, decrease the count by 1 for each right paren.
% If the count is ever negative, fail immediately.

count_parens_in_MSU_list([], FinalParenCount, FinalParenCount).
count_parens_in_MSU_list([FirstMSU|RestMSUs], ParenCountIn, ParenCountOut) :-
	count_parens_in_one_MSU(FirstMSU, ParenCountIn, ParenCountNext),
	count_parens_in_MSU_list(RestMSUs, ParenCountNext, ParenCountOut).

count_parens_in_one_MSU([], ParenCount, ParenCount).
count_parens_in_one_MSU([FirstElement|RestElements], ParenCountIn, ParenCountOut) :-
	maybe_change_paren_count(FirstElement, ParenCountIn, ParenCountNext),
	count_parens_in_one_MSU(RestElements, ParenCountNext, ParenCountOut).

maybe_change_paren_count(Element, ParenCountIn, ParenCountNext) :-
	( Element = punc(PuncFeatureList),
	  get_from_list(inputmatch, PuncFeatureList, InputMatch) ->
	  change_paren_count_for_paren(InputMatch, ParenCountIn, ParenCountNext)
	; ParenCountNext is ParenCountIn
	).

change_paren_count_for_paren([PuncChar], ParenCountIn, ParenCountNext) :-
	( left_parenthesis(PuncChar) ->
	  ParenCountNext is ParenCountIn + 1
	; right_parenthesis(PuncChar) ->
	  ParenCountNext is ParenCountIn - 1
	; ParenCountNext is ParenCountIn
	),
	!,
	ParenCountNext >= 0.

% If ParenCountNext is negative, that means we're closing off a parenthesized expression
% that was opened before we started counting, and that means failure.

% If the distance is exactly 1, then MSU and MatchingMSU must be appositives.
validate_MSU_distance(1, MSU, MatchingMSU, _IntSentList, _Definitions, _VarInfoList) :-
	!,
	must_be_appositives(MSU, MatchingMSU).
% If the distance is > 1, there must be an intervening "Be" or a verb
validate_MSU_distance(_Distance, _MSU, _MatchingMSU, IntSentList, Definitions, VarInfoList) :-
	find_intervening_BeOrVerbs(IntSentList, Definitions, VarInfoList),
	!.
% If the distance is 2, then an "Other" pattern must be found
validate_MSU_distance(2, _MSU, MatchingMSU, _IntSentList, _Definitions, _VarInfoList) :-
	!,
	find_Other_pattern(MatchingMSU).

/*
   find_next_matching_MSU (formerly find_any_pattern)
   called from semantic specification with Tst
   (Semantic Type of Head) already set to something.
   Find MSU and NextMSU that matches a given Tst, the distance between them.
   Used together with find_next_MSU.

   It will fail if the match is not found.
*/
  
% Find the next MSU in the list whose SemGroupList overlaps the SemGroupList
% of the first MSU; the intersection must be nonempty and *not* include "anat".
find_next_matching_MSU(RestMSUs, FirstMSU, MatchingMSU,
		       SemTypeList,         SemType,
		       MatchingSemTypeList, MatchingSemType,
		       SemGroup,            MatchingSemGroup,
		       Distance, RevList, [MatchingMSU|Remainder],
		       Domain) :-
	% get the semgroup(_) term from the MSU
	get_semgroup_list(FirstMSU, SemGroupList),
	% ensure that 'anat' is not in the semgroup list
	\+ memberchk(anat, SemGroupList),
	\+ memberchk(conc, SemGroupList),
	% nondeterministically generate one semgroup in the list
	member(SemGroup, SemGroupList),
	% nondeterministically generate one of the semtypes from the MSU
	checkSemType(FirstMSU, SemType, SemTypeList),
	% ensure the SemType and SemGroup match
	domain_specific_semgroup_member(Domain, SemType, SemGroup),
	find_next_matching_MSU_1(RestMSUs, MatchingMSU, SemGroupList,
				 MatchingSemTypeList, MatchingSemType, MatchingSemGroup,
				 RevList, Remainder, 1, Distance, Domain ).

find_next_matching_MSU_1([MatchingMSU|RestMSUs], MatchingMSU, SemGroupList,
			 MatchingSemTypeList, MatchingSemType, MatchingSemGroup,
			 [MatchingMSU], RestMSUs, Distance, Distance, Domain ) :-
	% get the semgroup(_) term from the MatchingMSU
	get_semgroup_list(MatchingMSU, MatchingSemGroupList),
	% ensure that 'anat' is not in the semgroup list
	\+ memberchk(anat, MatchingSemGroupList),
	\+ memberchk(conc, MatchingSemGroupList),
	% make sure the MSU's semgroup list and the MatchingMSU's semgroup list intersect...
	intersection(SemGroupList, MatchingSemGroupList, [Int1|RestInt]),
	% nondeterministically generate one semgroup in the intersection
	member(MatchingSemGroup, [Int1|RestInt]),
	% nondeterministically generate one of the semtypes from the MSU
	checkSemType(MatchingMSU, MatchingSemType, MatchingSemTypeList),
	% ensure the MatchingSemType and MatchingSemGroup match
	domain_specific_semgroup_member(Domain, MatchingSemType, MatchingSemGroup).

find_next_matching_MSU_1([NonMatchingMSU|RestMSUs], MatchingMSU, SemGroupList,
			 MatchingSemTypeList, MatchingSemType, MatchingSemGroup,
			 [NonMatchingMSU|RestRev], Remainder, CurrentDistance, Distance, Domain ) :-
	NextDistance is CurrentDistance + 1,
	find_next_matching_MSU_1(RestMSUs, MatchingMSU, SemGroupList,
				 MatchingSemTypeList, MatchingSemType, MatchingSemGroup,
				 RestRev, Remainder, NextDistance, Distance, Domain ).
/*
Below are some allowable syntatic patterns:

Pattern 1  = Apositives (MSU, NextMSU,)  (where Tst is head of those two MSUs) 
Pattern 1a = Parenthesis (MSU (NextMSU)) (where Tst is head of those two MSUs)
Pattern 1b = cues like such as, including  etc (MSU such as NexMSU)
             (where Tst is head of those two MSUs)
Pattern 2  = NPs that have the  base form of Be intervening  (MSU.. Be.. NextMSU)
             (Tst is the head of those two MSUs)
Pattern 3  = NPs that have some specific Verb intervening (MSU..Verb..NextMSU)
             (where Tst is the Head of those two MSUs) 
Pattern 4  = The other cue to hypernyms (MSU and|or other NextMSU)
             (where Tst is the Head of those two MSUs)

Name of predicates. There are some auxiliary predicated used
find_appositive is Pattern1 or Pattern2 distance betweeb MSU and NextMSU is 1
find_intervening_BeOrVerbs  is Pattern 3 or 4 distance between MSU and
NextMSU is >1 an =<4 but could be changed to allow more.
Now changed to allow verb be but not be abd past participle
find_Other_pattern   MSU, and|or other NextMSU distance between MSU and NextMSU is 2
Needs to be expanded to include more patterns.

*/

must_be_appositives(MSU, NextMSU) :-
	( check_comma(MSU),
	  check_comma(NextMSU) ->
	  true
	; check_parenthesis(NextMSU) ->
	  true
	; check_cues(NextMSU)
	),
	!.

/*
 Find intervening BE or Berbs.
 Be cannot have a past participle after it.
 VarInfoList has augmented the arity of semspec to 5 and is used in this prediacte
*/

find_intervening_BeOrVerbs([],_,_) :- !, fail.
find_intervening_BeOrVerbs([AuxOrVerbMSU|More], Definitions,VarInfoList) :-
   ( (check_aux(AuxOrVerbMSU, Definitions),
      More  = [NextMsu|_],
      \+pastpart(NextMsu,VarInfoList))
      ;
      (check_aux(AuxOrVerbMSU, Definitions),
      More  = [NextMsu|Rest],
      pastpart(NextMsu, VarInfoList),
      Rest = [AsMsu|_],
      check_as(AsMsu))
      ;
      check_verb(AuxOrVerbMSU, Definitions)
   ) ,!.
find_intervening_BeOrVerbs([_nomatch|More], Definitions, VarInfoList) :-
    find_intervening_BeOrVerbs(More, Definitions, VarInfoList).  


/*
%   Old Find intervening verbs this one did not take care of BE followed by
%   verbs in past participle it did not require VarInfoList.
%   VarInfoList has augmented the arity of semspec to 5
%
% find_intervening_BeOrVerbs([],_) :- !, fail.
% find_intervening_BeOrVerbs([AuxOrVerbMSU|_], Definitions) :-
%   (check_aux(AuxOrVerbMSU, Definitions)
%    ;
%    check_verb(AuxOrVerbMSU, Definitions)), !.
% find_intervening_BeOrVerbs([_nomatch|More], Definitions) :-
%    find_intervening_BeOrVerbs(More, Definitions).  
*/


/*
Find the other pattern in the second NP.
It nust be at a distance of 2 [MSU] [and|or] [other NP] Other NP is NextMSU
*/

find_Other_pattern(OtherNP) :-
    get_from_list(det,OtherNP , OtherList),
    get_from_list(lexmatch, OtherList, ['other']), !.


intervening_negation([],_IntSentList,_Definitions,pos_env):- !.
intervening_negation([MSU|_More], IntSentList, Definitions,neg_env) :-
    rev(IntSentList,RevIntSentList),	
    check_for_neg(pos_env,MSU,Definitions,RevIntSentList,neg_env),
    \+ but_not(MSU,IntSentList),
    !.
intervening_negation([_nomatch|More], IntSentList, Definitions, Env) :-
    intervening_negation(More, IntSentList, Definitions, Env).


but_not(ThisMSU,IntSentList) :-
	get_from_list(adv, ThisMSU, AdvList),
	nextto(PrevMSU,ThisMSU,IntSentList),
	get_from_list(conj, PrevMSU, ConjList),
	get_from_list(lexmatch, ConjList, [but]),
	get_from_list(lexmatch, AdvList, [not]),
	!.


/* pastpart predicate 
returns true if MSU is a verb and is allowed to be in the past participle.
Important to nore that it requires VarInfoList that comes from semrep.
The arity of semspec had to be changed.
VarinfoList lists al the possibilities for the verb; it doesn't mean that
the verb is really instantiated in the past participle.
This predicates looks only for past participle in the verb MSU.
*/  


pastpart(MSU, _VarInfoList) :-   
    get_from_list(pastpart, MSU, _VerbList), !.
pastpart(MSU, VarInfoList) :-   
    get_from_list(verb, MSU, VerbList),
    get_from_list(lexmatch, VerbList, Lexmatch),
    [Word] = Lexmatch,
    get_from_list(Word, VarInfoList, Inflection),
    memberchk( verb:[pastpart], Inflection), !.

/* verb  predicate
% Returns true  for if  MSU is a verb.
% It might be important just be a verb and not require a past participle.
% I will keep this predicate here in case we decide to use it instead of the above pastpart.
% 
%  verb(MSU) :-
%     get_from_list(verb, MSU, VerbList),
%     get_from_list(tag, VerbList, 'verb').
*/

get_semgroup_list(MSU, SemGroupList) :-
	get_from_list(head, MSU, Head),
%	( get_from_list(head, MSU, Head)
%	; get_from_list(empty_head, MSU,Head)
%	),
	get_from_list(semgroup, Head, SemGroupList).

% Check if Semtype o fSemType matches head of MSU, or generate the 
checkSemType(MSU, SemType, SemTypeList) :-
	get_from_list(head, MSU, HeadList),
%	( get_from_list(head, MSU, HeadList)
%	; get_from_list(empty_head, MSU,HeadList)
%	),
	get_sem_info(HeadList, _ConcList, _CUI, SemTypeList),
	% should this be member/2 or memberchk/2?
	member(SemType, SemTypeList).

% Check if MSU is of type aux and a base form of BE
% AuxMSU = [aux([index(2),lexmatch([is]),inputmatch([is]),tag(aux),tokens([is])])]
% % get_base( +Word, +Cat, +Definitions, -Base )
check_aux(MSU, Definitions) :-
	get_from_list(aux, MSU, AuxList),
	get_from_list(lexmatch, AuxList, Lexmatch),
	[Word] = Lexmatch,
	get_base(Word, aux, Definitions, Base),
	Base == be.

% Check if MSU is of type Verb and a base form of a lsit of potential verbs
% This list of verbs only contains remain, but will be increased as we find more verbs
% % get_base( +Word, +Cat, +Definitions, -Base )
check_verb(MSU, Definitions) :-
	get_from_list(verb, MSU, VerbList),
	get_from_list(lexmatch, VerbList, Lexmatch),
	[Word] = Lexmatch,
	get_base(Word, verb, Definitions, Base),
	memberchk(Base,[remain]).

% Check if there is comma in the end of MSU 
check_comma(MSU) :-
     % rev(MSU, RevMSU),
     get_from_list_nd(punc, MSU, HeadList),
     get_from_list(inputmatch, HeadList, [',']).

% Check if  NextMSU is a parenthesized structure
check_parenthesis(MSU) :-
     get_from_list(punc, MSU, HeadList),
     get_from_list(inputmatch, HeadList, ['(']).
     %The second parenthesis is not on the List ask tom why ?
     %rev(MSU, MSUInv),
     %get_from_list(punc, MSUInv, HeadList),
     %get_from_list(inputmatch, HeadList, [')']).

% Check lexical cues
check_cues(MSU) :-
     ( get_from_list(prep,  MSU , PrepList) ->
       get_from_list(lexmatch, PrepList, ['such as'])
     ; get_from_list(adv,  MSU , AdvList) ->
       get_from_list(lexmatch, AdvList, ['particularly'])
     ; get_from_list(prep,  MSU , PrepList) ->
       get_from_list(lexmatch, PrepList, ['including'])
     ),
     !.
     
% Check as
check_as(MSU) :-
	get_from_list(prep,  MSU , PrepList),
	get_from_list(lexmatch, PrepList, ['as']),
	!.

% Get head metaconcepts (MetaConc1 and 2)  from two identified NPs (First NP, NP2).
get_metaconcs_matching_semtypes(MSU,         SemTypeList,         SemType,
				MatchingMSU, MatchingSemTypeList, MatchingSemType,
				MetaConc, MatchingMetaConc,
				CUI, MatchingCUI) :-
	get_from_list(head, MSU, MSUHead),
	get_from_list(metaconc, MSUHead, MetaConcTerm),
	get_metaconc_name_and_semtype_list(MetaConcTerm, MetaConc, CUI, SemTypeList),
	memberchk(SemType, SemTypeList),
	get_from_list(head, MatchingMSU, MatchingMSUHead),
	get_from_list(metaconc, MatchingMSUHead, MatchingMetaConcTerm),
	get_metaconc_name_and_semtype_list(MatchingMetaConcTerm, MatchingMetaConc, MatchingCUI, MatchingSemTypeList),
	memberchk(MatchingSemType, MatchingSemTypeList),
	!.

% Get head metaconcepts (Metaconc1 and 2) from two
% identified Head Lists (FirstHeadList, SecondHedaList). 
get_mod_and_head_metaconc_names(ModifierList, ModifierSemTypeList,
				HeadList, HeadSemTypeList,
				ModifierConcept, HeadConcept,
			        ModifierCUI, HeadCUI) :-
	get_from_list(metaconc, ModifierList, Metaconc1),
	get_metaconc_name_and_semtype_list(Metaconc1, ModifierConcept, ModifierCUI, ModifierSemTypeList),
	get_from_list(metaconc, HeadList, Metaconc2),
	get_metaconc_name_and_semtype_list(Metaconc2, HeadConcept, HeadCUI, HeadSemTypeList),
	!. % need cut here because some predicates above are non-deterministic

% Depending on whether the first and second CUIs are
% specific and general, respectively, or vice versa,
% instantiate the Specific and General MetaConcs, MSUs, and SemGroups appropriately
get_genspec_concepts(SpecGenAtom, 

		     MSU, Index, MetaConc, CUI,
		     SemTypeList, SemType, SemGroup,

		     MatchingMSU, MatchingIndex, MatchingMetaConc,MatchingCUI,
		     MatchingSemTypeList, MatchingSemType, MatchingSemGroup,

		     SpecificMSU, SpecificIndex, SpecificMetaConc, SpecificCUI,
		     SpecificSemTypeList, SpecificSemType, SpecificSemGroup,

		     GeneralMSU, GeneralIndex, GeneralMetaConc, GeneralCUI,
		     GeneralSemTypeList,  GeneralSemType, GeneralSemGroup
		     				     ) :-
	midstring(SpecGenAtom, ConcCUISpec, _, 0, 8),
	( ConcCUISpec      == CUI ->

	  SpecificMSU          = MSU,
	  GeneralMSU           = MatchingMSU,

	  SpecificIndex        = Index,
	  GeneralIndex         = MatchingIndex,
	  SpecificCUI          = CUI,
	  GeneralCUI           = MatchingCUI,
	  SpecificMetaConc     = MetaConc,
	  GeneralMetaConc      = MatchingMetaConc,

	  SpecificSemTypeList  = SemTypeList,
	  SpecificSemType      = SemType,
	  GeneralSemTypeList   = MatchingSemTypeList,
	  GeneralSemType       = MatchingSemType,

	  SpecificSemGroup     = SemGroup,
	  GeneralSemGroup      = MatchingSemGroup

        ; SpecificMSU          = MatchingMSU,
	  GeneralMSU           = MSU,

	  SpecificIndex        = MatchingIndex,
	  GeneralIndex         = Index,
	  
	  SpecificCUI          = MatchingCUI,
	  GeneralCUI           = CUI,
	  SpecificMetaConc     = MatchingMetaConc,
	  GeneralMetaConc      = MetaConc,

	  SpecificSemTypeList  = MatchingSemTypeList,
	  SpecificSemType      = MatchingSemType,
	  GeneralSemTypeList   = SemTypeList,
	  GeneralSemType       = SemType,

	  SpecificSemGroup     = MatchingSemGroup,
	  GeneralSemGroup      = SemGroup
        ).

get_genspec_concepts_ModHead(SpecGenAtom, 
			     ModifierIndex,       ModifierCUI, ModifierConcept,
			     ModifierSemTypeList, ModifierSemType,
			     HeadIndex,           HeadCUI, HeadConcept,
			     HeadSemTypeList,     HeadSemType, 
			     SpecificIndex,       SpecificCUI, SpecificConcept,
			     SpecificSemTypeList, SpecificSemType,
			     GeneralIndex,        GeneralCUI, GeneralConcept,
			     GeneralSemTypeList,  GeneralSemType) :-
	% SpecGenAtom is something like 'C0023570 C0087111', where
	% 'C0023570' is the SpecificCUI and 'C0087111' is the GeneralCUI.
	% We pass in also ModCUI, the CUI of the modifier concept
	midstring(SpecGenAtom, SpecificCUI, _GeneralCUI, 0, 8),
	( SpecificCUI == ModifierCUI ->
	  % In this case, the SpecificCUI is the same as the ModCUI,
	  % and thus the modifier concept is the specific concept, so instantiate
	  % * SpecificConcept with ModifierConcept
	  % * GeneralConcept with HeadConcept
	  % * SpecificSemType with ModifierSemType
	  % * GeneralSemType with HeadSemType
	  SpecificIndex       = ModifierIndex,
	  GeneralIndex        = HeadIndex,
	  SpecificCUI         = ModifierCUI,
	  SpecificConcept     = ModifierConcept,
	  GeneralCUI          = HeadCUI,
	  GeneralConcept      = HeadConcept,
	  SpecificSemTypeList = ModifierSemTypeList,
	  SpecificSemType     = ModifierSemType,
	  GeneralSemTypeList  = HeadSemTypeList,
	  GeneralSemType      = HeadSemType
	; SpecificIndex       = HeadIndex,
	  GeneralIndex        = ModifierIndex,
	  SpecificCUI         = HeadCUI,
	  SpecificConcept     = HeadConcept, % This is the reverse case, so instantiate the opposite
	  GeneralCUI          = ModifierCUI,
	  GeneralConcept      = ModifierConcept,
	  SpecificSemTypeList = HeadSemTypeList,
	  SpecificSemType     = HeadSemType,
	  GeneralSemTypeList  = ModifierSemTypeList,
	  GeneralSemType      = ModifierSemType
        ).
    

get_metaconc_name_and_semtype_list([MetaConc:CUI:SemGroupList|_More], MetaConc, CUI, SemGroupList).
get_metaconc_name_and_semtype_list([_NoMatch|More], MetaConc, CUI, SemGroupList) :-
	get_metaconc_name_and_semtype_list(More, MetaConc, CUI, SemGroupList).

% ----- NPs are not coordinate 
%  Fail if FirstNP and SecondNP are coordinate to each other;
%  the only exception is the the last NP is cued by other

nps_are_not_coordinate(FirstNP,SecondNP,ConjList) :-
    get_from_list(head,FirstNP,FirstHeadlist),
    get_from_list(head,SecondNP,SecondHeadlist),
    npu_has_conjunct(FirstHeadlist,ConjList,_Conj,_LConjHeadList,SecondHeadlist, _ ),
    find_Other_pattern(SecondNP),
    !.
nps_are_not_coordinate(FirstNP,SecondNP,ConjList) :-
    get_from_list(head,FirstNP,FirstHeadlist),
    get_from_list(head,SecondNP,SecondHeadlist),
    npu_has_conjunct(FirstHeadlist,ConjList,_Conj,_LConjHeadList,SecondHeadlist, _ ),
    !,fail.
nps_are_not_coordinate(FirstNP,SecondNP,ConjList) :-
    get_from_list(head,FirstNP,FirstHeadlist),
    npu_has_conjunct(FirstHeadlist,ConjList,_Conj,LConjHeadList,_RConjHeadList, _ ),
    get_from_list(head,SecondNP,SecondHeadlist),
    memberchk(SecondHeadlist,LConjHeadList),
    !,fail.
nps_are_not_coordinate(_FirstNP,_SecondNP,_ConjList).

% share_common_semantic_type(MSU1, MSU2, SemTypeList1, SemTypeList2, CommonSemType, GroupName) :-
% 	get_sem_info(MSU1, _Conc1, SemTypeList1),
% 	get_sem_info(MSU2, _Conc2, SemTypeList2),
% 	member(CommonSemType, SemTypeList1),
% 	memberchk(CommonSemType, SemTypeList2),
%       semgroup_member(CommonSemType, GroupName).

msus_share_common_semantic_group(Domain, MSU1, MSU2,
				 SemTypeList1, SemTypeList2,
				 SemType1, SemType2, GroupName) :-
	get_sem_info(MSU1, _Conc1, _CUI1, SemTypeList1),
	get_sem_info(MSU2, _Conc2, _CUI2, SemTypeList2),
	semtype_lists_share_common_semantic_group(Domain,
						  SemTypeList1, SemTypeList2,
						  SemType1, SemType2,
						  GroupName).
semtype_lists_share_common_semantic_group(Domain,
					  SemTypeList1, SemTypeList2,
					  SemType1, SemType2,
					  GroupName) :-
	member(SemType1, SemTypeList1),
	member(SemType2, SemTypeList2),
        domain_specific_semgroup_member(Domain, SemType1, GroupName),
        domain_specific_semgroup_member(Domain, SemType2, GroupName).

%% % Check if the Semantic Type list of the MSU contains a SemType contained in MatchingSemanticTypes
%% checkSemTypelist(MatchingSemanticTypes, MSU, SemTypeList, TstMatch) :-
%%	% Get the head of the MSU.
%%	get_from_list(head, MSU, HeadList),
%%	% Get the MSU's Semantic Type list.
%%	get_sem_info(HeadList, _ConcList, SemTypeList),
%%	% Generate a SemanticType in the MSU's Semantic Type list
%%	member(TstMatch, SemTypeList),
%%	% Ensure that this Semantic Type is in SemanticTypeGroupList
%%	member(TstMatch, MatchingSemanticTypes).

/*
For Mod Head:
Given a structure,
First, get the list of semantic types associated with the metaconc in the list.

 [ index(6),
   usemtype(['Disease or Syndrome']),
   ausemtype([dsyn]),
   isemtype([disorder]),
   lexmatch(['viral infection']),
   inputmatch([viral,infection]),
   tag(noun),
   tokens([viral,infection]),
   bases([]),
   metaconc(['Virus Diseases':[dsyn]])]

For the list above, get_sem_info/4 returns ['Virus Diseases'] and [dsyn] in its 2nd and 3rd args.
Next, choose one of the Semantic Types in the list.
And finally, identify the Semantic Group associated with that Semantic Type.
*/

/* ---------- Deal With Coordination  from Seminterp----------

Deal_with_coordination is inside semspec,  because I need to check the Metathesaurus
when I build the predication and not after it.
It is copied from seminterp and modified to chech Meta relationships.
ConjList = a list of: coord( Conj, LeftConjunct, RightConjunct, [] ) 
This whole predicate of coordination  must be adapted to make use of semantic groups
 rather than Semantic Types.

 */

deal_with_coordination(NegatedRelation, NormalizedRelation, IndicatorIndex, Domain,
		       Subject, SubjectSemTypeList, SubjectSemType,
		       SubjectMetaConc, SubjectCUI, SubjectIndex,
		       Object,  ObjectSemTypeList, ObjectSemType,
		       ObjectMetaConc,  ObjectCUI, ObjectIndex,
		       ConjList, 
		       UsedHeadListWithRoles, ObjGap, %%% both these are []
		       AllPredications, Gap) :-
	create_coord_list(subj, Subject, SubjectSemType,
			   NormalizedRelation,
			   ObjectSemType, ConjList,
			   SubjectCoordList,
			   SubjectListWithRoles, SubjGap),
	create_coord_list(obj, Object, ObjectSemType,
			   NormalizedRelation,
			   SubjectSemType, ConjList,
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
	build_all_predications([SubjectMetaConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex-_SubjectNeg
			            |SubjectCoordList],
			       NegatedRelation, IndicatorIndex, Domain,
			       [ObjectMetaConc-ObjectCUI-ObjectSemTypeList-ObjectSemType-ObjectIndex-_ObjectNeg
			            |ObjectCoordList],
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

create_coord_list(HeadAtom, HeadList, HeadSemType, Relation, OtherSemType, ConjList,
		  CoordList, [], []) :-
	( npu_has_conjunct(HeadList, ConjList,
			   Conj, LeftConjList, RightConjunct, _) ->
	  get_conjunct_data(HeadList, [HeadSemType], Relation,
			    HeadAtom, [OtherSemType],
			    Conj, LeftConjList, RightConjunct, CoordList)
	  %%% add_roles_to_conjuncts(LeftConjList, RightConjunct,
	  %%%			 HeadAtom, HeadListsWithRoles, Gap)
	; CoordList = []
	  %%% HeadListsWithRoles = [[role(HeadAtom)|HeadList]|Gap]
	).



% The new version of build_all_predications/6 takes
% SubjectList: a list of MetaConc-Semtype terms, one for each conjoined Subject;
%              if the subject is not conjoined, this list has only one element.
% Relation:    the relation, e.g., 'TREATS', 'OCCURS_IN', etc.
% ObjectList: same structure for objects

% It simply creates a predication using Relation for each subject/object pair
% in the Cartesian Product of the SubjectList and ObjectList

build_all_predications([], _, _, _, _, Predications, Predications).
build_all_predications([FirstSubject|RestSubjects], Relation, IndicatorIndex, Domain, ObjectList,
		       PredicationsIn, PredicationsOut) :-
	build_all_predications_1(ObjectList, Relation, IndicatorIndex, Domain, FirstSubject,
				 PredicationsIn, PredicationsNext),
	build_all_predications(RestSubjects, Relation, IndicatorIndex, Domain, ObjectList,
			       PredicationsNext, PredicationsOut).

% distances need to be fixed.
build_all_predications_1([], _, _, _, _, Predications, Predications).
build_all_predications_1([ObjectConc-ObjectCUI-ObjectSemTypeList-ObjectSemType-ObjectIndex-ObjectNeg|RestObjects],
			 Relation, IndicatorIndex, Domain,
			 SubjectConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex-SubjectNeg,
%			 [_SubjectMaxDist-_SubjectDist-SubjectIndex-
			 [0-0-SubjectIndex-
			 SubjectConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-
%			  Relation-IndicatorIndex-
	                 'SPEC'-RelationOut-IndicatorIndex-
%			 _ObjectMaxDist-_ObjectDist-ObjectIndex-
			 0-0-ObjectIndex-
			 ObjectConc-ObjectCUI-ObjectSemTypeList-ObjectSemType
			 |PredicationsIn],
			 PredicationsOut) :-
%	get_concept_cui(SubjectConc, SubjectCUI),
%	get_concept_cui(ObjectConc, ObjectCUI),
	SubjectCUI \== ObjectCUI,
	meta_ancestors(SubjectCUI, ObjectCUI, SpecGenAtom),
	SpecGenAtom \== 'None.',
	concat_atom([SubjectCUI, ' ', ObjectCUI], ConcatenatedCUIs),
	SpecGenAtom == ConcatenatedCUIs,
%	!,
	semtype_lists_share_common_semantic_group(Domain,
						  SubjectSemTypeList, ObjectSemTypeList,
						  _SubjectSemType, _ObjectSemType,
						  _GroupName),
	!,
	( ( ObjectNeg == 1
	  ; SubjectNeg == 1
	  ) ->
	  ( midstring(Relation,'NEG_',RelationNegRemoved,0,4) -> 
	    RelationOut = RelationNegRemoved
	  ; concat_atom(['NEG_',Relation],RelationOut)
	  )
	; RelationOut = Relation
	),
	!,
	build_all_predications_1(RestObjects, Relation, IndicatorIndex, Domain,
				 SubjectConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex,
				 PredicationsIn, PredicationsOut).
build_all_predications_1([_ObjectConc-_ObjectCUI-_ObjectSemTypeList-_ObjectSemType-_ObjectIndex|RestObjects],
			 Relation, IndicatorIndex, Domain,
			 SubjectConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex,
			 PredicationsIn, PredicationsOut) :-
	build_all_predications_1(RestObjects, Relation, IndicatorIndex, Domain,
				 SubjectConc-SubjectCUI-SubjectSemTypeList-SubjectSemType-SubjectIndex,
				 PredicationsIn, PredicationsOut).


allowed_geoa(geoa,geoa,MetaConc) :-
	!,
	atom_codes(MetaConc,MetaConcCodes),
	split_string_completely(MetaConcCodes," ",ListOfMetaConcCodes),
	last(ListOfMetaConcCodes,LastCode),
	atom_codes(LastAtom,LastCode),
	lower(LastAtom,LastAtomLower),
	allowed_geoa_object_match(LastAtomLower).
allowed_geoa(_SemType1,_SemType2,_MetaConc).

allowed_geoa_object_match(country).
allowed_geoa_object_match(countries).
allowed_geoa_object_match(islands).
allowed_geoa_object_match(continent).
allowed_geoa_object_match(locations).
allowed_geoa_object_match(cities).
