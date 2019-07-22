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

% File:	    nominal_args.pl
% Module    nominal_args 
% Author    halil
% Purpose   finding arguments of nominal indicators


% ----- Module declaration and exported predicates

:- module( nominal_args, [
        get_args_of_nominalization/9
    ]).

:- use_module( skr_lib(nls_lists), [
                get_from_list/3
   ]).

:- use_module( skr_lib(nls_system), [
		control_value/2
   ]).

:- use_module( usemrep_lib(exceptions_GEN), [
		non_prepositionally_cued_object_GEN/1,
		non_prepositionally_cued_subject_GEN/1
  ]).

:- use_module( usemrep_domain(exceptions_DOM), [
		non_prepositionally_cued_object_DOM/1,
		non_prepositionally_cued_subject_DOM/1
  ]).

:- use_module( usemrep_lib(semspec), [
	count_parens_in_one_MSU/3,
	count_parens_in_MSU_list/3
  ]).

:- use_module( usemrep_lib(ssuppserv), [
		get_intervening_structure/3,
		is_verb_indicator/1,
		locate_npu_and_head/5,
		locate_npu_head/3,
		no_np/1,
		no_verbal/1,
		preceding_mod/3
    ]).

:- use_module( library(lists), [
	append/2,
	nextto/3,
	rev/2
   ]).

% ---  GET_ARGS_OF_NOMINALIZATION ---
% New algorithm based on the recent set of linguistic generalizations
% based on Sean's and then Halil and Tom's work.

% Start from the right of the nominalization, with a predetermined ObjectCue	
% Base argument is necessary to allow arguments in parentheses for nominalizations
% of the form "X inhibitor (Y)". While disallowing all other arguments within parentheses, 
% we still want to allow these.

get_args_of_nominalization(Indicator, ObjectCue, ThisNPU, MoreNPUs,
			   LeftPartition, ObjectCueList, SubjectList, ObjectList, Base) :-
	( np_to_the_right(ThisNPU, Indicator, MoreNPUs, LeftPartition, ObjectCue,
	                  ObjectCueList, SubjectList, SubjectNPU, ObjectList, ObjectNPU, Base)
	; no_np_to_the_right(ThisNPU, Indicator, LeftPartition,
			     SubjectList, SubjectNPU, ObjectList, ObjectNPU, Base)
        ),
	check_parens(LeftPartition, ThisNPU, SubjectNPU, ObjectNPU, MoreNPUs).

check_parens(_LeftPartition, _ThisNPU, SubjectNPU, SubjectNPU, _MoreNPUs):- !.
check_parens(LeftPartition, ThisNPU, SubjectNPU, ObjectNPU, MoreNPUs) :-
	rev(LeftPartition, RevLeftPartition), % RevLeftPartition is now in ascending MSU order
	% TargetNPUs will contain either 2 NPUs (if ThisNPU is either SubjectNPU or ObjectNPU),
	% or 3 NPUs (if ThisNPU, SubjectNPU, and ObjectNPU are distinct).
	sort([ThisNPU,SubjectNPU,ObjectNPU], TargetNPUs),
	% Assemble a complete list of all NPUs under consideration in ascending NPU order
	append([RevLeftPartition, [ThisNPU], MoreNPUs], AllNPUs),
	get_intervening_NPUs(TargetNPUs, AllNPUs, InterveningNPULists),
	append(InterveningNPULists, InterveningNPUs),
	StartingParenCount is 0,
	do_paren_check_for_individual_NPUs(TargetNPUs, StartingParenCount),
	count_parens_in_MSU_list(InterveningNPUs, StartingParenCount, FinalParenCount),
	FinalParenCount is 0.
	
% consider "MSU" and "NPU" as more or less synonymous
% must verify that each individual NPU in TargetNPUs has a net 0 paren count,
% and that InterveningNPUs has a net 0 paren count.
do_paren_check_for_individual_NPUs([], _StartingParenCount).
do_paren_check_for_individual_NPUs([FirstNPU|RestNPUs], StartingParenCount) :-
	count_parens_in_one_MSU(FirstNPU, StartingParenCount, FinalParenCount),
	FinalParenCount is 0,
	do_paren_check_for_individual_NPUs(RestNPUs, StartingParenCount).

% Find all NPUs strictly between NPU1 and NPU2
get_intervening_NPUs([NPU1,NPU2], AllNPUs, [Between12]) :-
	!,
	append([_Before,[NPU1],Between12,[NPU2],_After], AllNPUs),
	!.
% Find all NPUs strictly between NPU1 and NPU2, and strictly between NPU2 and NPU3
get_intervening_NPUs([NPU1,NPU2,NPU3], AllNPUs, [Between12,Between23]) :-
	!,
	append([_Before,[NPU1],Between12,[NPU2],Between23,[NPU3],_After], AllNPUs),
	!.

% ThisNPU is the NPU containing Indicator.
% Remainder is all NPUs in RestNPUs after Arg1NPU.
% After identifying 
% Need to look for parens
% (1) in ThisNPU after Indicator,
% (2) in Arg1NPU after Arg1,
% (3) in MoreNPUs before ThisNPU

np_to_the_right(ThisNPU, Indicator, MoreNPUs, _LeftPartition, between-and,ObjectCueList,
	        SubjectList, SubjectNPU, ObjectList, ObjectNPU, Base) :-
	next_argument_to_the_right(Indicator, ThisNPU, MoreNPUs, between, between,CueList1,
				   Arg1, Arg1NPU, Arg1Value, Remainder, Base0),
	next_argument_to_the_right(Indicator, Arg1NPU, Remainder, and, between,CueList2,
				   Arg2, Arg2NPU, Arg2Value, _, Base1),
	!,
	ObjectCueList = [CueList1-CueList2],
	\+ Arg1Value == Arg2Value,
	( Arg1Value == object ->
	  SubjectList = Arg2,
	  SubjectNPU  = Arg2NPU,
	  ObjectList  = Arg1,
	  ObjectNPU   = Arg1NPU,
	  Base        = Base1,
	  \+ intervening_conj_and_NPs_in_pphr(Arg1NPU, Arg2NPU, MoreNPUs)
        ; SubjectList = Arg1,
	  SubjectNPU  = Arg1NPU,
	  ObjectList  = Arg2,
	  ObjectNPU   = Arg2NPU,
	  Base        = Base0
        ).
np_to_the_right(ThisNPU, Indicator, MoreNPUs, LeftPartition, ObjectCue, ObjectCueList,
	        SubjectList, SubjectNPU, ObjectList, ObjectNPU, Base) :-
        \+ ObjectCue == between-and,
	(ObjectCue = FirstCue-SecondCue ->
	  true
	; FirstCue = ObjectCue,
	    SecondCue = ObjectCue
	),
	next_argument_to_the_right(Indicator, ThisNPU, MoreNPUs, FirstCue, FirstCue, FirstCueList,
				   Arg1, Arg1NPU, Arg1Value, Remainder, Base0),
	( next_argument_to_the_right(Indicator, Arg1NPU, Remainder, SecondCue, FirstCue, SecondCueList,
				     Arg2, Arg2NPU, Arg2Value, _, Base1),
	  ObjectCueList = [FirstCueList-SecondCueList]
        ; next_argument_to_the_left(ThisNPU, Indicator, LeftPartition, Arg2,
				    Arg2Value, Arg2NPU, Base1),
	  ObjectCueList = FirstCueList
        ),
	\+ Arg1Value == Arg2Value,
	( Arg1Value == object ->
	  SubjectList = Arg2,
	  SubjectNPU  = Arg2NPU,
	  ObjectList  = Arg1,
	  ObjectNPU   = Arg1NPU,
	  Base        = Base1,
	  \+ intervening_conj_and_NPs_in_pphr(Arg1NPU, Arg2NPU, MoreNPUs)
        ; SubjectList = Arg1,
	  SubjectNPU  = Arg1NPU,
	  ObjectList  = Arg2,
	  ObjectNPU   = Arg2NPU,
	  Base        = Base0,
	% this is needed to disallow an NP-external object to the left
	  ( var(Arg2Value) -> FirstCue == of ; true )
        ),
	check_heavy_NP_shift(ThisNPU, ObjectCue, SubjectNPU, ObjectNPU).
%	check_heavy_NP_shift_for_EFFECT(Indicator, SubjectNPU, ObjectNPU, MoreNPUs).


% Bugzilla #13:
% "These agents have no effect on the pharmacokinetics of TIKOSYN."
% generates the infelicitous predication TIKOSYN-NEG_AFFECTS-Pharmacokinetics.
% This next predicate tests the following:
% (1)  If the indicator lexmatch is "effect", then either
% (2a) the indicator subject must precede the indicator object,
%      i.e., SubjectIndex < ObjectIndex, or
% (2b) the subject (which follows the object) must be a heavy NP.

% Examples:
% (1) the effect of chemotherapy on cancer
%                ---------------|---------
%                    SUBJECT       OBJECT
% subject precedes object, so succeed.

% (2) the effect on outcomes of the procedure
%                -----------|----------------
%                   OBJECT      SUBJECT
% subject follows object, so fail.

% (3) the effect on cancer of chemotherapy
%                ---------|---------------
%                  OBJECT      SUBJECT
% subject follows object, so fail.

% (4) the effect on cancer of a newly developed chemotherapy
%                ---------|---------------------------------
%                  OBJECT              SUBJECT
% subject follows object, but subject is heavy, so succeed

% (4) the effect on cancer of chemotherapy developed in Australia
%                ---------|--------------------------------------
%                  OBJECT                  SUBJECT
% subject follows object, but subject is heavy, so succeed

check_heavy_NP_shift_for_EFFECT(Indicator, SubjectNPU, ObjectNPU, MoreNPUs) :-
	arg(1, Indicator, IndicatorList),
	get_from_list(lexmatch, IndicatorList, [LexMatch]),
%	( LexMatch == effect ->
        ( effect_lexeme(LexMatch) ->
	  get_msu_index(SubjectNPU, SubjectIndex),
	  get_msu_index(ObjectNPU,  ObjectIndex),
	  ( SubjectIndex < ObjectIndex ->
	    true
	  ; subject_is_heavy_NP(SubjectNPU, MoreNPUs)
	  )
	; true
	).

effect_lexeme(effect).
effect_lexeme(impact).
effect_lexeme(influence).

check_heavy_NP_shift(ThisNPU, ObjectCue, SubjectNPU, ObjectNPU) :-
        nonvar(ObjectCue),
	get_msu_index(ThisNPU, IndicatorIndex),
	get_msu_index(SubjectNPU, SubjectIndex),
	get_msu_index(ObjectNPU,  ObjectIndex),
	IndicatorIndex < ObjectIndex,
	ObjectIndex < SubjectIndex,
	ObjectCue \== of,
	memberchk(prep(SubjectPrepList),SubjectNPU),
	get_from_list(lexmatch,SubjectPrepList,[of]),
	!,
	subject_is_heavy_NP(SubjectNPU).
check_heavy_NP_shift(_ThisNPU, _ObjectCue, _SubjectNPU, _ObjectNPU).

%subject_is_heavy_NP(SubjectNPU, MoreNPUs) :-
%	% The SubjectNPU itself is heavy (e.g., "a newly developed chemotherapy")
%	( length(SubjectNPU, SubjectNPULength),
%	  SubjectNPULength > 5 ->
%	  true
%	% The SubjectNPU itself is not heavy, but it has postmodifiers
%	% (e.g., "chemotherapy developed in Australia")
%	; append(_Before, [SubjectNPU|After], MoreNPUs),
%	  After \== []
%	).

% I don't think Francois's implementation for "chemotehrapy developed in Australia"
% is sufficient, unless it takes into account past participles, reduced relative clauses, etc.
% So, for simplicity, I am just considering the case where heavy NPs are simply those that
% have more than 7 elements.
% Note that 7 includes msu_index() and confid(), so it is really 5.
subject_is_heavy_NP(SubjectNPU) :-
	% The SubjectNPU itself is heavy (e.g., "a newly developed chemotherapy")
        length(SubjectNPU, SubjectNPULength),
	SubjectNPULength > 7.



% From Bugzilla #15:
% 16733164.ab.9 After additional administration of heparin, platelet inhibition
% was only comparable for the glass bead test with heparinase and aggregometry,
% and the correlation coefficient remained unchanged for the glass bead test
% with heparinase versus aggregometry (rho = 0.878, P < .001).
% Simplified example: platelet inhibition was (rho).
% We want to block the predication rho-DISRUPTS-Blood Platelets
% by verifying that the next arg to the right does NOT contain a left paren.
% Solved by importing contains_left_paren/1 from semspec.pl.

% From Bugzilla #30:
% 2342648.ab.3 The results largely confirmed the usefulness of nifedipine
% in the treatment of hypertension at rest and at the top of an isometric exercise.
% We want to block the predication
% C0022206|Exercise, Isometric|topp|topp|||TREATS|C0020538|Hypertensive disease|dsyn|dsyn||
% Logic:
% (1)  a conjunction appears between the two NPU args, AND (to strengthen the test)
% (2a) Arg1NPU (which contains Arg1) is a prep phrase, and if
% (2b) Arg2NPU (which contains Arg2) is a prep phrase.

intervening_conj_and_NPs_in_pphr(Arg1NPU, Arg2NPU, MoreNPUs) :-
	intervening_conjunction(Arg1NPU, Arg2NPU, MoreNPUs),
	nps_in_pphr(Arg1NPU, Arg2NPU).
	
intervening_conjunction(Arg1NPU, Arg2NPU, MoreNPUs) :-
	nonvar(Arg1NPU),
	nonvar(Arg2NPU),
	member(InterveningNPU, MoreNPUs),
	memberchk(conj(_), InterveningNPU),
	get_msu_index(Arg1NPU, Arg1MSUIndex),
	get_msu_index(Arg2NPU, Arg2MSUIndex),
	get_msu_index(InterveningNPU, InterveningMSUIndex),
	Arg1MSUIndex < InterveningMSUIndex,
	Arg2MSUIndex > InterveningMSUIndex.


nps_in_pphr(Arg1NPU, Arg2NPU) :-
	memberchk(prep(_), Arg1NPU),
	memberchk(prep(_), Arg2NPU).

get_msu_index(Arg1NPU, Arg1MSUIndex) :-
	memberchk(msu_index(Arg1MSUIndex), Arg1NPU).

no_np_to_the_right(ThisNPU, Indicator, LeftPartition,
		   SubjectList, SubjectNPU, ObjectList, ObjectNPU, Base) :-
	preceding_mod(Indicator, ThisNPU, ObjectList),
	ObjectNPU = ThisNPU,
%	( nextto(ObjectTerm,Indicator,ThisNPU),
	(  preceding_mod(ObjectList, ThisNPU, SubjectList),
	  SubjectNPU = ThisNPU
        ; argument_to_the_left(ThisNPU, Indicator, LeftPartition, SubjectList, SubjectNPU, Base)
        ).

next_argument_to_the_right(Indicator,ThisNPU,MoreNPUs,ObjectCue,OtherCue,ObjectCueList,HeadList,ArgNPU,ArgValue,Remainder,Base) :-
	locate_npu_and_head(MoreNPUs,_Type,ArgNPU,HeadList,Remainder),
	get_intervening_structure(MoreNPUs,HeadList,InterveningStructure),
	determine_argument_type(Indicator,ThisNPU,ArgNPU,ObjectCue,OtherCue,ObjectCueList,InterveningStructure,ArgValue,Base).
	
next_argument_to_the_left(ThisNPU,Indicator,LeftPartition,HeadList,ArgValue,ArgNPU,Base):- 
	( preceding_mod(Indicator,ThisNPU,HeadList),
	  ArgNPU = ThisNPU
        ; ArgValue = subject,
	  argument_to_the_left(ThisNPU,Indicator,LeftPartition,HeadList,ArgNPU,Base)	   
        ).

argument_to_the_left(ThisNPU,Indicator,LeftPartition,HeadList,NPU,Base) :-
	locate_npu_and_head(LeftPartition,_Type,NPU,HeadList,_Remainder),
	get_intervening_structure(LeftPartition,HeadList,InterveningStructure),
	( nominalization_is_cued(ThisNPU,NPU,InterveningStructure,Base)
	% allow non-cuing when the indicator is multi-word?
	; InterveningStructure == [],
	  \+ memberchk(Indicator,ThisNPU)
	).

determine_argument_type(_Indicator,_PrevNPU,NP,ObjectCue,_OtherCue,CueList,InterveningStructure,object,none) :-
	adverb_or_heavy_np_intervening(InterveningStructure,ObjectCue),
	memberchk(prep(CueList),NP),
	get_from_list(lexmatch,CueList,[ObjectCue]).
determine_argument_type(Indicator,_PrevNPU,NP,ObjectCue,_OtherCue,CueList,InterveningStructure,subject,none) :-
	argument_allowed_to_the_right(Indicator),
	potential_subject_np(NP,ObjectCue,CueList),
	no_verbal(InterveningStructure).
determine_argument_type(Indicator,PrevNPU,NP,ObjectCue,OtherCue,CueList,InterveningStructure,subject,Base) :-
	\+ memberchk(prep(_),NP),
	argument_allowed_to_the_right(Indicator),
	( intervening_subject_cue(InterveningStructure,[],right,Base)
	; InterveningStructure == [], intervening_subject_cue([PrevNPU],[],right,Base)
	; InterveningStructure == [], intervening_subject_cue([NP],[],right,Base)
	; intervening_conj_for_reciprocal(OtherCue,ObjectCue,CueList,InterveningStructure,right)
	).

nominalization_is_cued(ThisNPU,ArgumentNPU,InterveningStructure,Base) :-
	( memberchk(prep(_PrepList),ThisNPU)
	; intervening_subject_cue(InterveningStructure,[],left,Base) 
	; intervening_subject_cue([ThisNPU],[],left,Base) 
	; intervening_subject_cue([ArgumentNPU],[],left,Base)
	).

adverb_or_heavy_np_intervening([],_Cue) :- !.
adverb_or_heavy_np_intervening([ThisNPU|Rest],Cue) :-
	potential_subject_np(ThisNPU,Cue,_CueList),
	!,
	adverb_or_heavy_np_intervening(Rest,Cue).
adverb_or_heavy_np_intervening([ThisNPU|Rest],Cue) :-
	memberchk(adv(_),ThisNPU),
	% no head-like token
	no_np([ThisNPU]),
	adverb_or_heavy_np_intervening(Rest,Cue).

potential_subject_np(ThisNPU,Cue,CueList) :-
	memberchk(prep(CueList),ThisNPU),
	get_from_list(lexmatch,CueList,[Prep]),
	memberchk(Prep,[of,by,with,via]),
        \+ Prep == [Cue],
	% It seems weird, but I had to add this block "CEA" as the subject of "risk" in the example
	% "There was no difference in the risk of death/stroke relative to the timing of CEA"
	% I am not sure why the \+ Prep == [Cue] works.
	\+ Prep == Cue,
	!.

intervening_subject_cue([ThisNPU|Rest],_Prev,right,Base) :-
	( get_from_list(aux, ThisNPU, AuxList) 
        ; get_from_list(verb, ThisNPU,AuxList)
	), 
	get_from_list(bases, AuxList, [Base]),
	memberchk(Base,[be,remain]),
        !,
        ( Rest == []
        ; Rest = [NPU|_],
          \+ followed_by_verb(NPU)
        ).
intervening_subject_cue([ThisNPU|Rest],Prev,left,Base) :-
        ( Rest == []
        ;  \+ followed_by_verb(Prev)
        ),
        ( get_from_list(aux, ThisNPU, AuxList)
	; get_from_list(verb, ThisNPU, AuxList)
	),
        get_from_list(bases, AuxList, [Base]),
	memberchk(Base,[be,remain]),
        !. 
% support verbs
intervening_subject_cue([ThisNPU|_Rest],_Prev,left,Base) :-
        ( get_from_list(aux, ThisNPU, AuxList)
	; get_from_list(verb,ThisNPU,AuxList)
	),
        get_from_list(bases, AuxList, [Base]),
	intervening_subject_cue_verb(Base),
	% memberchk(Base,[have,play,increase,decrease,reduce,take]),
        !.  
intervening_subject_cue([ThisNPU|_Rest],_Prev,_Direction,Base) :-
	get_from_list(punc, ThisNPU, PuncList),
        get_from_list(bases, PuncList, [Base]),
	memberchk(Base,['(',',']),
        !.
intervening_subject_cue([ThisNPU|Rest],_Prev,Direction,Base) :-
	intervening_subject_cue(Rest,ThisNPU,Direction,Base).

intervening_subject_cue_verb(decrease).
intervening_subject_cue_verb(have).
intervening_subject_cue_verb(increase).
intervening_subject_cue_verb(lower).
intervening_subject_cue_verb(maximize).
intervening_subject_cue_verb(minimize).
intervening_subject_cue_verb(play).
intervening_subject_cue_verb(reduce).
intervening_subject_cue_verb(take).
%intervening_subject_cue_verb(provide).

% 'association between a and b' InterveningStructure should have the conjunction 
intervening_conj_for_reciprocal(between,ConjBase,Conj,[ThisNPU|_Rest],right) :-
	get_from_list(conj,ThisNPU,Conj),
	get_from_list(bases,Conj,[ConjBase]),
	!.

% certain nominalizations do not allow prepositionally cued subjects to the right.
argument_allowed_to_the_right(Indicator) :-
	arg(1,Indicator,IndicatorList),
	get_from_list(bases,IndicatorList,BasesList),
	argument_allowed_to_the_right_aux(BasesList),
	!.	

argument_allowed_to_the_right_aux([]) :- !.
argument_allowed_to_the_right_aux([Bases|_Rest]) :-
	non_prepositionally_cued_subject_BOTH(Bases),
	!,
	fail.
argument_allowed_to_the_right_aux([Bases|_Rest]) :-
	non_prepositionally_cued_object_BOTH(Bases),
	!,
	fail.
argument_allowed_to_the_right_aux([_Bases|Rest]) :-
	argument_allowed_to_the_right_aux(Rest).

followed_by_verb([msu_index(_),NPU|_]) :-
        is_verb_indicator(NPU).

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

% non_prepositionally_cued_subject is part of exceptions, so the second set of rules applies
non_prepositionally_cued_subject_BOTH(Bases) :-
	( control_value(domain, _Domain) ->
	  non_prepositionally_cued_subject_DOM(Bases)
	; non_prepositionally_cued_subject_GEN(Bases)
	).

% non_prepositionally_cued_object is part of exceptions, so the second set of rules applies
non_prepositionally_cued_object_BOTH(Bases) :-
	( control_value(domain, _Domain) ->
	  non_prepositionally_cued_object_DOM(Bases)
	; non_prepositionally_cued_object_GEN(Bases)
	).
