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

% File:	    inference.pl
% Module:   inference
% Author:   tcr, NLS
% Purpose:  drawing inferences from predications produced by usemrep
%           Draw inferences also has been changed  from the original version by Tom and MF.

% ----- Module declaration and exported predicates

:- module( inference, [
	draw_inferences/4
   ]).

:- use_module( usemrep_lib(ssuppserv), [
	check_any_relation/6,
	check_relation/3
   ]).

% :- use_module( usemrep_lib(ssuppserv), [
% 	append_diff/6,
% 	var_memberchk/2
%    ]).

:- use_module( library( lists ), [
	append/2
   ]).

:- use_module( library( sets ), [
	intersect/2,
	list_to_set/2
   ]).

:- use_module( skr_lib(sicstus_utils ), [
	lower/2,
	midstring/5,
	concat_atom/2
   ]).


/*

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BACKGROUND for INFERENCE %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Semantic types for each argument have been added during seminterp.

Rules apply within a sentence only.

---- Sample Rule 1:

 X TREATS Y
 Y = [popg,podg]  }
 Z OCCURS_IN Y    }  --> X TREATS Z

That is, "aspirin treats patients with headache" -->
         "aspirin treats headache"

    Aspirin-TREATS-Patients &
    Headache-OCCURS_IN-Patients -->
    Aspirin-TREATS(INFER)-Headache

X-XST REL1 Y-YST
Z-ZST REL2 Y-YST --> X-XST REL1 Z-ZST

Cond: YST = [popg,podg]
      REL1 = TREATS
      REL2 = OCCURS_IN


---- Sample Rule 2:

 X TREATS Y      }
 Y = [humn,mamm] }
 Z PROCESS_OF Y  } --> X TREATS Z

That is, "rifampin treats dogs with tuberculosis" -->
         "rifampin treats tuberculosis"

     Rifampin-TREATS-Dogs &
     Tuberculosis-PROCESS_OF-Dogs -->
     Rifampin-TREATS(INFER)-Tuberculosis

X-SXT REL1 Y-YST
Z-ZST REL2 Y-YST --> X-XST REL1 Z-ZST

Cond: YST = [humn,mamm]
      REL1 = TREATS
      REL2 = PROCESS_OF


---- Sample Rule 3:

 X-XST TREATS/PREVENTS Y-YST }
 ZST = [phsu]                }
 X-XST USES Z-ZST            } --> Z TREATS/PREVENTS Y

That is, "treatment with aspirin prevents headache" -->
         "aspirin prevents headache"

      Treament-PREVENTS-Headache &
      Treatment-USES-Aspirin -->
      Aspirin-PREVENTS(INFER)-Headache

X-XST REL1 Y-YST
X-XST REL2 Z-ZST --> Z-ZST REL1 Y-YST

Cond: REL1 = TREATS or PREVENTS
      REL2 = USES
      ZST = [phsu]


ISA inferences do not check Semantic Types.
It matches at the Hypernym level at either the object 
or subject side.  Consider the difference in examples.

"Aspirin is a medication for headache"

X ISA Y        (X = Aspirin, Y = Pharmaceutical preparation, From SemSpec)
Y TREATS Z     (Y = Pharmaceutical preparation,  Z = Headache, From Usemrep)
------------
X TREATS(SPEC) Z 


"Pneumonia is an infection of the lung"

X ISA Y                (X = Pneumonia , Y = Infection, From SemSpec)
Z LOCATION_OF Y        (Y = Infection,  Z = Lung, From Usemrep)
----------------------
Z LOCATION_OF(SPEC) X 

This predicate needed  an auxilliary because there might be more than 1 predicate
that matches ISA, like in the sentence
"Mycoplasma pneumonia is an infection of the lungs caused by Mycoplasma pneumoniae"


The predicate cannot instantiate Circular Z == X.
A statement was added to the predicate get_all_inferences_aux
to make sure this will not happen.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% HOW to write INFERENCE RULES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

AP = antecedent predication
CP = consequent predication

Format of new inference rules:
inference_rule(RuleName,
	       [ AP1Rel, AP1Subj, AP1Obj, AP1SubjSemTypes, AP1ObjSemTypes,
	         AP2Rel, AP2Subj, AP2Obj, AP2SubjSemTypes, AP2ObjSemTypes,	
	         CPRel,  CPSubj,  CPObj ]).

  Keeping all the rule data in a list comes at a slight cost in efficiency,
  but it makes the code that calls the rules a LOT more readable, e.g.,
  get_all_rule_names/2, desired_inference_rule/2, apply_one_rule_2/5.

  RuleName:        An arbitrary Atom identifying the name of the rule.

  AP1Rel:          The relation that must be in the first AP

  AP1Subj,
  AP1Obj:          Variables to be instantiated to the subject and object of the first AP

  AP1SubjSemTypes,
  AP1ObjSemTypes:  The allowable SemTypes for the subject and object of the first AP

  APSemTypes12:    The allowable SemTypes for the Object argument of the first AP

  AP2Rel:          The relation that must be in the second AP

  AP2Subj,
  AP2Obj:          Variables to be instantiated to the subject and object of the second AP

  AP2SubjSemTypes,
  AP2ObjSemTypes:  The allowable SemTypes for the Subject argument of the second AP

  CPRel:           The relation in the CP

  CPSubj,
  CPObj:           Variables to be instantiated to the subject and object of the CP

  Note: There are no SemType restrictions on the CP predications.

  Note: See determine_desired_semtypes/7 predicate below. Changes in inference rules
  may need to be reflected in that predicate.
*/

% Original type-1 rule 1
% EX: Aspirin treats patients with headache.
%     -------        --------      --------
%        X               Y             Z

inference_rule(old11,
	       [ 'TREATS',     X, Y, [], [popg,podg,aggp],
	         'PROCESS_OF',  Z, Y, [], [],    % GR OCCURS_IN no longer used
	         'TREATS',     X, Z   ]).

% Original type-1 rule 2
% EX: Rifampin treats dogs with tuberculosis.
%     --------        ----      ------------
%         X            Y             Z
inference_rule(old12,
	       [ 'TREATS',      X, Y, [], [humn,mamm],
	         'PROCESS_OF',  Z, Y, [], [],
	         'TREATS',      X, Z   ]).

% Original type-2 rule 1
% EX: Treatment with aspirin prevents headache.
%     ---------      -------          --------
%         X             Z                 Y

inference_rule(old21,
	       [ 'PREVENTS',      X, Y, [], [phsu],
	         'USES',          X, Z, [], [],
	         'PREVENTS',      Z, Y   ]).

% Original type-2 rule 1
% EX: Treatment with aspirin prevents headache.
%     ---------      -------          --------
%         X             Z                 Y

inference_rule(old22,
	       [ 'TREATS',      X, Y, [], [phsu,antb],
	         'USES',        X, Z, [], [],
	         'TREATS',      Z, Y   ]).

% ISA rule 1:
% EX: Aspirin is a medication for headache.
%     -------      ----------     --------
%        X             Y             Z

inference_rule('ISA1',
	       [ 'ISA',  X, Y, [], [],
	         VERB,   Y, Z, [], [],
	         VERB,   X, Z   ]).

% ISA rule 2:
% EX: Pneumonia is an infection of the lung.
%     ---------       ---------        -----
%         X               Y             Z

inference_rule('ISA2',
	       [ 'ISA',  X, Y, [], [],
 	         VERB,   Z, Y, [], [],
 	         VERB,   Z, X   ]).

% CA rule 1
% EX: CHF patients with VDR gene have bone loss.
%     --- --------          ----
%      X     Y                Z

inference_rule('CA1',
	       [ 'PROCESS_OF',      X, Y, [], [humn,popg,podg,aggp,mamm],
	         'PART_OF',         Z, Y, [], [],
	         'ASSOCIATED_WITH', Z, X   ]).

% CA rule 2
% EX: Lithium treats patients with CYP2B6 gene.
%     -------        --------             ----
%        X              Y                   Z

inference_rule('CA2',
	       [ 'TREATS',         X, Y, [], [humn,popg,podg,aggp,mamm,neop],
	         'PART_OF',        Z, Y, [], [],
	         'INTERACTS_WITH', Z, X   ]).

% CA rule 3
% EX: Two multiple sclerosis patients who developed pancytopenia.
%                  --------- --------               ------------
%  
%                     X        Y                       Z
% This is not a very good inference rule that is fired too often
% and we remove it.
%inference_rule('CA3',
%	       [ 'PROCESS_OF',     X, Y, [], [humn,mamm],
%	         'PROCESS_OF',     Z, Y, [], [],
%	         'COEXISTS_WITH', X, Z   ]).  

% CA rule 4
% EX: protein beta3 subunit is associated with clozapine induced treatement body weight changes.
%                   -------                    ---------                    -------------------
%                      Z                           X                                 Y

inference_rule('CA4',
	       [ 'CAUSES',          X, Y, [], [comd,dsyn,mobd,neop,patf,sosy],
	         'ASSOCIATED_WITH', Z, Y, [], [],
	         'INTERACTS_WITH',  Z, X   ]).


% CA rule 5
% EX: BDNF receptors are involved in regulating the symptoms of ADHD.
%          ---------                                --------    -----
%              X                                       Y          Z

inference_rule('CA5',
	       [ 'ASSOCIATED_WITH',  X, Y, [], [dsyn,mobd,sosy,patf],
	         'MANIFESTATION_OF', Y, Z, [], [],
	         'ASSOCIATED_WITH',  X, Z   ]).

% CA rule 6
% EX: BDNF phenotypes are important in patients taking tricyclic antidepressants
%          ----------                  --------                  ---------------
%              X                           Y                           Z

inference_rule('CA6',
	       [ 'PART_OF',         X, Y, [], [humn,popg,podg,aggp,mamm],
	         'ADMINISTERED_TO', Z, Y, [], [],
	         'INTERACTS_WITH',  X, Z   ]).

% CA rule 7
% EX: Erlotinib for tumors with epidermal growth factor receptor mutations.
%     ---------     ------                                       ---------
%         X            Y                                             Z

inference_rule('CA7',
	       [ 'TREATS',          X, Y, [], [dsyn,neop,mobd, sosy],
	         'ASSOCIATED_WITH', Z, Y, [], [],
	         'INTERACTS_WITH',  X, Z   ]).

% CA rule 8
% EX: Insulin secretory function is impaired in isolated human cells carrying BDNF.
%     -------                                                  -----          ----
%        X                                                       Y              Z

inference_rule('CA8',
	       [ 'HAS_LOCATION',   X, Y, [], [bpoc,tisu,cell,ffas,anst,emst],
	         'COEXISTS_WITH', Z, Y, [], [],
	         'INTERACTS_WITH', X, Z   ]).

% Determine the semtypes of the consequent from those of the antecedents.
% Previously, this was part of the unification in assemble_consequent_predication.
% However, semtypes of the anchor concept may not match (as in Patients-popg
% and Patients-humn in the antecedent predications), which led to issues
% in unification. This predicate accommodates these differences, based on
% the knowledge of the rule. If inference rules are modified or added,
% this predicate may also need to be updated.	       
determine_desired_semtypes(RuleName, AP1SubjSemType, _AP1ObjSemType,
			   AP2SubjSemType, _AP2ObjSemType,
			   AP1SubjSemType, AP2SubjSemType) :-
	memberchk(RuleName,[old11,old12,'CA7','CA8']),
	!.
determine_desired_semtypes(RuleName, AP1SubjSemType, _AP1ObjSemType,
			   _AP2SubjSemType, AP2ObjSemType,
			   AP1SubjSemType, AP2ObjSemType) :-
	memberchk(RuleName,['ISA1','CA5']),
	!.
determine_desired_semtypes(RuleName, _AP1SubjSemType, AP1ObjSemType,
			   _AP2SubjSemType, AP2ObjSemType,
			   AP2ObjSemType, AP1ObjSemType) :-
	memberchk(RuleName,[old21,old22]),
	!.
determine_desired_semtypes(RuleName, AP1SubjSemType, _AP1ObjSemType,
			   AP2SubjSemType, _AP2ObjSemType,
			   AP2SubjSemType, AP1SubjSemType) :-
	memberchk(RuleName,['ISA2','CA1','CA2','CA4']),
	!.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Inference Logic %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% AntecedentPredications comes in to the inference module as either
% * an uninstantiated variable, or
% a list with a variable tail
% in order to facilitate unification with the Inferences.

draw_inferences(AntecedentPredications, DebugBitVector,
		ConsequentPredicationsIN, ConsequentPredicationsOUT) :-
	( var(AntecedentPredications) ->
	  ConsequentPredicationsIN = ConsequentPredicationsOUT
	; get_all_rule_names(AllRuleNames, _),
	% ; get_all_rule_data(AllRuleData, _),
	  apply_all_inference_rules(AllRuleNames, DebugBitVector,
				    AntecedentPredications, AntecedentPredications, 
				    [], _UsedAntecedentPairs,
				    ConsequentPredicationsIN, ConsequentPredicationsOUT)	
	).

apply_all_inference_rules([], _DebugBitVector,
			  _RestAntecedentPredications, _AntecedentPredications,
			  UsedAntecedentPairs, UsedAntecedentPairs,
			  ConsequentPredications, ConsequentPredications).
apply_all_inference_rules([RuleName|RestInferenceRuleNames], DebugBitVector,
			  AntecedentPredications, AntecedentPredications,
			  UsedAntecedentPairsIn, UsedAntecedentPairsOut,
			  ConsequentPredicationsIN, ConsequentPredicationsOUT) :-
	apply_one_rule(AntecedentPredications, AntecedentPredications,
	               UsedAntecedentPairsIn,UsedAntecedentPairs,
		       DebugBitVector, RuleName,
		       ConsequentPredicationsIN, ConsequentPredicationsNEXT),
        ( var(UsedAntecedentPairs) ->
	  UsedAntecedentPairsNext = UsedAntecedentPairsIn
%        ; UsedAntecedentPairsNext = [UsedAntecedentPairsIn|UsedAntecedentPairs]
        ; append(UsedAntecedentPairsIn,UsedAntecedentPairs,UsedAntecedentPairsNextList),
	  list_to_set(UsedAntecedentPairsNextList,UsedAntecedentPairsNext)
        ),
	apply_all_inference_rules(RestInferenceRuleNames, DebugBitVector,
				  AntecedentPredications, AntecedentPredications,
				  UsedAntecedentPairsNext, UsedAntecedentPairsOut,
				  ConsequentPredicationsNEXT, ConsequentPredicationsOUT).

% First, look for inferences using
% * all remaining Antecedent Predications and
% * all Antecedent Predications

% The first time apply_one_rule is called, we know that the first two args are nonvar.
% apply_one_rule(AntecedentPredicationList, AllAntecedentPredications,


apply_one_rule([AntecedentPredication1|RestAntecedentPredications],
	       AllAntecedentPredications,
	       UsedAntecedentPairsIn, UsedAntecedentPairsOut,
	       DebugBitVector, RuleName,
	       ConsequentPredicationsIN, ConsequentPredicationsOUT) :-
	% First, look for inferences using
	% * all remaining Antecedent Predications and
	% * AntecedentPredication1 
	apply_one_rule_1(AllAntecedentPredications, AntecedentPredication1,
	                 UsedAntecedentPairsIn, UsedAntecedentPairsNext,
			 DebugBitVector, RuleName,
			 ConsequentPredicationsIN, ConsequentPredicationsNEXT),
	% Then look for inferences using
	% * the rest of the AntecedentPredications and
	% * AllAntecedentPredications
	( var(RestAntecedentPredications) ->
	  ConsequentPredicationsNEXT = ConsequentPredicationsOUT,
	  UsedAntecedentPairsNext = UsedAntecedentPairsOut
	; apply_one_rule(RestAntecedentPredications,
			 AllAntecedentPredications,
			 UsedAntecedentPairsNext, UsedAntecedentPairsOut,
			 DebugBitVector, RuleName,
			 ConsequentPredicationsNEXT, ConsequentPredicationsOUT)
	).

% Look for inferences by using
% * all remaining Antecedent Predications and
% * AntecedentPredication1

apply_one_rule_1([AntecedentPredication2|RestAntecedentPredications],
		 AntecedentPredication1,
		 UsedAntecedentPairsIn, UsedAntecedentPairsOut,
		 DebugBitVector, RuleName,
		 ConsequentPredicationsIN, ConsequentPredicationsOUT) :-

	  % Look for inferences using only
	  % * AntecedentPredication2 and
	  % * AntecedentPredication1
	  apply_one_rule_2(RuleName, DebugBitVector,
			     AntecedentPredication1, AntecedentPredication2,
			     UsedAntecedentPairsIn, UsedAntecedentPairsNext,
			     ConsequentPredicationsIN, ConsequentPredicationsNEXT),
	  ( var(RestAntecedentPredications) ->
	    ConsequentPredicationsNEXT = ConsequentPredicationsOUT,
	    UsedAntecedentPairsNext = UsedAntecedentPairsOut
	  ; % format('~nCalling Rule ~w:~nTemplate: ~q~nRule: ~q~n',
	    %        [RuleName,InferenceRuleTemplate,InferenceRule]),
	    apply_one_rule_1(RestAntecedentPredications,
			     AntecedentPredication1,
			     UsedAntecedentPairsNext, UsedAntecedentPairsOut,
			     DebugBitVector, RuleName,
			     ConsequentPredicationsNEXT, ConsequentPredicationsOUT)
	  ).

% In the code below,
% AP = Antecedent Predication, and
% CP = Consequent Predication

% Look for inferences by using
% * AntecedentPredication1 and 
% * AntecedentPredication2

apply_one_rule_2(RuleName, DebugBitVector,
		   AntecedentPredication1, AntecedentPredication2,
		   UsedAntecedentPairsIn, UsedAntecedentPairsOut, 
		   [ConsequentPredication|RestConsequentPredications],
		   RestConsequentPredications) :-

	nonvar(RuleName),
	inference_rule(RuleName, RuleData),

	RuleData = [ RequiredAP1Rel, AP1SubjStruct, AP1ObjStruct,
		     RequiredAP1SubjSemTypes, RequiredAP1ObjSemTypes,
		     RequiredAP2Rel, AP2SubjStruct, AP2ObjStruct,
		     RequiredAP2SubjSemTypes, RequiredAP2ObjSemTypes,
		     CPRel, CPSubjStruct, CPObjStruct ],
	
	% Ensure that the two antecedent predications are different.
	% Moreover, if the two relations are the same,
	% arbitrarily require that Subj1 be @< Subj2.
	% This test wasn't necessary before, because the two antecedent predications
	% always had different predicates.

	AntecedentPredication1 \== AntecedentPredication2,
	\+ memberchk(AntecedentPredication1-AntecedentPredication2,UsedAntecedentPairsIn),
	\+ memberchk(AntecedentPredication2-AntecedentPredication1,UsedAntecedentPairsIn),
	% format('~nACP~n', []),
	% format('~nReqAP1Rel: ~q~nReqAP2Rel: ~q~n', [RequiredAP1Rel, RequiredAP2Rel]),

	% announce_antecedent_predications(AntecedentPredication1, AP1Rel,
	% 				 AntecedentPredication2, AP2Rel),

	assemble_consequent_predication(DebugBitVector, RuleName,
					AntecedentPredication1, AP1SubjStruct, AP1ObjStruct,
					AP1Rel, AP1SubjSemTypeList, AP1ObjSemTypeList,
					
					AntecedentPredication2, AP2SubjStruct, AP2ObjStruct,
					AP2Rel, AP2SubjSemTypeList,AP2ObjSemTypeList,

					ConsequentPredication, CPRelInfer,
					CPSubjStruct, CPSubjSemTypeList,
					DefaultCPSubjSemType, DesiredCPSubjSemType,
					CPObjStruct,  CPObjSemTypeList,
					DefaultCPObjSemType, DesiredCPObjSemType),

	AP2Rel = RequiredAP2Rel,					
	% format('~nAP1Rel: ~q~nAP2Rel: ~q~n', [AP1Rel, AP2Rel]),
	not_both_isa(AP1Rel, AP2Rel),
	verify_relations(AP1Rel, RequiredAP1Rel, CPRel, CPRelWithNeg),
	create_consequent_predicate(CPRel, AP1Rel, CPRelWithNeg, CPRelLowerCase, CPRelInfer),

	semtype_check_all(RequiredAP1SubjSemTypes, AP1SubjSemTypeList,
			  RequiredAP1ObjSemTypes,  AP1ObjSemTypeList,
			  RequiredAP2SubjSemTypes, AP2SubjSemTypeList,
			  RequiredAP2ObjSemTypes,  AP2ObjSemTypeList),

	% format('~nAP1Rel: ~w~nAP2Rel: ~w~n', [AP1Rel, AP2Rel]),

	check_any_non_isa_relation(DebugBitVector, AP1Rel, AP2Rel,
				   CPSubjSemTypeList, CPRelLowerCase, CPObjSemTypeList,
				   DefaultCPSubjSemType, DesiredCPSubjSemType,
				   DefaultCPObjSemType,  DesiredCPObjSemType),
	% format('~nSUCCESS of Rule ~q with ~n~q~n', [RuleName, ConsequentPredication]),
	% format('~nDefault Subj: ~w~nDefault Obj: ~w~n',
	%        [DefaultCPSubjSemType, DefaultCPObjSemType],
        append(UsedAntecedentPairsIn,[AntecedentPredication1-AntecedentPredication2],
               UsedAntecedentPairsOut),
	!.

apply_one_rule_2(_RuleName, _DebugBitVector,
		   _AntecedentPredication1, _AntecedentPredication2,
		   UsedAntecedentPairs, UsedAntecedentPairs,
		   ConsequentPredications, ConsequentPredications).


% Match RequiredAP1Rel and AP1Rel to ensure that AP2's relation (AP2Rel)
% is either RequiredAP1Rel or 'NEG_' + RequiredAP1Rel
verify_relations(AP1Rel, RequiredAP1Rel, CPRel, CPRelWithNeg) :-
	( midstring(AP1Rel, 'NEG_', RequiredAP1Rel, 0, 4) ->
	  ( midstring(CPRel, 'NEG_', _, 0, 4) ->
	    CPRelWithNeg = CPRel
	  ; concat_atom(['NEG_', CPRel], CPRelWithNeg)
	  ) 
	; AP1Rel = RequiredAP1Rel,
	  CPRelWithNeg = CPRel
	).

create_consequent_predicate(CPRel, AP1Rel, CPRelWithNeg, CPRelLowerCase, CPRelInfer) :-
	lower(CPRel, CPRelLowerCase),
	get_infer_or_spec(AP1Rel, InferOrSpecAtom),
	concat_atom([CPRelWithNeg, '(', InferOrSpecAtom, ')'], CPRelInfer).	


semtype_check_all(RequiredAP1SubjSemTypes, AP1SubjSemTypeList,
		  RequiredAP1ObjSemTypes,  AP1ObjSemTypeList,
		  RequiredAP2SubjSemTypes, AP2SubjSemTypeList,
		  RequiredAP2ObjSemTypes,  AP2ObjSemTypeList) :-
	semtype_check(RequiredAP1SubjSemTypes, AP1SubjSemTypeList), 
	semtype_check(RequiredAP1ObjSemTypes,  AP1ObjSemTypeList), 
	semtype_check(RequiredAP2SubjSemTypes, AP2SubjSemTypeList), 
	semtype_check(RequiredAP2ObjSemTypes,  AP2ObjSemTypeList).

:- op(700, xfx, wreq).
% wreq(X, Y) :- X = Y.


X wreq Y :-
	format('~nChecking for unification~n~w~nand~n~w~n', [X,Y]),
	( X = Y ->
	  format('...SUCCEEDED~n', [])
	; format('...FAILED~n', []),
	  fail
	).	

% RequiredAP1Rel and RequiredAP2Rel are the relations specified in the inf_rule/14 facts.
assemble_consequent_predication(_DebugBitVector, RuleName,
				AntecedentPredication1, AP1SubjStruct, AP1ObjStruct,
				AP1Rel, AP1SubjSemTypeList, AP1ObjSemTypeList,

				AntecedentPredication2, AP2SubjStruct, AP2ObjStruct,
				AP2Rel, AP2SubjSemTypeList,AP2ObjSemTypeList,

				ConsequentPredication, CPRelInfer,
				CPSubjStruct, CPSubjSemTypeList,
				_DefaultCPSubjSemType, DesiredCPSubjSemType, 
				CPObjStruct,  CPObjSemTypeList,
				_DefaultCPObjSemType, DesiredCPObjSemType) :-

	% Unification #1
	AntecedentPredication2 =
		_AP2SubjMaxDist-_AP2SubjDist-AP2SubjIndex-
		AP2Subj-AP2SubjCUI-AP2SubjSemTypeList-AP2SubjSemType-
%		AP2Rel-_AP2RelIndex-
		_AP2IndicatorType-AP2Rel-_AP2RelIndex-
		_AP2ObjMaxDist-_AP2ObjDist-AP2ObjIndex-
		AP2Obj-AP2ObjCUI-AP2ObjSemTypeList-AP2ObjSemType,


	% Unification #2
	AP2SubjStruct = AP2SubjIndex-AP2Subj-AP2SubjCUI-AP2SubjSemTypeList,

	% Unification #3
	AP2ObjStruct  = AP2ObjIndex-AP2Obj-AP2ObjCUI-AP2ObjSemTypeList,
	
	% Unification #4
	AntecedentPredication1 =
		_AP1SubjMaxDist-_AP1SubjDist-AP1SubjIndex-
		AP1Subj-AP1SubjCUI-AP1SubjSemTypeList-AP1SubjSemType-
%		AP1Rel-AP1RelIndex-
		_AP1IndicatorType-AP1Rel-AP1RelIndex-
		_AP1ObjMaxDist-_AP1ObjDist-AP1ObjIndex-
		AP1Obj-AP1ObjCUI-AP1ObjSemTypeList-AP1ObjSemType,

	% Unification #5
	AP1SubjStruct = AP1SubjIndex-AP1Subj-AP1SubjCUI-AP1SubjSemTypeList,

	% Unification #6
	AP1ObjStruct  = AP1ObjIndex-AP1Obj-AP1ObjCUI-AP1ObjSemTypeList,

	% Unification #7
	CPSubjStruct  = CPSubjIndex-CPSubj-CPSubjCUI-CPSubjSemTypeList,
	% Unification #8
	CPObjStruct   = CPObjIndex-CPObj-CPObjCUI-CPObjSemTypeList,

	determine_desired_semtypes(RuleName,
				   AP1SubjSemType, AP1ObjSemType,
				   AP2SubjSemType, AP2ObjSemType,
				   DesiredCPSubjSemType, DesiredCPObjSemType),

	% Unification #9
	\+ ( CPSubjCUI = CPObjCUI ), 
	ConsequentPredication =
		0-0-CPSubjIndex-
		CPSubj-CPSubjCUI-CPSubjSemTypeList-DesiredCPSubjSemType-
	        'INFER'-CPRelInfer-AP1RelIndex-
		0-0-CPObjIndex-
		CPObj-CPObjCUI-CPObjSemTypeList-DesiredCPObjSemType.

% Allow the relation specified in the consequent predication
% if *any* of the subject's and object's SemTypes enter into a valid relation;
% do no type checking if either relation is ISA.

check_any_non_isa_relation(DebugBitVector, AP1Rel, AP2Rel,
			   CPSubjSemTypeList, CPRelLowerCase, CPObjSemTypeList,
			   DefaultCPSubjSemType, DesiredCPSubjSemType,
			   DefaultCPObjSemType, DesiredCPObjSemType) :-
	( AP1Rel == 'ISA' ->
	  DefaultCPSubjSemType = DesiredCPSubjSemType,
	  DefaultCPObjSemType  = DesiredCPObjSemType
	; AP2Rel == 'ISA' ->
	  DefaultCPSubjSemType = DesiredCPSubjSemType,
	  DefaultCPObjSemType  = DesiredCPObjSemType
	; check_any_relation(DebugBitVector,
			     CPSubjSemTypeList, DesiredCPSubjSemType,
			     CPRelLowerCase,
			     CPObjSemTypeList, DesiredCPObjSemType)
	).

% announce_antecedent_predications(AntecedentPredication1, AP1Rel,
% 				 AntecedentPredication2, AP2Rel) :-
% 
% 	AntecedentPredication1 \== AntecedentPredication2,
% 
% 	format('~nAntecedentPred1:~n~q~n~q~n',
% 	       [AntecedentPredication1,
% 	        AP1SubjMaxDist-AP1SubjDist-AP1SubjIndex-
% 		AP1Subj-AP1SubjCUI-AP1SubjSemTypeList-_AP1SubjSemType-
% 		AP1Rel-AP1RelIndex-
% 		_AP1ObjMaxDist-_AP1ObjDist-AP1ObjIndex-
% 		AP1Obj-AP1ObjCUI-AP1ObjSemTypeList-_AP1ObjSemType1]),
% 
% 	format('~nAntecedentPred2:~n~q~n~q~n',
% 	       [AntecedentPredication2,
% 	        AP2SubjMaxDist-AP2SubjDist-AP2SubjIndex-
% 		AP2Subj-AP2SubjCUI-AP2SubjSemTypeList-_AP2SubjSemType-
% 		AP2Rel-_AP2RelIndex- %%% ???
% 		_AP2ObjMaxDist-_AP2ObjDist-AP2ObjIndex-
% 		AP2Obj-AP2ObjCUI-AP2ObjSemTypeList-_AP2ObjSemType2]),
% 
% 
% 	( AntecedentPredication2 =
% 		AP2SubjMaxDist-AP2SubjDist-AP2SubjIndex-
% 		AP2Subj-AP2SubjCUI-AP2SubjSemTypeList-_AP2SubjSemType-
% 		AP2Rel-_AP2RelIndex- %%% ???
% 		_AP2ObjMaxDist-_AP2ObjDist-AP2ObjIndex-
% 		AP2Obj-AP2ObjCUI-AP2ObjSemTypeList-_AP2ObjSemType2 ->
%  	  format('~nAntecedent Predication 2 SUCCEEDS~n', [])
% 	; format('~nAntecedent Predication 2 FAILS~n', []),
% 	  format('~*c~n', [80,45]),
% 	  fail
% 	),
% 
% 	% The relation AP2Rel of the first semantic interpretation in the list
% 	% e.g., 'TREATS', must be the same as the relation given in the second argument
% 
%         %      _546096-'Aspirin'-phsu-        'TREATS'-'Patients'-podg
% 
% 	( AntecedentPredication1 =
% 		AP1SubjMaxDist-AP1SubjDist-AP1SubjIndex-
% 		AP1Subj-AP1SubjCUI-AP1SubjSemTypeList-_AP1SubjSemType-
% 		AP1Rel-AP1RelIndex-
% 		_Pred1ObjMaxDist-_Pred1ObjDist-AP1ObjIndex-
% 		AP1Obj-AP1ObjCUI-AP1ObjSemTypeList-_AP1ObjSemType1 ->
%       	  format('~nAntecedent Predication 1 SUCCEEDS~n', [])
% 	; format('~nAntecedent Predication 1 FAILS~n', []),
% 	  format('~*c~n', [80,45]),
% 	  fail
% 	),
% 
% 	format('~*c~n', [80,45]).
	

get_all_rule_names(AllRuleNames, ChosenRules) :-
	setof(RuleName,
	      ChosenRules^desired_inference_rule(RuleName, ChosenRules),
	      AllRuleNames).

desired_inference_rule(RuleName, ChosenRules) :-
	inference_rule(RuleName, _RuleData),
	memberchk(RuleName, ChosenRules).

get_infer_or_spec(AP1Rel, InferOrSpecAtom) :-
	( ( AP1Rel == 'ISA'
	  ; AP1Rel == 'NEG_ISA'
	  ) ->
	  InferOrSpecAtom = 'SPEC'
	; InferOrSpecAtom = 'INFER'
	).

semtype_check([],    _ActualSemTypes).
semtype_check([H|T],  ActualSemTypes) :- intersect([H|T], ActualSemTypes).


not_both_isa(RequiredAP1Rel, RequiredAP2Rel) :-
	( ( RequiredAP1Rel == 'ISA'
	  ; RequiredAP1Rel == 'NEG_ISA'
	  ) ->
	  RequiredAP2Rel \== 'ISA',
	  RequiredAP2Rel \== 'NEG_ISA'  
	; true
	).

