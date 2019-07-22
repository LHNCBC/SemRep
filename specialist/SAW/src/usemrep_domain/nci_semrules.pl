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
%      Do Not Modify This File    %
%     It is machine generated.    %
:- module(semrules_DOM,	[
	word_corresponds_to_semnet_relation_DOM/4,	multiphrase_corresponds_to_semnet_relation_DOM/6,	phrase_corresponds_to_semnet_relation_DOM/6
]).

multiphrase_corresponds_to_semnet_relation_DOM(_, _, _, _, _, _) :- !, fail.

phrase_corresponds_to_semnet_relation_DOM(_, _, _, _, _, _) :- !, fail.

% ----- Source Code Control System
%
%  Set of current semantic types, many not in UMLS Semantic Network
%  Needs to be manually updated
%  word_corresponds_to_semnet_relation_DOM( ?Word, ?POS, ?Cue, ?Relation )
%

% ----- AFFECTS / AFFECTED_BY
word_corresponds_to_semnet_relation_DOM(alter,verb,_,affects).
word_corresponds_to_semnet_relation_DOM(change,verb,in,affected_by).
word_corresponds_to_semnet_relation_DOM(impact,noun,on,affects).
word_corresponds_to_semnet_relation_DOM(impact,verb,_,affects).
word_corresponds_to_semnet_relation_DOM(infuence,verb,_,affects).
word_corresponds_to_semnet_relation_DOM(infuence,noun,on,affects).
word_corresponds_to_semnet_relation_DOM(modify,verb,_,affects).
% ----- AFFECTS_PREDISPOSITION_FOR/ PREDISPOSITION_AFFECTED_BY
word_corresponds_to_semnet_relation_DOM(risk,noun,for,affects_predisposition_for).
word_corresponds_to_semnet_relation_DOM(predisposition,noun,for,affects_predisposition_for).
word_corresponds_to_semnet_relation_DOM(risk,noun,of,affects_predisposition_for).
word_corresponds_to_semnet_relation_DOM(predisposition,noun,of,affects_predisposition_for).
% ----- EVALUATES / EVALUATED_BY
word_corresponds_to_semnet_relation_DOM(analyze,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(assess,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(assessment,noun,_,estimates).
word_corresponds_to_semnet_relation_DOM(compare,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(determine,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(estimate,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(evaluate,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(overestimate,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(revisit,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(study,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(underestimate,verb,_,estimates).
word_corresponds_to_semnet_relation_DOM(weigh,verb,_,estimates).
% ----- LOWERS_RISK_OF/LOWERS_RISK_OF
word_corresponds_to_semnet_relation_DOM(reduction,noun,in,lowers_risk_of). 
word_corresponds_to_semnet_relation_DOM(decline,noun,in,lowers_risk_of).
% ----- PARTICIPATES_IN/HAS_PARTICIPATION
word_corresponds_to_semnet_relation_DOM(involve,verb,in,participates_in).
word_corresponds_to_semnet_relation_DOM(involved,adj,in,participates_in).
word_corresponds_to_semnet_relation_DOM(attend,verb,_,participates_in).
word_corresponds_to_semnet_relation_DOM(engage,verb,in,participates_in).
word_corresponds_to_semnet_relation_DOM(engaged,adj,in,participates_in).
word_corresponds_to_semnet_relation_DOM(report,verb,_,participates_in).
word_corresponds_to_semnet_relation_DOM(participate,verb,in,participates_in).
word_corresponds_to_semnet_relation_DOM(participation,noun,in,has_participation_in).
word_corresponds_to_semnet_relation_DOM(undertake,verb,_,participates_in).
% ----- RAISES_RISK_OF/RAISES_RISK_OF
word_corresponds_to_semnet_relation_DOM(predispose,verb,_,raises_risk_of).
word_corresponds_to_semnet_relation_DOM(predisposition,noun,for,raises_risk_of).
word_corresponds_to_semnet_relation_DOM(predisposed,verb,_,raises_risk_of).
word_corresponds_to_semnet_relation_DOM(predisposed,adj,to,raises_risk_of).
word_corresponds_to_semnet_relation_DOM(increase,noun,in,raises_risk_of).
% ----- NEEDS/NEEDED_BY
word_corresponds_to_semnet_relation_DOM(need,verb,_,needs).
word_corresponds_to_semnet_relation_DOM(seek,verb,_,needs).
word_corresponds_to_semnet_relation_DOM(necessitate,verb,_,needs).
word_corresponds_to_semnet_relation_DOM(needed,adj,for,needed_by).
% -----PREFERS/PREFERRED_BY
word_corresponds_to_semnet_relation_DOM(prefer,verb,_,prefers).
word_corresponds_to_semnet_relation_DOM(preference,noun,for,has_preference).
word_corresponds_to_semnet_relation_DOM(desire,verb,_,prefers).
word_corresponds_to_semnet_relation_DOM(tend,verb,to,prefers).
% ----- PERCEIVES/PERCEIVED_BY
word_corresponds_to_semnet_relation_DOM(feel,verb,_,perceives).
word_corresponds_to_semnet_relation_DOM(perceive,verb,_,perceives).
word_corresponds_to_semnet_relation_DOM(perception,noun,_,perceives).
% -----TARGETS/TARGETED_BY
word_corresponds_to_semnet_relation_DOM(address,verb,_,targets).
word_corresponds_to_semnet_relation_DOM(focus,verb,on,targets).
word_corresponds_to_semnet_relation_DOM(reach,verb,_,targets).
word_corresponds_to_semnet_relation_DOM(target,verb,_,targets).
word_corresponds_to_semnet_relation_DOM(expect,verb,_,targets).

% stubs

multiphrase_corresponds_to_semnet_relation_GEN(_, _, _, _, _, _) :- fail.

phrase_corresponds_to_semnet_relation_GEN(_, _, _, _, _, _) :- fail.

