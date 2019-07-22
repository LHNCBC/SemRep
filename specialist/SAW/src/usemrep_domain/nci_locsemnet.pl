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
:- module(locsemnet_DOM, [
		local_preferred_relation_DOM/1,
		local_relation_inverse_DOM/2,
		local_semnet_DOM/3
%	local_semnet_1/3
	]).

:- load_files( usemrep_lib(module_version), [
		when(compile_time)
	]).

:- use_module( usemrep_lib( semnet_access ),[
		preferred_relation/2,
		relation_inverse/3
	]).

:- use_module( usemrep_lib(module_version), [
		global_module_version/1
	]).

local_semnet_DOM(Type1, Relation, Type2) :-
	( Relation == 'ISA' ->
	  true 
	; local_semnet_1_DOM(Type1, Relation, Type2) ->
	  true
	; local_relation_inverse_DOM(Relation, Inverse) ->
	  local_semnet_1_DOM(Type2, Inverse, Type1)
	; Relation \== unspecified_relation ->
%	  format('~n~n### ERROR in locsemnet: ~q is neither preferred nor inverse relation.~n~n',
%		 [Relation]),
	  fail
		).

local_relation_inverse_DOM(Relation, Inverse) :-
	global_module_version(Version),
		( relation_inverse(Version, Relation, Inverse) ->
		  true
	      ; local_relation_inverse_1_DOM(Relation, Inverse)
	).

local_preferred_relation_DOM(Relation) :-
	global_module_version(Version),
		( preferred_relation(Version, Relation) ->
		  true
	       ; local_preferred_relation_1_DOM(Relation)
).

% ----- Source Code Control System
local_preferred_relation_1_DOM(affects).
local_preferred_relation_1_DOM(affects_predisposition_for).
local_preferred_relation_1_DOM(evaluates).
local_preferred_relation_1_DOM(lowers_risk_of).
local_preferred_relation_1_DOM(needs).
local_preferred_relation_1_DOM(participates_in).
local_preferred_relation_1_DOM(perceives).
local_preferred_relation_1_DOM(prefers).
local_preferred_relation_1_DOM(raises_risk_of).
local_preferred_relation_1_DOM(targets).
local_relation_inverse_1_DOM(affects,affected_by).
local_relation_inverse_1_DOM(affects_predisposition_for,predisposition_affected_by).
local_relation_inverse_1_DOM(evaluates,evaluated_by).
local_relation_inverse_1_DOM(lowers_risk_of,lowers_risk_of).
local_relation_inverse_1_DOM(needs,needed_by).
local_relation_inverse_1_DOM(participates_in,has_participation).
local_relation_inverse_1_DOM(perceives,perceived_by).
local_relation_inverse_1_DOM(prefers,preferred_by).
local_relation_inverse_1_DOM(raises_risk_of,raises_risk_of).
local_relation_inverse_1_DOM(targets,targeted_by).
local_relation_inverse_1_DOM(affected_by,affects).
local_relation_inverse_1_DOM(predisposition_affected_by,affects_predisposition_for).
local_relation_inverse_1_DOM(evaluated_by,evaluates).
local_relation_inverse_1_DOM(lowers_risk_of,lowers_risk_of).
local_relation_inverse_1_DOM(needed_by,needs).
local_relation_inverse_1_DOM(has_participation,participates_in).
local_relation_inverse_1_DOM(perceived_by,perceives).
local_relation_inverse_1_DOM(preferred_by,prefers).
local_relation_inverse_1_DOM(raises_risk_of,raises_risk_of).
local_relation_inverse_1_DOM(targeted_by,targets).
%---------
%%% The following are Graciela's additions for NCI texts
% ADMINISTERED_TO/ HAS_ADMINISTRATION
local_semnet_1_DOM(edac,administered_to,prog). % new for medical informatics
local_semnet_1_DOM(edac,administered_to,humn).
local_semnet_1_DOM(edac,administered_to,popg).
% AFFECTS/AFFECTED_BY
local_semnet_1_DOM(infc,affects,acty).
local_semnet_1_DOM(infc,affects,mcha).
local_semnet_1_DOM(infc,affects,popg).
local_semnet_1_DOM(infc,affects,prog).
local_semnet_1_DOM(infc,affects,pref).
local_semnet_1_DOM(infc,affects,cgpr).
local_semnet_1_DOM(infc,affects,cgtn).
local_semnet_1_DOM(lbtr,affects,mcha).
local_semnet_1_DOM(lbtr,affects,pref).
local_semnet_1_DOM(resa,affects,mcha).
local_semnet_1_DOM(rslt,affects,cgpr).
local_semnet_1_DOM(rslt,affects,topp).
local_semnet_1_DOM(rslt,affects,hlca).
local_semnet_1_DOM(rslt,affects,suju).
local_semnet_1_DOM(pref,affects,optn).
local_semnet_1_DOM(pref,affects,dora).
local_semnet_1_DOM(dora,affects,suju).
local_semnet_1_DOM(dora,affects,risk).
local_semnet_1_DOM(dora,affects,umes).
local_semnet_1_DOM(optn,affects,pref).
local_semnet_1_DOM(optn,affects,cgpr).
% ----EVALUATES/EVALUATED_BY
local_semnet_1_DOM(prog,evaluates,stat).
local_semnet_1_DOM(popg,evaluates,stat).
local_semnet_1_DOM(podg,evaluates,stat).
local_semnet_1_DOM(humn,evaluates,stat).
local_semnet_1_DOM(famg,evaluates,stat).
local_semnet_1_DOM(prog,evaluates,optn).
local_semnet_1_DOM(popg,evaluates,optn).
local_semnet_1_DOM(podg,evaluates,optn).
local_semnet_1_DOM(humn,evaluates,optn).
local_semnet_1_DOM(famg,evaluates,optn).
local_semnet_1_DOM(prog,evaluates,rskf).
local_semnet_1_DOM(popg,evaluates,rskf).
local_semnet_1_DOM(podg,evaluates,rskf).
local_semnet_1_DOM(humn,evaluates,rskf).
local_semnet_1_DOM(famg,evaluates,rskf).
local_semnet_1_DOM(prog,evaluates,rslt).
local_semnet_1_DOM(popg,evaluates,rslt).
local_semnet_1_DOM(podg,evaluates,rslt).
local_semnet_1_DOM(humn,evaluates,rslt).
local_semnet_1_DOM(famg,evaluates,rslt).
local_semnet_1_DOM(famg,evaluates,risk).
local_semnet_1_DOM(prog,evaluates,risk).
local_semnet_1_DOM(popg,evaluates,risk).
local_semnet_1_DOM(podg,evaluates,risk).
local_semnet_1_DOM(humn,evaluates,risk).
local_semnet_1_DOM(prog,evaluates,typg).
local_semnet_1_DOM(popg,evaluates,typg).
local_semnet_1_DOM(podg,evaluates,typg).
local_semnet_1_DOM(humn,evaluates,typg).
local_semnet_1_DOM(humn,evaluates,suju).
local_semnet_1_DOM(popg,evaluates,suju).
local_semnet_1_DOM(famg,evaluates,typg).
local_semnet_1_DOM(prog,evaluates,issu).
local_semnet_1_DOM(popg,evaluates,issu).
local_semnet_1_DOM(podg,evaluates,issu).
local_semnet_1_DOM(humn,evaluates,issu).
local_semnet_1_DOM(famg,evaluates,issu).
local_semnet_1_DOM(prog,evaluates,bene).
local_semnet_1_DOM(popg,evaluates,bene).
local_semnet_1_DOM(podg,evaluates,bene).
local_semnet_1_DOM(humn,evaluates,bene).
local_semnet_1_DOM(famg,evaluates,bene).
local_semnet_1_DOM(humn,evaluates,umes).
% ----- LOWERS_RISK_OF/LOWERS_RISK_OF
local_semnet_1_DOM(inbe,lowers_risk_of,dsyn).
local_semnet_1_DOM(inbe,lowers_risk_of,fndg).
local_semnet_1_DOM(inbe,lowers_risk_of,inpo).
local_semnet_1_DOM(inbe,lowers_risk_of,mobd).
local_semnet_1_DOM(inbe,lowers_risk_of,neop).
local_semnet_1_DOM(dora,lowers_risk_of,dsyn).
local_semnet_1_DOM(dora,lowers_risk_of,fndg).
local_semnet_1_DOM(dora,lowers_risk_of,inpo).
local_semnet_1_DOM(dora,lowers_risk_of,mobd).
local_semnet_1_DOM(dora,lowers_risk_of,neop).
local_semnet_1_DOM(fndg,lowers_risk_of,dsyn).
local_semnet_1_DOM(fndg,lowers_risk_of,fndg).
local_semnet_1_DOM(fndg,lowers_risk_of,inpo).
local_semnet_1_DOM(fndg,lowers_risk_of,mobd).
local_semnet_1_DOM(fndg,lowers_risk_of,neop).
local_semnet_1_DOM(clna,lowers_risk_of,dsyn).
local_semnet_1_DOM(clna,lowers_risk_of,fndg).
local_semnet_1_DOM(clna,lowers_risk_of,inpo).
local_semnet_1_DOM(clna,lowers_risk_of,mobd).
local_semnet_1_DOM(clna,lowers_risk_of,neop).
local_semnet_1_DOM(atti,lowers_risk_of,dsyn).
local_semnet_1_DOM(atti,lowers_risk_of,fndg).
local_semnet_1_DOM(atti,lowers_risk_of,inpo).
local_semnet_1_DOM(atti,lowers_risk_of,mobd).
local_semnet_1_DOM(atti,lowers_risk_of,neop).
local_semnet_1_DOM(tech,lowers_risk_of,dsyn).
local_semnet_1_DOM(tech,lowers_risk_of,fndg).
local_semnet_1_DOM(tech,lowers_risk_of,inpo).
local_semnet_1_DOM(tech,lowers_risk_of,mobd).
local_semnet_1_DOM(tech,lowers_risk_of,neop).
local_semnet_1_DOM(typg,lowers_risk_of,dsyn).
local_semnet_1_DOM(typg,lowers_risk_of,fndg).
local_semnet_1_DOM(typg,lowers_risk_of,inpo).
local_semnet_1_DOM(typg,lowers_risk_of,mobd).
local_semnet_1_DOM(typg,lowers_risk_of,neop).
local_semnet_1_DOM(socb,lowers_risk_of,dsyn).
local_semnet_1_DOM(socb,lowers_risk_of,fndg).
local_semnet_1_DOM(socb,lowers_risk_of,inpo).
local_semnet_1_DOM(socb,lowers_risk_of,mobd).
local_semnet_1_DOM(socb,lowers_risk_of,neop).
% ----NEEDS/NEEDED_BY
local_semnet_1_DOM(grup,needs,diap).
local_semnet_1_DOM(grup,needs,resa).
local_semnet_1_DOM(grup,needs,infc).
local_semnet_1_DOM(grup,needs,hlca).
local_semnet_1_DOM(grup,needs,topp).
local_semnet_1_DOM(grup,needs,optn).
local_semnet_1_DOM(grup,needs,cgpr).
local_semnet_1_DOM(grup,needs,typg).
local_semnet_1_DOM(popg,needs,diap).
local_semnet_1_DOM(popg,needs,resa).
local_semnet_1_DOM(popg,needs,infc).
local_semnet_1_DOM(popg,needs,hlca).
local_semnet_1_DOM(popg,needs,topp).
local_semnet_1_DOM(popg,needs,optn).
local_semnet_1_DOM(popg,needs,cgpr).
local_semnet_1_DOM(popg,needs,typg).
local_semnet_1_DOM(podg,needs,diap).
local_semnet_1_DOM(podg,needs,resa).
local_semnet_1_DOM(podg,needs,infc).
local_semnet_1_DOM(podg,needs,hlca).
local_semnet_1_DOM(podg,needs,topp).
local_semnet_1_DOM(podg,needs,optn).
local_semnet_1_DOM(podg,needs,cgpr).
local_semnet_1_DOM(podg,needs,typg).
local_semnet_1_DOM(humn,needs,diap).
local_semnet_1_DOM(humn,needs,resa).
local_semnet_1_DOM(humn,needs,infc).
local_semnet_1_DOM(humn,needs,hlca).
local_semnet_1_DOM(humn,needs,topp).
local_semnet_1_DOM(humn,needs,optn).
local_semnet_1_DOM(humn,needs,cgpr).
local_semnet_1_DOM(humn,needs,typg).
local_semnet_1_DOM(aggp,needs,diap).
local_semnet_1_DOM(aggp,needs,resa).
local_semnet_1_DOM(aggp,needs,infc).
local_semnet_1_DOM(aggp,needs,hlca).
local_semnet_1_DOM(aggp,needs,topp).
local_semnet_1_DOM(aggp,needs,optn).
local_semnet_1_DOM(aggp,needs,cgpr).
local_semnet_1_DOM(aggp,needs,typg).
local_semnet_1_DOM(humn,needs,bene).
local_semnet_1_DOM(popg,needs,bene).
% ----- PARTICIPATES_IN/HAS_PARTICIPATION_IN
local_semnet_1_DOM(humn,participates_in,dora).
local_semnet_1_DOM(podg,participates_in,dora).
local_semnet_1_DOM(popg,participates_in,dora).
local_semnet_1_DOM(aggp,participates_in,dora).
local_semnet_1_DOM(famg,participates_in,dora).
local_semnet_1_DOM(humn,participates_in,edac). % knowledge acquisition
local_semnet_1_DOM(podg,participates_in,edac).
local_semnet_1_DOM(popg,participates_in,edac).
local_semnet_1_DOM(aggp,participates_in,edac).
local_semnet_1_DOM(famg,participates_in,edac).
local_semnet_1_DOM(humn,participates_in,evnt). % race, festival, marathon
local_semnet_1_DOM(podg,participates_in,evnt).
local_semnet_1_DOM(popg,participates_in,evnt).
local_semnet_1_DOM(aggp,participates_in,evnt).
local_semnet_1_DOM(famg,participates_in,evnt).
local_semnet_1_DOM(humn,participates_in,acty).
local_semnet_1_DOM(podg,participates_in,acty).
local_semnet_1_DOM(popg,participates_in,acty).
local_semnet_1_DOM(aggp,participates_in,acty).
local_semnet_1_DOM(famg,participates_in,acty).
local_semnet_1_DOM(humn,participates_in,inbe). % smoking
local_semnet_1_DOM(podg,participates_in,inbe).
local_semnet_1_DOM(popg,participates_in,inbe).
local_semnet_1_DOM(aggp,participates_in,inbe).
local_semnet_1_DOM(famg,participates_in,inbe).
local_semnet_1_DOM(humn,participates_in,socb).
local_semnet_1_DOM(podg,participates_in,socb).
local_semnet_1_DOM(popg,participates_in,socb).
local_semnet_1_DOM(aggp,participates_in,socb).
local_semnet_1_DOM(famg,participates_in,socb).
local_semnet_1_DOM(humn,participates_in,cgpr). % decision-making
local_semnet_1_DOM(podg,participates_in,cgpr).
local_semnet_1_DOM(popg,participates_in,cgpr).
local_semnet_1_DOM(aggp,participates_in,cgpr).
local_semnet_1_DOM(famg,participates_in,cgpr).
local_semnet_1_DOM(grup,participates_in,dora).
local_semnet_1_DOM(grup,participates_in,edac).
local_semnet_1_DOM(grup,participates_in,evnt).
local_semnet_1_DOM(grup,participates_in,acty).
local_semnet_1_DOM(grup,participates_in,inbe).
local_semnet_1_DOM(grup,participates_in,socb).
local_semnet_1_DOM(grup,participates_in,cgpr).
% ----- PREFERS/HAS_PREFERENCE
local_semnet_1_DOM(grup,prefers,hlca).
local_semnet_1_DOM(grup,prefers,tech).
local_semnet_1_DOM(grup,prefers,topp).
local_semnet_1_DOM(grup,prefers,optn).
local_semnet_1_DOM(grup,prefers,cgpr).
local_semnet_1_DOM(grup,prefers,typg).
local_semnet_1_DOM(popg,prefers,hlca).
local_semnet_1_DOM(popg,prefers,tech).
local_semnet_1_DOM(popg,prefers,topp).
local_semnet_1_DOM(popg,prefers,optn).
local_semnet_1_DOM(popg,prefers,cgpr).
local_semnet_1_DOM(popg,prefers,typg).
local_semnet_1_DOM(podg,prefers,hlca).
local_semnet_1_DOM(podg,prefers,tech).
local_semnet_1_DOM(podg,prefers,topp).
local_semnet_1_DOM(podg,prefers,optn).
local_semnet_1_DOM(podg,prefers,cgpr).
local_semnet_1_DOM(podg,prefers,typg).
local_semnet_1_DOM(humn,prefers,hlca).
local_semnet_1_DOM(humn,prefers,tech).
local_semnet_1_DOM(humn,prefers,topp).
local_semnet_1_DOM(humn,prefers,optn).
local_semnet_1_DOM(humn,prefers,cgpr).
local_semnet_1_DOM(humn,prefers,typg).
local_semnet_1_DOM(humn,prefers,dora).
local_semnet_1_DOM(aggp,prefers,hlca).
local_semnet_1_DOM(aggp,prefers,tech).
local_semnet_1_DOM(aggp,prefers,topp).
local_semnet_1_DOM(aggp,prefers,optn).
local_semnet_1_DOM(aggp,prefers,cgpr).
local_semnet_1_DOM(aggp,prefers,typg).
% ----- PERCEIVES/PERCEIVED_BY
local_semnet_1_DOM(podg,perceives,bene).
local_semnet_1_DOM(popg,perceives,bene).
local_semnet_1_DOM(humn,perceives,bene).
local_semnet_1_DOM(aggp,perceives,bene).
local_semnet_1_DOM(podg,perceives,risk).
local_semnet_1_DOM(popg,perceives,risk).
local_semnet_1_DOM(humn,perceives,risk).
local_semnet_1_DOM(aggp,perceives,risk).
local_semnet_1_DOM(podg,perceives,stat).
local_semnet_1_DOM(popg,perceives,stat).
local_semnet_1_DOM(humn,perceives,stat).
local_semnet_1_DOM(aggp,perceives,stat).
local_semnet_1_DOM(podg,perceives,issu).
local_semnet_1_DOM(popg,perceives,issu).
local_semnet_1_DOM(humn,perceives,issu).
local_semnet_1_DOM(aggp,perceives,issu).
% ----- RAISES_RISK_OF/RAISES_RISK_OF
local_semnet_1_DOM(inbe,raises_risk_of,dsyn).
local_semnet_1_DOM(inbe,raises_risk_of,fndg).
local_semnet_1_DOM(inbe,raises_risk_of,inpo).
local_semnet_1_DOM(inbe,raises_risk_of,mobd).
local_semnet_1_DOM(inbe,raises_risk_of,neop).
local_semnet_1_DOM(dora,raises_risk_of,dsyn).
local_semnet_1_DOM(dora,raises_risk_of,fndg).
local_semnet_1_DOM(dora,raises_risk_of,inpo).
local_semnet_1_DOM(dora,raises_risk_of,mobd).
local_semnet_1_DOM(dora,raises_risk_of,neop).
local_semnet_1_DOM(fndg,raises_risk_of,dsyn).
local_semnet_1_DOM(fndg,raises_risk_of,fndg).
local_semnet_1_DOM(fndg,raises_risk_of,inpo).
local_semnet_1_DOM(fndg,raises_risk_of,mobd).
local_semnet_1_DOM(fndg,raises_risk_of,neop).
local_semnet_1_DOM(clna,raises_risk_of,dsyn).
local_semnet_1_DOM(clna,raises_risk_of,fndg).
local_semnet_1_DOM(clna,raises_risk_of,inpo).
local_semnet_1_DOM(clna,raises_risk_of,mobd).
local_semnet_1_DOM(clna,raises_risk_of,neop).
local_semnet_1_DOM(atti,raises_risk_of,dsyn).
local_semnet_1_DOM(atti,raises_risk_of,fndg).
local_semnet_1_DOM(atti,raises_risk_of,inpo).
local_semnet_1_DOM(atti,raises_risk_of,mobd).
local_semnet_1_DOM(atti,raises_risk_of,neop).
local_semnet_1_DOM(pref,raises_risk_of,dsyn).
local_semnet_1_DOM(pref,raises_risk_of,fndg).
local_semnet_1_DOM(pref,raises_risk_of,inpo).
local_semnet_1_DOM(pref,raises_risk_of,mobd).
local_semnet_1_DOM(pref,raises_risk_of,neop).
local_semnet_1_DOM(rskf,raises_risk_of,dsyn).
local_semnet_1_DOM(rskf,raises_risk_of,fndg).
local_semnet_1_DOM(rskf,raises_risk_of,inpo).
local_semnet_1_DOM(rskf,raises_risk_of,mobd).
local_semnet_1_DOM(rskf,raises_risk_of,neop).
local_semnet_1_DOM(socb,raises_risk_of,dsyn).
local_semnet_1_DOM(socb,raises_risk_of,fndg).
local_semnet_1_DOM(socb,raises_risk_of,inpo).
local_semnet_1_DOM(socb,raises_risk_of,mobd).
local_semnet_1_DOM(socb,raises_risk_of,neop).
local_semnet_1_DOM(ment,raises_risk_of,dsyn).
local_semnet_1_DOM(ment,raises_risk_of,fndg).
local_semnet_1_DOM(ment,raises_risk_of,inpo).
local_semnet_1_DOM(ment,raises_risk_of,mobd).
local_semnet_1_DOM(ment,raises_risk_of,neop).
local_semnet_1_DOM(risk,raises_risk_of,dsyn).
local_semnet_1_DOM(risk,raises_risk_of,fndg).
local_semnet_1_DOM(risk,raises_risk_of,inpo).
local_semnet_1_DOM(risk,raises_risk_of,mobd).
local_semnet_1_DOM(risk,raises_risk_of,neop).
local_semnet_1_DOM(issu,raises_risk_of,dsyn).
local_semnet_1_DOM(issu,raises_risk_of,fndg).
local_semnet_1_DOM(issu,raises_risk_of,inpo).
local_semnet_1_DOM(issu,raises_risk_of,mobd).
local_semnet_1_DOM(issu,raises_risk_of,neop).
local_semnet_1_DOM(rslt,raises_risk_of,dsyn).
local_semnet_1_DOM(rslt,raises_risk_of,fndg).
local_semnet_1_DOM(rslt,raises_risk_of,inpo).
local_semnet_1_DOM(rslt,raises_risk_of,mobd).
local_semnet_1_DOM(rslt,raises_risk_of,neop).
local_semnet_1_DOM(suju,raises_risk_of,dsyn).
local_semnet_1_DOM(suju,raises_risk_of,fndg).
local_semnet_1_DOM(suju,raises_risk_of,inpo).
local_semnet_1_DOM(suju,raises_risk_of,mobd).
local_semnet_1_DOM(suju,raises_risk_of,neop).
% -----TARGETS/TARGETED_BY
local_semnet_1_DOM(dora,targets,dsyn).
local_semnet_1_DOM(dora,targets,neop).
local_semnet_1_DOM(dora,targets,humn).
local_semnet_1_DOM(dora,targets,popg).
local_semnet_1_DOM(dora,targets,podg).
local_semnet_1_DOM(dora,targets,rslt).
local_semnet_1_DOM(infc,targets,dsyn).
local_semnet_1_DOM(infc,targets,neop).
local_semnet_1_DOM(infc,targets,humn).
local_semnet_1_DOM(infc,targets,popg).
local_semnet_1_DOM(infc,targets,podg).
local_semnet_1_DOM(typg,targets,dsyn).
local_semnet_1_DOM(typg,targets,neop).
local_semnet_1_DOM(typg,targets,humn).
local_semnet_1_DOM(typg,targets,popg).
local_semnet_1_DOM(typg,targets,podg).
local_semnet_1_DOM(typg,targets,issu).
local_semnet_1_DOM(typg,targets,suju).
local_semnet_1_DOM(typg,targets,umes).
local_semnet_1_DOM(typg,targets,rskf).
local_semnet_1_DOM(hlca,targets,dsyn).
local_semnet_1_DOM(hlca,targets,neop).
local_semnet_1_DOM(hlca,targets,humn).
local_semnet_1_DOM(hlca,targets,popg).
local_semnet_1_DOM(hlca,targets,podg).
local_semnet_1_DOM(hlca,targets,issu).
local_semnet_1_DOM(hlca,targets,suju).
local_semnet_1_DOM(hlca,targets,umes).
local_semnet_1_DOM(hlca,targets,risk).
local_semnet_1_DOM(topp,targets,dsyn).
local_semnet_1_DOM(topp,targets,neop).
local_semnet_1_DOM(topp,targets,humn).
local_semnet_1_DOM(topp,targets,popg).
local_semnet_1_DOM(topp,targets,podg).
local_semnet_1_DOM(topp,targets,issu).
local_semnet_1_DOM(topp,targets,umes).
local_semnet_1_DOM(topp,targets,risk).

