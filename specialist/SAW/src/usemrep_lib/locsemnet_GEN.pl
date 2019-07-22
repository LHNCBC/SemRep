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
:- module(locsemnet_GEN, [
		local_preferred_relation_GEN/1,
		local_relation_inverse_GEN/2,
		local_semnet_GEN/3
%	local_semnet_1_GEN/3
	]).

:- load_files( usemrep_lib(module_version), [
		when(compile_time)
	]).

:- use_module( usemrep_lib(module_version), [
		global_module_version/1
	]).

:- use_module( usemrep_lib( semnet_access ),[
		preferred_relation/2,
		relation_inverse/3
	]).

local_semnet_GEN(Type1, Relation, Type2) :-
	( Relation == 'ISA' ->
	  true 
	; local_semnet_1_GEN(Type1, Relation, Type2) ->
	  true
	; local_relation_inverse_GEN(Relation, Inverse) ->
	  local_semnet_1_GEN(Type2, Inverse, Type1)
	; Relation \== unspecified_relation ->
%	  format(user_put, '~n~n### ERROR in locsemnet: ~q is neither preferred nor inverse relation.~n~n',
%		 [Relation]),
	  fail
		).

local_relation_inverse_GEN(Relation, Inverse) :-
	global_module_version(Version),
		( relation_inverse(Version, Relation, Inverse) ->
		  true
	      ; local_relation_inverse_1_GEN(Relation, Inverse)
	).

local_preferred_relation_GEN(Relation) :-
	global_module_version(Version),
		( preferred_relation(Version, Relation) ->
		  true
	       ; local_preferred_relation_1_GEN(Relation)
).

%      Do Not Modify This File    %
%     It is machine generated.    %
%	local_semnet_1_GEN/3
%	  format(user_put, '~n~n### ERROR in locsemnet: ~q is neither preferred nor inverse relation.~n~n',
%		 [Relation]),
%          local_semnet_1_GEN/3 
%:- set_global_module_version('06').
%:- use_versioned_module( usemrep_lib( semnet_access ),
%	locsemnet, [
%	        preferred_relation/1,
%	        relation_inverse/2
%   ]).
%:- use_module( usemrep_lib( semnet_access_2012AA ), [
%	        preferred_relation/1,
%	        relation_inverse/2
%   ]).
%:- use_module( usemrep_lib( semnet_access06 ), [
%	        preferred_relation/1,
%	        relation_inverse/2
%   ]).
% 		 [Relation]),
local_relation_inverse_1_GEN(administered_to,has_administration).
local_relation_inverse_1_GEN(affects,affected_by).
local_relation_inverse_1_GEN(associated_with,associated_with_r).
local_relation_inverse_1_GEN(augments,has_augmentation).
local_relation_inverse_1_GEN(causes,caused_by).
local_relation_inverse_1_GEN(coexists_with,coexists_with).
local_relation_inverse_1_GEN(complicates,complicated_by).
local_relation_inverse_1_GEN(co-occurs_with,co-occurs_with).
local_relation_inverse_1_GEN(converts_to,has_conversion).
%local_relation_inverse_1_GEN(enhances,enhanced_by).
local_relation_inverse_1_GEN(diagnoses,diagnosed_by).
local_relation_inverse_1_GEN(disrupts,disrupted_by).
local_relation_inverse_1_GEN(indicates,indicated_by).
local_relation_inverse_1_GEN(inhibits,inhibited_by).
local_relation_inverse_1_GEN(interacts_with,interacts_with).
local_relation_inverse_1_GEN(location_of,has_location).
local_relation_inverse_1_GEN(manifestation_of,has_manifestation).
local_relation_inverse_1_GEN(method_of,has_method).
local_relation_inverse_1_GEN(occurs_in,has_occurrence).
local_relation_inverse_1_GEN(part_of,has_part).
local_relation_inverse_1_GEN(precedes,follows).
local_relation_inverse_1_GEN(predisposes,predisposed_by).
local_relation_inverse_1_GEN(prevents,prevented_by).
local_relation_inverse_1_GEN(process_of,has_process).
local_relation_inverse_1_GEN(produces,produced_by).
local_relation_inverse_1_GEN(stimulates,stimulated_by).
local_relation_inverse_1_GEN(treats,treated_by).
local_relation_inverse_1_GEN(uses,used_by).
local_relation_inverse_1_GEN(has_administration,administered_to).
local_relation_inverse_1_GEN(affected_by,affects).
local_relation_inverse_1_GEN(caused_by,causes).
local_relation_inverse_1_GEN(complicated_by,complicates).
local_relation_inverse_1_GEN(diagnosed_by,diagnoses).
local_relation_inverse_1_GEN(disrupted_by,disrupts).
%local_relation_inverse_1_GEN(enhanced_by,enhances).
local_relation_inverse_1_GEN(follows,precedes).
local_relation_inverse_1_GEN(indicated_by,indicates).
local_relation_inverse_1_GEN(inhibited_by,inhibits).
local_relation_inverse_1_GEN(interacts_with,interacts_with).
local_relation_inverse_1_GEN(predisposed_by,predisposes).
local_relation_inverse_1_GEN(prevented_by,prevents).
local_relation_inverse_1_GEN(stimulated_by,stimulates).
local_relation_inverse_1_GEN(associated_with_r,associated_with).
local_relation_inverse_1_GEN(coexists_with,coexists_with).
local_relation_inverse_1_GEN(co-occurs_with,co-occurs_with).
local_relation_inverse_1_GEN(diagnosed_by,diagnoses).
local_relation_inverse_1_GEN(has_augmentation,augments).
local_relation_inverse_1_GEN(has_occurrence,occurs_in).
local_relation_inverse_1_GEN(has_conversion,converts_to).
local_relation_inverse_1_GEN(has_location,location_of).
local_relation_inverse_1_GEN(has_method,method_of).
local_relation_inverse_1_GEN(has_part,part_of).
local_relation_inverse_1_GEN(has_process,process_of).
local_relation_inverse_1_GEN(has_manifestation,manifestation_of).
local_relation_inverse_1_GEN(produced_by,produces).
local_relation_inverse_1_GEN(treated_by,treats).
local_relation_inverse_1_GEN(used_by,uses).
local_preferred_relation_1_GEN(administered_to).
local_preferred_relation_1_GEN(affects).
local_preferred_relation_1_GEN(associated_with).
local_preferred_relation_1_GEN(augments).
local_preferred_relation_1_GEN(causes).
local_preferred_relation_1_GEN(coexists_with).
local_preferred_relation_1_GEN(complicates).
local_preferred_relation_1_GEN(converts_to).
local_preferred_relation_1_GEN(co-occurs_with).
local_preferred_relation_1_GEN(diagnoses).
local_preferred_relation_1_GEN(disrupts).
%local_preferred_relation_1_GEN(enhances).
local_preferred_relation_1_GEN(indicates).
local_preferred_relation_1_GEN(inhibits).
local_preferred_relation_1_GEN(interacts_with).
local_preferred_relation_1_GEN(location_of).
local_preferred_relation_1_GEN(manifestation_of).
local_preferred_relation_1_GEN(method_of).
local_preferred_relation_1_GEN(occurrs_in).
local_preferred_relation_1_GEN(part_of).
local_preferred_relation_1_GEN(precedes).
local_preferred_relation_1_GEN(predisposes).
local_preferred_relation_1_GEN(prevents).
local_preferred_relation_1_GEN(process_of).
local_preferred_relation_1_GEN(produces).
local_preferred_relation_1_GEN(stimulates).
local_preferred_relation_1_GEN(treats).
local_preferred_relation_1_GEN(uses).
% FML added in order to get "rifampin treats dogs with tuberculosis".
% FML added in order to get "rifampin treats dogs with tuberculosis".
% FML added in order to get "rifampin treats dogs with tuberculosis".
local_semnet_1_GEN(antb,treats,mamm).
local_semnet_1_GEN(horm,treats,mamm).
local_semnet_1_GEN(vita,treats,mamm).
% local_semnet_1_GEN(topp, affects, aggp).  %MF
% local_semnet_1_GEN(topp, affects, famg).  %MF
local_semnet_1_GEN(antb,treats,famg). %MF
local_semnet_1_GEN(antb,treats,aggp). %MF
local_semnet_1_GEN(antb,treats,podg). %MF
local_semnet_1_GEN(medd,treats,podg).
local_semnet_1_GEN(phsu,treats,podg).
local_semnet_1_GEN(topp,treats,podg).
local_semnet_1_GEN(medd,treats,popg).
local_semnet_1_GEN(phsu,treats,popg).
local_semnet_1_GEN(topp,treats,popg).
local_semnet_1_GEN(medd,treats,humn).
local_semnet_1_GEN(phsu,treats,humn).
local_semnet_1_GEN(topp,treats,humn).
local_semnet_1_GEN(medd,treats,mamm).
local_semnet_1_GEN(phsu,treats,mamm).
local_semnet_1_GEN(topp,treats,mamm).
local_semnet_1_GEN(topp,treats,famg). %MF
local_semnet_1_GEN(topp,treats,aggp). %MF
%local_semnet_1_GEN(medd,treats,dsyn). % in Semantic Network '06
local_semnet_1_GEN(phsu,treats,famg). %MF
local_semnet_1_GEN(phsu,treats,aggp). %MF
local_semnet_1_GEN(lipd,treats,dsyn). % GR 06/2015
local_semnet_1_GEN(vita,treats,dsyn). %MF
local_semnet_1_GEN(vita,treats,famg). %MF
local_semnet_1_GEN(vita,treats,aggp). %MF
local_semnet_1_GEN(horm,treats,dsyn). %MF Dangerous Ideally Corticosteroids should be phsu
local_semnet_1_GEN(horm,treats,famg). %MF
local_semnet_1_GEN(horm,treats,aggp). %MF
local_semnet_1_GEN(horm,treats,podg). %MF
% NHLBI project maybe there us a need to add more
% PREVENTS
local_semnet_1_GEN(dora,prevents,dsyn). %MF NHLBI
local_semnet_1_GEN(hlca,prevents,dsyn). %MF NHLBI
local_semnet_1_GEN(dora,prevents,fndg). %MF NHLBI
local_semnet_1_GEN(hlca,prevents,fndg). %MF NHLBI
local_semnet_1_GEN(dora,prevents,sosy). %MF NHLBI
local_semnet_1_GEN(hlca,prevents,sosy). %MF NHLBI
local_semnet_1_GEN(dora,prevents,patf). %MF NHLBI
local_semnet_1_GEN(hlca,prevents,patf). %MF NHLBI
% TREATS
local_semnet_1_GEN(dora,treats,dsyn). %MF NHLBI
local_semnet_1_GEN(hlca,treats,dsyn). %MF NHLBI
local_semnet_1_GEN(dora,treats,humn). %MF NHLBI
local_semnet_1_GEN(hlca,treats,humn). %MF NHLBI
local_semnet_1_GEN(dora,treats,fndg). %MF NHLBI
local_semnet_1_GEN(hlca,treats,fndg). %MF NHLBI
local_semnet_1_GEN(dora,treats,sosy). %MF NHLBI
local_semnet_1_GEN(hlca,treats,sosy). %MF NHLBI
local_semnet_1_GEN(dora,treats,patf). %MF NHLBI
local_semnet_1_GEN(hlca,treats,patf). %MF NHLBI
%AFFECTS
local_semnet_1_GEN(edac,affects,dsyn). %MF NHLBI
local_semnet_1_GEN(edac,affects,orga). %MF NHLBI
local_semnet_1_GEN(edac,affects,fndg). %MF NHLBI
local_semnet_1_GEN(edac,affects,sosy). %MF NHLBI
%---------
% Old prevents
local_semnet_1_GEN(vita,prevents,dsyn).
local_semnet_1_GEN(imft,prevents,dsyn). % GR 06/2015
local_semnet_1_GEN(medd,prevents,sosy).
local_semnet_1_GEN(phsu,prevents,sosy).
local_semnet_1_GEN(topp,prevents,sosy).
local_semnet_1_GEN(topp,prevents,inpo). %MF
local_semnet_1_GEN(topp,precedes,topp).
local_semnet_1_GEN(topp,follows,topp).
local_semnet_1_GEN(podg,has_occurrence,sosy). %MF
local_semnet_1_GEN(sosy,caused_by,dsyn). %MF
% for cortex.pl
%local_semnet_1_GEN(bpoc,unspecified_relation,bpoc).
%These were all removed by MF it gives more false positives than benefit
%local_semnet_1_GEN(bacs,treats,podg).
%local_semnet_1_GEN(bacs,treats,popg).
%local_semnet_1_GEN(bacs,treats,humn).
%local_semnet_1_GEN(bacs,treats,anab).
%local_semnet_1_GEN(bacs,treats,inpo).
%local_semnet_1_GEN(bacs,treats,sosy).
%local_semnet_1_GEN(bacs,treats,dsyn).
%local_semnet_1_GEN(bacs,treats,neop).
%local_semnet_1_GEN(bacs,treats,mobd).
%local_semnet_1_GEN(food,treats,dsyn). %MF innapropriate
%%%% Caroline's Semantic Network Additions for pharmacogenetic SEMRep %%%%%
% ADMINISTERED_TO/HAS_ADMINISTRATION
local_semnet_1_GEN(phsu,administered_to,humn).
local_semnet_1_GEN(humn,has_administration,phsu).
local_semnet_1_GEN(antb,administered_to,humn).
local_semnet_1_GEN(bacs,administered_to,humn).
local_semnet_1_GEN(imft,administered_to,humn).
local_semnet_1_GEN(vita,administered_to,humn).
local_semnet_1_GEN(enzy,administered_to,humn).
local_semnet_1_GEN(horm,administered_to,humn).
local_semnet_1_GEN(rcpt,administered_to,humn).
local_semnet_1_GEN(nsba,administered_to,humn).
local_semnet_1_GEN(hops,administered_to,humn).
local_semnet_1_GEN(inch,administered_to,humn).
local_semnet_1_GEN(orch,administered_to,humn).
local_semnet_1_GEN(lipd,administered_to,humn).
local_semnet_1_GEN(eico,administered_to,humn).
local_semnet_1_GEN(strd,administered_to,humn).
local_semnet_1_GEN(elii,administered_to,humn).
local_semnet_1_GEN(opco,administered_to,humn).
local_semnet_1_GEN(nusq,administered_to,humn).
local_semnet_1_GEN(carb,administered_to,humn).
local_semnet_1_GEN(phsu,administered_to,mamm).
local_semnet_1_GEN(antb,administered_to,mamm).
local_semnet_1_GEN(bacs,administered_to,mamm).
local_semnet_1_GEN(imft,administered_to,mamm).
local_semnet_1_GEN(vita,administered_to,mamm).
local_semnet_1_GEN(enzy,administered_to,mamm).
local_semnet_1_GEN(horm,administered_to,mamm).
local_semnet_1_GEN(rcpt,administered_to,mamm).
local_semnet_1_GEN(nsba,administered_to,mamm).
local_semnet_1_GEN(hops,administered_to,mamm).
local_semnet_1_GEN(inch,administered_to,mamm).
local_semnet_1_GEN(orch,administered_to,mamm).
local_semnet_1_GEN(lipd,administered_to,mamm).
local_semnet_1_GEN(eico,administered_to,mamm).
local_semnet_1_GEN(strd,administered_to,mamm).
local_semnet_1_GEN(elii,administered_to,mamm).
local_semnet_1_GEN(opco,administered_to,mamm).
local_semnet_1_GEN(nusq,administered_to,mamm).
local_semnet_1_GEN(carb,administered_to,mamm).
local_semnet_1_GEN(phsu,administered_to,anim).
local_semnet_1_GEN(antb,administered_to,anim).
local_semnet_1_GEN(bacs,administered_to,anim).
local_semnet_1_GEN(imft,administered_to,anim).
local_semnet_1_GEN(vita,administered_to,anim).
local_semnet_1_GEN(enzy,administered_to,anim).
local_semnet_1_GEN(horm,administered_to,anim).
local_semnet_1_GEN(rcpt,administered_to,anim).
local_semnet_1_GEN(nsba,administered_to,anim).
local_semnet_1_GEN(hops,administered_to,anim).
local_semnet_1_GEN(inch,administered_to,anim).
local_semnet_1_GEN(orch,administered_to,anim).
local_semnet_1_GEN(lipd,administered_to,anim).
local_semnet_1_GEN(eico,administered_to,anim).
local_semnet_1_GEN(strd,administered_to,anim).
local_semnet_1_GEN(elii,administered_to,anim).
local_semnet_1_GEN(opco,administered_to,anim).
local_semnet_1_GEN(nusq,administered_to,anim).
local_semnet_1_GEN(carb,administered_to,anim).
local_semnet_1_GEN(phsu,administered_to,orgm).
local_semnet_1_GEN(antb,administered_to,orgm).
local_semnet_1_GEN(bacs,administered_to,orgm).
local_semnet_1_GEN(imft,administered_to,orgm).
local_semnet_1_GEN(vita,administered_to,orgm).
local_semnet_1_GEN(enzy,administered_to,orgm).
local_semnet_1_GEN(horm,administered_to,orgm).
local_semnet_1_GEN(rcpt,administered_to,orgm).
local_semnet_1_GEN(nsba,administered_to,orgm).
local_semnet_1_GEN(hops,administered_to,orgm).
local_semnet_1_GEN(inch,administered_to,orgm).
local_semnet_1_GEN(orch,administered_to,orgm).
local_semnet_1_GEN(lipd,administered_to,orgm).
local_semnet_1_GEN(eico,administered_to,orgm).
local_semnet_1_GEN(strd,administered_to,orgm).
local_semnet_1_GEN(elii,administered_to,orgm).
local_semnet_1_GEN(opco,administered_to,orgm).
local_semnet_1_GEN(nusq,administered_to,orgm).
local_semnet_1_GEN(carb,administered_to,orgm).
local_semnet_1_GEN(bacs,administered_to,cell). % GR 06/2015
local_semnet_1_GEN(topp,administered_to,mamm). % GR 06/2015
local_semnet_1_GEN(orch,administered_to,cell). % GR 05/25/2017 source: GS - PMID 15561572
local_semnet_1_GEN(antb,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(bacs,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(imft,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(vita,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(enzy,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(horm,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(rcpt,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(hops,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(strd,administered_to,emod). % GR 05/2017 source: GS
local_semnet_1_GEN(hops,administered_to,invt). % GR 05/2017 source: GS - PMID 15499431
local_semnet_1_GEN(aapp,administered_to,cell). % GR 06/05/2017 source: GENIA
local_semnet_1_GEN(nnon,administered_to,cell). % GR 06/05/2017 source: GENIA
local_semnet_1_GEN(lipd,administered_to,cell). % GR 06/27/2017 source: GS
% ASSOCIATED_WITH
%%% local_semnet_1_GEN(gngm,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(gngm,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(gngm,associated_with,celf).  % CA
% local_semnet_1_GEN(gngm,associated_with,menp).
local_semnet_1_GEN(gngm,associated_with,patf).
local_semnet_1_GEN(gngm,associated_with,dsyn).
local_semnet_1_GEN(gngm,associated_with,mobd).
local_semnet_1_GEN(gngm,associated_with,neop).
local_semnet_1_GEN(gngm,associated_with,comd).
%%% local_semnet_1_GEN(gngm,associated_with,fndg).  % CA
local_semnet_1_GEN(gngm,associated_with,sosy).
%%% local_semnet_1_GEN(gngm,associated_with,lbtr).  % CA
local_semnet_1_GEN(gngm,associated_with,inpo).
%%% local_semnet_1_GEN(phsu,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(phsu,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(phsu,associated_with,celf).  % CA
% local_semnet_1_GEN(phsu,associated_with,menp).%  CA-treats
% local_semnet_1_GEN(phsu,associated_with,patf).%  CA-treats
% local_semnet_1_GEN(phsu,associated_with,dsyn).%  CA-treats
% local_semnet_1_GEN(phsu,associated_with,mobd).%  CA-treats
% local_semnet_1_GEN(phsu,associated_with,neop).%  CA-treats
% local_semnet_1_GEN(phsu,associated_with,comd).%  CA-treats
%%% % local_semnet_1_GEN(phsu,associated_with,fndg).%  CA-treats  % CA
% local_semnet_1_GEN(phsu,associated_with,sosy).%  CA-treats
%%% % local_semnet_1_GEN(phsu,associated_with,lbtr).%  CA-treats  % CA
% local_semnet_1_GEN(phsu,associated_with,inpo).%  CA-treats
%%% % local_semnet_1_GEN(antb,associated_with,phsf).%  CA-treats  % CA
%%% % local_semnet_1_GEN(antb,associated_with,orgf).%  CA-treats  % CA
%%% % local_semnet_1_GEN(antb,associated_with,celf).%  CA-treats  % CA
% local_semnet_1_GEN(antb,associated_with,menp).%  CA-treats
%local_semnet_1_GEN(antb,associated_with,patf).
%local_semnet_1_GEN(antb,associated_with,dsyn).
%local_semnet_1_GEN(antb,associated_with,mobd).
%local_semnet_1_GEN(antb,associated_with,neop).
%local_semnet_1_GEN(antb,associated_with,comd).
%%% %local_semnet_1_GEN(antb,associated_with,fndg).  % CA
%local_semnet_1_GEN(antb,associated_with,sosy).
%%% %local_semnet_1_GEN(antb,associated_with,lbtr).  % CA
%local_semnet_1_GEN(antb,associated_with,inpo).
%%% %local_semnet_1_GEN(bacs,associated_with,phsf).  % CA
%%% %local_semnet_1_GEN(bacs,associated_with,orgf).  % CA
%%% %local_semnet_1_GEN(bacs,associated_with,celf).  % CA
% local_semnet_1_GEN(bacs,associated_with,menp).
local_semnet_1_GEN(bacs,associated_with,patf).
local_semnet_1_GEN(bacs,associated_with,dsyn).
local_semnet_1_GEN(bacs,associated_with,mobd).
local_semnet_1_GEN(bacs,associated_with,neop).
local_semnet_1_GEN(bacs,associated_with,comd).
%%% local_semnet_1_GEN(bacs,associated_with,fndg).  % CA
local_semnet_1_GEN(bacs,associated_with,sosy).
%%% local_semnet_1_GEN(bacs,associated_with,lbtr).  % CA
local_semnet_1_GEN(bacs,associated_with,inpo).
%%% local_semnet_1_GEN(imft,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(imft,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(imft,associated_with,celf).  % CA
%local_semnet_1_GEN(imft,associated_with,menp).
local_semnet_1_GEN(imft,associated_with,patf).
local_semnet_1_GEN(imft,associated_with,mobd).
local_semnet_1_GEN(imft,associated_with,neop).
local_semnet_1_GEN(imft,associated_with,comd).
%%% local_semnet_1_GEN(imft,associated_with,fndg).  % CA
local_semnet_1_GEN(imft,associated_with,sosy).
%%% local_semnet_1_GEN(imft,associated_with,lbtr).  % CA
local_semnet_1_GEN(imft,associated_with,inpo).
%%% local_semnet_1_GEN(vita,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(vita,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(vita,associated_with,celf).  % CA
%local_semnet_1_GEN(vita,associated_with,menp).
local_semnet_1_GEN(vita,associated_with,patf).
local_semnet_1_GEN(vita,associated_with,dsyn).
local_semnet_1_GEN(vita,associated_with,mobd).
local_semnet_1_GEN(vita,associated_with,neop).
local_semnet_1_GEN(vita,associated_with,comd).
%%% local_semnet_1_GEN(vita,associated_with,fndg).  % CA
local_semnet_1_GEN(vita,associated_with,sosy).
%%% local_semnet_1_GEN(vita,associated_with,lbtr).  % CA
local_semnet_1_GEN(vita,associated_with,inpo).
%%% local_semnet_1_GEN(enzy,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(enzy,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(enzy,associated_with,celf).  % CA
%local_semnet_1_GEN(enzy,associated_with,menp).
local_semnet_1_GEN(enzy,associated_with,patf).
local_semnet_1_GEN(enzy,associated_with,dsyn).
local_semnet_1_GEN(enzy,associated_with,mobd).
local_semnet_1_GEN(enzy,associated_with,neop).
local_semnet_1_GEN(enzy,associated_with,comd).
%%% local_semnet_1_GEN(enzy,associated_with,fndg).  % CA
local_semnet_1_GEN(enzy,associated_with,sosy).
%%% local_semnet_1_GEN(enzy,associated_with,lbtr).  % CA
local_semnet_1_GEN(enzy,associated_with,inpo).
%%% local_semnet_1_GEN(horm,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(horm,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(horm,associated_with,celf).  % CA
%local_semnet_1_GEN(horm,associated_with,menp).
local_semnet_1_GEN(horm,associated_with,patf).
local_semnet_1_GEN(horm,associated_with,dsyn).
local_semnet_1_GEN(horm,associated_with,mobd).
local_semnet_1_GEN(horm,associated_with,neop).
local_semnet_1_GEN(horm,associated_with,comd).
%%% local_semnet_1_GEN(horm,associated_with,fndg).  % CA
local_semnet_1_GEN(horm,associated_with,sosy).
%%% local_semnet_1_GEN(horm,associated_with,lbtr).  % CA
local_semnet_1_GEN(horm,associated_with,inpo).
%%% local_semnet_1_GEN(rcpt,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(rcpt,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(rcpt,associated_with,celf).  % CA
%local_semnet_1_GEN(rcpt,associated_with,menp).
local_semnet_1_GEN(rcpt,associated_with,patf).
local_semnet_1_GEN(rcpt,associated_with,dsyn).
local_semnet_1_GEN(rcpt,associated_with,mobd).
local_semnet_1_GEN(rcpt,associated_with,neop).
local_semnet_1_GEN(rcpt,associated_with,comd).
%%% local_semnet_1_GEN(rcpt,associated_with,fndg).  % CA
local_semnet_1_GEN(rcpt,associated_with,sosy).
%%% local_semnet_1_GEN(rcpt,associated_with,lbtr).  % CA
local_semnet_1_GEN(rcpt,associated_with,inpo).
%%% local_semnet_1_GEN(nsba,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(nsba,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(nsba,associated_with,celf).  % CA
%local_semnet_1_GEN(nsba,associated_with,menp).
local_semnet_1_GEN(nsba,associated_with,patf).
local_semnet_1_GEN(nsba,associated_with,dsyn).
local_semnet_1_GEN(nsba,associated_with,mobd).
local_semnet_1_GEN(nsba,associated_with,neop).
local_semnet_1_GEN(nsba,associated_with,comd).
%%% local_semnet_1_GEN(nsba,associated_with,fndg).  % CA
local_semnet_1_GEN(nsba,associated_with,sosy).
%%% local_semnet_1_GEN(nsba,associated_with,lbtr).  % CA
local_semnet_1_GEN(nsba,associated_with,inpo).
%%% local_semnet_1_GEN(hops,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(hops,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(hops,associated_with,celf).  % CA
%local_semnet_1_GEN(hops,associated_with,menp).
local_semnet_1_GEN(hops,associated_with,patf).
local_semnet_1_GEN(hops,associated_with,dsyn).
local_semnet_1_GEN(hops,associated_with,mobd).
local_semnet_1_GEN(hops,associated_with,neop).
local_semnet_1_GEN(hops,associated_with,comd).
%%% local_semnet_1_GEN(hops,associated_with,fndg).  % CA
local_semnet_1_GEN(hops,associated_with,sosy).
%%% local_semnet_1_GEN(hops,associated_with,lbtr).  % CA
local_semnet_1_GEN(hops,associated_with,inpo).
%%% local_semnet_1_GEN(inch,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(inch,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(inch,associated_with,celf).  % CA
%local_semnet_1_GEN(inch,associated_with,menp).
local_semnet_1_GEN(inch,associated_with,patf).
local_semnet_1_GEN(inch,associated_with,dsyn).
local_semnet_1_GEN(inch,associated_with,mobd).
local_semnet_1_GEN(inch,associated_with,neop).
local_semnet_1_GEN(inch,associated_with,comd).
%%% local_semnet_1_GEN(inch,associated_with,fndg).  % CA
local_semnet_1_GEN(inch,associated_with,sosy).
%%% local_semnet_1_GEN(inch,associated_with,lbtr).  % CA
local_semnet_1_GEN(inch,associated_with,inpo).
%%% local_semnet_1_GEN(orch,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(orch,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(orch,associated_with,celf).  % CA
%local_semnet_1_GEN(orch,associated_with,menp).
%local_semnet_1_GEN(orch,associated_with,patf).
%local_semnet_1_GEN(orch,associated_with,dsyn).
%local_semnet_1_GEN(orch,associated_with,mobd).
%local_semnet_1_GEN(orch,associated_with,neop).
%local_semnet_1_GEN(orch,associated_with,comd).
%%% %local_semnet_1_GEN(orch,associated_with,fndg).  % CA
%local_semnet_1_GEN(orch,associated_with,sosy).
%%% %local_semnet_1_GEN(orch,associated_with,lbtr).  % CA
%local_semnet_1_GEN(orch,associated_with,inpo).
%%% %local_semnet_1_GEN(lipd,associated_with,phsf).  % CA
%%% %local_semnet_1_GEN(lipd,associated_with,orgf).  % CA
%%% %local_semnet_1_GEN(lipd,associated_with,celf).  % CA
%local_semnet_1_GEN(lipd,associated_with,menp).
local_semnet_1_GEN(lipd,associated_with,patf).
local_semnet_1_GEN(lipd,associated_with,dsyn).
local_semnet_1_GEN(lipd,associated_with,mobd).
local_semnet_1_GEN(lipd,associated_with,neop).
local_semnet_1_GEN(lipd,associated_with,comd).
%%% local_semnet_1_GEN(lipd,associated_with,fndg).  % CA
local_semnet_1_GEN(lipd,associated_with,sosy).
%%% local_semnet_1_GEN(lipd,associated_with,lbtr).  % CA
local_semnet_1_GEN(lipd,associated_with,inpo).
%%% local_semnet_1_GEN(eico,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(eico,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(eico,associated_with,celf).  % CA
%local_semnet_1_GEN(eico,associated_with,menp).
local_semnet_1_GEN(eico,associated_with,patf).
local_semnet_1_GEN(eico,associated_with,dsyn).
local_semnet_1_GEN(eico,associated_with,mobd).
local_semnet_1_GEN(eico,associated_with,neop).
local_semnet_1_GEN(eico,associated_with,comd).
%%% local_semnet_1_GEN(eico,associated_with,fndg).  % CA
local_semnet_1_GEN(eico,associated_with,sosy).
%%% local_semnet_1_GEN(eico,associated_with,lbtr).  % CA
local_semnet_1_GEN(eico,associated_with,inpo).
%%% local_semnet_1_GEN(strd,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(strd,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(strd,associated_with,celf).  % CA
%local_semnet_1_GEN(strd,associated_with,menp).
local_semnet_1_GEN(strd,associated_with,patf).
local_semnet_1_GEN(strd,associated_with,dsyn).
local_semnet_1_GEN(strd,associated_with,mobd).
local_semnet_1_GEN(strd,associated_with,neop).
local_semnet_1_GEN(strd,associated_with,comd).
%%% local_semnet_1_GEN(strd,associated_with,fndg).  % CA
local_semnet_1_GEN(strd,associated_with,sosy).
%%% local_semnet_1_GEN(strd,associated_with,lbtr).  % CA
local_semnet_1_GEN(strd,associated_with,inpo).
%%% local_semnet_1_GEN(aapp,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(aapp,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(aapp,associated_with,celf).  % CA
%local_semnet_1_GEN(aapp,associated_with,menp).
local_semnet_1_GEN(aapp,associated_with,patf).
local_semnet_1_GEN(aapp,associated_with,dsyn).
local_semnet_1_GEN(aapp,associated_with,mobd).
local_semnet_1_GEN(aapp,associated_with,neop).
local_semnet_1_GEN(aapp,associated_with,comd).
%%% local_semnet_1_GEN(aapp,associated_with,fndg).  % CA
local_semnet_1_GEN(aapp,associated_with,sosy).
%%% local_semnet_1_GEN(aapp,associated_with,lbtr).  % CA
local_semnet_1_GEN(aapp,associated_with,inpo).
%%% local_semnet_1_GEN(opco,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(opco,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(opco,associated_with,celf).  % CA
%local_semnet_1_GEN(opco,associated_with,menp).
%local_semnet_1_GEN(opco,associated_with,patf).
%%local_semnet_1_GEN(opco,associated_with,dsyn).
%local_semnet_1_GEN(opco,associated_with,mobd).
%local_semnet_1_GEN(opco,associated_with,neop).
%local_semnet_1_GEN(opco,associated_with,comd).
%%% %local_semnet_1_GEN(opco,associated_with,fndg).  % CA
%local_semnet_1_GEN(opco,associated_with,sosy).
%%% %local_semnet_1_GEN(opco,associated_with,lbtr).  % CA
%local_semnet_1_GEN(opco,associated_with,inpo).
%%% %local_semnet_1_GEN(nusq,associated_with,phsf).  % CA
%%% %local_semnet_1_GEN(nusq,associated_with,orgf).  % CA
%%% %local_semnet_1_GEN(nusq,associated_with,celf).  % CA
%local_semnet_1_GEN(nusq,associated_with,menp).
local_semnet_1_GEN(nusq,associated_with,patf).
local_semnet_1_GEN(nusq,associated_with,dsyn).
local_semnet_1_GEN(nusq,associated_with,mobd).
local_semnet_1_GEN(nusq,associated_with,neop).
local_semnet_1_GEN(nusq,associated_with,comd).
%%% local_semnet_1_GEN(nusq,associated_with,fndg).  % CA
local_semnet_1_GEN(nusq,associated_with,sosy).
%%% local_semnet_1_GEN(nusq,associated_with,lbtr).  % CA
local_semnet_1_GEN(nusq,associated_with,inpo).
%%% local_semnet_1_GEN(carb,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(carb,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(carb,associated_with,celf).  % CA
%local_semnet_1_GEN(carb,associated_with,menp).
local_semnet_1_GEN(carb,associated_with,patf).
local_semnet_1_GEN(carb,associated_with,dsyn).
local_semnet_1_GEN(carb,associated_with,mobd).
local_semnet_1_GEN(carb,associated_with,neop).
local_semnet_1_GEN(carb,associated_with,comd).
%%% local_semnet_1_GEN(carb,associated_with,fndg).  % CA
local_semnet_1_GEN(carb,associated_with,sosy).
%%% local_semnet_1_GEN(carb,associated_with,lbtr).  % CA
local_semnet_1_GEN(carb,associated_with,inpo).
%%% local_semnet_1_GEN(elii,associated_with,phsf).  % CA
%%% local_semnet_1_GEN(elii,associated_with,orgf).  % CA
%%% local_semnet_1_GEN(elii,associated_with,celf).  % CA
% local_semnet_1_GEN(elii,associated_with,menp).
local_semnet_1_GEN(elii,associated_with,patf).
local_semnet_1_GEN(elii,associated_with,dsyn).
local_semnet_1_GEN(elii,associated_with,mobd).
local_semnet_1_GEN(elii,associated_with,neop).
local_semnet_1_GEN(elii,associated_with,comd).
%%% local_semnet_1_GEN(elii,associated_with,fndg).  % CA
local_semnet_1_GEN(elii,associated_with,sosy).
%%% local_semnet_1_GEN(elii,associated_with,lbtr).  % CA
local_semnet_1_GEN(elii,associated_with,inpo).
%local_semnet_1_GEN(aapp,affects,celf). % GR 05/25/2017 source: GS - PMID 15561572, Specific LDLR-mediated uptake… NOTE: let's keep under observation
%local_semnet_1_GEN(rcpt,affects,celf). % GR 05/25/2017 source: GS - PMID 15561572, Specific LDLR-mediated uptake… NOTE: let's keep under observation
local_semnet_1_GEN(sbst,affects,moft). % GR 05/26/2017 source: GS - PMID 15722357 ...a number of common contacts were observed with residues also involved in fatty acid binding.
%local_semnet_1_GEN(aapp,affects,ortf). % GR 06/27/2017 source: GS 
%local_semnet_1_GEN(aapp,affects,phsf). % GR 06/27/2017 source: GS 
% AFFECTS/AFFECTED_BY
% CAUSES
local_semnet_1_GEN(gngm,causes,phsf).
local_semnet_1_GEN(gngm,causes,orgf).
local_semnet_1_GEN(gngm,causes,celf).
% local_semnet_1_GEN(gngm, causes, menp).
local_semnet_1_GEN(gngm,causes,patf).
local_semnet_1_GEN(gngm,causes,dsyn).
local_semnet_1_GEN(gngm,causes,mobd).
local_semnet_1_GEN(gngm,causes,neop).
local_semnet_1_GEN(gngm,causes,comd).
%%% % local_semnet_1_GEN(gngm, causes, fndg).  % CA
local_semnet_1_GEN(gngm,causes,sosy).
local_semnet_1_GEN(gngm,causes,lbtr).
local_semnet_1_GEN(gngm,causes,inpo).
local_semnet_1_GEN(phsu,causes,sosy).
local_semnet_1_GEN(antb,causes,sosy).
local_semnet_1_GEN(bacs,causes,sosy).
local_semnet_1_GEN(imft,causes,sosy).
local_semnet_1_GEN(vita,causes,sosy).
local_semnet_1_GEN(enzy,causes,sosy).
local_semnet_1_GEN(horm,causes,sosy).
local_semnet_1_GEN(rcpt,causes,sosy).
local_semnet_1_GEN(nsba,causes,sosy).
local_semnet_1_GEN(hops,causes,sosy).
local_semnet_1_GEN(inch,causes,sosy).
local_semnet_1_GEN(orch,causes,sosy).
local_semnet_1_GEN(lipd,causes,sosy).
local_semnet_1_GEN(eico,causes,sosy).
local_semnet_1_GEN(strd,causes,sosy).
local_semnet_1_GEN(elii,causes,sosy).
local_semnet_1_GEN(opco,causes,sosy).
local_semnet_1_GEN(nusq,causes,sosy).
local_semnet_1_GEN(carb,causes,sosy).
local_semnet_1_GEN(nusq,causes,genf). % 10/26/2017 PMID 16185328, line 2: A single-nucleotide polymorphism (C1858T) causing an amino acid substitution (R620W) in the lymphoid protein tyrosine phosphatase gene 
% CO-OCCURS_WITH
% INHIBITS
local_semnet_1_GEN(gngm,inhibits,gngm).
local_semnet_1_GEN(gngm,inhibits,phsu).
local_semnet_1_GEN(gngm,inhibits,antb).
local_semnet_1_GEN(gngm,inhibits,bacs).
local_semnet_1_GEN(gngm,inhibits,imft).
local_semnet_1_GEN(gngm,inhibits,vita).
local_semnet_1_GEN(gngm,inhibits,enzy).
local_semnet_1_GEN(gngm,inhibits,horm).
local_semnet_1_GEN(gngm,inhibits,rcpt).
local_semnet_1_GEN(gngm,inhibits,nsba).
local_semnet_1_GEN(gngm,inhibits,hops).
local_semnet_1_GEN(gngm,inhibits,inch).
local_semnet_1_GEN(gngm,inhibits,orch).
local_semnet_1_GEN(gngm,inhibits,lipd).
local_semnet_1_GEN(gngm,inhibits,eico).
local_semnet_1_GEN(gngm,inhibits,strd).
local_semnet_1_GEN(gngm,inhibits,elii).
local_semnet_1_GEN(gngm,inhibits,opco).
local_semnet_1_GEN(gngm,inhibits,nusq).
local_semnet_1_GEN(gngm,inhibits,carb).
local_semnet_1_GEN(phsu,inhibits,gngm).
local_semnet_1_GEN(phsu,inhibits,phsu).
local_semnet_1_GEN(phsu,inhibits,antb).
local_semnet_1_GEN(phsu,inhibits,bacs).
local_semnet_1_GEN(phsu,inhibits,imft).
local_semnet_1_GEN(phsu,inhibits,vita).
local_semnet_1_GEN(phsu,inhibits,enzy).
local_semnet_1_GEN(phsu,inhibits,horm).
local_semnet_1_GEN(phsu,inhibits,rcpt).
local_semnet_1_GEN(phsu,inhibits,nsba).
local_semnet_1_GEN(phsu,inhibits,hops).
local_semnet_1_GEN(phsu,inhibits,inch).
local_semnet_1_GEN(phsu,inhibits,orch).
local_semnet_1_GEN(phsu,inhibits,lipd).
local_semnet_1_GEN(phsu,inhibits,eico).
local_semnet_1_GEN(phsu,inhibits,strd).
local_semnet_1_GEN(phsu,inhibits,elii).
local_semnet_1_GEN(phsu,inhibits,opco).
local_semnet_1_GEN(phsu,inhibits,nusq).
local_semnet_1_GEN(phsu,inhibits,carb).
local_semnet_1_GEN(antb,inhibits,gngm).
local_semnet_1_GEN(antb,inhibits,phsu).
local_semnet_1_GEN(antb,inhibits,antb).
local_semnet_1_GEN(antb,inhibits,bacs).
local_semnet_1_GEN(antb,inhibits,imft).
local_semnet_1_GEN(antb,inhibits,vita).
local_semnet_1_GEN(antb,inhibits,enzy).
local_semnet_1_GEN(antb,inhibits,horm).
local_semnet_1_GEN(antb,inhibits,rcpt).
local_semnet_1_GEN(antb,inhibits,nsba).
local_semnet_1_GEN(antb,inhibits,hops).
local_semnet_1_GEN(antb,inhibits,inch).
local_semnet_1_GEN(antb,inhibits,orch).
local_semnet_1_GEN(antb,inhibits,lipd).
local_semnet_1_GEN(antb,inhibits,eico).
local_semnet_1_GEN(antb,inhibits,strd).
local_semnet_1_GEN(antb,inhibits,elii).
local_semnet_1_GEN(antb,inhibits,opco).
local_semnet_1_GEN(antb,inhibits,nusq).
local_semnet_1_GEN(antb,inhibits,carb).
local_semnet_1_GEN(bacs,inhibits,gngm).
local_semnet_1_GEN(bacs,inhibits,phsu).
local_semnet_1_GEN(bacs,inhibits,antb).
local_semnet_1_GEN(bacs,inhibits,bacs).
local_semnet_1_GEN(bacs,inhibits,imft).
local_semnet_1_GEN(bacs,inhibits,vita).
local_semnet_1_GEN(bacs,inhibits,enzy).
local_semnet_1_GEN(bacs,inhibits,horm).
local_semnet_1_GEN(bacs,inhibits,rcpt).
local_semnet_1_GEN(bacs,inhibits,nsba).
local_semnet_1_GEN(bacs,inhibits,hops).
local_semnet_1_GEN(bacs,inhibits,inch).
local_semnet_1_GEN(bacs,inhibits,orch).
local_semnet_1_GEN(bacs,inhibits,lipd).
local_semnet_1_GEN(bacs,inhibits,eico).
local_semnet_1_GEN(bacs,inhibits,strd).
local_semnet_1_GEN(bacs,inhibits,elii).
local_semnet_1_GEN(bacs,inhibits,opco).
local_semnet_1_GEN(bacs,inhibits,nusq).
local_semnet_1_GEN(bacs,inhibits,carb).
local_semnet_1_GEN(imft,inhibits,gngm).
local_semnet_1_GEN(imft,inhibits,phsu).
local_semnet_1_GEN(imft,inhibits,antb).
local_semnet_1_GEN(imft,inhibits,bacs).
local_semnet_1_GEN(imft,inhibits,imft).
local_semnet_1_GEN(imft,inhibits,vita).
local_semnet_1_GEN(imft,inhibits,enzy).
local_semnet_1_GEN(imft,inhibits,horm).
local_semnet_1_GEN(imft,inhibits,rcpt).
local_semnet_1_GEN(imft,inhibits,nsba).
local_semnet_1_GEN(imft,inhibits,hops).
local_semnet_1_GEN(imft,inhibits,inch).
local_semnet_1_GEN(imft,inhibits,orch).
local_semnet_1_GEN(imft,inhibits,lipd).
local_semnet_1_GEN(imft,inhibits,eico).
local_semnet_1_GEN(imft,inhibits,strd).
local_semnet_1_GEN(imft,inhibits,elii).
local_semnet_1_GEN(imft,inhibits,opco).
local_semnet_1_GEN(imft,inhibits,nusq).
local_semnet_1_GEN(imft,inhibits,carb).
local_semnet_1_GEN(vita,inhibits,gngm).
local_semnet_1_GEN(vita,inhibits,phsu).
local_semnet_1_GEN(vita,inhibits,antb).
local_semnet_1_GEN(vita,inhibits,bacs).
local_semnet_1_GEN(vita,inhibits,imft).
local_semnet_1_GEN(vita,inhibits,vita).
local_semnet_1_GEN(vita,inhibits,enzy).
local_semnet_1_GEN(vita,inhibits,horm).
local_semnet_1_GEN(vita,inhibits,rcpt).
local_semnet_1_GEN(vita,inhibits,nsba).
local_semnet_1_GEN(vita,inhibits,hops).
local_semnet_1_GEN(vita,inhibits,inch).
local_semnet_1_GEN(vita,inhibits,orch).
local_semnet_1_GEN(vita,inhibits,lipd).
local_semnet_1_GEN(vita,inhibits,eico).
local_semnet_1_GEN(vita,inhibits,strd).
local_semnet_1_GEN(vita,inhibits,elii).
local_semnet_1_GEN(vita,inhibits,opco).
local_semnet_1_GEN(vita,inhibits,nusq).
local_semnet_1_GEN(vita,inhibits,carb).
local_semnet_1_GEN(enzy,inhibits,gngm).
local_semnet_1_GEN(enzy,inhibits,phsu).
local_semnet_1_GEN(enzy,inhibits,antb).
local_semnet_1_GEN(enzy,inhibits,bacs).
local_semnet_1_GEN(enzy,inhibits,imft).
local_semnet_1_GEN(enzy,inhibits,vita).
local_semnet_1_GEN(enzy,inhibits,enzy).
local_semnet_1_GEN(enzy,inhibits,horm).
local_semnet_1_GEN(enzy,inhibits,rcpt).
local_semnet_1_GEN(enzy,inhibits,nsba).
local_semnet_1_GEN(enzy,inhibits,hops).
local_semnet_1_GEN(enzy,inhibits,inch).
local_semnet_1_GEN(enzy,inhibits,orch).
local_semnet_1_GEN(enzy,inhibits,lipd).
local_semnet_1_GEN(enzy,inhibits,eico).
local_semnet_1_GEN(enzy,inhibits,strd).
local_semnet_1_GEN(enzy,inhibits,elii).
local_semnet_1_GEN(enzy,inhibits,opco).
local_semnet_1_GEN(enzy,inhibits,nusq).
local_semnet_1_GEN(enzy,inhibits,carb).
local_semnet_1_GEN(horm,inhibits,gngm).
local_semnet_1_GEN(horm,inhibits,phsu).
local_semnet_1_GEN(horm,inhibits,antb).
local_semnet_1_GEN(horm,inhibits,bacs).
local_semnet_1_GEN(horm,inhibits,imft).
local_semnet_1_GEN(horm,inhibits,vita).
local_semnet_1_GEN(horm,inhibits,enzy).
local_semnet_1_GEN(horm,inhibits,horm).
local_semnet_1_GEN(horm,inhibits,rcpt).
local_semnet_1_GEN(horm,inhibits,nsba).
local_semnet_1_GEN(horm,inhibits,hops).
local_semnet_1_GEN(horm,inhibits,inch).
local_semnet_1_GEN(horm,inhibits,orch).
local_semnet_1_GEN(horm,inhibits,lipd).
local_semnet_1_GEN(horm,inhibits,eico).
local_semnet_1_GEN(horm,inhibits,strd).
local_semnet_1_GEN(horm,inhibits,elii).
local_semnet_1_GEN(horm,inhibits,opco).
local_semnet_1_GEN(horm,inhibits,nusq).
local_semnet_1_GEN(horm,inhibits,carb).
local_semnet_1_GEN(rcpt,inhibits,gngm).
local_semnet_1_GEN(rcpt,inhibits,phsu).
local_semnet_1_GEN(rcpt,inhibits,antb).
local_semnet_1_GEN(rcpt,inhibits,bacs).
local_semnet_1_GEN(rcpt,inhibits,imft).
local_semnet_1_GEN(rcpt,inhibits,vita).
local_semnet_1_GEN(rcpt,inhibits,enzy).
local_semnet_1_GEN(rcpt,inhibits,horm).
local_semnet_1_GEN(rcpt,inhibits,rcpt).
local_semnet_1_GEN(rcpt,inhibits,nsba).
local_semnet_1_GEN(rcpt,inhibits,hops).
local_semnet_1_GEN(rcpt,inhibits,inch).
local_semnet_1_GEN(rcpt,inhibits,orch).
local_semnet_1_GEN(rcpt,inhibits,lipd).
local_semnet_1_GEN(rcpt,inhibits,eico).
local_semnet_1_GEN(rcpt,inhibits,strd).
local_semnet_1_GEN(rcpt,inhibits,elii).
local_semnet_1_GEN(rcpt,inhibits,opco).
local_semnet_1_GEN(rcpt,inhibits,nusq).
local_semnet_1_GEN(rcpt,inhibits,carb).
local_semnet_1_GEN(nsba,inhibits,gngm).
local_semnet_1_GEN(nsba,inhibits,phsu).
local_semnet_1_GEN(nsba,inhibits,antb).
local_semnet_1_GEN(nsba,inhibits,bacs).
local_semnet_1_GEN(nsba,inhibits,imft).
local_semnet_1_GEN(nsba,inhibits,vita).
local_semnet_1_GEN(nsba,inhibits,enzy).
local_semnet_1_GEN(nsba,inhibits,horm).
local_semnet_1_GEN(nsba,inhibits,rcpt).
local_semnet_1_GEN(nsba,inhibits,nsba).
local_semnet_1_GEN(nsba,inhibits,hops).
local_semnet_1_GEN(nsba,inhibits,inch).
local_semnet_1_GEN(nsba,inhibits,orch).
local_semnet_1_GEN(nsba,inhibits,lipd).
local_semnet_1_GEN(nsba,inhibits,eico).
local_semnet_1_GEN(nsba,inhibits,strd).
local_semnet_1_GEN(nsba,inhibits,elii).
local_semnet_1_GEN(nsba,inhibits,opco).
local_semnet_1_GEN(nsba,inhibits,nusq).
local_semnet_1_GEN(nsba,inhibits,carb).
local_semnet_1_GEN(hops,inhibits,gngm).
local_semnet_1_GEN(hops,inhibits,phsu).
local_semnet_1_GEN(hops,inhibits,antb).
local_semnet_1_GEN(hops,inhibits,bacs).
local_semnet_1_GEN(hops,inhibits,imft).
local_semnet_1_GEN(hops,inhibits,vita).
local_semnet_1_GEN(hops,inhibits,enzy).
local_semnet_1_GEN(hops,inhibits,horm).
local_semnet_1_GEN(hops,inhibits,rcpt).
local_semnet_1_GEN(hops,inhibits,nsba).
local_semnet_1_GEN(hops,inhibits,hops).
local_semnet_1_GEN(hops,inhibits,inch).
local_semnet_1_GEN(hops,inhibits,orch).
local_semnet_1_GEN(hops,inhibits,lipd).
local_semnet_1_GEN(hops,inhibits,eico).
local_semnet_1_GEN(hops,inhibits,strd).
local_semnet_1_GEN(hops,inhibits,elii).
local_semnet_1_GEN(hops,inhibits,opco).
local_semnet_1_GEN(hops,inhibits,nusq).
local_semnet_1_GEN(hops,inhibits,carb).
local_semnet_1_GEN(inch,inhibits,gngm).
local_semnet_1_GEN(inch,inhibits,phsu).
local_semnet_1_GEN(inch,inhibits,antb).
local_semnet_1_GEN(inch,inhibits,bacs).
local_semnet_1_GEN(inch,inhibits,imft).
local_semnet_1_GEN(inch,inhibits,vita).
local_semnet_1_GEN(inch,inhibits,enzy).
local_semnet_1_GEN(inch,inhibits,horm).
local_semnet_1_GEN(inch,inhibits,rcpt).
local_semnet_1_GEN(inch,inhibits,nsba).
local_semnet_1_GEN(inch,inhibits,hops).
local_semnet_1_GEN(inch,inhibits,inch).
local_semnet_1_GEN(inch,inhibits,orch).
local_semnet_1_GEN(inch,inhibits,lipd).
local_semnet_1_GEN(inch,inhibits,eico).
local_semnet_1_GEN(inch,inhibits,strd).
local_semnet_1_GEN(inch,inhibits,elii).
local_semnet_1_GEN(inch,inhibits,opco).
local_semnet_1_GEN(inch,inhibits,nusq).
local_semnet_1_GEN(inch,inhibits,carb).
local_semnet_1_GEN(orch,inhibits,gngm).
local_semnet_1_GEN(orch,inhibits,phsu).
local_semnet_1_GEN(orch,inhibits,antb).
local_semnet_1_GEN(orch,inhibits,bacs).
local_semnet_1_GEN(orch,inhibits,imft).
local_semnet_1_GEN(orch,inhibits,vita).
local_semnet_1_GEN(orch,inhibits,enzy).
local_semnet_1_GEN(orch,inhibits,horm).
local_semnet_1_GEN(orch,inhibits,rcpt).
local_semnet_1_GEN(orch,inhibits,nsba).
local_semnet_1_GEN(orch,inhibits,hops).
local_semnet_1_GEN(orch,inhibits,inch).
local_semnet_1_GEN(orch,inhibits,orch).
local_semnet_1_GEN(orch,inhibits,lipd).
local_semnet_1_GEN(orch,inhibits,eico).
local_semnet_1_GEN(orch,inhibits,strd).
local_semnet_1_GEN(orch,inhibits,elii).
local_semnet_1_GEN(orch,inhibits,opco).
local_semnet_1_GEN(orch,inhibits,nusq).
local_semnet_1_GEN(orch,inhibits,carb).
local_semnet_1_GEN(lipd,inhibits,gngm).
local_semnet_1_GEN(lipd,inhibits,phsu).
local_semnet_1_GEN(lipd,inhibits,antb).
local_semnet_1_GEN(lipd,inhibits,bacs).
local_semnet_1_GEN(lipd,inhibits,imft).
local_semnet_1_GEN(lipd,inhibits,vita).
local_semnet_1_GEN(lipd,inhibits,enzy).
local_semnet_1_GEN(lipd,inhibits,horm).
local_semnet_1_GEN(lipd,inhibits,rcpt).
local_semnet_1_GEN(lipd,inhibits,nsba).
local_semnet_1_GEN(lipd,inhibits,hops).
local_semnet_1_GEN(lipd,inhibits,inch).
local_semnet_1_GEN(lipd,inhibits,orch).
local_semnet_1_GEN(lipd,inhibits,lipd).
local_semnet_1_GEN(lipd,inhibits,eico).
local_semnet_1_GEN(lipd,inhibits,strd).
local_semnet_1_GEN(lipd,inhibits,elii).
local_semnet_1_GEN(lipd,inhibits,opco).
local_semnet_1_GEN(lipd,inhibits,nusq).
local_semnet_1_GEN(lipd,inhibits,carb).
local_semnet_1_GEN(eico,inhibits,gngm).
local_semnet_1_GEN(eico,inhibits,phsu).
local_semnet_1_GEN(eico,inhibits,antb).
local_semnet_1_GEN(eico,inhibits,bacs).
local_semnet_1_GEN(eico,inhibits,imft).
local_semnet_1_GEN(eico,inhibits,vita).
local_semnet_1_GEN(eico,inhibits,enzy).
local_semnet_1_GEN(eico,inhibits,horm).
local_semnet_1_GEN(eico,inhibits,rcpt).
local_semnet_1_GEN(eico,inhibits,nsba).
local_semnet_1_GEN(eico,inhibits,hops).
local_semnet_1_GEN(eico,inhibits,inch).
local_semnet_1_GEN(eico,inhibits,orch).
local_semnet_1_GEN(eico,inhibits,lipd).
local_semnet_1_GEN(eico,inhibits,eico).
local_semnet_1_GEN(eico,inhibits,strd).
local_semnet_1_GEN(eico,inhibits,elii).
local_semnet_1_GEN(eico,inhibits,opco).
local_semnet_1_GEN(eico,inhibits,nusq).
local_semnet_1_GEN(eico,inhibits,carb).
local_semnet_1_GEN(strd,inhibits,gngm).
local_semnet_1_GEN(strd,inhibits,phsu).
local_semnet_1_GEN(strd,inhibits,antb).
local_semnet_1_GEN(strd,inhibits,bacs).
local_semnet_1_GEN(strd,inhibits,imft).
local_semnet_1_GEN(strd,inhibits,vita).
local_semnet_1_GEN(strd,inhibits,enzy).
local_semnet_1_GEN(strd,inhibits,horm).
local_semnet_1_GEN(strd,inhibits,rcpt).
local_semnet_1_GEN(strd,inhibits,nsba).
local_semnet_1_GEN(strd,inhibits,hops).
local_semnet_1_GEN(strd,inhibits,inch).
local_semnet_1_GEN(strd,inhibits,orch).
local_semnet_1_GEN(strd,inhibits,lipd).
local_semnet_1_GEN(strd,inhibits,eico).
local_semnet_1_GEN(strd,inhibits,strd).
local_semnet_1_GEN(strd,inhibits,elii).
local_semnet_1_GEN(strd,inhibits,opco).
local_semnet_1_GEN(strd,inhibits,nusq).
local_semnet_1_GEN(strd,inhibits,carb).
local_semnet_1_GEN(aapp,inhibits,gngm).
local_semnet_1_GEN(aapp,inhibits,phsu).
local_semnet_1_GEN(aapp,inhibits,antb).
local_semnet_1_GEN(aapp,inhibits,bacs).
local_semnet_1_GEN(aapp,inhibits,imft).
local_semnet_1_GEN(aapp,inhibits,vita).
local_semnet_1_GEN(aapp,inhibits,enzy).
local_semnet_1_GEN(aapp,inhibits,horm).
local_semnet_1_GEN(aapp,inhibits,rcpt).
local_semnet_1_GEN(aapp,inhibits,nsba).
local_semnet_1_GEN(aapp,inhibits,hops).
local_semnet_1_GEN(aapp,inhibits,inch).
local_semnet_1_GEN(aapp,inhibits,orch).
local_semnet_1_GEN(aapp,inhibits,lipd).
local_semnet_1_GEN(aapp,inhibits,eico).
local_semnet_1_GEN(aapp,inhibits,strd).
local_semnet_1_GEN(aapp,inhibits,elii).
local_semnet_1_GEN(aapp,inhibits,opco).
local_semnet_1_GEN(aapp,inhibits,nusq).
local_semnet_1_GEN(aapp,inhibits,aapp). % GR 06/2015
local_semnet_1_GEN(aapp,inhibits,carb).
local_semnet_1_GEN(opco,inhibits,gngm).
local_semnet_1_GEN(opco,inhibits,phsu).
local_semnet_1_GEN(opco,inhibits,antb).
local_semnet_1_GEN(opco,inhibits,bacs).
local_semnet_1_GEN(opco,inhibits,imft).
local_semnet_1_GEN(opco,inhibits,vita).
local_semnet_1_GEN(opco,inhibits,enzy).
local_semnet_1_GEN(opco,inhibits,horm).
local_semnet_1_GEN(opco,inhibits,rcpt).
local_semnet_1_GEN(opco,inhibits,nsba).
local_semnet_1_GEN(opco,inhibits,hops).
local_semnet_1_GEN(opco,inhibits,inch).
local_semnet_1_GEN(opco,inhibits,orch).
local_semnet_1_GEN(opco,inhibits,lipd).
local_semnet_1_GEN(opco,inhibits,eico).
local_semnet_1_GEN(opco,inhibits,strd).
local_semnet_1_GEN(opco,inhibits,elii).
local_semnet_1_GEN(opco,inhibits,opco).
local_semnet_1_GEN(opco,inhibits,nusq).
local_semnet_1_GEN(opco,inhibits,carb).
local_semnet_1_GEN(nusq,inhibits,gngm).
local_semnet_1_GEN(nusq,inhibits,phsu).
local_semnet_1_GEN(nusq,inhibits,antb).
local_semnet_1_GEN(nusq,inhibits,bacs).
local_semnet_1_GEN(nusq,inhibits,imft).
local_semnet_1_GEN(nusq,inhibits,vita).
local_semnet_1_GEN(nusq,inhibits,enzy).
local_semnet_1_GEN(nusq,inhibits,horm).
local_semnet_1_GEN(nusq,inhibits,rcpt).
local_semnet_1_GEN(nusq,inhibits,nsba).
local_semnet_1_GEN(nusq,inhibits,hops).
local_semnet_1_GEN(nusq,inhibits,inch).
local_semnet_1_GEN(nusq,inhibits,orch).
local_semnet_1_GEN(nusq,inhibits,lipd).
local_semnet_1_GEN(nusq,inhibits,eico).
local_semnet_1_GEN(nusq,inhibits,strd).
local_semnet_1_GEN(nusq,inhibits,elii).
local_semnet_1_GEN(nusq,inhibits,opco).
local_semnet_1_GEN(nusq,inhibits,nusq).
local_semnet_1_GEN(nusq,inhibits,carb).
local_semnet_1_GEN(carb,inhibits,gngm).
local_semnet_1_GEN(carb,inhibits,phsu).
local_semnet_1_GEN(carb,inhibits,antb).
local_semnet_1_GEN(carb,inhibits,bacs).
local_semnet_1_GEN(carb,inhibits,imft).
local_semnet_1_GEN(carb,inhibits,vita).
local_semnet_1_GEN(carb,inhibits,enzy).
local_semnet_1_GEN(carb,inhibits,horm).
local_semnet_1_GEN(carb,inhibits,rcpt).
local_semnet_1_GEN(carb,inhibits,nsba).
local_semnet_1_GEN(carb,inhibits,hops).
local_semnet_1_GEN(carb,inhibits,inch).
local_semnet_1_GEN(carb,inhibits,orch).
local_semnet_1_GEN(carb,inhibits,lipd).
local_semnet_1_GEN(carb,inhibits,eico).
local_semnet_1_GEN(carb,inhibits,strd).
local_semnet_1_GEN(carb,inhibits,elii).
local_semnet_1_GEN(carb,inhibits,opco).
local_semnet_1_GEN(carb,inhibits,nusq).
local_semnet_1_GEN(carb,inhibits,carb).
local_semnet_1_GEN(elii,inhibits,gngm).
local_semnet_1_GEN(elii,inhibits,phsu).
local_semnet_1_GEN(elii,inhibits,antb).
local_semnet_1_GEN(elii,inhibits,bacs).
local_semnet_1_GEN(elii,inhibits,imft).
local_semnet_1_GEN(elii,inhibits,vita).
local_semnet_1_GEN(elii,inhibits,enzy).
local_semnet_1_GEN(elii,inhibits,horm).
local_semnet_1_GEN(elii,inhibits,rcpt).
local_semnet_1_GEN(elii,inhibits,nsba).
local_semnet_1_GEN(elii,inhibits,hops).
local_semnet_1_GEN(elii,inhibits,inch).
local_semnet_1_GEN(elii,inhibits,orch).
local_semnet_1_GEN(elii,inhibits,lipd).
local_semnet_1_GEN(elii,inhibits,eico).
local_semnet_1_GEN(elii,inhibits,strd).
local_semnet_1_GEN(elii,inhibits,elii).
local_semnet_1_GEN(elii,inhibits,opco).
local_semnet_1_GEN(elii,inhibits,nusq).
local_semnet_1_GEN(elii,inhibits,carb).
% INTERACTS_WITH
local_semnet_1_GEN(gngm,interacts_with,gngm).
local_semnet_1_GEN(gngm,interacts_with,phsu).
local_semnet_1_GEN(gngm,interacts_with,antb).
local_semnet_1_GEN(gngm,interacts_with,bacs).
local_semnet_1_GEN(gngm,interacts_with,imft).
local_semnet_1_GEN(gngm,interacts_with,vita).
local_semnet_1_GEN(gngm,interacts_with,enzy).
local_semnet_1_GEN(gngm,interacts_with,horm).
local_semnet_1_GEN(gngm,interacts_with,rcpt).
local_semnet_1_GEN(gngm,interacts_with,nsba).
local_semnet_1_GEN(gngm,interacts_with,hops).
local_semnet_1_GEN(gngm,interacts_with,inch).
local_semnet_1_GEN(gngm,interacts_with,orch).
local_semnet_1_GEN(gngm,interacts_with,lipd).
local_semnet_1_GEN(gngm,interacts_with,eico).
local_semnet_1_GEN(gngm,interacts_with,strd).
local_semnet_1_GEN(gngm,interacts_with,elii).
local_semnet_1_GEN(gngm,interacts_with,opco).
local_semnet_1_GEN(gngm,interacts_with,nusq).
local_semnet_1_GEN(phsu,interacts_with,gngm).
local_semnet_1_GEN(antb,interacts_with,gngm).
local_semnet_1_GEN(bacs,interacts_with,gngm).
local_semnet_1_GEN(chvf,interacts_with,enzy). % GR 09/2015 to get inhibitor INTERACTS_WITH enzyme
local_semnet_1_GEN(enzy,interacts_with,chvf). % GR 09/2015 to get inhibitor INTERACTS_WITH enzyme
local_semnet_1_GEN(imft,interacts_with,gngm).
local_semnet_1_GEN(vita,interacts_with,gngm).
local_semnet_1_GEN(enzy,interacts_with,gngm).
local_semnet_1_GEN(horm,interacts_with,gngm).
local_semnet_1_GEN(rcpt,interacts_with,gngm).
local_semnet_1_GEN(nsba,interacts_with,gngm).
local_semnet_1_GEN(hops,interacts_with,gngm).
local_semnet_1_GEN(inch,interacts_with,gngm).
local_semnet_1_GEN(orch,interacts_with,gngm).
local_semnet_1_GEN(lipd,interacts_with,gngm).
local_semnet_1_GEN(eico,interacts_with,gngm).
local_semnet_1_GEN(strd,interacts_with,gngm).
local_semnet_1_GEN(elii,interacts_with,gngm).
local_semnet_1_GEN(opco,interacts_with,gngm).
local_semnet_1_GEN(nusq,interacts_with,gngm).
local_semnet_1_GEN(carb,interacts_with,gngm).
local_semnet_1_GEN(gngm,interacts_with,carb).
%MANIFESTATION_OF
%local_semnet_1_GEN(sosy,manifestation_of,dsyn). % 07/2015 tiple already in SN '06
% PREDISPOSES
%%% local_semnet_1_GEN(gngm,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(phsu,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(antb,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(bacs,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(imft,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(vita,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(enzy,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(horm,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(rcpt,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(nsba,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(hops,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(inch,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(orch,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(lipd,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(eico,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(strd,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(elii,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(opco,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(nusq,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(carb,predisposes,phsf).  % CA
%%% local_semnet_1_GEN(gngm,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(phsu,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(antb,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(bacs,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(imft,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(vita,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(enzy,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(horm,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(rcpt,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(nsba,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(hops,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(inch,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(orch,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(lipd,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(eico,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(strd,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(elii,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(opco,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(nusq,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(carb,predisposes,orgf).  % CA
%%% local_semnet_1_GEN(gngm,predisposes,celf).  % CA
%%% local_semnet_1_GEN(phsu,predisposes,celf).  % CA
%%% local_semnet_1_GEN(antb,predisposes,celf).  % CA
%%% local_semnet_1_GEN(bacs,predisposes,celf).  % CA
%%% local_semnet_1_GEN(imft,predisposes,celf).  % CA
%%% local_semnet_1_GEN(vita,predisposes,celf).  % CA
%%% local_semnet_1_GEN(enzy,predisposes,celf).  % CA
%%% local_semnet_1_GEN(horm,predisposes,celf).  % CA
%%% local_semnet_1_GEN(rcpt,predisposes,celf).  % CA
%%% local_semnet_1_GEN(nsba,predisposes,celf).  % CA
%%% local_semnet_1_GEN(hops,predisposes,celf).  % CA
%%% local_semnet_1_GEN(inch,predisposes,celf).  % CA
%%% local_semnet_1_GEN(orch,predisposes,celf).  % CA
%%% local_semnet_1_GEN(lipd,predisposes,celf).  % CA
%%% local_semnet_1_GEN(eico,predisposes,celf).  % CA
%%% local_semnet_1_GEN(strd,predisposes,celf).  % CA
%%% local_semnet_1_GEN(elii,predisposes,celf).  % CA
%%% local_semnet_1_GEN(opco,predisposes,celf).  % CA
%%% local_semnet_1_GEN(nusq,predisposes,celf).  % CA
%%% local_semnet_1_GEN(carb,predisposes,celf).  % CA
% local_semnet_1_GEN(gngm,predisposes,menp).
% local_semnet_1_GEN(phsu,predisposes,menp).
% local_semnet_1_GEN(antb,predisposes,menp).
% local_semnet_1_GEN(bacs,predisposes,menp).
% local_semnet_1_GEN(imft,predisposes,menp).
% local_semnet_1_GEN(vita,predisposes,menp).
% local_semnet_1_GEN(enzy,predisposes,menp).
% local_semnet_1_GEN(horm,predisposes,menp).
% local_semnet_1_GEN(rcpt,predisposes,menp).
% local_semnet_1_GEN(nsba,predisposes,menp).
% local_semnet_1_GEN(hops,predisposes,menp).
% local_semnet_1_GEN(inch,predisposes,menp).
% local_semnet_1_GEN(orch,predisposes,menp).
% local_semnet_1_GEN(lipd,predisposes,menp).
% local_semnet_1_GEN(eico,predisposes,menp).
% local_semnet_1_GEN(strd,predisposes,menp).
% local_semnet_1_GEN(elii,predisposes,menp).
% local_semnet_1_GEN(opco,predisposes,menp).
% local_semnet_1_GEN(nusq,predisposes,menp).
% local_semnet_1_GEN(carb,predisposes,menp).
% dsyn
local_semnet_1_GEN(aapp,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(bacs,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(fndg,predisposes,dsyn). %MF RF project ? This one is right, but may retrieve several wsa
local_semnet_1_GEN(dsyn,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(sosy,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(horm,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(strd,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(carb,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(imft,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(enzy,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(lipd,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(lbtr,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(patf,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(dora,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(food,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(inbe,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(clna,predisposes,dsyn). %MF RF project
local_semnet_1_GEN(eico,predisposes,dsyn).
local_semnet_1_GEN(rcpt,predisposes,dsyn).
local_semnet_1_GEN(nsba,predisposes,dsyn).
local_semnet_1_GEN(hops,predisposes,dsyn).
local_semnet_1_GEN(inch,predisposes,dsyn).
local_semnet_1_GEN(orch,predisposes,dsyn).
local_semnet_1_GEN(vita,predisposes,dsyn).
local_semnet_1_GEN(elii,predisposes,dsyn).
local_semnet_1_GEN(opco,predisposes,dsyn).
local_semnet_1_GEN(nusq,predisposes,dsyn).
local_semnet_1_GEN(phsu,predisposes,dsyn).
local_semnet_1_GEN(antb,predisposes,dsyn).
local_semnet_1_GEN(gngm,predisposes,dsyn).
local_semnet_1_GEN(orga,predisposes,dsyn).
local_semnet_1_GEN(comd,predisposes,patf). % Graciela request: 2/11/13.
local_semnet_1_GEN(fndg,predisposes,patf). % Graciela 10/20/ 2017.  stress was not predictive of poor glycemic control  - PMID 22676426, line 9
%mobd
local_semnet_1_GEN(aapp,predisposes,mobd). %MF RF project
local_semnet_1_GEN(bacs,predisposes,mobd). %MF RF project
local_semnet_1_GEN(fndg,predisposes,mobd). %MF RF project ? This one is right, but may retrieve several wsa
local_semnet_1_GEN(dsyn,predisposes,mobd). %MF RF project
local_semnet_1_GEN(sosy,predisposes,mobd). %MF RF project
local_semnet_1_GEN(horm,predisposes,mobd). %MF RF project
local_semnet_1_GEN(strd,predisposes,mobd). %MF RF project
local_semnet_1_GEN(carb,predisposes,mobd). %MF RF project
local_semnet_1_GEN(imft,predisposes,mobd). %MF RF project
local_semnet_1_GEN(enzy,predisposes,mobd). %MF RF project
local_semnet_1_GEN(lipd,predisposes,mobd). %MF RF project
local_semnet_1_GEN(lbtr,predisposes,mobd). %MF RF project
local_semnet_1_GEN(patf,predisposes,mobd). %MF RF project
local_semnet_1_GEN(dora,predisposes,mobd). %MF RF project
local_semnet_1_GEN(food,predisposes,mobd). %MF RF project
local_semnet_1_GEN(inbe,predisposes,mobd). %MF RF project
local_semnet_1_GEN(clna,predisposes,mobd). %MF RF project
local_semnet_1_GEN(eico,predisposes,mobd).
local_semnet_1_GEN(rcpt,predisposes,mobd).
local_semnet_1_GEN(nsba,predisposes,mobd).
local_semnet_1_GEN(hops,predisposes,mobd).
local_semnet_1_GEN(inch,predisposes,mobd).
local_semnet_1_GEN(orch,predisposes,mobd).
local_semnet_1_GEN(vita,predisposes,mobd).
local_semnet_1_GEN(elii,predisposes,mobd).
local_semnet_1_GEN(opco,predisposes,mobd).
local_semnet_1_GEN(nusq,predisposes,mobd).
local_semnet_1_GEN(phsu,predisposes,mobd).
local_semnet_1_GEN(antb,predisposes,mobd).
local_semnet_1_GEN(gngm,predisposes,mobd).
% neop
local_semnet_1_GEN(aapp,predisposes,neop). %MF RF project
local_semnet_1_GEN(bacs,predisposes,neop). %MF RF project
local_semnet_1_GEN(fndg,predisposes,neop). %MF RF project ? This one is right, but may retrieve several wsa
local_semnet_1_GEN(dsyn,predisposes,neop). %MF RF project
local_semnet_1_GEN(sosy,predisposes,neop). %MF RF project
local_semnet_1_GEN(horm,predisposes,neop). %MF RF project
local_semnet_1_GEN(strd,predisposes,neop). %MF RF project
local_semnet_1_GEN(carb,predisposes,neop). %MF RF project
local_semnet_1_GEN(imft,predisposes,neop). %MF RF project
local_semnet_1_GEN(enzy,predisposes,neop). %MF RF project
local_semnet_1_GEN(lipd,predisposes,neop). %MF RF project
local_semnet_1_GEN(lbtr,predisposes,neop). %MF RF project
local_semnet_1_GEN(patf,predisposes,neop). %MF RF project
local_semnet_1_GEN(dora,predisposes,neop). %MF RF project
local_semnet_1_GEN(food,predisposes,neop). %MF RF project
local_semnet_1_GEN(inbe,predisposes,neop). %MF RF project
local_semnet_1_GEN(clna,predisposes,neop). %MF RF project
local_semnet_1_GEN(eico,predisposes,neop).
local_semnet_1_GEN(rcpt,predisposes,neop).
local_semnet_1_GEN(nsba,predisposes,neop).
local_semnet_1_GEN(hops,predisposes,neop).
local_semnet_1_GEN(inch,predisposes,neop).
local_semnet_1_GEN(orch,predisposes,neop).
local_semnet_1_GEN(vita,predisposes,neop).
local_semnet_1_GEN(elii,predisposes,neop).
local_semnet_1_GEN(opco,predisposes,neop).
local_semnet_1_GEN(nusq,predisposes,neop).
local_semnet_1_GEN(phsu,predisposes,neop).
local_semnet_1_GEN(antb,predisposes,neop).
local_semnet_1_GEN(gngm,predisposes,neop).
% patf no change
local_semnet_1_GEN(gngm,predisposes,patf).
local_semnet_1_GEN(phsu,predisposes,patf).
local_semnet_1_GEN(antb,predisposes,patf).
local_semnet_1_GEN(bacs,predisposes,patf).
local_semnet_1_GEN(imft,predisposes,patf).
local_semnet_1_GEN(vita,predisposes,patf).
local_semnet_1_GEN(enzy,predisposes,patf).
local_semnet_1_GEN(horm,predisposes,patf).
local_semnet_1_GEN(rcpt,predisposes,patf).
local_semnet_1_GEN(nsba,predisposes,patf).
local_semnet_1_GEN(hops,predisposes,patf).
local_semnet_1_GEN(inch,predisposes,patf).
local_semnet_1_GEN(orch,predisposes,patf).
local_semnet_1_GEN(lipd,predisposes,patf).
local_semnet_1_GEN(eico,predisposes,patf).
local_semnet_1_GEN(strd,predisposes,patf).
local_semnet_1_GEN(elii,predisposes,patf).
local_semnet_1_GEN(opco,predisposes,patf).
local_semnet_1_GEN(nusq,predisposes,patf).
local_semnet_1_GEN(carb,predisposes,patf).
%comd no change
local_semnet_1_GEN(gngm,predisposes,comd).
local_semnet_1_GEN(phsu,predisposes,comd).
local_semnet_1_GEN(antb,predisposes,comd).
local_semnet_1_GEN(bacs,predisposes,comd).
local_semnet_1_GEN(imft,predisposes,comd).
local_semnet_1_GEN(vita,predisposes,comd).
local_semnet_1_GEN(enzy,predisposes,comd).
local_semnet_1_GEN(horm,predisposes,comd).
local_semnet_1_GEN(rcpt,predisposes,comd).
local_semnet_1_GEN(nsba,predisposes,comd).
local_semnet_1_GEN(hops,predisposes,comd).
local_semnet_1_GEN(inch,predisposes,comd).
local_semnet_1_GEN(orch,predisposes,comd).
local_semnet_1_GEN(lipd,predisposes,comd).
local_semnet_1_GEN(eico,predisposes,comd).
local_semnet_1_GEN(strd,predisposes,comd).
local_semnet_1_GEN(elii,predisposes,comd).
local_semnet_1_GEN(opco,predisposes,comd).
local_semnet_1_GEN(nusq,predisposes,comd).
local_semnet_1_GEN(carb,predisposes,comd).
%%% % local_semnet_1_GEN(gngm,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(phsu,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(antb,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(bacs,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(imft,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(vita,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(enzy,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(horm,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(rcpt,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(nsba,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(hops,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(inch,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(orch,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(lipd,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(eico,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(strd,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(elii,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(opco,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(nusq,predisposes,fndg).  % CA
%%% % local_semnet_1_GEN(carb,predisposes,fndg).  % CA
% sosy no change
local_semnet_1_GEN(gngm,predisposes,sosy).
local_semnet_1_GEN(phsu,predisposes,sosy).
local_semnet_1_GEN(antb,predisposes,sosy).
local_semnet_1_GEN(bacs,predisposes,sosy).
local_semnet_1_GEN(imft,predisposes,sosy).
local_semnet_1_GEN(vita,predisposes,sosy).
local_semnet_1_GEN(enzy,predisposes,sosy).
local_semnet_1_GEN(horm,predisposes,sosy).
local_semnet_1_GEN(rcpt,predisposes,sosy).
local_semnet_1_GEN(nsba,predisposes,sosy).
local_semnet_1_GEN(hops,predisposes,sosy).
local_semnet_1_GEN(inch,predisposes,sosy).
local_semnet_1_GEN(orch,predisposes,sosy).
local_semnet_1_GEN(lipd,predisposes,sosy).
local_semnet_1_GEN(eico,predisposes,sosy).
local_semnet_1_GEN(strd,predisposes,sosy).
local_semnet_1_GEN(elii,predisposes,sosy).
local_semnet_1_GEN(opco,predisposes,sosy).
local_semnet_1_GEN(nusq,predisposes,sosy).
local_semnet_1_GEN(carb,predisposes,sosy).
local_semnet_1_GEN(patf,predisposes,sosy). % GR 06/2015
%local_semnet_1_GEN(carb,predisposes,lbtr).
%local_semnet_1_GEN(gngm,predisposes,lbtr).
%local_semnet_1_GEN(phsu,predisposes,lbtr).
%local_semnet_1_GEN(antb,predisposes,lbtr).
%local_semnet_1_GEN(bacs,predisposes,lbtr).
%local_semnet_1_GEN(imft,predisposes,lbtr).
%local_semnet_1_GEN(vita,predisposes,lbtr).
%local_semnet_1_GEN(enzy,predisposes,lbtr).
%local_semnet_1_GEN(horm,predisposes,lbtr).
%local_semnet_1_GEN(rcpt,predisposes,lbtr).
%local_semnet_1_GEN(nsba,predisposes,lbtr).
%local_semnet_1_GEN(hops,predisposes,lbtr).
%local_semnet_1_GEN(inch,predisposes,lbtr).
%local_semnet_1_GEN(orch,predisposes,lbtr).
%local_semnet_1_GEN(lipd,predisposes,lbtr).
%local_semnet_1_GEN(eico,predisposes,lbtr).
%local_semnet_1_GEN(strd,predisposes,lbtr).
%local_semnet_1_GEN(elii,predisposes,lbtr).
%local_semnet_1_GEN(opco,predisposes,lbtr).
%local_semnet_1_GEN(nusq,predisposes,lbtr).
% no change
local_semnet_1_GEN(gngm,predisposes,inpo).
local_semnet_1_GEN(phsu,predisposes,inpo).
local_semnet_1_GEN(antb,predisposes,inpo).
local_semnet_1_GEN(bacs,predisposes,inpo).
local_semnet_1_GEN(imft,predisposes,inpo).
local_semnet_1_GEN(vita,predisposes,inpo).
local_semnet_1_GEN(enzy,predisposes,inpo).
local_semnet_1_GEN(horm,predisposes,inpo).
local_semnet_1_GEN(rcpt,predisposes,inpo).
local_semnet_1_GEN(nsba,predisposes,inpo).
local_semnet_1_GEN(hops,predisposes,inpo).
local_semnet_1_GEN(inch,predisposes,inpo).
local_semnet_1_GEN(orch,predisposes,inpo).
local_semnet_1_GEN(lipd,predisposes,inpo).
local_semnet_1_GEN(eico,predisposes,inpo).
local_semnet_1_GEN(strd,predisposes,inpo).
local_semnet_1_GEN(elii,predisposes,inpo).
local_semnet_1_GEN(opco,predisposes,inpo).
local_semnet_1_GEN(nusq,predisposes,inpo).
local_semnet_1_GEN(carb,predisposes,inpo).
% MISSING PREDISPOSE ?MF why not generalize. 
%%% local_semnet_1_GEN(aapp,predisposes,celf).  % CA
local_semnet_1_GEN(aapp,predisposes,comd).
%local_semnet_1_GEN(aapp,predisposes,fndg).
local_semnet_1_GEN(dsyn,predisposes,fndg). % GR 06/2015
local_semnet_1_GEN(aapp,predisposes,inpo).
%local_semnet_1_GEN(aapp,predisposes,lbtr).
%local_semnet_1_GEN(aapp,predisposes,menp).
%%% local_semnet_1_GEN(aapp,predisposes,orgf).  % CA
local_semnet_1_GEN(aapp,predisposes,patf).
%%% local_semnet_1_GEN(aapp,predisposes,phsf).  % CA
local_semnet_1_GEN(aapp,predisposes,sosy).
local_semnet_1_GEN(genf,predisposes,mobd). % GR 05/24/2017 source: GS PMID15526143
local_semnet_1_GEN(food,predisposes,fndg). % GR 10/26/2017 source: NEG PMID 17537359, line 3
% PROCESS_OF
%%% %%% local_semnet_1_GEN(fndg,process_of,humn).  % CA  % CA
local_semnet_1_GEN(sosy,process_of,humn).
%%% local_semnet_1_GEN(lbtr,process_of,humn).  % CA
% STIMULATES
local_semnet_1_GEN(gngm,stimulates,gngm).
local_semnet_1_GEN(gngm,stimulates,phsu).
local_semnet_1_GEN(gngm,stimulates,antb).
local_semnet_1_GEN(gngm,stimulates,bacs).
local_semnet_1_GEN(gngm,stimulates,imft).
local_semnet_1_GEN(gngm,stimulates,vita).
local_semnet_1_GEN(gngm,stimulates,enzy).
local_semnet_1_GEN(gngm,stimulates,horm).
local_semnet_1_GEN(gngm,stimulates,rcpt).
local_semnet_1_GEN(gngm,stimulates,nsba).
local_semnet_1_GEN(gngm,stimulates,hops).
local_semnet_1_GEN(gngm,stimulates,inch).
local_semnet_1_GEN(gngm,stimulates,orch).
local_semnet_1_GEN(gngm,stimulates,lipd).
local_semnet_1_GEN(gngm,stimulates,eico).
local_semnet_1_GEN(gngm,stimulates,strd).
local_semnet_1_GEN(gngm,stimulates,elii).
local_semnet_1_GEN(gngm,stimulates,opco).
local_semnet_1_GEN(gngm,stimulates,nusq).
local_semnet_1_GEN(gngm,stimulates,carb).
local_semnet_1_GEN(phsu,stimulates,gngm).
local_semnet_1_GEN(phsu,stimulates,phsu).
local_semnet_1_GEN(phsu,stimulates,antb).
local_semnet_1_GEN(phsu,stimulates,bacs).
local_semnet_1_GEN(phsu,stimulates,imft).
local_semnet_1_GEN(phsu,stimulates,vita).
local_semnet_1_GEN(phsu,stimulates,enzy).
local_semnet_1_GEN(phsu,stimulates,horm).
local_semnet_1_GEN(phsu,stimulates,rcpt).
local_semnet_1_GEN(phsu,stimulates,nsba).
local_semnet_1_GEN(phsu,stimulates,hops).
local_semnet_1_GEN(phsu,stimulates,inch).
local_semnet_1_GEN(phsu,stimulates,orch).
local_semnet_1_GEN(phsu,stimulates,lipd).
local_semnet_1_GEN(phsu,stimulates,eico).
local_semnet_1_GEN(phsu,stimulates,strd).
local_semnet_1_GEN(phsu,stimulates,elii).
local_semnet_1_GEN(phsu,stimulates,opco).
local_semnet_1_GEN(phsu,stimulates,nusq).
local_semnet_1_GEN(phsu,stimulates,carb).
local_semnet_1_GEN(antb,stimulates,gngm).
local_semnet_1_GEN(antb,stimulates,phsu).
local_semnet_1_GEN(antb,stimulates,antb).
local_semnet_1_GEN(antb,stimulates,bacs).
local_semnet_1_GEN(antb,stimulates,imft).
local_semnet_1_GEN(antb,stimulates,vita).
local_semnet_1_GEN(antb,stimulates,enzy).
local_semnet_1_GEN(antb,stimulates,horm).
local_semnet_1_GEN(antb,stimulates,rcpt).
local_semnet_1_GEN(antb,stimulates,nsba).
local_semnet_1_GEN(antb,stimulates,hops).
local_semnet_1_GEN(antb,stimulates,inch).
local_semnet_1_GEN(antb,stimulates,orch).
local_semnet_1_GEN(antb,stimulates,lipd).
local_semnet_1_GEN(antb,stimulates,eico).
local_semnet_1_GEN(antb,stimulates,strd).
local_semnet_1_GEN(antb,stimulates,elii).
local_semnet_1_GEN(antb,stimulates,opco).
local_semnet_1_GEN(antb,stimulates,nusq).
local_semnet_1_GEN(antb,stimulates,carb).
local_semnet_1_GEN(bacs,stimulates,gngm).
local_semnet_1_GEN(bacs,stimulates,phsu).
local_semnet_1_GEN(bacs,stimulates,antb).
local_semnet_1_GEN(bacs,stimulates,bacs).
local_semnet_1_GEN(bacs,stimulates,imft).
local_semnet_1_GEN(bacs,stimulates,vita).
local_semnet_1_GEN(bacs,stimulates,enzy).
local_semnet_1_GEN(bacs,stimulates,horm).
local_semnet_1_GEN(bacs,stimulates,rcpt).
local_semnet_1_GEN(bacs,stimulates,nsba).
local_semnet_1_GEN(bacs,stimulates,hops).
local_semnet_1_GEN(bacs,stimulates,inch).
local_semnet_1_GEN(bacs,stimulates,orch).
local_semnet_1_GEN(bacs,stimulates,lipd).
local_semnet_1_GEN(bacs,stimulates,eico).
local_semnet_1_GEN(bacs,stimulates,strd).
local_semnet_1_GEN(bacs,stimulates,elii).
local_semnet_1_GEN(bacs,stimulates,opco).
local_semnet_1_GEN(bacs,stimulates,nusq).
local_semnet_1_GEN(bacs,stimulates,carb).
local_semnet_1_GEN(imft,stimulates,gngm).
local_semnet_1_GEN(imft,stimulates,phsu).
local_semnet_1_GEN(imft,stimulates,antb).
local_semnet_1_GEN(imft,stimulates,bacs).
local_semnet_1_GEN(imft,stimulates,imft).
local_semnet_1_GEN(imft,stimulates,vita).
local_semnet_1_GEN(imft,stimulates,enzy).
local_semnet_1_GEN(imft,stimulates,horm).
local_semnet_1_GEN(imft,stimulates,rcpt).
local_semnet_1_GEN(imft,stimulates,nsba).
local_semnet_1_GEN(imft,stimulates,hops).
local_semnet_1_GEN(imft,stimulates,inch).
local_semnet_1_GEN(imft,stimulates,orch).
local_semnet_1_GEN(imft,stimulates,lipd).
local_semnet_1_GEN(imft,stimulates,eico).
local_semnet_1_GEN(imft,stimulates,strd).
local_semnet_1_GEN(imft,stimulates,elii).
local_semnet_1_GEN(imft,stimulates,opco).
local_semnet_1_GEN(imft,stimulates,nusq).
local_semnet_1_GEN(imft,stimulates,carb).
local_semnet_1_GEN(vita,stimulates,gngm).
local_semnet_1_GEN(vita,stimulates,phsu).
local_semnet_1_GEN(vita,stimulates,antb).
local_semnet_1_GEN(vita,stimulates,bacs).
local_semnet_1_GEN(vita,stimulates,imft).
local_semnet_1_GEN(vita,stimulates,vita).
local_semnet_1_GEN(vita,stimulates,enzy).
local_semnet_1_GEN(vita,stimulates,horm).
local_semnet_1_GEN(vita,stimulates,rcpt).
local_semnet_1_GEN(vita,stimulates,nsba).
local_semnet_1_GEN(vita,stimulates,hops).
local_semnet_1_GEN(vita,stimulates,inch).
local_semnet_1_GEN(vita,stimulates,orch).
local_semnet_1_GEN(vita,stimulates,lipd).
local_semnet_1_GEN(vita,stimulates,eico).
local_semnet_1_GEN(vita,stimulates,strd).
local_semnet_1_GEN(vita,stimulates,elii).
local_semnet_1_GEN(vita,stimulates,opco).
local_semnet_1_GEN(vita,stimulates,nusq).
local_semnet_1_GEN(vita,stimulates,carb).
local_semnet_1_GEN(enzy,stimulates,gngm).
local_semnet_1_GEN(enzy,stimulates,phsu).
local_semnet_1_GEN(enzy,stimulates,antb).
local_semnet_1_GEN(enzy,stimulates,bacs).
local_semnet_1_GEN(enzy,stimulates,imft).
local_semnet_1_GEN(enzy,stimulates,vita).
local_semnet_1_GEN(enzy,stimulates,enzy).
local_semnet_1_GEN(enzy,stimulates,horm).
local_semnet_1_GEN(enzy,stimulates,rcpt).
local_semnet_1_GEN(enzy,stimulates,nsba).
local_semnet_1_GEN(enzy,stimulates,hops).
local_semnet_1_GEN(enzy,stimulates,inch).
local_semnet_1_GEN(enzy,stimulates,orch).
local_semnet_1_GEN(enzy,stimulates,lipd).
local_semnet_1_GEN(enzy,stimulates,eico).
local_semnet_1_GEN(enzy,stimulates,strd).
local_semnet_1_GEN(enzy,stimulates,elii).
local_semnet_1_GEN(enzy,stimulates,opco).
local_semnet_1_GEN(enzy,stimulates,nusq).
local_semnet_1_GEN(enzy,stimulates,carb).
local_semnet_1_GEN(horm,stimulates,gngm).
local_semnet_1_GEN(horm,stimulates,phsu).
local_semnet_1_GEN(horm,stimulates,antb).
local_semnet_1_GEN(horm,stimulates,bacs).
local_semnet_1_GEN(horm,stimulates,imft).
local_semnet_1_GEN(horm,stimulates,vita).
local_semnet_1_GEN(horm,stimulates,enzy).
local_semnet_1_GEN(horm,stimulates,horm).
local_semnet_1_GEN(horm,stimulates,rcpt).
local_semnet_1_GEN(horm,stimulates,nsba).
local_semnet_1_GEN(horm,stimulates,hops).
local_semnet_1_GEN(horm,stimulates,inch).
local_semnet_1_GEN(horm,stimulates,orch).
local_semnet_1_GEN(horm,stimulates,lipd).
local_semnet_1_GEN(horm,stimulates,eico).
local_semnet_1_GEN(horm,stimulates,strd).
local_semnet_1_GEN(horm,stimulates,elii).
local_semnet_1_GEN(horm,stimulates,opco).
local_semnet_1_GEN(horm,stimulates,nusq).
local_semnet_1_GEN(horm,stimulates,carb).
local_semnet_1_GEN(rcpt,stimulates,gngm).
local_semnet_1_GEN(rcpt,stimulates,phsu).
local_semnet_1_GEN(rcpt,stimulates,antb).
local_semnet_1_GEN(rcpt,stimulates,bacs).
local_semnet_1_GEN(rcpt,stimulates,imft).
local_semnet_1_GEN(rcpt,stimulates,vita).
local_semnet_1_GEN(rcpt,stimulates,enzy).
local_semnet_1_GEN(rcpt,stimulates,horm).
local_semnet_1_GEN(rcpt,stimulates,rcpt).
local_semnet_1_GEN(rcpt,stimulates,nsba).
local_semnet_1_GEN(rcpt,stimulates,hops).
local_semnet_1_GEN(rcpt,stimulates,inch).
local_semnet_1_GEN(rcpt,stimulates,orch).
local_semnet_1_GEN(rcpt,stimulates,lipd).
local_semnet_1_GEN(rcpt,stimulates,eico).
local_semnet_1_GEN(rcpt,stimulates,strd).
local_semnet_1_GEN(rcpt,stimulates,elii).
local_semnet_1_GEN(rcpt,stimulates,opco).
local_semnet_1_GEN(rcpt,stimulates,nusq).
local_semnet_1_GEN(rcpt,stimulates,carb).
local_semnet_1_GEN(nsba,stimulates,gngm).
local_semnet_1_GEN(nsba,stimulates,phsu).
local_semnet_1_GEN(nsba,stimulates,antb).
local_semnet_1_GEN(nsba,stimulates,bacs).
local_semnet_1_GEN(nsba,stimulates,imft).
local_semnet_1_GEN(nsba,stimulates,vita).
local_semnet_1_GEN(nsba,stimulates,enzy).
local_semnet_1_GEN(nsba,stimulates,horm).
local_semnet_1_GEN(nsba,stimulates,rcpt).
local_semnet_1_GEN(nsba,stimulates,nsba).
local_semnet_1_GEN(nsba,stimulates,hops).
local_semnet_1_GEN(nsba,stimulates,inch).
local_semnet_1_GEN(nsba,stimulates,orch).
local_semnet_1_GEN(nsba,stimulates,lipd).
local_semnet_1_GEN(nsba,stimulates,eico).
local_semnet_1_GEN(nsba,stimulates,strd).
local_semnet_1_GEN(nsba,stimulates,elii).
local_semnet_1_GEN(nsba,stimulates,opco).
local_semnet_1_GEN(nsba,stimulates,nusq).
local_semnet_1_GEN(nsba,stimulates,carb).
local_semnet_1_GEN(hops,stimulates,gngm).
local_semnet_1_GEN(hops,stimulates,phsu).
local_semnet_1_GEN(hops,stimulates,antb).
local_semnet_1_GEN(hops,stimulates,bacs).
local_semnet_1_GEN(hops,stimulates,imft).
local_semnet_1_GEN(hops,stimulates,vita).
local_semnet_1_GEN(hops,stimulates,enzy).
local_semnet_1_GEN(hops,stimulates,horm).
local_semnet_1_GEN(hops,stimulates,rcpt).
local_semnet_1_GEN(hops,stimulates,nsba).
local_semnet_1_GEN(hops,stimulates,hops).
local_semnet_1_GEN(hops,stimulates,inch).
local_semnet_1_GEN(hops,stimulates,orch).
local_semnet_1_GEN(hops,stimulates,lipd).
local_semnet_1_GEN(hops,stimulates,eico).
local_semnet_1_GEN(hops,stimulates,strd).
local_semnet_1_GEN(hops,stimulates,elii).
local_semnet_1_GEN(hops,stimulates,opco).
local_semnet_1_GEN(hops,stimulates,nusq).
local_semnet_1_GEN(hops,stimulates,carb).
local_semnet_1_GEN(inch,stimulates,gngm).
local_semnet_1_GEN(inch,stimulates,phsu).
local_semnet_1_GEN(inch,stimulates,antb).
local_semnet_1_GEN(inch,stimulates,bacs).
local_semnet_1_GEN(inch,stimulates,imft).
local_semnet_1_GEN(inch,stimulates,vita).
local_semnet_1_GEN(inch,stimulates,enzy).
local_semnet_1_GEN(inch,stimulates,horm).
local_semnet_1_GEN(inch,stimulates,rcpt).
local_semnet_1_GEN(inch,stimulates,nsba).
local_semnet_1_GEN(inch,stimulates,hops).
local_semnet_1_GEN(inch,stimulates,inch).
local_semnet_1_GEN(inch,stimulates,orch).
local_semnet_1_GEN(inch,stimulates,lipd).
local_semnet_1_GEN(inch,stimulates,eico).
local_semnet_1_GEN(inch,stimulates,strd).
local_semnet_1_GEN(inch,stimulates,elii).
local_semnet_1_GEN(inch,stimulates,opco).
local_semnet_1_GEN(inch,stimulates,nusq).
local_semnet_1_GEN(inch,stimulates,carb).
local_semnet_1_GEN(orch,stimulates,gngm).
local_semnet_1_GEN(orch,stimulates,phsu).
local_semnet_1_GEN(orch,stimulates,antb).
local_semnet_1_GEN(orch,stimulates,bacs).
local_semnet_1_GEN(orch,stimulates,imft).
local_semnet_1_GEN(orch,stimulates,vita).
local_semnet_1_GEN(orch,stimulates,enzy).
local_semnet_1_GEN(orch,stimulates,horm).
local_semnet_1_GEN(orch,stimulates,rcpt).
local_semnet_1_GEN(orch,stimulates,nsba).
local_semnet_1_GEN(orch,stimulates,hops).
local_semnet_1_GEN(orch,stimulates,inch).
local_semnet_1_GEN(orch,stimulates,orch).
local_semnet_1_GEN(orch,stimulates,lipd).
local_semnet_1_GEN(orch,stimulates,eico).
local_semnet_1_GEN(orch,stimulates,strd).
local_semnet_1_GEN(orch,stimulates,elii).
local_semnet_1_GEN(orch,stimulates,opco).
local_semnet_1_GEN(orch,stimulates,nusq).
local_semnet_1_GEN(orch,stimulates,carb).
local_semnet_1_GEN(lipd,stimulates,gngm).
local_semnet_1_GEN(lipd,stimulates,phsu).
local_semnet_1_GEN(lipd,stimulates,antb).
local_semnet_1_GEN(lipd,stimulates,bacs).
local_semnet_1_GEN(lipd,stimulates,imft).
local_semnet_1_GEN(lipd,stimulates,vita).
local_semnet_1_GEN(lipd,stimulates,enzy).
local_semnet_1_GEN(lipd,stimulates,horm).
local_semnet_1_GEN(lipd,stimulates,rcpt).
local_semnet_1_GEN(lipd,stimulates,nsba).
local_semnet_1_GEN(lipd,stimulates,hops).
local_semnet_1_GEN(lipd,stimulates,inch).
local_semnet_1_GEN(lipd,stimulates,orch).
local_semnet_1_GEN(lipd,stimulates,lipd).
local_semnet_1_GEN(lipd,stimulates,eico).
local_semnet_1_GEN(lipd,stimulates,strd).
local_semnet_1_GEN(lipd,stimulates,elii).
local_semnet_1_GEN(lipd,stimulates,opco).
local_semnet_1_GEN(lipd,stimulates,nusq).
local_semnet_1_GEN(lipd,stimulates,carb).
local_semnet_1_GEN(eico,stimulates,gngm).
local_semnet_1_GEN(eico,stimulates,phsu).
local_semnet_1_GEN(eico,stimulates,antb).
local_semnet_1_GEN(eico,stimulates,bacs).
local_semnet_1_GEN(eico,stimulates,imft).
local_semnet_1_GEN(eico,stimulates,vita).
local_semnet_1_GEN(eico,stimulates,enzy).
local_semnet_1_GEN(eico,stimulates,horm).
local_semnet_1_GEN(eico,stimulates,rcpt).
local_semnet_1_GEN(eico,stimulates,nsba).
local_semnet_1_GEN(eico,stimulates,hops).
local_semnet_1_GEN(eico,stimulates,inch).
local_semnet_1_GEN(eico,stimulates,orch).
local_semnet_1_GEN(eico,stimulates,lipd).
local_semnet_1_GEN(eico,stimulates,eico).
local_semnet_1_GEN(eico,stimulates,strd).
local_semnet_1_GEN(eico,stimulates,elii).
local_semnet_1_GEN(eico,stimulates,opco).
local_semnet_1_GEN(eico,stimulates,nusq).
local_semnet_1_GEN(eico,stimulates,carb).
local_semnet_1_GEN(eico,stimulates,moft). %10/26/2017 PMID 17150260, line 6): In addition, EETs and DHETs might stimulate lipid metabolism 
local_semnet_1_GEN(strd,stimulates,gngm).
local_semnet_1_GEN(strd,stimulates,phsu).
local_semnet_1_GEN(strd,stimulates,antb).
local_semnet_1_GEN(strd,stimulates,bacs).
local_semnet_1_GEN(strd,stimulates,imft).
local_semnet_1_GEN(strd,stimulates,vita).
local_semnet_1_GEN(strd,stimulates,enzy).
local_semnet_1_GEN(strd,stimulates,horm).
local_semnet_1_GEN(strd,stimulates,rcpt).
local_semnet_1_GEN(strd,stimulates,nsba).
local_semnet_1_GEN(strd,stimulates,hops).
local_semnet_1_GEN(strd,stimulates,inch).
local_semnet_1_GEN(strd,stimulates,orch).
local_semnet_1_GEN(strd,stimulates,lipd).
local_semnet_1_GEN(strd,stimulates,eico).
local_semnet_1_GEN(strd,stimulates,strd).
local_semnet_1_GEN(strd,stimulates,elii).
local_semnet_1_GEN(strd,stimulates,opco).
local_semnet_1_GEN(strd,stimulates,nusq).
local_semnet_1_GEN(strd,stimulates,carb).
local_semnet_1_GEN(aapp,stimulates,gngm).
local_semnet_1_GEN(aapp,stimulates,phsu).
local_semnet_1_GEN(aapp,stimulates,antb).
local_semnet_1_GEN(aapp,stimulates,bacs).
local_semnet_1_GEN(aapp,stimulates,imft).
local_semnet_1_GEN(aapp,stimulates,vita).
local_semnet_1_GEN(aapp,stimulates,enzy).
local_semnet_1_GEN(aapp,stimulates,horm).
local_semnet_1_GEN(aapp,stimulates,rcpt).
local_semnet_1_GEN(aapp,stimulates,nsba).
local_semnet_1_GEN(aapp,stimulates,hops).
local_semnet_1_GEN(aapp,stimulates,inch).
local_semnet_1_GEN(aapp,stimulates,orch).
local_semnet_1_GEN(aapp,stimulates,lipd).
local_semnet_1_GEN(aapp,stimulates,eico).
local_semnet_1_GEN(aapp,stimulates,strd).
local_semnet_1_GEN(aapp,stimulates,elii).
local_semnet_1_GEN(aapp,stimulates,opco).
local_semnet_1_GEN(aapp,stimulates,nusq).
local_semnet_1_GEN(aapp,stimulates,carb).
local_semnet_1_GEN(opco,stimulates,gngm).
local_semnet_1_GEN(opco,stimulates,phsu).
local_semnet_1_GEN(opco,stimulates,antb).
local_semnet_1_GEN(opco,stimulates,bacs).
local_semnet_1_GEN(opco,stimulates,imft).
local_semnet_1_GEN(opco,stimulates,vita).
local_semnet_1_GEN(opco,stimulates,enzy).
local_semnet_1_GEN(opco,stimulates,horm).
local_semnet_1_GEN(opco,stimulates,rcpt).
local_semnet_1_GEN(opco,stimulates,nsba).
local_semnet_1_GEN(opco,stimulates,hops).
local_semnet_1_GEN(opco,stimulates,inch).
local_semnet_1_GEN(opco,stimulates,orch).
local_semnet_1_GEN(opco,stimulates,lipd).
local_semnet_1_GEN(opco,stimulates,eico).
local_semnet_1_GEN(opco,stimulates,strd).
local_semnet_1_GEN(opco,stimulates,elii).
local_semnet_1_GEN(opco,stimulates,opco).
local_semnet_1_GEN(opco,stimulates,nusq).
local_semnet_1_GEN(opco,stimulates,carb).
local_semnet_1_GEN(nusq,stimulates,gngm).
local_semnet_1_GEN(nusq,stimulates,phsu).
local_semnet_1_GEN(nusq,stimulates,antb).
local_semnet_1_GEN(nusq,stimulates,bacs).
local_semnet_1_GEN(nusq,stimulates,imft).
local_semnet_1_GEN(nusq,stimulates,vita).
local_semnet_1_GEN(nusq,stimulates,enzy).
local_semnet_1_GEN(nusq,stimulates,horm).
local_semnet_1_GEN(nusq,stimulates,rcpt).
local_semnet_1_GEN(nusq,stimulates,nsba).
local_semnet_1_GEN(nusq,stimulates,hops).
local_semnet_1_GEN(nusq,stimulates,inch).
local_semnet_1_GEN(nusq,stimulates,orch).
local_semnet_1_GEN(nusq,stimulates,lipd).
local_semnet_1_GEN(nusq,stimulates,eico).
local_semnet_1_GEN(nusq,stimulates,strd).
local_semnet_1_GEN(nusq,stimulates,elii).
local_semnet_1_GEN(nusq,stimulates,opco).
local_semnet_1_GEN(nusq,stimulates,nusq).
local_semnet_1_GEN(nusq,stimulates,carb).
local_semnet_1_GEN(carb,stimulates,gngm).
local_semnet_1_GEN(carb,stimulates,phsu).
local_semnet_1_GEN(carb,stimulates,antb).
local_semnet_1_GEN(carb,stimulates,bacs).
local_semnet_1_GEN(carb,stimulates,imft).
local_semnet_1_GEN(carb,stimulates,vita).
local_semnet_1_GEN(carb,stimulates,enzy).
local_semnet_1_GEN(carb,stimulates,horm).
local_semnet_1_GEN(carb,stimulates,rcpt).
local_semnet_1_GEN(carb,stimulates,nsba).
local_semnet_1_GEN(carb,stimulates,hops).
local_semnet_1_GEN(carb,stimulates,inch).
local_semnet_1_GEN(carb,stimulates,orch).
local_semnet_1_GEN(carb,stimulates,lipd).
local_semnet_1_GEN(carb,stimulates,eico).
local_semnet_1_GEN(carb,stimulates,strd).
local_semnet_1_GEN(carb,stimulates,elii).
local_semnet_1_GEN(carb,stimulates,opco).
local_semnet_1_GEN(carb,stimulates,nusq).
local_semnet_1_GEN(carb,stimulates,carb).
local_semnet_1_GEN(elii,stimulates,gngm).
local_semnet_1_GEN(elii,stimulates,phsu).
local_semnet_1_GEN(elii,stimulates,antb).
local_semnet_1_GEN(elii,stimulates,bacs).
local_semnet_1_GEN(elii,stimulates,imft).
local_semnet_1_GEN(elii,stimulates,vita).
local_semnet_1_GEN(elii,stimulates,enzy).
local_semnet_1_GEN(elii,stimulates,horm).
local_semnet_1_GEN(elii,stimulates,rcpt).
local_semnet_1_GEN(elii,stimulates,nsba).
local_semnet_1_GEN(elii,stimulates,hops).
local_semnet_1_GEN(elii,stimulates,inch).
local_semnet_1_GEN(elii,stimulates,orch).
local_semnet_1_GEN(elii,stimulates,lipd).
local_semnet_1_GEN(elii,stimulates,eico).
local_semnet_1_GEN(elii,stimulates,strd).
local_semnet_1_GEN(elii,stimulates,elii).
local_semnet_1_GEN(elii,stimulates,opco).
local_semnet_1_GEN(elii,stimulates,nusq).
local_semnet_1_GEN(elii,stimulates,carb).
% PART_OF
local_semnet_1_GEN(bacs,part_of,anim).
local_semnet_1_GEN(bacs,part_of,arch).
local_semnet_1_GEN(bacs,part_of,bact).
local_semnet_1_GEN(bacs,part_of,fngs).
local_semnet_1_GEN(bacs,part_of,humn).
%%% local_semnet_1_GEN(bacs,part_of,invt).  % CA
local_semnet_1_GEN(bacs,part_of,mamm).
local_semnet_1_GEN(bacs,part_of,orgm).
local_semnet_1_GEN(bacs,part_of,virs).
local_semnet_1_GEN(bacs,part_of,vtbt).
local_semnet_1_GEN(bacs,part_of,anst).
local_semnet_1_GEN(bacs,part_of,emst).
local_semnet_1_GEN(bacs,part_of,ffas).
local_semnet_1_GEN(bacs,part_of,bpoc).
local_semnet_1_GEN(bacs,part_of,tisu).
local_semnet_1_GEN(bacs,part_of,cell).
local_semnet_1_GEN(bacs,part_of,celc).
local_semnet_1_GEN(bacs,part_of,anab).
local_semnet_1_GEN(aapp,part_of,anim).
local_semnet_1_GEN(aapp,part_of,arch).
local_semnet_1_GEN(aapp,part_of,bact).
local_semnet_1_GEN(aapp,part_of,fngs).
local_semnet_1_GEN(aapp,part_of,humn).
%%% local_semnet_1_GEN(aapp,part_of,invt).  % CA
local_semnet_1_GEN(aapp,part_of,mamm).
local_semnet_1_GEN(aapp,part_of,orgm).
local_semnet_1_GEN(aapp,part_of,virs).
local_semnet_1_GEN(aapp,part_of,vtbt).
local_semnet_1_GEN(aapp,part_of,anst).
local_semnet_1_GEN(aapp,part_of,emst).
local_semnet_1_GEN(aapp,part_of,ffas).
local_semnet_1_GEN(aapp,part_of,bpoc).
local_semnet_1_GEN(aapp,part_of,tisu).
local_semnet_1_GEN(aapp,part_of,cell).
local_semnet_1_GEN(aapp,part_of,celc).
local_semnet_1_GEN(aapp,part_of,anab).
local_semnet_1_GEN(bdsu,part_of,cell). % GR 06/2015
local_semnet_1_GEN(bdsu,part_of,humn). % GR 06/2015
local_semnet_1_GEN(bacs,part_of,aapp). % GR 06/2015
local_semnet_1_GEN(imft,part_of,anim).
local_semnet_1_GEN(imft,part_of,arch).
local_semnet_1_GEN(imft,part_of,bact).
local_semnet_1_GEN(imft,part_of,fngs).
local_semnet_1_GEN(imft,part_of,humn).
%%% local_semnet_1_GEN(imft,part_of,invt).  % CA
local_semnet_1_GEN(imft,part_of,mamm).
local_semnet_1_GEN(imft,part_of,orgm).
local_semnet_1_GEN(imft,part_of,virs).
local_semnet_1_GEN(imft,part_of,vtbt).
local_semnet_1_GEN(imft,part_of,anst).
local_semnet_1_GEN(imft,part_of,emst).
local_semnet_1_GEN(imft,part_of,ffas).
local_semnet_1_GEN(imft,part_of,bpoc).
local_semnet_1_GEN(imft,part_of,tisu).
local_semnet_1_GEN(imft,part_of,cell).
local_semnet_1_GEN(imft,part_of,celc).
local_semnet_1_GEN(imft,part_of,anab).
local_semnet_1_GEN(enzy,part_of,anim).
local_semnet_1_GEN(enzy,part_of,arch).
local_semnet_1_GEN(enzy,part_of,bact).
local_semnet_1_GEN(enzy,part_of,fngs).
local_semnet_1_GEN(enzy,part_of,humn).
%%% local_semnet_1_GEN(enzy,part_of,invt).  % CA
local_semnet_1_GEN(enzy,part_of,mamm).
local_semnet_1_GEN(enzy,part_of,orgm).
local_semnet_1_GEN(enzy,part_of,virs).
local_semnet_1_GEN(enzy,part_of,vtbt).
local_semnet_1_GEN(enzy,part_of,anst).
local_semnet_1_GEN(enzy,part_of,emst).
local_semnet_1_GEN(enzy,part_of,ffas).
local_semnet_1_GEN(enzy,part_of,bpoc).
local_semnet_1_GEN(enzy,part_of,tisu).
local_semnet_1_GEN(enzy,part_of,cell).
local_semnet_1_GEN(enzy,part_of,celc).
local_semnet_1_GEN(enzy,part_of,anab).
local_semnet_1_GEN(horm,part_of,anim).
local_semnet_1_GEN(horm,part_of,arch).
local_semnet_1_GEN(horm,part_of,bact).
local_semnet_1_GEN(horm,part_of,fngs).
local_semnet_1_GEN(horm,part_of,humn).
%%% local_semnet_1_GEN(horm,part_of,invt).  % CA
local_semnet_1_GEN(horm,part_of,mamm).
local_semnet_1_GEN(horm,part_of,orgm).
local_semnet_1_GEN(horm,part_of,virs).
local_semnet_1_GEN(horm,part_of,vtbt).
local_semnet_1_GEN(horm,part_of,anst).
local_semnet_1_GEN(horm,part_of,emst).
local_semnet_1_GEN(horm,part_of,ffas).
local_semnet_1_GEN(horm,part_of,bpoc).
local_semnet_1_GEN(horm,part_of,tisu).
local_semnet_1_GEN(horm,part_of,cell).
local_semnet_1_GEN(horm,part_of,celc).
local_semnet_1_GEN(horm,part_of,anab).
local_semnet_1_GEN(rcpt,part_of,anim).
local_semnet_1_GEN(rcpt,part_of,arch).
local_semnet_1_GEN(rcpt,part_of,bact).
local_semnet_1_GEN(rcpt,part_of,fngs).
local_semnet_1_GEN(rcpt,part_of,humn).
%%% local_semnet_1_GEN(rcpt,part_of,invt).  % CA
local_semnet_1_GEN(rcpt,part_of,mamm).
local_semnet_1_GEN(rcpt,part_of,orgm).
local_semnet_1_GEN(rcpt,part_of,virs).
local_semnet_1_GEN(rcpt,part_of,vtbt).
local_semnet_1_GEN(rcpt,part_of,anst).
local_semnet_1_GEN(rcpt,part_of,emst).
local_semnet_1_GEN(rcpt,part_of,ffas).
local_semnet_1_GEN(rcpt,part_of,bpoc).
local_semnet_1_GEN(rcpt,part_of,tisu).
local_semnet_1_GEN(rcpt,part_of,cell).
local_semnet_1_GEN(rcpt,part_of,celc).
local_semnet_1_GEN(rcpt,part_of,anab).
local_semnet_1_GEN(nsba,part_of,anim).
local_semnet_1_GEN(nsba,part_of,arch).
local_semnet_1_GEN(nsba,part_of,bact).
local_semnet_1_GEN(nsba,part_of,fngs).
local_semnet_1_GEN(nsba,part_of,humn).
%%% local_semnet_1_GEN(nsba,part_of,invt).  % CA
local_semnet_1_GEN(nsba,part_of,mamm).
local_semnet_1_GEN(nsba,part_of,orgm).
local_semnet_1_GEN(nsba,part_of,virs).
local_semnet_1_GEN(nsba,part_of,vtbt).
local_semnet_1_GEN(nsba,part_of,anst).
local_semnet_1_GEN(nsba,part_of,emst).
local_semnet_1_GEN(nsba,part_of,ffas).
local_semnet_1_GEN(nsba,part_of,bpoc).
local_semnet_1_GEN(nsba,part_of,tisu).
local_semnet_1_GEN(nsba,part_of,cell).
local_semnet_1_GEN(nsba,part_of,celc).
local_semnet_1_GEN(nsba,part_of,anab).
local_semnet_1_GEN(lipd,part_of,anim).
local_semnet_1_GEN(lipd,part_of,arch).
local_semnet_1_GEN(lipd,part_of,bact).
local_semnet_1_GEN(lipd,part_of,fngs).
local_semnet_1_GEN(lipd,part_of,humn).
%%% local_semnet_1_GEN(lipd,part_of,invt).  % CA
local_semnet_1_GEN(lipd,part_of,mamm).
local_semnet_1_GEN(lipd,part_of,orgm).
local_semnet_1_GEN(lipd,part_of,virs).
local_semnet_1_GEN(lipd,part_of,vtbt).
local_semnet_1_GEN(lipd,part_of,anst).
local_semnet_1_GEN(lipd,part_of,emst).
local_semnet_1_GEN(lipd,part_of,ffas).
local_semnet_1_GEN(lipd,part_of,bpoc).
local_semnet_1_GEN(lipd,part_of,tisu).
local_semnet_1_GEN(lipd,part_of,cell).
local_semnet_1_GEN(lipd,part_of,celc).
local_semnet_1_GEN(lipd,part_of,anab).
local_semnet_1_GEN(eico,part_of,anim).
local_semnet_1_GEN(eico,part_of,arch).
local_semnet_1_GEN(eico,part_of,bact).
local_semnet_1_GEN(eico,part_of,fngs).
local_semnet_1_GEN(eico,part_of,humn).
%%% local_semnet_1_GEN(eico,part_of,invt).  % CA
local_semnet_1_GEN(eico,part_of,mamm).
local_semnet_1_GEN(eico,part_of,orgm).
local_semnet_1_GEN(eico,part_of,virs).
local_semnet_1_GEN(eico,part_of,vtbt).
local_semnet_1_GEN(eico,part_of,anst).
local_semnet_1_GEN(eico,part_of,emst).
local_semnet_1_GEN(eico,part_of,ffas).
local_semnet_1_GEN(eico,part_of,bpoc).
local_semnet_1_GEN(eico,part_of,tisu).
local_semnet_1_GEN(eico,part_of,cell).
local_semnet_1_GEN(eico,part_of,celc).
local_semnet_1_GEN(eico,part_of,anab).
local_semnet_1_GEN(strd,part_of,anim).
local_semnet_1_GEN(strd,part_of,arch).
local_semnet_1_GEN(strd,part_of,bact).
local_semnet_1_GEN(strd,part_of,fngs).
local_semnet_1_GEN(strd,part_of,humn).
%%% local_semnet_1_GEN(strd,part_of,invt).  % CA
local_semnet_1_GEN(strd,part_of,mamm).
local_semnet_1_GEN(strd,part_of,orgm).
local_semnet_1_GEN(strd,part_of,virs).
local_semnet_1_GEN(strd,part_of,vtbt).
local_semnet_1_GEN(strd,part_of,anst).
local_semnet_1_GEN(strd,part_of,emst).
local_semnet_1_GEN(strd,part_of,ffas).
local_semnet_1_GEN(strd,part_of,bpoc).
local_semnet_1_GEN(strd,part_of,tisu).
local_semnet_1_GEN(strd,part_of,cell).
local_semnet_1_GEN(strd,part_of,celc).
local_semnet_1_GEN(strd,part_of,anab).
local_semnet_1_GEN(nusq,part_of,anim).
local_semnet_1_GEN(nusq,part_of,arch).
local_semnet_1_GEN(nusq,part_of,bact).
local_semnet_1_GEN(nusq,part_of,fngs).
local_semnet_1_GEN(nusq,part_of,humn).
%%% local_semnet_1_GEN(nusq,part_of,invt).  % CA
local_semnet_1_GEN(nusq,part_of,mamm).
local_semnet_1_GEN(nusq,part_of,nonn). % GR 06/2015
local_semnet_1_GEN(nusq,part_of,orgm).
local_semnet_1_GEN(nusq,part_of,virs).
local_semnet_1_GEN(nusq,part_of,vtbt).
local_semnet_1_GEN(nusq,part_of,anst).
local_semnet_1_GEN(nusq,part_of,emst).
local_semnet_1_GEN(nusq,part_of,ffas).
local_semnet_1_GEN(nusq,part_of,bpoc).
local_semnet_1_GEN(nusq,part_of,tisu).
local_semnet_1_GEN(nusq,part_of,cell).
local_semnet_1_GEN(nusq,part_of,celc).
local_semnet_1_GEN(nusq,part_of,anab).
local_semnet_1_GEN(elii,part_of,anim).
local_semnet_1_GEN(elii,part_of,arch).
local_semnet_1_GEN(elii,part_of,bact).
local_semnet_1_GEN(elii,part_of,fngs).
local_semnet_1_GEN(elii,part_of,humn).
%%% local_semnet_1_GEN(elii,part_of,invt).  % CA
local_semnet_1_GEN(elii,part_of,mamm).
local_semnet_1_GEN(elii,part_of,orgm).
local_semnet_1_GEN(elii,part_of,virs).
local_semnet_1_GEN(elii,part_of,vtbt).
local_semnet_1_GEN(elii,part_of,anst).
local_semnet_1_GEN(elii,part_of,emst).
local_semnet_1_GEN(elii,part_of,ffas).
local_semnet_1_GEN(elii,part_of,bpoc).
local_semnet_1_GEN(elii,part_of,tisu).
local_semnet_1_GEN(elii,part_of,cell).
local_semnet_1_GEN(elii,part_of,celc).
local_semnet_1_GEN(elii,part_of,anab).
% ADDITIONS FOR NEOP GENERALIZATION
local_semnet_1_GEN(neop,part_of,alga).
local_semnet_1_GEN(neop,part_of,amph).
local_semnet_1_GEN(neop,part_of,anim).
local_semnet_1_GEN(neop,part_of,arch).
local_semnet_1_GEN(neop,part_of,bact).
local_semnet_1_GEN(neop,part_of,bird).
%local_semnet_1_GEN(neop,part_of,bpoc). % Commented out 06/16/16
% local_semnet_1_GEN(neop,part_of,cell).
local_semnet_1_GEN(neop,part_of,fish).
local_semnet_1_GEN(neop,part_of,fngs).
local_semnet_1_GEN(neop,part_of,humn).
%%% local_semnet_1_GEN(neop,part_of,invt).  % CA
local_semnet_1_GEN(neop,part_of,mamm).
local_semnet_1_GEN(neop,part_of,orgm).
local_semnet_1_GEN(neop,part_of,plnt).
local_semnet_1_GEN(neop,part_of,rept).
local_semnet_1_GEN(neop,part_of,rich).
%local_semnet_1_GEN(neop,part_of,tisu).
local_semnet_1_GEN(neop,part_of,virs).
local_semnet_1_GEN(neop,part_of,vtbt).
local_semnet_1_GEN(acab,part_of,neop).
local_semnet_1_GEN(anab,part_of,neop).
local_semnet_1_GEN(anst,part_of,neop).
%local_semnet_1_GEN(bpoc,part_of,neop).
local_semnet_1_GEN(celc,part_of,neop).
% local_semnet_1_GEN(cell,part_of,neop).
local_semnet_1_GEN(cgab,part_of,neop).
local_semnet_1_GEN(emst,part_of,neop).
local_semnet_1_GEN(ffas,part_of,neop).
local_semnet_1_GEN(gngm,part_of,neop).
local_semnet_1_GEN(tisu,part_of,neop).
local_semnet_1_GEN(gngm,part_of,anst).
local_semnet_1_GEN(gngm,part_of,emst).
local_semnet_1_GEN(gngm,part_of,ffas).
%local_semnet_1_GEN(gngm,part_of,bpoc). % 07/2015 also declared in SN '06
%local_semnet_1_GEN(gngm,part_of,tisu). % 07/2015 also declared in SN '06
%local_semnet_1_GEN(gngm,part_of,cell). % 07/2015 also declared in SN '06
%local_semnet_1_GEN(gngm,part_of,celc). % 07/2015 also declared in SN '06
local_semnet_1_GEN(gngm,part_of,blor).
local_semnet_1_GEN(gngm,part_of,invt). % 05/26/2017 source: GS - PMID 15634358 
% ADDITIONS FOR LOCATION_OF
local_semnet_1_GEN(alga,location_of,hops).
local_semnet_1_GEN(bact,location_of,hops).
local_semnet_1_GEN(fngs,location_of,hops).
local_semnet_1_GEN(plnt,location_of,hops).
local_semnet_1_GEN(rich,location_of,hops).
local_semnet_1_GEN(virs,location_of,hops).
local_semnet_1_GEN(alga,location_of,inch).
local_semnet_1_GEN(bact,location_of,inch).
local_semnet_1_GEN(fngs,location_of,inch).
local_semnet_1_GEN(plnt,location_of,inch).
local_semnet_1_GEN(rich,location_of,inch).
local_semnet_1_GEN(virs,location_of,inch).
local_semnet_1_GEN(alga,location_of,orch).
local_semnet_1_GEN(bact,location_of,orch).
local_semnet_1_GEN(fngs,location_of,orch).
local_semnet_1_GEN(plnt,location_of,orch).
local_semnet_1_GEN(rich,location_of,orch).
local_semnet_1_GEN(virs,location_of,orch).
local_semnet_1_GEN(alga,location_of,elii).
local_semnet_1_GEN(bact,location_of,elii).
local_semnet_1_GEN(fngs,location_of,elii).
local_semnet_1_GEN(plnt,location_of,elii).
local_semnet_1_GEN(rich,location_of,elii).
local_semnet_1_GEN(virs,location_of,elii).
local_semnet_1_GEN(alga,location_of,lipd).
local_semnet_1_GEN(bact,location_of,lipd).
local_semnet_1_GEN(fngs,location_of,lipd).
local_semnet_1_GEN(plnt,location_of,lipd).
local_semnet_1_GEN(rich,location_of,lipd).
local_semnet_1_GEN(virs,location_of,lipd).
local_semnet_1_GEN(alga,location_of,eico).
local_semnet_1_GEN(bact,location_of,eico).
local_semnet_1_GEN(fngs,location_of,eico).
local_semnet_1_GEN(plnt,location_of,eico).
local_semnet_1_GEN(rich,location_of,eico).
local_semnet_1_GEN(virs,location_of,eico).
local_semnet_1_GEN(alga,location_of,strd).
local_semnet_1_GEN(bact,location_of,strd).
local_semnet_1_GEN(fngs,location_of,strd).
local_semnet_1_GEN(plnt,location_of,strd).
local_semnet_1_GEN(rich,location_of,strd).
local_semnet_1_GEN(virs,location_of,strd).
local_semnet_1_GEN(alga,location_of,aapp).
local_semnet_1_GEN(bact,location_of,aapp).
local_semnet_1_GEN(fngs,location_of,aapp).
local_semnet_1_GEN(plnt,location_of,aapp).
local_semnet_1_GEN(rich,location_of,aapp).
local_semnet_1_GEN(virs,location_of,aapp).
local_semnet_1_GEN(alga,location_of,opco).
local_semnet_1_GEN(bact,location_of,opco).
local_semnet_1_GEN(fngs,location_of,opco).
local_semnet_1_GEN(plnt,location_of,opco).
local_semnet_1_GEN(rich,location_of,opco).
local_semnet_1_GEN(virs,location_of,opco).
local_semnet_1_GEN(alga,location_of,nusq).
local_semnet_1_GEN(bact,location_of,nusq).
local_semnet_1_GEN(fngs,location_of,nusq).
local_semnet_1_GEN(plnt,location_of,nusq).
local_semnet_1_GEN(rich,location_of,nusq).
local_semnet_1_GEN(virs,location_of,nusq).
local_semnet_1_GEN(alga,location_of,carb).
local_semnet_1_GEN(bact,location_of,carb).
local_semnet_1_GEN(fngs,location_of,carb).
local_semnet_1_GEN(plnt,location_of,carb).
local_semnet_1_GEN(rich,location_of,carb).
local_semnet_1_GEN(virs,location_of,carb).
local_semnet_1_GEN(humn,location_of,hops).
local_semnet_1_GEN(humn,location_of,inch).
% local_semnet_1_GEN(humn,location_of,orch).
local_semnet_1_GEN(humn,location_of,elii).
local_semnet_1_GEN(humn,location_of,lipd).
local_semnet_1_GEN(humn,location_of,eico).
local_semnet_1_GEN(humn,location_of,strd).
local_semnet_1_GEN(humn,location_of,aapp).
local_semnet_1_GEN(humn,location_of,opco).
local_semnet_1_GEN(humn,location_of,nusq).
local_semnet_1_GEN(humn,location_of,carb).
local_semnet_1_GEN(humn,location_of,horm).
local_semnet_1_GEN(mamm,location_of,hops).
local_semnet_1_GEN(mamm,location_of,inch).
local_semnet_1_GEN(mamm,location_of,orch).
local_semnet_1_GEN(mamm,location_of,elii).
local_semnet_1_GEN(mamm,location_of,lipd).
local_semnet_1_GEN(mamm,location_of,eico).
local_semnet_1_GEN(mamm,location_of,strd).
local_semnet_1_GEN(mamm,location_of,aapp).
local_semnet_1_GEN(mamm,location_of,opco).
local_semnet_1_GEN(mamm,location_of,nusq).
local_semnet_1_GEN(mamm,location_of,carb).
local_semnet_1_GEN(orgm,location_of,hops).
local_semnet_1_GEN(orgm,location_of,inch).
local_semnet_1_GEN(orgm,location_of,orch).
local_semnet_1_GEN(orgm,location_of,elii).
local_semnet_1_GEN(orgm,location_of,lipd).
local_semnet_1_GEN(orgm,location_of,eico).
local_semnet_1_GEN(orgm,location_of,strd).
local_semnet_1_GEN(orgm,location_of,aapp).
local_semnet_1_GEN(orgm,location_of,opco).
local_semnet_1_GEN(orgm,location_of,nusq).
local_semnet_1_GEN(orgm,location_of,carb).
local_semnet_1_GEN(anim,location_of,carb).
local_semnet_1_GEN(anim,location_of,hops).
local_semnet_1_GEN(anim,location_of,inch).
local_semnet_1_GEN(anim,location_of,orch).
local_semnet_1_GEN(anim,location_of,elii).
local_semnet_1_GEN(anim,location_of,lipd).
local_semnet_1_GEN(anim,location_of,eico).
local_semnet_1_GEN(anim,location_of,strd).
local_semnet_1_GEN(anim,location_of,aapp).
local_semnet_1_GEN(anim,location_of,opco).
local_semnet_1_GEN(anim,location_of,nusq).
local_semnet_1_GEN(arch,location_of,hops).
local_semnet_1_GEN(arch,location_of,inch).
local_semnet_1_GEN(arch,location_of,orch).
local_semnet_1_GEN(arch,location_of,elii).
local_semnet_1_GEN(arch,location_of,lipd).
local_semnet_1_GEN(arch,location_of,eico).
local_semnet_1_GEN(arch,location_of,strd).
local_semnet_1_GEN(arch,location_of,aapp).
local_semnet_1_GEN(arch,location_of,opco).
local_semnet_1_GEN(arch,location_of,nusq).
local_semnet_1_GEN(arch,location_of,carb).
local_semnet_1_GEN(bdsu,location_of,aapp). % GR 06/2015
local_semnet_1_GEN(bdsu,location_of,bacs). % GR 06/2015
local_semnet_1_GEN(bdsu,location_of,inch). % GR 06/2015
local_semnet_1_GEN(bdsu,location_of,orch). % GR 06/2015
local_semnet_1_GEN(bdsy,location_of,topp). % GR 06/2015
local_semnet_1_GEN(celc,location_of,npop). % GR 06/2015
%%% local_semnet_1_GEN(invt,location_of,carb).  % CA
%%% local_semnet_1_GEN(invt,location_of,hops).  % CA
%%% local_semnet_1_GEN(invt,location_of,inch).  % CA
%%% local_semnet_1_GEN(invt,location_of,orch).  % CA
%%% local_semnet_1_GEN(invt,location_of,elii).  % CA
%%% local_semnet_1_GEN(invt,location_of,lipd).  % CA
%%% local_semnet_1_GEN(invt,location_of,eico).  % CA
%%% local_semnet_1_GEN(invt,location_of,strd).  % CA
%%% local_semnet_1_GEN(invt,location_of,aapp).  % CA
%%% local_semnet_1_GEN(invt,location_of,opco).  % CA
%%% local_semnet_1_GEN(invt,location_of,nusq).  % CA
%%% local_semnet_1_GEN(invt,location_of,carb).  % CA
local_semnet_1_GEN(vtbt,location_of,carb).
local_semnet_1_GEN(vtbt,location_of,hops).
local_semnet_1_GEN(vtbt,location_of,inch).
local_semnet_1_GEN(vtbt,location_of,orch).
local_semnet_1_GEN(vtbt,location_of,elii).
local_semnet_1_GEN(vtbt,location_of,lipd).
local_semnet_1_GEN(vtbt,location_of,eico).
local_semnet_1_GEN(vtbt,location_of,strd).
local_semnet_1_GEN(vtbt,location_of,aapp).
local_semnet_1_GEN(vtbt,location_of,opco).
local_semnet_1_GEN(vtbt,location_of,nusq).
local_semnet_1_GEN(humn,location_of,anab). % GR 01/09/09
local_semnet_1_GEN(gngm,location_of,genf). % 05/26/2017 source: GS - PMID 15634358  The evolution of ray pattern has involved allelic variation at multiple loci.
local_semnet_1_GEN(tisu,location_of,ortf). % 06/26/2017 source: GS 
local_semnet_1_GEN(tisu,location_of,orgf). % 06/26/2017 source: GS 
local_semnet_1_GEN(tisu,location_of,lbpr). % 06/27/2017 source: GS 15592935 Immunohistochemical staining for type 1 and 2 HO isoforms were carried out in nasal inferior turbinate mucosa from six patients with persistent allergic rhinitis
% ADDITIONS FOR AFFECTS
local_semnet_1_GEN(phsu,affects,anab).
local_semnet_1_GEN(antb,affects,anab).
local_semnet_1_GEN(bacs,affects,anab).
local_semnet_1_GEN(imft,affects,anab).
local_semnet_1_GEN(vita,affects,anab).
local_semnet_1_GEN(enzy,affects,anab).
local_semnet_1_GEN(horm,affects,anab).
local_semnet_1_GEN(rcpt,affects,anab).
local_semnet_1_GEN(nsba,affects,anab).
local_semnet_1_GEN(hops,affects,anab).
local_semnet_1_GEN(inch,affects,anab).
local_semnet_1_GEN(orch,affects,anab).
local_semnet_1_GEN(lipd,affects,anab).
local_semnet_1_GEN(eico,affects,anab).
local_semnet_1_GEN(strd,affects,anab).
local_semnet_1_GEN(aapp,affects,anab).
local_semnet_1_GEN(opco,affects,anab).
local_semnet_1_GEN(nusq,affects,anab).
local_semnet_1_GEN(carb,affects,anab).
local_semnet_1_GEN(elii,affects,anab).
local_semnet_1_GEN(phsu,affects,acab).
local_semnet_1_GEN(antb,affects,acab).
local_semnet_1_GEN(bacs,affects,acab).
local_semnet_1_GEN(imft,affects,acab).
local_semnet_1_GEN(vita,affects,acab).
local_semnet_1_GEN(enzy,affects,acab).
local_semnet_1_GEN(horm,affects,acab).
local_semnet_1_GEN(rcpt,affects,acab).
local_semnet_1_GEN(nsba,affects,acab).
local_semnet_1_GEN(hops,affects,acab).
local_semnet_1_GEN(inch,affects,acab).
local_semnet_1_GEN(orch,affects,acab).
local_semnet_1_GEN(lipd,affects,acab).
local_semnet_1_GEN(eico,affects,acab).
local_semnet_1_GEN(strd,affects,acab).
local_semnet_1_GEN(aapp,affects,acab).
local_semnet_1_GEN(opco,affects,acab).
local_semnet_1_GEN(nusq,affects,acab).
local_semnet_1_GEN(carb,affects,acab).
local_semnet_1_GEN(elii,affects,acab).
local_semnet_1_GEN(aapp,affects,bdsy). % GR 06/2015
local_semnet_1_GEN(phsu,affects,cgab).
local_semnet_1_GEN(antb,affects,cgab).
local_semnet_1_GEN(bacs,affects,cgab).
local_semnet_1_GEN(imft,affects,cgab).
local_semnet_1_GEN(vita,affects,cgab).
local_semnet_1_GEN(enzy,affects,cgab).
local_semnet_1_GEN(horm,affects,cgab).
local_semnet_1_GEN(rcpt,affects,cgab).
local_semnet_1_GEN(nsba,affects,cgab).
local_semnet_1_GEN(hops,affects,cgab).
local_semnet_1_GEN(inch,affects,cgab).
local_semnet_1_GEN(orch,affects,cgab).
local_semnet_1_GEN(lipd,affects,cgab).
local_semnet_1_GEN(eico,affects,cgab).
local_semnet_1_GEN(strd,affects,cgab).
local_semnet_1_GEN(aapp,affects,cgab).
local_semnet_1_GEN(opco,affects,cgab).
local_semnet_1_GEN(nusq,affects,cgab).
local_semnet_1_GEN(carb,affects,cgab).
local_semnet_1_GEN(elii,affects,cgab).
local_semnet_1_GEN(phsu,affects,sosy).
local_semnet_1_GEN(antb,affects,sosy).
local_semnet_1_GEN(bacs,affects,sosy).
local_semnet_1_GEN(imft,affects,sosy).
local_semnet_1_GEN(vita,affects,sosy).
local_semnet_1_GEN(enzy,affects,sosy).
local_semnet_1_GEN(horm,affects,sosy).
local_semnet_1_GEN(rcpt,affects,sosy).
local_semnet_1_GEN(nsba,affects,sosy).
local_semnet_1_GEN(hops,affects,sosy).
local_semnet_1_GEN(inch,affects,sosy).
local_semnet_1_GEN(orch,affects,sosy).
local_semnet_1_GEN(lipd,affects,sosy).
local_semnet_1_GEN(eico,affects,sosy).
local_semnet_1_GEN(strd,affects,sosy).
local_semnet_1_GEN(aapp,affects,sosy).
local_semnet_1_GEN(opco,affects,sosy).
local_semnet_1_GEN(nusq,affects,sosy).
local_semnet_1_GEN(carb,affects,sosy).
local_semnet_1_GEN(elii,affects,sosy).
local_semnet_1_GEN(phsu,affects,inpo).
local_semnet_1_GEN(antb,affects,inpo).
local_semnet_1_GEN(bacs,affects,inpo).
local_semnet_1_GEN(imft,affects,inpo).
local_semnet_1_GEN(vita,affects,inpo).
local_semnet_1_GEN(enzy,affects,inpo).
local_semnet_1_GEN(horm,affects,inpo).
local_semnet_1_GEN(rcpt,affects,inpo).
local_semnet_1_GEN(nsba,affects,inpo).
local_semnet_1_GEN(hops,affects,inpo).
local_semnet_1_GEN(inch,affects,inpo).
local_semnet_1_GEN(orch,affects,inpo).
local_semnet_1_GEN(lipd,affects,inpo).
local_semnet_1_GEN(eico,affects,inpo).
local_semnet_1_GEN(strd,affects,inpo).
local_semnet_1_GEN(aapp,affects,inpo).
local_semnet_1_GEN(opco,affects,inpo).
local_semnet_1_GEN(nusq,affects,inpo).
local_semnet_1_GEN(carb,affects,inpo).
local_semnet_1_GEN(elii,affects,inpo).
local_semnet_1_GEN(phsu,affects,anst).
local_semnet_1_GEN(antb,affects,anst).
local_semnet_1_GEN(bacs,affects,anst).
local_semnet_1_GEN(imft,affects,anst).
local_semnet_1_GEN(vita,affects,anst).
local_semnet_1_GEN(enzy,affects,anst).
local_semnet_1_GEN(horm,affects,anst).
local_semnet_1_GEN(rcpt,affects,anst).
local_semnet_1_GEN(nsba,affects,anst).
local_semnet_1_GEN(hops,affects,anst).
local_semnet_1_GEN(antb,affects,emst).
local_semnet_1_GEN(bacs,affects,emst).
local_semnet_1_GEN(imft,affects,emst).
local_semnet_1_GEN(vita,affects,emst).
local_semnet_1_GEN(enzy,affects,emst).
local_semnet_1_GEN(horm,affects,emst).
local_semnet_1_GEN(rcpt,affects,emst).
local_semnet_1_GEN(nsba,affects,emst).
local_semnet_1_GEN(hops,affects,emst).
local_semnet_1_GEN(antb,affects,ffas).
local_semnet_1_GEN(bacs,affects,ffas).
local_semnet_1_GEN(imft,affects,ffas).
local_semnet_1_GEN(vita,affects,ffas).
local_semnet_1_GEN(enzy,affects,ffas).
local_semnet_1_GEN(horm,affects,ffas).
local_semnet_1_GEN(rcpt,affects,ffas).
local_semnet_1_GEN(nsba,affects,ffas).
local_semnet_1_GEN(hops,affects,ffas).
local_semnet_1_GEN(orch,affects,ffas).
local_semnet_1_GEN(lipd,affects,ffas).
local_semnet_1_GEN(eico,affects,ffas).
local_semnet_1_GEN(strd,affects,ffas).
local_semnet_1_GEN(antb,affects,bpoc).
local_semnet_1_GEN(bacs,affects,bpoc).
local_semnet_1_GEN(imft,affects,bpoc).
local_semnet_1_GEN(vita,affects,bpoc).
local_semnet_1_GEN(enzy,affects,bpoc).
local_semnet_1_GEN(horm,affects,bpoc).
local_semnet_1_GEN(rcpt,affects,bpoc).
local_semnet_1_GEN(nsba,affects,bpoc).
local_semnet_1_GEN(hops,affects,bpoc).
local_semnet_1_GEN(inch,affects,bpoc).
local_semnet_1_GEN(orch,affects,bpoc).
local_semnet_1_GEN(lipd,affects,bpoc).
local_semnet_1_GEN(eico,affects,bpoc).
local_semnet_1_GEN(strd,affects,bpoc).
local_semnet_1_GEN(aapp,affects,bpoc).
local_semnet_1_GEN(opco,affects,bpoc).
local_semnet_1_GEN(nusq,affects,bpoc).
local_semnet_1_GEN(carb,affects,bpoc).
local_semnet_1_GEN(elii,affects,bpoc).
local_semnet_1_GEN(phsu,affects,tisu).
local_semnet_1_GEN(antb,affects,tisu).
local_semnet_1_GEN(bacs,affects,tisu).
local_semnet_1_GEN(imft,affects,tisu).
local_semnet_1_GEN(vita,affects,tisu).
local_semnet_1_GEN(enzy,affects,tisu).
local_semnet_1_GEN(horm,affects,tisu).
local_semnet_1_GEN(rcpt,affects,tisu).
local_semnet_1_GEN(nsba,affects,tisu).
local_semnet_1_GEN(hops,affects,tisu).
local_semnet_1_GEN(inch,affects,tisu).
local_semnet_1_GEN(orch,affects,tisu).
local_semnet_1_GEN(lipd,affects,tisu).
local_semnet_1_GEN(eico,affects,tisu).
local_semnet_1_GEN(strd,affects,tisu).
local_semnet_1_GEN(aapp,affects,tisu).
local_semnet_1_GEN(opco,affects,tisu).
local_semnet_1_GEN(nusq,affects,tisu).
local_semnet_1_GEN(carb,affects,tisu).
local_semnet_1_GEN(elii,affects,tisu).
local_semnet_1_GEN(antb,affects,cell).
local_semnet_1_GEN(bacs,affects,cell).
local_semnet_1_GEN(imft,affects,cell).
local_semnet_1_GEN(vita,affects,cell).
local_semnet_1_GEN(enzy,affects,cell).
local_semnet_1_GEN(horm,affects,cell).
local_semnet_1_GEN(rcpt,affects,cell).
local_semnet_1_GEN(nsba,affects,cell).
local_semnet_1_GEN(hops,affects,cell).
local_semnet_1_GEN(inch,affects,cell).
local_semnet_1_GEN(orch,affects,cell).
local_semnet_1_GEN(lipd,affects,cell).
local_semnet_1_GEN(eico,affects,cell).
local_semnet_1_GEN(strd,affects,cell).
local_semnet_1_GEN(aapp,affects,cell).
local_semnet_1_GEN(opco,affects,cell).
local_semnet_1_GEN(nusq,affects,cell).
local_semnet_1_GEN(carb,affects,cell).
local_semnet_1_GEN(elii,affects,cell).
local_semnet_1_GEN(antb,affects,celc).
local_semnet_1_GEN(bacs,affects,celc).
local_semnet_1_GEN(imft,affects,celc).
local_semnet_1_GEN(vita,affects,celc).
local_semnet_1_GEN(enzy,affects,celc).
local_semnet_1_GEN(horm,affects,celc).
local_semnet_1_GEN(rcpt,affects,celc).
local_semnet_1_GEN(nsba,affects,celc).
local_semnet_1_GEN(hops,affects,celc).
local_semnet_1_GEN(inch,affects,celc).
local_semnet_1_GEN(orch,affects,celc).
local_semnet_1_GEN(lipd,affects,celc).
local_semnet_1_GEN(eico,affects,celc).
local_semnet_1_GEN(strd,affects,celc).
local_semnet_1_GEN(aapp,affects,celc).
local_semnet_1_GEN(opco,affects,celc).
local_semnet_1_GEN(nusq,affects,celc).
local_semnet_1_GEN(carb,affects,celc).
local_semnet_1_GEN(elii,affects,celc).
local_semnet_1_GEN(antb,affects,gngm). % GR 06/2015
local_semnet_1_GEN(moft,affects,gngm). % GR 06/2015
local_semnet_1_GEN(moft,affects,orgf). % GR 06/2015
local_semnet_1_GEN(gngm,affects,ffas).
local_semnet_1_GEN(gngm,affects,bpoc).
local_semnet_1_GEN(gngm,affects,tisu).
local_semnet_1_GEN(gngm,affects,cell).
local_semnet_1_GEN(gngm,affects,celc).
%local_semnet_1_GEN(gngm,affects,phsf). % GR 07/2015 triple also in SN '06
%local_semnet_1_GEN(gngm,affects,orgf). % GR 07/2015 triple also in SN '06
%local_semnet_1_GEN(gngm,affects,celf). % GR 07/2015 triple also in SN '06
%local_semnet_1_GEN(gngm,affects,menp). % GR 07/2015 triple also in exceptions.pl
local_semnet_1_GEN(gngm,affects,patf).
local_semnet_1_GEN(gngm,affects,dsyn).
local_semnet_1_GEN(gngm,affects,neop).
local_semnet_1_GEN(gngm,affects,comd).
%%% local_semnet_1_GEN(gngm,affects,fndg).  % CA
local_semnet_1_GEN(gngm,affects,sosy).
local_semnet_1_GEN(gngm,affects,inpo).
local_semnet_1_GEN(gngm,affects,anab).
local_semnet_1_GEN(gngm,affects,acab).
local_semnet_1_GEN(gngm,affects,cgab).
% ADDITIONS FOR DISRUPTS
local_semnet_1_GEN(aapp,disrupts,acab).
local_semnet_1_GEN(antb,disrupts,acab).
local_semnet_1_GEN(bacs,disrupts,acab).
local_semnet_1_GEN(carb,disrupts,acab).
local_semnet_1_GEN(eico,disrupts,acab).
local_semnet_1_GEN(elii,disrupts,acab).
local_semnet_1_GEN(enzy,disrupts,acab).
local_semnet_1_GEN(gngm,disrupts,acab).
local_semnet_1_GEN(hops,disrupts,acab).
local_semnet_1_GEN(horm,disrupts,acab).
local_semnet_1_GEN(imft,disrupts,acab).
local_semnet_1_GEN(inch,disrupts,acab).
local_semnet_1_GEN(lipd,disrupts,acab).
local_semnet_1_GEN(nnon,disrupts,acab).
local_semnet_1_GEN(nsba,disrupts,acab).
local_semnet_1_GEN(nusq,disrupts,acab).
local_semnet_1_GEN(opco,disrupts,acab).
local_semnet_1_GEN(orch,disrupts,acab).
local_semnet_1_GEN(phsu,disrupts,acab).
local_semnet_1_GEN(inch,disrupts,anst).
local_semnet_1_GEN(rcpt,disrupts,acab).
local_semnet_1_GEN(strd,disrupts,acab).
local_semnet_1_GEN(vita,disrupts,acab).
local_semnet_1_GEN(aapp,disrupts,anab).
local_semnet_1_GEN(antb,disrupts,anab).
local_semnet_1_GEN(bacs,disrupts,anab).
local_semnet_1_GEN(carb,disrupts,anab).
local_semnet_1_GEN(eico,disrupts,anab).
local_semnet_1_GEN(elii,disrupts,anab).
local_semnet_1_GEN(enzy,disrupts,anab).
local_semnet_1_GEN(gngm,disrupts,anab).
local_semnet_1_GEN(hops,disrupts,anab).
local_semnet_1_GEN(horm,disrupts,anab).
local_semnet_1_GEN(imft,disrupts,anab).
local_semnet_1_GEN(inch,disrupts,anab).
local_semnet_1_GEN(lipd,disrupts,anab).
local_semnet_1_GEN(nnon,disrupts,anab).
local_semnet_1_GEN(nsba,disrupts,anab).
local_semnet_1_GEN(nusq,disrupts,anab).
local_semnet_1_GEN(opco,disrupts,anab).
local_semnet_1_GEN(orch,disrupts,anab).
local_semnet_1_GEN(phsu,disrupts,anab).
local_semnet_1_GEN(rcpt,disrupts,anab).
local_semnet_1_GEN(strd,disrupts,anab).
local_semnet_1_GEN(vita,disrupts,anab).
local_semnet_1_GEN(aapp,disrupts,anst).
local_semnet_1_GEN(carb,disrupts,anst).
local_semnet_1_GEN(eico,disrupts,anst).
local_semnet_1_GEN(elii,disrupts,anst).
local_semnet_1_GEN(gngm,disrupts,anst).
local_semnet_1_GEN(lipd,disrupts,anst).
local_semnet_1_GEN(nnon,disrupts,anst).
local_semnet_1_GEN(nusq,disrupts,anst).
local_semnet_1_GEN(opco,disrupts,anst).
local_semnet_1_GEN(orch,disrupts,anst).
local_semnet_1_GEN(phsu,disrupts,anst).
local_semnet_1_GEN(strd,disrupts,anst).
local_semnet_1_GEN(aapp,disrupts,biof).
local_semnet_1_GEN(carb,disrupts,biof).
local_semnet_1_GEN(eico,disrupts,biof).
local_semnet_1_GEN(elii,disrupts,biof).
local_semnet_1_GEN(gngm,disrupts,biof).
local_semnet_1_GEN(inch,disrupts,biof).
local_semnet_1_GEN(lipd,disrupts,biof).
local_semnet_1_GEN(nnon,disrupts,biof).
local_semnet_1_GEN(nusq,disrupts,biof).
local_semnet_1_GEN(opco,disrupts,biof).
local_semnet_1_GEN(orch,disrupts,biof).
local_semnet_1_GEN(phsu,disrupts,biof).
local_semnet_1_GEN(strd,disrupts,biof).
local_semnet_1_GEN(aapp,disrupts,bpoc).
local_semnet_1_GEN(carb,disrupts,bpoc).
local_semnet_1_GEN(eico,disrupts,bpoc).
local_semnet_1_GEN(elii,disrupts,bpoc).
local_semnet_1_GEN(gngm,disrupts,bpoc).
local_semnet_1_GEN(inch,disrupts,bpoc).
local_semnet_1_GEN(lipd,disrupts,bpoc).
local_semnet_1_GEN(nnon,disrupts,bpoc).
local_semnet_1_GEN(nusq,disrupts,bpoc).
local_semnet_1_GEN(opco,disrupts,bpoc).
local_semnet_1_GEN(orch,disrupts,bpoc).
local_semnet_1_GEN(strd,disrupts,bpoc).
local_semnet_1_GEN(aapp,disrupts,celc).
local_semnet_1_GEN(carb,disrupts,celc).
local_semnet_1_GEN(eico,disrupts,celc).
local_semnet_1_GEN(elii,disrupts,celc).
local_semnet_1_GEN(gngm,disrupts,celc).
local_semnet_1_GEN(inch,disrupts,celc).
local_semnet_1_GEN(lipd,disrupts,celc).
local_semnet_1_GEN(nnon,disrupts,celc).
local_semnet_1_GEN(nusq,disrupts,celc).
local_semnet_1_GEN(opco,disrupts,celc).
local_semnet_1_GEN(orch,disrupts,celc).
local_semnet_1_GEN(strd,disrupts,celc).
local_semnet_1_GEN(aapp,disrupts,celf).
local_semnet_1_GEN(carb,disrupts,celf).
local_semnet_1_GEN(eico,disrupts,celf).
local_semnet_1_GEN(elii,disrupts,celf).
local_semnet_1_GEN(gngm,disrupts,celf).
local_semnet_1_GEN(inch,disrupts,celf).
local_semnet_1_GEN(lipd,disrupts,celf).
local_semnet_1_GEN(nnon,disrupts,celf).
local_semnet_1_GEN(nusq,disrupts,celf).
local_semnet_1_GEN(opco,disrupts,celf).
local_semnet_1_GEN(orch,disrupts,celf).
local_semnet_1_GEN(strd,disrupts,celf).
local_semnet_1_GEN(aapp,disrupts,cell).
local_semnet_1_GEN(carb,disrupts,cell).
local_semnet_1_GEN(eico,disrupts,cell).
local_semnet_1_GEN(elii,disrupts,cell).
local_semnet_1_GEN(gngm,disrupts,cell).
local_semnet_1_GEN(inch,disrupts,cell).
local_semnet_1_GEN(lipd,disrupts,cell).
local_semnet_1_GEN(nnon,disrupts,cell).
local_semnet_1_GEN(nusq,disrupts,cell).
local_semnet_1_GEN(opco,disrupts,cell).
local_semnet_1_GEN(orch,disrupts,cell).
local_semnet_1_GEN(strd,disrupts,cell).
local_semnet_1_GEN(aapp,disrupts,cgab).
local_semnet_1_GEN(antb,disrupts,cgab).
local_semnet_1_GEN(bacs,disrupts,cgab).
local_semnet_1_GEN(carb,disrupts,cgab).
local_semnet_1_GEN(eico,disrupts,cgab).
local_semnet_1_GEN(elii,disrupts,cgab).
local_semnet_1_GEN(enzy,disrupts,cgab).
local_semnet_1_GEN(gngm,disrupts,cgab).
local_semnet_1_GEN(hops,disrupts,cgab).
local_semnet_1_GEN(horm,disrupts,cgab).
local_semnet_1_GEN(imft,disrupts,cgab).
local_semnet_1_GEN(inch,disrupts,cgab).
local_semnet_1_GEN(lipd,disrupts,cgab).
local_semnet_1_GEN(nnon,disrupts,cgab).
local_semnet_1_GEN(nsba,disrupts,cgab).
local_semnet_1_GEN(nusq,disrupts,cgab).
local_semnet_1_GEN(opco,disrupts,cgab).
local_semnet_1_GEN(orch,disrupts,cgab).
local_semnet_1_GEN(phsu,disrupts,cgab).
local_semnet_1_GEN(rcpt,disrupts,cgab).
local_semnet_1_GEN(strd,disrupts,cgab).
local_semnet_1_GEN(vita,disrupts,cgab).
local_semnet_1_GEN(aapp,disrupts,comd).
local_semnet_1_GEN(antb,disrupts,comd).
local_semnet_1_GEN(bacs,disrupts,comd).
local_semnet_1_GEN(carb,disrupts,comd).
local_semnet_1_GEN(eico,disrupts,comd).
local_semnet_1_GEN(elii,disrupts,comd).
local_semnet_1_GEN(enzy,disrupts,comd).
local_semnet_1_GEN(gngm,disrupts,comd).
local_semnet_1_GEN(hops,disrupts,comd).
local_semnet_1_GEN(horm,disrupts,comd).
local_semnet_1_GEN(imft,disrupts,comd).
local_semnet_1_GEN(inch,disrupts,comd).
local_semnet_1_GEN(lipd,disrupts,comd).
local_semnet_1_GEN(nnon,disrupts,comd).
local_semnet_1_GEN(nsba,disrupts,comd).
local_semnet_1_GEN(nusq,disrupts,comd).
local_semnet_1_GEN(opco,disrupts,comd).
local_semnet_1_GEN(orch,disrupts,comd).
local_semnet_1_GEN(phsu,disrupts,comd).
local_semnet_1_GEN(rcpt,disrupts,comd).
local_semnet_1_GEN(strd,disrupts,comd).
local_semnet_1_GEN(vita,disrupts,comd).
local_semnet_1_GEN(aapp,disrupts,dsyn).
local_semnet_1_GEN(antb,disrupts,dsyn).
local_semnet_1_GEN(bacs,disrupts,dsyn).
local_semnet_1_GEN(carb,disrupts,dsyn).
local_semnet_1_GEN(eico,disrupts,dsyn).
local_semnet_1_GEN(elii,disrupts,dsyn).
local_semnet_1_GEN(enzy,disrupts,dsyn).
local_semnet_1_GEN(gngm,disrupts,dsyn).
local_semnet_1_GEN(hops,disrupts,dsyn).
local_semnet_1_GEN(horm,disrupts,dsyn).
local_semnet_1_GEN(imft,disrupts,dsyn).
local_semnet_1_GEN(inch,disrupts,dsyn).
local_semnet_1_GEN(lipd,disrupts,dsyn).
local_semnet_1_GEN(nnon,disrupts,dsyn).
local_semnet_1_GEN(nsba,disrupts,dsyn).
local_semnet_1_GEN(nusq,disrupts,dsyn).
local_semnet_1_GEN(opco,disrupts,dsyn).
local_semnet_1_GEN(orch,disrupts,dsyn).
local_semnet_1_GEN(phsu,disrupts,dsyn).
local_semnet_1_GEN(rcpt,disrupts,dsyn).
local_semnet_1_GEN(strd,disrupts,dsyn).
local_semnet_1_GEN(vita,disrupts,dsyn).
local_semnet_1_GEN(aapp,disrupts,emst).
local_semnet_1_GEN(carb,disrupts,emst).
local_semnet_1_GEN(eico,disrupts,emst).
local_semnet_1_GEN(elii,disrupts,emst).
local_semnet_1_GEN(gngm,disrupts,emst).
local_semnet_1_GEN(inch,disrupts,emst).
local_semnet_1_GEN(lipd,disrupts,emst).
local_semnet_1_GEN(nnon,disrupts,emst).
local_semnet_1_GEN(nusq,disrupts,emst).
local_semnet_1_GEN(opco,disrupts,emst).
local_semnet_1_GEN(orch,disrupts,emst).
local_semnet_1_GEN(strd,disrupts,emst).
local_semnet_1_GEN(aapp,disrupts,ffas).
local_semnet_1_GEN(carb,disrupts,ffas).
local_semnet_1_GEN(eico,disrupts,ffas).
local_semnet_1_GEN(elii,disrupts,ffas).
local_semnet_1_GEN(gngm,disrupts,ffas).
local_semnet_1_GEN(inch,disrupts,ffas).
local_semnet_1_GEN(lipd,disrupts,ffas).
local_semnet_1_GEN(nnon,disrupts,ffas).
local_semnet_1_GEN(nusq,disrupts,ffas).
local_semnet_1_GEN(opco,disrupts,ffas).
local_semnet_1_GEN(orch,disrupts,ffas).
local_semnet_1_GEN(strd,disrupts,ffas).
local_semnet_1_GEN(aapp,disrupts,fndg).
local_semnet_1_GEN(carb,disrupts,fndg).
local_semnet_1_GEN(eico,disrupts,fndg).
local_semnet_1_GEN(elii,disrupts,fndg).
local_semnet_1_GEN(gngm,disrupts,fndg).
local_semnet_1_GEN(inch,disrupts,fndg).
local_semnet_1_GEN(lipd,disrupts,fndg).
local_semnet_1_GEN(nnon,disrupts,fndg).
local_semnet_1_GEN(nusq,disrupts,fndg).
local_semnet_1_GEN(opco,disrupts,fndg).
local_semnet_1_GEN(orch,disrupts,fndg).
local_semnet_1_GEN(strd,disrupts,fndg).
local_semnet_1_GEN(aapp,disrupts,inpo).
local_semnet_1_GEN(antb,disrupts,inpo).
local_semnet_1_GEN(bacs,disrupts,inpo).
local_semnet_1_GEN(carb,disrupts,inpo).
local_semnet_1_GEN(eico,disrupts,inpo).
local_semnet_1_GEN(elii,disrupts,inpo).
local_semnet_1_GEN(enzy,disrupts,inpo).
local_semnet_1_GEN(gngm,disrupts,inpo).
local_semnet_1_GEN(hops,disrupts,inpo).
local_semnet_1_GEN(horm,disrupts,inpo).
local_semnet_1_GEN(imft,disrupts,inpo).
local_semnet_1_GEN(inch,disrupts,inpo).
local_semnet_1_GEN(lipd,disrupts,inpo).
local_semnet_1_GEN(nnon,disrupts,inpo).
local_semnet_1_GEN(nsba,disrupts,inpo).
local_semnet_1_GEN(nusq,disrupts,inpo).
local_semnet_1_GEN(opco,disrupts,inpo).
local_semnet_1_GEN(orch,disrupts,inpo).
local_semnet_1_GEN(phsu,disrupts,inpo).
local_semnet_1_GEN(rcpt,disrupts,inpo).
local_semnet_1_GEN(strd,disrupts,inpo).
local_semnet_1_GEN(vita,disrupts,inpo).
local_semnet_1_GEN(aapp,disrupts,menp).
local_semnet_1_GEN(carb,disrupts,menp).
local_semnet_1_GEN(eico,disrupts,menp).
local_semnet_1_GEN(elii,disrupts,menp).
local_semnet_1_GEN(gngm,disrupts,menp).
local_semnet_1_GEN(inch,disrupts,menp).
local_semnet_1_GEN(lipd,disrupts,menp).
local_semnet_1_GEN(nnon,disrupts,menp).
local_semnet_1_GEN(nusq,disrupts,menp).
local_semnet_1_GEN(opco,disrupts,menp).
local_semnet_1_GEN(orch,disrupts,menp).
local_semnet_1_GEN(strd,disrupts,menp).
local_semnet_1_GEN(aapp,disrupts,moft).
%local_semnet_1_GEN(antb,disrupts,moft). % GR 08/2015 commented out and left it blocked in exceptions
local_semnet_1_GEN(bacs,disrupts,moft).
%local_semnet_1_GEN(carb,disrupts,moft). % GR 08/2015 commented out
local_semnet_1_GEN(enzy,disrupts,moft).
local_semnet_1_GEN(gngm,disrupts,moft).
local_semnet_1_GEN(hops,disrupts,moft).
local_semnet_1_GEN(horm,disrupts,moft).
local_semnet_1_GEN(imft,disrupts,moft).
local_semnet_1_GEN(nsba,disrupts,moft).
local_semnet_1_GEN(nusq,disrupts,moft).
local_semnet_1_GEN(opco,disrupts,moft).
local_semnet_1_GEN(phsu,disrupts,moft).
local_semnet_1_GEN(rcpt,disrupts,moft).
local_semnet_1_GEN(strd,disrupts,moft).
local_semnet_1_GEN(aapp,disrupts,neop).
local_semnet_1_GEN(antb,disrupts,neop).
local_semnet_1_GEN(bacs,disrupts,neop).
local_semnet_1_GEN(carb,disrupts,neop).
local_semnet_1_GEN(eico,disrupts,neop).
local_semnet_1_GEN(elii,disrupts,neop).
local_semnet_1_GEN(enzy,disrupts,neop).
local_semnet_1_GEN(gngm,disrupts,neop).
local_semnet_1_GEN(hops,disrupts,neop).
local_semnet_1_GEN(horm,disrupts,neop).
local_semnet_1_GEN(imft,disrupts,neop).
local_semnet_1_GEN(inch,disrupts,neop).
local_semnet_1_GEN(lipd,disrupts,neop).
local_semnet_1_GEN(nnon,disrupts,neop).
local_semnet_1_GEN(nsba,disrupts,neop).
local_semnet_1_GEN(nusq,disrupts,neop).
local_semnet_1_GEN(opco,disrupts,neop).
local_semnet_1_GEN(orch,disrupts,neop).
local_semnet_1_GEN(phsu,disrupts,neop).
local_semnet_1_GEN(rcpt,disrupts,neop).
local_semnet_1_GEN(strd,disrupts,neop).
local_semnet_1_GEN(vita,disrupts,neop).
local_semnet_1_GEN(aapp,disrupts,npop).
local_semnet_1_GEN(antb,disrupts,npop).
local_semnet_1_GEN(bacs,disrupts,npop).
local_semnet_1_GEN(carb,disrupts,npop).
local_semnet_1_GEN(eico,disrupts,npop).
local_semnet_1_GEN(elii,disrupts,npop).
local_semnet_1_GEN(enzy,disrupts,npop).
local_semnet_1_GEN(gngm,disrupts,npop).
local_semnet_1_GEN(hops,disrupts,npop).
local_semnet_1_GEN(horm,disrupts,npop).
local_semnet_1_GEN(imft,disrupts,npop).
local_semnet_1_GEN(inch,disrupts,npop).
local_semnet_1_GEN(lipd,disrupts,npop).
local_semnet_1_GEN(nnon,disrupts,npop).
local_semnet_1_GEN(nsba,disrupts,npop).
local_semnet_1_GEN(nusq,disrupts,npop).
local_semnet_1_GEN(opco,disrupts,npop).
local_semnet_1_GEN(orch,disrupts,npop).
local_semnet_1_GEN(phsu,disrupts,npop).
local_semnet_1_GEN(rcpt,disrupts,npop).
local_semnet_1_GEN(strd,disrupts,npop).
local_semnet_1_GEN(vita,disrupts,npop).
local_semnet_1_GEN(aapp,disrupts,orgf).
local_semnet_1_GEN(carb,disrupts,orgf).
local_semnet_1_GEN(eico,disrupts,orgf).
local_semnet_1_GEN(elii,disrupts,orgf).
local_semnet_1_GEN(gngm,disrupts,orgf).
local_semnet_1_GEN(inch,disrupts,orgf).
local_semnet_1_GEN(lipd,disrupts,orgf).
local_semnet_1_GEN(nnon,disrupts,orgf).
local_semnet_1_GEN(nusq,disrupts,orgf).
local_semnet_1_GEN(opco,disrupts,orgf).
local_semnet_1_GEN(orch,disrupts,orgf).
local_semnet_1_GEN(strd,disrupts,orgf).
local_semnet_1_GEN(aapp,disrupts,patf).
local_semnet_1_GEN(antb,disrupts,patf).
local_semnet_1_GEN(bacs,disrupts,patf).
local_semnet_1_GEN(carb,disrupts,patf).
local_semnet_1_GEN(eico,disrupts,patf).
local_semnet_1_GEN(elii,disrupts,patf).
local_semnet_1_GEN(enzy,disrupts,patf).
local_semnet_1_GEN(gngm,disrupts,patf).
local_semnet_1_GEN(hops,disrupts,patf).
local_semnet_1_GEN(horm,disrupts,patf).
local_semnet_1_GEN(imft,disrupts,patf).
local_semnet_1_GEN(inch,disrupts,patf).
local_semnet_1_GEN(lipd,disrupts,patf).
local_semnet_1_GEN(nnon,disrupts,patf).
local_semnet_1_GEN(nsba,disrupts,patf).
local_semnet_1_GEN(nusq,disrupts,patf).
local_semnet_1_GEN(opco,disrupts,patf).
local_semnet_1_GEN(orch,disrupts,patf).
local_semnet_1_GEN(rcpt,disrupts,patf).
local_semnet_1_GEN(strd,disrupts,patf).
local_semnet_1_GEN(vita,disrupts,patf).
local_semnet_1_GEN(aapp,disrupts,phsf).
local_semnet_1_GEN(carb,disrupts,phsf).
local_semnet_1_GEN(eico,disrupts,phsf).
local_semnet_1_GEN(elii,disrupts,phsf).
local_semnet_1_GEN(gngm,disrupts,phsf).
local_semnet_1_GEN(inch,disrupts,phsf).
local_semnet_1_GEN(lipd,disrupts,phsf).
local_semnet_1_GEN(nnon,disrupts,phsf).
local_semnet_1_GEN(nusq,disrupts,phsf).
local_semnet_1_GEN(opco,disrupts,phsf).
local_semnet_1_GEN(orch,disrupts,phsf).
%local_semnet_1_GEN(phsu,disrupts,phsf).
%local_semnet_1_GEN(strd,disrupts,phsf). % 07/2015 triple also in SN '06
local_semnet_1_GEN(aapp,disrupts,sosy).
local_semnet_1_GEN(antb,disrupts,sosy).
local_semnet_1_GEN(bacs,disrupts,sosy).
local_semnet_1_GEN(carb,disrupts,sosy).
local_semnet_1_GEN(eico,disrupts,sosy).
local_semnet_1_GEN(elii,disrupts,sosy).
local_semnet_1_GEN(enzy,disrupts,sosy).
local_semnet_1_GEN(gngm,disrupts,sosy).
local_semnet_1_GEN(hops,disrupts,sosy).
local_semnet_1_GEN(horm,disrupts,sosy).
local_semnet_1_GEN(imft,disrupts,sosy).
local_semnet_1_GEN(inch,disrupts,sosy).
local_semnet_1_GEN(lipd,disrupts,sosy).
local_semnet_1_GEN(nnon,disrupts,sosy).
local_semnet_1_GEN(nsba,disrupts,sosy).
local_semnet_1_GEN(nusq,disrupts,sosy).
local_semnet_1_GEN(opco,disrupts,sosy).
local_semnet_1_GEN(orch,disrupts,sosy).
local_semnet_1_GEN(phsu,disrupts,sosy).
local_semnet_1_GEN(rcpt,disrupts,sosy).
local_semnet_1_GEN(strd,disrupts,sosy).
local_semnet_1_GEN(vita,disrupts,sosy).
local_semnet_1_GEN(aapp,disrupts,tisu).
local_semnet_1_GEN(carb,disrupts,tisu).
local_semnet_1_GEN(eico,disrupts,tisu).
local_semnet_1_GEN(elii,disrupts,tisu).
local_semnet_1_GEN(gngm,disrupts,tisu).
local_semnet_1_GEN(inch,disrupts,tisu).
local_semnet_1_GEN(lipd,disrupts,tisu).
local_semnet_1_GEN(nnon,disrupts,tisu).
local_semnet_1_GEN(nusq,disrupts,tisu).
local_semnet_1_GEN(opco,disrupts,tisu).
local_semnet_1_GEN(orch,disrupts,tisu).
local_semnet_1_GEN(strd,disrupts,tisu).
local_semnet_1_GEN(genf,disrupts,moft). % GR 06/2015
local_semnet_1_GEN(phsu,disrupts,fndg). % GR 06/2015
local_semnet_1_GEN(topp,disrupts,emst). % GR 06/2015
local_semnet_1_GEN(orch,disrupts,genf). % GR 06/02/2017 Source: GENIA 9764907_S9 Cepharanthine was found to suppress HIV-1 LTR-driven gene expression through the inhibition of NF-kappaB activation.
%local_semnet_1_GEN(phsu,disrupts,genf). % GR 06/02/2017 Source: GENIA 9764907_S9 to get predication cepharanthine-phsu,orch-disrupts-genf but it appears it's already in SN
local_semnet_1_GEN(dsyn,disrupts,celf). % GR 10/26/2017 Source: NEG PMID 17389712, line 1 Rat maternal diabetes impairs pancreatic beta-cell function in the offspring
local_semnet_1_GEN(dsyn,disrupts,cell). % GR 10/26/2017 Source: NEG PMID 17389712, line 1 Rat maternal diabetes impairs pancreatic beta-cell function in the offspring
% augments/has_augmentation
local_semnet_1_GEN(aapp,augments,acab).
local_semnet_1_GEN(antb,augments,acab).
local_semnet_1_GEN(bacs,augments,acab).
local_semnet_1_GEN(carb,augments,acab).
local_semnet_1_GEN(eico,augments,acab).
local_semnet_1_GEN(elii,augments,acab).
local_semnet_1_GEN(enzy,augments,acab).
local_semnet_1_GEN(gngm,augments,acab).
local_semnet_1_GEN(hops,augments,acab).
local_semnet_1_GEN(horm,augments,acab).
local_semnet_1_GEN(imft,augments,acab).
local_semnet_1_GEN(inch,augments,acab).
local_semnet_1_GEN(lipd,augments,acab).
local_semnet_1_GEN(nnon,augments,acab).
local_semnet_1_GEN(nsba,augments,acab).
local_semnet_1_GEN(nusq,augments,acab).
local_semnet_1_GEN(opco,augments,acab).
local_semnet_1_GEN(orch,augments,acab).
local_semnet_1_GEN(phsu,augments,acab).
local_semnet_1_GEN(rcpt,augments,acab).
local_semnet_1_GEN(strd,augments,acab).
local_semnet_1_GEN(vita,augments,acab).
local_semnet_1_GEN(aapp,augments,anab).
local_semnet_1_GEN(antb,augments,anab).
local_semnet_1_GEN(bacs,augments,anab).
local_semnet_1_GEN(carb,augments,anab).
local_semnet_1_GEN(eico,augments,anab).
local_semnet_1_GEN(elii,augments,anab).
local_semnet_1_GEN(enzy,augments,anab).
local_semnet_1_GEN(gngm,augments,anab).
local_semnet_1_GEN(hops,augments,anab).
local_semnet_1_GEN(horm,augments,anab).
local_semnet_1_GEN(imft,augments,anab).
local_semnet_1_GEN(inch,augments,anab).
local_semnet_1_GEN(lipd,augments,anab).
local_semnet_1_GEN(nnon,augments,anab).
local_semnet_1_GEN(nsba,augments,anab).
local_semnet_1_GEN(nusq,augments,anab).
local_semnet_1_GEN(opco,augments,anab).
local_semnet_1_GEN(orch,augments,anab).
local_semnet_1_GEN(phsu,augments,anab).
local_semnet_1_GEN(rcpt,augments,anab).
local_semnet_1_GEN(strd,augments,anab).
local_semnet_1_GEN(vita,augments,anab).
local_semnet_1_GEN(aapp,augments,anst).
local_semnet_1_GEN(carb,augments,anst).
local_semnet_1_GEN(eico,augments,anst).
local_semnet_1_GEN(elii,augments,anst).
local_semnet_1_GEN(gngm,augments,anst).
local_semnet_1_GEN(inch,augments,anst).
local_semnet_1_GEN(lipd,augments,anst).
local_semnet_1_GEN(nnon,augments,anst).
local_semnet_1_GEN(nusq,augments,anst).
local_semnet_1_GEN(opco,augments,anst).
local_semnet_1_GEN(orch,augments,anst).
local_semnet_1_GEN(phsu,augments,anst).
local_semnet_1_GEN(strd,augments,anst).
local_semnet_1_GEN(aapp,augments,biof).
local_semnet_1_GEN(carb,augments,biof).
local_semnet_1_GEN(eico,augments,biof).
local_semnet_1_GEN(elii,augments,biof).
local_semnet_1_GEN(gngm,augments,biof).
local_semnet_1_GEN(inch,augments,biof).
local_semnet_1_GEN(lipd,augments,biof).
local_semnet_1_GEN(nnon,augments,biof).
local_semnet_1_GEN(nusq,augments,biof).
local_semnet_1_GEN(opco,augments,biof).
local_semnet_1_GEN(orch,augments,biof).
local_semnet_1_GEN(phsu,augments,biof).
local_semnet_1_GEN(strd,augments,biof).
local_semnet_1_GEN(aapp,augments,bpoc).
local_semnet_1_GEN(carb,augments,bpoc).
local_semnet_1_GEN(eico,augments,bpoc).
local_semnet_1_GEN(elii,augments,bpoc).
local_semnet_1_GEN(gngm,augments,bpoc).
local_semnet_1_GEN(inch,augments,bpoc).
local_semnet_1_GEN(lipd,augments,bpoc).
local_semnet_1_GEN(nnon,augments,bpoc).
local_semnet_1_GEN(nusq,augments,bpoc).
local_semnet_1_GEN(opco,augments,bpoc).
local_semnet_1_GEN(orch,augments,bpoc).
local_semnet_1_GEN(phsu,augments,bpoc).
local_semnet_1_GEN(strd,augments,bpoc).
local_semnet_1_GEN(aapp,augments,celc).
local_semnet_1_GEN(carb,augments,celc).
local_semnet_1_GEN(eico,augments,celc).
local_semnet_1_GEN(elii,augments,celc).
local_semnet_1_GEN(gngm,augments,celc).
local_semnet_1_GEN(inch,augments,celc).
local_semnet_1_GEN(lipd,augments,celc).
local_semnet_1_GEN(nnon,augments,celc).
local_semnet_1_GEN(nusq,augments,celc).
local_semnet_1_GEN(opco,augments,celc).
local_semnet_1_GEN(orch,augments,celc).
local_semnet_1_GEN(phsu,augments,celc).
local_semnet_1_GEN(strd,augments,celc).
local_semnet_1_GEN(aapp,augments,celf).
local_semnet_1_GEN(carb,augments,celf).
local_semnet_1_GEN(eico,augments,celf).
local_semnet_1_GEN(elii,augments,celf).
local_semnet_1_GEN(gngm,augments,celf).
local_semnet_1_GEN(inch,augments,celf).
local_semnet_1_GEN(lipd,augments,celf).
local_semnet_1_GEN(nnon,augments,celf).
local_semnet_1_GEN(nusq,augments,celf).
local_semnet_1_GEN(opco,augments,celf).
local_semnet_1_GEN(orch,augments,celf).
local_semnet_1_GEN(phsu,augments,celf). % rcpt augments celf is added further down - 06/2015
local_semnet_1_GEN(strd,augments,celf).
local_semnet_1_GEN(aapp,augments,cell).
local_semnet_1_GEN(carb,augments,cell).
local_semnet_1_GEN(eico,augments,cell).
local_semnet_1_GEN(elii,augments,cell).
local_semnet_1_GEN(gngm,augments,cell).
local_semnet_1_GEN(inch,augments,cell).
local_semnet_1_GEN(lipd,augments,cell).
local_semnet_1_GEN(nnon,augments,cell).
local_semnet_1_GEN(nusq,augments,cell).
local_semnet_1_GEN(opco,augments,cell).
local_semnet_1_GEN(orch,augments,cell).
local_semnet_1_GEN(phsu,augments,cell).
local_semnet_1_GEN(strd,augments,cell).
local_semnet_1_GEN(aapp,augments,cgab).
local_semnet_1_GEN(antb,augments,cgab).
local_semnet_1_GEN(bacs,augments,cgab).
local_semnet_1_GEN(carb,augments,cgab).
local_semnet_1_GEN(eico,augments,cgab).
local_semnet_1_GEN(elii,augments,cgab).
local_semnet_1_GEN(enzy,augments,cgab).
local_semnet_1_GEN(gngm,augments,cgab).
local_semnet_1_GEN(hops,augments,cgab).
local_semnet_1_GEN(horm,augments,cgab).
local_semnet_1_GEN(imft,augments,cgab).
local_semnet_1_GEN(inch,augments,cgab).
local_semnet_1_GEN(lipd,augments,cgab).
local_semnet_1_GEN(nnon,augments,cgab).
local_semnet_1_GEN(nsba,augments,cgab).
local_semnet_1_GEN(nusq,augments,cgab).
local_semnet_1_GEN(opco,augments,cgab).
local_semnet_1_GEN(orch,augments,cgab).
local_semnet_1_GEN(phsu,augments,cgab).
local_semnet_1_GEN(rcpt,augments,cgab).
local_semnet_1_GEN(strd,augments,cgab).
local_semnet_1_GEN(vita,augments,cgab).
local_semnet_1_GEN(aapp,augments,comd).
local_semnet_1_GEN(antb,augments,comd).
local_semnet_1_GEN(bacs,augments,comd).
local_semnet_1_GEN(carb,augments,comd).
local_semnet_1_GEN(eico,augments,comd).
local_semnet_1_GEN(elii,augments,comd).
local_semnet_1_GEN(enzy,augments,comd).
local_semnet_1_GEN(gngm,augments,comd).
local_semnet_1_GEN(hops,augments,comd).
local_semnet_1_GEN(horm,augments,comd).
local_semnet_1_GEN(imft,augments,comd).
local_semnet_1_GEN(inch,augments,comd).
local_semnet_1_GEN(lipd,augments,comd).
local_semnet_1_GEN(nnon,augments,comd).
local_semnet_1_GEN(nsba,augments,comd).
local_semnet_1_GEN(nusq,augments,comd).
local_semnet_1_GEN(opco,augments,comd).
local_semnet_1_GEN(orch,augments,comd).
local_semnet_1_GEN(phsu,augments,comd).
local_semnet_1_GEN(rcpt,augments,comd).
local_semnet_1_GEN(strd,augments,comd).
local_semnet_1_GEN(vita,augments,comd).
local_semnet_1_GEN(aapp,augments,dsyn).
local_semnet_1_GEN(antb,augments,dsyn).
local_semnet_1_GEN(bacs,augments,dsyn).
local_semnet_1_GEN(carb,augments,dsyn).
local_semnet_1_GEN(eico,augments,dsyn).
local_semnet_1_GEN(elii,augments,dsyn).
local_semnet_1_GEN(enzy,augments,dsyn).
local_semnet_1_GEN(gngm,augments,dsyn).
local_semnet_1_GEN(hops,augments,dsyn).
local_semnet_1_GEN(horm,augments,dsyn).
local_semnet_1_GEN(imft,augments,dsyn).
local_semnet_1_GEN(inch,augments,dsyn).
local_semnet_1_GEN(lipd,augments,dsyn).
local_semnet_1_GEN(nnon,augments,dsyn).
local_semnet_1_GEN(nsba,augments,dsyn).
local_semnet_1_GEN(nusq,augments,dsyn).
local_semnet_1_GEN(opco,augments,dsyn).
local_semnet_1_GEN(orch,augments,dsyn).
local_semnet_1_GEN(phsu,augments,dsyn).
local_semnet_1_GEN(rcpt,augments,dsyn).
local_semnet_1_GEN(strd,augments,dsyn).
local_semnet_1_GEN(vita,augments,dsyn).
local_semnet_1_GEN(aapp,augments,emst).
local_semnet_1_GEN(carb,augments,emst).
local_semnet_1_GEN(eico,augments,emst).
local_semnet_1_GEN(elii,augments,emst).
local_semnet_1_GEN(gngm,augments,emst).
local_semnet_1_GEN(inch,augments,emst).
local_semnet_1_GEN(lipd,augments,emst).
local_semnet_1_GEN(nnon,augments,emst).
local_semnet_1_GEN(nusq,augments,emst).
local_semnet_1_GEN(opco,augments,emst).
local_semnet_1_GEN(orch,augments,emst).
local_semnet_1_GEN(phsu,augments,emst).
local_semnet_1_GEN(strd,augments,emst).
local_semnet_1_GEN(aapp,augments,ffas).
local_semnet_1_GEN(carb,augments,ffas).
local_semnet_1_GEN(eico,augments,ffas).
local_semnet_1_GEN(elii,augments,ffas).
local_semnet_1_GEN(gngm,augments,ffas).
local_semnet_1_GEN(inch,augments,ffas).
local_semnet_1_GEN(lipd,augments,ffas).
local_semnet_1_GEN(nnon,augments,ffas).
local_semnet_1_GEN(nusq,augments,ffas).
local_semnet_1_GEN(opco,augments,ffas).
local_semnet_1_GEN(orch,augments,ffas).
local_semnet_1_GEN(phsu,augments,ffas).
local_semnet_1_GEN(strd,augments,ffas).
local_semnet_1_GEN(aapp,augments,fndg).
local_semnet_1_GEN(carb,augments,fndg).
local_semnet_1_GEN(eico,augments,fndg).
local_semnet_1_GEN(elii,augments,fndg).
local_semnet_1_GEN(gngm,augments,fndg).
local_semnet_1_GEN(inch,augments,fndg).
local_semnet_1_GEN(lipd,augments,fndg).
local_semnet_1_GEN(nnon,augments,fndg).
local_semnet_1_GEN(nusq,augments,fndg).
local_semnet_1_GEN(opco,augments,fndg).
local_semnet_1_GEN(orch,augments,fndg).
local_semnet_1_GEN(phsu,augments,fndg).
local_semnet_1_GEN(strd,augments,fndg).
local_semnet_1_GEN(aapp,augments,inpo).
local_semnet_1_GEN(antb,augments,inpo).
local_semnet_1_GEN(bacs,augments,inpo).
local_semnet_1_GEN(carb,augments,inpo).
local_semnet_1_GEN(eico,augments,inpo).
local_semnet_1_GEN(elii,augments,inpo).
local_semnet_1_GEN(enzy,augments,inpo).
local_semnet_1_GEN(gngm,augments,inpo).
local_semnet_1_GEN(hops,augments,inpo).
local_semnet_1_GEN(horm,augments,inpo).
local_semnet_1_GEN(imft,augments,inpo).
local_semnet_1_GEN(inch,augments,inpo).
local_semnet_1_GEN(lipd,augments,inpo).
local_semnet_1_GEN(nnon,augments,inpo).
local_semnet_1_GEN(nsba,augments,inpo).
local_semnet_1_GEN(nusq,augments,inpo).
local_semnet_1_GEN(opco,augments,inpo).
local_semnet_1_GEN(orch,augments,inpo).
local_semnet_1_GEN(phsu,augments,inpo).
local_semnet_1_GEN(rcpt,augments,inpo).
local_semnet_1_GEN(strd,augments,inpo).
local_semnet_1_GEN(vita,augments,inpo).
local_semnet_1_GEN(aapp,augments,menp).
local_semnet_1_GEN(carb,augments,menp).
local_semnet_1_GEN(eico,augments,menp).
local_semnet_1_GEN(elii,augments,menp).
local_semnet_1_GEN(gngm,augments,menp).
%local_semnet_1_GEN(inch,augments,menp).
local_semnet_1_GEN(lipd,augments,menp).
local_semnet_1_GEN(nnon,augments,menp).
local_semnet_1_GEN(nusq,augments,menp).
local_semnet_1_GEN(opco,augments,menp).
%local_semnet_1_GEN(orch,augments,menp).
%local_semnet_1_GEN(phsu,augments,menp).
local_semnet_1_GEN(strd,augments,menp).
local_semnet_1_GEN(aapp,augments,moft).
local_semnet_1_GEN(antb,augments,moft).
local_semnet_1_GEN(bacs,augments,moft).
local_semnet_1_GEN(carb,augments,moft).
local_semnet_1_GEN(enzy,augments,moft).
local_semnet_1_GEN(gngm,augments,moft).
local_semnet_1_GEN(hops,augments,moft).
local_semnet_1_GEN(horm,augments,moft).
local_semnet_1_GEN(imft,augments,moft).
local_semnet_1_GEN(nsba,augments,moft).
local_semnet_1_GEN(nusq,augments,moft).
local_semnet_1_GEN(opco,augments,moft).
local_semnet_1_GEN(phsu,augments,moft).
local_semnet_1_GEN(rcpt,augments,moft).
local_semnet_1_GEN(strd,augments,moft).
local_semnet_1_GEN(vita,augments,moft).
local_semnet_1_GEN(aapp,augments,neop).
local_semnet_1_GEN(antb,augments,neop).
local_semnet_1_GEN(bacs,augments,neop).
local_semnet_1_GEN(carb,augments,neop).
local_semnet_1_GEN(eico,augments,neop).
local_semnet_1_GEN(elii,augments,neop).
local_semnet_1_GEN(enzy,augments,neop).
local_semnet_1_GEN(gngm,augments,neop).
local_semnet_1_GEN(hops,augments,neop).
local_semnet_1_GEN(horm,augments,neop).
local_semnet_1_GEN(imft,augments,neop).
local_semnet_1_GEN(inch,augments,neop).
local_semnet_1_GEN(lipd,augments,neop).
local_semnet_1_GEN(nnon,augments,neop).
local_semnet_1_GEN(nsba,augments,neop).
local_semnet_1_GEN(nusq,augments,neop).
local_semnet_1_GEN(opco,augments,neop).
local_semnet_1_GEN(orch,augments,neop).
local_semnet_1_GEN(phsu,augments,neop).
local_semnet_1_GEN(rcpt,augments,neop).
local_semnet_1_GEN(strd,augments,neop).
local_semnet_1_GEN(vita,augments,neop).
local_semnet_1_GEN(aapp,augments,npop).
local_semnet_1_GEN(antb,augments,npop).
local_semnet_1_GEN(bacs,augments,npop).
local_semnet_1_GEN(carb,augments,npop).
local_semnet_1_GEN(eico,augments,npop).
local_semnet_1_GEN(elii,augments,npop).
local_semnet_1_GEN(enzy,augments,npop).
local_semnet_1_GEN(gngm,augments,npop).
local_semnet_1_GEN(hops,augments,npop).
local_semnet_1_GEN(horm,augments,npop).
local_semnet_1_GEN(imft,augments,npop).
local_semnet_1_GEN(inch,augments,npop).
local_semnet_1_GEN(lipd,augments,npop).
local_semnet_1_GEN(nnon,augments,npop).
local_semnet_1_GEN(nsba,augments,npop).
local_semnet_1_GEN(nusq,augments,npop).
local_semnet_1_GEN(opco,augments,npop).
local_semnet_1_GEN(orch,augments,npop).
local_semnet_1_GEN(phsu,augments,npop).
local_semnet_1_GEN(rcpt,augments,npop).
local_semnet_1_GEN(strd,augments,npop).
local_semnet_1_GEN(vita,augments,npop).
local_semnet_1_GEN(aapp,augments,orgf).
local_semnet_1_GEN(carb,augments,orgf).
local_semnet_1_GEN(eico,augments,orgf).
local_semnet_1_GEN(elii,augments,orgf).
local_semnet_1_GEN(gngm,augments,orgf).
local_semnet_1_GEN(inch,augments,orgf).
local_semnet_1_GEN(lipd,augments,orgf).
local_semnet_1_GEN(nnon,augments,orgf).
local_semnet_1_GEN(nusq,augments,orgf).
local_semnet_1_GEN(opco,augments,orgf).
local_semnet_1_GEN(orch,augments,orgf).
local_semnet_1_GEN(phsu,augments,orgf).
local_semnet_1_GEN(strd,augments,orgf).
local_semnet_1_GEN(aapp,augments,patf).
local_semnet_1_GEN(antb,augments,patf).
local_semnet_1_GEN(bacs,augments,patf).
local_semnet_1_GEN(carb,augments,patf).
local_semnet_1_GEN(eico,augments,patf).
local_semnet_1_GEN(elii,augments,patf).
local_semnet_1_GEN(enzy,augments,patf).
local_semnet_1_GEN(gngm,augments,patf).
local_semnet_1_GEN(hops,augments,patf).
local_semnet_1_GEN(horm,augments,patf).
local_semnet_1_GEN(imft,augments,patf).
local_semnet_1_GEN(inch,augments,patf).
local_semnet_1_GEN(lipd,augments,patf).
local_semnet_1_GEN(nnon,augments,patf).
local_semnet_1_GEN(nsba,augments,patf).
local_semnet_1_GEN(nusq,augments,patf).
local_semnet_1_GEN(opco,augments,patf).
local_semnet_1_GEN(orch,augments,patf).
local_semnet_1_GEN(phsu,augments,patf).
local_semnet_1_GEN(rcpt,augments,patf).
local_semnet_1_GEN(strd,augments,patf).
local_semnet_1_GEN(vita,augments,patf).
local_semnet_1_GEN(aapp,augments,phsf).
local_semnet_1_GEN(carb,augments,phsf).
local_semnet_1_GEN(eico,augments,phsf).
local_semnet_1_GEN(elii,augments,phsf).
local_semnet_1_GEN(gngm,augments,phsf).
local_semnet_1_GEN(inch,augments,phsf).
local_semnet_1_GEN(lipd,augments,phsf).
local_semnet_1_GEN(nnon,augments,phsf).
local_semnet_1_GEN(nusq,augments,phsf).
local_semnet_1_GEN(opco,augments,phsf).
local_semnet_1_GEN(orch,augments,phsf).
local_semnet_1_GEN(phsu,augments,phsf).
local_semnet_1_GEN(strd,augments,phsf).
local_semnet_1_GEN(aapp,augments,sosy).
local_semnet_1_GEN(antb,augments,sosy).
local_semnet_1_GEN(bacs,augments,sosy).
local_semnet_1_GEN(carb,augments,sosy).
local_semnet_1_GEN(eico,augments,sosy).
local_semnet_1_GEN(elii,augments,sosy).
local_semnet_1_GEN(enzy,augments,sosy).
local_semnet_1_GEN(gngm,augments,sosy).
local_semnet_1_GEN(hops,augments,sosy).
local_semnet_1_GEN(horm,augments,sosy).
local_semnet_1_GEN(imft,augments,sosy).
local_semnet_1_GEN(inch,augments,sosy).
local_semnet_1_GEN(lipd,augments,sosy).
local_semnet_1_GEN(nnon,augments,sosy).
local_semnet_1_GEN(nsba,augments,sosy).
local_semnet_1_GEN(nusq,augments,sosy).
local_semnet_1_GEN(opco,augments,sosy).
local_semnet_1_GEN(orch,augments,sosy).
local_semnet_1_GEN(phsu,augments,sosy).
local_semnet_1_GEN(rcpt,augments,sosy).
local_semnet_1_GEN(strd,augments,sosy).
local_semnet_1_GEN(vita,augments,sosy).
local_semnet_1_GEN(aapp,augments,tisu).
local_semnet_1_GEN(carb,augments,tisu).
local_semnet_1_GEN(eico,augments,tisu).
local_semnet_1_GEN(elii,augments,tisu).
local_semnet_1_GEN(gngm,augments,tisu).
local_semnet_1_GEN(inch,augments,tisu).
local_semnet_1_GEN(lipd,augments,tisu).
local_semnet_1_GEN(nnon,augments,tisu).
local_semnet_1_GEN(nusq,augments,tisu).
local_semnet_1_GEN(opco,augments,tisu).
local_semnet_1_GEN(orch,augments,tisu).
local_semnet_1_GEN(phsu,augments,tisu).
local_semnet_1_GEN(strd,augments,tisu).
% affects aug FOR moft
local_semnet_1_GEN(strd,affects,moft).
local_semnet_1_GEN(aapp,affects,moft).
%local_semnet_1_GEN(opco,affects,moft). % GR commented out 08/2015
local_semnet_1_GEN(nusq,affects,moft).
%local_semnet_1_GEN(carb,affects,moft). % GR commented out 08/2015
local_semnet_1_GEN(gngm,affects,moft).
local_semnet_1_GEN(phsu,affects,moft).
%local_semnet_1_GEN(antb,affects,moft). % GR commented out 08/2015
local_semnet_1_GEN(bacs,affects,moft).
%local_semnet_1_GEN(imft,affects,moft). % GR commented out 08/2015
local_semnet_1_GEN(vita,affects,moft).
local_semnet_1_GEN(enzy,affects,moft).
local_semnet_1_GEN(horm,affects,moft).
local_semnet_1_GEN(rcpt,affects,moft).
local_semnet_1_GEN(nsba,affects,moft).
%local_semnet_1_GEN(hops,affects,moft). % GR commented out 08/2015
% LOCATION_OF (anatomical Structures)
local_semnet_1_GEN(anst,location_of,carb).
local_semnet_1_GEN(anst,location_of,hops).
local_semnet_1_GEN(anst,location_of,inch).
local_semnet_1_GEN(anst,location_of,orch).
local_semnet_1_GEN(anst,location_of,elii).
local_semnet_1_GEN(anst,location_of,lipd).
local_semnet_1_GEN(anst,location_of,eico).
local_semnet_1_GEN(anst,location_of,strd).
local_semnet_1_GEN(anst,location_of,aapp).
local_semnet_1_GEN(anst,location_of,opco).
local_semnet_1_GEN(anst,location_of,nusq).
local_semnet_1_GEN(emst,location_of,carb).
local_semnet_1_GEN(emst,location_of,hops).
local_semnet_1_GEN(emst,location_of,inch).
local_semnet_1_GEN(emst,location_of,orch).
local_semnet_1_GEN(emst,location_of,elii).
local_semnet_1_GEN(emst,location_of,lipd).
local_semnet_1_GEN(emst,location_of,eico).
local_semnet_1_GEN(emst,location_of,strd).
local_semnet_1_GEN(emst,location_of,aapp).
local_semnet_1_GEN(emst,location_of,opco).
local_semnet_1_GEN(emst,location_of,nusq).
local_semnet_1_GEN(ffas,location_of,carb).
local_semnet_1_GEN(ffas,location_of,hops).
local_semnet_1_GEN(ffas,location_of,inch).
local_semnet_1_GEN(ffas,location_of,orch).
local_semnet_1_GEN(ffas,location_of,elii).
local_semnet_1_GEN(ffas,location_of,lipd).
local_semnet_1_GEN(ffas,location_of,eico).
local_semnet_1_GEN(ffas,location_of,strd).
local_semnet_1_GEN(ffas,location_of,aapp).
local_semnet_1_GEN(ffas,location_of,opco).
local_semnet_1_GEN(ffas,location_of,nusq).
local_semnet_1_GEN(bpoc,location_of,carb).
local_semnet_1_GEN(bpoc,location_of,hops).
local_semnet_1_GEN(bpoc,location_of,inch).
local_semnet_1_GEN(bpoc,location_of,orch).
local_semnet_1_GEN(bpoc,location_of,elii).
local_semnet_1_GEN(bpoc,location_of,lipd).
local_semnet_1_GEN(bpoc,location_of,eico).
local_semnet_1_GEN(bpoc,location_of,strd).
local_semnet_1_GEN(bpoc,location_of,aapp).
local_semnet_1_GEN(bpoc,location_of,opco).
local_semnet_1_GEN(bpoc,location_of,nusq).
local_semnet_1_GEN(tisu,location_of,carb).
local_semnet_1_GEN(tisu,location_of,hops).
local_semnet_1_GEN(tisu,location_of,inch).
local_semnet_1_GEN(tisu,location_of,orch).
local_semnet_1_GEN(tisu,location_of,elii).
local_semnet_1_GEN(tisu,location_of,lipd).
local_semnet_1_GEN(tisu,location_of,eico).
local_semnet_1_GEN(tisu,location_of,strd).
local_semnet_1_GEN(tisu,location_of,aapp).
local_semnet_1_GEN(tisu,location_of,opco).
local_semnet_1_GEN(tisu,location_of,nusq).
local_semnet_1_GEN(cell,location_of,carb).
local_semnet_1_GEN(cell,location_of,hops).
local_semnet_1_GEN(cell,location_of,inch).
local_semnet_1_GEN(cell,location_of,orch).
local_semnet_1_GEN(cell,location_of,elii).
local_semnet_1_GEN(cell,location_of,lipd).
local_semnet_1_GEN(cell,location_of,eico).
local_semnet_1_GEN(cell,location_of,strd).
local_semnet_1_GEN(cell,location_of,aapp).
local_semnet_1_GEN(cell,location_of,opco).
local_semnet_1_GEN(cell,location_of,nusq).
local_semnet_1_GEN(celc,location_of,carb).
local_semnet_1_GEN(celc,location_of,hops).
local_semnet_1_GEN(celc,location_of,inch).
local_semnet_1_GEN(celc,location_of,orch).
local_semnet_1_GEN(celc,location_of,elii).
local_semnet_1_GEN(celc,location_of,lipd).
local_semnet_1_GEN(celc,location_of,eico).
local_semnet_1_GEN(celc,location_of,strd).
local_semnet_1_GEN(celc,location_of,aapp).
local_semnet_1_GEN(celc,location_of,opco).
local_semnet_1_GEN(celc,location_of,nusq).
% COEXISTS_WITH
local_semnet_1_GEN(gngm,coexists_with,gngm).
local_semnet_1_GEN(gngm,coexists_with,phsu).
local_semnet_1_GEN(gngm,coexists_with,antb).
local_semnet_1_GEN(gngm,coexists_with,bacs).
local_semnet_1_GEN(gngm,coexists_with,imft).
local_semnet_1_GEN(gngm,coexists_with,vita).
local_semnet_1_GEN(gngm,coexists_with,enzy).
local_semnet_1_GEN(gngm,coexists_with,horm).
local_semnet_1_GEN(gngm,coexists_with,rcpt).
local_semnet_1_GEN(gngm,coexists_with,nsba).
local_semnet_1_GEN(gngm,coexists_with,hops).
local_semnet_1_GEN(gngm,coexists_with,inch).
local_semnet_1_GEN(gngm,coexists_with,orch).
local_semnet_1_GEN(gngm,coexists_with,lipd).
local_semnet_1_GEN(gngm,coexists_with,eico).
local_semnet_1_GEN(gngm,coexists_with,strd).
local_semnet_1_GEN(gngm,coexists_with,elii).
local_semnet_1_GEN(gngm,coexists_with,opco).
local_semnet_1_GEN(gngm,coexists_with,nusq).
local_semnet_1_GEN(gngm,coexists_with,carb).
local_semnet_1_GEN(phsu,coexists_with,gngm).
local_semnet_1_GEN(phsu,coexists_with,phsu).
local_semnet_1_GEN(phsu,coexists_with,antb).
local_semnet_1_GEN(phsu,coexists_with,bacs).
local_semnet_1_GEN(phsu,coexists_with,imft).
local_semnet_1_GEN(phsu,coexists_with,vita).
local_semnet_1_GEN(phsu,coexists_with,enzy).
local_semnet_1_GEN(phsu,coexists_with,horm).
local_semnet_1_GEN(phsu,coexists_with,rcpt).
local_semnet_1_GEN(phsu,coexists_with,nsba).
local_semnet_1_GEN(phsu,coexists_with,hops).
local_semnet_1_GEN(phsu,coexists_with,inch).
local_semnet_1_GEN(phsu,coexists_with,orch).
local_semnet_1_GEN(phsu,coexists_with,lipd).
local_semnet_1_GEN(phsu,coexists_with,eico).
local_semnet_1_GEN(phsu,coexists_with,strd).
local_semnet_1_GEN(phsu,coexists_with,elii).
local_semnet_1_GEN(phsu,coexists_with,opco).
local_semnet_1_GEN(phsu,coexists_with,nusq).
local_semnet_1_GEN(phsu,coexists_with,carb).
local_semnet_1_GEN(antb,coexists_with,gngm).
local_semnet_1_GEN(antb,coexists_with,phsu).
local_semnet_1_GEN(antb,coexists_with,antb).
local_semnet_1_GEN(antb,coexists_with,bacs).
local_semnet_1_GEN(antb,coexists_with,imft).
local_semnet_1_GEN(antb,coexists_with,vita).
local_semnet_1_GEN(antb,coexists_with,enzy).
local_semnet_1_GEN(antb,coexists_with,horm).
local_semnet_1_GEN(antb,coexists_with,rcpt).
local_semnet_1_GEN(antb,coexists_with,nsba).
local_semnet_1_GEN(antb,coexists_with,hops).
local_semnet_1_GEN(antb,coexists_with,inch).
local_semnet_1_GEN(antb,coexists_with,orch).
local_semnet_1_GEN(antb,coexists_with,lipd).
local_semnet_1_GEN(antb,coexists_with,eico).
local_semnet_1_GEN(antb,coexists_with,strd).
local_semnet_1_GEN(antb,coexists_with,elii).
local_semnet_1_GEN(antb,coexists_with,opco).
local_semnet_1_GEN(antb,coexists_with,nusq).
local_semnet_1_GEN(antb,coexists_with,carb).
local_semnet_1_GEN(bacs,coexists_with,gngm).
local_semnet_1_GEN(bacs,coexists_with,phsu).
local_semnet_1_GEN(bacs,coexists_with,antb).
local_semnet_1_GEN(bacs,coexists_with,bacs).
local_semnet_1_GEN(bacs,coexists_with,imft).
local_semnet_1_GEN(bacs,coexists_with,vita).
local_semnet_1_GEN(bacs,coexists_with,enzy).
local_semnet_1_GEN(bacs,coexists_with,horm).
local_semnet_1_GEN(bacs,coexists_with,rcpt).
local_semnet_1_GEN(bacs,coexists_with,nsba).
local_semnet_1_GEN(bacs,coexists_with,hops).
local_semnet_1_GEN(bacs,coexists_with,inch).
local_semnet_1_GEN(bacs,coexists_with,orch).
local_semnet_1_GEN(bacs,coexists_with,lipd).
local_semnet_1_GEN(bacs,coexists_with,eico).
local_semnet_1_GEN(bacs,coexists_with,strd).
local_semnet_1_GEN(bacs,coexists_with,elii).
local_semnet_1_GEN(bacs,coexists_with,opco).
local_semnet_1_GEN(bacs,coexists_with,nusq).
local_semnet_1_GEN(bacs,coexists_with,carb).
local_semnet_1_GEN(imft,coexists_with,gngm).
local_semnet_1_GEN(imft,coexists_with,phsu).
local_semnet_1_GEN(imft,coexists_with,antb).
local_semnet_1_GEN(imft,coexists_with,bacs).
local_semnet_1_GEN(imft,coexists_with,imft).
local_semnet_1_GEN(imft,coexists_with,vita).
local_semnet_1_GEN(imft,coexists_with,enzy).
local_semnet_1_GEN(imft,coexists_with,horm).
local_semnet_1_GEN(imft,coexists_with,rcpt).
local_semnet_1_GEN(imft,coexists_with,nsba).
local_semnet_1_GEN(imft,coexists_with,hops).
local_semnet_1_GEN(imft,coexists_with,inch).
local_semnet_1_GEN(imft,coexists_with,orch).
local_semnet_1_GEN(imft,coexists_with,lipd).
local_semnet_1_GEN(imft,coexists_with,eico).
local_semnet_1_GEN(imft,coexists_with,strd).
local_semnet_1_GEN(imft,coexists_with,elii).
local_semnet_1_GEN(imft,coexists_with,opco).
local_semnet_1_GEN(imft,coexists_with,nusq).
local_semnet_1_GEN(imft,coexists_with,carb).
local_semnet_1_GEN(vita,coexists_with,gngm).
local_semnet_1_GEN(vita,coexists_with,phsu).
local_semnet_1_GEN(vita,coexists_with,antb).
local_semnet_1_GEN(vita,coexists_with,bacs).
local_semnet_1_GEN(vita,coexists_with,imft).
local_semnet_1_GEN(vita,coexists_with,vita).
local_semnet_1_GEN(vita,coexists_with,enzy).
local_semnet_1_GEN(vita,coexists_with,horm).
local_semnet_1_GEN(vita,coexists_with,rcpt).
local_semnet_1_GEN(vita,coexists_with,nsba).
local_semnet_1_GEN(vita,coexists_with,hops).
local_semnet_1_GEN(vita,coexists_with,inch).
local_semnet_1_GEN(vita,coexists_with,orch).
local_semnet_1_GEN(vita,coexists_with,lipd).
local_semnet_1_GEN(vita,coexists_with,eico).
local_semnet_1_GEN(vita,coexists_with,strd).
local_semnet_1_GEN(vita,coexists_with,elii).
local_semnet_1_GEN(vita,coexists_with,opco).
local_semnet_1_GEN(vita,coexists_with,nusq).
local_semnet_1_GEN(vita,coexists_with,carb).
local_semnet_1_GEN(enzy,coexists_with,gngm).
local_semnet_1_GEN(enzy,coexists_with,phsu).
local_semnet_1_GEN(enzy,coexists_with,antb).
local_semnet_1_GEN(enzy,coexists_with,bacs).
local_semnet_1_GEN(enzy,coexists_with,imft).
local_semnet_1_GEN(enzy,coexists_with,vita).
local_semnet_1_GEN(enzy,coexists_with,enzy).
local_semnet_1_GEN(enzy,coexists_with,horm).
local_semnet_1_GEN(enzy,coexists_with,rcpt).
local_semnet_1_GEN(enzy,coexists_with,nsba).
local_semnet_1_GEN(enzy,coexists_with,hops).
local_semnet_1_GEN(enzy,coexists_with,inch).
local_semnet_1_GEN(enzy,coexists_with,orch).
local_semnet_1_GEN(enzy,coexists_with,lipd).
local_semnet_1_GEN(enzy,coexists_with,eico).
local_semnet_1_GEN(enzy,coexists_with,strd).
local_semnet_1_GEN(enzy,coexists_with,elii).
local_semnet_1_GEN(enzy,coexists_with,opco).
local_semnet_1_GEN(enzy,coexists_with,nusq).
local_semnet_1_GEN(enzy,coexists_with,carb).
local_semnet_1_GEN(horm,coexists_with,gngm).
local_semnet_1_GEN(horm,coexists_with,phsu).
local_semnet_1_GEN(horm,coexists_with,antb).
local_semnet_1_GEN(horm,coexists_with,bacs).
local_semnet_1_GEN(horm,coexists_with,imft).
local_semnet_1_GEN(horm,coexists_with,vita).
local_semnet_1_GEN(horm,coexists_with,enzy).
local_semnet_1_GEN(horm,coexists_with,horm).
local_semnet_1_GEN(horm,coexists_with,rcpt).
local_semnet_1_GEN(horm,coexists_with,nsba).
local_semnet_1_GEN(horm,coexists_with,hops).
local_semnet_1_GEN(horm,coexists_with,inch).
local_semnet_1_GEN(horm,coexists_with,orch).
local_semnet_1_GEN(horm,coexists_with,lipd).
local_semnet_1_GEN(horm,coexists_with,eico).
local_semnet_1_GEN(horm,coexists_with,strd).
local_semnet_1_GEN(horm,coexists_with,elii).
local_semnet_1_GEN(horm,coexists_with,opco).
local_semnet_1_GEN(horm,coexists_with,nusq).
local_semnet_1_GEN(horm,coexists_with,carb).
local_semnet_1_GEN(rcpt,coexists_with,gngm).
local_semnet_1_GEN(rcpt,coexists_with,phsu).
local_semnet_1_GEN(rcpt,coexists_with,antb).
local_semnet_1_GEN(rcpt,coexists_with,bacs).
local_semnet_1_GEN(rcpt,coexists_with,imft).
local_semnet_1_GEN(rcpt,coexists_with,vita).
local_semnet_1_GEN(rcpt,coexists_with,enzy).
local_semnet_1_GEN(rcpt,coexists_with,horm).
local_semnet_1_GEN(rcpt,coexists_with,rcpt).
local_semnet_1_GEN(rcpt,coexists_with,nsba).
local_semnet_1_GEN(rcpt,coexists_with,hops).
local_semnet_1_GEN(rcpt,coexists_with,inch).
local_semnet_1_GEN(rcpt,coexists_with,orch).
local_semnet_1_GEN(rcpt,coexists_with,lipd).
local_semnet_1_GEN(rcpt,coexists_with,eico).
local_semnet_1_GEN(rcpt,coexists_with,strd).
local_semnet_1_GEN(rcpt,coexists_with,elii).
local_semnet_1_GEN(rcpt,coexists_with,opco).
local_semnet_1_GEN(rcpt,coexists_with,nusq).
local_semnet_1_GEN(rcpt,coexists_with,carb).
local_semnet_1_GEN(nsba,coexists_with,gngm).
local_semnet_1_GEN(nsba,coexists_with,phsu).
local_semnet_1_GEN(nsba,coexists_with,antb).
local_semnet_1_GEN(nsba,coexists_with,bacs).
local_semnet_1_GEN(nsba,coexists_with,imft).
local_semnet_1_GEN(nsba,coexists_with,vita).
local_semnet_1_GEN(nsba,coexists_with,enzy).
local_semnet_1_GEN(nsba,coexists_with,horm).
local_semnet_1_GEN(nsba,coexists_with,rcpt).
local_semnet_1_GEN(nsba,coexists_with,nsba).
local_semnet_1_GEN(nsba,coexists_with,hops).
local_semnet_1_GEN(nsba,coexists_with,inch).
local_semnet_1_GEN(nsba,coexists_with,orch).
local_semnet_1_GEN(nsba,coexists_with,lipd).
local_semnet_1_GEN(nsba,coexists_with,eico).
local_semnet_1_GEN(nsba,coexists_with,strd).
local_semnet_1_GEN(nsba,coexists_with,elii).
local_semnet_1_GEN(nsba,coexists_with,opco).
local_semnet_1_GEN(nsba,coexists_with,nusq).
local_semnet_1_GEN(nsba,coexists_with,carb).
local_semnet_1_GEN(hops,coexists_with,gngm).
local_semnet_1_GEN(hops,coexists_with,phsu).
local_semnet_1_GEN(hops,coexists_with,antb).
local_semnet_1_GEN(hops,coexists_with,bacs).
local_semnet_1_GEN(hops,coexists_with,imft).
local_semnet_1_GEN(hops,coexists_with,vita).
local_semnet_1_GEN(hops,coexists_with,enzy).
local_semnet_1_GEN(hops,coexists_with,horm).
local_semnet_1_GEN(hops,coexists_with,rcpt).
local_semnet_1_GEN(hops,coexists_with,nsba).
local_semnet_1_GEN(hops,coexists_with,hops).
local_semnet_1_GEN(hops,coexists_with,inch).
local_semnet_1_GEN(hops,coexists_with,orch).
local_semnet_1_GEN(hops,coexists_with,lipd).
local_semnet_1_GEN(hops,coexists_with,eico).
local_semnet_1_GEN(hops,coexists_with,strd).
local_semnet_1_GEN(hops,coexists_with,elii).
local_semnet_1_GEN(hops,coexists_with,opco).
local_semnet_1_GEN(hops,coexists_with,nusq).
local_semnet_1_GEN(hops,coexists_with,carb).
local_semnet_1_GEN(inch,coexists_with,gngm).
local_semnet_1_GEN(inch,coexists_with,phsu).
local_semnet_1_GEN(inch,coexists_with,antb).
local_semnet_1_GEN(inch,coexists_with,bacs).
local_semnet_1_GEN(inch,coexists_with,imft).
local_semnet_1_GEN(inch,coexists_with,vita).
local_semnet_1_GEN(inch,coexists_with,enzy).
local_semnet_1_GEN(inch,coexists_with,horm).
local_semnet_1_GEN(inch,coexists_with,rcpt).
local_semnet_1_GEN(inch,coexists_with,nsba).
local_semnet_1_GEN(inch,coexists_with,hops).
local_semnet_1_GEN(inch,coexists_with,inch).
local_semnet_1_GEN(inch,coexists_with,orch).
local_semnet_1_GEN(inch,coexists_with,lipd).
local_semnet_1_GEN(inch,coexists_with,eico).
local_semnet_1_GEN(inch,coexists_with,strd).
local_semnet_1_GEN(inch,coexists_with,elii).
local_semnet_1_GEN(inch,coexists_with,opco).
local_semnet_1_GEN(inch,coexists_with,nusq).
local_semnet_1_GEN(inch,coexists_with,carb).
local_semnet_1_GEN(orch,coexists_with,gngm).
local_semnet_1_GEN(orch,coexists_with,phsu).
local_semnet_1_GEN(orch,coexists_with,antb).
local_semnet_1_GEN(orch,coexists_with,bacs).
local_semnet_1_GEN(orch,coexists_with,imft).
local_semnet_1_GEN(orch,coexists_with,vita).
local_semnet_1_GEN(orch,coexists_with,enzy).
local_semnet_1_GEN(orch,coexists_with,horm).
local_semnet_1_GEN(orch,coexists_with,rcpt).
local_semnet_1_GEN(orch,coexists_with,nsba).
local_semnet_1_GEN(orch,coexists_with,hops).
local_semnet_1_GEN(orch,coexists_with,inch).
local_semnet_1_GEN(orch,coexists_with,orch).
local_semnet_1_GEN(orch,coexists_with,lipd).
local_semnet_1_GEN(orch,coexists_with,eico).
local_semnet_1_GEN(orch,coexists_with,strd).
local_semnet_1_GEN(orch,coexists_with,elii).
local_semnet_1_GEN(orch,coexists_with,opco).
local_semnet_1_GEN(orch,coexists_with,nusq).
local_semnet_1_GEN(orch,coexists_with,carb).
local_semnet_1_GEN(lipd,coexists_with,gngm).
local_semnet_1_GEN(lipd,coexists_with,phsu).
local_semnet_1_GEN(lipd,coexists_with,antb).
local_semnet_1_GEN(lipd,coexists_with,bacs).
local_semnet_1_GEN(lipd,coexists_with,imft).
local_semnet_1_GEN(lipd,coexists_with,vita).
local_semnet_1_GEN(lipd,coexists_with,enzy).
local_semnet_1_GEN(lipd,coexists_with,horm).
local_semnet_1_GEN(lipd,coexists_with,rcpt).
local_semnet_1_GEN(lipd,coexists_with,nsba).
local_semnet_1_GEN(lipd,coexists_with,hops).
local_semnet_1_GEN(lipd,coexists_with,inch).
local_semnet_1_GEN(lipd,coexists_with,orch).
local_semnet_1_GEN(lipd,coexists_with,lipd).
local_semnet_1_GEN(lipd,coexists_with,eico).
local_semnet_1_GEN(lipd,coexists_with,strd).
local_semnet_1_GEN(lipd,coexists_with,elii).
local_semnet_1_GEN(lipd,coexists_with,opco).
local_semnet_1_GEN(lipd,coexists_with,nusq).
local_semnet_1_GEN(lipd,coexists_with,carb).
local_semnet_1_GEN(eico,coexists_with,gngm).
local_semnet_1_GEN(eico,coexists_with,phsu).
local_semnet_1_GEN(eico,coexists_with,antb).
local_semnet_1_GEN(eico,coexists_with,bacs).
local_semnet_1_GEN(eico,coexists_with,imft).
local_semnet_1_GEN(eico,coexists_with,vita).
local_semnet_1_GEN(eico,coexists_with,enzy).
local_semnet_1_GEN(eico,coexists_with,horm).
local_semnet_1_GEN(eico,coexists_with,rcpt).
local_semnet_1_GEN(eico,coexists_with,nsba).
local_semnet_1_GEN(eico,coexists_with,hops).
local_semnet_1_GEN(eico,coexists_with,inch).
local_semnet_1_GEN(eico,coexists_with,orch).
local_semnet_1_GEN(eico,coexists_with,lipd).
local_semnet_1_GEN(eico,coexists_with,eico).
local_semnet_1_GEN(eico,coexists_with,strd).
local_semnet_1_GEN(eico,coexists_with,elii).
local_semnet_1_GEN(eico,coexists_with,opco).
local_semnet_1_GEN(eico,coexists_with,nusq).
local_semnet_1_GEN(eico,coexists_with,carb).
local_semnet_1_GEN(strd,coexists_with,gngm).
local_semnet_1_GEN(strd,coexists_with,phsu).
local_semnet_1_GEN(strd,coexists_with,antb).
local_semnet_1_GEN(strd,coexists_with,bacs).
local_semnet_1_GEN(strd,coexists_with,imft).
local_semnet_1_GEN(strd,coexists_with,vita).
local_semnet_1_GEN(strd,coexists_with,enzy).
local_semnet_1_GEN(strd,coexists_with,horm).
local_semnet_1_GEN(strd,coexists_with,rcpt).
local_semnet_1_GEN(strd,coexists_with,nsba).
local_semnet_1_GEN(strd,coexists_with,hops).
local_semnet_1_GEN(strd,coexists_with,inch).
local_semnet_1_GEN(strd,coexists_with,orch).
local_semnet_1_GEN(strd,coexists_with,lipd).
local_semnet_1_GEN(strd,coexists_with,eico).
local_semnet_1_GEN(strd,coexists_with,strd).
local_semnet_1_GEN(strd,coexists_with,elii).
local_semnet_1_GEN(strd,coexists_with,opco).
local_semnet_1_GEN(strd,coexists_with,nusq).
local_semnet_1_GEN(strd,coexists_with,carb).
local_semnet_1_GEN(aapp,coexists_with,gngm).
local_semnet_1_GEN(aapp,coexists_with,phsu).
local_semnet_1_GEN(aapp,coexists_with,antb).
local_semnet_1_GEN(aapp,coexists_with,bacs).
local_semnet_1_GEN(aapp,coexists_with,imft).
local_semnet_1_GEN(aapp,coexists_with,vita).
local_semnet_1_GEN(aapp,coexists_with,enzy).
local_semnet_1_GEN(aapp,coexists_with,horm).
local_semnet_1_GEN(aapp,coexists_with,rcpt).
local_semnet_1_GEN(aapp,coexists_with,nsba).
local_semnet_1_GEN(aapp,coexists_with,hops).
local_semnet_1_GEN(aapp,coexists_with,inch).
local_semnet_1_GEN(aapp,coexists_with,orch).
local_semnet_1_GEN(aapp,coexists_with,lipd).
local_semnet_1_GEN(aapp,coexists_with,eico).
local_semnet_1_GEN(aapp,coexists_with,strd).
local_semnet_1_GEN(aapp,coexists_with,elii).
local_semnet_1_GEN(aapp,coexists_with,opco).
local_semnet_1_GEN(aapp,coexists_with,nusq).
local_semnet_1_GEN(aapp,coexists_with,carb).
local_semnet_1_GEN(opco,coexists_with,gngm).
local_semnet_1_GEN(opco,coexists_with,phsu).
local_semnet_1_GEN(opco,coexists_with,antb).
local_semnet_1_GEN(opco,coexists_with,bacs).
local_semnet_1_GEN(opco,coexists_with,imft).
local_semnet_1_GEN(opco,coexists_with,vita).
local_semnet_1_GEN(opco,coexists_with,enzy).
local_semnet_1_GEN(opco,coexists_with,horm).
local_semnet_1_GEN(opco,coexists_with,rcpt).
local_semnet_1_GEN(opco,coexists_with,nsba).
local_semnet_1_GEN(opco,coexists_with,hops).
local_semnet_1_GEN(opco,coexists_with,inch).
local_semnet_1_GEN(opco,coexists_with,orch).
local_semnet_1_GEN(opco,coexists_with,lipd).
local_semnet_1_GEN(opco,coexists_with,eico).
local_semnet_1_GEN(opco,coexists_with,strd).
local_semnet_1_GEN(opco,coexists_with,elii).
local_semnet_1_GEN(opco,coexists_with,opco).
local_semnet_1_GEN(opco,coexists_with,nusq).
local_semnet_1_GEN(opco,coexists_with,carb).
local_semnet_1_GEN(nusq,coexists_with,gngm).
local_semnet_1_GEN(nusq,coexists_with,phsu).
local_semnet_1_GEN(nusq,coexists_with,antb).
local_semnet_1_GEN(nusq,coexists_with,bacs).
local_semnet_1_GEN(nusq,coexists_with,imft).
local_semnet_1_GEN(nusq,coexists_with,vita).
local_semnet_1_GEN(nusq,coexists_with,enzy).
local_semnet_1_GEN(nusq,coexists_with,horm).
local_semnet_1_GEN(nusq,coexists_with,rcpt).
local_semnet_1_GEN(nusq,coexists_with,nsba).
local_semnet_1_GEN(nusq,coexists_with,hops).
local_semnet_1_GEN(nusq,coexists_with,inch).
local_semnet_1_GEN(nusq,coexists_with,orch).
local_semnet_1_GEN(nusq,coexists_with,lipd).
local_semnet_1_GEN(nusq,coexists_with,eico).
local_semnet_1_GEN(nusq,coexists_with,strd).
local_semnet_1_GEN(nusq,coexists_with,elii).
local_semnet_1_GEN(nusq,coexists_with,opco).
local_semnet_1_GEN(nusq,coexists_with,nusq).
local_semnet_1_GEN(nusq,coexists_with,carb).
local_semnet_1_GEN(carb,coexists_with,gngm).
local_semnet_1_GEN(carb,coexists_with,phsu).
local_semnet_1_GEN(carb,coexists_with,antb).
local_semnet_1_GEN(carb,coexists_with,bacs).
local_semnet_1_GEN(carb,coexists_with,imft).
local_semnet_1_GEN(carb,coexists_with,vita).
local_semnet_1_GEN(carb,coexists_with,enzy).
local_semnet_1_GEN(carb,coexists_with,horm).
local_semnet_1_GEN(carb,coexists_with,rcpt).
local_semnet_1_GEN(carb,coexists_with,nsba).
local_semnet_1_GEN(carb,coexists_with,hops).
local_semnet_1_GEN(carb,coexists_with,inch).
local_semnet_1_GEN(carb,coexists_with,orch).
local_semnet_1_GEN(carb,coexists_with,lipd).
local_semnet_1_GEN(carb,coexists_with,eico).
local_semnet_1_GEN(carb,coexists_with,strd).
local_semnet_1_GEN(carb,coexists_with,elii).
local_semnet_1_GEN(carb,coexists_with,opco).
local_semnet_1_GEN(carb,coexists_with,nusq).
local_semnet_1_GEN(carb,coexists_with,carb).
local_semnet_1_GEN(elii,coexists_with,gngm).
local_semnet_1_GEN(elii,coexists_with,phsu).
local_semnet_1_GEN(elii,coexists_with,antb).
local_semnet_1_GEN(elii,coexists_with,bacs).
local_semnet_1_GEN(elii,coexists_with,imft).
local_semnet_1_GEN(elii,coexists_with,vita).
local_semnet_1_GEN(elii,coexists_with,enzy).
local_semnet_1_GEN(elii,coexists_with,horm).
local_semnet_1_GEN(elii,coexists_with,rcpt).
local_semnet_1_GEN(elii,coexists_with,nsba).
local_semnet_1_GEN(elii,coexists_with,hops).
local_semnet_1_GEN(elii,coexists_with,inch).
local_semnet_1_GEN(elii,coexists_with,orch).
local_semnet_1_GEN(elii,coexists_with,lipd).
local_semnet_1_GEN(elii,coexists_with,eico).
local_semnet_1_GEN(elii,coexists_with,strd).
local_semnet_1_GEN(elii,coexists_with,elii).
local_semnet_1_GEN(elii,coexists_with,opco).
local_semnet_1_GEN(elii,coexists_with,nusq).
local_semnet_1_GEN(elii,coexists_with,carb).
% TREATS ADDITION
%local_semnet_1_GEN(phsu,treats,cell).
%local_semnet_1_GEN(orch,treats,cell).
%CONVERTS_TO
local_semnet_1_GEN(gngm,converts_to,gngm).
local_semnet_1_GEN(gngm,converts_to,phsu).
local_semnet_1_GEN(gngm,converts_to,antb).
local_semnet_1_GEN(gngm,converts_to,bacs).
local_semnet_1_GEN(gngm,converts_to,imft).
local_semnet_1_GEN(gngm,converts_to,vita).
local_semnet_1_GEN(gngm,converts_to,enzy).
local_semnet_1_GEN(gngm,converts_to,horm).
local_semnet_1_GEN(gngm,converts_to,rcpt).
local_semnet_1_GEN(gngm,converts_to,nsba).
local_semnet_1_GEN(gngm,converts_to,hops).
local_semnet_1_GEN(gngm,converts_to,inch).
local_semnet_1_GEN(gngm,converts_to,orch).
local_semnet_1_GEN(gngm,converts_to,lipd).
local_semnet_1_GEN(gngm,converts_to,eico).
local_semnet_1_GEN(gngm,converts_to,strd).
local_semnet_1_GEN(gngm,converts_to,elii).
local_semnet_1_GEN(gngm,converts_to,opco).
local_semnet_1_GEN(gngm,converts_to,nusq).
local_semnet_1_GEN(gngm,converts_to,carb).
local_semnet_1_GEN(phsu,converts_to,gngm).
local_semnet_1_GEN(phsu,converts_to,phsu).
local_semnet_1_GEN(phsu,converts_to,antb).
local_semnet_1_GEN(phsu,converts_to,bacs).
local_semnet_1_GEN(phsu,converts_to,imft).
local_semnet_1_GEN(phsu,converts_to,vita).
local_semnet_1_GEN(phsu,converts_to,enzy).
local_semnet_1_GEN(phsu,converts_to,horm).
local_semnet_1_GEN(phsu,converts_to,rcpt).
local_semnet_1_GEN(phsu,converts_to,nsba).
local_semnet_1_GEN(phsu,converts_to,hops).
local_semnet_1_GEN(phsu,converts_to,inch).
local_semnet_1_GEN(phsu,converts_to,orch).
local_semnet_1_GEN(phsu,converts_to,lipd).
local_semnet_1_GEN(phsu,converts_to,eico).
local_semnet_1_GEN(phsu,converts_to,strd).
local_semnet_1_GEN(phsu,converts_to,elii).
local_semnet_1_GEN(phsu,converts_to,opco).
local_semnet_1_GEN(phsu,converts_to,nusq).
local_semnet_1_GEN(phsu,converts_to,carb).
local_semnet_1_GEN(antb,converts_to,gngm).
local_semnet_1_GEN(antb,converts_to,phsu).
local_semnet_1_GEN(antb,converts_to,antb).
local_semnet_1_GEN(antb,converts_to,bacs).
local_semnet_1_GEN(antb,converts_to,imft).
local_semnet_1_GEN(antb,converts_to,vita).
local_semnet_1_GEN(antb,converts_to,enzy).
local_semnet_1_GEN(antb,converts_to,horm).
local_semnet_1_GEN(antb,converts_to,rcpt).
local_semnet_1_GEN(antb,converts_to,nsba).
local_semnet_1_GEN(antb,converts_to,hops).
local_semnet_1_GEN(antb,converts_to,inch).
local_semnet_1_GEN(antb,converts_to,orch).
local_semnet_1_GEN(antb,converts_to,lipd).
local_semnet_1_GEN(antb,converts_to,eico).
local_semnet_1_GEN(antb,converts_to,strd).
local_semnet_1_GEN(antb,converts_to,elii).
local_semnet_1_GEN(antb,converts_to,opco).
local_semnet_1_GEN(antb,converts_to,nusq).
local_semnet_1_GEN(antb,converts_to,carb).
local_semnet_1_GEN(bacs,converts_to,gngm).
local_semnet_1_GEN(bacs,converts_to,phsu).
local_semnet_1_GEN(bacs,converts_to,antb).
local_semnet_1_GEN(bacs,converts_to,bacs).
local_semnet_1_GEN(bacs,converts_to,imft).
local_semnet_1_GEN(bacs,converts_to,vita).
local_semnet_1_GEN(bacs,converts_to,enzy).
local_semnet_1_GEN(bacs,converts_to,horm).
local_semnet_1_GEN(bacs,converts_to,rcpt).
local_semnet_1_GEN(bacs,converts_to,nsba).
local_semnet_1_GEN(bacs,converts_to,hops).
local_semnet_1_GEN(bacs,converts_to,inch).
local_semnet_1_GEN(bacs,converts_to,orch).
local_semnet_1_GEN(bacs,converts_to,lipd).
local_semnet_1_GEN(bacs,converts_to,eico).
local_semnet_1_GEN(bacs,converts_to,strd).
local_semnet_1_GEN(bacs,converts_to,elii).
local_semnet_1_GEN(bacs,converts_to,opco).
local_semnet_1_GEN(bacs,converts_to,nusq).
local_semnet_1_GEN(bacs,converts_to,carb).
local_semnet_1_GEN(imft,converts_to,gngm).
local_semnet_1_GEN(imft,converts_to,phsu).
local_semnet_1_GEN(imft,converts_to,antb).
local_semnet_1_GEN(imft,converts_to,bacs).
local_semnet_1_GEN(imft,converts_to,imft).
local_semnet_1_GEN(imft,converts_to,vita).
local_semnet_1_GEN(imft,converts_to,enzy).
local_semnet_1_GEN(imft,converts_to,horm).
local_semnet_1_GEN(imft,converts_to,rcpt).
local_semnet_1_GEN(imft,converts_to,nsba).
local_semnet_1_GEN(imft,converts_to,hops).
local_semnet_1_GEN(imft,converts_to,inch).
local_semnet_1_GEN(imft,converts_to,orch).
local_semnet_1_GEN(imft,converts_to,lipd).
local_semnet_1_GEN(imft,converts_to,eico).
local_semnet_1_GEN(imft,converts_to,strd).
local_semnet_1_GEN(imft,converts_to,elii).
local_semnet_1_GEN(imft,converts_to,opco).
local_semnet_1_GEN(imft,converts_to,nusq).
local_semnet_1_GEN(imft,converts_to,carb).
local_semnet_1_GEN(vita,converts_to,gngm).
local_semnet_1_GEN(vita,converts_to,phsu).
local_semnet_1_GEN(vita,converts_to,antb).
local_semnet_1_GEN(vita,converts_to,bacs).
local_semnet_1_GEN(vita,converts_to,imft).
local_semnet_1_GEN(vita,converts_to,vita).
local_semnet_1_GEN(vita,converts_to,enzy).
local_semnet_1_GEN(vita,converts_to,horm).
local_semnet_1_GEN(vita,converts_to,rcpt).
local_semnet_1_GEN(vita,converts_to,nsba).
local_semnet_1_GEN(vita,converts_to,hops).
local_semnet_1_GEN(vita,converts_to,inch).
local_semnet_1_GEN(vita,converts_to,orch).
local_semnet_1_GEN(vita,converts_to,lipd).
local_semnet_1_GEN(vita,converts_to,eico).
local_semnet_1_GEN(vita,converts_to,strd).
local_semnet_1_GEN(vita,converts_to,elii).
local_semnet_1_GEN(vita,converts_to,opco).
local_semnet_1_GEN(vita,converts_to,nusq).
local_semnet_1_GEN(vita,converts_to,carb).
local_semnet_1_GEN(enzy,converts_to,gngm).
local_semnet_1_GEN(enzy,converts_to,phsu).
local_semnet_1_GEN(enzy,converts_to,antb).
local_semnet_1_GEN(enzy,converts_to,bacs).
local_semnet_1_GEN(enzy,converts_to,imft).
local_semnet_1_GEN(enzy,converts_to,vita).
local_semnet_1_GEN(enzy,converts_to,enzy).
local_semnet_1_GEN(enzy,converts_to,horm).
local_semnet_1_GEN(enzy,converts_to,rcpt).
local_semnet_1_GEN(enzy,converts_to,nsba).
local_semnet_1_GEN(enzy,converts_to,hops).
local_semnet_1_GEN(enzy,converts_to,inch).
local_semnet_1_GEN(enzy,converts_to,orch).
local_semnet_1_GEN(enzy,converts_to,lipd).
local_semnet_1_GEN(enzy,converts_to,eico).
local_semnet_1_GEN(enzy,converts_to,strd).
local_semnet_1_GEN(enzy,converts_to,elii).
local_semnet_1_GEN(enzy,converts_to,opco).
local_semnet_1_GEN(enzy,converts_to,nusq).
local_semnet_1_GEN(enzy,converts_to,carb).
local_semnet_1_GEN(horm,converts_to,gngm).
local_semnet_1_GEN(horm,converts_to,phsu).
local_semnet_1_GEN(horm,converts_to,antb).
local_semnet_1_GEN(horm,converts_to,bacs).
local_semnet_1_GEN(horm,converts_to,imft).
local_semnet_1_GEN(horm,converts_to,vita).
local_semnet_1_GEN(horm,converts_to,enzy).
local_semnet_1_GEN(horm,converts_to,horm).
local_semnet_1_GEN(horm,converts_to,rcpt).
local_semnet_1_GEN(horm,converts_to,nsba).
local_semnet_1_GEN(horm,converts_to,hops).
local_semnet_1_GEN(horm,converts_to,inch).
local_semnet_1_GEN(horm,converts_to,orch).
local_semnet_1_GEN(horm,converts_to,lipd).
local_semnet_1_GEN(horm,converts_to,eico).
local_semnet_1_GEN(horm,converts_to,strd).
local_semnet_1_GEN(horm,converts_to,elii).
local_semnet_1_GEN(horm,converts_to,opco).
local_semnet_1_GEN(horm,converts_to,nusq).
local_semnet_1_GEN(horm,converts_to,carb).
local_semnet_1_GEN(rcpt,converts_to,gngm).
local_semnet_1_GEN(rcpt,converts_to,phsu).
local_semnet_1_GEN(rcpt,converts_to,antb).
local_semnet_1_GEN(rcpt,converts_to,bacs).
local_semnet_1_GEN(rcpt,converts_to,imft).
local_semnet_1_GEN(rcpt,converts_to,vita).
local_semnet_1_GEN(rcpt,converts_to,enzy).
local_semnet_1_GEN(rcpt,converts_to,horm).
local_semnet_1_GEN(rcpt,converts_to,rcpt).
local_semnet_1_GEN(rcpt,converts_to,nsba).
local_semnet_1_GEN(rcpt,converts_to,hops).
local_semnet_1_GEN(rcpt,converts_to,inch).
local_semnet_1_GEN(rcpt,converts_to,orch).
local_semnet_1_GEN(rcpt,converts_to,lipd).
local_semnet_1_GEN(rcpt,converts_to,eico).
local_semnet_1_GEN(rcpt,converts_to,strd).
local_semnet_1_GEN(rcpt,converts_to,elii).
local_semnet_1_GEN(rcpt,converts_to,opco).
local_semnet_1_GEN(rcpt,converts_to,nusq).
local_semnet_1_GEN(rcpt,converts_to,carb).
local_semnet_1_GEN(nsba,converts_to,gngm).
local_semnet_1_GEN(nsba,converts_to,phsu).
local_semnet_1_GEN(nsba,converts_to,antb).
local_semnet_1_GEN(nsba,converts_to,bacs).
local_semnet_1_GEN(nsba,converts_to,imft).
local_semnet_1_GEN(nsba,converts_to,vita).
local_semnet_1_GEN(nsba,converts_to,enzy).
local_semnet_1_GEN(nsba,converts_to,horm).
local_semnet_1_GEN(nsba,converts_to,rcpt).
local_semnet_1_GEN(nsba,converts_to,nsba).
local_semnet_1_GEN(nsba,converts_to,hops).
local_semnet_1_GEN(nsba,converts_to,inch).
local_semnet_1_GEN(nsba,converts_to,orch).
local_semnet_1_GEN(nsba,converts_to,lipd).
local_semnet_1_GEN(nsba,converts_to,eico).
local_semnet_1_GEN(nsba,converts_to,strd).
local_semnet_1_GEN(nsba,converts_to,elii).
local_semnet_1_GEN(nsba,converts_to,opco).
local_semnet_1_GEN(nsba,converts_to,nusq).
local_semnet_1_GEN(nsba,converts_to,carb).
local_semnet_1_GEN(hops,converts_to,gngm).
local_semnet_1_GEN(hops,converts_to,phsu).
local_semnet_1_GEN(hops,converts_to,antb).
local_semnet_1_GEN(hops,converts_to,bacs).
local_semnet_1_GEN(hops,converts_to,imft).
local_semnet_1_GEN(hops,converts_to,vita).
local_semnet_1_GEN(hops,converts_to,enzy).
local_semnet_1_GEN(hops,converts_to,horm).
local_semnet_1_GEN(hops,converts_to,rcpt).
local_semnet_1_GEN(hops,converts_to,nsba).
local_semnet_1_GEN(hops,converts_to,hops).
local_semnet_1_GEN(hops,converts_to,inch).
local_semnet_1_GEN(hops,converts_to,orch).
local_semnet_1_GEN(hops,converts_to,lipd).
local_semnet_1_GEN(hops,converts_to,eico).
local_semnet_1_GEN(hops,converts_to,strd).
local_semnet_1_GEN(hops,converts_to,elii).
local_semnet_1_GEN(hops,converts_to,opco).
local_semnet_1_GEN(hops,converts_to,nusq).
local_semnet_1_GEN(hops,converts_to,carb).
local_semnet_1_GEN(inch,converts_to,gngm).
local_semnet_1_GEN(inch,converts_to,phsu).
local_semnet_1_GEN(inch,converts_to,antb).
local_semnet_1_GEN(inch,converts_to,bacs).
local_semnet_1_GEN(inch,converts_to,imft).
local_semnet_1_GEN(inch,converts_to,vita).
local_semnet_1_GEN(inch,converts_to,enzy).
local_semnet_1_GEN(inch,converts_to,horm).
local_semnet_1_GEN(inch,converts_to,rcpt).
local_semnet_1_GEN(inch,converts_to,nsba).
local_semnet_1_GEN(inch,converts_to,hops).
local_semnet_1_GEN(inch,converts_to,inch).
local_semnet_1_GEN(inch,converts_to,orch).
local_semnet_1_GEN(inch,converts_to,lipd).
local_semnet_1_GEN(inch,converts_to,eico).
local_semnet_1_GEN(inch,converts_to,strd).
local_semnet_1_GEN(inch,converts_to,elii).
local_semnet_1_GEN(inch,converts_to,opco).
local_semnet_1_GEN(inch,converts_to,nusq).
local_semnet_1_GEN(inch,converts_to,carb).
local_semnet_1_GEN(orch,converts_to,gngm).
local_semnet_1_GEN(orch,converts_to,phsu).
local_semnet_1_GEN(orch,converts_to,antb).
local_semnet_1_GEN(orch,converts_to,bacs).
local_semnet_1_GEN(orch,converts_to,imft).
local_semnet_1_GEN(orch,converts_to,vita).
local_semnet_1_GEN(orch,converts_to,enzy).
local_semnet_1_GEN(orch,converts_to,horm).
local_semnet_1_GEN(orch,converts_to,rcpt).
local_semnet_1_GEN(orch,converts_to,nsba).
local_semnet_1_GEN(orch,converts_to,hops).
local_semnet_1_GEN(orch,converts_to,inch).
local_semnet_1_GEN(orch,converts_to,orch).
local_semnet_1_GEN(orch,converts_to,lipd).
local_semnet_1_GEN(orch,converts_to,eico).
local_semnet_1_GEN(orch,converts_to,strd).
local_semnet_1_GEN(orch,converts_to,elii).
local_semnet_1_GEN(orch,converts_to,opco).
local_semnet_1_GEN(orch,converts_to,nusq).
local_semnet_1_GEN(orch,converts_to,carb).
local_semnet_1_GEN(lipd,converts_to,gngm).
local_semnet_1_GEN(lipd,converts_to,phsu).
local_semnet_1_GEN(lipd,converts_to,antb).
local_semnet_1_GEN(lipd,converts_to,bacs).
local_semnet_1_GEN(lipd,converts_to,imft).
local_semnet_1_GEN(lipd,converts_to,vita).
local_semnet_1_GEN(lipd,converts_to,enzy).
local_semnet_1_GEN(lipd,converts_to,horm).
local_semnet_1_GEN(lipd,converts_to,rcpt).
local_semnet_1_GEN(lipd,converts_to,nsba).
local_semnet_1_GEN(lipd,converts_to,hops).
local_semnet_1_GEN(lipd,converts_to,inch).
local_semnet_1_GEN(lipd,converts_to,orch).
local_semnet_1_GEN(lipd,converts_to,lipd).
local_semnet_1_GEN(lipd,converts_to,eico).
local_semnet_1_GEN(lipd,converts_to,strd).
local_semnet_1_GEN(lipd,converts_to,elii).
local_semnet_1_GEN(lipd,converts_to,opco).
local_semnet_1_GEN(lipd,converts_to,nusq).
local_semnet_1_GEN(lipd,converts_to,carb).
local_semnet_1_GEN(eico,converts_to,gngm).
local_semnet_1_GEN(eico,converts_to,phsu).
local_semnet_1_GEN(eico,converts_to,antb).
local_semnet_1_GEN(eico,converts_to,bacs).
local_semnet_1_GEN(eico,converts_to,imft).
local_semnet_1_GEN(eico,converts_to,vita).
local_semnet_1_GEN(eico,converts_to,enzy).
local_semnet_1_GEN(eico,converts_to,horm).
local_semnet_1_GEN(eico,converts_to,rcpt).
local_semnet_1_GEN(eico,converts_to,nsba).
local_semnet_1_GEN(eico,converts_to,hops).
local_semnet_1_GEN(eico,converts_to,inch).
local_semnet_1_GEN(eico,converts_to,orch).
local_semnet_1_GEN(eico,converts_to,lipd).
local_semnet_1_GEN(eico,converts_to,eico).
local_semnet_1_GEN(eico,converts_to,strd).
local_semnet_1_GEN(eico,converts_to,elii).
local_semnet_1_GEN(eico,converts_to,opco).
local_semnet_1_GEN(eico,converts_to,nusq).
local_semnet_1_GEN(eico,converts_to,carb).
local_semnet_1_GEN(strd,converts_to,gngm).
local_semnet_1_GEN(strd,converts_to,phsu).
local_semnet_1_GEN(strd,converts_to,antb).
local_semnet_1_GEN(strd,converts_to,bacs).
local_semnet_1_GEN(strd,converts_to,imft).
local_semnet_1_GEN(strd,converts_to,vita).
local_semnet_1_GEN(strd,converts_to,enzy).
local_semnet_1_GEN(strd,converts_to,horm).
local_semnet_1_GEN(strd,converts_to,rcpt).
local_semnet_1_GEN(strd,converts_to,nsba).
local_semnet_1_GEN(strd,converts_to,hops).
local_semnet_1_GEN(strd,converts_to,inch).
local_semnet_1_GEN(strd,converts_to,orch).
local_semnet_1_GEN(strd,converts_to,lipd).
local_semnet_1_GEN(strd,converts_to,eico).
local_semnet_1_GEN(strd,converts_to,strd).
local_semnet_1_GEN(strd,converts_to,elii).
local_semnet_1_GEN(strd,converts_to,opco).
local_semnet_1_GEN(strd,converts_to,nusq).
local_semnet_1_GEN(strd,converts_to,carb).
local_semnet_1_GEN(aapp,converts_to,gngm).
local_semnet_1_GEN(aapp,converts_to,phsu).
local_semnet_1_GEN(aapp,converts_to,antb).
local_semnet_1_GEN(aapp,converts_to,bacs).
local_semnet_1_GEN(aapp,converts_to,imft).
local_semnet_1_GEN(aapp,converts_to,vita).
local_semnet_1_GEN(aapp,converts_to,enzy).
local_semnet_1_GEN(aapp,converts_to,horm).
local_semnet_1_GEN(aapp,converts_to,rcpt).
local_semnet_1_GEN(aapp,converts_to,nsba).
local_semnet_1_GEN(aapp,converts_to,hops).
local_semnet_1_GEN(aapp,converts_to,inch).
local_semnet_1_GEN(aapp,converts_to,orch).
local_semnet_1_GEN(aapp,converts_to,lipd).
local_semnet_1_GEN(aapp,converts_to,eico).
local_semnet_1_GEN(aapp,converts_to,strd).
local_semnet_1_GEN(aapp,converts_to,elii).
local_semnet_1_GEN(aapp,converts_to,opco).
local_semnet_1_GEN(aapp,converts_to,nusq).
local_semnet_1_GEN(aapp,converts_to,carb).
local_semnet_1_GEN(opco,converts_to,gngm).
local_semnet_1_GEN(opco,converts_to,phsu).
local_semnet_1_GEN(opco,converts_to,antb).
local_semnet_1_GEN(opco,converts_to,bacs).
local_semnet_1_GEN(opco,converts_to,imft).
local_semnet_1_GEN(opco,converts_to,vita).
local_semnet_1_GEN(opco,converts_to,enzy).
local_semnet_1_GEN(opco,converts_to,horm).
local_semnet_1_GEN(opco,converts_to,rcpt).
local_semnet_1_GEN(opco,converts_to,nsba).
local_semnet_1_GEN(opco,converts_to,hops).
local_semnet_1_GEN(opco,converts_to,inch).
local_semnet_1_GEN(opco,converts_to,orch).
local_semnet_1_GEN(opco,converts_to,lipd).
local_semnet_1_GEN(opco,converts_to,eico).
local_semnet_1_GEN(opco,converts_to,strd).
local_semnet_1_GEN(opco,converts_to,elii).
local_semnet_1_GEN(opco,converts_to,opco).
local_semnet_1_GEN(opco,converts_to,nusq).
local_semnet_1_GEN(opco,converts_to,carb).
local_semnet_1_GEN(nusq,converts_to,gngm).
local_semnet_1_GEN(nusq,converts_to,phsu).
local_semnet_1_GEN(nusq,converts_to,antb).
local_semnet_1_GEN(nusq,converts_to,bacs).
local_semnet_1_GEN(nusq,converts_to,imft).
local_semnet_1_GEN(nusq,converts_to,vita).
local_semnet_1_GEN(nusq,converts_to,enzy).
local_semnet_1_GEN(nusq,converts_to,horm).
local_semnet_1_GEN(nusq,converts_to,rcpt).
local_semnet_1_GEN(nusq,converts_to,nsba).
local_semnet_1_GEN(nusq,converts_to,hops).
local_semnet_1_GEN(nusq,converts_to,inch).
local_semnet_1_GEN(nusq,converts_to,orch).
local_semnet_1_GEN(nusq,converts_to,lipd).
local_semnet_1_GEN(nusq,converts_to,eico).
local_semnet_1_GEN(nusq,converts_to,strd).
local_semnet_1_GEN(nusq,converts_to,elii).
local_semnet_1_GEN(nusq,converts_to,opco).
local_semnet_1_GEN(nusq,converts_to,nusq).
local_semnet_1_GEN(nusq,converts_to,carb).
local_semnet_1_GEN(carb,converts_to,gngm).
local_semnet_1_GEN(carb,converts_to,phsu).
local_semnet_1_GEN(carb,converts_to,antb).
local_semnet_1_GEN(carb,converts_to,bacs).
local_semnet_1_GEN(carb,converts_to,imft).
local_semnet_1_GEN(carb,converts_to,vita).
local_semnet_1_GEN(carb,converts_to,enzy).
local_semnet_1_GEN(carb,converts_to,horm).
local_semnet_1_GEN(carb,converts_to,rcpt).
local_semnet_1_GEN(carb,converts_to,nsba).
local_semnet_1_GEN(carb,converts_to,hops).
local_semnet_1_GEN(carb,converts_to,inch).
local_semnet_1_GEN(carb,converts_to,orch).
local_semnet_1_GEN(carb,converts_to,lipd).
local_semnet_1_GEN(carb,converts_to,eico).
local_semnet_1_GEN(carb,converts_to,strd).
local_semnet_1_GEN(carb,converts_to,elii).
local_semnet_1_GEN(carb,converts_to,opco).
local_semnet_1_GEN(carb,converts_to,nusq).
local_semnet_1_GEN(carb,converts_to,carb).
local_semnet_1_GEN(elii,converts_to,gngm).
local_semnet_1_GEN(elii,converts_to,phsu).
local_semnet_1_GEN(elii,converts_to,antb).
local_semnet_1_GEN(elii,converts_to,bacs).
local_semnet_1_GEN(elii,converts_to,imft).
local_semnet_1_GEN(elii,converts_to,vita).
local_semnet_1_GEN(elii,converts_to,enzy).
local_semnet_1_GEN(elii,converts_to,horm).
local_semnet_1_GEN(elii,converts_to,rcpt).
local_semnet_1_GEN(elii,converts_to,nsba).
local_semnet_1_GEN(elii,converts_to,hops).
local_semnet_1_GEN(elii,converts_to,inch).
local_semnet_1_GEN(elii,converts_to,orch).
local_semnet_1_GEN(elii,converts_to,lipd).
local_semnet_1_GEN(elii,converts_to,eico).
local_semnet_1_GEN(elii,converts_to,strd).
local_semnet_1_GEN(elii,converts_to,elii).
local_semnet_1_GEN(elii,converts_to,opco).
local_semnet_1_GEN(elii,converts_to,nusq).
local_semnet_1_GEN(elii,converts_to,carb).
% SPECIAL CASES FOR CHROMOSOME LOCATIONS
local_semnet_1_GEN(gngm,part_of,nusq).
%local_semnet_1_GEN(gngm,part_of,celc). % 07/2015 triple already in SN '06
local_semnet_1_GEN(nusq,location_of,dsyn).
local_semnet_1_GEN(nusq,location_of,neop).
local_semnet_1_GEN(nusq,location_of,mobd).
% AFFECTS - ANATOMY
local_semnet_1_GEN(inch,affects,anst).
local_semnet_1_GEN(elii,affects,anst).
local_semnet_1_GEN(orch,affects,anst).
local_semnet_1_GEN(lipd,affects,anst).
local_semnet_1_GEN(eico,affects,anst).
local_semnet_1_GEN(strd,affects,anst).
local_semnet_1_GEN(aapp,affects,anst).
local_semnet_1_GEN(opco,affects,anst).
local_semnet_1_GEN(nusq,affects,anst).
local_semnet_1_GEN(carb,affects,anst).
local_semnet_1_GEN(gngm,affects,anst).
local_semnet_1_GEN(nnon,affects,anst).
local_semnet_1_GEN(inch,affects,bopc).
local_semnet_1_GEN(elii,affects,bopc).
local_semnet_1_GEN(lipd,affects,bopc).
local_semnet_1_GEN(eico,affects,bopc).
local_semnet_1_GEN(opco,affects,bopc).
local_semnet_1_GEN(nusq,affects,bopc).
local_semnet_1_GEN(carb,affects,bopc).
local_semnet_1_GEN(orch,affects,bopc).
local_semnet_1_GEN(strd,affects,bopc).
local_semnet_1_GEN(aapp,affects,bopc).
local_semnet_1_GEN(gngm,affects,bopc).
local_semnet_1_GEN(nnon,affects,bopc).
local_semnet_1_GEN(phsu,affects,bopc).
local_semnet_1_GEN(nnon,affects,celc).
local_semnet_1_GEN(phsu,affects,celc).
local_semnet_1_GEN(nnon,affects,cell).
local_semnet_1_GEN(phsu,affects,cell).
local_semnet_1_GEN(inch,affects,emst).
local_semnet_1_GEN(elii,affects,emst).
local_semnet_1_GEN(orch,affects,emst).
local_semnet_1_GEN(lipd,affects,emst).
local_semnet_1_GEN(strd,affects,emst).
local_semnet_1_GEN(aapp,affects,emst).
local_semnet_1_GEN(opco,affects,emst).
local_semnet_1_GEN(nusq,affects,emst).
local_semnet_1_GEN(carb,affects,emst).
local_semnet_1_GEN(gngm,affects,emst).
local_semnet_1_GEN(nnon,affects,emst).
local_semnet_1_GEN(phsu,affects,emst).
local_semnet_1_GEN(inch,affects,ffas).
local_semnet_1_GEN(aapp,affects,ffas).
local_semnet_1_GEN(nusq,affects,ffas).
local_semnet_1_GEN(carb,affects,ffas).
local_semnet_1_GEN(nnon,affects,ffas).
local_semnet_1_GEN(opco,affects,ffas).
local_semnet_1_GEN(phsu,affects,ffas).
local_semnet_1_GEN(nnon,affects,tisu).
% CAUSES: ADD SUBSTANCEas arg1
%%% local_semnet_1_GEN(antb,causes,celf).  % CA
%%% local_semnet_1_GEN(bacs,causes,celf).  % CA
%%% local_semnet_1_GEN(carb,causes,celf).  % CA
%%% local_semnet_1_GEN(eico,causes,celf).  % CA
%%% local_semnet_1_GEN(elii,causes,celf).  % CA
local_semnet_1_GEN(enzy,causes,celf). % CA blocked this, but GR unblocks this as per TCR & MFA
%%% local_semnet_1_GEN(gngm,causes,celf).  % CA
%%% local_semnet_1_GEN(hops,causes,celf).  % CA
%%% local_semnet_1_GEN(horm,causes,celf).  % CA
%%% local_semnet_1_GEN(imft,causes,celf).  % CA
%%% local_semnet_1_GEN(inch,causes,celf).  % CA
%%% local_semnet_1_GEN(lipd,causes,celf).  % CA
%%% local_semnet_1_GEN(nnon,causes,celf).  % CA
%%% local_semnet_1_GEN(nsba,causes,celf).  % CA
%%% local_semnet_1_GEN(nusq,causes,celf).  % CA
%%% local_semnet_1_GEN(opco,causes,celf).  % CA
local_semnet_1_GEN(orch,causes,celf). % CA blocked this, but GR unblocks this as per TCR & MF
local_semnet_1_GEN(phsu,causes,celf). % Same as above
%%% local_semnet_1_GEN(rcpt,causes,celf).  % CA
%%% local_semnet_1_GEN(strd,causes,celf).  % CA
%%% local_semnet_1_GEN(vita,causes,celf).  % CA
%local_semnet_1_GEN(antb,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(bacs,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(carb,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(eico,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(elii,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(enzy,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(hops,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(horm,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(imft,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(inch,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(lipd,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(nnon,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(nsba,causes,comd). % 07/2015 triple also in SN '06
local_semnet_1_GEN(nusq,causes,comd).
%local_semnet_1_GEN(opco,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(orch,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(phsu,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(rcpt,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(strd,causes,comd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(vita,causes,comd). % 07/2015 triple also in SN '06
local_semnet_1_GEN(nusq,causes,dsyn).
local_semnet_1_GEN(antb,causes,fndg).
local_semnet_1_GEN(bacs,causes,fndg).
local_semnet_1_GEN(carb,causes,fndg).
local_semnet_1_GEN(eico,causes,fndg).
local_semnet_1_GEN(elii,causes,fndg).
local_semnet_1_GEN(enzy,causes,fndg).
local_semnet_1_GEN(gngm,causes,fndg).
local_semnet_1_GEN(hops,causes,fndg).
local_semnet_1_GEN(horm,causes,fndg).
local_semnet_1_GEN(imft,causes,fndg).
local_semnet_1_GEN(inch,causes,fndg).
local_semnet_1_GEN(lipd,causes,fndg).
local_semnet_1_GEN(nnon,causes,fndg).
local_semnet_1_GEN(nsba,causes,fndg).
local_semnet_1_GEN(nusq,causes,fndg).
local_semnet_1_GEN(opco,causes,fndg).
local_semnet_1_GEN(orch,causes,fndg).
local_semnet_1_GEN(phsu,causes,fndg).
local_semnet_1_GEN(rcpt,causes,fndg).
local_semnet_1_GEN(strd,causes,fndg).
local_semnet_1_GEN(vita,causes,fndg).
%local_semnet_1_GEN(antb,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(bacs,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(carb,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(eico,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(elii,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(enzy,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(hops,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(horm,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(imft,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(inch,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(lipd,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(nnon,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(nsba,causes,inpo). % 07/2015 triple also in SN '06
local_semnet_1_GEN(nusq,causes,inpo).
%local_semnet_1_GEN(opco,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(orch,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(phsu,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(rcpt,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(strd,causes,inpo). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(vita,causes,inpo). % 07/2015 triple also in SN '06
local_semnet_1_GEN(antb,causes,lbtr).
local_semnet_1_GEN(bacs,causes,lbtr).
local_semnet_1_GEN(carb,causes,lbtr).
local_semnet_1_GEN(eico,causes,lbtr).
local_semnet_1_GEN(elii,causes,lbtr).
local_semnet_1_GEN(enzy,causes,lbtr).
local_semnet_1_GEN(hops,causes,lbtr).
local_semnet_1_GEN(horm,causes,lbtr).
local_semnet_1_GEN(imft,causes,lbtr).
local_semnet_1_GEN(inch,causes,lbtr).
local_semnet_1_GEN(lipd,causes,lbtr).
local_semnet_1_GEN(nnon,causes,lbtr).
local_semnet_1_GEN(nsba,causes,lbtr).
local_semnet_1_GEN(nusq,causes,lbtr).
local_semnet_1_GEN(opco,causes,lbtr).
local_semnet_1_GEN(orch,causes,lbtr).
local_semnet_1_GEN(phsu,causes,lbtr).
local_semnet_1_GEN(rcpt,causes,lbtr).
local_semnet_1_GEN(strd,causes,lbtr).
local_semnet_1_GEN(vita,causes,lbtr).
%%% local_semnet_1_GEN(antb,causes,menp).  % CA
%%% local_semnet_1_GEN(bacs,causes,menp).  % CA
%%% local_semnet_1_GEN(carb,causes,menp).  % CA
%%% local_semnet_1_GEN(eico,causes,menp).  % CA
%%% local_semnet_1_GEN(elii,causes,menp).  % CA
%%% local_semnet_1_GEN(enzy,causes,menp).  % CA
%%% local_semnet_1_GEN(gngm,causes,menp).  % CA
%%% local_semnet_1_GEN(hops,causes,menp).  % CA
%%% local_semnet_1_GEN(horm,causes,menp).  % CA
%%% local_semnet_1_GEN(imft,causes,menp).  % CA
%%% local_semnet_1_GEN(inch,causes,menp).  % CA
%%% local_semnet_1_GEN(lipd,causes,menp).  % CA
%%% local_semnet_1_GEN(nnon,causes,menp).  % CA
%%% local_semnet_1_GEN(nsba,causes,menp).  % CA
%%% local_semnet_1_GEN(nusq,causes,menp).  % CA
%%% local_semnet_1_GEN(opco,causes,menp).  % CA
%%% local_semnet_1_GEN(orch,causes,menp).  % CA
%%% local_semnet_1_GEN(phsu,causes,menp).  % CA
%%% local_semnet_1_GEN(rcpt,causes,menp).  % CA
%%% local_semnet_1_GEN(strd,causes,menp).  % CA
%%% local_semnet_1_GEN(vita,causes,menp).  % CA
%local_semnet_1_GEN(antb,causes,mobd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(bacs,causes,mobd). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(carb,causes,mobd). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(eico,causes,mobd). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(elii,causes,mobd). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(enzy,causes,mobd). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(hops,causes,mobd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(horm,causes,mobd). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(imft,causes,mobd). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(inch,causes,mobd). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(lipd,causes,mobd). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(nnon,causes,mobd). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(nsba,causes,mobd). % 07/2015 triple also in SN '12
local_semnet_1_GEN(nusq,causes,mobd).
%local_semnet_1_GEN(opco,causes,mobd). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(orch,causes,mobd). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(phsu,causes,mobd). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(rcpt,causes,mobd). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(strd,causes,mobd). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(vita,causes,mobd). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(antb,causes,neop). % 07/2015 triple also in SN '12
%local_semnet_1_GEN(bacs,causes,neop). % 07/2015 triple also in SN '13
%local_semnet_1_GEN(carb,causes,neop). % 07/2015 triple also in SN '14
%local_semnet_1_GEN(eico,causes,neop). % 07/2015 triple also in SN '15
%local_semnet_1_GEN(elii,causes,neop). % 07/2015 triple also in SN '16
%local_semnet_1_GEN(enzy,causes,neop). % 07/2015 triple also in SN '17
%local_semnet_1_GEN(hops,causes,neop). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(horm,causes,neop). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(imft,causes,neop). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(inch,causes,neop). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(lipd,causes,neop). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(nnon,causes,neop). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(nsba,causes,neop). % 07/2015 triple also in SN '12
local_semnet_1_GEN(nusq,causes,neop).
%local_semnet_1_GEN(opco,causes,neop). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(orch,causes,neop). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(phsu,causes,neop). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(rcpt,causes,neop). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(strd,causes,neop). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(vita,causes,neop). % 07/2015 triple also in SN '11
%%% local_semnet_1_GEN(antb,causes,orgf).  % CA
%%% local_semnet_1_GEN(bacs,causes,orgf).  % CA
%%% local_semnet_1_GEN(carb,causes,orgf).  % CA
%%% local_semnet_1_GEN(eico,causes,orgf).  % CA
%%% local_semnet_1_GEN(elii,causes,orgf).  % CA
%%% local_semnet_1_GEN(enzy,causes,orgf).  % CA
%%% local_semnet_1_GEN(gngm,causes,orgf).  % CA
%%% local_semnet_1_GEN(hops,causes,orgf).  % CA
%%% local_semnet_1_GEN(horm,causes,orgf).  % CA
%%% local_semnet_1_GEN(imft,causes,orgf).  % CA
%%% local_semnet_1_GEN(inch,causes,orgf).  % CA
%%% local_semnet_1_GEN(lipd,causes,orgf).  % CA
%%% local_semnet_1_GEN(nnon,causes,orgf).  % CA
%%% local_semnet_1_GEN(nsba,causes,orgf).  % CA
%%% local_semnet_1_GEN(nusq,causes,orgf).  % CA
%%% local_semnet_1_GEN(opco,causes,orgf).  % CA
%%% local_semnet_1_GEN(orch,causes,orgf).  % CA
%%% local_semnet_1_GEN(phsu,causes,orgf).  % CA
%%% local_semnet_1_GEN(rcpt,causes,orgf).  % CA
%%% local_semnet_1_GEN(strd,causes,orgf).  % CA
%%% local_semnet_1_GEN(vita,causes,orgf).  % CA
%local_semnet_1_GEN(antb,causes,patf). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(bacs,causes,patf). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(carb,causes,patf). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(eico,causes,patf). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(elii,causes,patf). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(enzy,causes,patf). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(hops,causes,patf). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(horm,causes,patf). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(imft,causes,patf). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(inch,causes,patf). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(lipd,causes,patf). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(nnon,causes,patf). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(nsba,causes,patf). % 07/2015 triple also in SN '12
local_semnet_1_GEN(nusq,causes,patf).
%local_semnet_1_GEN(opco,causes,patf). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(orch,causes,patf). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(phsu,causes,patf). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(rcpt,causes,patf). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(strd,causes,patf). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(vita,causes,patf). % 07/2015 triple also in SN '11
local_semnet_1_GEN(topp,causes,patf). % GR 06/2015
local_semnet_1_GEN(genf,causes,dsyn). % GR 06/2015
local_semnet_1_GEN(genf,causes,sosy). % GR 06/2015
local_semnet_1_GEN(bacs,causes,biof). % GR 06/2015
local_semnet_1_GEN(inpo,causes,anab). % GR 06/2015
local_semnet_1_GEN(dsyn,causes,acab). % GR 06/2015
local_semnet_1_GEN(ortf,causes,orgf). % GR 06/2015
local_semnet_1_GEN(celf,causes,celf). % GR 06/2015 imported from SPA domain
local_semnet_1_GEN(moft,causes,celf). % GR 06/2015 imported from SPA domain
local_semnet_1_GEN(moft,causes,moft). % GR 06/2015 imported from SPA domain
local_semnet_1_GEN(orgm,causes,dsyn). % GR 06/2015 imported from SPA domain
local_semnet_1_GEN(ortf,causes,fndg). % GR 06/2015 imported from SPA domain
local_semnet_1_GEN(phsf,causes,moft). % GR 06/2015 imported from SPA domain
%%% local_semnet_1_GEN(antb,causes,phsf).  % CA
%%% local_semnet_1_GEN(bacs,causes,phsf).  % CA
%%% local_semnet_1_GEN(carb,causes,phsf).  % CA
%%% local_semnet_1_GEN(eico,causes,phsf).  % CA
%%% local_semnet_1_GEN(elii,causes,phsf).  % CA
local_semnet_1_GEN(enzy,causes,phsf). % CA blocked this, but GR unblocks this as per TCR & MF
%%% local_semnet_1_GEN(hops,causes,phsf).  % CA
%%% local_semnet_1_GEN(horm,causes,phsf).  % CA
%%% local_semnet_1_GEN(imft,causes,phsf).  % CA
local_semnet_1_GEN(inch,causes,phsf). % CA blocked this, but GR unblocks this as per TCR & MF
%%% local_semnet_1_GEN(lipd,causes,phsf).  % CA
%%% local_semnet_1_GEN(nnon,causes,phsf).  % CA
%%% local_semnet_1_GEN(nsba,causes,phsf).  % CA
%%% local_semnet_1_GEN(nusq,causes,phsf).  % CA
%%% local_semnet_1_GEN(opco,causes,phsf).  % CA
local_semnet_1_GEN(orch,causes,phsf). % CA blocked this, but GR unblocks this as per TCR & MF
local_semnet_1_GEN(phsu,causes,phsf). % CA blocked this, but GR unblocks this as per TCR & MF
%%% local_semnet_1_GEN(rcpt,causes,phsf).  % CA
local_semnet_1_GEN(strd,causes,phsf). % CA blocked this, but GR unblocks this as per TCR & MF
%%% local_semnet_1_GEN(vita,causes,phsf).  % CA
local_semnet_1_GEN(nnon,causes,sosy).
% Addins for summarization
local_semnet_1_GEN(aapp,disrupts,mobd).
local_semnet_1_GEN(antb,disrupts,mobd).
local_semnet_1_GEN(bacs,disrupts,mobd).
local_semnet_1_GEN(carb,disrupts,mobd).
local_semnet_1_GEN(eico,disrupts,mobd).
local_semnet_1_GEN(elii,disrupts,mobd).
local_semnet_1_GEN(enzy,disrupts,mobd).
local_semnet_1_GEN(gngm,disrupts,mobd).
local_semnet_1_GEN(hops,disrupts,mobd).
local_semnet_1_GEN(horm,disrupts,mobd).
local_semnet_1_GEN(imft,disrupts,mobd).
local_semnet_1_GEN(inch,disrupts,mobd).
local_semnet_1_GEN(lipd,disrupts,mobd).
local_semnet_1_GEN(nnon,disrupts,mobd).
local_semnet_1_GEN(nsba,disrupts,mobd).
local_semnet_1_GEN(nusq,disrupts,mobd).
local_semnet_1_GEN(opco,disrupts,mobd).
local_semnet_1_GEN(orch,disrupts,mobd).
local_semnet_1_GEN(phsu,disrupts,mobd).
local_semnet_1_GEN(rcpt,disrupts,mobd).
local_semnet_1_GEN(strd,disrupts,mobd).
local_semnet_1_GEN(vita,disrupts,mobd).
local_semnet_1_GEN(orch,disrupts,moft). % GR 06/26/2016
local_semnet_1_GEN(aapp,augments,mobd).
local_semnet_1_GEN(antb,augments,mobd).
local_semnet_1_GEN(bacs,augments,mobd).
local_semnet_1_GEN(carb,augments,mobd).
local_semnet_1_GEN(eico,augments,mobd).
local_semnet_1_GEN(elii,augments,mobd).
local_semnet_1_GEN(enzy,augments,mobd).
local_semnet_1_GEN(gngm,augments,mobd).
local_semnet_1_GEN(hops,augments,mobd).
local_semnet_1_GEN(horm,augments,mobd).
local_semnet_1_GEN(imft,augments,mobd).
local_semnet_1_GEN(inch,augments,mobd).
local_semnet_1_GEN(lipd,augments,mobd).
local_semnet_1_GEN(nnon,augments,mobd).
local_semnet_1_GEN(nsba,augments,mobd).
local_semnet_1_GEN(nusq,augments,mobd).
local_semnet_1_GEN(opco,augments,mobd).
local_semnet_1_GEN(orch,augments,mobd).
local_semnet_1_GEN(phsu,augments,mobd).
local_semnet_1_GEN(rcpt,augments,mobd).
local_semnet_1_GEN(strd,augments,mobd).
local_semnet_1_GEN(vita,augments,mobd).
local_semnet_1_GEN(aapp,augments,ortf).
local_semnet_1_GEN(antb,augments,ortf).
local_semnet_1_GEN(bacs,augments,ortf).
local_semnet_1_GEN(carb,augments,ortf).
local_semnet_1_GEN(eico,augments,ortf).
local_semnet_1_GEN(elii,augments,ortf).
local_semnet_1_GEN(enzy,augments,ortf).
local_semnet_1_GEN(gngm,augments,ortf).
local_semnet_1_GEN(hops,augments,ortf).
local_semnet_1_GEN(horm,augments,ortf).
local_semnet_1_GEN(imft,augments,ortf).
local_semnet_1_GEN(inch,augments,ortf).
local_semnet_1_GEN(lipd,augments,ortf).
local_semnet_1_GEN(nnon,augments,ortf).
local_semnet_1_GEN(nsba,augments,ortf).
local_semnet_1_GEN(nusq,augments,ortf).
local_semnet_1_GEN(opco,augments,ortf).
local_semnet_1_GEN(orch,augments,ortf).
local_semnet_1_GEN(phsu,augments,ortf).
local_semnet_1_GEN(rcpt,augments,ortf).
local_semnet_1_GEN(strd,augments,ortf).
local_semnet_1_GEN(vita,augments,ortf).
local_semnet_1_GEN(aapp,augments,genf).
local_semnet_1_GEN(antb,augments,genf).
local_semnet_1_GEN(bacs,augments,genf).
local_semnet_1_GEN(carb,augments,genf).
local_semnet_1_GEN(eico,augments,genf).
local_semnet_1_GEN(elii,augments,genf).
local_semnet_1_GEN(enzy,augments,genf).
local_semnet_1_GEN(gngm,augments,genf).
local_semnet_1_GEN(hops,augments,genf).
local_semnet_1_GEN(horm,augments,genf).
local_semnet_1_GEN(imft,augments,genf).
local_semnet_1_GEN(inch,augments,genf).
local_semnet_1_GEN(lipd,augments,genf).
local_semnet_1_GEN(nnon,augments,genf).
local_semnet_1_GEN(nsba,augments,genf).
local_semnet_1_GEN(nusq,augments,genf).
local_semnet_1_GEN(opco,augments,genf).
local_semnet_1_GEN(orch,augments,genf).
local_semnet_1_GEN(phsu,augments,genf).
local_semnet_1_GEN(rcpt,augments,genf).
local_semnet_1_GEN(strd,augments,genf).
local_semnet_1_GEN(vita,augments,genf).
local_semnet_1_GEN(rcpt,augments,celf).
local_semnet_1_GEN(chem,augments,moft). % GR 06/02/2017 Source: GENIA 7565683_S7  The mutation of either residue is sufficient to abolish ligand-induced degradation. (ligand semantic type is chem)
% The following by GR 01/11/08
%local_semnet_1_GEN(topp,uses,bpoc). % GR put it in and comments out of Aug_09/ it causes FPs
%local_semnet_1_GEN(topp,uses,topp). % GR put it in and GR commented it out the (SPEC) module interferes here
local_semnet_1_GEN(topp,uses,aapp). % GR 06/2015
local_semnet_1_GEN(topp,uses,lipd). % GR 06/2015
local_semnet_1_GEN(topp,uses,carb). % GR 06/2015
local_semnet_1_GEN(diap,diagnoses,fndg).
local_semnet_1_GEN(lbpr,diagnoses,fndg).
local_semnet_1_GEN(mbrt,diagnoses,comd). % GR 06/2015
local_semnet_1_GEN(prog,diagnoses,sosy).
%local_semnet_1_GEN(sosy,diagnoses,sosy).
local_semnet_1_GEN(diap,diagnoses,sosy).
local_semnet_1_GEN(hlca,uses,diap). % GR 06/2015
local_semnet_1_GEN(hlca,uses,medd). % GR 01/18/2018
local_semnet_1_GEN(lbpr,uses,aapp). % GR 06/2015
local_semnet_1_GEN(lbpr,uses,topp). % GR 06/2015
local_semnet_1_GEN(dsyn,augments,dsyn). % GR 06/2015
local_semnet_1_GEN(comd,causes,dsyn).
local_semnet_1_GEN(diap,causes,dsyn).
local_semnet_1_GEN(diap,causes,sosy).
local_semnet_1_GEN(dsyn,causes,dsyn).
local_semnet_1_GEN(dsyn,causes,patf).
local_semnet_1_GEN(fndg,causes,patf). % Negation corpus PMID 15080783
% local_semnet_1_GEN(dsyn,causes,sosy). MF put it in
local_semnet_1_GEN(dsyn,causes,hlca).
local_semnet_1_GEN(dsyn,causes,orgf).
local_semnet_1_GEN(sosy,causes,hlca).
local_semnet_1_GEN(sosy,causes,dsyn).
local_semnet_1_GEN(sosy,causes,sosy).
local_semnet_1_GEN(inpo,causes,sosy). % GR 01/23/2018
local_semnet_1_GEN(mobd,causes,dsyn).
local_semnet_1_GEN(dora,causes,fndg).
local_semnet_1_GEN(patf,causes,orgf).
local_semnet_1_GEN(topp,causes,dsyn).
local_semnet_1_GEN(cgab,causes,acab).
local_semnet_1_GEN(cgab,causes,dsyn). % GR 06/12/2017
local_semnet_1_GEN(fndg,process_of,humn). % Did MF comment it out? Sb. did
local_semnet_1_GEN(inbe,process_of,humn).
local_semnet_1_GEN(inpo,process_of,humn).
local_semnet_1_GEN(dsyn,disrupts,socb).
local_semnet_1_GEN(diap,indicates,dsyn).
local_semnet_1_GEN(diap,indicates,sosy).
local_semnet_1_GEN(diap,indicates,patf).
local_semnet_1_GEN(diap,indicates,fndg).
local_semnet_1_GEN(fndg,predisposes,fndg).
local_semnet_1_GEN(fndg,predisposes,ortf).
local_semnet_1_GEN(dsyn,predisposes,patf). % Negation corpus PMID 15010711
%%local_semnet_1_GEN(fndg,predisposes,acab).
local_semnet_1_GEN(topp,predisposes,dsyn).
local_semnet_1_GEN(topp,predisposes,sosy).
local_semnet_1_GEN(topp,predisposes,patf).
local_semnet_1_GEN(fndg,coexists_with,dsyn).
local_semnet_1_GEN(fndg,coexists_with,patf). % 09/2015
local_semnet_1_GEN(sosy,coexists_with,comd).
local_semnet_1_GEN(anab,coexists_with,sosy). % 05/24/2017 source: GS PMID 15534249
local_semnet_1_GEN(fndg,coexists_with,sosy). % 05/24/2017 source: GS PMID 15534249
local_semnet_1_GEN(moft,coexists_with,dsyn). % GR 12/12/2017  [Our results indicate that lipid peroxidation [moft] occurs in X-ALD [dsyn] - I also added the new indicator for coexists
local_semnet_1_GEN(neop,coexists_with,patf). % GR 12/20/2017  
local_semnet_1_GEN(patf,coexists_with,neop). % GR 12/20/2017  
local_semnet_1_GEN(diap,administered_to,humn).
local_semnet_1_GEN(topp,administered_to,humn).
local_semnet_1_GEN(hlca,administered_to,humn).
local_semnet_1_GEN(clna,associated_with,sosy).
local_semnet_1_GEN(imft,associated_with,dsyn).
local_semnet_1_GEN(dsyn,associated_with,moft).
local_semnet_1_GEN(dsyn,associated_with,rich).
local_semnet_1_GEN(dsyn,associated_with,genf).
local_semnet_1_GEN(genf,associated_with,genf). % 05/25/2017 source: GS PMID 15634358 Transgressive segregation|genf| was significantly associated with allelic variation [genf]
%%local_semnet_1_GEN(aapp,associated_with,orgf).  In GR list, but commented out by CA
%%local_semnet_1_GEN(aapp,associated_with,patf).  Already in file, proposed by CA
%%local_semnet_1_GEN(aapp,associated_with,dsyn).  Already in file, proposed by CA
local_semnet_1_GEN(orga,associated_with,dsyn).
local_semnet_1_GEN(orgf,associated_with,dsyn). % GR comments it out to test sentence 08/07/09
local_semnet_1_GEN(orgf,associated_with,sosy).
local_semnet_1_GEN(bdsy,associated_with,fndg).
local_semnet_1_GEN(bdsu,associated_with,fndg).
local_semnet_1_GEN(phsf,associated_with,phsu).
local_semnet_1_GEN(phsf,associated_with,orch).
local_semnet_1_GEN(food,associated_with,inpo).
local_semnet_1_GEN(sosy,associated_with,dsyn).
local_semnet_1_GEN(patf,associated_with,gngm).
%local_semnet_1_GEN(moft,interacts_with,topp). % Commented out on 6/22/12 by Halil.
%local_semnet_1_GEN(phsu,interacts_with,dsyn). % Commented out on 6/22/12 by Halil.
%local_semnet_1_GEN(genf,interacts_with,clna). % Commented out on 6/22/12 by Halil.
local_semnet_1_GEN(aapp,interacts_with,nusq). % GR 12/06/2017
local_semnet_1_GEN(nusq,interacts_with,aapp). % GR 12/06/2017
local_semnet_1_GEN(dora,stimulates,topp).
local_semnet_1_GEN(topp,stimulates,orgf).
local_semnet_1_GEN(celf,augments,genf). % GR 05/31/2017 GENIA 9916709_S1 T cell priming enhances IL-4 gene expression by increasing nuclear factor of activated T cells.
local_semnet_1_GEN(aapp,stimulates,moft). % GR 06/21/2017 GENIA 9710600_S3 Tax expression promotes N-terminal phosphorylation and degradation of IkappaB alpha, a principal cytoplasmic inhibitor of NF-kappaB.  [tax protein STIMULATES phosphorylation]
local_semnet_1_GEN(aapp,stimulates,cell). % GR indicated by Halil on 11/09/2017
local_semnet_1_GEN(dsyn,affects,fndg).
local_semnet_1_GEN(phsu,affects,fndg).
local_semnet_1_GEN(aapp,affects,fndg).
local_semnet_1_GEN(hlca,affects,fndg).
local_semnet_1_GEN(inpo,affects,lbtr).
local_semnet_1_GEN(tisu,affects,patf).
%%local_semnet_1_GEN(topp,affects,orgf).
%%local_semnet_1_GEN(dsyn,affects,tisu).
local_semnet_1_GEN(topp,treats,fndg).
local_semnet_1_GEN(topp,treats,orga).
local_semnet_1_GEN(medd,treats,fndg).
local_semnet_1_GEN(phsu,treats,fndg).
local_semnet_1_GEN(chvs,treats,dsyn).
local_semnet_1_GEN(clnd,treats,humn).
local_semnet_1_GEN(phsu,inhibits,fndg).
local_semnet_1_GEN(virs,coexists_with,dsyn).
local_semnet_1_GEN(virs,coexists_with,virs). % 06/2015
local_semnet_1_GEN(orch,inhibits,ortf). % 04/28/09
local_semnet_1_GEN(phsu,inhibits,ortf). % 04/28/09
local_semnet_1_GEN(fndg,complicates,dsyn). % GR 10/31/2017 
local_semnet_1_GEN(fndg,complicates,inpo). % GR 10/31/2017 
local_semnet_1_GEN(fndg,complicates,sosy). % GR 10/31/2017 
local_semnet_1_GEN(blor,location_of,fndg).
local_semnet_1_GEN(blor,location_of,lbpr). % GR 06/2015 imported from DIM
local_semnet_1_GEN(bpoc,location_of,fndg).
%local_semnet_1_GEN(bpoc,location_of,neop). % 07/2015 triple also in SN '06
local_semnet_1_GEN(bpoc,location_of,medd).
local_semnet_1_GEN(bpoc,location_of,sosy).
local_semnet_1_GEN(bpoc,location_of,bacs). % GR 06/2015
local_semnet_1_GEN(bpoc,location_of,bdsu). % GR 06/2015
local_semnet_1_GEN(bsoj,location_of,fndg). % GR 06/2015
local_semnet_1_GEN(bsoj,location_of,sosy).
local_semnet_1_GEN(spco,location_of,cell). % GR 06/2015
local_semnet_1_GEN(emod,location_of,emst). % GR 06/2015
local_semnet_1_GEN(bpoc,location_of,anab). % GR 05/24/2017 source: GS PMID 15534249
local_semnet_1_GEN(humn,location_of,bdsu). % GR 12/18/2017 
local_semnet_1_GEN(dsyn,coexists_with,fndg).
%local_semnet_1_GEN(imft,affects,phsf). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(phsu,affects,phsf). % 07/2015 triple also in SN '07
local_semnet_1_GEN(humn,location_of,cgab). % GR 06/27/08
local_semnet_1_GEN(bact,produces,bacs). % GR 02/03/09 probiotics produces glutathione
local_semnet_1_GEN(bact,produces,aapp). % GR 02/03/09 probiotics produces glutathione
local_semnet_1_GEN(aapp,produces,chvf). % GR 06/2015
local_semnet_1_GEN(neop,produces,aapp). % GR 06/2015
local_semnet_1_GEN(diap,produces,lbtr). % GR 06/2015 imported from SPA domain
local_semnet_1_GEN(lbpr,produces,lbtr). % GR 06/2015 imported from SPA domain
local_semnet_1_GEN(aapp,produces,hops). % GR 05/25/2017 source: GS PMID 15592935  as in: Carbon monoxide [hops] is an endogenously produced gas mediator produced by heme oxygenase [enzy,aapp].
local_semnet_1_GEN(aapp,produces,inch). % GR 05/25/2017 source: GS PMID 15592935  as in: Carbon monoxide is an endogenously produced gas mediator produced by heme oxygenase.
local_semnet_1_GEN(enzy,produces,hops). % GR 05/25/2017 source: GS PMID 15592935  as in: Carbon monoxide is an endogenously produced gas mediator produced by heme oxygenase.
local_semnet_1_GEN(enzy,produces,inch). % GR 05/25/2017 source: GS PMID 15592935  as in: Carbon monoxide is an endogenously produced gas mediator produced by heme oxygenase.
local_semnet_1_GEN(bact,interacts_with,aapp). % GR 02/09/09 effect of probiotics on cytokines
%local_semnet_1_GEN(lipd,interacts_with,irda). % GR 05/25/2017 source: GS PMID 15561572  as in: a brief incubation of LDL with PTIR267 (where incubation of-with=INTERACTS_WITH) but it appears ontological predication is already in SN
%local_semnet_1_GEN(aapp,interacts_with,irda). % GR 05/25/2017 source: GS PMID 15561572  as in: a brief incubation of LDL with PTIR267 (where incubation of-with=INTERACTS_WITH) but it appears ontological predication is already in SN
local_semnet_1_GEN(cell,interacts_with,cell). % GR 06/15/2017 GENIA 7537762_S3 & 9170401_S2 Adhesion molecules that tether circulating leukocytes to endothelial cells may also transduce or modulate outside-in signals for cellular activation, providing an initial regulatory point in the inflammatory response.  Activated blood platelets tether and activate myeloid leukocytes.
local_semnet_1_GEN(gngm,interacts_with,cell). % GR 06/22/2017 GENIA  CD28 coligation of CD2-activated LPMC does not result in increased binding of trans-factors to the CD28RE.
local_semnet_1_GEN(aapp,interacts_with,gngm). % GR 06/22/2017 GENIA  protein induces/increases gene expression
local_semnet_1_GEN(npop,causes,inpo). % GR 02/09/09  radiation-induced injury
local_semnet_1_GEN(bacs,prevents,inpo). % GR 02/16/09 AEOL 10150 protects lungs from radiation-induced injury
local_semnet_1_GEN(bacs,prevents,dsyn). % GR Negation corpus PMID 16301356 
local_semnet_1_GEN(bact,causes,orgf). % GR 02/13/09  Probiotics induce an immunologic response
local_semnet_1_GEN(inpo,causes,dsyn). % GR 02/16/09 Mitral valve insufficiency caused by a rupture of chordae tendineae
%local_semnet_1_GEN(humn,treats,humn). % GR 02/16/09
local_semnet_1_GEN(patf,causes,dsyn).
local_semnet_1_GEN(sosy,causes,orgf).
local_semnet_1_GEN(clna,interacts_with,sosy).
local_semnet_1_GEN(virs,interacts_with,dsyn).
%local_semnet_1_GEN(virs,interacts_with,virs). % 07/2015 triple also in SN '06
local_semnet_1_GEN(bact,associated_with,dsyn). % GR 12/06/17
local_semnet_1_GEN(dsyn,associated_with,bact). % GR 12/11/17
local_semnet_1_GEN(bact,associated_with,sosy). % GR 12/06/17
local_semnet_1_GEN(sosy,associated_with,bact). % GR 12/11/17
local_semnet_1_GEN(orgm,associated_with,dsyn). % GR 12/06/17
local_semnet_1_GEN(dsyn,associated_with,orgm). % GR 12/11/17
local_semnet_1_GEN(neop,causes,dsyn).
local_semnet_1_GEN(neop,causes,neop). % 09/2015 to get liver metastases causes colorectal carcinoma
%local_semnet_1_GEN(fndg,causes,dsyn). % Under observation 05/07/09 by GR. Decided to comment out on 05/08/09
local_semnet_1_GEN(hlca,uses,phsu). % GR 04/28/09
local_semnet_1_GEN(sosy,coexists_with,dsyn). % GR 06/05/09
local_semnet_1_GEN(celf,coexists_with,neop). % GR 06/2015
local_semnet_1_GEN(topp,coexists_with,hlca). % GR 06/2015
local_semnet_1_GEN(inpo,causes,patf). % GR 06/09/09
local_semnet_1_GEN(dsyn,precedes,phsf). % GR 06/2015
local_semnet_1_GEN(diap,precedes,hlca). % GR 06/2015
local_semnet_1_GEN(topp,precedes,acab). % GR 06/2015
local_semnet_1_GEN(topp,precedes,diap). % GR 06/2015
local_semnet_1_GEN(chvf,associated_with,inpo). % GR 12/11/17
local_semnet_1_GEN(inpo,associated_with,chvf). % GR 12/11/17
local_semnet_1_GEN(inbe,affects,orgf). % Negation corpus: Alcohol ingestion acutely lowers blood pressure (BP) with vasodilation
local_semnet_1_GEN(bacs,interacts_with,aapp). % Negation corpus PNID 1793065
local_semnet_1_GEN(bdsy,affects,orgf). % Negation corpus PMID 23053984
% The following have been blocked from OCCURS_IN and become COEXISTS_WITH
%local_semnet_1_GEN(dsyn,coexists_with,dsyn). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(dsyn,coexists_with,inpo). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(dsyn,coexists_with,mobd). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(dsyn,coexists_with,neop). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(mobd,coexists_with,dsyn). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(mobd,coexists_with,inpo). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(mobd,coexists_with,mobd). % 07/2015 triple also in SN '12
%local_semnet_1_GEN(mobd,coexists_with,neop). % 07/2015 triple also in SN '13
%local_semnet_1_GEN(neop,coexists_with,dsyn). % 07/2015 triple also in SN '14
%local_semnet_1_GEN(neop,coexists_with,inpo). % 07/2015 triple also in SN '15
%local_semnet_1_GEN(neop,coexists_with,mobd). % 07/2015 triple also in SN '16
%local_semnet_1_GEN(neop,coexists_with,neop). % 07/2015 triple also in SN '17
%local_semnet_1_GEN(patf,coexists_with,dsyn). % 07/2015 triple also in SN '18
local_semnet_1_GEN(patf,coexists_with,fndg). % Added 09/2015
%local_semnet_1_GEN(patf,coexists_with,inpo). % 07/2015 triple also in SN '19
%local_semnet_1_GEN(patf,coexists_with,mobd). % 07/2015 triple also in SN '20
%local_semnet_1_GEN(patf,coexists_with,neop). % 07/2015 triple also in SN '21
% from Graciela, 11/10/09
%local_semnet_1_GEN(acab,occurs_in,grup). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(anab,occurs_in,grup). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(cgab,occurs_in,grup). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(dsyn,occurs_in,grup). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(inpo,occurs_in,grup). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(mobd,occurs_in,grup). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(neop,occurs_in,grup). % 07/2015 triple also in SN '12
local_semnet_1_GEN(orgf,occurs_in,grup).
%local_semnet_1_GEN(patf,occurs_in,grup). % 07/2015 triple also in SN '06
local_semnet_1_GEN(phpr,occurs_in,grup).
%local_semnet_1_GEN(acab,occurs_in,aggp). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(anab,occurs_in,aggp). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(cgab,occurs_in,aggp). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(dsyn,occurs_in,aggp). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(inpo,occurs_in,aggp). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(mobd,occurs_in,aggp). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(neop,occurs_in,aggp). % 07/2015 triple also in SN '12
local_semnet_1_GEN(orgf,occurs_in,aggp).
local_semnet_1_GEN(phpr,occurs_in,aggp).
%local_semnet_1_GEN(acab,occurs_in,famg). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(anab,occurs_in,famg). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(cgab,occurs_in,famg). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(dsyn,occurs_in,famg). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(inpo,occurs_in,famg). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(mobd,occurs_in,famg). % 07/2015 triple also in SN '11
%local_semnet_1_GEN(neop,occurs_in,famg). % 07/2015 triple also in SN '12
local_semnet_1_GEN(orgf,occurs_in,famg).
local_semnet_1_GEN(phpr,occurs_in,famg).
%local_semnet_1_GEN(acab,occurs_in,podg). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(anab,occurs_in,podg). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(cgab,occurs_in,podg). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(dsyn,occurs_in,podg). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(inpo,occurs_in,podg). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(mobd,occurs_in,podg). % 07/2015 triple also in SN '11
local_semnet_1_GEN(orgf,occurs_in,podg).
local_semnet_1_GEN(phpr,occurs_in,podg).
%local_semnet_1_GEN(acab,occurs_in,popg). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(anab,occurs_in,popg). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(cgab,occurs_in,popg). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(dsyn,occurs_in,popg). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(inpo,occurs_in,popg). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(mobd,occurs_in,popg). % 07/2015 triple also in SN '11
local_semnet_1_GEN(orgf,occurs_in,popg).
local_semnet_1_GEN(phpr,occurs_in,popg).
%local_semnet_1_GEN(acab,occurs_in,prog). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(anab,occurs_in,prog). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(cgab,occurs_in,prog). % 07/2015 triple also in SN '08
%local_semnet_1_GEN(dsyn,occurs_in,prog). % 07/2015 triple also in SN '09
%local_semnet_1_GEN(inpo,occurs_in,prog). % 07/2015 triple also in SN '10
%local_semnet_1_GEN(mobd,occurs_in,prog). % 07/2015 triple also in SN '11
local_semnet_1_GEN(orgf,occurs_in,prog).
local_semnet_1_GEN(phpr,occurs_in,prog).
%local_semnet_1_GEN(dsyn,process_of,amph). % 07/2015 triple also in SN '06
local_semnet_1_GEN(dsyn,process_of,bird).
%local_semnet_1_GEN(dsyn,process_of,fish). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(dsyn,process_of,humn). % 07/2015 triple also in SN '07
local_semnet_1_GEN(dsyn,process_of,mamm).
local_semnet_1_GEN(dsyn,process_of,plnt).
%local_semnet_1_GEN(dsyn,process_of,rept). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(dsyn,process_of,vtbt). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(neop,process_of,amph). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(neop,process_of,bird). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(neop,process_of,fish). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(neop,process_of,humn). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(neop,process_of,mamm). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(neop,process_of,plnt). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(neop,process_of,rept). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(neop,process_of,vtbt). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(patf,process_of,amph). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(patf,process_of,bird). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(patf,process_of,fish). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(patf,process_of,humn). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(patf,process_of,mamm). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(patf,process_of,plnt). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(patf,process_of,rept). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(patf,process_of,vtbt). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(mobd,process_of,humn). % GR 07/2015 as it's in the SN
%local_semnet_1_GEN(mobd,process_of,mamm). % GR 07/2015 as it's in the SN
local_semnet_1_GEN(moft,process_of,cell). % GR 05/25/2017 as per GS - PMID 15592935 Vascular endothelium, erythrocytes, smooth muscle and inflammatory cells (except macrophages) in the allergic patients exhibited stronger HO-1 immunoreaction compared to the control.
local_semnet_1_GEN(moft,process_of,tisu). % GR 05/25/2017 as per GS - PMID 15592935 Vascular endothelium, erythrocytes, smooth muscle and inflammatory cells (except macrophages) in the allergic patients exhibited stronger HO-1 immunoreaction compared to the control.
local_semnet_1_GEN(genf,process_of,gngm). % GR 05/26/2017 as per GS - PMID 15634358 Transgressive segregation was significantly associated with allelic variation in the C. briggsae abdominal B homologue, Cb-egl-5
local_semnet_1_GEN(virs,process_of,humn). % GR 06/2015 from DIM
local_semnet_1_GEN(virs,process_of,mamm).
local_semnet_1_GEN(virs,process_of,bird). % GR 06/2015 from DIM
local_semnet_1_GEN(bacs,affects,fngs). % GR 06/2015 as per GS sentence 16228202.ab.3
local_semnet_1_GEN(celf,affects,cell). % GR 06/2015 
local_semnet_1_GEN(celf,affects,bacs). % GR 06/2015 
local_semnet_1_GEN(diap,affects,hlca). % GR 06/2015 
local_semnet_1_GEN(phsf,affects,bpoc). % GR 06/2015 
local_semnet_1_GEN(genf,affects,aapp). % GR 06/2015  06/28/2017 must keep undr observation
local_semnet_1_GEN(neop,affects,bpoc). % GR 06/2015 
local_semnet_1_GEN(orch,affects,clna). % GR 06/2015
local_semnet_1_GEN(cell,affects,orgf). % 05/31/2017 source: GENIA - PMID 27936497 Regulatory T cells (Treg ) are CD4+ CD25++ forkhead box protein 3 (FoxP3+ ) cells that regulate the immune response.
local_semnet_1_GEN(cell,affects,genf). % 06/20/2017 source: GENIA -  T cells regulate genetic transcription. 
%local_semnet_1_GEN(rcpt,affects,genf). % 06/21/2017 source: GENIA -  T cells regulate genetic transcription.  - relation already in the UMLS
local_semnet_1_GEN(imft,affects,genf). % GR as indicated by Halil on 11/09/2017
local_semnet_1_GEN(diap,diagnoses,virs).
local_semnet_1_GEN(lbpr,diagnoses,virs).
local_semnet_1_GEN(mbrt,diagnoses,virs).
local_semnet_1_GEN(medd,diagnoses,virs).
local_semnet_1_GEN(phpr,causes,dsyn).
local_semnet_1_GEN(virs,causes,ortf).
local_semnet_1_GEN(imft,causes,dsyn). % GR as indicated by Halil on 11/09/2017
local_semnet_1_GEN(imft,prevents,virs).
local_semnet_1_GEN(phsu,prevents,virs).
local_semnet_1_GEN(topp,prevents,virs). % GR 06/2015 from DIM
local_semnet_1_GEN(resa,uses,medd).
local_semnet_1_GEN(resa,uses,chvf). % 06/21/2017 source: GS - PMID 8816424_S11 ...as shown by inhibition experiments with specific antagonists
local_semnet_1_GEN(resa,uses,topp). % 05/26/2017 source: GS - PMID 15765262 PA final test trial with no injection assessed final place preference.
local_semnet_1_GEN(resa,uses,mbrt). % 06/23/2017 source: GENIA - transfection experiments
% from Graciela, 5/7/2012
%local_semnet_1_GEN(lbtr,method_of,diap). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(lbtr,method_of,lbpr). % 07/2015 triple also in SN '07
local_semnet_1_GEN(mbrt,method_of,mbrt).
local_semnet_1_GEN(hlca,method_of,topp).
local_semnet_1_GEN(mcha,method_of,mcha).
local_semnet_1_GEN(hlca,method_of,hlca). % GR 06/2015
%local_semnet_1_GEN(diap,uses,phsu). % 07/2015 triple also in SN '06
%local_semnet_1_GEN(diap,uses,medd). % 07/2015 triple also in SN '07
%local_semnet_1_GEN(diap,uses,resd). % 07/2015 triple also in SN '08
local_semnet_1_GEN(diap,uses,irda). % why is this not being triggered?
local_semnet_1_GEN(diap,uses,elii).
local_semnet_1_GEN(diap,uses,lbpr). % GR 05/24/2017
local_semnet_1_GEN(lbpr,uses,phsu).
local_semnet_1_GEN(lbpr,uses,medd). % why is this not being triggered?
local_semnet_1_GEN(lbpr,uses,resd).
local_semnet_1_GEN(lbpr,uses,irda).
local_semnet_1_GEN(lbpr,uses,elii).
local_semnet_1_GEN(mbrt,uses,irda). % GR 06/2015 from SPA domain
local_semnet_1_GEN(mbrt,uses,medd).
%local_semnet_1_GEN(mbrt,uses,resd). % 07/2015 triple also in SN '06
local_semnet_1_GEN(mbrt,uses,lbpr). % GR 06/2015 from SPA domain
local_semnet_1_GEN(mbrt,uses,mnob). % GR 06/2015 from SPA domain
local_semnet_1_GEN(mbrt,uses,mbrt). % GR 06/2015 from SPA domain
local_semnet_1_GEN(medd,uses,medd). % GR 06/2015 from SPA domain
local_semnet_1_GEN(medd,uses,mnob). % GR 06/2015 from SPA domain
local_semnet_1_GEN(medd,uses,phsu). % GR 06/2015 from SPA domain
local_semnet_1_GEN(lbpr,uses,lbpr). % GR 05/24/2017 source: GS
%local_semnet_1_GEN(phsf,follows,phsf). % 07/2015 triple also in SN '06
local_semnet_1_GEN(celf,follows,inpo). % GR 06/2015 imported from SPA domain
local_semnet_1_GEN(phsf,process_of,bpoc). % GR 06/2015
local_semnet_1_GEN(diap,measures,lbtr). % new GR 06/13/2017 as per conversation with Halil
local_semnet_1_GEN(lbpr,measures,lbtr). % new GR 06/13/2017 as per conversation with Halil
local_semnet_1_GEN(lbpr,measures,lbpr). % new GR 06/13/2017 as per conversation with Halil
local_semnet_1_GEN(orch,treats,bact). % new GR 06/13/2017 as per conversation with Halil
local_semnet_1_GEN(phsu,treats,bact). % new GR 06/13/2017 as per conversation with Halil
local_semnet_1_GEN(topp,treats,bact). % new GR 06/13/2017 as per conversation with Halil
local_semnet_1_GEN(phsu,treats,virs). % new GR 06/21/2017 as per comment from Halil
local_semnet_1_GEN(topp,treats,virs). % new GR 06/21/2017 as per comment from Halil
local_semnet_1_GEN(topp,disrupts,inpo). % GR - new 06/26/2017
%local_semnet_1_GEN(,,). 
local_semnet_1_GEN(cell ,replicates,virs). % GR - new 06/14/2017
%local_semnet_1_GEN(,,). 
local_semnet_1_GEN(cell,transcribes,gngm). % GR - new 06/14/2017
local_semnet_1_GEN(cell,transcribes,nnon). % GR - new 06/14/2017
%%%Aurelie's additions
local_semnet_1_GEN(chvf,treats,sosy).
%% AFFECTS
%local_semnet_1_GEN(aapp,affects,dsyn).
%local_semnet_1_GEN(aapp,affects,mobd).
%local_semnet_1_GEN(aapp,affects,neop).
%local_semnet_1_GEN(aapp,affects,patf).
%local_semnet_1_GEN(antb,affects,dsyn).
%local_semnet_1_GEN(antb,affects,mobd).
%local_semnet_1_GEN(antb,affects,neop).
%local_semnet_1_GEN(antb,affects,patf).
local_semnet_1_GEN(chvf,affects,acab).
local_semnet_1_GEN(chvf,affects,anab).
local_semnet_1_GEN(chvf,affects,cgab).
local_semnet_1_GEN(chvf,affects,dsyn).
local_semnet_1_GEN(chvf,affects,inpo).
local_semnet_1_GEN(chvf,affects,mobd).
local_semnet_1_GEN(chvf,affects,neop).
local_semnet_1_GEN(chvf,affects,patf).
local_semnet_1_GEN(chvs,affects,acab).
local_semnet_1_GEN(chvs,affects,anab).
local_semnet_1_GEN(chvs,affects,cgab).
%local_semnet_1_GEN(chvs,affects,dsyn).
local_semnet_1_GEN(chvs,affects,inpo).
%local_semnet_1_GEN(chvs,affects,mobd).
%local_semnet_1_GEN(chvs,affects,neop).
%local_semnet_1_GEN(chvs,affects,patf).
local_semnet_1_GEN(chvf,affects,phsf). % GR 11/03/2017 As per Halil's, as in These agents have no effect on the pharmacokinetics of TIKOSYN
local_semnet_1_GEN(clnd,affects,acab).
local_semnet_1_GEN(clnd,affects,anab).
local_semnet_1_GEN(clnd,affects,cgab).
local_semnet_1_GEN(clnd,affects,dsyn).
local_semnet_1_GEN(clnd,affects,inpo).
local_semnet_1_GEN(clnd,affects,mobd).
local_semnet_1_GEN(clnd,affects,neop).
local_semnet_1_GEN(clnd,affects,patf).
local_semnet_1_GEN(nonn,affects,acab).
local_semnet_1_GEN(nonn,affects,anab).
local_semnet_1_GEN(nonn,affects,cgab).
local_semnet_1_GEN(nonn,affects,dsyn).
local_semnet_1_GEN(nonn,affects,inpo).
local_semnet_1_GEN(nonn,affects,mobd).
local_semnet_1_GEN(nonn,affects,neop).
local_semnet_1_GEN(nonn,affects,patf).
%% CAUSES
local_semnet_1_GEN(chvf,causes,acab).
local_semnet_1_GEN(chvf,causes,anab).
local_semnet_1_GEN(chvf,causes,cgab).
local_semnet_1_GEN(chvf,causes,dsyn).
local_semnet_1_GEN(chvf,causes,inpo).
local_semnet_1_GEN(chvf,causes,mobd).
local_semnet_1_GEN(chvf,causes,neop).
local_semnet_1_GEN(chvf,causes,patf).
local_semnet_1_GEN(eehu,causes,dsyn). % GR 10/19/2017
local_semnet_1_GEN(eehu,causes,inpo). % GR 10/19/2017
local_semnet_1_GEN(eehu,causes,neop). % GR 10/19/2017
local_semnet_1_GEN(eehu,causes,patf). % GR 10/19/2017
local_semnet_1_GEN(eehu,causes,hlca). % GR 10/19/2017 Air pollution triggered hospital admission PMID 22156960
local_semnet_1_GEN(nonn,causes,acab).
local_semnet_1_GEN(nonn,causes,anab).
local_semnet_1_GEN(nonn,causes,cgab).
local_semnet_1_GEN(nonn,causes,dsyn).
local_semnet_1_GEN(nonn,causes,inpo).
local_semnet_1_GEN(nonn,causes,mobd).
local_semnet_1_GEN(nonn,causes,neop).
local_semnet_1_GEN(nonn,causes,patf).
%local_semnet_1_GEN(strd,causes,patf).
%local_semnet_1_GEN(vita,causes,acab).
%local_semnet_1_GEN(vita,causes,anab).
%local_semnet_1_GEN(vita,causes,cgab).
%local_semnet_1_GEN(vita,causes,dsyn).
%local_semnet_1_GEN(vita,causes,inpo).
%local_semnet_1_GEN(vita,causes,mobd).
%local_semnet_1_GEN(vita,causes,neop).
%local_semnet_1_GEN(vita,causes,patf).
%% COMPLICATES
local_semnet_1_GEN(aapp,complicates,acab).
local_semnet_1_GEN(aapp,complicates,anab).
local_semnet_1_GEN(aapp,complicates,cgab).
local_semnet_1_GEN(aapp,complicates,dsyn).
local_semnet_1_GEN(aapp,complicates,inpo).
local_semnet_1_GEN(aapp,complicates,mobd).
local_semnet_1_GEN(aapp,complicates,neop).
local_semnet_1_GEN(aapp,complicates,patf).
local_semnet_1_GEN(eehu,complicates,dsyn). % GR 10/19/2017 Air pollution triggered hospital admission PMID 22156960
local_semnet_1_GEN(eehu,complicates,sosy). % GR 10/19/2017
%local_semnet_1_GEN(horm,complicates,patf).
local_semnet_1_GEN(nonn,complicates,acab).
local_semnet_1_GEN(nonn,complicates,anab).
local_semnet_1_GEN(nonn,complicates,patf).
local_semnet_1_GEN(orch,complicates,acab).
local_semnet_1_GEN(orch,complicates,anab).
local_semnet_1_GEN(orch,complicates,cgab).
local_semnet_1_GEN(orch,complicates,dsyn).
local_semnet_1_GEN(orch,complicates,inpo).
local_semnet_1_GEN(orch,complicates,mobd).
local_semnet_1_GEN(orch,complicates,neop).
local_semnet_1_GEN(orch,complicates,patf).
%local_semnet_1_GEN(phsu,complicates,acab).
%local_semnet_1_GEN(phsu,complicates,anab).
%local_semnet_1_GEN(phsu,complicates,dsyn).
%local_semnet_1_GEN(phsu,complicates,mobd).
%local_semnet_1_GEN(phsu,complicates,patf).
%local_semnet_1_GEN(vita,complicates,anab).
%local_semnet_1_GEN(vita,complicates,cgab).
%local_semnet_1_GEN(vita,complicates,dsyn).
%local_semnet_1_GEN(vita,complicates,inpo).
%local_semnet_1_GEN(vita,complicates,mobd).
%local_semnet_1_GEN(vita,complicates,neop).
%local_semnet_1_GEN(vita,complicates,patf).
%% PREVENTS
local_semnet_1_GEN(aapp,prevents,acab).
local_semnet_1_GEN(aapp,prevents,anab).
local_semnet_1_GEN(aapp,prevents,inpo).
local_semnet_1_GEN(aapp,prevents,mobd).
local_semnet_1_GEN(aapp,prevents,neop).
local_semnet_1_GEN(aapp,prevents,patf).
local_semnet_1_GEN(antb,prevents,acab).
local_semnet_1_GEN(antb,prevents,anab).
local_semnet_1_GEN(antb,prevents,cgab).
local_semnet_1_GEN(antb,prevents,inpo).
local_semnet_1_GEN(chvf,prevents,acab).
local_semnet_1_GEN(chvf,prevents,anab).
local_semnet_1_GEN(chvs,prevents,dsyn).
local_semnet_1_GEN(chvs,prevents,inpo).
local_semnet_1_GEN(chvs,prevents,mobd).
local_semnet_1_GEN(chvs,prevents,neop).
local_semnet_1_GEN(chvs,prevents,patf).
local_semnet_1_GEN(clnd,prevents,acab).
local_semnet_1_GEN(clnd,prevents,anab).
local_semnet_1_GEN(clnd,prevents,cgab).
local_semnet_1_GEN(clnd,prevents,dsyn).
local_semnet_1_GEN(clnd,prevents,inpo).
local_semnet_1_GEN(clnd,prevents,mobd).
local_semnet_1_GEN(clnd,prevents,neop).
local_semnet_1_GEN(clnd,prevents,patf).
local_semnet_1_GEN(hops,prevents,acab).
local_semnet_1_GEN(hops,prevents,anab).
local_semnet_1_GEN(hops,prevents,cgab).
local_semnet_1_GEN(hops,prevents,dsyn).
local_semnet_1_GEN(hops,prevents,inpo).
local_semnet_1_GEN(hops,prevents,mobd).
local_semnet_1_GEN(hops,prevents,neop).
local_semnet_1_GEN(hops,prevents,patf).
local_semnet_1_GEN(horm,prevents,acab).
local_semnet_1_GEN(horm,prevents,anab).
local_semnet_1_GEN(horm,prevents,cgab).
local_semnet_1_GEN(horm,prevents,dsyn).
local_semnet_1_GEN(horm,prevents,inpo).
local_semnet_1_GEN(horm,prevents,mobd).
local_semnet_1_GEN(horm,prevents,neop).
local_semnet_1_GEN(horm,prevents,patf).
local_semnet_1_GEN(nonn,prevents,acab).
local_semnet_1_GEN(nonn,prevents,anab).
local_semnet_1_GEN(nonn,prevents,cgab).
local_semnet_1_GEN(nonn,prevents,dsyn).
local_semnet_1_GEN(nonn,prevents,inpo).
local_semnet_1_GEN(nonn,prevents,mobd).
local_semnet_1_GEN(nonn,prevents,neop).
local_semnet_1_GEN(nonn,prevents,patf).
local_semnet_1_GEN(orch,prevents,acab).
local_semnet_1_GEN(orch,prevents,anab).
local_semnet_1_GEN(orch,prevents,cgab).
local_semnet_1_GEN(orch,prevents,dsyn).
local_semnet_1_GEN(orch,prevents,inpo).
local_semnet_1_GEN(orch,prevents,mobd).
local_semnet_1_GEN(orch,prevents,neop).
local_semnet_1_GEN(orch,prevents,patf).
local_semnet_1_GEN(phsu,prevents,acab).
local_semnet_1_GEN(phsu,prevents,anab).
local_semnet_1_GEN(phsu,prevents,cgab).
local_semnet_1_GEN(phsu,prevents,inpo).
local_semnet_1_GEN(strd,prevents,acab).
local_semnet_1_GEN(strd,prevents,anab).
local_semnet_1_GEN(strd,prevents,cgab).
local_semnet_1_GEN(strd,prevents,dsyn).
local_semnet_1_GEN(strd,prevents,mobd).
local_semnet_1_GEN(strd,prevents,neop).
local_semnet_1_GEN(strd,prevents,patf).
local_semnet_1_GEN(vita,prevents,acab).
local_semnet_1_GEN(vita,prevents,anab).
local_semnet_1_GEN(vita,prevents,cgab).
local_semnet_1_GEN(vita,prevents,inpo).
local_semnet_1_GEN(vita,prevents,mobd).
local_semnet_1_GEN(vita,prevents,neop).
local_semnet_1_GEN(vita,prevents,patf).
%%% TREATS
%local_semnet_1_GEN(aapp,treats,acab). % GR 05/24/2017 as per a call from Marcelo to disallow these that originated in Aurelie's file with changes
%local_semnet_1_GEN(aapp,treats,anab). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(aapp,treats,cgab). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(aapp,treats,dsyn). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(aapp,treats,inpo). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(aapp,treats,mobd). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(aapp,treats,neop). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(aapp,treats,patf). % GR 05/24/2017 as per a call from Marcelo to disallow these
local_semnet_1_GEN(chvf,treats,acab).
local_semnet_1_GEN(chvf,treats,anab).
local_semnet_1_GEN(chvf,treats,cgab).
local_semnet_1_GEN(chvf,treats,dsyn).
local_semnet_1_GEN(chvf,treats,inpo).
local_semnet_1_GEN(chvf,treats,mobd).
local_semnet_1_GEN(chvf,treats,neop).
local_semnet_1_GEN(chvf,treats,patf).
local_semnet_1_GEN(chvs,treats,acab).
local_semnet_1_GEN(chvs,treats,anab).
local_semnet_1_GEN(chvs,treats,cgab).
local_semnet_1_GEN(chvs,treats,inpo).
local_semnet_1_GEN(chvs,treats,mobd).
local_semnet_1_GEN(chvs,treats,neop).
local_semnet_1_GEN(chvs,treats,patf).
local_semnet_1_GEN(clnd,treats,acab).
local_semnet_1_GEN(clnd,treats,anab).
local_semnet_1_GEN(clnd,treats,cgab).
local_semnet_1_GEN(clnd,treats,dsyn).
local_semnet_1_GEN(clnd,treats,inpo).
local_semnet_1_GEN(clnd,treats,mobd).
local_semnet_1_GEN(clnd,treats,neop).
local_semnet_1_GEN(clnd,treats,patf).
%local_semnet_1_GEN(hops,treats,acab). %GR 06/12/2017 as per FP such as glyphosate-TREATS-Encephalomyelitis, Western Equine (PMID 26163579)
%local_semnet_1_GEN(hops,treats,anab). %GR 06/12/2017 as per FP such as glyphosate-TREATS-Encephalomyelitis, Western Equine (PMID 26163579)
%local_semnet_1_GEN(hops,treats,cgab). %GR 06/12/2017 as per FP such as glyphosate-TREATS-Encephalomyelitis, Western Equine (PMID 26163579)
%local_semnet_1_GEN(hops,treats,dsyn). %GR 06/12/2017 as per FP such as glyphosate-TREATS-Encephalomyelitis, Western Equine (PMID 26163579)
%local_semnet_1_GEN(hops,treats,inpo). %GR 06/12/2017 as per FP such as glyphosate-TREATS-Encephalomyelitis, Western Equine (PMID 26163579)
%local_semnet_1_GEN(hops,treats,mobd). %GR 06/12/2017 as per FP such as glyphosate-TREATS-Encephalomyelitis, Western Equine (PMID 26163579)
%local_semnet_1_GEN(hops,treats,patf). %GR 06/12/2017 as per FP such as glyphosate-TREATS-Encephalomyelitis, Western Equine (PMID 26163579)
local_semnet_1_GEN(horm,treats,acab).
local_semnet_1_GEN(horm,treats,anab).
local_semnet_1_GEN(horm,treats,cgab).
local_semnet_1_GEN(horm,treats,inpo).
local_semnet_1_GEN(horm,treats,mobd).
local_semnet_1_GEN(horm,treats,neop).
local_semnet_1_GEN(horm,treats,patf).
%local_semnet_1_GEN(nonn,treats,acab). %GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(nonn,treats,anab). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(nonn,treats,cgab). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(nonn,treats,dsyn). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(nonn,treats,inpo). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(nonn,treats,mobd). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(nonn,treats,neop). % GR 05/24/2017 as per a call from Marcelo to disallow these
%local_semnet_1_GEN(nonn,treats,patf). % GR 05/24/2017 as per a call from Marcelo to disallow these
local_semnet_1_GEN(orch,treats,inpo).
local_semnet_1_GEN(orch,treats,mobd).
local_semnet_1_GEN(orch,treats,neop).
local_semnet_1_GEN(orch,treats,patf).
local_semnet_1_GEN(orch,treats,sosy). % GR - new 09/22/2017 as per François while working on the CONJ module
local_semnet_1_GEN(strd,treats,acab).
local_semnet_1_GEN(strd,treats,anab).
local_semnet_1_GEN(strd,treats,cgab).
local_semnet_1_GEN(strd,treats,dsyn).
local_semnet_1_GEN(strd,treats,inpo).
local_semnet_1_GEN(strd,treats,mobd).
local_semnet_1_GEN(strd,treats,neop).
local_semnet_1_GEN(strd,treats,patf).
local_semnet_1_GEN(vita,treats,acab).
local_semnet_1_GEN(vita,treats,anab).
local_semnet_1_GEN(vita,treats,cgab).
local_semnet_1_GEN(vita,treats,inpo).
local_semnet_1_GEN(vita,treats,mobd).
local_semnet_1_GEN(vita,treats,neop).
local_semnet_1_GEN(vita,treats,patf).

local_semnet_1_GEN(bpoc,location_of,cgab).
local_semnet_1_GEN(bsoj,location_of,cgab).
local_semnet_1_GEN(celf,process_of,bact).
local_semnet_1_GEN(celc,location_of,moft).
