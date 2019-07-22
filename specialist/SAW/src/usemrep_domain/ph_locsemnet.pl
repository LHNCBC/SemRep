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

local_preferred_relation_1_DOM(addresses).
local_preferred_relation_1_DOM(analyzes).
local_preferred_relation_1_DOM(assesses_effect_of).
local_preferred_relation_1_DOM(attribute_of).
local_preferred_relation_1_DOM(characteristic_of).
local_preferred_relation_1_DOM(coordinates_with).
local_preferred_relation_1_DOM(evaluates).
local_preferred_relation_1_DOM(has_access_to).
local_preferred_relation_1_DOM(impediment_to).
local_preferred_relation_1_DOM(measures).
local_preferred_relation_1_DOM(monitors).
local_preferred_relation_1_DOM(promotes).
local_preferred_relation_1_DOM(provides).
local_preferred_relation_1_DOM(serves).
local_preferred_relation_1_DOM(setting_for).
local_preferred_relation_1_DOM(targets).
local_relation_inverse_1_DOM(addresses,addressed_by).
local_relation_inverse_1_DOM(analyzes,analyzed_by).
local_relation_inverse_1_DOM(assesses_effect_of,effect_assessed_by).
local_relation_inverse_1_DOM(attribute_of,has_attribute).
local_relation_inverse_1_DOM(characteristic_of,has_characteristic).
local_relation_inverse_1_DOM(coordinates_with,coordinates_with_r).
local_relation_inverse_1_DOM(evaluates,evaluated_by).
local_relation_inverse_1_DOM(has_access_to,accessed_by).
local_relation_inverse_1_DOM(impediment_to,hindered_by).
local_relation_inverse_1_DOM(measures,measured_by).
local_relation_inverse_1_DOM(monitors,monitored_by).
local_relation_inverse_1_DOM(promotes,promoted_by).
local_relation_inverse_1_DOM(provides,provided_by).
local_relation_inverse_1_DOM(serves,served_by).
local_relation_inverse_1_DOM(setting_for,has_setting).
local_relation_inverse_1_DOM(targets,targeted_by).
local_relation_inverse_1_DOM(addressed_by,addresses).
local_relation_inverse_1_DOM(analyzed_by,analyzes).
local_relation_inverse_1_DOM(effect_assessed_by,assesses_effect_of).
local_relation_inverse_1_DOM(has_attribute,attribute_of).
local_relation_inverse_1_DOM(has_characteristic,characteristic_of).
local_relation_inverse_1_DOM(coordinates_with_r,coordinates_with).
local_relation_inverse_1_DOM(evaluated_by,evaluates).
local_relation_inverse_1_DOM(accessed_by,has_access_to).
local_relation_inverse_1_DOM(hindered_by,impediment_to).
local_relation_inverse_1_DOM(measured_by,measures).
local_relation_inverse_1_DOM(monitored_by,monitors).
local_relation_inverse_1_DOM(promoted_by,promotes).
local_relation_inverse_1_DOM(provided_by,provides).
local_relation_inverse_1_DOM(served_by,serves).
local_relation_inverse_1_DOM(has_setting,setting_for).
local_relation_inverse_1_DOM(targeted_by,targets).
%local_semnet_1_DOM(dora,prevents,dsyn). %MF NHLBI  %GR commented out 12/28/09
local_semnet_1_DOM(hlca,prevents,dsyn). %MF NHLBI
%local_semnet_1_DOM(dora,prevents,fndg). %MF NHLBI  %GR commented out 12/28/09
local_semnet_1_DOM(hlca,prevents,fndg). %MF NHLBI
%local_semnet_1_DOM(dora,prevents,sosy). %MF NHLBI  %GR commented out 12/28/09
local_semnet_1_DOM(hlca,prevents,sosy). %MF NHLBI
%local_semnet_1_DOM(dora,prevents,patf). %MF NHLBI  %GR commented out 12/28/09
local_semnet_1_DOM(hlca,prevents,patf). %MF NHLBI
local_semnet_1_DOM(edac,addresses,comc).
local_semnet_1_DOM(edac,addresses,dsyn).
local_semnet_1_DOM(edac,addresses,fndg).
local_semnet_1_DOM(edac,addresses,grpa).
local_semnet_1_DOM(edac,addresses,inbe).
local_semnet_1_DOM(edac,addresses,menp). % June 2011
local_semnet_1_DOM(edac,addresses,sosy).
local_semnet_1_DOM(gora,addresses,comc).
local_semnet_1_DOM(gora,addresses,dsyn).
local_semnet_1_DOM(gora,addresses,fndg).
local_semnet_1_DOM(gora,addresses,grpa).
local_semnet_1_DOM(gora,addresses,inbe).
local_semnet_1_DOM(gora,addresses,sosy).
local_semnet_1_DOM(hlca,addresses,comc).
local_semnet_1_DOM(hlca,addresses,dsyn).
local_semnet_1_DOM(hlca,addresses,fndg).
local_semnet_1_DOM(hlca,addresses,grpa).
local_semnet_1_DOM(hlca,addresses,inbe).
local_semnet_1_DOM(hlca,addresses,sosy).
%local_semnet_1_DOM(infc,addresses,clna).
local_semnet_1_DOM(infc,addresses,comc).
local_semnet_1_DOM(infc,addresses,dsyn).
%local_semnet_1_DOM(infc,addresses,fndg).
local_semnet_1_DOM(infc,addresses,grpa).
local_semnet_1_DOM(infc,addresses,inbe).
%local_semnet_1_DOM(infc,addresses,sosy).
local_semnet_1_DOM(mnob,addresses,comc).
local_semnet_1_DOM(mnob,addresses,dsyn).
local_semnet_1_DOM(mnob,addresses,fndg).
local_semnet_1_DOM(mnob,addresses,grpa).
local_semnet_1_DOM(mnob,addresses,inbe).
local_semnet_1_DOM(mnob,addresses,sosy).
local_semnet_1_DOM(prog,addresses,clna).
local_semnet_1_DOM(prog,addresses,comc).
local_semnet_1_DOM(prog,addresses,dsyn).
local_semnet_1_DOM(prog,addresses,fndg).
local_semnet_1_DOM(prog,addresses,inbe).
local_semnet_1_DOM(prog,addresses,sosy).
local_semnet_1_DOM(prty,addresses,comc).
local_semnet_1_DOM(prty,addresses,dsyn).
local_semnet_1_DOM(prty,addresses,fndg).
local_semnet_1_DOM(prty,addresses,grpa).
local_semnet_1_DOM(prty,addresses,inbe).
local_semnet_1_DOM(prty,addresses,sosy).
local_semnet_1_DOM(prty,addresses,menp). % June 2011
local_semnet_1_DOM(resa,addresses,dsyn). % June 2011
local_semnet_1_DOM(resa,addresses,sosy). % June 2011
local_semnet_1_DOM(resa,addresses,inbe). % June 2011
local_semnet_1_DOM(resa,addresses,fndg). % June 2011
local_semnet_1_DOM(rnlw,addresses,comc).
local_semnet_1_DOM(rnlw,addresses,dsyn).
local_semnet_1_DOM(rnlw,addresses,fndg).
local_semnet_1_DOM(rnlw,addresses,grpa).
local_semnet_1_DOM(rnlw,addresses,inbe).
local_semnet_1_DOM(rnlw,addresses,sosy).
local_semnet_1_DOM(inbe,addresses,fndg). % under observation
local_semnet_1_DOM(edac,administered_to,popg).
local_semnet_1_DOM(edac,administered_to,prog).
local_semnet_1_DOM(hlca,administered_to,popg).
local_semnet_1_DOM(mnob,administered_to,popg).
local_semnet_1_DOM(mnob,administered_to,prog).
local_semnet_1_DOM(prty,administered_to,popg).
local_semnet_1_DOM(dora,affects,inbe). % 2012
local_semnet_1_DOM(dora,affects,dsyn). % 2012
local_semnet_1_DOM(dora,affects,mobd). % 2012
local_semnet_1_DOM(dsyn,affects,bsoj). %-----June 2011
local_semnet_1_DOM(dsyn,affects,bdsy). %-----June 2011
local_semnet_1_DOM(hlbe,affects,dsyn). % 2012 Role of physical activity on obesity
local_semnet_1_DOM(dsyn,affects,hlbe). % Impact of obesity on health
local_semnet_1_DOM(prty,affects,comc).
local_semnet_1_DOM(prty,affects,dsyn).
local_semnet_1_DOM(prty,affects,hlbe).
local_semnet_1_DOM(prty,affects,inbe).
local_semnet_1_DOM(prty,affects,fndg).
%local_semnet_1_DOM(prty,affects,grpa).
local_semnet_1_DOM(prty,affects,clna).
local_semnet_1_DOM(prty,affects,sosy).
local_semnet_1_DOM(socb,affects,acty). %-----June 2011
local_semnet_1_DOM(socb,affects,fndg). %-----June 2011
local_semnet_1_DOM(socb,affects,resa). %-----June 2011
local_semnet_1_DOM(inbe,affects,dsyn).
local_semnet_1_DOM(inbe,affects,fndg).
local_semnet_1_DOM(inbe,affects,hlbe).
local_semnet_1_DOM(resa,analyzes,acty).
local_semnet_1_DOM(resa,analyzes,fndg).
local_semnet_1_DOM(resa,analyzes,inbe).
local_semnet_1_DOM(resa,analyzes,hlca).
local_semnet_1_DOM(resa,analyzes,prty).
local_semnet_1_DOM(prog,analyzes,acty).
local_semnet_1_DOM(prog,analyzes,fndg).
local_semnet_1_DOM(prog,analyzes,inbe).
local_semnet_1_DOM(prog,analyzes,prty).
local_semnet_1_DOM(infc,assesses_effect_of,hlca).
local_semnet_1_DOM(mnob,assesses_effect_of,hlca).
local_semnet_1_DOM(resa,assesses_effect_of,hlca).
local_semnet_1_DOM(infc,assesses_effect_of,prty).
local_semnet_1_DOM(mnob,assesses_effect_of,prty).
local_semnet_1_DOM(resa,assesses_effect_of,prty).
local_semnet_1_DOM(mnob,assesses_effect_of,shro).
local_semnet_1_DOM(resa,assesses_effect_of,shro).
local_semnet_1_DOM(infc,assesses_effect_of,topp).
local_semnet_1_DOM(resa,assesses_effect_of,topp).
local_semnet_1_DOM(comc,attribute_of,popg).
local_semnet_1_DOM(grpa,attribute_of,popg).
local_semnet_1_DOM(menp,attribute_of,popg).
local_semnet_1_DOM(menp,attribute_of,prog).
local_semnet_1_DOM(socb,attribute_of,prog).
local_semnet_1_DOM(inbe,causes,dsyn).
local_semnet_1_DOM(fndg,causes,fndg).
local_semnet_1_DOM(socb,causes,fndg).
%local_semnet_1_DOM(qlco,characteristic_of,popg).
%local_semnet_1_DOM(qlco,characteristic_of,mnob).
local_semnet_1_DOM(qlco,characteristic_of,prty).
local_semnet_1_DOM(qlco,characteristic_of,hcro).
local_semnet_1_DOM(orgt,coordinates_with,orgt).
local_semnet_1_DOM(orgt,coordinates_with,prog).
local_semnet_1_DOM(orgt,coordinates_with,popg).
local_semnet_1_DOM(prog,coordinates_with,orgt).
local_semnet_1_DOM(prog,coordinates_with,prog).
local_semnet_1_DOM(prog,coordinates_with,popg).
local_semnet_1_DOM(popg,coordinates_with,orgt).
local_semnet_1_DOM(popg,coordinates_with,prog).
local_semnet_1_DOM(popg,coordinates_with,popg).
local_semnet_1_DOM(mnob,evaluates,clna).
local_semnet_1_DOM(mnob,evaluates,comc).
%local_semnet_1_DOM(mnob,evaluates,grpa).
local_semnet_1_DOM(mnob,evaluates,inbe).
local_semnet_1_DOM(mnob,evaluates,infc).
local_semnet_1_DOM(mnob,evaluates,phpr).
local_semnet_1_DOM(mnob,evaluates,prty).
local_semnet_1_DOM(mnob,evaluates,shro).
local_semnet_1_DOM(resa,evaluates,acty).
local_semnet_1_DOM(resa,evaluates,clna).
local_semnet_1_DOM(resa,evaluates,comc).
local_semnet_1_DOM(resa,evaluates,grpa).
local_semnet_1_DOM(resa,evaluates,hlca).
local_semnet_1_DOM(resa,evaluates,inbe).
local_semnet_1_DOM(resa,evaluates,infc).
local_semnet_1_DOM(resa,evaluates,mnob).
local_semnet_1_DOM(resa,evaluates,phpr).
local_semnet_1_DOM(resa,evaluates,prty).
local_semnet_1_DOM(resa,evaluates,shro).
local_semnet_1_DOM(medd,evaluates,clna).
local_semnet_1_DOM(medd,evaluates,humn).
local_semnet_1_DOM(medd,evaluates,popg).
local_semnet_1_DOM(orgt,evaluates,comc).
local_semnet_1_DOM(orgt,evaluates,grpa).
local_semnet_1_DOM(orgt,evaluates,inbe).
local_semnet_1_DOM(orgt,evaluates,infc).
local_semnet_1_DOM(orgt,evaluates,mnob).
local_semnet_1_DOM(orgt,evaluates,phpr).
local_semnet_1_DOM(orgt,evaluates,prty).
local_semnet_1_DOM(prog,evaluates,clna).
local_semnet_1_DOM(prog,evaluates,comc).
local_semnet_1_DOM(prog,evaluates,grpa).
local_semnet_1_DOM(prog,evaluates,inbe).
local_semnet_1_DOM(prog,evaluates,infc).
local_semnet_1_DOM(prog,evaluates,mnob).
local_semnet_1_DOM(prog,evaluates,phpr).
local_semnet_1_DOM(prog,evaluates,prty).
local_semnet_1_DOM(orgt,has_access_to,mnob).
local_semnet_1_DOM(orgt,has_access_to,hlca).
local_semnet_1_DOM(orgt,has_access_to,infc).
local_semnet_1_DOM(popg,has_access_to,diap).
local_semnet_1_DOM(popg,has_access_to,edac).
local_semnet_1_DOM(popg,has_access_to,hlca).
local_semnet_1_DOM(popg,has_access_to,hcro).
local_semnet_1_DOM(popg,has_access_to,infc).
local_semnet_1_DOM(popg,has_access_to,prog).
local_semnet_1_DOM(popg,has_access_to,prty).
local_semnet_1_DOM(popg,has_access_to,shro).
local_semnet_1_DOM(popg,has_access_to,topp).
local_semnet_1_DOM(fndg,impediment_to,hcro).
local_semnet_1_DOM(fndg,impediment_to,hlca).
local_semnet_1_DOM(fndg,impediment_to,hlbe).
local_semnet_1_DOM(fndg,impediment_to,shro).
local_semnet_1_DOM(fndg,impediment_to,topp).
local_semnet_1_DOM(menp,impediment_to,hlca).
local_semnet_1_DOM(menp,impediment_to,hlbe).
local_semnet_1_DOM(menp,impediment_to,topp).
local_semnet_1_DOM(mobd,impediment_to,hlca).
local_semnet_1_DOM(mobd,impediment_to,topp).
local_semnet_1_DOM(socb,impediment_to,hlbe).
local_semnet_1_DOM(socb,impediment_to,hlca).
local_semnet_1_DOM(socb,impediment_to,topp).
local_semnet_1_DOM(patf,impediment_to,hlca).
local_semnet_1_DOM(patf,impediment_to,topp).
local_semnet_1_DOM(geoa,location_of,popg).
local_semnet_1_DOM(orgt,location_of,popg).
local_semnet_1_DOM(diap,measures,clna).
local_semnet_1_DOM(lbpr,measures,clna).
local_semnet_1_DOM(mbrt,measures,clna).
local_semnet_1_DOM(medd,measures,clna).
local_semnet_1_DOM(medd,measures,fndg).
local_semnet_1_DOM(mnob,measures,clna).
local_semnet_1_DOM(mnob,measures,fndg).
local_semnet_1_DOM(resa,measures,fndg).
local_semnet_1_DOM(resa,measures,hlbe).
local_semnet_1_DOM(hlbe,has_method,hlca).
local_semnet_1_DOM(hlca,method_of,prty).
local_semnet_1_DOM(hlbe,has_method,topp).
local_semnet_1_DOM(hlca,method_of,topp).
local_semnet_1_DOM(prty,method_of,topp). % under observation
local_semnet_1_DOM(mnob,monitors,comc).
local_semnet_1_DOM(mnob,monitors,dsyn).
local_semnet_1_DOM(mnob,monitors,fndg).
local_semnet_1_DOM(mnob,monitors,grpa).
local_semnet_1_DOM(mnob,monitors,hlca).
local_semnet_1_DOM(mnob,monitors,inbe).
local_semnet_1_DOM(mnob,monitors,popg).
local_semnet_1_DOM(mnob,monitors,prty).
local_semnet_1_DOM(prog,monitors,comc).
local_semnet_1_DOM(prog,monitors,dsyn).
local_semnet_1_DOM(prog,monitors,fndg).
local_semnet_1_DOM(prog,monitors,grpa).
local_semnet_1_DOM(prog,monitors,hlca).
local_semnet_1_DOM(prog,monitors,inbe).
local_semnet_1_DOM(prog,monitors,phpr).
local_semnet_1_DOM(prog,monitors,popg).
local_semnet_1_DOM(prty,monitors,comc).
local_semnet_1_DOM(prty,monitors,dsyn).
local_semnet_1_DOM(prty,monitors,fndg).
%local_semnet_1_DOM(prty,monitors,grpa).
local_semnet_1_DOM(prty,monitors,hlca).
local_semnet_1_DOM(prty,monitors,inbe).
local_semnet_1_DOM(prty,monitors,popg).
local_semnet_1_DOM(prty,monitors,mnob).
local_semnet_1_DOM(fndg,occurs_in,popg).
local_semnet_1_DOM(fndg,occurs_in,grup).
local_semnet_1_DOM(fndg,occurs_in,aggp).
local_semnet_1_DOM(fndg,occurs_in,famg).
local_semnet_1_DOM(fndg,prevents,hlbe).
local_semnet_1_DOM(prty,prevents,comc).
local_semnet_1_DOM(prty,prevents,orgf).
local_semnet_1_DOM(prty,prevents,dsyn).
local_semnet_1_DOM(prty,prevents,fndg). % June 2011
local_semnet_1_DOM(prty,prevents,sosy).
local_semnet_1_DOM(prty,prevents,patf).
local_semnet_1_DOM(prty,prevents,inbe).
local_semnet_1_DOM(socb,prevents,fndg).
local_semnet_1_DOM(edac,promotes,acty).
local_semnet_1_DOM(edac,promotes,dora).
local_semnet_1_DOM(edac,promotes,hlbe).
local_semnet_1_DOM(edac,promotes,hlca).
local_semnet_1_DOM(edac,promotes,topp).
local_semnet_1_DOM(gora,promotes,acty).
local_semnet_1_DOM(gora,promotes,dora).
local_semnet_1_DOM(gora,promotes,hlbe).
local_semnet_1_DOM(gora,promotes,hlca).
local_semnet_1_DOM(prty,promotes,acty).
local_semnet_1_DOM(prty,promotes,dora).
local_semnet_1_DOM(prty,promotes,hlbe).
local_semnet_1_DOM(prty,promotes,hlca).
local_semnet_1_DOM(prty,promotes,topp).
local_semnet_1_DOM(hlca,promotes,acty).
local_semnet_1_DOM(hlca,promotes,dora).
local_semnet_1_DOM(hlca,promotes,hlbe).
local_semnet_1_DOM(hlca,promotes,hlca).
local_semnet_1_DOM(hlca,promotes,comc).
local_semnet_1_DOM(infc,promotes,hlbe).
local_semnet_1_DOM(infc,promotes,hlca).
local_semnet_1_DOM(prog,promotes,acty).
local_semnet_1_DOM(prog,promotes,dora).
local_semnet_1_DOM(prog,promotes,hlbe).
local_semnet_1_DOM(prog,promotes,hlca).
local_semnet_1_DOM(prog,promotes,topp).
local_semnet_1_DOM(shro,promotes,hlbe).
local_semnet_1_DOM(shro,promotes,hlca).
local_semnet_1_DOM(hcro,provides,prty).
local_semnet_1_DOM(hcro,provides,mnob).
local_semnet_1_DOM(hcro,provides,edac).
local_semnet_1_DOM(hcro,provides,hlca).
local_semnet_1_DOM(hcro,provides,topp).
local_semnet_1_DOM(hcro,provides,ocac). % added 2011
local_semnet_1_DOM(orgt,provides,prty).
local_semnet_1_DOM(orgt,provides,mnob).
local_semnet_1_DOM(orgt,provides,edac).
local_semnet_1_DOM(orgt,provides,hlca).
local_semnet_1_DOM(orgt,provides,topp).
local_semnet_1_DOM(podg,provides,infc). % added 2011
local_semnet_1_DOM(famg,provides,hlca). % added 2011
local_semnet_1_DOM(prog,provides,prty).
local_semnet_1_DOM(prog,provides,mnob).
local_semnet_1_DOM(prog,provides,edac).
local_semnet_1_DOM(prog,provides,hlca).
local_semnet_1_DOM(prog,provides,ocac). % added 2011
local_semnet_1_DOM(prog,provides,topp).
local_semnet_1_DOM(prty,provides,prty).
local_semnet_1_DOM(prty,provides,mnob).
local_semnet_1_DOM(prty,provides,edac).
local_semnet_1_DOM(prty,provides,hlca).
local_semnet_1_DOM(prty,provides,topp).
local_semnet_1_DOM(hcro,serves,popg).
local_semnet_1_DOM(prog,serves,popg).
local_semnet_1_DOM(orgt,serves,popg).
local_semnet_1_DOM(hcro,serves,podg).
local_semnet_1_DOM(prog,serves,podg).
local_semnet_1_DOM(orgt,serves,podg).
local_semnet_1_DOM(hcro,serves,aggp).
local_semnet_1_DOM(prog,serves,aggp).
local_semnet_1_DOM(orgt,serves,aggp).
local_semnet_1_DOM(hcro,serves,geoa).
local_semnet_1_DOM(prog,serves,geoa).
local_semnet_1_DOM(orgt,serves,geoa).
local_semnet_1_DOM(hcro,setting_for,prty).
local_semnet_1_DOM(hcro,setting_for,edac).
local_semnet_1_DOM(hcro,setting_for,acty).
local_semnet_1_DOM(venu,setting_for,edac).
local_semnet_1_DOM(venu,setting_for,hlca).
local_semnet_1_DOM(venu,setting_for,mnob).
local_semnet_1_DOM(venu,setting_for,prty).
local_semnet_1_DOM(edac,targets,aggp).
local_semnet_1_DOM(edac,targets,hlbe).
local_semnet_1_DOM(edac,targets,hlca). % June 2011
local_semnet_1_DOM(edac,targets,popg).
local_semnet_1_DOM(edac,targets,prog).
local_semnet_1_DOM(gora,targets,aggp).
local_semnet_1_DOM(gora,targets,hlbe).
local_semnet_1_DOM(gora,targets,popg).
local_semnet_1_DOM(gora,targets,prog).
local_semnet_1_DOM(gora,targets,aggp).
local_semnet_1_DOM(hlca,targets,hlbe).
local_semnet_1_DOM(hlca,targets,inbe). % June 2011
local_semnet_1_DOM(hlca,targets,popg).
%local_semnet_1_DOM(hlca,targets,prog).
local_semnet_1_DOM(infc,targets,hlbe).
local_semnet_1_DOM(infc,targets,popg).
local_semnet_1_DOM(prty,targets,aggp).
local_semnet_1_DOM(prty,targets,hlbe).
local_semnet_1_DOM(prty,targets,hlca).
local_semnet_1_DOM(prty,targets,popg).
local_semnet_1_DOM(prty,targets,prog).
local_semnet_1_DOM(prty,targets,topp). % June 2011
local_semnet_1_DOM(mnob,targets,hlbe).
local_semnet_1_DOM(mnob,targets,popg).
%local_semnet_1_DOM(mnob,targets,prog).
local_semnet_1_DOM(prog,targets,hlbe).
local_semnet_1_DOM(prog,targets,popg).
local_semnet_1_DOM(resa,targets,aggp).
local_semnet_1_DOM(resa,targets,popg).
local_semnet_1_DOM(rnlw,targets,aggp).
local_semnet_1_DOM(rnlw,targets,hlbe).
local_semnet_1_DOM(rnlw,targets,popg).
local_semnet_1_DOM(rnlw,targets,prog).
local_semnet_1_DOM(rnlw,targets,orgt).
local_semnet_1_DOM(shro,targets,hlbe).
local_semnet_1_DOM(shro,targets,inbe).
local_semnet_1_DOM(hcro,uses,prty).
local_semnet_1_DOM(orgt,uses,prty).
local_semnet_1_DOM(prog,uses,prty).
local_semnet_1_DOM(prty,uses,prty).

