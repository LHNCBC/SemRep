:- module(locsemnet_DOM, [
	   local_preferred_relation_DOM/1,
	   local_relation_inverse_DOM/2,
	   local_semnet_DOM/3
%          local_semnet_1/3 
   ]).

:- load_files( usemrep_lib(module_version), [
		when(compile_time)
   ]).

:- use_module( usemrep_lib( semnet_access ), [
	        preferred_relation/2,
	        relation_inverse/3
   ]).

:- use_module(usemrep_lib(module_version), [
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
%	  format(user_output,'~n~n### ERROR in locsemnet: ~q is neither preferred nor inverse relation.~n~n',
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

local_relation_inverse_1_DOM(alerts,	        alerted_by).
local_relation_inverse_1_DOM(epi_contains,		epi_contained_by).
local_relation_inverse_1_DOM(coordinates_with,	coordinates_with_r).
local_relation_inverse_1_DOM(detects,		detected_by).
local_relation_inverse_1_DOM(funds,			has_funding).
local_relation_inverse_1_DOM(guides,		guided_by).
local_relation_inverse_1_DOM(infects,		infected_by).
local_relation_inverse_1_DOM(informs,		informed_by).
local_relation_inverse_1_DOM(inspects,		inspected_by).
local_relation_inverse_1_DOM(manages,		managed_by).
local_relation_inverse_1_DOM(monitors,		monitored_by).
local_relation_inverse_1_DOM(prevents,		prevented_by).
local_relation_inverse_1_DOM(tests_for,		tested_for_by).
local_relation_inverse_1_DOM(transmits,		transmitted_by).

local_relation_inverse_1_DOM(alerted_by,		alerts).
local_relation_inverse_1_DOM(epi_contained_by,	epi_contains).
local_relation_inverse_1_DOM(coordinates_with_r,	coordinates_with).
local_relation_inverse_1_DOM(detected_by,		detects).
local_relation_inverse_1_DOM(has_funding,		funds).
local_relation_inverse_1_DOM(guided_by,		guides).
local_relation_inverse_1_DOM(infected_by,		infects).
local_relation_inverse_1_DOM(informed_by,		informs).
local_relation_inverse_1_DOM(inspected_by,		inspects).
local_relation_inverse_1_DOM(managed_by,		manages).
local_relation_inverse_1_DOM(monitored_by,		monitors).
local_relation_inverse_1_DOM(prevented_by,		prevents).
local_relation_inverse_1_DOM(tested_for_by,		tests_for).
local_relation_inverse_1_DOM(transmitted_by,	transmits).

local_preferred_relation_1_DOM(alerts).
local_preferred_relation_1_DOM(epi_contains).
local_preferred_relation_1_DOM(coordinates_with).
local_preferred_relation_1_DOM(detects).
local_preferred_relation_1_DOM(funds).
local_preferred_relation_1_DOM(guides).
local_preferred_relation_1_DOM(infects).
local_preferred_relation_1_DOM(informs).
local_preferred_relation_1_DOM(inspects).
local_preferred_relation_1_DOM(manages).
local_preferred_relation_1_DOM(monitors).
local_preferred_relation_1_DOM(prevents).
local_preferred_relation_1_DOM(tests_for).
local_preferred_relation_1_DOM(transmits).
	

% PREVENTS
%local_semnet_1_DOM(dora,  prevents, dsyn). %MF NHLBI
local_semnet_1_DOM(hlca,  prevents, dsyn). %MF NHLBI

%local_semnet_1_DOM(dora,  prevents, fndg). %MF NHLBI
local_semnet_1_DOM(hlca,  prevents, fndg). %MF NHLBI

%local_semnet_1_DOM(dora,  prevents, sosy). %MF NHLBI
local_semnet_1_DOM(hlca,  prevents, sosy). %MF NHLBI

%local_semnet_1_DOM(dora,  prevents, patf). %MF NHLBI
local_semnet_1_DOM(hlca,  prevents, patf). %MF NHLBI

%---------

% Old prevents

local_semnet_1_DOM(vita, prevents, dsyn).
local_semnet_1_DOM(medd, prevents, sosy).
local_semnet_1_DOM(phsu, prevents, sosy).
local_semnet_1_DOM(topp, prevents, sosy).
local_semnet_1_DOM(topp, prevents, inpo). %MF

%%% The following are Graciela's additions for Disaster Preparedness

% ALERTS/ALERTED_BY
local_semnet_1_DOM(orgt, alerts, orgt).
local_semnet_1_DOM(orgt, alerts, prog).
local_semnet_1_DOM(orgt, alerts, popg).
local_semnet_1_DOM(orgt, alerts, famg).
local_semnet_1_DOM(prog, alerts, orgt).
local_semnet_1_DOM(prog, alerts, famg).
local_semnet_1_DOM(prog, alerts, prog).
local_semnet_1_DOM(prog, alerts, popg).
local_semnet_1_DOM(popg, alerts, famg).
local_semnet_1_DOM(popg, alerts, orgt).
local_semnet_1_DOM(popg, alerts, prog).
local_semnet_1_DOM(popg, alerts, popg).
local_semnet_1_DOM(infc, alerts, orgt).
local_semnet_1_DOM(infc, alerts, famg).
local_semnet_1_DOM(infc, alerts, prog).
local_semnet_1_DOM(infc, alerts, popg).
local_semnet_1_DOM(mnob, alerts, famg).
local_semnet_1_DOM(mnob, alerts, orgt).
local_semnet_1_DOM(mnob, alerts, prog).
local_semnet_1_DOM(mnob, alerts, popg).

% CONTAINS/HAS_CONTAINMENT
local_semnet_1_DOM(acty, epi_contains, dsyn).
local_semnet_1_DOM(acty, epi_contains, fndg).
local_semnet_1_DOM(acty, epi_contains, npop).
%local_semnet_1_DOM(acty, epi_contains, qnco).
%local_semnet_1_DOM(acty, epi_contains, qlco).
local_semnet_1_DOM(acty, epi_contains, phpr).
local_semnet_1_DOM(acty, epi_contains, orgf).
local_semnet_1_DOM(acty, epi_contains, virs).
%local_semnet_1_DOM(dora, epi_contains, dsyn). %GR commented out 12/28/09
%local_semnet_1_DOM(dora, epi_contains, fndg).
%local_semnet_1_DOM(dora, epi_contains, npop).
%local_semnet_1_DOM(dora, epi_contains, qnco).
%local_semnet_1_DOM(dora, epi_contains, qlco).
%local_semnet_1_DOM(dora, epi_contains, phpr). %GR commented out 12/28/09
%local_semnet_1_DOM(dora, epi_contains, orgf). %GR commented out 12/28/09
%local_semnet_1_DOM(dora, epi_contains, virs).
%local_semnet_1_DOM(ftcn, epi_contains, dsyn).
%local_semnet_1_DOM(ftcn, epi_contains, fndg).
%local_semnet_1_DOM(ftcn, epi_contains, npop).
%local_semnet_1_DOM(ftcn, epi_contains, qnco). 
%local_semnet_1_DOM(ftcn, epi_contains, qlco).
%local_semnet_1_DOM(ftcn, epi_contains, phpr).
%local_semnet_1_DOM(ftcn, epi_contains, orgf).
%local_semnet_1_DOM(ftcn, epi_contains, virs).
local_semnet_1_DOM(hlca, epi_contains, dsyn).
local_semnet_1_DOM(hlca, epi_contains, fndg).
local_semnet_1_DOM(hlca, epi_contains, npop).
%local_semnet_1_DOM(hlca, epi_contains, qnco).
%local_semnet_1_DOM(hlca, epi_contains, qlco).
local_semnet_1_DOM(hlca, epi_contains, phpr).
local_semnet_1_DOM(hlca, epi_contains, orgf).
local_semnet_1_DOM(hlca, epi_contains, virs).
local_semnet_1_DOM(gora, epi_contains, dsyn).
%local_semnet_1_DOM(gora, epi_contains, fndg).
local_semnet_1_DOM(gora, epi_contains, npop).
%local_semnet_1_DOM(gora, epi_contains, qnco).
%local_semnet_1_DOM(gora, epi_contains, qlco).
local_semnet_1_DOM(gora, epi_contains, phpr).
local_semnet_1_DOM(gora, epi_contains, orgf).
local_semnet_1_DOM(gora, epi_contains, virs).
local_semnet_1_DOM(imft, epi_contains, dsyn).
%local_semnet_1_DOM(imft, epi_contains, fndg).
local_semnet_1_DOM(imft, epi_contains, npop).
%local_semnet_1_DOM(imft, epi_contains, qnco).
%local_semnet_1_DOM(imft, epi_contains, qlco).
local_semnet_1_DOM(imft, epi_contains, phpr).
local_semnet_1_DOM(imft, epi_contains, orgf).
%local_semnet_1_DOM(imft, epi_contains, virs).
%local_semnet_1_DOM(inbe, epi_contains, dsyn).
%local_semnet_1_DOM(inbe, epi_contains, fndg).
local_semnet_1_DOM(inbe, epi_contains, npop).
%local_semnet_1_DOM(inbe, epi_contains, qnco).
%local_semnet_1_DOM(inbe, epi_contains, qlco).
local_semnet_1_DOM(inbe, epi_contains, phpr).
%local_semnet_1_DOM(inbe, epi_contains, orgf).
%local_semnet_1_DOM(inbe, epi_contains, virs).
local_semnet_1_DOM(infc, epi_contains, dsyn).
%local_semnet_1_DOM(infc, epi_contains, fndg).
local_semnet_1_DOM(infc, epi_contains, npop).
%local_semnet_1_DOM(infc, epi_contains, qnco).
%local_semnet_1_DOM(infc, epi_contains, qlco).
local_semnet_1_DOM(infc, epi_contains, phpr).
%local_semnet_1_DOM(infc, epi_contains, orgf).
%local_semnet_1_DOM(infc, epi_contains, virs).
%local_semnet_1_DOM(inpr, epi_contains, dsyn).
%local_semnet_1_DOM(inpr, epi_contains, fndg).
%local_semnet_1_DOM(inpr, epi_contains, npop).
%local_semnet_1_DOM(inpr, epi_contains, qnco).
%local_semnet_1_DOM(inpr, epi_contains, qlco).
%local_semnet_1_DOM(inpr, epi_contains, phpr).
%local_semnet_1_DOM(inpr, epi_contains, orgf).
%local_semnet_1_DOM(inpr, epi_contains, virs).
local_semnet_1_DOM(mnob, epi_contains, dsyn). % After meeting with TCR in Dec'09, we decided to keep it.
%local_semnet_1_DOM(mnob, epi_contains, fndg).
%local_semnet_1_DOM(mnob, epi_contains, npop).
%local_semnet_1_DOM(mnob, epi_contains, qnco).
%local_semnet_1_DOM(mnob, epi_contains, qlco).
local_semnet_1_DOM(mnob, epi_contains, phpr).
%local_semnet_1_DOM(mnob, epi_contains, orgf).
local_semnet_1_DOM(mnob, epi_contains, virs). %After meeting with TCR in Dec'09, we decided to keep it.
local_semnet_1_DOM(orgt, epi_contains, dsyn).
%local_semnet_1_DOM(orgt, epi_contains, fndg).
local_semnet_1_DOM(orgt, epi_contains, npop).
%local_semnet_1_DOM(orgt, epi_contains, qnco).
%local_semnet_1_DOM(orgt, epi_contains, qlco).
local_semnet_1_DOM(orgt, epi_contains, phpr).
local_semnet_1_DOM(orgt, epi_contains, orgf).
local_semnet_1_DOM(orgt, epi_contains, virs).
local_semnet_1_DOM(phsu, epi_contains, dsyn).
local_semnet_1_DOM(phsu, epi_contains, fndg).
local_semnet_1_DOM(phsu, epi_contains, npop).
%local_semnet_1_DOM(phsu, epi_contains, qnco).
%local_semnet_1_DOM(phsu, epi_contains, qlco).
local_semnet_1_DOM(phsu, epi_contains, phpr).
local_semnet_1_DOM(phsu, epi_contains, orgf).
local_semnet_1_DOM(phsu, epi_contains, virs).
%local_semnet_1_DOM(popg, epi_contains, dsyn).
%local_semnet_1_DOM(popg, epi_contains, fndg).
%local_semnet_1_DOM(popg, epi_contains, npop).
%local_semnet_1_DOM(popg, epi_contains, qnco).
%local_semnet_1_DOM(popg, epi_contains, qlco).
%local_semnet_1_DOM(popg, epi_contains, phpr).
%local_semnet_1_DOM(popg, epi_contains, orgf).
%local_semnet_1_DOM(popg, epi_contains, virs).
local_semnet_1_DOM(topp, epi_contains, dsyn).
local_semnet_1_DOM(topp, epi_contains, fndg).
local_semnet_1_DOM(topp, epi_contains, npop).
%local_semnet_1_DOM(topp, epi_contains, qnco).
%local_semnet_1_DOM(topp, epi_contains, qlco).
local_semnet_1_DOM(topp, epi_contains, phpr).
local_semnet_1_DOM(topp, epi_contains, orgf).
local_semnet_1_DOM(topp, epi_contains, virs).
local_semnet_1_DOM(prog, epi_contains, dsyn).  % Scientists CONTAINS Disease outbreaks
local_semnet_1_DOM(prog, epi_contains, fndg).
local_semnet_1_DOM(prog, epi_contains, npop).
%local_semnet_1_DOM(prog, epi_contains, qnco).
%local_semnet_1_DOM(prog, epi_contains, qlco).
local_semnet_1_DOM(prog, epi_contains, phpr).
local_semnet_1_DOM(prog, epi_contains, orgf).
local_semnet_1_DOM(prog, epi_contains, virs).
local_semnet_1_DOM(socb, epi_contains, dsyn).
local_semnet_1_DOM(socb, epi_contains, npop).
local_semnet_1_DOM(socb, epi_contains, phpr).
%local_semnet_1_DOM(socb, epi_contains, virs).

% COORDINATES_WITH/COORDINATES_WITH_R
local_semnet_1_DOM(orgt, coordinates_with, orgt).
local_semnet_1_DOM(orgt, coordinates_with, prog).
local_semnet_1_DOM(orgt, coordinates_with, popg).
local_semnet_1_DOM(orgt, coordinates_with, famg).
local_semnet_1_DOM(prog, coordinates_with, orgt).
local_semnet_1_DOM(prog, coordinates_with, prog).
local_semnet_1_DOM(prog, coordinates_with, popg).
local_semnet_1_DOM(prog, coordinates_with, famg).
local_semnet_1_DOM(popg, coordinates_with, orgt).
local_semnet_1_DOM(popg, coordinates_with, prog).
local_semnet_1_DOM(popg, coordinates_with, popg).
local_semnet_1_DOM(popg, coordinates_with, famg).
local_semnet_1_DOM(famg, coordinates_with, orgt).
local_semnet_1_DOM(famg, coordinates_with, prog).
local_semnet_1_DOM(famg, coordinates_with, popg).
local_semnet_1_DOM(famg, coordinates_with, famg).

% DETECTS/DETECTED_BY
local_semnet_1_DOM(diap, detects, dsyn).
local_semnet_1_DOM(diap, detects, virs).
local_semnet_1_DOM(diap, detects, phpr).
local_semnet_1_DOM(diap, detects, orga).
%local_semnet_1_DOM(diap, detects, imft).
local_semnet_1_DOM(diap, detects, aapp).
%local_semnet_1_DOM(diap, detects, qlco).
local_semnet_1_DOM(diap, detects, orgf).
local_semnet_1_DOM(hlca, detects, dsyn).
local_semnet_1_DOM(hlca, detects, virs).
%local_semnet_1_DOM(hlca, detects, qnco).
local_semnet_1_DOM(hlca, detects, phpr).
local_semnet_1_DOM(hlca, detects, orga).
local_semnet_1_DOM(hlca, detects, npop).
%local_semnet_1_DOM(hlca, detects, imft).
local_semnet_1_DOM(hlca, detects, aapp).
%local_semnet_1_DOM(hlca, detects, qlco).
local_semnet_1_DOM(hlca, detects, orgf).
%local_semnet_1_DOM(hlca, detects, idcn).
local_semnet_1_DOM(hlca, detects, popg).
%local_semnet_1_DOM(hlca, detects, ftcn).
local_semnet_1_DOM(inpr, detects, dsyn).
local_semnet_1_DOM(inpr, detects, virs).
%local_semnet_1_DOM(inpr, detects, qnco).
local_semnet_1_DOM(inpr, detects, phpr).
local_semnet_1_DOM(inpr, detects, orga).
local_semnet_1_DOM(inpr, detects, npop).
%local_semnet_1_DOM(inpr, detects, imft).
local_semnet_1_DOM(inpr, detects, aapp).
%local_semnet_1_DOM(inpr, detects, qlco).
local_semnet_1_DOM(inpr, detects, orgf).
%local_semnet_1_DOM(inpr, detects, idcn).
local_semnet_1_DOM(inpr, detects, popg).
%local_semnet_1_DOM(inpr, detects, ftcn).
local_semnet_1_DOM(lbpr, detects, dsyn).
local_semnet_1_DOM(lbpr, detects, virs).
%local_semnet_1_DOM(lbpr, detects, qnco).
local_semnet_1_DOM(lbpr, detects, phpr).
local_semnet_1_DOM(lbpr, detects, orga).
local_semnet_1_DOM(lbpr, detects, npop).
%local_semnet_1_DOM(lbpr, detects, imft).
local_semnet_1_DOM(lbpr, detects, aapp).
%local_semnet_1_DOM(lbpr, detects, qlco).
local_semnet_1_DOM(lbpr, detects, orgf).
%local_semnet_1_DOM(lbpr, detects, idcn).
local_semnet_1_DOM(lbpr, detects, popg).
%local_semnet_1_DOM(lbpr, detects, ftcn).
local_semnet_1_DOM(mnob, detects, dsyn).
%local_semnet_1_DOM(mnob, detects, virs).
%local_semnet_1_DOM(mnob, detects, qnco).
%local_semnet_1_DOM(mnob, detects, phpr).
local_semnet_1_DOM(mnob, detects, orga).
local_semnet_1_DOM(mnob, detects, npop).
%local_semnet_1_DOM(mnob, detects, imft).
local_semnet_1_DOM(mnob, detects, aapp).
%local_semnet_1_DOM(mnob, detects, qlco).
local_semnet_1_DOM(mnob, detects, orgf).
%local_semnet_1_DOM(mnob, detects, idcn).
local_semnet_1_DOM(mnob, detects, popg).
%local_semnet_1_DOM(mnob, detects, ftcn).
local_semnet_1_DOM(orgt, detects, dsyn).
local_semnet_1_DOM(orgt, detects, virs).
%local_semnet_1_DOM(orgt, detects, qnco).
local_semnet_1_DOM(orgt, detects, phpr).
local_semnet_1_DOM(orgt, detects, orga).
local_semnet_1_DOM(orgt, detects, npop).
%local_semnet_1_DOM(orgt, detects, imft).
local_semnet_1_DOM(orgt, detects, aapp).
%local_semnet_1_DOM(orgt, detects, qlco).
local_semnet_1_DOM(orgt, detects, orgf).
%local_semnet_1_DOM(orgt, detects, idcn).
local_semnet_1_DOM(orgt, detects, popg).
%local_semnet_1_DOM(orgt, detects, ftcn).
local_semnet_1_DOM(prog, detects, dsyn).
local_semnet_1_DOM(prog, detects, virs).
%local_semnet_1_DOM(prog, detects, qnco).
local_semnet_1_DOM(prog, detects, phpr).
local_semnet_1_DOM(prog, detects, orga).
local_semnet_1_DOM(prog, detects, npop).
%local_semnet_1_DOM(prog, detects, imft).
local_semnet_1_DOM(prog, detects, aapp).
%local_semnet_1_DOM(prog, detects, qlco).
local_semnet_1_DOM(prog, detects, orgf).
%local_semnet_1_DOM(prog, detects, idcn).
local_semnet_1_DOM(prog, detects, popg).
%local_semnet_1_DOM(prog, detects, ftcn).
local_semnet_1_DOM(popg, detects, dsyn).
local_semnet_1_DOM(popg, detects, virs).
%local_semnet_1_DOM(popg, detects, qnco).
local_semnet_1_DOM(popg, detects, phpr).
local_semnet_1_DOM(popg, detects, orga).
local_semnet_1_DOM(popg, detects, npop).
%local_semnet_1_DOM(popg, detects, imft).
local_semnet_1_DOM(popg, detects, aapp).
%local_semnet_1_DOM(popg, detects, qlco).
local_semnet_1_DOM(popg, detects, orgf).
%local_semnet_1_DOM(popg, detects, idcn).
local_semnet_1_DOM(popg, detects, popg).
%local_semnet_1_DOM(popg, detects, ftcn).
local_semnet_1_DOM(gora, detects, dsyn).
local_semnet_1_DOM(gora, detects, virs). 
%local_semnet_1_DOM(gora, detects, qnco).
local_semnet_1_DOM(gora, detects, phpr).
%local_semnet_1_DOM(gora, detects, orga).
local_semnet_1_DOM(gora, detects, npop).
%local_semnet_1_DOM(gora, detects, imft).
%local_semnet_1_DOM(gora, detects, aapp).
%local_semnet_1_DOM(gora, detects, qlco).
local_semnet_1_DOM(gora, detects, orgf).
%local_semnet_1_DOM(gora, detects, idcn).
local_semnet_1_DOM(gora, detects, popg).
%local_semnet_1_DOM(gora, detects, ftcn). % Candidate for commenting out
local_semnet_1_DOM(resa, detects, virs). % Dec 09

% Per request from Graciela, by Halil, 11/13/09
local_semnet_1_DOM(mbrt,detects,virs).
local_semnet_1_DOM(medd,detects,virs).
local_semnet_1_DOM(lbpr,detects,bact).
local_semnet_1_DOM(mbrt,detects,bact).
local_semnet_1_DOM(mbrt,detects,bacs).
local_semnet_1_DOM(mbrt,detects,nnon).
local_semnet_1_DOM(mbrt,detects,dsyn). % GR DIM Dec07_09
local_semnet_1_DOM(mbrt,detects,neop). % GR DIM Dec07_09
local_semnet_1_DOM(mbrt,detects,nsba). % GR DIM Dec07_09
local_semnet_1_DOM(mbrt,detects,sosy). % GR DIM Dec07_09

% FUNDS/HAS_FUNDING
local_semnet_1_DOM(orgt, funds, orgt).
%local_semnet_1_DOM(orgt, funds, idcn).
local_semnet_1_DOM(orgt, funds, infc).
local_semnet_1_DOM(orgt, funds, phsu).
local_semnet_1_DOM(orgt, funds, resa).
%local_semnet_1_DOM(orgt, funds, imft).
local_semnet_1_DOM(orgt, funds, acty).
local_semnet_1_DOM(orgt, funds, lbpr).
local_semnet_1_DOM(orgt, funds, hcpp).
%local_semnet_1_DOM(orgt, funds, popg).
local_semnet_1_DOM(orgt, funds, prog).
local_semnet_1_DOM(orgt, funds, mnob).
local_semnet_1_DOM(orgt, funds, medd).
local_semnet_1_DOM(orgt, funds, topp).
local_semnet_1_DOM(orgt, funds, gora).
%local_semnet_1_DOM(inpr, funds, orgt). % Blocked 12/28/09
%local_semnet_1_DOM(inpr, funds, idcn).
%local_semnet_1_DOM(inpr, funds, infc). % Blocked 12/28/09
%local_semnet_1_DOM(inpr, funds, phsu). % Blocked 12/28/09
%local_semnet_1_DOM(inpr, funds, imft).
local_semnet_1_DOM(inpr, funds, acty).
local_semnet_1_DOM(inpr, funds, lbpr).
local_semnet_1_DOM(inpr, funds, hcpp).
%local_semnet_1_DOM(inpr, funds, popg).
local_semnet_1_DOM(inpr, funds, resa).
local_semnet_1_DOM(inpr, funds, prog).
local_semnet_1_DOM(inpr, funds, mnob).
%local_semnet_1_DOM(inpr, funds, medd). % Blocked 12/28/09
local_semnet_1_DOM(inpr, funds, topp).
%local_semnet_1_DOM(inpr, funds, gora). % Blocked 12/28/09
%local_semnet_1_DOM(popg, funds, orgt).
%local_semnet_1_DOM(popg, funds, idcn).
local_semnet_1_DOM(popg, funds, infc).
%local_semnet_1_DOM(popg, funds, phsu). % Blocked 12/28/09
%local_semnet_1_DOM(popg, funds, imft).
local_semnet_1_DOM(popg, funds, acty).
local_semnet_1_DOM(popg, funds, lbpr).
local_semnet_1_DOM(popg, funds, hcpp).
local_semnet_1_DOM(popg, funds, popg).
local_semnet_1_DOM(popg, funds, prog).
local_semnet_1_DOM(popg, funds, mnob).
local_semnet_1_DOM(popg, funds, medd).
local_semnet_1_DOM(popg, funds, topp).
local_semnet_1_DOM(popg, funds, gora).
local_semnet_1_DOM(popg, funds, resa).
local_semnet_1_DOM(prog, funds, resa).
local_semnet_1_DOM(prog, funds, orgt).
%local_semnet_1_DOM(prog, funds, idcn).
local_semnet_1_DOM(prog, funds, infc).
local_semnet_1_DOM(prog, funds, phsu).
%local_semnet_1_DOM(prog, funds, imft).
local_semnet_1_DOM(prog, funds, lbpr).
local_semnet_1_DOM(prog, funds, hcpp).
local_semnet_1_DOM(prog, funds, popg).
local_semnet_1_DOM(prog, funds, prog).
local_semnet_1_DOM(prog, funds, mnob).
local_semnet_1_DOM(prog, funds, medd).
local_semnet_1_DOM(prog, funds, topp).
local_semnet_1_DOM(prog, funds, acty).
local_semnet_1_DOM(prog, funds, gora).
local_semnet_1_DOM(infc, funds, infc).

% GUIDES/GUIDED_BY
local_semnet_1_DOM(orgt, guides, orgt).
local_semnet_1_DOM(orgt, guides, acty).
local_semnet_1_DOM(orgt, guides, prog).
local_semnet_1_DOM(orgt, guides, popg).
local_semnet_1_DOM(orgt, guides, gora).
local_semnet_1_DOM(orgt, guides, inpr).
local_semnet_1_DOM(orgt, guides, socb).
local_semnet_1_DOM(infc, guides, orgt).
local_semnet_1_DOM(infc, guides, acty).
local_semnet_1_DOM(infc, guides, prog).
local_semnet_1_DOM(infc, guides, popg).
local_semnet_1_DOM(infc, guides, gora).
local_semnet_1_DOM(infc, guides, inpr).
local_semnet_1_DOM(infc, guides, socb).
%local_semnet_1_DOM(infc, guides, ftcn).
local_semnet_1_DOM(famg, guides, orgt).
local_semnet_1_DOM(famg, guides, acty).
local_semnet_1_DOM(famg, guides, prog).
local_semnet_1_DOM(famg, guides, popg).
local_semnet_1_DOM(famg, guides, socb).
local_semnet_1_DOM(popg, guides, orgt).
local_semnet_1_DOM(popg, guides, acty).
local_semnet_1_DOM(popg, guides, prog).
local_semnet_1_DOM(popg, guides, popg).
local_semnet_1_DOM(popg, guides, gora).
local_semnet_1_DOM(popg, guides, inpr).
local_semnet_1_DOM(popg, guides, socb).
local_semnet_1_DOM(prog, guides, orgt).
local_semnet_1_DOM(prog, guides, acty).
local_semnet_1_DOM(prog, guides, prog).
local_semnet_1_DOM(prog, guides, popg).
local_semnet_1_DOM(prog, guides, gora).
local_semnet_1_DOM(prog, guides, resa).
local_semnet_1_DOM(prog, guides, inpr).
local_semnet_1_DOM(prog, guides, socb).

% INFECTS/INFECTED_BY
local_semnet_1_DOM(anim, infects, anim).
local_semnet_1_DOM(anim, infects, mamm).
local_semnet_1_DOM(anim, infects, humn).
local_semnet_1_DOM(dsyn, infects, prog).
local_semnet_1_DOM(dsyn, infects, sbst).
local_semnet_1_DOM(dsyn, infects, popg).
local_semnet_1_DOM(dsyn, infects, bird).
local_semnet_1_DOM(dsyn, infects, mamm).
local_semnet_1_DOM(dsyn, infects, anim).
local_semnet_1_DOM(dsyn, infects, humn).
local_semnet_1_DOM(dsyn, infects, geoa).
local_semnet_1_DOM(dsyn, infects, tisu).
local_semnet_1_DOM(dsyn, infects, aggp).
local_semnet_1_DOM(dsyn, infects, bopc).
local_semnet_1_DOM(virs, infects, orgt).
local_semnet_1_DOM(virs, infects, humn).
local_semnet_1_DOM(virs, infects, prog).
local_semnet_1_DOM(virs, infects, popg).
local_semnet_1_DOM(virs, infects, bird).
local_semnet_1_DOM(virs, infects, mamm).
local_semnet_1_DOM(virs, infects, anim).
local_semnet_1_DOM(virs, infects, aggp).
local_semnet_1_DOM(virs, infects, tisu).
local_semnet_1_DOM(virs, infects, sbst).
local_semnet_1_DOM(virs, infects, geoa).
local_semnet_1_DOM(food, infects, prog).
local_semnet_1_DOM(food, infects, popg).
local_semnet_1_DOM(food, infects, bird).
local_semnet_1_DOM(food, infects, mamm).
local_semnet_1_DOM(food, infects, anim).
local_semnet_1_DOM(food, infects, aggp).
local_semnet_1_DOM(food, infects, humn).
local_semnet_1_DOM(popg, infects, prog).
local_semnet_1_DOM(popg, infects, popg).
local_semnet_1_DOM(popg, infects, aggp).
%local_semnet_1_DOM(popg, infects, dsyn). % now reserved for 'transmits'
local_semnet_1_DOM(mamm, infects, mamm).
local_semnet_1_DOM(mamm, infects, anim).
local_semnet_1_DOM(mamm, infects, aggp).
local_semnet_1_DOM(mamm, infects, humn).
local_semnet_1_DOM(humn, infects, humn).  % 08/18/09
local_semnet_1_DOM(humn, infects, famg).  % 08/18/09
local_semnet_1_DOM(mamm, infects, popg).
%local_semnet_1_DOM(mamm, infects, dsyn). % for 'transmits'
local_semnet_1_DOM(bird, infects, popg).
local_semnet_1_DOM(bird, infects, mamm).
local_semnet_1_DOM(bird, infects, anim).
local_semnet_1_DOM(bird, infects, aggp).
local_semnet_1_DOM(bird, infects, humn).
local_semnet_1_DOM(bird, infects, bird).
%local_semnet_1_DOM(bird, infects, dsyn). % for 'transmits'
local_semnet_1_DOM(phpr, infects, geoa).

% INFORMS/INFORMED_BY
local_semnet_1_DOM(orgt, informs, orgt).
local_semnet_1_DOM(orgt, informs, prog).
local_semnet_1_DOM(orgt, informs, popg).
local_semnet_1_DOM(prog, informs, orgt).
local_semnet_1_DOM(prog, informs, prog).
local_semnet_1_DOM(prog, informs, popg).
local_semnet_1_DOM(popg, informs, orgt).
local_semnet_1_DOM(popg, informs, prog).
local_semnet_1_DOM(popg, informs, popg).
local_semnet_1_DOM(infc, informs, orgt).
local_semnet_1_DOM(infc, informs, prog).
local_semnet_1_DOM(infc, informs, popg).

% INSPECTS/INSPECTED_BY
local_semnet_1_DOM(orgt, inspects, infc).
local_semnet_1_DOM(orgt, inspects, sbst).
local_semnet_1_DOM(orgt, inspects, bird).
local_semnet_1_DOM(orgt, inspects, popg).
local_semnet_1_DOM(orgt, inspects, phsu).
local_semnet_1_DOM(orgt, inspects, imft).
local_semnet_1_DOM(orgt, inspects, topp).
local_semnet_1_DOM(orgt, inspects, medd).
local_semnet_1_DOM(orgt, inspects, mnob).
local_semnet_1_DOM(orgt, inspects, lbpr).
local_semnet_1_DOM(prog, inspects, infc).
local_semnet_1_DOM(prog, inspects, sbst).
local_semnet_1_DOM(prog, inspects, bird).
local_semnet_1_DOM(prog, inspects, popg).
local_semnet_1_DOM(prog, inspects, phsu).
local_semnet_1_DOM(prog, inspects, imft).
local_semnet_1_DOM(prog, inspects, topp).
local_semnet_1_DOM(prog, inspects, medd).
local_semnet_1_DOM(prog, inspects, mnob).
local_semnet_1_DOM(prog, inspects, lbpr).
local_semnet_1_DOM(prog, inspects, virs).
%local_semnet_1_DOM(lbpr, inspects, infc).% GR commented out on 12/08/09
%local_semnet_1_DOM(lbpr, inspects, sbst).% GR commented out on 12/08/09
%local_semnet_1_DOM(lbpr, inspects, bird).
%local_semnet_1_DOM(lbpr, inspects, popg).
%local_semnet_1_DOM(lbpr, inspects, phsu).% GR commented out on 12/08/09
local_semnet_1_DOM(lbpr, inspects, imft).
local_semnet_1_DOM(lbpr, inspects, topp).
local_semnet_1_DOM(lbpr, inspects, medd).
local_semnet_1_DOM(lbpr, inspects, mnob).
local_semnet_1_DOM(lbpr, inspects, lbpr).
local_semnet_1_DOM(lbpr, inspects, virs).
local_semnet_1_DOM(popg, inspects, infc).
local_semnet_1_DOM(popg, inspects, sbst).
local_semnet_1_DOM(popg, inspects, bird).
local_semnet_1_DOM(popg, inspects, popg).
local_semnet_1_DOM(popg, inspects, phsu).
%local_semnet_1_DOM(popg, inspects, imft).
local_semnet_1_DOM(popg, inspects, topp).
local_semnet_1_DOM(popg, inspects, medd).
local_semnet_1_DOM(popg, inspects, mnob).
local_semnet_1_DOM(popg, inspects, lbpr).

% MANAGES/MANAGED_BY
local_semnet_1_DOM(orgt, manages, acty).
local_semnet_1_DOM(orgt, manages, orgt).
local_semnet_1_DOM(orgt, manages, prog).
local_semnet_1_DOM(orgt, manages, popg).
local_semnet_1_DOM(orgt, manages, aggp).
local_semnet_1_DOM(orgt, manages, gora).
local_semnet_1_DOM(orgt, manages, mony).
local_semnet_1_DOM(orgt, manages, mnob).
local_semnet_1_DOM(orgt, manages, inpr).
local_semnet_1_DOM(orgt, manages, socb).
local_semnet_1_DOM(orgt, manages, topp).
local_semnet_1_DOM(prog, manages, acty).
local_semnet_1_DOM(prog, manages, orgt).
local_semnet_1_DOM(prog, manages, prog).
local_semnet_1_DOM(prog, manages, popg).
local_semnet_1_DOM(prog, manages, aggp).
local_semnet_1_DOM(prog, manages, gora).
local_semnet_1_DOM(prog, manages, mony).
local_semnet_1_DOM(prog, manages, mnob).
local_semnet_1_DOM(prog, manages, inpr).
local_semnet_1_DOM(prog, manages, socb).
local_semnet_1_DOM(prog, manages, topp).
local_semnet_1_DOM(popg, manages, acty).
local_semnet_1_DOM(popg, manages, orgt).
local_semnet_1_DOM(popg, manages, prog).
local_semnet_1_DOM(popg, manages, popg).
local_semnet_1_DOM(popg, manages, aggp).
local_semnet_1_DOM(popg, manages, gora).
%local_semnet_1_DOM(popg, manages, mony).
local_semnet_1_DOM(popg, manages, mnob).
local_semnet_1_DOM(popg, manages, inpr).
local_semnet_1_DOM(popg, manages, socb).
local_semnet_1_DOM(popg, manages, topp).
local_semnet_1_DOM(famg, manages, acty).
local_semnet_1_DOM(famg, manages, orgt).
local_semnet_1_DOM(famg, manages, prog).
local_semnet_1_DOM(famg, manages, popg).
local_semnet_1_DOM(famg, manages, aggp).
%local_semnet_1_DOM(famg, manages, mony).
local_semnet_1_DOM(famg, manages, mnob).
local_semnet_1_DOM(famg, manages, inpr).
local_semnet_1_DOM(famg, manages, socb).
local_semnet_1_DOM(famg, manages, topp).

% MONITORS/MONITORED_BY
local_semnet_1_DOM(orgt, monitors, dsyn).
local_semnet_1_DOM(orgt, monitors, virs).
%local_semnet_1_DOM(orgt, monitors, qnco).
local_semnet_1_DOM(orgt, monitors, comc).
local_semnet_1_DOM(orgt, monitors, phpr).
local_semnet_1_DOM(orgt, monitors, hlca).
%local_semnet_1_DOM(orgt, monitors, qlco).
%local_semnet_1_DOM(orgt, monitors, ftcn).
%local_semnet_1_DOM(orgt, monitors, idcn).
%local_semnet_1_DOM(orgt, monitors, fndg).
local_semnet_1_DOM(orgt, monitors, bird).
local_semnet_1_DOM(orgt, monitors, orgt).
local_semnet_1_DOM(orgt, monitors, humn).
local_semnet_1_DOM(orgt, monitors, popg).
local_semnet_1_DOM(orgt, monitors, prog).
local_semnet_1_DOM(orgt, monitors, aggp).
local_semnet_1_DOM(orgt, monitors, npop).
local_semnet_1_DOM(orgt, monitors, tisu).
local_semnet_1_DOM(orgt, monitors, anim).
local_semnet_1_DOM(orgt, monitors, lbpr).
local_semnet_1_DOM(orgt, monitors, mamm).
local_semnet_1_DOM(orgt, monitors, acty).
local_semnet_1_DOM(prog, monitors, dsyn).
local_semnet_1_DOM(prog, monitors, virs).
%local_semnet_1_DOM(prog, monitors, qnco).
local_semnet_1_DOM(prog, monitors, comc).
local_semnet_1_DOM(prog, monitors, phpr).
local_semnet_1_DOM(prog, monitors, hlca).
%local_semnet_1_DOM(prog, monitors, qlco).
%local_semnet_1_DOM(prog, monitors, ftcn).
%local_semnet_1_DOM(prog, monitors, idcn).
local_semnet_1_DOM(prog, monitors, fndg).
local_semnet_1_DOM(prog, monitors, bird).
local_semnet_1_DOM(prog, monitors, orgt).
local_semnet_1_DOM(prog, monitors, popg).
local_semnet_1_DOM(prog, monitors, prog).
local_semnet_1_DOM(prog, monitors, aggp).
local_semnet_1_DOM(prog, monitors, npop).
local_semnet_1_DOM(prog, monitors, tisu).
local_semnet_1_DOM(prog, monitors, anim).
local_semnet_1_DOM(prog, monitors, lbpr).
local_semnet_1_DOM(prog, monitors, mamm).
local_semnet_1_DOM(prog, monitors, humn).
local_semnet_1_DOM(prog, monitors, acty).
%local_semnet_1_DOM(popg, monitors, dsyn). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, virs). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, qnco). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, comc). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, phpr). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, hlca). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, qlco). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, ftcn). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, idcn). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, fndg). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, bird). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, orgt). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, popg). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, prog). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, aggp). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, npop). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, tisu). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, anim). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, lbpr). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, mamm). % GR 08/26/09
%local_semnet_1_DOM(popg, monitors, acty). % GR 08/26/09
local_semnet_1_DOM(medd, monitors, dsyn).
local_semnet_1_DOM(medd, monitors, virs).
%local_semnet_1_DOM(medd, monitors, qnco).
local_semnet_1_DOM(medd, monitors, comc).
local_semnet_1_DOM(medd, monitors, phpr).
%local_semnet_1_DOM(medd, monitors, hlca).
%local_semnet_1_DOM(medd, monitors, qlco).
%local_semnet_1_DOM(medd, monitors, ftcn).
%local_semnet_1_DOM(medd, monitors, idcn).
local_semnet_1_DOM(medd, monitors, fndg).
local_semnet_1_DOM(medd, monitors, bird).
%local_semnet_1_DOM(medd, monitors, orgt).
local_semnet_1_DOM(medd, monitors, popg).
local_semnet_1_DOM(medd, monitors, prog).
local_semnet_1_DOM(medd, monitors, aggp).
local_semnet_1_DOM(medd, monitors, npop).
local_semnet_1_DOM(medd, monitors, tisu).
local_semnet_1_DOM(medd, monitors, anim).
local_semnet_1_DOM(medd, monitors, humn).
local_semnet_1_DOM(medd, monitors, lbpr).
local_semnet_1_DOM(medd, monitors, mamm).
local_semnet_1_DOM(mnob, monitors, dsyn).
local_semnet_1_DOM(mnob, monitors, virs).
%local_semnet_1_DOM(mnob, monitors, qnco).
local_semnet_1_DOM(mnob, monitors, comc).
local_semnet_1_DOM(mnob, monitors, phpr).
%local_semnet_1_DOM(mnob, monitors, hlca).
%local_semnet_1_DOM(mnob, monitors, qlco).
%local_semnet_1_DOM(mnob, monitors, ftcn).
%local_semnet_1_DOM(mnob, monitors, idcn).
local_semnet_1_DOM(mnob, monitors, fndg).
local_semnet_1_DOM(mnob, monitors, bird).
local_semnet_1_DOM(mnob, monitors, orgt).
local_semnet_1_DOM(mnob, monitors, popg).
local_semnet_1_DOM(mnob, monitors, prog).
local_semnet_1_DOM(mnob, monitors, aggp).
local_semnet_1_DOM(mnob, monitors, npop).
local_semnet_1_DOM(mnob, monitors, tisu).
local_semnet_1_DOM(mnob, monitors, anim).
local_semnet_1_DOM(mnob, monitors, humn).
local_semnet_1_DOM(mnob, monitors, lbpr).
local_semnet_1_DOM(mnob, monitors, mamm).
local_semnet_1_DOM(acty, monitors, dsyn).
local_semnet_1_DOM(acty, monitors, virs).
%local_semnet_1_DOM(acty, monitors, qnco).
local_semnet_1_DOM(acty, monitors, comc).
local_semnet_1_DOM(acty, monitors, phpr).
local_semnet_1_DOM(acty, monitors, hlca).
%local_semnet_1_DOM(acty, monitors, qlco).
%local_semnet_1_DOM(acty, monitors, ftcn).
%local_semnet_1_DOM(acty, monitors, idcn).
local_semnet_1_DOM(acty, monitors, fndg).
local_semnet_1_DOM(acty, monitors, bird).
%local_semnet_1_DOM(acty, monitors, orgt).
%local_semnet_1_DOM(acty, monitors, popg).
%local_semnet_1_DOM(acty, monitors, prog).
%local_semnet_1_DOM(acty, monitors, aggp).
local_semnet_1_DOM(acty, monitors, npop).
local_semnet_1_DOM(acty, monitors, tisu).
%local_semnet_1_DOM(acty, monitors, anim).
local_semnet_1_DOM(acty, monitors, lbpr).
%local_semnet_1_DOM(acty, monitors, mamm).
%local_semnet_1_DOM(acty, monitors, humn).
local_semnet_1_DOM(famg, monitors, sosy).
local_semnet_1_DOM(famg, monitors, dsyn).

% TESTS_FOR/TESTED_FOR_BY
local_semnet_1_DOM(resa, tests_for, orga).
local_semnet_1_DOM(resa, tests_for, patf).
local_semnet_1_DOM(resa, tests_for, virs).
local_semnet_1_DOM(resa, tests_for, dsyn).
local_semnet_1_DOM(orgt, tests_for, virs).
local_semnet_1_DOM(orgt, tests_for, dsyn).
local_semnet_1_DOM(orgt, tests_for, patf).
local_semnet_1_DOM(orgt, tests_for, orga).
local_semnet_1_DOM(prog, tests_for, virs).
local_semnet_1_DOM(prog, tests_for, dsyn).
local_semnet_1_DOM(prog, tests_for, patf).
local_semnet_1_DOM(prog, tests_for, orga).
local_semnet_1_DOM(hlca, tests_for, virs).
local_semnet_1_DOM(hlca, tests_for, dsyn).
local_semnet_1_DOM(hlca, tests_for, patf).
local_semnet_1_DOM(hlca, tests_for, orga).
local_semnet_1_DOM(lbpr, tests_for, virs).
local_semnet_1_DOM(lbpr, tests_for, dsyn).
local_semnet_1_DOM(lbpr, tests_for, patf).
local_semnet_1_DOM(lbpr, tests_for, orga).
local_semnet_1_DOM(lbpr, tests_for, orga).

% PREVENTS/PREVENTED_BY

local_semnet_1_DOM(clnd, prevents, dsyn).
local_semnet_1_DOM(clnd, prevents, virs).
local_semnet_1_DOM(socb, prevents, dsyn).
%local_semnet_1_DOM(socb, prevents, npop).
local_semnet_1_DOM(socb, prevents, virs).
local_semnet_1_DOM(imft, prevents, virs). % GR DIM Dec07_09
local_semnet_1_DOM(inbe, prevents, comc).
local_semnet_1_DOM(inbe, prevents, dsyn).
local_semnet_1_DOM(inbe, prevents, sosy).
local_semnet_1_DOM(inbe, prevents, virs).
local_semnet_1_DOM(orgt, prevents, phpr).
local_semnet_1_DOM(orgt, prevents, dsyn).
local_semnet_1_DOM(orgt, prevents, sosy).
local_semnet_1_DOM(orgt, prevents, virs).
%local_semnet_1_DOM(orgt, prevents, qnco).
local_semnet_1_DOM(orgt, prevents, comc).
local_semnet_1_DOM(prog, prevents, phpr).
local_semnet_1_DOM(prog, prevents, dsyn).
local_semnet_1_DOM(prog, prevents, sosy).
local_semnet_1_DOM(prog, prevents, virs).
%local_semnet_1_DOM(prog, prevents, qnco).
local_semnet_1_DOM(prog, prevents, comc).
local_semnet_1_DOM(popg, prevents, phpr).
local_semnet_1_DOM(popg, prevents, dsyn).
local_semnet_1_DOM(popg, prevents, sosy).
local_semnet_1_DOM(popg, prevents, virs).
%local_semnet_1_DOM(popg, prevents, qnco).
local_semnet_1_DOM(popg, prevents, comc).
local_semnet_1_DOM(topp, prevents, phpr).
local_semnet_1_DOM(topp, prevents, orgf).
local_semnet_1_DOM(topp, prevents, dsyn).
local_semnet_1_DOM(topp, prevents, sosy).
local_semnet_1_DOM(topp, prevents, virs).
%%local_semnet_1_DOM(topp, prevents, qnco).
local_semnet_1_DOM(topp, prevents, comc).
local_semnet_1_DOM(hlca, prevents, phpr).
local_semnet_1_DOM(hlca, prevents, dsyn).
local_semnet_1_DOM(hlca, prevents, sosy).
local_semnet_1_DOM(hlca, prevents, virs).
%local_semnet_1_DOM(hlca, prevents, qnco).
local_semnet_1_DOM(hlca, prevents, comc).
local_semnet_1_DOM(hlca, prevents, orgf).
local_semnet_1_DOM(acty, prevents, phpr).
local_semnet_1_DOM(acty, prevents, dsyn).
local_semnet_1_DOM(acty, prevents, sosy).
local_semnet_1_DOM(acty, prevents, virs).
local_semnet_1_DOM(acty, prevents, comc).
local_semnet_1_DOM(gora, prevents, phpr).
local_semnet_1_DOM(gora, prevents, dsyn).
local_semnet_1_DOM(gora, prevents, sosy).
local_semnet_1_DOM(gora, prevents, virs).
%local_semnet_1_DOM(gora, prevents, qnco).
local_semnet_1_DOM(gora, prevents, comc).
local_semnet_1_DOM(phsu, prevents, virs).  % GR DIM Dec07_09
local_semnet_1_DOM(rnlw, prevents, phpr).
%local_semnet_1_DOM(rnlw, prevents, dsyn).
%local_semnet_1_DOM(rnlw, prevents, virs).
%local_semnet_1_DOM(rnlw, prevents, qnco).
local_semnet_1_DOM(rnlw, prevents, comc).

% TRANSMITS/TRANSMITTED_BY
local_semnet_1_DOM(popg, transmits, dsyn). % for 'transmits infection'
local_semnet_1_DOM(humn, transmits, dsyn).  % 08/28/09
local_semnet_1_DOM(mamm, transmits, dsyn).
local_semnet_1_DOM(bird, transmits, dsyn).
local_semnet_1_DOM(bdsu, transmits, dsyn).
local_semnet_1_DOM(mamm, transmits, virs). 
local_semnet_1_DOM(humn, transmits, virs).
local_semnet_1_DOM(popg, transmits, virs).
local_semnet_1_DOM(bird, transmits, virs).
local_semnet_1_DOM(bdsu, transmits, virs).
local_semnet_1_DOM(mamm, transmits, bact). 
local_semnet_1_DOM(humn, transmits, bact).
local_semnet_1_DOM(popg, transmits, bact).
local_semnet_1_DOM(bird, transmits, bact).
local_semnet_1_DOM(food, transmits, bact). 
local_semnet_1_DOM(sbst, transmits, bact).
local_semnet_1_DOM(inch, transmits, bact).
