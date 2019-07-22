% ----- Module Declaration and Exported Predicates for Disaster Preparedness

:- module(semrules_DOM,	[
   word_corresponds_to_semnet_relation_DOM/4,
   multiphrase_corresponds_to_semnet_relation_DOM/6,
   phrase_corresponds_to_semnet_relation_DOM/6
]).

multiphrase_corresponds_to_semnet_relation_DOM(_, _, _, _, _, _) :- !, fail.

phrase_corresponds_to_semnet_relation_DOM(_, _, _, _, _, _) :- !, fail.

% ----- Source Code Control System

% Set of current semantic types, many not in UMLS Semantic Network
% Needs to be manually updated
% word_corresponds_to_semnet_relation_DOM( ?Word, ?POS, ?Cue, ?Relation )
word_corresponds_to_semnet_relation_DOM(be,aux,_,unspecified_relation).    %  !!!!!!! - tcr

% ----- ALERTS
word_corresponds_to_semnet_relation_DOM(alert,     verb, _, alerts).
word_corresponds_to_semnet_relation_DOM(caution,   verb, _, alerts).
word_corresponds_to_semnet_relation_DOM(warn,      verb, _, alerts).

% ----- CONTAINS   
word_corresponds_to_semnet_relation_DOM(check,        verb, _, epi_contains).
word_corresponds_to_semnet_relation_DOM(contain,      verb, _, epi_contains).
%word_corresponds_to_semnet_relation_DOM(containment,  noun, of, epi_contains). % NOM remove
word_corresponds_to_semnet_relation_DOM(containment,  noun, _, epi_contains). % NOM add
word_corresponds_to_semnet_relation_DOM(control,      verb, _, epi_contains).
%word_corresponds_to_semnet_relation_DOM(control,      noun, of, epi_contains). % NOM remove
word_corresponds_to_semnet_relation_DOM(control,      noun, _, epi_contains). % NOM add
word_corresponds_to_semnet_relation_DOM(curb,         verb, _, epi_contains).
word_corresponds_to_semnet_relation_DOM(curtail,      verb, _, epi_contains).
word_corresponds_to_semnet_relation_DOM(delimit,      verb, _, epi_contains).
word_corresponds_to_semnet_relation_DOM(effective,     adj, against, epi_contains).
word_corresponds_to_semnet_relation_DOM(halt,         verb, _, epi_contains).
word_corresponds_to_semnet_relation_DOM(limit,        verb, _, epi_contains). % Added 12/08/09
word_corresponds_to_semnet_relation_DOM(quarantine,   verb, _, epi_contains). % not sure this belongs here
word_corresponds_to_semnet_relation_DOM(quarantine,   noun, _, epi_contains). % does this belong here? % NOM untouched
word_corresponds_to_semnet_relation_DOM(secure,       verb, _, epi_contains).
%word_corresponds_to_semnet_relation_DOM(mod_head,        _, _, epi_contains). % 08/18/09  Commented out on 12/08/09 - FPs

% ----- COORDINATES
word_corresponds_to_semnet_relation_DOM(collaborate,   verb, with, coordinates_with).
word_corresponds_to_semnet_relation_DOM(collaboration, noun, with, coordinates_with). % NOM untouched
word_corresponds_to_semnet_relation_DOM(coordinate,    verb, with, coordinates_with).
word_corresponds_to_semnet_relation_DOM(partner,       verb, with, coordinates_with).
word_corresponds_to_semnet_relation_DOM(partnership,   noun, with, coordinates_with). % NOM untouched
word_corresponds_to_semnet_relation_DOM(work,          verb, with, coordinates_with).

% ----- DETECTS
%word_corresponds_to_semnet_relation_DOM(confirmation, noun,of-by, detected_by). % Dec 09 % NOM remove
%word_corresponds_to_semnet_relation_DOM(confirmation,	noun,of,detects). % 12/09/09 % NOM remove
word_corresponds_to_semnet_relation_DOM(confirmation,	noun,_,detects). % 12/09/09 % NOM add
word_corresponds_to_semnet_relation_DOM(detect,    	verb, _,  detects).
%word_corresponds_to_semnet_relation_DOM(detection, 	noun, of, detects). % NOM remove
word_corresponds_to_semnet_relation_DOM(detection, 	noun, _, detects). % NOM add
word_corresponds_to_semnet_relation_DOM(identify,  	verb, _,  detects). % GR 12/08/09
word_corresponds_to_semnet_relation_DOM(register,  	verb, _,  detects).
word_corresponds_to_semnet_relation_DOM(uncover,   	verb, _,  detects).

% -------FUNDS
word_corresponds_to_semnet_relation_DOM(allocate,   verb, _,  funds).
%word_corresponds_to_semnet_relation_DOM(allocation, noun, of, funds). % NOM remove
word_corresponds_to_semnet_relation_DOM(allocation, noun, _, funds). % NOM add
word_corresponds_to_semnet_relation_DOM(award,      verb, for,funds).
word_corresponds_to_semnet_relation_DOM(back,    	verb, up, funds).
word_corresponds_to_semnet_relation_DOM(budget,     verb, for, funds).
%word_corresponds_to_semnet_relation_DOM(budgeting,  noun, for, funds). % NOM remove
word_corresponds_to_semnet_relation_DOM(budgeting,  noun, _, funds). % NOM add
word_corresponds_to_semnet_relation_DOM(contribute, verb, _,  funds).
%word_corresponds_to_semnet_relation_DOM(contribution, noun, of, funds). % NOM remove
word_corresponds_to_semnet_relation_DOM(contribution, noun, _, funds). % NOM add
%word_corresponds_to_semnet_relation_DOM(commit,     verb, _,  funds). % Must be deleted. Too many FPs
%word_corresponds_to_semnet_relation_DOM(distribute, verb, _,  funds). % GR Too many FP's
%word_corresponds_to_semnet_relation_DOM(distribution, noun, of, funds). % GR Too many FP's - commented out on 12/08/09
word_corresponds_to_semnet_relation_DOM(fund,       verb, _,  funds).
word_corresponds_to_semnet_relation_DOM(funding,    noun, from, funded_by). % NOM untouched
word_corresponds_to_semnet_relation_DOM(funding,    noun, for, funds). % NOM untouched
word_corresponds_to_semnet_relation_DOM(invest,     verb, _,  funds).
word_corresponds_to_semnet_relation_DOM(invest,     verb, in, funds).
%word_corresponds_to_semnet_relation_DOM(investment, noun, in, funds). % NOM remove
%word_corresponds_to_semnet_relation_DOM(investment, noun, of, funds). % NOM remove
word_corresponds_to_semnet_relation_DOM(investment, noun, _, funds).
word_corresponds_to_semnet_relation_DOM(promote, 	verb, _,  funds).
word_corresponds_to_semnet_relation_DOM(sponsor, 	verb, _,  funds).
word_corresponds_to_semnet_relation_DOM(sponsored, 	adj, _,  funds). % GR added on 12/08/09
word_corresponds_to_semnet_relation_DOM(sponsored, 	verb, _,  funds). % GR added on 12/08/09
word_corresponds_to_semnet_relation_DOM(support, 	verb, _,  funds).

% ----- GUIDES
word_corresponds_to_semnet_relation_DOM(advise,     verb, _, guides).
word_corresponds_to_semnet_relation_DOM(direct,     verb, _, guides).
word_corresponds_to_semnet_relation_DOM(encourage,  verb, _, guides).
word_corresponds_to_semnet_relation_DOM(guide,      verb, _, guides).
%word_corresponds_to_semnet_relation_DOM(lead,       verb, _, guides). % Each time 'led to' appears it is pegged to GUIDES as opposed to CAUSES
word_corresponds_to_semnet_relation_DOM(orient,     verb, _, guides).
word_corresponds_to_semnet_relation_DOM(plan,       verb, for, guides).
word_corresponds_to_semnet_relation_DOM(recommend,  verb, _, guides).
word_corresponds_to_semnet_relation_DOM(strengthen, verb, _, guides).

% ----- INFECTS
word_corresponds_to_semnet_relation_DOM('co-infect',     verb, with, infected_by).
word_corresponds_to_semnet_relation_DOM('co-infect',     verb,   _,  infects).
word_corresponds_to_semnet_relation_DOM('co-infected',    adj, with, infected_by).  % added 12/08/08
word_corresponds_to_semnet_relation_DOM('co-infected',   verb, with, infected_by).  % added 12/08/08
%word_corresponds_to_semnet_relation_DOM('co-infection',  noun, of, infects). % NOM remove
word_corresponds_to_semnet_relation_DOM('co-infection',  noun, with, infected_by).
word_corresponds_to_semnet_relation_DOM('co-infection',  noun, _, infects). % NOM add
%word_corresponds_to_semnet_relation_DOM(contagion,     noun, of, infects). % NOM remove
word_corresponds_to_semnet_relation_DOM(contagion,     noun, _, infects).
word_corresponds_to_semnet_relation_DOM(contaminate,   verb, _,  infects).
%word_corresponds_to_semnet_relation_DOM(contamination, noun, of, infects). % NOM remove
%word_corresponds_to_semnet_relation_DOM(contamination, noun, with, infected_by). % NOM remove
word_corresponds_to_semnet_relation_DOM(contamination, noun, _, infects). % NOM add
word_corresponds_to_semnet_relation_DOM(contract,	   verb, _,  infected_by).
word_corresponds_to_semnet_relation_DOM(decimate,      verb, _,  infects).
%word_corresponds_to_semnet_relation_DOM(decimation,    noun, of, infects). % NOM remove
word_corresponds_to_semnet_relation_DOM(decimation,    noun, _, infects). % NOM add
word_corresponds_to_semnet_relation_DOM(devastate,     verb, _,  infects).
%word_corresponds_to_semnet_relation_DOM(devastation,   noun, of, infects). % NOM remove
word_corresponds_to_semnet_relation_DOM(devastation,   noun, _, infects). % NOM add
word_corresponds_to_semnet_relation_DOM(infect,        verb, with, infected_by).
word_corresponds_to_semnet_relation_DOM(infect,        verb, _,  infects).
word_corresponds_to_semnet_relation_DOM(infected,      adj,  with, infected_by).  % added 12/08/08
word_corresponds_to_semnet_relation_DOM(infected,      verb, with, infected_by).  % added 12/08/08
%word_corresponds_to_semnet_relation_DOM(infection,     noun, of, infects). % NOM remove
%word_corresponds_to_semnet_relation_DOM(infection,     noun, by, infected_by). % NOM remove
word_corresponds_to_semnet_relation_DOM(infection,     noun, with, infected_by).
word_corresponds_to_semnet_relation_DOM(infection,     noun, _, infects). % NOM add
word_corresponds_to_semnet_relation_DOM(harm,          verb, _,  infects).
word_corresponds_to_semnet_relation_DOM(hurt,          verb, _,  infects).
word_corresponds_to_semnet_relation_DOM(pass,          verb, on, infects). 
word_corresponds_to_semnet_relation_DOM(pollute,       verb, _,  infects).
word_corresponds_to_semnet_relation_DOM(ravage,        verb, _,  infects).
word_corresponds_to_semnet_relation_DOM(shed,          verb, _,  infects).
word_corresponds_to_semnet_relation_DOM(spill,         verb, into, infects).
word_corresponds_to_semnet_relation_DOM(spillage,      noun, into, 	  infects). % NOM untouched
word_corresponds_to_semnet_relation_DOM(spread,        verb,   among, infects).  % added 12/08/08
word_corresponds_to_semnet_relation_DOM(spread,        verb, between, infects).
word_corresponds_to_semnet_relation_DOM(spread,        verb, through, infects).
word_corresponds_to_semnet_relation_DOM(spread,        verb,       to, infects).
%word_corresponds_to_semnet_relation_DOM(spread,        noun, of-among, infects). % added 12/08/08 % NOM remove
%word_corresponds_to_semnet_relation_DOM(spread,        noun, of, infects). % NOM remove
word_corresponds_to_semnet_relation_DOM(spread,        noun, among, infects). % added 12/08/08 % NOM add
word_corresponds_to_semnet_relation_DOM(spread,        noun, _, infects). % added 12/08/08 % NOM add


% ----- INFORMS
word_corresponds_to_semnet_relation_DOM(inform,  verb, _, informs).
word_corresponds_to_semnet_relation_DOM(notify,  verb, _, informs).

% ----- INSPECTS
%word_corresponds_to_semnet_relation_DOM(examination, noun, of, inspects). % NOM remove
word_corresponds_to_semnet_relation_DOM(examination, noun, _, inspects). % NOM add
word_corresponds_to_semnet_relation_DOM(examine,     verb, _,  inspects).
word_corresponds_to_semnet_relation_DOM(inspect,     verb, _,  inspects).
%word_corresponds_to_semnet_relation_DOM(inspection,  noun, of, inspects). % NOM remove
word_corresponds_to_semnet_relation_DOM(inspection,  noun, _, inspects). % NOM add
word_corresponds_to_semnet_relation_DOM(screen,      verb, _,  inspects).
%word_corresponds_to_semnet_relation_DOM(screening,   noun, of, inspects). % NOM remove
word_corresponds_to_semnet_relation_DOM(screening,   noun, _, inspects). % NOM add
word_corresponds_to_semnet_relation_DOM(test,        verb, _,  inspects).

% ----- MANAGES / MANAGED BY
word_corresponds_to_semnet_relation_DOM(coordinate,  verb, _,  manages).
word_corresponds_to_semnet_relation_DOM(manage,      verb, _,  manages).
%word_corresponds_to_semnet_relation_DOM(management,  noun, of, manages). % NOM remove
word_corresponds_to_semnet_relation_DOM(management,  noun, _, manages). % NOM add
word_corresponds_to_semnet_relation_DOM(operate,	 verb, under, managed_by).
word_corresponds_to_semnet_relation_DOM(oversee,     verb, _,  manages).
word_corresponds_to_semnet_relation_DOM(regulate,    verb, _,  manages).
%word_corresponds_to_semnet_relation_DOM(regulation,  noun, of, manages). % NOM remove
word_corresponds_to_semnet_relation_DOM(regulation,  noun, _, manages). % NOM add
word_corresponds_to_semnet_relation_DOM(supervise,   verb, _,  manages).
%word_corresponds_to_semnet_relation_DOM(supervision, noun, of, manages). % NOM remove
word_corresponds_to_semnet_relation_DOM(supervision, noun, _, manages). % NOM add

% ----- MONITORS
word_corresponds_to_semnet_relation_DOM('conduct surveillance', verb, _,  monitors). % multi-word 
word_corresponds_to_semnet_relation_DOM(monitor,                verb, _,  monitors).
%word_corresponds_to_semnet_relation_DOM(monitoring,  	noun, of-by,  monitored_by). % Added 12/08/09 % NOM remove
word_corresponds_to_semnet_relation_DOM(monitoring,  	noun, _,  monitors). % Added 12/08/09 % NOM add
word_corresponds_to_semnet_relation_DOM(survey,                 verb, _,  monitors).
word_corresponds_to_semnet_relation_DOM(surveillance,           noun, for, monitors). % Added on 12/08/09 % NOM untouched
%word_corresponds_to_semnet_relation_DOM(surveillance,           noun, of, monitors). % NOM remove
word_corresponds_to_semnet_relation_DOM(surveillance,           noun, _, monitors). % NOM add

% ----- PREVENTS
word_corresponds_to_semnet_relation_DOM(avert,	  verb,       _, prevents).
word_corresponds_to_semnet_relation_DOM(forestall,    verb,       _, prevents). 
%word_corresponds_to_semnet_relation_DOM(immunization, noun, against, prevents). % NOM remove
word_corresponds_to_semnet_relation_DOM(immunization, noun, _, prevents). % NOM add
word_corresponds_to_semnet_relation_DOM(immunize,     verb, against, prevents).
word_corresponds_to_semnet_relation_DOM(inoculate,    verb, against, prevents).
word_corresponds_to_semnet_relation_DOM(prevent,      verb,       _, prevents).
word_corresponds_to_semnet_relation_DOM(prevention,   noun,       _, prevents). % NOM untouched
%word_corresponds_to_semnet_relation_DOM(prevention,   noun,      of, prevents).  % NOM remove
%word_corresponds_to_semnet_relation_DOM(prevention,   noun,   of-by, prevented_by). % NOM remove
%word_corresponds_to_semnet_relation_DOM(prevention,   noun, of-with, prevented_by). % NOM remove
%word_corresponds_to_semnet_relation_DOM(sterilization,noun, of-with, prevented_by). % NOM remove
word_corresponds_to_semnet_relation_DOM(sterilization,noun, _, prevents). % NOM add 
word_corresponds_to_semnet_relation_DOM(sterilization,noun, against, prevents). % NOM untouched
word_corresponds_to_semnet_relation_DOM(sterilize,    verb, against, prevents).
word_corresponds_to_semnet_relation_DOM(vaccinate,    verb, against, prevents).
word_corresponds_to_semnet_relation_DOM(vaccinate,    verb,       _, prevents).
%word_corresponds_to_semnet_relation_DOM(vaccination,  noun, against, prevents). % NOM remove
word_corresponds_to_semnet_relation_DOM(vaccination,  noun, _, prevents). % NOM add

% ----- TEST_FOR
word_corresponds_to_semnet_relation_DOM(screen, verb, for, test_for).
word_corresponds_to_semnet_relation_DOM(test,   verb, for, test_for).


% ----- TRANSMITS
word_corresponds_to_semnet_relation_DOM(spread,        	verb, _,  transmits).
word_corresponds_to_semnet_relation_DOM(spread,        	adj,  by, transmitted_by). % 12/08/09
word_corresponds_to_semnet_relation_DOM(spread,        	verb, by, transmitted_by). % 12/08/09
word_corresponds_to_semnet_relation_DOM(spread,        	verb, from-to, transmits). % 12/16/09
word_corresponds_to_semnet_relation_DOM(transmit,      	verb, between, transmits).
word_corresponds_to_semnet_relation_DOM(transmit,      	verb, to, transmits).
word_corresponds_to_semnet_relation_DOM(transmit,      	verb, _,  transmits).
word_corresponds_to_semnet_relation_DOM(transmitted,    	adj,   from, transmitted_by).
word_corresponds_to_semnet_relation_DOM(transmissible, 	adj,  among, transmitted_by). % added 12/08/08
%word_corresponds_to_semnet_relation_DOM(transmission,  	noun, of-among, transmitted_by). % added 12/08/08 % NOM remove
word_corresponds_to_semnet_relation_DOM(transmission,  	noun, among, transmitted_by). % added 12/08/08 % NOM add
%word_corresponds_to_semnet_relation_DOM(transmission,  	noun, of-by, transmitted_by).  % NOM remove
word_corresponds_to_semnet_relation_DOM(transmission,  	noun, _, transmits). % added 12/08/08 % NOM add
%word_corresponds_to_semnet_relation_DOM(transmission,  	noun, of-to,  transmits). % NOM remove
word_corresponds_to_semnet_relation_DOM(transmission,  	noun, to,  transmitted_by). % NOM add
%word_corresponds_to_semnet_relation_DOM(transmission,  	noun, of-between, transmits). % NOM remove
word_corresponds_to_semnet_relation_DOM(transmission,  	noun, between, transmitted_by). % NOM add
%word_corresponds_to_semnet_relation_DOM(transmission,  	noun, of, transmits). % NOM remove
%word_corresponds_to_semnet_relation_DOM(transmissibility,  	noun, between, transmits). % NOM remove
word_corresponds_to_semnet_relation_DOM(transmissibility,  	noun, between, transmitted_by). % NOM add
%word_corresponds_to_semnet_relation_DOM(transmissibility,  	noun, of-to,transmits). % NOM remove
word_corresponds_to_semnet_relation_DOM(transmissibility,  	noun, to,transmitted_by). % NOM add
%word_corresponds_to_semnet_relation_DOM(transmissibility,  	noun, of-by, transmitted_by). % NOM remove
word_corresponds_to_semnet_relation_DOM(transmissibility,  	noun, _, transmits). % NOM add

% stubs
multiphrase_corresponds_to_semnet_relation_GEN(_, _, _, _, _, _) :- fail.

phrase_corresponds_to_semnet_relation_GEN(_, _, _, _, _, _) :- fail.