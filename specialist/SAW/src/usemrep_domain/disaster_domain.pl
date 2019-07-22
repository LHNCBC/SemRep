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

% file:	    disaster_domain.pl
% module:   disaster_domain.pl
% author:   graciela, halil
% purpose:  describe the disaster management domain -- for SIS

:- module(disaster_domain, [
	domain_name/1,
	domain_semtype/3,
	domain_replace/2,
	domain_concept/2
]).

domain_name(disaster).

% new semtypes
% abbreviation-long name-semgroup triples
% TODO: ensure that the semgroups are correct
domain_semtype(comc,'Community Characteristics',phen).
% domain_semtype(gova,'Governmental Activity',acti). We replaced gova with gora, in the Semantic Network
domain_semtype(infc,'Information Construct',conc).
domain_semtype(mony,'Money',objc).
domain_semtype(phst,'Physical Structure',objc).

domain_replace('Registries':'C0034975':[inpr,qnco],'Registries':'C0034975':[infc]). 
% The existing UMLS MT concepts that we would like to update.
% Currently, we mostly update the semantic types, but we can modify preferred names and CUIs, too. (see Fowls, Domestic -> Poultry, for instance)
domain_replace('Agencies':'C1556110':[inpr],'Agencies':'C1556110':[orgt]).
domain_replace('Airport':'C0442629':[mnob],'Airport':'C0442629':[phst]).
domain_replace('Authority':'C0599437':[socb],'Authority':'C0599437':[prog,orgt]).
domain_replace('Centers for Disease Control and Prevention (U.S.)':'C0007670':[hcro],'Centers for Disease Control and Prevention':'C0007670':[orgt]).
domain_replace('United States Food and Drug Administration':'C0041714':[hcro],'United States Food and Drug Administration':'C0041714':[orgt]).
domain_replace('District of Columbia':'C0012764':[geoa],'District of Columbia':'C0012764':[geoa,orgt]).
domain_replace('United Kingdom':'C0041700':[geoa],'United Kingdom':'C0041700':[geoa,orgt]).
domain_replace('United States':'C0041703':[geoa],'United States':'C0041703':[geoa,orgt]).
domain_replace('Afghanistan':'C0001732':[geoa],'Afghanistan':'C0001732':[geoa,orgt]).
domain_replace('Indonesia':'C0021247':[geoa],'Indonesia':'C0021247':[geoa,orgt]).
domain_replace('jurisdiction':'C0680647':[idcn],'jurisdiction':'C0680647':[geoa,orgt]).
domain_replace('Laos':'C0023034':[geoa],'Laos':'C0023034':[geoa,orgt]).
domain_replace('Maryland':'C0024858':[geoa],'Maryland':'C0024858':[geoa,orgt]).
domain_replace('Memphis':'C1061320':[invt],'Memphis':'C1061320':[geoa,orgt]).
domain_replace('New York':'C0027976':[geoa],'New York':'C0027976':[geoa,orgt]).
domain_replace('China':'C0008115':[geoa],'China':'C0008115':[geoa,orgt]).
domain_replace('Thailand':'C0039725':[geoa],'Thailand':'C0039725':[geoa,orgt]).
domain_replace('Vietnam':'C0042658':[geoa],'Vietnam':'C0042658':[geoa,orgt]).
domain_replace('Virginia':'C0042753':[geoa],'Virginia':'C0042753':[geoa,orgt]).
domain_replace('Advice':'C0150600':[hlca],'Advice':'C0150600':[inpr]).
domain_replace('Awareness':'C0004448':[menp],'Awareness':'C0004448':[inpr]).
domain_replace('Commitment':'C0870312':[menp],'Commitment':'C0870312':[inpr]).
domain_replace('Council':'C1551807':[popg],'Council':'C1551807':[orgt]).
domain_replace('Country':'C0454664':[geoa],'Country':'C0454664':[geoa,orgt]).
domain_replace('Data':'C1511726':[idcn],'Data':'C1511726':[infc]).
domain_replace('Discussion':'C0557061':[topp],'Discussion':'C0557061':[socb,inpr]).
domain_replace('Disease Management':'C0376636':[hlca],'Disease Management':'C0376636':[gora]).
domain_replace('disease transmission':'C0242781':[patf],'disease transmission':'C0242781':[phpr,comc]).
domain_replace('Engaged to be married':'C0425152':[socb],'Engagement':'C0425152':[inpr]).
domain_replace('experience':'C0596545':[menp],'experience':'C0596545':[inpr]).
domain_replace('incubation period':'C1320226':[fndg],'incubation period':'C1320226':[fndg,tmco]).
domain_replace('Facility':'C1547538':[idcn],'Facility':'C1547538':[phst,orgt]).
domain_replace('Falls':'C0085639':[fndg],'Fall':'C0085639':[tmco]).
domain_replace('Fowls, Domestic':'C0032850':[bird],'Poultry':'C0032850':[bird]).
domain_replace('Morbidity - disease rate':'C0026538':[qnco],'Morbidity':'C0026538':[comc]).
domain_replace('Funds':'C0016820':[qnco],'Funds':'C0016820':[mony]).
domain_replace('Funding':'C0243098':[qnco],'Funding':'C0243098':[mony]).
domain_replace('Generations':'C0079411':[tmco],'Generations':'C0079411':[popg]).
domain_replace('Germ':'C1517528':[idcn],'Germ':'C1517528':[orgm]).
domain_replace('Handwashing':'C0018581':[dora,topp],'Handwashing':'C0018581':[hlca]).
domain_replace('Health Authority':'C1273803':[hcro],'Health Authority':'C1273803':[orgt,prog]).
domain_replace('Incidence':'C0021149':[qnco],'Incidence':'C0021149':[comc]).
domain_replace('disease risk':'C0683481':[qnco],'disease risk':'C0683481':[comc,orga]).
domain_replace('Incubation period':'C1320226':[fndg],'Incubation period':'C1320226':[fndg,tmco]).
domain_replace('INDUSTRIAL WORKER':'C0239996':[prog],'INDUSTRIAL WORKER':'C0239996':[popg]).
domain_replace('Initiative':'C0424093':[menp],'Initiative':'C0424093':[gora,inpr]).
domain_replace('Information':'C1533716':[idcn],'Information':'C1533716':[infc]).
domain_replace('Investment':'C0021953':[mnob],'Investments':'C0021953':[mony,mnob]).
domain_replace('Materials':'C0520510':[sbst],'Materials':'C0520510':[mnob]).
domain_replace('Methods':'C0025663':[inpr],'Methods':'C0025663':[infc,inpr]).
domain_replace('Methodology':'C0025664':[inpr],'Methodology':'C0025664':[infc,inpr]).
domain_replace('Money':'C0870909':[mnob],'Money':'C0870909':[mony]).
domain_replace('Planning':'C0032074':[menp],'Planning':'C0032074':[inpr]).
domain_replace('Public Health':'C1552283':[prog],'Public Health':'C1552283':[bmod,gora]).
domain_replace('Recommendation':'C0034866':[idcn],'Recommendation':'C0034866':[gora]).
domain_replace('Region':'C0205147':[spco],'Region':'C0205147':[geoa,orgt]).
domain_replace('research study':'C0681814':[resa],'research study':'C0681814':[resa,infc]).
domain_replace('research':'C0035168':[resa],'research':'C0035168':[resa,infc]).
domain_replace('roth':'C0152110':[dsyn],'Roth':'C0152110':[humn]).
domain_replace('Step':'C1261552':[ftcn],'Step':'C1261552':[gora]).
domain_replace('strategy':'C0679199':[menp],'strategy':'C0679199':[gora,inpr]).
domain_replace('research outcome':'C0683954':[fndg],'research outcome':'C0683954':[infc]). % from study result, which maps to research outcome
domain_replace('Scientific Study':'C0947630':[lbpr],'Scientific Study':'C0947630':[infc,inpr]). %study
domain_replace('subject':'C1550501':[idcn],'subject':'C1550501':[popg]).
domain_replace('transmission process':'C1521797':[npop],'transmission process':'C1521797':[phpr,comc]).
domain_replace('Human to human transmission':'C1444005':[clas],'Human to human transmission':'C1444005':[phpr,comc]).
domain_replace('disease transmission':'C0242781':[patf],'disease transmission':'C0242781':[phpr,comc]).
domain_replace('spreading':'C0332261':[qlco],'spreading':'C0332261':[phpr,comc]).
domain_replace('Owner':'C1546511':[idcn],'Owner':'C1546511':[popg,orgt]).
domain_replace('Policy Makers':'C0242170':[prog],'Policy Makers':'C0242170':[orgt,prog]).
domain_replace('viral transmission':'C1160716':[celf],'viral transmission':'C1160716':[phpr,comc]).
domain_replace('healthful behavior':'C0679786':[inbe],'healthful behavior':'C0679786':[inbe,hlca]).
domain_replace('Hospitals':'C0019994':[hcro,mnob],'Hospitals':'C0019994':[hcro,phst]).
domain_replace('Cash - ActCode':'C1619834':[inpr],'Cash':'C1619834':[mony]).
domain_replace('Century':'C0719214':[phsu,vita],'century':'C0719214':[tmco]).
domain_replace('Press Releases (PT)':'C0282424':[inpr],'Press Releases':'C0282424':[infc]).
domain_replace('Messages':'C0470166':[inpr],'Messages':'C0470166':[infc]).
domain_replace('Military':'C1552433':[hcro,mnob],'Military':'C1552433':[popg,orgt]).
domain_replace('Geographic state':'C1301808':[geoa],'Geographic state':'C1301808':[geoa,orgt]).
domain_replace('Diagnostic tests':'C0086143':[diap],'Diagnostic tests':'C0086143':[diap,lbpr]).
domain_replace('ship':'C0036971':[mnob],'ship':'C0036971':[mnob,phst]).
domain_replace('H1N1':'C0580264':[inpr],'Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_replace('H2N2':'C0580265':[inpr],'Influenza A Virus, H2N2 Subtype':'C1620282':[virs]).
domain_replace('H3N2':'C0580267':[inpr],'Influenza A Virus, H3N2 Subtype':'C1615053':[virs]).
domain_replace('reinfection':'C0205339':[ftcn],'reinfection':'C0205339':[phpr,dsyn]).
domain_replace('Private Sector':'C0033176':[popg],'Private Sector':'C0033176':[orgt]).
domain_replace('Public Sector':'C0034035':[popg],'Public Sector':'C0034035':[orgt]).
domain_replace('Teams':'C0871489':[grup],'Teams':'C0871489':[prog,orgt]).
domain_replace('Technology':'C0039421':[ocdi],'Technology':'C0039421':[mnob,ocdi]).
domain_replace('National Oceanic and Atmospheric Administration':'C1549453':[prog],'National Oceanic and Atmospheric Administration':'C1549453':[orgt]).
domain_replace('Mail':'C0024492':[gora,mnob],'Mail':'C0024492':[mnob]).
domain_replace('Child':'C0008059':[aggp,inpr],'Child':'C0008059':[humn]).
domain_replace('family member':'C0086282':[famg],'Family member':'C0086282':[humn,famg]).
domain_replace('United States National Institutes of Health':'C0027468':[hcro],'National Institutes of Health':'C0027468':[orgt]).
domain_replace('Species':'C1548151':[idcn],'Species':'C1548151':[orgm,idcn]).
domain_replace('Orthomyxovirus Type A, Human':'C0029344':[virs],'Influenza virus type A, human':'C0029344':[virs]).
domain_replace('Closure':'C1521802':[resa],'Closure':'C1521802':[gora]).
domain_replace('Workplace':'C0162579':[spco],'Workplace':'C0162579':[phst]).
domain_replace('Prisons':'C0033168':[mnob],'Prisons':'C0033168':[phst,mnob]).
domain_replace('Worker':'C1306056':[ocdi],'Worker':'C1306056':[humn,popg]).
domain_replace('Missions':'C0026219':[orgt],'Missions':'C0026219':[acty,gora]).
domain_replace('Approach':'C0449445':[spco],'Approach':'C0449445':[inpr]).
domain_replace('Meetings':'C0556656':[hlca],'Meetings':'C0556656':[hcpp]).
domain_replace('Hearing':'C0018767':[phsf],'Hearing':'C0018767':[hcpp,phsf]).
domain_replace('Mental Recall':'C0034770':[menp],'Recall':'C0034770':[gora,acty]).
domain_replace('Readiness':'C1318963':[fndg],'Readiness':'C1318963':[acty]).
domain_replace('Soaps':'C0037392':[lipd,mnob],'Soaps':'C0037392':[mnob]).
domain_replace('Immunocompromised Host':'C0085393':[fndg],'Immunocompromised Host':'C0085393':[humn,orgm]).
domain_replace('Diagnosis':'C0011900':[hlca],'Diagnosis':'C0011900':[diap]).
domain_replace('Resources':'C0035201':[idcn],'Resources':'C0035201':[mnob,idcn]).
domain_replace('Death Rate':'C0205848':[qnco],'Death Rate':'C0205848':[comc]).
domain_replace('attack rate':'C0683920':[qnco],'attack rate':'C0683920':[comc]).
domain_replace('Vaccines':'C0042210':[phsu,imft],'Vaccines':'C0042210':[phsu]).
domain_replace('Influenza virus vaccine':'C0021403':[orch,phsu,imft],'Influenza virus vaccine':'C0021403':[phsu]).
domain_replace('Vaccines, Attenuated':'C0042211':[phsu,imft],'Live Vaccine':'C0042211':[phsu]).
domain_replace('Approach':'C0449445':[spco],'Approach':'C0449445':[inpr]).
domain_replace('Techniques':'C0449851':[ftcn],'Techniques':'C0449851':[inpr]).
domain_replace('finding':'C0243095':[ftcn],'Finding':'C0243095':[fndg]).
domain_replace('Pamphlets':'C0030258':[mnob,inpr],'Pamphlets':'C0030258':[infc]).
domain_replace('Diagnostic test device':'C1263440':[medd],'Diagnostic test':'C1263440':[diap,lbpr]).
domain_replace('PROTECTION':'C1545588':[fndg],'PROTECTION':'C1545588':[gora,acty]).
domain_replace('Warning Signs':'C0871598':[gora],'Warning Signs':'C0871598':[sosy,gora]).
domain_replace('Publications':'C0034036':[mnob,inpr],'Publications':'C0034036':[infc]).
domain_replace('Advisory Committees':'C0162458':[gora],'Advisory Committees':'C0162458':[orgt]).
domain_replace('Nasal Spray':'C0461725':[bodm],'Nasal Spray':'C0461725':[clnd]).
domain_replace('Nasal drops':'C0991524':[bodm],'Nasal drops':'C0991524':[clnd]).
domain_replace('Magazines':'C0162443':[inpr,mnob],'Magazines':'C0162443':[infc]).
domain_replace('Identification/Security Systems':'C0810533':[medd],'Security Systems':'C0810533':[mnob]).
domain_replace('Surgical face mask':'C0181758':[medd],'Surgical face mask':'C0181758':[mnob]).
domain_replace('Closure':'C1521802':[resa],'Closure':'C1521802':[gora]).
domain_replace('Enactments':'C0870493':[topp],'Enactments':'C0870493':[rnlw]).
domain_replace('Applicants':'C0696628':[phsu],'Applicants':'C0696628':[popg]).
domain_replace('Medical record, device':'C0025102':[inpr,mnob],'Medical record':'C0025102':[infc]).
domain_replace('Controllers':'C0180112':[medd],'Controllers':'C0180112':[popg]).
domain_replace('workforce':'C0024752':[ftcn],'workforce':'C0024752':[popg]).
domain_replace('Health':'C0018684':[idcn],'Health':'C0018684':[comc,orgf]).
domain_replace('State':'C1442792':[ftcn],'State':'C1442792':[geoa,orgt]).
domain_replace('Reaction':'C0443286':[ftcn],'Reaction':'C0443286':[clna]).
domain_replace('Teleconference':'C0039450':[mcha],'Teleconference':'C0039450':[acty]).
domain_replace('Holidays':'C0019843':[acty],'Holidays':'C0019843':[tmco]).
domain_replace('Verbal':'C0439824':[orgf],'Verbal':'C0439824':[qlco]).
domain_replace('Newsletters':'C0027988':[inpr,mnob],'Newsletters':'C0027988':[infc]).
domain_replace('Termination':'C1549081':[inpr],'Termination':'C1549081':[tmco]).
domain_replace('Epidemiologists':'C1516908':[bmod],'Epidemiologists':'C1516908':[prog]).
domain_replace('Placebos':'C0032042':[topp,medd],'Placebos':'C0032042':[medd]).
domain_replace('Crisis':'C0231224':[fndg],'Crisis':'C0231224':[phpr]).
domain_replace('Staff Member':'C1552089':[idcn],'Staff Member':'C1552089':[popg,humn]).
domain_replace('US Army':'C1549454':[prog],'US Army':'C1549454':[prog,orgt]).
domain_replace('viral resistance':'C0520989':[clna],'viral resistance':'C0520989':[phsf,clna]).
domain_replace('Immunocompromised Host':'C0085393':[fndg],'Immunocompromised Host':'C0085393':[humn,anim]).
domain_replace('reverse transcription polymerase chain reaction':'C0814037':[genf],'reverse transcription polymerase chain reaction':'C0814037':[mbrt]).
domain_replace('beneficiary':'C1550502':[idcn],'beneficiary':'C1550502':[humn]).
domain_replace('network':'C0679670':[popg],'network':'C0679670':[orgt]).
domain_replace('Restaurants':'C0035255':[mnob],'Restaurants':'C0035255':[mnob,phst]).
domain_replace('host':'C1167395':[celc],'host':'C1167395':[anim,humn,celc]).
domain_replace('Subgroup':'C1515021':[clas],'Subgroup':'C1515021':[grup]).
domain_replace('Screening procedure':'C0220908':[hlca],'Screening procedure':'C0220908':[diap]).
domain_replace('Postmortem Diagnosis':'c0332145':[fndg],'Postmortem Diagnosis':'c0332145':[lbtr]).
domain_replace('Printed media':'C0033159':[phob],'Printed media':'C0033159':[infc,phob]).
domain_replace('Resident':'C1549439':[inpr],'Resident':'C1549439':[humn,popg]).
domain_replace('RAPID TEST':'c1441717':[lbpr],'RAPID TEST':'c1441717':[diap,lbpr]).
domain_replace('respiratory sample':'C0444279':[bdsu],'Respiratory sample':'C0444279':[bdsu,sbst]).

domain_replace(PreferredName:CUI:[hcro],PreferredName:CUI:[orgt,hcro]). 
domain_replace(PreferredName:CUI:[geoa],PreferredName:CUI:[orgt,geoa]).
domain_replace(PreferredName:CUI:[shro],PreferredName:CUI:[orgt,shro]).
domain_replace(PreferredName:CUI:[imft],PreferredName:CUI:[phsu]).
domain_replace(PreferredName:CUI:[imft,phsu],PreferredName:CUI:[phsu]).

%domain_replace(PreferredName:CUI:[geoa],PreferredName:CUI:[geoa,orgt]). % We could use this to generalize to all geoa concepts.

% domain_concept clauses are of the form (NewSynonym,PreferredName:CUI:SemanticTypes).
% This predicate covers two types of concepts.
% (1) Concepts already in UMLS that we want to add a new synonym for, so that we can map a new string to the concept.
% (2) Entirely new domain concepts not found in UMLS.
% The first argument is the string that we want to map and should be in lowercase.

% The following clauses cover the concepts of type (1). 
domain_concept('u.s. centers for disease control and prevention','Centers for Disease Control and Prevention':'C0007670':[orgt]).
domain_concept('h5n1 virus','Influenza A Virus, H5N1 Subtype':'C1613950':[virs]).
domain_concept('h5n1','Influenza A Virus, H5N1 Subtype':'C1613950':[virs]).
domain_concept('swine influenza a (h1n1) virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('new h1n1 swine flu virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('h1n1 swine flu virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('swine influenza a (h1n1)','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('novel influenza a (h1n1) virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('novel influenza a (h1n1)','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('novel influenza a','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('novel h1n1 virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('new h1n1 virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('novel h1n1','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('2009 pandemic influenza A (h1n1) virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('2009 pandemic influenza A (h1n1)','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('2009 h1n1 influenza virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('pandemic a (h1n1) 2009 influenza virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('pandemic (h1n1) 2009 influenza virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('pandemic h1n1 influenza a virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('pandemic h1n1 influenza virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('pandemic (h1n1) 2009 virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('pandemic h1n1 virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('pandemic h1n1','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('2009 influenza virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('2009 h1n1 flu virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('2009 h1n1 virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('2009 h1n1','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('influenza a pandemic (h1n1) 2009 virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('influenza a (h1n1) virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('influenza a h1n1 virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('h1n1 pandemic influenza virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('h1n1 pandemic virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('h1n1 influenza virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('h1n1 virus','Influenza A Virus, H1N1 Subtype':'C1615607':[virs]).
domain_concept('h2n2 virus','Influenza A Virus, H2N2 Subtype':'C1620282':[virs]). 
domain_concept('h3n2 virus','Influenza A Virus, H3N2 Subtype':'C1615053':[virs]).
domain_concept('avian influenza a h5n1','Influenza A Virus, H5N1 Subtype':'C1613950':[virs]).
domain_concept('avian influenza a(h5n1)','Influenza A Virus, H5N1 Subtype':'C1613950':[virs]).
domain_concept('fda','United States Food and Drug Administration':'C0041714':[orgt]).
domain_concept('u.s. food and drug administration','United States Food and Drug Administration':'C0041714':[orgt]).
domain_concept('uk','United Kingdom':'C0041700':[geoa,orgt]).
domain_concept('usa','United States':'C0041703':[geoa,orgt]).
domain_concept('u.s.','United States':'C0041703':[geoa,orgt]).
domain_concept('us','United States':'C0041703':[geoa,orgt]).
domain_concept('d.c.','District of Columbia':'C0012764':[geoa,orgt]).
domain_concept('dc','District of Columbia':'C0012764':[geoa,orgt]).
domain_concept('usda','United States Department of Agriculture':'C0085410':[orgt]).
domain_concept('department of agriculture','United States Department of Agriculture':'C0085410':[orgt]).
domain_concept('u.s. department of agriculture','United States Department of Agriculture':'C0085410':[orgt]).
domain_concept('who','World Health Organization':'C0043237':[orgt]).
domain_concept('lao people '' s democratic republic','Laos':'C0023034':[geoa,orgt]).
domain_concept('ny','New York':'C0027976':[geoa,orgt]).
domain_concept('people ''s republic of china','China':'C0008115':[geoa,orgt]).
domain_concept('hand washing','Handwashing':'C0018581':[inbe,hlca]).
domain_concept('hcw','Health Personnel':'C0018724':[prog]).
domain_concept('hospital healthcare worker','Health Personnel':'C0018724':[prog]).
domain_concept('method','Methods':'C0025663':[infc,inpr]).
domain_concept('noaa','National Oceanic and Atmospheric Administration':'C1549453':[orgt]).
domain_concept('ppe','Personal protective equipment':'C1443871':[mnob]).
domain_concept('outbreak','Disease Outbreaks':'C0012652':[phpr]).
domain_concept('osha','United States Occupational Safety and Health Administration':'C0041731':[hcro]).
domain_concept('protective equipment','Personal protective equipment':'C1443871':[mnob]).
domain_concept('study','Scientific Study':'C0947630':[infc,inpr]).
domain_concept('transmission','transmission process':'C1521797':[phpr,comc]).
domain_concept('transmission of disease','disease transmission':'C0242781':[phpr,comc]).
domain_concept('policymaker','Policy Makers':'C0242170':[orgt,prog]). 
domain_concept('press release','Press Releases (PT)':'C0282424':[infc]).
domain_concept('public health official','public health officer':'C0401875':[prog]).
domain_concept('infection control practice','Infectious disease prevention / control':'C0085557':[hlca]).
domain_concept('re - infection','reinfection':'C0205339':[phpr,dsyn]).
domain_concept('rainfall','Rain':'C0034640':[npop]).
domain_concept('swine influenza virus infection','Swine influenza':'C0276357':[dsyn]).
domain_concept('new h1n1 flu (swine flu)','Swine influenza':'C0276357':[dsyn]).
domain_concept('h1n1 flu (swine flu)','Swine influenza':'C0276357':[dsyn]).
domain_concept('swine flu','Swine influenza':'C0276357':[dsyn]).
domain_concept('public health guidelines','Guidelines':'C0162791':[inpr]).
domain_concept('biological agent','Infectious agent':'C0314732':[orgm]).
domain_concept('human population','Human':'C0020114':[humn]).
domain_concept('avian influenza a virus','Influenza A Virus, Avian':'C0029343':[virs]).
domain_concept('avian influenza a','Influenza A Virus, Avian':'C0029343':[virs]).
domain_concept('avian influenza virus','Influenza A Virus, Avian':'C0029343':[virs]).
%domain_concept('influenza a','Influenza A Virus, Avian':'C0029343':[virs]).
domain_concept('antiviral treatment','Antiviral Therapy':'C0280274':[topp]).
domain_concept('h7n7 virus','Influenza A Virus, H7N7 Subtype':'C1612845':[virs]).
domain_concept('h3n8 virus','Influenza A Virus, H3N8 Subtype':'C1614502':[virs]).
domain_concept('h7n7','Influenza A Virus, H7N7 Subtype':'C1612845':[virs]).
domain_concept('h3n8','Influenza A Virus, H3N8 Subtype':'C1614502':[virs]).
domain_concept('human influenza a virus','Influenza virus type A, human':'C0029344':[virs]).
domain_concept('human strain of influenza a virus','Influenza virus type A, human':'C0029344':[virs]).
domain_concept('influenza a subtype','Influenza A virus':'C0029347':[virs]).
domain_concept('influenza a','influenza A':'C2062441':[dsyn]).
domain_concept('immune protection','immunity':'C0020964':[phsf]).
domain_concept('virus transmission','viral transmission':'C1160716':[phpr,comc]).
domain_concept('spread of the virus','viral transmission':'C1160716':[phpr,comc]).
domain_concept('spread of this virus','viral transmission':'C1160716':[phpr,comc]).
domain_concept('u.s. environmental protection agency','United States Environmental Protection Agency':'C0041712':[orgt]).
domain_concept('iom','Institute of Medicine (U.S.)':'C0021621':[orgt,hcro]).
domain_concept('parents','parent':'C0030551':[famg]).
domain_concept('viruses','Virus':'C0042776':[virs]).
domain_concept('mortality rate','Death Rate':'C0205848':[comc]).
domain_concept('occupational safety and health administration','United States Occupational Safety and Health Administration':'C0041731':[orgt]).
domain_concept('osha','United States Occupational Safety and Health Administration':'C0041731':[orgt]).
domain_concept('pediatric recipient','Child':'C0008059':[aggp,inpr]).
domain_concept('Respiratory illness','Respiration Disorders':'C0035204':[dsyn]).
domain_concept('laboratory test results','Laboratory Results':'C1254595':[lbtr]).
domain_concept('u.s. army','US Army':'C1549454':[prog,orgt]).
domain_concept('antiviral medication','Antiviral agents':'C0003451':[phsu]).
domain_concept('immune compromised host','Immunocompromised Host':'C0085393':[humn,anim]).
domain_concept('immunosuppressed host','Immunocompromised Host':'C0085393':[humn,anim]).
domain_concept('immunosuppressed patient','Immunocompromised Host':'C0085393':[humn,anim]).
domain_concept('immune compromised patient','Immunocompromised Host':'C0085393':[humn,anim]).
domain_concept('immunosuppressed person','Immunocompromised Host':'C0085393':[humn,anim]).
domain_concept('immune compromised person','Immunocompromised Host':'C0085393':[humn,anim]).
domain_concept('immunocompromised person','Immunocompromised Host':'C0085393':[humn,anim]).
domain_concept('rt - polymerase chain reaction','Reverse Transcriptase Polymerase Chain Reaction':'C0599161':[mbrt]).
domain_concept('service member','Servicemen':'C0871346':[prog]).
domain_concept('servicemember','Servicemen':'C0871346':[prog]).
domain_concept('antiretrovirals','Anti-Retroviral Agents':'C0599685':[phsu]).
domain_concept('memory immune response','Immunologic Memory':'C0021055':[ortf]).
domain_concept('adult population','adult':'C0001675':[aggp]).
domain_concept('immunologic response','Immune response':'C0301872':[ortf]).
domain_concept('flu virus','Orthomyxoviridae':'C0029341':[virs]).
domain_concept('influenza virus of swine origin','Orthomyxovirus Type A, Porcine':'C0029345':[virs]).
domain_concept('flu virus of swine origin','Orthomyxovirus Type A, Porcine':'C0029345':[virs]).
domain_concept('group of the population','Population Group':'C1257890':[popg]).
domain_concept('nasal wash specimen','Specimen':'C0370003':[sbst]).
domain_concept('migratory bird','Aves':'C0005595':[bird]).
domain_concept('recurrence of leukaemia','leukaemia recurrent':'C0920028':[neop]).
domain_concept('respiratory specimen','Respiratory sample':'C0444279':[bdsu,sbst]).


% The following clauses cover the concepts of type (2). 
% The CUIs start with D(disaster) and are assigned in ascending order. The current maximum number is D0000260.
domain_concept('aclu','ACLU':'D0000001':[orgt]).
domain_concept('agriculture coordinating council','Agriculture coordinating council':'D0000002':[orgt]).
domain_concept('animal and plant health inspection service','Animal and plant health inspection service':'D0000003':[orgt]).
domain_concept('aphis','Aphis':'D0000004':[orgt]). % assuming a different concept from aphis C1034429,invt
domain_concept('bbg '' s international broadcasting bureau','BBG''s International Broadcasting Bureau':'D0000005':[orgt]).
domain_concept('cdrh office','CDRH office':'D0000006':[orgt]).
domain_concept('center for biologics evaluation and research','Center for Biologics Evaluation and Research':'D0000007':[orgt]).
domain_concept('center for drug evaluation and research','Center for Drug Evaluation and Research':'D0000008':[orgt]).
domain_concept('department of natural resources','Department of Natural Resources':'D0000009':[orgt]). 
domain_concept('dnr','Department of Natural Resources':'D0000009':[orgt]). % assuming dnr is Dept of Natural Resources
domain_concept('department of state','Department of State':'D0000010':[orgt]).
domain_concept('state department','Department of State':'D0000010':[orgt]).
domain_concept('department of homeland security','Department of Homeland Security':'D0000011':[orgt]).
domain_concept('dhs','Department of Homeland Security':'D0000011':[orgt]).
domain_concept('fao','FAO':'D0000012':[orgt]).
domain_concept('food and agriculture organization','FAO':'D0000012':[orgt]).
domain_concept('united nations food and agriculture organization','FAO':'D0000012':[orgt]).
domain_concept('hhs official','HHS official':'D0000013':[prog]).
domain_concept('hhs','HHS':'D0000014':[orgt]).
domain_concept('homeland security council','Homeland Security Council':'D0000015':[orgt]).
domain_concept('hsc','Homeland Security Council':'D0000015':[orgt]).
domain_concept('homeland security','Homeland Security':'D0000016':[orgt]).
domain_concept('mit','MIT':'D0000017':[orgt]).
domain_concept('mit '' s department','MIT''s department':'D0000018':[orgt]).
domain_concept('ngo','NGO':'D0000019':[orgt]).
domain_concept('secretary of health and human services','Secretary of Health and Human Services':'D0000020':[orgt,prog]).
domain_concept('tsa','TSA':'D0000021':[orgt]).
domain_concept('virginia department of health','Virginia Department of Health':'D0000022':[orgt]).
domain_concept('wal mart','Wal Mart':'D0000023':[orgt]).
domain_concept('wal mart partnership','Wal Mart partnership':'D0000024':[orgt]).
domain_concept('atlanta','Atlanta':'D0000025':[geoa,orgt]).
domain_concept('atlantic','Atlantic':'D0000026':[geoa,orgt]).
domain_concept('brisbane','Brisbane':'D0000027':[geoa,orgt]).
domain_concept('east south','East south':'D0000028':[geoa,orgt]).
domain_concept('eurasia','Eurasia':'D0000029':[geoa,orgt]).
domain_concept('fujian','Fujian':'D0000030':[geoa,orgt]).
domain_concept('guangxi','Guangxi':'D0000031':[geoa,orgt]).
domain_concept('hamburg','Hamburg':'D0000032':[geoa,orgt]).
domain_concept('hanoi','Hanoi':'D0000033':[geoa,orgt]).
domain_concept('hiroshima','Hiroshima':'D0000034':[geoa,orgt]).
domain_concept('hunan','Hunan':'D0000035':[geoa,orgt]).
domain_concept('influsim','Influsim':'D0000036':[mnob,inpr]).
domain_concept('international broadcasting bureau','International Broadcasting Bureau':'D0000037':[orgt]).
domain_concept('mannheim','Mannheim':'D0000038':[geoa,orgt]).
domain_concept('midwest','Midwest':'D0000039':[geoa,orgt]).
domain_concept('milwaukee','Milwaukee':'D0000040':[geoa,orgt]).
domain_concept('northern hemisphere','Northern hemisphere':'D0000041':[geoa]).
domain_concept('padova','Padova':'D0000042':[geoa,orgt]).
domain_concept('pittsburgh','Pittsburgh':'D0000043':[geoa,orgt]).
domain_concept('prussia','Prussia':'D0000044':[geoa,orgt]).
domain_concept('republic','Republic':'D0000045':[geoa,orgt]).
domain_concept('shantou','Shantou':'D0000046':[geoa,orgt]).
domain_concept('vienna','Vienna':'D0000047':[geoa,orgt]).
domain_concept('zhejiang','Zhejiang':'D0000048':[geoa,orgt]).
domain_concept('action','Action':'D0000049':[acty]).
domain_concept('animal outbreak','Animal outbreak':'D0000050':[phpr]).
domain_concept('announcement','Announcement':'D0000051':[infc]).
domain_concept('antiviral resistance','Antiviral resistance':'D0000052':[orga,com]).
domain_concept('billion','Billion':'D0000053':[mony]).
domain_concept('building','Building':'D0000054':[phst]). % physical structure
domain_concept('burden of disease','Burden of disease':'D0000055':[comc]).
domain_concept('finacial burden','Financial Burden':'D0000056':[mony]).
domain_concept('bush administration','Bush administration':'D0000057':[orgt]).
domain_concept('campaign','Campaign':'D0000058':[gora]).
domain_concept('cargo','Cargo':'D0000059':[mnob]).
domain_concept('colleague','Colleague':'D0000060':[prog]).
domain_concept('committee','Committee':'D0000061':[orgt]).
domain_concept('containment strategy','Containment strategy':'D0000062':[gora,inpr]).
domain_concept('cough etiquette','Cough etiquette':'D0000063':[hlca,inbe]).
domain_concept('diagnostician','Diagnostician':'D0000064':[prog]).
domain_concept('disease eradication','Disease eradication':'D0000065':[gora]).
domain_concept('disease spread','Disease spread':'D0000066':[phpr,comc]).
domain_concept('mortality','Mortality':'D0000067':[comc]).
domain_concept('environment of collaboration','Environment of collaboration':'D0000068':[socb]).
domain_concept('epidemic containment','Epidemic containment':'D0000069':[gora]).
domain_concept('epidemic surveillance','Epidemic surveillance':'D0000070':[gora]).
domain_concept('evacuation','Evacuation':'D0000071':[gora]).
domain_concept('centers for disease control','Centers for Disease Control':'D0000072':[orgt]).
domain_concept('expert','Expert':'D0000073':[prog]).
domain_concept('face mask','Face mask':'D0000074':[mnob]).
domain_concept('facemask','Face mask':'D0000074':[mnob]).
domain_concept('firewall','Firewall':'D0000075':[inpr,mnob]).
domain_concept('governor','Governor':'D0000076':[prog]).
domain_concept('government official','Government official':'D0000077':[prog]).
domain_concept('public official','public official':'D0000077':[prog]).
domain_concept('grocery store','Grocery store':'D0000078':[phst,orgt]).
domain_concept('guideline','Guideline':'D0000079':[infc]).
domain_concept('gulf coast','Gulf coast':'D0000080':[geoa]).
domain_concept('number of people affected','Number of people affected':'D0000081':[comc]).
domain_concept('infection rate','Infection rate':'D0000082':[comc]).
domain_concept('rate of infection','Infection rate':'D0000082':[comc]).
domain_concept('viral concentration','viral concentration':'D0000083':[comc,orga,fndg]).
domain_concept('infrastructure','Infrastructure':'D0000084':[phst,inpr]).
domain_concept('operation','Operation':'D0000085':[acty]).
domain_concept('inhabitant','Inhabitant':'D0000086':[popg]).
domain_concept('leader','Leader':'D0000087':[prog,idcn]).
domain_concept('measure','Measure':'D0000088':[gora]).
domain_concept('megacity','Megacity':'D0000089':[geoa]).
domain_concept('microbiologic study','Microbiologic study':'D0000090':[infc,resa]).
domain_concept('swine flu vaccine','swine flu vaccine':'D0000091':[phsu]).
domain_concept('novel h1n1 vaccine','swine flu vaccine':'D0000091':[phsu]).
domain_concept('2009 h1n1 vaccine','swine flu vaccine':'D0000091':[phsu]).
domain_concept('pandemic h1n1 vaccine','swine flu vaccine':'D0000091':[phsu]).
domain_concept('h1n1 vaccine','swine flu vaccine':'D0000091':[phsu]).
domain_concept('fluad vaccine','swine flu vaccine':'D0000091':[phsu]).
domain_concept('fluad','swine flu vaccine':'D0000091':[phsu]).
domain_concept('observation','Observation':'D0000092':[gora,infc]).
domain_concept('locality','Locality':'D0000093':[orgt,geoa]).
domain_concept('passport','Passport':'D0000094':[infc,idcn]).
domain_concept('partnership','Partnership':'D0000095':[orgt]).
domain_concept('partner','Partner':'D0000096':[orgt,popg]).
domain_concept('plan of action','Plan of action':'D0000097':[infc,inpr]).
domain_concept('action plan','Plan of action':'D0000097':[infc,inpr]).
domain_concept('plan of care','Plan of care':'D0000098':[infc,inpr]).
domain_concept('plan','Plan':'D0000099':[infc,inpr]).
domain_concept('planner','Planner':'D0000100':[popg,orgt]).
domain_concept('precaution','Precaution':'D0000101':[acty]).
domain_concept('preparedness kit','Preparedness kit':'D0000102':[mnob]).
domain_concept('preparedness','Preparedness':'D0000103':[acty]).
domain_concept('program','Program':'D0000104':[infc,inpr]).
domain_concept('protocol','Protocol':'D0000105':[infc,inpr]).
domain_concept('gathering','Gathering':'D0000106':[socb]).
domain_concept('public gathering','Gathering':'D0000106':[socb]).
domain_concept('advisor','advisor':'D0000107':[prog]).
domain_concept('report','Report':'D0000108':[infc]).
domain_concept('response action','Response action':'D0000109':[acty]).
domain_concept('sample','Sample':'D0000110':[sbst]).
domain_concept('shipyard','Shipyard':'D0000111':[orgt,phst]).
domain_concept('southern hemisphere','Southern hemisphere':'D0000112':[geoa]).
domain_concept('spread of disease','Spread of disease':'D0000113':[phpr,comc]).
domain_concept('stakeholder','Stakeholder':'D0000114':[popg,orgt,idcn]).
domain_concept('storm','Storm':'D0000115':[npop]).
domain_concept('subregion','Subregion':'D0000116':[geoa,orgt]).
domain_concept('surveillance system','Surveillance system':'D0000117':[mnob]).
domain_concept('surveillance','Surveillance':'D0000118':[gora,acty]).
domain_concept('hpaiv','HPAIV':'D0000119':[virs]).
domain_concept('trade','Trade':'D0000120':[acty]).
domain_concept('virus isolation','Virus isolation':'D0000121':[lbpr]).
domain_concept('waterfowl','Waterfowl':'D0000122':[bird]).
domain_concept('domestic waterfowl','Waterfowl':'D0000122':[bird]).
domain_concept('we','We':'D0000123':[humn,orgt]).
domain_concept('website','Website':'D0000124':[inpr,mnob]).
domain_concept('web site','Website':'D0000124':[inpr,mnob]).
domain_concept('wetlands','Wetlands':'D0000125':[geoa]).
domain_concept('wildlife department','Wildlife Department':'D0000126':[orgt]).
domain_concept('wildlife','Wildlife':'D0000127':[anim]).
domain_concept('workgroup','Workgroup':'D0000128':[popg,orgt]).
domain_concept('world','World':'D0000129':[geoa]).
domain_concept('you','You':'D0000130':[humn,orgt]).
domain_concept('sc18','SC18':'D0000131':[virs]).
domain_concept('ny18','NY18':'D0000132':[virs]).
domain_concept('av18','AV18':'D0000133':[virs]).
domain_concept('tx18','TX18':'D0000134':[virs]).
domain_concept('national institute of general medical sciences','National Institute of General Medical Sciences':'D0000135':[orgt]).
domain_concept('singapore - mit alliance for research and technology','Singapore-MIT Alliance for Research and  Technology':'D0000136':[orgt]).
domain_concept('proper respiratory hygiene','Proper respiratory hygiene':'D0000137':[inbe,hlca]).
domain_concept('department of public health','Department of Public Health':'D0000138':[orgt]).
domain_concept('lamp assay','LAMP assay':'D0000139':[lbpr]).
domain_concept('laboratory - confirmed influenza infection','laboratory-confirmed influenza infection':'D0000140':[dsyn]).
domain_concept('public health actor','public health actor':'D0000141':[orgt,prog]).
domain_concept('quarantine measure','quarantine measure':'D0000142':[gora]).
domain_concept('sequester','sequester':'D0000143':[gora]).
domain_concept('pharmaceutical manufacturer','pharmaceutical manufacturer':'D0000144':[orgt,prog]).
domain_concept('monitoring','Monitoring':'D0000145':[gora]).
domain_concept('illness','Illness':'D0000146':[dsyn]).
domain_concept('mild illness','Illness':'D0000146':[dsyn]).
domain_concept('asian development bank','Asian Development Bank':'D0000147':[orgt]).
domain_concept('decision maker','decision maker':'D0000148':[prog]).
domain_concept('fao oie crisis management center','FAO OIE Crisis Management Center':'D0000149':[orgt]).
domain_concept('national center for biotechnology information','National Center for Biotechnology Information':'D0000150':[orgt]).
domain_concept('state public health agency','state public health agency':'D0000151':[orgt]).
domain_concept('spha','state public health agency':'D0000151':[orgt]).
domain_concept('sphas','state public health agency':'D0000151':[orgt]).
domain_concept('diplomatic activity','diplomatic activity':'D0000152':[gora,acty]).
domain_concept('world organization for animal health','World Organization for Animal Health':'D0000153':[orgt]).
domain_concept('world organisation for animal health','World Organization for Animal Health':'D0000153':[orgt]).
domain_concept('oie','World Organization for Animal Health':'D0000153':[orgt]).
domain_concept('white house','White House':'D0000154':[orgt]).
domain_concept('agriculture sector','agriculture sector':'D0000155':[orgt]).
domain_concept('business','business':'D0000156':[orgt,idcn]).
domain_concept('bureau','Bureau':'D0000157':[orgt]).
domain_concept('district','district':'D0000158':[geoa,orgt]).
domain_concept('department','department':'D0000159':[orgt]).
domain_concept('workgroup','workgroup':'D0000160':[prog,orgt]).
domain_concept('health official','health official':'D0000161':[prog]).
domain_concept('evacuee','evacuee':'D0000162':[popg]).
domain_concept('countermeasure','countermeasure':'D0000163':[gora]).
domain_concept('commodity','commodity':'D0000164':[mnob,inpr]).
domain_concept('emergency management official','emergency management official':'D0000165':[prog]).
domain_concept('paralysis','Paralysis':'D0000166':[dsyn]).
domain_concept('first responder','first responder':'D0000167':[prog]).
domain_concept('subpopulation','subpopulation':'D0000168':[popg]).
domain_concept('cooperator','cooperator':'D0000169':[popg,orgt]).
domain_concept('coordinator','coordinator':'D0000170':[popg,orgt]).
domain_concept('forum','forum':'D0000171':[mnob,orgt]).
domain_concept('asia pacific economic cooperation forum','Asia Pacific Economic Cooperation Forum':'D0000172':[orgt,hcpp]).
domain_concept('restriction','restriction':'D0000173':[inpr,infc]).
domain_concept('panel','panel':'D0000174':[mnob,orgt]).
domain_concept('producer','producer':'D0000175':[humn,orgt,popg]).
domain_concept('national governors association','National Governors Association ':'D0000176':[orgt]).
domain_concept('national governors '' association','National Governors Association ':'D0000176':[orgt]).
domain_concept('nga','National Governors Association ':'D0000176':[orgt]).
domain_concept('governor','governor':'D0000177':[prog]).
domain_concept('recipient','Recipient':'D0000178':[popg,podg,orgt]).
domain_concept('power outage','power outage':'D0000179':[phpr]).
domain_concept('outage','power outage':'D0000179':[phpr]).
domain_concept('vaccine candidate','vaccine candidate':'D0000180':[phsu]).
domain_concept('council of state and territorial epidemiologists','Council of State and Territorial Epidemiologists':'D0000181':[orgt]).
domain_concept('cste','Council of State and Territorial Epidemiologists':'D0000181':[orgt]).
domain_concept('emergency operations center','emergency operations center':'D0000182':[orgt]).
domain_concept('eoc','emergency operations center':'D0000182':[orgt]).
domain_concept('specialist','specialist':'D0000183':[prog]).
domain_concept('public health emergency','public health emergency':'D0000184':[phpr]).
domain_concept('h5n1 virus infection','H5N1 virus infection':'D0000185':[dsyn]).
domain_concept('h5n1 infection','H5N1 virus infection':'D0000185':[dsyn]).
domain_concept('h5n1 flu','H5N1 virus infection':'D0000185':[dsyn]).
domain_concept('field operation','field operation':'D0000186':[acty,gora]).
domain_concept('h1n2','H1N2':'D0000187':[virs]).
domain_concept('influenza virus infection','Influenza virus infection':'D0000188':[dsyn]).
domain_concept('influenza infection','Influenza virus infection':'D0000188':[dsyn]).
domain_concept('un system influenza coordination','UN System Influenza Coordination':'D0000189':[orgt]).
domain_concept('united nations system influenza coordination','UN System Influenza Coordination':'D0000189':[orgt]).
domain_concept('unsic','UN System Influenza Coordination':'D0000189':[orgt]).
domain_concept('un system','UN System':'D0000190':[orgt]).
domain_concept('united nations system','UN System':'D0000190':[orgt]).
domain_concept('un development group','UN Development Group':'D0000191':[orgt]).
domain_concept('united nations development group','UN Development Group':'D0000191':[orgt]).
domain_concept('ministry of the interior','Ministry of the Interior':'D0000192':[orgt]).
domain_concept('interior ministry','Ministry of the Interior':'D0000192':[orgt]).
domain_concept('task force on epidemic, alert and response','Task Force on Epidemic, Alert and Response':'D0000193':[orgt]).
domain_concept('member country','Member Country':'D0000194':[geoa,orgt]).
domain_concept('global influenza preparedness plan','Global Influenza Preparedness Plan':'D0000195':[infc,inpr]).
domain_concept('warning system','Warning system':'D0000196':[inpr,mnob]).
domain_concept('practice','practice':'D0000197':[inpr]).
domain_concept('member state','Member State':'D0000198':[geoa,orgt]).
domain_concept('lee','Lee':'D0000199':[humn]).
domain_concept('minister of health','Minister of Health':'D0000200':[prog]).
domain_concept('operation','operation':'D0000201':[acty,topp]).
domain_concept('concentration of influenza virus','concentration of influenza virus':'D0000202':[comc,orga,fndg]).
domain_concept('transmission fitness','transmission fitness':'D0000203':[orga,fndg]).
domain_concept('natural sciences and engineering research council of canada','Natural Sciences and Engineering Research Council of Canada':'D0000204':[orgt]).
domain_concept('nserc','Natural Sciences and Engineering Research Council of Canada':'D0000204':[orgt]).
domain_concept('mathematics of information technology and complex systems','Mathematics of Information Technology and Complex Systems':'D0000205':[orgt]).
domain_concept('mitacs','Mathematics of Information Technology and Complex Systems':'D0000205':[orgt]).
domain_concept('infectivity','Infectivity':'D0000206':[orga]).
domain_concept('control intervention','Control Intervention':'D0000207':[gora,acty]).
domain_concept('australian commonwealth department of heath and aging','Australian Commonwealth Department of Heath and Aging':'D0000208':[orgt]).
domain_concept('hand hygiene','Hand hygiene':'D0000209':[inbe,hlca]).
domain_concept('mask wearing','Mask wearing':'D0000210':[inbe,hlca]).
domain_concept('ministry of health','Ministry of Health':'D0000211':[orgt]). 
domain_concept('long - lasting insecticidal nets','long-lasting insecticidal nets':'D0000212':[mnob]).
domain_concept('llin','long - lasting insecticidal nets':'D0000212':[mnob]).
domain_concept('malaria burden','malaria burden':'D0000213':[comc]).
domain_concept('insecticide - treated bed nets','insecticide-treated bed nets':'D0000214':[mnob]).
domain_concept('entomological inoculation rate','Entomological Inoculation Rate':'D0000215':[comc]).
domain_concept('bioattack','Bioattack':'D0000216':[hcpp]).
domain_concept('briefing','Briefing':'D0000217':[infc]).
domain_concept('office of regulatory affairs','Office of Regulatory Affairs':'D0000218':[orgt]).
domain_concept('office of food safety','Office of Food Safety':'D0000219':[orgt]).
domain_concept('buffalo','Buffalo':'D0000220':[geoa,orgt]).
domain_concept('handler','Handler':'D0000221':[humn]).
domain_concept('commissioner','Commissioner':'D0000222':[humn]).
domain_concept('center for food safety and applied nutrition','Center for Food Safety and Applied Nutrition':'D0000223':[orgt]).
domain_concept('cfsan','Center for Food Safety and Applied Nutrition':'D0000223':[orgt]).
domain_concept('watchdog','Watchdog':'D0000224':[humn]).
domain_concept('government accountability office','Government Accountability Office':'D0000225':[orgt]).
domain_concept('gao','Government Accountability Office':'D0000225':[orgt]).
domain_concept('federal emergency management agency','Federal Emergency Management Agency':'D0000226':[orgt]).
domain_concept('fema','Federal Emergency Management Agency':'D0000226':[orgt]).
domain_concept('committee on homeland security','Committee on Homeland Security':'D0000227':[orgt]).
domain_concept('associate director','Associate Director':'D0000228':[prog]).
domain_concept('food safety administration','Food Safety Administration':'D0000229':[orgt]).
domain_concept('fsa','Food Safety Administration':'D0000229':[orgt]).
domain_concept('advisory committee on immunization practices','Advisory Committee on Immunization Practices':'D0000230':[orgt]).
domain_concept('acip','Advisory Committee on Immunization Practices':'D0000230':[orgt]).
domain_concept('killed and live vaccines','Inactivated and Attenuated Vaccines':'D0000231':[phsu]).
domain_concept('inactivated flu vaccine and live','Inactivated and Attenuated Vaccines':'D0000231':[phsu]).
domain_concept('inactivated flu vaccine & live','Inactivated and Attenuated Vaccines':'D0000231':[phsu]).
domain_concept('national center for preparedness, detection, and control of infectious diseases','National Center for Preparedness, Detection, and Control of Infectious Diseases':'D0000232':[orgt]).
domain_concept('health alert notice','Health alert notice':'D0000233':[infc]).
domain_concept('health alert','Health alert notice':'D0000233':[infc]).
domain_concept('health advisory','Health advisory':'D0000234':[infc]).
domain_concept('news conference','News conference':'D0000235':[infc]).
domain_concept('spread of germ','spread of germs':'D0000236':[phpr,comc]).
domain_concept('health update','Health update':'D0000237':[infc]).
domain_concept('criminal or terrorist activity','Criminal or Terrorist Activity':'D0000238':[evnt]).
domain_concept('influenza a and b virus infection','Influenza A and B Virus Infection':'D0000239':[dsyn]).
domain_concept('influenza a and b','Influenza A and B Virus Infection':'D0000239':[dsyn]).
domain_concept('policymaking','Policymaking':'D0000240':[gora,acty]).
domain_concept('cabinet secretary','Cabinet Secretary':'D0000241':[prog]).
domain_concept('secretary of state','Secretary of State':'D0000242':[prog]).
domain_concept('scotland yard official','Scotland Yard Official':'D0000243':[prog]).
domain_concept('commission','Commission':'D0000244':[orgt]).
domain_concept('centers for disease control and protection','Centers for Disease Control and Protection':'D0000245':[orgt]).
domain_concept('pandemic influenza plan','Pandemic Influenza Plan':'D0000246':[infc,inpr]).
domain_concept('national institute for occupational safety and health','National Institute for Occupational Safety and Health':'D0000247':[orgt]).
domain_concept('niosh','National Institute for Occupational Safety and Health':'D0000247':[orgt]).
domain_concept('department of veterans affairs','Department of Veterans Affairs':'D0000248':[orgt]).
domain_concept('school closure','School closure':'D0000249':[gora]).
domain_concept('tax records','Tax records':'D0000250':[infc]).
domain_concept('bank account','Bank account':'D0000251':[infc]).
domain_concept('department of health','Department of Health':'D0000252':[orgt]).
domain_concept('health department','Department of Health':'D0000252':[orgt]).
domain_concept('council','Council':'D0000253':[orgt]).
domain_concept('department of the interior','Department of the Interior':'D0000254':[orgt]).
domain_concept('developing world','Developing world':'D0000255':[geoa,orgt]).
domain_concept('all - cause mortality','all-cause mortality':'D0000256':[comc]).
domain_concept('monitoring program','Monitoring Program':'D0000257':[infc,gora]).
domain_concept('pandemic containment','Pandemic containment':'D0000258':[gora]).
domain_concept('hrv - c','HRV-C':'D0000259':[virs]).
domain_concept('practitioner','Practitioner':'D0000260':[prog]).
domain_concept('vaccination campaign','vaccination campaign':'D0000261':[gora]).
domain_concept('fatality rate','Fatality rate':'D0000262':[comc]).
domain_concept('oseltamivir carboxylate','Oseltamivir':'D0000263':[orch,phsu]).
domain_concept('sewage treatment plant','sewage treatment plant':'D0000264':[phst]).
domain_concept('pyrosequencing','Pyrosequencing':'D0000265':[mbrt]).
domain_concept('antiviral resistance','Antiviral resistance':'D0000266':[phsf,clna]).
domain_concept('n95 respirator','N95 respirator':'D0000267':[mnob]).
domain_concept('n95','N95 respirator':'D0000267':[mnob]).
domain_concept('manufacturer','Manufacturer':'D0000268':[prog,orgt]).
domain_concept('vaccine program','Vaccine program':'D0000269':[gora]).
domain_concept('coinfection','Coinfection':'D0000270':[dsyn]).
domain_concept('co - infection','Coinfection':'D0000270':[dsyn]).
domain_concept('United States Government','United States Government':'D0000271':[orgt]).
domain_concept('U.S. Government','United States Government':'D0000271':[orgt]).
domain_concept('US Government','United States Government':'D0000271':[orgt]).
domain_concept('non - elderly person','non-elderly person':'D0000272':[aggp]).
domain_concept('non - elderly adult','non-elderly person':'D0000272':[aggp]).
domain_concept('non - elderly','non-elderly':'D0000273':[aggp,popg]).
domain_concept('pandemic influenza virus','pandemic influenza virus':'D0000275':[virs]).
domain_concept('2009 influenza a (h1n1) virus pandemic','2009 influenza A (H1N1) virus pandemic':'D0000276':[phpr,comc]).
domain_concept('2009 influenza pandemic','2009 influenza A (H1N1) virus pandemic':'D0000276':[phpr,comc]).
domain_concept('influenza a pandemic (h1n1) 2009','2009 influenza A (H1N1) virus pandemic':'D0000276':[phpr,comc]).
domain_concept('pandemic (h1n1) 2009','2009 influenza A (H1N1) virus pandemic':'D0000276':[phpr,comc]).
domain_concept('influenza a (h1n1) virus infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('influenza a (h1n1) flu','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('novel influenza a (h1n1) virus infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('novel influenza a virus infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('novel influenza a (h1n1) infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('novel h1n1 infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('novel h1n1 illness','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('novel h1n1 flu','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('2009 h1n1 influenza virus infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('2009 h1n1 influenza infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('2009 h1n1 influenza virus','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('2009 h1n1 influenza','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('2009 h1n1 illness','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('2009 h1n1 flu','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('influenza a (h1n1) infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('h1n1 virus infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('h1n1 infection','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('h1n1 influenza','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('h1n1 illness','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('h1n1 disease','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('h1n1 flu','influenza A (H1N1) virus infection':'D0000277':[dsyn]).
domain_concept('2009 pandemic influenza a (h1n1) virus infection','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('2009 pandemic h1n1 illness','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('influenza a pandemic (h1n1) 2009 virus infection','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('influenza a pandemic (h1n1) 2009 infection','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('pandemic 2009 (h1n1) influenza virus infection','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('pandemic (h1n1) 2009 influenza virus infection','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('pandemic (h1n1) 2009 virus infection','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('pandemic (h1n1) 2009 infection','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('pandemic h1n1 virus infection','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('pandemic h1n1 influenza','2009 pandemic influenza A (H1N1) virus infection':'D0000278':[dsyn]).
domain_concept('influenza epidemic','Influenza epidemic':'D0000279':[phpr,comc]).
domain_concept('pandemic influenza','pandemic influenza':'D0000280':[dsyn]).
domain_concept('pandemic flu','pandemic influenza':'D0000280':[dsyn]).
domain_concept('household member','household member':'D0000281':[famg,humn]).
domain_concept('influenza pandemic','Influenza pandemic':'D0000282':[phpr,comc]).
domain_concept('flu pandemic','Influenza pandemic':'D0000282':[phpr,comc]).
domain_concept('seasonal influenza vaccine','seasonal influenza vaccine':'D0000283':[phsu]).
domain_concept('seasonal flu vaccine','seasonal influenza vaccine':'D0000283':[phsu]).


%domain_concept('h1n1 avian flu virus', [virs]).
%domain_concept('h5n1 virus', [virs]). --already a synonym for Influenza A Virus, H5N1 subtype
%domain_concept('antibiotic resistance', [orga]). % this is already in MT.
%domain_concept('decision', [inpr]). % this is already in MT.
%domain_concept('domestic partner', [famg]). % already in MT.
%domain_concept('expert system', [mnob,inpr]). % already in MT.
%domain_concept('personal protective equipment', [mnob]). %already exists in MT.
%domain_concept('thyroid storm', [dsyn]). % already in MT as a synonym of Thyroid Crisis.


