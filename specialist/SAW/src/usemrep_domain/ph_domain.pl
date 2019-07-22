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
% file:	    ph_domain.pl
% module:   ph_domain.pl
:- module(ph_domain, [
	domain_name/1,
	domain_concept/2,
	domain_replace/2,
	domain_semtype/3
]).
domain_name(ph).
domain_semtype(comc,'Community Characteristics',phen).
domain_semtype(infc,'Information Construct',conc).
domain_semtype(venu,'Venue',conc).
domain_semtype(hlbe,'Healthy Behavior',acti).
domain_semtype(prty,'Program Type',conc).
% ---Wrong Concept name for "C1556110" in Replace---------
% ---Correct Concept name is "NUCC Provider Codes - Agencies"---------
% domain_replace('Agencies':'C1556110':[inpr],'Agencies':'C1556110':[orgt]).
domain_replace('Participant observation':'C0558038':[hlca],'Participant observation':'C0558038':[acty]). % Public Health
domain_replace('Employee':'C0599987':[prog,humn],'Employee':'C0599987':[popg,humn]).
domain_replace('Physical Therapist - NUCCProviderCodes':'C0871082':[inpr],'Physical Therapist':'C0871082':[prog]).
domain_replace('Current non-drinker of alcohol':'C0457801':[socb],'Abstinence':'C0457801':[inbe]).
domain_replace('social inequality':'C0680381':[qlco],'social inequality':'C0680381':[fndg]).
domain_replace('Inequalities':'C0242503':[qlco],'Inequalities':'C0242503':[fndg]).
domain_replace('Incentives':'C0021147':[qlco],'Incentives':'C0021147':[mnob]).
domain_replace('Questionnaires':'C0034394':[inpr],'Questionnaires':'C0034394':[infc]).
domain_replace('Authority':'C0599437':[socb],'Authority':'C0599437':[prog,orgt]).
domain_replace('Smoking Prevention':'C1519385':[resa],'Smoking Prevention':'C1519385':[topp,hlca]).
domain_replace('Advertisement':'C0949214':[inpr],'Advertisement':'C0949214':[infc]).
domain_replace('jurisdiction':'C0680647':[idcn],'jurisdiction':'C0680647':[geoa,orgt]).
domain_replace('Suggestion':'C0038659':[topp],'Suggestion':'C0038659':[mnob]).
domain_replace('Workplace':'C0162579':[spco],'Workplace':'C0162579':[mnob,orgt]).
domain_replace('Newspapers':'C0027989':[inpr,mnob],'Newspapers':'C0027989':[infc,mnob]).
domain_replace('Paper':'C0030351':[mnob],'Paper':'C0030351':[infc]).
domain_replace('Awareness':'C0004448':[menp],'Awareness':'C0004448':[hlbe,menp]).
domain_replace('Commitment':'C0870312':[menp],'Commitment':'C0870312':[hlbe]).
domain_replace('Data':'C1511726':[idcn],'Data':'C1511726':[infc]).
domain_replace('Biological Models':'C0026339':[inpr,resd],'Models':'C0026339':[resd]).
domain_replace('Discussion':'C0557061':[topp],'Discussion':'C0557061':[socb,acty]).
% ---Wrong type name for "C0341618" used in Replace---------
% ---Correct type is "humn,prog"---------
% domain_replace('Counsel - legal':'C0341618':[prog],'Guidance':'C0341618':[hlca]). % June 2011, Entry in exceptions.pl does not block this
domain_replace('Disease Management':'C0376636':[hlca],'Disease Management':'C0376636':[hlbe,hlca]).
domain_replace('Milieu Therapy':'C0026118':[topp],'Environmental Intervention':'C0026118':[topp,hlca]).
domain_replace('disease transmission':'C0242781':[patf],'disease transmission':'C0242781':[phpr,comc]).
domain_replace('AUSTRALIAN':'C0238711':[fndg],'AUSTRALIAN':'C0238711':[popg]).
domain_replace('experience':'C0596545':[menp],'experience':'C0596545':[menp,orga]).
domain_replace('Rural Communities':'C0086944':[popg,humn],'Rural Communities':'C0086944':[popg]).
domain_replace('Facility':'C1547538':[idcn],'Facility':'C1547538':[orgt,mnob]).
domain_replace('Morbidity - disease rate':'C0026538':[qnco],'Morbidity':'C0026538':[comc,fndg]).
domain_replace('Generations':'C0079411':[tmco],'Generations':'C0079411':[popg]).
domain_replace('Germ':'C1517528':[idcn],'Germ':'C1517528':[orgm]).
domain_replace('Handwashing':'C0018581':[dora,topp],'Handwashing':'C0018581':[hlca]).
domain_replace('Health Authority':'C1273803':[hcro],'Health Authority':'C1273803':[orgt,prog]).
domain_replace('Health Status':'C0018759':[qlco],'Health Status':'C0018759':[comc]).
domain_replace('Incidence':'C0021149':[qnco],'Incidence':'C0021149':[comc]).
domain_replace('disease risk':'C0683481':[qnco],'disease risk':'C0683481':[comc,orga]).
domain_replace('Cigarette Smoking':'C0700219':[dora],'Cigarette Smoking':'C0700219':[fndg]).
domain_replace('Information':'C1533716':[idcn],'Information':'C1533716':[infc]).
domain_replace('Information, health':'C0850397':[hlca],'Health information':'C0850397':[infc]). % Public Health
domain_replace('Methods':'C0025663':[inpr],'Methods':'C0025663':[mnob]).
domain_replace('Methodology':'C0025664':[inpr],'Methodology':'C0025664':[mnob]).
% ---Wrong Concept name for "C0032074" in Replace---------
% ---Correct Concept name is "Cognitive function: planning"---------
% domain_replace('Planning':'C0032074':[menp],'Planning':'C0032074':[acty,inpr]).
domain_replace('Region':'C0205147':[spco],'Region':'C0205147':[geoa]).
domain_replace('research study':'C0681814':[resa],'research study':'C0681814':[resa,infc]).
domain_replace('Program Evaluation':'C0033336':[ocac],'Program Evaluation':'C0033336':[resa,infc]).
domain_replace('research':'C0035168':[resa],'research':'C0035168':[resa,infc]).
domain_replace('transmission process':'C1521797':[npop],'transmission process':'C1521797':[phpr,comc]).
domain_replace('Human to human transmission':'C1444005':[clas],'Human to human transmission':'C1444005':[phpr,comc]).
% ---Wrong Concept name for "C0332261" in Replace---------
% ---Correct Concept name is "Spreading"---------
% domain_replace('spreading':'C0332261':[qlco],'spreading':'C0332261':[phpr,comc]).
domain_replace('Owner':'C1546511':[idcn],'Owner':'C1546511':[popg,orgt]).
% ---Wrong type name for "C0242170" used in Replace---------
% ---Correct type is "humn,prog"---------
% domain_replace('Policy Makers':'C0242170':[prog],'Policy Makers':'C0242170':[orgt,prog]).
domain_replace('viral transmission':'C1160716':[celf],'viral transmission':'C1160716':[phpr,comc]).
domain_replace('Century':'C0719214':[phsu,vita],'century':'C0719214':[tmco]).
domain_replace('Press Releases (PT)':'C0282424':[inpr],'Press Releases':'C0282424':[infc]).
domain_replace('Messages':'C0470166':[inpr],'Messages':'C0470166':[infc]).
% ---Wrong Concept name for "C1552433" in Replace---------
% ---Correct Concept name is "Clinic / Center - Military"---------
% domain_replace('Military':'C1552433':[hcro,mnob],'Military':'C1552433':[popg,orgt]).
% ---Wrong Concept name for "C0205339" in Replace---------
% ---Correct Concept name is "Reinfection"---------
% domain_replace('reinfection':'C0205339':[ftcn],'reinfection':'C0205339':[phpr,dsyn]).
% ---Wrong type name for "C0033176" used in Replace---------
% ---Correct type is "humn,popg"---------
% domain_replace('Private Sector':'C0033176':[popg],'Private Sector':'C0033176':[orgt]).
% ---Wrong type name for "C0034035" used in Replace---------
% ---Correct type is "humn,popg"---------
% domain_replace('Public Sector':'C0034035':[popg],'Public Sector':'C0034035':[orgt]).
% ---Wrong type name for "C0871489" used in Replace---------
% ---Correct type is "grup,humn"---------
% domain_replace('Teams':'C0871489':[grup],'Teams':'C0871489':[prog,orgt]).
domain_replace('Technology':'C0039421':[ocdi],'Technology':'C0039421':[mnob,ocdi]).
% ---Wrong type name for "C0008059" used in Replace---------
% ---Correct type is "aggp,humn,inpr"---------
% domain_replace('Child':'C0008059':[aggp,inpr],'Child':'C0008059':[humn]).
% ---Wrong Concept name for "C0086282" in Replace---------
% ---Correct Concept name is "Family member"---------
% domain_replace('family member':'C0086282':[famg],'Family member':'C0086282':[humn,famg]).
domain_replace('United States National Institutes of Health':'C0027468':[hcro],'National Institutes of Health':'C0027468':[orgt]).
domain_replace('Readiness':'C1318963':[fndg],'Readiness':'C1318963':[acty]).
domain_replace('Soaps':'C0037392':[lipd,mnob],'Soaps':'C0037392':[mnob]).
domain_replace('Immunocompromised Host':'C0085393':[fndg],'Immunocompromised Host':'C0085393':[humn,orgm]).
domain_replace('Diagnosis':'C0011900':[hlca],'Diagnosis':'C0011900':[diap]).
domain_replace('Communication Barriers':'C0009454':[idcn],'Communication Barriers':'C0009454':[fndg]).
domain_replace('Health Resources':'C0018741':[idcn],'Health Resources':'C0018741':[mnob]).
domain_replace('Resources':'C0035201':[idcn],'Resources':'C0035201':[mnob]).
domain_replace('Death Rate':'C0205848':[qnco],'Death Rate':'C0205848':[comc]).
domain_replace('Rural':'C0240919':[fndg],'Rural':'C0240919':[qlco]).
domain_replace('attack rate':'C0683920':[qnco],'attack rate':'C0683920':[comc]).
domain_replace('Techniques':'C0449851':[ftcn],'Techniques':'C0449851':[mnob]).
% ---Wrong Concept name for "C0243095" in Replace---------
% ---Correct Concept name is "Finding"---------
% domain_replace('finding':'C0243095':[ftcn],'Finding':'C0243095':[fndg]).
domain_replace('Pamphlets':'C0030258':[mnob,inpr],'Pamphlets':'C0030258':[infc,mnob]).
domain_replace('Warning Signs':'C0871598':[gora],'Warning Signs':'C0871598':[sosy,gora]).
domain_replace('Publications':'C0034036':[mnob,inpr],'Publications':'C0034036':[infc]).
domain_replace('Advisory Committees':'C0162458':[gora],'Advisory Committees':'C0162458':[orgt]).
domain_replace('Nasal Spray':'C0461725':[bodm],'Nasal Spray':'C0461725':[clnd]).
domain_replace('Nasal drops':'C0991524':[bodm],'Nasal drops':'C0991524':[clnd]).
domain_replace('Magazines':'C0162443':[inpr,mnob],'Magazines':'C0162443':[infc,mnob]).
domain_replace('Applicants':'C0696628':[phsu],'Applicants':'C0696628':[popg,humn]).
domain_replace('Medical record, device':'C0025102':[inpr,mnob],'Medical record':'C0025102':[infc]).
domain_replace('workforce':'C0024752':[ftcn],'workforce':'C0024752':[popg]).
domain_replace('Health':'C0018684':[idcn],'Health':'C0018684':[hlbe]).
domain_replace('Reaction':'C0443286':[ftcn],'Reaction':'C0443286':[clna]).
domain_replace('Teleconference':'C0039450':[mcha],'Teleconference':'C0039450':[acty]).
domain_replace('Conferences':'C0086047':[hcpp],'Conference':'C0086047':[acty,venu]).
domain_replace('Newsletters':'C0027988':[inpr,mnob],'Newsletters':'C0027988':[infc]).
% ---Wrong Concept name for "C1516908" in Replace---------
% ---Correct Concept name is "Epidemiologist"---------
% domain_replace('Epidemiologists':'C1516908':[bmod],'Epidemiologists':'C1516908':[prog]).
domain_replace('Placebos':'C0032042':[topp,medd],'Placebos':'C0032042':[medd]).
domain_replace('Benefits':'C0814225':[qnco],'Benefits':'C0814225':[mnob,qnco]).
domain_replace('Crisis':'C0231224':[fndg],'Crisis':'C0231224':[phpr]).
domain_replace('Staff Member':'C1552089':[idcn],'Staff Member':'C1552089':[popg,humn]).
domain_replace('beneficiary':'C1550502':[idcn],'beneficiary':'C1550502':[humn,popg]).
% ---Wrong type name for "C0679670" used in Replace---------
% ---Correct type is "humn,popg"---------
% domain_replace('network':'C0679670':[popg],'network':'C0679670':[mnob,orgt]).
domain_replace('Subgroup':'C1515021':[clas],'Subgroup':'C1515021':[grup]).
domain_replace('Screening procedure':'C0220908':[hlca],'Screening procedure':'C0220908':[diap,hlca]).
domain_replace('Resident':'C1549439':[inpr],'Resident':'C1549439':[humn,popg]).
domain_replace('Developed Countries':'C0282613':[qlco],'Developed Countries':'C0282613':[geoa]). % From here on, entries Climate & Health
domain_replace('Health Hazards':'C0079483':[hops],'Health Hazards':'C0079483':[comc]).
domain_replace('food shortage':'C0598456':[qnco],'food shortage':'C0598456':[npop,fndg]).
domain_replace('Ingested food':'C1179481':[bdsu],'Ingested food':'C1179481':[food]).
% ---Wrong Concept name for "C347950" in Replace---------
% ---Correct Concept name is "null"---------
% domain_replace('Asthma attack NOS':'C347950':[dsyn],'Asthma attack':'C347950':[dsyn]).
% ---Wrong Concept name for "C0185251" in Replace---------
% ---Correct Concept name is "Joint capsule excision"---------
% domain_replace('metropolitan area':'C0185251':[spco],'metropolitan area':'C0185251':[geoa]).
domain_replace('urban area':'C0178876':[spco],'urban area':'C0178876':[geoa]).
domain_replace('Health Benefits':'C0086387':[qnco],'Health Benefits':'C0086387':[mnob]).
% ---Wrong type name for "C0392363" used in Replace---------
% ---Correct type is "aggp,humn"---------
% domain_replace('Elderly person':'C0392363':[aggp],'Senior citizens':'C0392363':[popg]).
domain_replace('treatment barriers':'C0679881':[qlco],'treatment barriers':'C0679881':[fndg]).
domain_replace('poor health':'C0683321':[fndg],'poor health':'C0683321':[comc,fndg]).
domain_replace('health risk assessment':'C0679809':[topp],'health risk assessment':'C0679809':[diap]).
domain_replace('Obesity screen':'C0740216':[topp],'Obesity screen':'C0740216':[diap]).
domain_replace('Infant Health':'C0205806':[hlca],'Infant Health':'C0205806':[comc]).
domain_replace('Health Services Accessibility':'C0018748':[qlco],'Health Services Accessibility':'C0018748':[mnob]). % check semtype with Tom
domain_replace('Evaluation':'C0220825':[ftcn],'Evaluation':'C0220825':[mnob,infc]).
domain_replace('Approach':'C0449445':[spco],'Approach':'C0449445':[prty,mnob]). % Already in domain file, but we need a different semtype
domain_replace('strategy':'C0679199':[menp],'strategy':'C0679199':[prty,mnob]). % same as comment above
domain_replace('health disparity':'C1171307':[hlca],'health disparity':'C1171307':[fndg]). % grpa? 
domain_replace('Health Outcomes':'C1550208':[inpr],'Health Outcomes':'C1550208':[fndg]). % mnob?
domain_replace('Arthritis Pain':'C0718667':[orch,phsu],'Arthritis Pain':'C0718667':[sosy]).
domain_replace('Medical care, unspecified':'C0496675':[fndg],'Medical care':'C0496675':[hlca,topp]).
domain_replace('business partnership':'C0680935':[orgt],'partnership':'C0680935':[orgt]).
domain_replace('risk factors':'C0035648':[qnco],'risk factors':'C0035648':[comc,grpa,clna]).
domain_replace('prevention program':'C0679717':[inpr],'prevention program':'C0679717':[prty,hlca,mnob]).
domain_replace('low socioeconomic status':'C1328812':[qnco],'low socioeconomic status':'C1328812':[grpa,qlco]).
domain_replace('Coronary heart disease risk':'C1277690':[inpr],'Coronary heart disease risk':'C1277690':[comc,grpa,clna]).
domain_replace('cardiovascular disorder risk':'C1113685':[fndg],'cardiovascular disorder risk':'C1113685':[comc,grpa,clna]).
domain_replace('Nutrition Policy':'C0242888':[inpr],'Nutrition Policy':'C0242888':[prty,mnob,rnlw]). % also gora?
domain_replace('Cessation of smoking':'C0085134':[inbe],'Cessation of smoking':'C0085134':[hlbe,hlca]).
domain_replace('Tobacco Use Cessation':'C0600549':[inbe],'Tobacco Use Cessation':'C0600549':[hlbe]).
domain_replace('Initiative':'C0424093':[menp],'Initiative':'C0424093':[prty]).
% ---Wrong Concept name for "C0424093" in Replace---------
% ---Correct Concept name is "Initiative"---------
% domain_replace('Step':'C0424093':[ftcn],'Step':'C0424093':[prty,acty]).
domain_replace('prevention or treatment protocol':'C0814279':[inpr],'prevention or treatment protocol':'C0814279':[topp,hlca]).
domain_replace('Tuberculosis screening':'C0420004':[lbpr],'Tuberculosis screening':'C0420004':[lbpr,hlca]).
domain_replace('Intervention regimes':'C1273869':[hlca],'Interventions':'C1273869':[prty,hlca]).
domain_replace('intervention program':'C0599917':[topp],'intervention program':'C0599917':[prty,hlca,topp]).
domain_replace('prevention campaign':'C0679719':[inpr],'prevention campaign':'C0679719':[prty,hlca]).
domain_replace('program planning, implementation, and evaluation':'C0680833':[ocac],'program planning, implementation, and evaluation':'C0680833':[acty]).
domain_replace('program implementation':'C0680839':[ocac],'program implementation':'C0680839':[acty]).
% ---Wrong Concept name for "C0034366" in Replace---------
% ---Correct Concept name is "Qatar"---------
% domain_replace('Recommendation':'C0034366':[idcn],'Recommendation':'C0034366':[prty,hlca]).
domain_replace('Physical activity':'C0026606':[dora],'Physical activity':'C0026606':[hlbe,acty]).
domain_replace('Community':'C0009462':[geoa],'Community':'C0009462':[venu,geoa,popg]).
domain_replace('Environment':'C0014406':[spco],'Environment':'C0014406':[venu]).
domain_replace('Crisis Intervention':'C0010332':[topp],'Crisis Intervention':'C0010332':[prty,topp]).
domain_replace('Course':'C0750729':[tmco],'Course':'C0750729':[edac,hlca]).
domain_replace('Developers':'C0180397':[irda],'Developers':'C0180397':[orgt,prog]).
domain_replace('Patient education (procedure)':'C0030688':[edac],'Patient education':'C0030688':[edac]).
% ---Wrong type name for "C0520510" used in Replace---------
% ---Correct type is "sbst"---------
% domain_replace('Materials':'C0520510':[subs],'Materials':'C0520510':[mnob]).
domain_replace('Health Communication':'C1512347':[hlca],'Health Communication':'C1512347':[mnob,hlca]).
domain_replace('healthful behavior':'C0679786':[inbe],'Healthful behavior':'C0679786':[hlbe]).
domain_replace('alliance':'C0680207':[socb],'church coalition':'C0680207':[orgt]).
domain_replace('Food Supply':'C0016491':[qnco],'Food Supply':'C0016491':[food,mnob]).
domain_replace('School (environment)':'C0036375':[orgt,mnob],'Schools':'C0036375':[orgt,mnob]).
domain_replace('Low income':'C1331016':[qlco],'Low income':'C1331016':[qlco,grpa]).
domain_replace('Health Promotion':'C0018738':[hlca],'Health Promotion':'C0018738':[hlbe,hlca]).
domain_replace('Physical Fitness':'C0031812':[idcn],'Physical Fitness':'C0031812':[hlbe,grpa]).
domain_replace('Clinical Practice Guideline':'C0282451':[inpr],'Clinical Practice Guideline':'C0282451':[gora,mnob,prty]).
domain_replace('Guidelines':'C0162791':[inpr],'Guidelines':'C0162791':[gora,mnob,prty]).
domain_replace('Exertion':'C0015264':[orgf],'Effort':'C0015264':[orgf,prty]).
domain_replace('Non-smoker':'C0337672':[fndg],'Non-smokers':'C0337672':[popg]).
domain_replace('Smoker':'C0337664':[fndg],'Smoker':'C0337664':[popg]).
domain_replace('Tobacco':'C0040329':[phsu,orch,hops],'Tobacco':'C0040329':[hops]).
domain_replace('ethnic':'C0680174':[fndg],'ethnic population group':'C0680174':[popg]).
domain_replace('counselor':'C1561602':[fndg],'counselor':'C1561602':[humn,popg]).
domain_replace('Bone Density':'C0005938':[lbtr],'Bone Density':'C0005938':[lbtr,clna]).
domain_replace('cultural competence':'C0679748':[socb],'cultural competence':'C0679748':[inbe,fndg]).
domain_replace('Professional Competence':'C0033278':[qlco],'Professional Competence':'C0033278':[fndg]).
domain_replace('Services':'C0557854':[ftcn],'Services':'C0557854':[mnob]).
domain_replace('Telephone':'C0039457':[mnob],'Telephone':'C0039457':[venu,mnob]).
domain_replace('clinician':'C1550470':[idcn],'clinician':'C1550470':[prog]).
domain_replace('Clinical':'C0205210':[qlco],'Clinical Setting':'C0205210':[venu]).
domain_replace('Printed Media':'C0033159':[phob],'Printed Media':'C0033159':[venu]).
domain_replace('Electronic':'C0013850':[mnob],'Electronic':'C0013850':[venu]).
domain_replace('Indigenous':'C1512704':[grpa],'Indigenous':'C1512704':[qlco]).
domain_replace('Smoking cessation assistance':'C1273715':[topp],'Smoking cessation assistance':'C1273715':[hlca]).
domain_replace('Tobacco use':'C0543414':[fndg],'Tobacco use':'C0543414':[inbe,fndg]).
% ---Wrong Concept name for "C0679871" in Replace---------
% ---Correct Concept name is "preventive health care"---------
% domain_replace('Preventive Health Care':'C0679871':[topp],'Preventive Health Care':'C0679871':[hlca]).
domain_replace('Universities':'C0041740':[mnob,orgt],'Universities':'C0041740':[orgt]).
domain_replace('Unsafe Sex':'C0556482':[socb],'Unsafe Sex':'C0556482':[inbe,socb]).
% ---Wrong type name for "C0038951" used in Replace---------
% ---Correct type is "resa"---------
% domain_replace('Surveys':'C0038951':[inpr],'Surveys':'C0038951':[resa,infc]).
domain_replace('Problems':'C1546466':[idcn],'Problems':'C1546466':[fndg]).
domain_replace('subject':'C1550501':[idcn],'subject':'C1550501':[humn]).
% ---Wrong type name for "C0039593" used in Replace---------
% ---Correct type is "resa"---------
% domain_replace('Testing':'C0039593':[ftcn],'Testing':'C0039593':[resa]).
% ---Wrong type name for "C0085756" used in Replace---------
% ---Correct type is "humn,popg"---------
% domain_replace('African American':'C0085756':[popg],'African American':'C0085756':[qlco,popg]).
domain_replace('Public Housing':'C0034028':[gora],'Public Housing':'C0034028':[phst,mnob]).
% ---Wrong type name for "C0683975" used in Replace---------
% ---Correct type is "fndg,qlco"---------
% domain_replace('cultural differences':'C0683975':[qlco],'cultural differences':'C0683975':[fndg]).
domain_replace('Ambulatory Care Facilities':'C0002424':[hcro,mnob],'Clinic':'C0002424':[hcro]).
% ---Wrong Concept name for "C0850254" in Replace---------
% ---Correct Concept name is "Advice/education, smoking"---------
% domain_replace('Advice about smoking cessation':'C0850254':[topp],'Smoking cessation advice':'C0850254':[hlca]).
% ---Wrong type name for "C0087134" used in Replace---------
% ---Correct type is "humn,popg"---------
% domain_replace('Uninsured':'C0087134':[popg],'Uninsured':'C0087134':[popg,qlco]).
domain_replace('Underinsured':'C0087132':[popg],'Underinsured':'C0087132':[popg,qlco]).
domain_replace('Communication Programs':'C0009457':[inpr],'Communication Programs':'C0009457':[prty]).
domain_replace('Sociocultural Factors':'C0871911':[idcn],'Sociocultural Factors':'C0871911':[mnob]).
domain_replace('Documentation':'C0920316':[inpr],'Documentation':'C0920316':[infc]).
domain_replace('Married':'C0555047':[fndg],'Married':'C0555047':[qlco,fndg]).
domain_replace('Caregiver':'C0085537':[prog,humn],'Caregiver':'C0085537':[popg,humn]).
% ---Wrong Concept name for "C1317574" in Replace---------
% ---Correct Concept name is "NOTE"---------
% domain_replace('Note':'C1317574':[inpr],'Note':'C1317574':[infc]).
domain_replace('RECENT IMMIGRANT':'C0240867':[fndg],'RECENT IMMIGRANT':'C0240867':[popg,humn]).
% ---Wrong Concept name for "C0004951" in Replace---------
% ---Correct Concept name is "Beliefs"---------
% domain_replace('Religious Beliefs':'C0004951':[idcn],'Religious Beliefs':'C0035045':[menp]).
domain_replace('Beliefs':'C0004951':[idcn],'Beliefs':'C0004951':[menp]).
domain_replace('educational intervention':'C0281163':[topp],'educational intervention':'C0281163':[edac,prty]).
domain_replace('Epidemic':'C0014499':[phpr],'Epidemic':'C0014499':[phpr,fndg]).
domain_replace('Tobacco abuse prevention':'C1171222':[topp],'Tobacco abuse prevention':'C1171222':[topp,hlca]).
domain_replace('Treatment outcome':'C0085415':[qlco],'Treatment outcome':'C0085415':[fndg]).
domain_replace('Young':'C0332239':[tmco],'Young':'C0332239':[qlco,aggp]).
domain_replace('Clinical Data':'C1516606':[inpr],'Clinical Data':'C1516606':[infc]).
domain_replace('treatment issues':'C0679859':[inpr],'treatment issues':'C0679859':[fndg]).
% ---Wrong Concept name for "C1553514" in Replace---------
% ---Correct Concept name is "face-to-face"---------
% domain_replace('face - to - face':'C1553514':[idcn],'face-to-face':'C1553514':[qlco]).
domain_replace('perceived risk':'C0814102':[qlco],'perceived risk':'C0814102':[menp]).
domain_replace('Adherence to treatment plan':'C0516958':[fndg],'Adherence to treatment plan':'C0516958':[hlca,fndg]).
domain_replace('Risk':'C0035647':[qlco],'Risk':'C0035647':[idcn]).
% ---Wrong type name for "C0402883" used in Replace---------
% ---Correct type is "humn,prog"---------
% domain_replace('Tailor':'C0402883':[prog],'Tailor':'C0402883':[idcn]).
domain_replace('Encounter due to tobacco use':'C0040335':[fndg],'Tobacco use':'C0040335':[fndg]).
domain_replace('Able':'C1299581':[fndg],'Able':'C1299581':[ftcn]).
domain_replace('Introduction procedure':'C1293116':[hlca],'Introduction procedure':'C1293116':[ftcn]).
domain_replace('Apartment building':'C0557593':[mnob],'Apartment building':'C0557593':[ftcn]).
% ---Wrong type name for "C0240816" used in Replace---------
% ---Correct type is "humn,prog"---------
% domain_replace('Prostitute NOS':'C0240816':[humn],'Sex worker':'C0240816':[humn]).
domain_replace('Educational process of instructing':'C0039401':[edac],'Educational activity':'C0039401':[edac]).
% ---Wrong type name for "C0872261" used in Replace---------
% ---Correct type is "hcro"---------
% domain_replace('repository':'C0872261':[orgt],'repository':'C0872261':[idcn]).
domain_replace('state of health':'C0683314':[fndg],'state of health':'C0683314':[idcn]).
domain_replace('Identifying goals':'C0557971':[hlca],'Identifying goals':'C0557971':[ftcn]).
domain_replace('informant':'C1550484':[idcn],'informant':'C1550484':[humn]).
domain_replace('Policy':'C0242456':[inpr],'Policy':'C0242456':[prty]).
% ---Wrong type name for "C0221460" used in Replace---------
% ---Correct type is "humn,prog"---------
% domain_replace('Farmer, unspecified':'C0221460':[prog],'Farmer':'C0221460':[prog]).
domain_replace('Feeling content':'C0423896':[menp],'Content':'C0423896':[infc,menp]).
domain_replace('informant':'C1550484':[idcn],'informant':'C1550484':[popg]).
domain_replace('Coach':'C0557773':[mnob],'Coach':'C0557773':[prog]).
domain_replace('Body Weight decreased':'C0043096':[fndg],'Body Weight decreased':'C0043096':[hlbe,fndg]).
domain_replace('Smoking':'C0037369':[inbe],'Smoking':'C0037369':[fndg]).
domain_replace('criteria':'C0243161':[inpr],'criteria':'C0243161':[mnob]).
domain_replace('Knowledge':'C0376554':[inpr],'Knowledge':'C0376554':[mnob]).
% ---Wrong Concept name for "CUI" in Replace---------
% ---Correct Concept name is "null"---------
% domain_replace('PreferredName':'CUI':[hcro],'PreferredName:CUI:orgt,hcro':'':[]).
% domain_replace('PreferredName':'CUI':[humn],',PreferredName:CUI:humn,popg,grup':'':[]).% ---Wrong Concept name for "CUI" in Replace---------
% ---Correct Concept name is "null"---------
% domain_replace('PreferredName':'CUI':[humn],',PreferredName:CUI:humn,popg,grup':'':[]).
% ---Wrong Concept name for "CUI" in Replace---------
% ---Correct Concept name is "null"---------
% domain_replace('PreferredName':'CUI':[shro],'PreferredName:CUI:orgt,shro':'':[]).
domain_concept('promotion of health','Health Promotion':'C0018738':[hlbe,hlca]).
domain_concept('public health guidelines','Guidelines':'C0162791':[gora,mnob,prty]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('preventive care','Preventive Health Care':'C0679871':[hlca]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('preventive healthcare','Preventive Health Care':'C0679871':[hlca]).
domain_concept('prevention intervention','prevention or treatment protocol':'C0814279':[topp,hlca]).
domain_concept('disparities in health Status','health disparity':'C1171307':[fndg]).
domain_concept('disparities in health','health disparity':'C1171307':[fndg]).
domain_concept('healthcare disparities','health disparity':'C1171307':[fndg]).
domain_concept('health care disparities','health disparity':'C1171307':[fndg]).
domain_concept('health status disparities','health disparity':'C1171307':[fndg]).
domain_concept('health inequality','health disparity':'C1171307':[fndg]).
domain_concept('civil rights act','civil rights law':'C0680515':[rnlw]).
domain_concept('medical attention','Medical care':'C0496675':[hlca,topp]).
domain_concept('school of medicine','Schools, Medical':'C0036378':[hcro,mnob]).
domain_concept('nutrition professional','Nutritionist':'C0237083':[prog]).
domain_concept('health risk factor','risk factors':'C0035648':[comc,grpa,clna]).
domain_concept('risk factor','risk factors':'C0035648':[comc,grpa,clna]).
domain_concept('low - ses','low socioeconomic status':'C1328812':[qlco,grpa]).
domain_concept('low socio - economic status','low socioeconomic status':'C1328812':[qlco,grpa]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('commercial drivers','Drivers of Vehicles':'C0684312':[popg]).
domain_concept('coronary risk','Coronary heart disease risk':'C1277690':[comc,grpa,clna]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('older people','Elderly person':'C0392363':[aggp]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('elders','Elderly person':'C0392363':[aggp]).
domain_concept('cardiovascular disease risk','cardiovascular disorder risk':'C1113685':[comc,grpa,clna]).
domain_concept('cardiovascular risk','cardiovascular disorder risk':'C1113685':[comc,grpa,clna]).
domain_concept('workshops','Educational workshop':'C0242262':[edac]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('population subgroups','Population Group':'C1257890':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('sub - population','Population Group':'C1257890':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('tobacco cessation counseling','Smoking cessation advice':'C0850254':[hlca]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('subpopulation','Population Group':'C1257890':[popg]).
domain_concept('smoking cessation','Cessation of smoking':'C0085134':[hlbe,hlca]).
domain_concept('quit smoking','Cessation of smoking':'C0085134':[hlbe,hlca]).
domain_concept('quitting','Cessation of smoking':'C0085134':[hlbe,hlca]).
domain_concept('quit','Cessation of smoking':'C0085134':[hlbe,hlca]).
domain_concept('primary care','Primary Health Care':'C0033137':[hlca]).
domain_concept('healthy behavior','Healthful behavior':'C0679786':[hlbe]).
domain_concept('setting','Environment':'C0014406':[venu]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('nutritional education','Health education - diet':'C0199401':[hlca,edac,topp]).
domain_concept('medical evaluation','Medical assessment':'C0582103':[hlca]).
domain_concept('tb screening','Tuberculosis screening':'C0420004':[lbpr,hlca]).
domain_concept('screening','Screening procedure':'C0220908':[diap,hlca]).
domain_concept('low - income','Low income':'C1331016':[qlco,grpa]).
domain_concept('chronic condition','Chronic Disease':'C0008679':[dsyn]).
domain_concept('nyc','New York City':'C0027977':[geoa]).
domain_concept('ny','New York':'C0027976':[geoa]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('health professions workforce','Health Personnel':'C0018724':[prog]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('hospital healthcare worker','Health Personnel':'C0018724':[prog]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('wellness staff','Health Personnel':'C0018724':[prog]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('policymaker','Policy Makers':'C0242170':[orgt,prog]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('public health official','public health officer':'C0401875':[prog]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('mortality rate','Death Rate':'C0205848':[comc,fndg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('mortality','human mortality':'C0178686':[comc,fndg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('adult population','adult':'C0001675':[aggp]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('group of the population','Population Group':'C1257890':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('individuals','Persons':'C0027361':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('racial minority groups','Minority Groups':'C0026192':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('minorities','Minority Groups':'C0026192':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('ethnic minority group','Ethnic group':'C0015031':[popg]).
domain_concept('community health workers','Community Workers':'C0009484':[prog,humn]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('morbidity','human morbidity':'C0178685':[comc,fndg]).
domain_concept('alcohol use','Alcohol abuse':'C0085762':[mobd]).
domain_concept('use of marijuana','Marijuana Smoking':'C0024810':[inbe]).
domain_concept('drug use','Drug usage':'C0242510':[mobd]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('immigrant population','Immigrants':'C0282163':[popg]).
domain_concept('preventive service','Preventive service healthcare':'C0199175':[hlca]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('internal medicine resident physician','Internal medicine physicians':'C1555750':[prog,humn]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('resident physicians','Resident (physician)':'C1320928':[prog]).
domain_concept('Tobacco Cessation','Tobacco Use Cessation':'C0600549':[hlbe]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('tobacco use history','History of tobacco use':'C0841002':[fndg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('counseling session','Specific patient counseling session':'C0199393':[hlca]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('whites','Caucasians':'C0043157':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('smoking cessation advice','Smoking cessation advice':'C0850254':[hlca]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('cessation advice','Smoking cessation advice':'C0850254':[hlca]).
domain_concept('educational approach','Techniques, Educational':'C0013660':[edac]).
domain_concept('patient - provider communication','Health Communication':'C1512347':[mnob,hlca]).
domain_concept('gatekeeper','Gatekeepers, Health Service':'C0017203':[prog]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('tobacco prevention and control','Tobacco abuse prevention management':'C1443460':[hlca,topp]).
domain_concept('tobacco prevention','Tobacco abuse prevention':'C1171222':[topp,hlca]).
domain_concept('cessation','Cessation of smoking':'C0085134':[hlbe,hlca]).
domain_concept('physical problems','Physical health problems':'C1446390':[fndg]).
domain_concept('treatment issue','treatment issues':'C0679859':[fndg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('quitter','Former smoker':'C0337671':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('smoking cessation treatment','smoking cessation therapy':'C1095963':[topp]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('cessation treatment','smoking cessation therapy':'C1095963':[topp]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('cessation therapy','smoking cessation therapy':'C1095963':[topp]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('teen','teens':'C1521910':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('medically underserved','Underserved Population':'C0872319':[popg]).
domain_concept('preventive action','Other preventive procedure':'C0497109':[topp]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('homeless individuals','Homeless persons':'C0019863':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('homeless adult','Homeless persons':'C0019863':[popg]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('the homeless','Homeless persons':'C0019863':[popg]).
domain_concept('alcohol dependency','ALCOHOL DEPENDENCE CHRONIC':'C0740866':[mobd]).
domain_concept('alcohol dependence','ALCOHOL DEPENDENCE CHRONIC':'C0740866':[mobd]).
domain_concept('treatment adherence','Adherence to treatment plan':'C0516958':[hlca,fndg]).
domain_concept('adherence to treatment','Adherence to treatment plan':'C0516958':[hlca,fndg]).
domain_concept('dual x - ray absorptiometry','Dual-Energy X-Ray Absorptiometry':'C1510486':[diap]).
domain_concept('dxa','Dual-Energy X-Ray Absorptiometry':'C1510486':[diap]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('African - American','African American':'C0085756':[qlco,popg]).
domain_concept('emergency','Emergency Situation':'C0013956':[phpr]).
% --- the concept is defined differently in UMLS 2006AA. Either preferred name or semantic type is incorrectly used ---------
% domain_concept('target audience','audience':'C0681273':[popg]).
domain_concept('smoking relapse','smoking relapse':'D0000048':[fndg]).
domain_concept('action','Action':'D0000049':[acty]).
domain_concept('animal outbreak','Animal outbreak':'D0000050':[phpr]).
domain_concept('colleague','Colleague':'D0000060':[prog]).
domain_concept('committee','Committee':'D0000061':[orgt]).
domain_concept('announcement','Announcement':'D0000051':[infc]).
domain_concept('inhabitant','Inhabitant':'D0000086':[popg]).
domain_concept('plan of action','Plan of action':'D0000097':[prty,mnob]).
domain_concept('action plan','Plan of action':'D0000097':[prty,mnob]).
domain_concept('plan of care','Plan of care':'D0000098':[prty,mnob]).
domain_concept('plan','Plan':'D0000099':[prty,mnob]).
domain_concept('planner','Planner':'D0000100':[popg,orgt]).
domain_concept('precaution','Precaution':'D0000101':[acty]).
domain_concept('advisor','advisor':'D0000107':[prog]).
domain_concept('report','Report':'D0000108':[infc]).
domain_concept('public health actor','public health actor':'D0000141':[orgt,prog]).
domain_concept('monitoring','Monitoring':'D0000145':[gora,acty]).
domain_concept('illness','Illness':'D0000146':[dsyn]).
domain_concept('mild illness','Illness':'D0000146':[dsyn]).
domain_concept('business','business':'D0000156':[orgt,idcn]).
domain_concept('bureau','Bureau':'D0000157':[orgt]).
domain_concept('district','district':'D0000158':[geoa,orgt]).
domain_concept('department','department':'D0000159':[orgt]).
domain_concept('workgroup','workgroup':'D0000160':[prog,orgt]).
domain_concept('health official','health official':'D0000161':[prog]).
domain_concept('cooperator','cooperator':'D0000169':[popg,orgt]).
domain_concept('coordinator','coordinator':'D0000170':[popg,orgt]).
domain_concept('forum','forum':'D0000171':[venu]).
domain_concept('advisory panel','Advisory Panel':'D0000174':[orgt]).
domain_concept('panel','Advisory Panel':'D0000174':[orgt]).
domain_concept('all - cause mortality','all-cause mortality':'D0000256':[comc]).
domain_concept('monitoring program','Monitoring Program':'D0000257':[prty]).
domain_concept('practitioner','Practitioner':'D0000260':[prog]).
domain_concept('united states government','United States Government':'D0000271':[orgt]).
domain_concept('u.s. government','United States Government':'D0000271':[orgt]).
domain_concept('us government','United States Government':'D0000271':[orgt]).
domain_concept('non - elderly person','non-elderly person':'D0000272':[aggp]).
domain_concept('non - elderly adult','non-elderly person':'D0000272':[aggp]).
domain_concept('non - elderly','non-elderly':'D0000273':[aggp,popg]).
domain_concept('health threat','Health Threat':'D0000303':[comc,orga,clna]).
domain_concept('human health threat','Health Threat':'D0000303':[comc,orga,clna]).
domain_concept('health risk','Health Risk':'D0000308':[comc,orga,clna]).
domain_concept('public health planners','Public Health planners':'D0000325':[prog]).
domain_concept('human health','human health':'D0000343':[comc,orgf]).
domain_concept('adverse health effects','Adverse Health Effects':'D0000344':[patf,orga,clna]).
domain_concept('adverse health impact','Adverse Health Effects':'D0000344':[patf,orga,clna]).
domain_concept('adverse impacts','Adverse Health Effects':'D0000344':[patf,orga,clna]).
domain_concept('health effects','Health Effects':'D0000345':[comc,orga]).
domain_concept('health impacts','Health Effects':'D0000345':[comc,orga]).
domain_concept('education course','Education course':'D0000516':[edac]).
domain_concept('older','older':'D0000517':[grpa,aggp]).
domain_concept('public health','Public Health':'D0000518':[ocdi,gora]).
domain_concept('welfare reform act','Welfare Reform Act':'D0000519':[rnlw]).
domain_concept('self - care','self-care':'D0000520':[hlca,topp]).
domain_concept('self - care strategies','self-care':'D0000520':[hlca,topp]).
domain_concept('lifestyle modification','lifestyle modification':'D0000521':[hlbe]).
domain_concept('healthy weight','Healthy weight':'D0000522':[comc,hlbe]).
domain_concept('healthy body weight','Healthy weight':'D0000522':[comc,hlbe]).
domain_concept('fifth graders','Fifth graders':'D0000523':[aggp,popg]).
domain_concept('healthy lifestyle','Healthy lifestyle':'D0000524':[hlbe]).
domain_concept('healthier lifestyle','Healthier lifestyle':'D0000525':[hlbe]).
domain_concept('nonsmoking behavior','Nonsmoking behavior':'D0000526':[hlbe]).
domain_concept('non - smoking behavior','Nonsmoking behavior':'D0000526':[hlbe]).
domain_concept('nonsmoking','Nonsmoking behavior':'D0000526':[hlbe]).
domain_concept('nonsmoking','Nonsmoking behavior':'D0000526':[hlbe]).
domain_concept('non - smoking','Nonsmoking behavior':'D0000526':[hlbe]).
domain_concept('preventative effort','Preventative effort':'D0000527':[hlca]).
domain_concept('preventative and therapeutic effort','Preventative effort':'D0000527':[hlca]).
domain_concept('campaign','Campaign':'D0000528':[prty]).
domain_concept('ethnoracial disparities','ethnoracial disparities':'D0000529':[fndg]).
domain_concept('ethnic and racial disparities','ethnoracial disparities':'D0000529':[fndg]).
domain_concept('racial and ethnic disparities','ethnoracial disparities':'D0000529':[fndg]).
domain_concept('ethnoracial differences','ethnoracial disparities':'D0000529':[fndg]).
domain_concept('public health practitioner','Public Health Practitioner':'D0000530':[prog]).
domain_concept('evidence - based','evidence-based':'D0000531':[venu]).
domain_concept('pharmacy - based','pharmacy-based':'D0000532':[venu]).
domain_concept('virtual community','virtual community':'D0000533':[venu]).
domain_concept('implementer','Implementer':'D0000534':[prog,orgt]).
domain_concept('local authority','Local Authority':'D0000535':[orgt]).
domain_concept('healthcare setting','Healthcare setting':'D0000536':[venu]).
domain_concept('health care setting','Healthcare setting':'D0000536':[venu]).
domain_concept('community - based','Community-based':'D0000537':[venu]).
domain_concept('community health','Community Health':'D0000538':[comc,grpa]).
domain_concept('vaccination coverage','Vaccination coverage':'D0000539':[comc]).
domain_concept('vaccination project','Vaccination project':'D0000540':[gora]).
domain_concept('project','Project':'D0000541':[prty]).
domain_concept('healthy nutrition','Healthy Nutrition':'D0000542':[topp]).
domain_concept('pilot intervention','Pilot Intervention':'D0000543':[prty,hlca]). % June 2011 
domain_concept('food insecurity','Food insecurity':'D0000544':[fndg]).
domain_concept('food security','Food security':'D0000545':[fndg]).
domain_concept('meal','Meals':'D0000546':[food,mnob]).
domain_concept('school - based','school-based':'D0000547':[venu]).
domain_concept('web - based','web-based':'D0000548':[venu]).
domain_concept('high stress','High stress':'D0000549':[sosy]).
domain_concept('emergency responder','Emergency responder':'D0000550':[prog]).
domain_concept('sixth graders','Sixth graders':'D0000551':[aggp,popg]).
domain_concept('neighborhood - based','Neighborhood-based':'D0000552':[venu]).
domain_concept('clinic - based','Clinic-based':'D0000553':[venu]).
domain_concept('health consequences','Health Consequences':'D0000554':[fndg]).
domain_concept('community - wide','Community-wide':'D0000555':[venu]).
domain_concept('seminars','Seminars':'D0000556':[edac,prty]).
domain_concept('seminar','Seminars':'D0000556':[edac,prty]).
domain_concept('safety net','Safety Net':'D0000557':[mnob,orgt]).
domain_concept('health communication program','Health Communication Program':'D0000558':[prty,mnob]).
domain_concept('health communications program','Health Communication Program':'D0000558':[prty,mnob]).
domain_concept('physical inactivity','physical inactivity':'D0000559':[fndg,grpa]).
domain_concept('nonwhite persons','Nonwhite persons':'D0000560':[popg]).
domain_concept('nonwhites','Nonwhite persons':'D0000560':[popg]).
domain_concept('nonwhite','Nonwhite persons':'D0000560':[popg]).
domain_concept('black patients','Black Patients':'D0000561':[podg]).
domain_concept('patients of color','Black Patients':'D0000561':[podg]).
domain_concept('black communities','Black Communities':'D0000562':[popg]).
domain_concept('communities of color','Black Communities':'D0000562':[popg]).
domain_concept('villager','Villager':'D0000563':[popg]).
domain_concept('drug and alcohol use','Drug and Alcohol use':'D0000564':[fndg]).
domain_concept('smokeless tobacco use','Smokeless Tobacco Use':'D0000565':[fndg]).
domain_concept('medically underserved','Medically Underserved':'D0000566':[popg,qlco]).
domain_concept('patient education materials','Patient Education Materials':'D0000567':[infc,edac]).
domain_concept('self - management program','Self-management program':'D0000568':[prty,hlca]).
domain_concept('bone mineral density testing','Bone Mineral Density Testing':'D0000569':[diap,hlca]).
domain_concept('lack of access to health care','Lack of access to health care':'D0000570':[fndg]).
domain_concept('tobacco control program','Tobacco control program':'D0000571':[prty]).
domain_concept('tobacco control','Tobacco control':'D0000572':[gora,acty]).
domain_concept('tobacco control advocacy','Tobacco control':'D0000572':[gora,acty]). % NOTE: [advocacy: C0150446, socb]
domain_concept('tobacco policy advocacy','Tobacco policy advocacy':'D0000573':[acty]).
domain_concept('smoking prevalence','Smoking prevalence':'D0000574':[fndg]).
domain_concept('prevalence of smoking','Smoking prevalence':'D0000574':[fndg]).
domain_concept('smoking rate','Smoking prevalence':'D0000574':[fndg]).
domain_concept('health issues','Health issues':'D0000575':[fndg]).
domain_concept('health issue','Health issues':'D0000575':[fndg]).
domain_concept('health problem','Health issues':'D0000575':[fndg]).
domain_concept('ethnically diverse','Ethnically diverse':'D0000576':[qlco]).
domain_concept('policy issue','Policy issues':'D0000577':[fndg,rnlw]).
domain_concept('language factors','Language factors':'D0000578':[fndg]).
domain_concept('linguistic factors','Language factors':'D0000578':[fndg]).
domain_concept('healthy people 2011','Healthy People 2011':'D0000579':[hlca]).
domain_concept('population - based','population-based':'D0000580':[venu]).
domain_concept('multisite','Multisite':'D0000581':[venu]).
domain_concept('translating services','Translating Services':'D0000582':[orgt,ocac]).
domain_concept('translation services','Translating Services':'D0000582':[orgt,ocac]).
domain_concept('urban dwelling','Urban dwelling':'D0000583':[qlco]).
domain_concept('fieldnote','Fieldnote':'D0000584':[infc]).
domain_concept('coping strategies','Coping strategies':'D0000585':[mnob,menp,prty]).
domain_concept('asian american / pacific islander','Asian American/Pacific Islander':'D0000586':[popg]).
domain_concept('aapi','Asian American/Pacific Islander':'D0000586':[popg]).
domain_concept('national agenda','National agenda':'D0000587':[prty,rnlw]).
domain_concept('dietary counseling','Dietary counseling':'D0000588':[hlca]).
domain_concept('smoking cessation trial','smoking cessation trial':'D0000589':[resa]).
domain_concept('teenaged','teenaged':'D0000590':[qlco]).
domain_concept('disparities','Disparities':'D0000591':[fndg]).
domain_concept('disparity','Disparities':'D0000591':[fndg]).
domain_concept('fagerstrom test for nicotine dependence','Fagerstrom Test for Nicotine Dependence':'D0000592':[resa]).
domain_concept('FTND','Fagerstrom Test for Nicotine Dependence':'D0000592':[resa]).
domain_concept('non - african american','non-African American':'D0000593':[popg]).
domain_concept('cessation treatment intervention','cessation treatment intervention':'D0000594':[prty,hlca]).
domain_concept('cessation intervention','cessation treatment intervention':'D0000594':[prty,hlca]).
domain_concept('psychiatric condition','Psychiatric condition':'D0000595':[mobd,fndg]).
domain_concept('treatment research','Treatment research':'D0000596':[resa]).
domain_concept('issue','Issues':'D0000597':[fndg]).
domain_concept('u.s. preventive services task force','U.S. Preventive Services Task Force':'D0000598':[gora,hcro]).
domain_concept('preventive services task force','U.S. Preventive Services Task Force':'D0000598':[gora,hcro]).
% --- Redefinition of the same CUI with different preferred name or semantic type ---------
% domain_concept('USPSTF','Preventive Services Task Force':'D0000598':[gora,hcro]).
domain_concept('task force on community preventive services','Task Force on Community Preventive Services':'D0000599':[gora,hcro]).
domain_concept('u.s. task force on community preventive services','Task Force on Community Preventive Services':'D0000599':[gora,hcro]).
domain_concept('ctf','Task Force on Community Preventive Services':'D0000599':[gora,hcro]).
domain_concept('telephone support','Telephone support':'D0000600':[hlca]).
domain_concept('primary care clinicians','Primary Care Clinicians':'D0000601':[prog]).
domain_concept('project toward no tobacco','Project Toward No Tobacco':'D0000602':[prty]).
domain_concept('intervention group','Intervention Group':'D0000603':[popg,podg]).
domain_concept('health intervention','Health Intervention':'D0000604':[prty,hlca]).
domain_concept('intervention','Health Intervention':'D0000604':[prty,hlca]).
domain_concept('counseling ability','Counseling ability':'D0000605':[menp,inbe]).
domain_concept('counseling skill','Counseling ability':'D0000605':[menp,inbe]).
domain_concept('uninsured status','uninsured status':'D0000606':[fndg]).
domain_concept('lack of reimbursement','lack of reimbursement':'D0000607':[fndg]).
domain_concept('negative belief','negative belief':'D0000608':[fndg,menp]).
domain_concept('low health literacy','Low health literacy':'D0000609':[fndg]).
domain_concept('cultural barrier','Cultural barriers':'D0000610':[fndg]).
domain_concept('linguistic barriers','Linguistic barriers':'D0000611':[fndg]).
domain_concept('lack of motivation','Lack of motivation':'D0000613':[fndg]).
domain_concept('loss of motivation','Lack of motivation':'D0000613':[fndg]).
domain_concept('loss of cultural competence','Lack of cultural competence':'D0000614':[fndg]).
domain_concept('process evaluation','Process Evaluation':'D0000615':[resa,infc]).
domain_concept('low bone density','Low Bone Density':'D0000616':[fndg]).
domain_concept('community sample','community sample':'D0000617':[popg]).
% --- Redefinition of the same CUI with different preferred name or semantic type ---------
% domain_concept('population sample','population sample':'D0000617':[popg]).
domain_concept('tuberculosis knowledge','Tuberculosis knowledge':'D0000618':[fndg]).
domain_concept('tb knowledge','Tuberculosis knowledge':'D0000618':[fndg]).
domain_concept('appointment keeping','Appointment keeping':'D0000619':[hlca]).
domain_concept('medicare population','Medicare population':'D0000620':[popg]).
domain_concept('smoking cessation services','smoking cessation services':'D0000621':[mnob]).
domain_concept('smoking cessation initiative','smoking cessation initiative':'D0000622':[prty]).
domain_concept('screening practices','screening practices':'D0000623':[hlca,acty]).
% --- Redefinition of the same CUI with different preferred name or semantic type ---------
% domain_concept('national heart , lung , and blood institute','National Heart, Lung, and Blood Institute':'D0000623':[orgt]).
domain_concept('lesson','Lesson':'D0000624':[edac]).
domain_concept('obesity - related behavior','obesity-related behavior':'D0000625':[inbe]).
domain_concept('needs analysis','Needs analysis':'D0000626':[resa]).
domain_concept('cardiovascular dysfunction','Cardiovascular dysfunction':'D0000627':[patf]).
domain_concept('virtual environment','virtual environment':'D0000628':[venu]).
domain_concept('unhealthy outcomes','unhealthy outcome':'D0000629':[fndg]).
domain_concept('engineering plant','engineering plant':'D0000630':[mnob,geoa]).
domain_concept('obesity risk','Obesity risk':'D0000631':[fndg]).
domain_concept('non communicable disease','non communicable disease':'D0000632':[dsyn]).
domain_concept('story','Story':'D0000633':[infc]).
domain_concept('high - quality','high-quality':'D0000634':[qlco]).
domain_concept('emergency assistance','Emergency assistance':'D0000635':[hlca]).
domain_concept('coaching','Coaching':'D0000636':[acty,edac]).
domain_concept('expert classification system','Expert classification system':'D0000637':[mnob]).
domain_concept('review','Review':'D0000638':[infc]).
domain_concept('framework','Framework':'D0000639':[mnob]).
domain_concept('improved nutrition','Improved Nutrition':'D0000640':[topp]).
domain_concept('better nutrition','Improved Nutrition':'D0000640':[topp]).
domain_concept('nutrition','Nutrition':'D0000641':[orgf]).
domain_concept('descriptive','descriptive':'D0000642':[qlco]).
