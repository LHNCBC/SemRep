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
% file:	    nci_domain.pl
% module:   nci_domain.pl
:- module(nci_domain, [
	domain_name/1,
	domain_concept/2,
	domain_replace/2,
	domain_semtype/3
]).
domain_name(nci).
domain_semtype(bene,'Benefit',conc). % e.g., benefit, advantage, value, pros
domain_semtype(cgpr,'Cognitive Process',phys). % e.g., decision, thought-process, decision making
domain_semtype(cgtn,'Cognition',conc). % e.g., knowledge, problem-solving skills
domain_semtype(infc,'Information Construct',objc). % e.g., video, survey
domain_semtype(atti,'Attitude',conc). % e.g., attribute, disposition, predisposition
% domain_semtype(char,'Characteristic',conc). % e.g., attribute, experimental
domain_semtype(umes,'Unit of Measure',conc). % e.g., outcome measure, 
domain_semtype(stat,'Statistical Concept',conc). % e.g., probability, mortality, incidence
domain_semtype(ment,'Mental or Emotional State',phys). % e.g., mood, distress, anxiety
domain_semtype(rskf,'Risk Factor',conc). % e.g., risk factor 
domain_semtype(issu,'Issue or Problem',conc). % e.g., issue, problem, concern, difficulty
domain_semtype(lart,'Linguistic Artifact',conc). % e.g., anaphoric expression, phrases, sentences
domain_semtype(optn,'Options',conc). % e.g., options, alternatives, choices, array
domain_semtype(pref,'Preference',conc). % e.g., desire, preference, 
domain_semtype(pfac,'Prognostic Factor',conc). % e.g., prognostic factor 
domain_semtype(tech,'Technique',conc). % e.g., methodology, technique, approach, plan, strategy
domain_semtype(time,'Time',conc). % e.g., 'time period', year, days
domain_semtype(typg,'Type of Program',conc). % e.g., program, effort, initiative
domain_semtype(risk,'Risk',conc). % e.g., risk, cancer risk
domain_semtype(rslt,'Result',conc). % e.g., outcome, result, effect
domain_semtype(suju,'Subjective Judgment',conc). % e.g., risk perception; perceived value; perceived benefit
% The existing UMLS MT concepts that we would like to update by changing the preferred name, semantic type(s)
% The existing UMLS MT concepts that we would like to update by changing the preferred name, semantic type(s)
% domain_replace('Attribute':'C0449234':[idcn],'Attribute':'C0449234':[char]).domain_replace('Attribute':'C0449234':[idcn],'Attribute':'C0449234':[char]).
domain_replace('Cancer Risk':'C0596244':[qnco],'Cancer Risk':'C0596244':[risk]).
domain_replace('Cigarette smoker':'C0337667':[inbe],'Cigarette smoker':'C0337667':[popg,humn]).
domain_replace('Smoker':'C0337664':[fndg],'Smoker':'C0337664':[popg,humn]).
domain_replace('Heavy smoker':'C0857118':[socb],'Heavy smoker':'C0857118':[popg,humn]).
domain_replace('Cigarette Smoking':'C0700219':[dora],'Cigarette Smoking':'C0700219':[inbe]).
domain_replace('Non-smoker':'C0337672':[fndg],'Non-smoker':'C0337672':[popg,humn]).
domain_replace('Chain smoker':'C0459849':[fndg],'Chain smoker':'C0459849':[popg,humn]).
domain_replace('Continuance of life':'C0038952':[acty],'Survival':'C0038952':[orgf]).
domain_replace('Medical care, unspecified':'C0496675':[fndg],'Medical care':'C0496675':[topp]). 
domain_replace('Subgroup':'C1515021':[clas],'Subgroup':'C1515021':[grup]). 
domain_replace('Knowledge':'C0376554':[inpr],'Knowledge':'C0376554':[cgtn]). % cognition
domain_replace('Encounter due to other counseling NEC':'C0362061':[fndg],'Health education':'C0362061':[edac]).
domain_replace('problem-solving skills':'C0815099':[inbe],'problem-solving skills':'C0815099':[cgtn]). % cognition
domain_replace('Groups':'C0441833':[inpr],'Groups':'C0441833':[grup,popg]).
domain_replace('Risk Reduction':'C1137094':[topp],'Risk Reduction':'C1137094':[topp,stat]).
domain_replace('risk factors':'C0035648':[qnco],'risk factors':'C0035648':[rskf]).
domain_replace('Prognostic Factor':'C1514474':[clna],'Prognostic Factor':'C1514474':[pfac]).
domain_replace('Outcome Measures':'C0086749':[qnco],'Outcome Measures':'C0086749':[umes,stat]). %?
domain_replace('result':'C1274040':[ftcn],'result':'C1274040':[rslt,lbtr]). 
domain_replace('Decision Making':'C0011109':[menp],'Decision Making':'C0011109':[cgpr]). %?
domain_replace('desires':'C0871633':[inbe,menp],'desires':'C0871633':[pref]). %?
domain_replace('Food Preferences':'C0016483':[qlco],'Food Preferences':'C0016483':[pref]).
domain_replace('Patient Preference':'C0376409':[qlco],'Patient Preference':'C0376409':[pref]).
domain_replace('preference':'C0558295':[clna],'preference':'C0558295':[pref]).
domain_replace('decision':'C0679006':[inpr],'decision':'C0679006':[cgpr]).
domain_replace('videocassette':'C0042655':[mnob],'videocassette':'C0042655':[infc]).
domain_replace('Video':'C0441069':[mnob],'Video':'C0441069':[infc]).
domain_replace('Calories':'C0439259':[qnco],'Calories':'C0439259':[umes,stat]). 
domain_replace('Body Fat Distribution':'C0424621':[fndg],'Body Fat Distribution':'C0424621':[clna,stat]).
domain_replace('Perception':'C0030971':[menp],'Perception':'C0030971':[cgpr]).
domain_replace('Factor':'C1521761':[ftcn],'Factor':'C1521761':[?]).
domain_replace('Risk':'C0035647':[qlco],'Risk':'C0035647':[risk]).
domain_replace('experience':'C0596545':[menp],'experience':'C0596545':[cgtn]).
domain_replace('Probability':'C0033204':[qnco],'Probability':'C0033204':[stat]).
domain_replace('Probable diagnosis':'C0332148':[qlco],'Probable diagnosis':'C0332148':[diap]).
domain_replace('Medical Records, Computerized':'C0079150':[inpr,mnob],'Medical Records, Computerized':'C0079150':[infc]).
domain_replace('Risk Estimate':'C1519101':[qnco],'Risk Estimate':'C1519101':[stat,risk]).
domain_replace('subject':'C1550501':[idcn],'subject':'C1550501':[humn]).
domain_replace('GROWTH HORMONE TREATMENT':'C0744483':[dsyn],'GROWTH HORMONE TREATMENT':'C0744483':[topp]).
domain_replace('disease palliation':'C0679251':[fndg],'disease palliation':'C0679251':[topp]).
domain_replace('Maximum':'C0806909':[fndg],'Maximum':'C0806909':[stat]).
domain_replace('Thrombolysis':'C0520997':[clna],'Thrombolysis':'C0520997':[topp]).
domain_replace('Injection, intratympanic':'C1555371':[ftcn],'Injection, intratympanic':'C1555371':[topp]).
domain_replace('Skier':'C0425040':[fndg],'Skier':'C0425040':[humn]).
domain_replace('criteria':'C0243161':[inpr],'criteria':'C0243161':[umes,stat]).
domain_replace('Test Result':'C0456984':[fndg],'Test Result':'C0456984':[lbtr]).
domain_replace('Cream':'C1378128':[bodm],'Cream':'C1378128':[mnob,food]).
domain_replace('Milk':'C0026131':[bdsu],'Milk':'C0026131':[food,bdsu]).
domain_replace('Disposition':'C0743223':[idcn],'Disposition':'C0743223':[atti]).
domain_replace('Idea':'C1031756':[invt],'Idea':'C1031756':[cgpr]).
domain_replace('Beliefs':'C0004951':[idcn],'Beliefs':'C0004951':[cgpr]).
domain_replace('Motor function':'C0234130':[ftcn],'Motor function':'C0234130':[orgf]).
domain_replace('CORTICOSTEROID USE':'C0239126':[fndg],'CORTICOSTEROID USE':'C0239126':[topp]).
domain_replace('Reporting':'C0700287':[hlca],'Reporting':'C0700287':[acty]).
domain_replace('Cost Analysis':'C0010171':[ocac],'Cost Analysis':'C0010171':[resa,stat]).
domain_replace('Prior Therapy':'C1514463':[clna],'Prior Therapy':'C1514463':[topp]).
domain_replace('Prior Chemotherapy':'C1514457':[clna],'Prior Chemotherapy':'C1514457':[topp]).
domain_replace('Reaction':'C0443286':[ftcn],'Reaction':'C0443286':[inbe,phpr]).
domain_replace('response':'C0871261':[clna],'response':'C0871261':[inbe,phpr]).
domain_replace('Methodology':'C0025664':[inpr],'Methodology':'C0025664':[tech]).
domain_replace('Approach':'C0449445':[spco],'Approach':'C0449445':[tech]).
domain_replace('Longitudinal Studies':'C0023981':[qnco,resa],'Longitudinal Studies':'C0023981':[resa,infc]).
domain_replace('Surgical intervention':'C0549433':[fndg],'Surgical intervention':'C0549433':[topp]).
domain_replace('Cognitive function: planning':'C0032074':[menp],'Plan':'C0032074':[tech,cgpr]).
domain_replace('Disease Grade 2':'C1522446':[qlco],'Disease Grade 2':'C1522446':[dsyn]).
domain_replace('H/O: surgery NOS':'C0455610':[fndg],'prior surgery':'C0455610':[topp]).
domain_replace('Etiologic Agent':'C1254373':[sbst],'Etiologic Agent':'C1254373':[orgm]).
domain_replace('Health':'C0018684':[idcn],'Health':'C0018684':[orgf]).
domain_replace('Resting state':'C0679218':[fndg],'Resting state':'C0679218':[orgf]).
domain_replace('Registries':'C0034975':[inpr,qnco],'Registries':'C0034975':[infc]).
domain_replace('Surveys':'C0038951':[resa],'Surveys':'C0038951':[resa,infc]).
domain_replace('Mood':'C0026516':[menp],'Mood':'C0026516':[ment]).
domain_replace('Feelings':'C1527305':[menp],'Feelings':'C1527305':[ment]).
domain_replace('Imagery':'C0175631':[menp],'Imagery':'C0175631':[topp]).
domain_replace('year':'C0439234':[tmco],'year':'C0439234':[time]).
domain_replace('Interview':'C0021822':[inpr,resa],'Interview':'C0021822':[infc]).
domain_replace('week':'C0439230':[tmco],'week':'C0439230':[time]).
domain_replace('Questionnaires':'C0034394':[inpr],'Questionnaires':'C0034394':[infc]).
domain_replace('Data':'C1511726':[idcn],'Data':'C1511726':[infc]).
domain_replace('Calories':'C0439259':[qnco],'Calories':'C0439259':[umes]).
domain_replace('Long-term':'C0443252':[tmco],'Long-term':'C0443252':[time]).
domain_replace('premenopausal':'C0279752':[tmco],'premenopausal':'C0279752':[time]).
domain_replace('Frequencies (time pattern)':'C0439603':[tmco],'Frequency':'C0439603':[time]).
domain_replace('Combined result':'C1548228':[inpr],'Combined result':'C1548228':[infc,rslt]).
domain_replace('Preliminary results':'C1548161':[idcn],'Preliminary results':'C1548161':[infc,rslt]).
domain_replace('Patient name':'C1299487':[fndg],'Patient name':'C1299487':[infc]). % 7 March 2013
domain_replace('Finding':'C0243095':[ftcn],'Finding':'C0243095':[lbtr,fndg]). % Feb 2013 added lbtr & on March 7 deleted fndg, but put it back in on March 14. It needs to be there
domain_replace('Injury due to exposure to external cause':'C0274281':[inpo],'Exposure':'C0274281':[inpo]). % Feb 25 2013
domain_replace('Extraction':'C0185115':[topp],'Extraction':'C0185115':[mcha,acty]).
domain_replace('United States Department of Veterans Affairs':'C0041735':[orgt],'Department of Veterans Affairs':'C0041735':[orgt]).
domain_replace('Hippa':'C1193480':[invt],'HIPPA':'C1193480':[rnlw,mnob]).
domain_replace('Literature':'C0023866':[inpr],'Literature':'C0023866':[infc]).
domain_replace('Confidential patient data':'C0554948':[inpr],'Confidential patient data':'C0554948':[infc]).
domain_replace('literature citation':'C0596849':[inpr],'literature citation':'C0596849':[infc]).
domain_replace('Citation':'C0552371':[inpr],'Citation':'C0552371':[infc]).
domain_replace('PROGRESS NOTE':'C0747978':[inpr],'Progress Note':'C0747978':[infc]).
domain_replace('Progress Report':'C0242586':[inpr],'Progress Report':'C0242586':[infc]).
domain_replace('Division (procedure)':'C1293097':[topp],'Section':'C1293097':[infc]).
domain_replace('Patient Data Management Systems':'C0180292':[inpr,medd],'Patient Data Management Systems':'C0180292':[mnob]).
domain_replace('data mining':'C1328866':[mcha,ocac],'data mining':'C1328866':[mcha]).
domain_replace('GOLD STANDARD':'C0150110':[qlco],'Gold standard':'C0150110':[infc]). % Feb 2013 deleted mnob
domain_replace('Reference Standards':'C0034925':[qnco],'Reference Standard':'C0034925':[infc]). % Feb 2013 deleted mnob
domain_replace('PATIENT REPORT':'C0747307':[inpr],'Patient Report':'C0747307':[infc]).
domain_replace('Patient Outcome':'C1547647':[idcn],'Patient Outcome':'C1547647':[rslt]).
domain_replace('Task analysis':'C0556033':[topp],'Task analysis':'C0556033':[resa,mcha]).
domain_replace('Documentation':'C0920316':[inpr],'Documentation':'C0920316':[infc]).
domain_replace('Medical record, device':'C0025102':[inpr,mnob],'Medical Record':'C0025102':[infc]).
domain_replace('programmed text':'C0681465':[inpr],'programmed text':'C0681465':[infc]).
domain_replace('Text':'C1527021':[inpr],'Text':'C1527021':[infc]).
domain_replace('Information Retrieval Systems':'C0021422':[inpr,mcha],'Information Retrieval Systems':'C0021422':[mcha]).
domain_replace('Core':'C0444669':[spco],'Core document':'C0444669':[infc]).
domain_replace('Text data':'C1547407':[idcn],'Text data':'C1547407':[infc]).
domain_replace('data set':'C0150098':[inpr],'Data Set':'C0150098':[infc]).
domain_replace('Databases':'C0242356':[inpr],'Databases':'C0242356':[infc]).
domain_replace('Data Sources':'C0011001':[qnco],'Data Sources':'C0011001':[mnob]).
domain_replace('algorithm':'C0002045':[inpr],'algorithm':'C0002045':[mcha,mnob]).
domain_replace('Radiology report':'C1299496':[inpr],'Radiology report':'C1299496':[infc]).
domain_replace('Terminology':'C0028275':[inpr],'Terminology':'C0028275':[infc]).
domain_replace('concept':'C0178566':[idcn],'Concept':'C0178566':[lart,lang]).
domain_replace('Sentences':'C0876929':[inpr],'Sentences':'C0876929':[lart]).
domain_replace('expert system software':'C0681435':[inpr],'expert system software':'C0681435':[mnob]).
domain_replace('Foundations':'C0016617':[orgt],'Foundations':'C0016617':[mnob,orgt]).
domain_replace('Programs [Publication Type]':'C0376691':[inpr],'Programs':'C0376691':[mnob,typg]).
domain_replace('Initiative':'C0424093':[menp],'Initiative':'C0424093':[typg]).
domain_replace('Publications':'C0034036':[inpr,mnob],'Publications':'C0034036':[infc]).
domain_replace('lexicon':'C0683875':[inpr],'lexicon':'C0683875':[infc,mnob]).
domain_replace('Ontology':'C1518584':[ocdi],'Ontology':'C1518584':[infc,mnob]).
domain_replace('Schema':'C0871287':[inpr],'Schema':'C0871287':[infc,mnob]).
domain_replace('Scheme':'C1519193':[inpr],'Scheme':'C1519193':[infc,mnob]).
domain_replace('Guidelines':'C0162791':[inpr],'Guidelines':'C0162791':[infc]).
domain_replace('IDENTIFYING INFORMATION':'C0489557':[fndg],'Identifying Information':'C0489557':[mcha,infc]).
domain_replace('Logistic Models':'C0023965':[inpr,qnco],'Logistic Models':'C0023965':[mcha,mnob]).
domain_replace('error':'C0743559':[qlco],'error':'C0743559':[fndg,lbtr]).
domain_replace('Computer Applications':'C0870325':[ftcn],'Computer Application':'C0870325':[mnob]).
domain_replace('Computer software':'C0037585':[inpr,mnob],'Computer software':'C0037585':[mnob]).
domain_replace('Medical Records, Computerized':'C0079150':[inpr,mnob],'Electronic Medical Records':'C0079150':[infc]).
domain_replace('Unified Medical Language System':'C0085567':[inpr,resa],'Unified Medical Language System':'C0085567':[mnob,infc]).
domain_replace('Data Element':'C1511728':[inpr],'Data Element':'C1511728':[infc]).
domain_replace('Medical center':'C0565990':[hcro,mnob],'Medical center':'C0565990':[orgt,hcro]).
domain_replace('Vocabulary, Controlled':'C0282503':[inpr],'Vocabulary, Controlled':'C0282503':[lart,infc]).
domain_replace('Vocabulary':'C0042926':[inpr],'Vocabulary':'C0042926':[infc]).
domain_replace('information organization':'C0681317':[inpr],'knowledge representation':'C0681317':[mnob]).
domain_replace('Gleason grading system for prostatic cancer':'C0332326':[clas],'Gleason score':'C0332326':[lbtr,stat]).
domain_replace('Observation parameter':'C0449381':[fndg],'parameter':'C0449381':[infc]).
domain_replace('mental health':'C0025353':[idcn],'mental health':'C0025353':[orgf]).
domain_replace('Retrieval':'C1514918':[hlca],'Retrieval':'C1514918':[mcha]).
domain_replace('Architecture':'C0003737':[ocdi],'Architecture':'C0003737':[mnob]).
domain_replace('System':'C0449913':[ftcn],'System':'C0449913':[mnob]).
domain_replace('Report (document)':'C0684224':[inpr],'Report':'C0684224':[infc]).
domain_replace('Code':'C0805701':[inpr],'Code':'C0805701':[infc]).
domain_replace('Administrative':'C0849568':[hlca],'Administrative':'C0849568':[ftcn]).
domain_replace('clinical document':'C1551343':[idcn],'clinical document':'C1551343':[infc]).
domain_replace('Document type':'C1547673':[clas],'Document type':'C1547673':[infc]).
domain_replace('Documents':'C1301746':[inpr],'Documents':'C1301746':[infc]).
domain_replace('Needs':'C0027552':[qlco],'Needs':'C0027552':[pref]).
domain_replace('Experimental Laboratories':'C0237699':[mnob],'Experimental Laboratories':'C0237699':[orgt]). %New
% domain_replace('Experimental':'C1517004':[ftcn],'Experimental':'C1517004':[char]).domain_replace('Experimental':'C1517004':[ftcn],'Experimental':'C1517004':[char]).
domain_replace('electronic data':'C1553520':[idcn],'electronic data':'C1553520':[infc]).
domain_replace('Paper':'C0030351':[mnob],'Paper':'C0030351':[infc]).
domain_replace('Classification':'C0008902':[clas],'Classification':'C0008902':[mcha,acty]).
domain_replace('Synonyms':'C0871468':[inpr],'Synonyms':'C0871468':[lart]).
domain_replace('Transcript':'C1519595':[nnon],'Transcript':'C1519595':[infc]).
domain_replace('instruction':'C1186996':[edac],'instruction':'C1186996':[infc,edac]).
domain_replace('Logistics':'C0242415':[ocac],'Logistics':'C0242415':[cgpr]).
domain_replace('Psychotherapy':'C0033968':[topp],'Psychotherapy':'C0033968':[ocdi,topp]).
domain_replace('Systematized Nomenclature of Medicine':'C1136257':[inpr],'SNOMED-CT':'C1136257':[infc]).
domain_replace('Decision Support Systems, Clinical':'C0525070':[inpr],'Clinical Decision Support Systems':'C0525070':[mnob]).
domain_replace('Decision Support Systems':'C0870393':[inpr,mnob],'Decision Support Systems':'C0870393':[mnob]).
domain_replace('roentgenographic':'C0034571':[ftcn],'X - ray':'C0034571':[diap]).
domain_replace('Imaging modality':'C1275506':[ftcn],'Imaging modality':'C1275506':[diap]).
domain_replace('Does hit':'C0596020':[fndg],'HITS':'C0596020':[mcha,acty]).
domain_replace('Identification (Psychology)':'C0020792':[menp],'Identification':'C0020792':[acty,diap]).
domain_replace('Techniques':'C0449851':[ftcn],'Techniques':'C0449851':[tech]). % changed from [hccp,mnob] to just [mnob]
domain_replace('Mechanisms':'C0441712':[ftcn],'Mechanisms':'C0441712':[mnob]).
domain_replace('Specificity':'C0037791':[qnco],'Specificity':'C0037791':[umes]).
domain_replace('Antimicrobial susceptibility':'C0427965':[fndg],'Sensitivity':'C0427965':[umes]).
domain_replace('BODY WEIGHT:MASS:POINT IN TIME:^PATIENT:QUANTITATIVE':'C0944911':[clna],'Weight':'C0944911':[umes]).
domain_replace('Body mass index':'C1305855':[clna],'Body mass index':'C1305855':[umes]).
domain_replace('Intake':'C1512806':[ftcn],'Intake':'C1512806':[orgf,biof]).
domain_replace('essential nutrients':'C0684140':[chvf],'essential nutrients':'C0684140':[sbst,food]).
domain_replace('Daily':'C0332173':[tmco],'Daily':'C0332173':[time]).
domain_replace('Health Surveys':'C0018762':[resa],'Health Surveys':'C0018762':[resa,infc]).
domain_replace('Relative Risk':'C0242492':[qlco,qnco],'Relative Risk':'C0242492':[stat,risk]).
domain_replace('Kilogram':'C0439209':[qnco],'Kilogram':'C0439209':[umes]).
domain_replace('Adolescence':'C0001578':[tmco],'Adolescence':'C0001578':[time]).
domain_replace('Current non-drinker of alcohol':'C0457801':[socb],'Current non-drinker of alcohol':'C0457801':[humn,popg]).
domain_replace('Recommendation':'C0034866':[idcn],'Recommendation':'C0034866':[infc]).
domain_replace('Incidence':'C0021149':[qnco],'Incidence':'C0021149':[stat]).
domain_replace('Mortality Vital Statistics':'C0026565':[qnco],'Mortality rate':'C0026565':[stat]).
domain_replace('Statistically Significant':'C1514961':[qnco],'Statistically Significant':'C1514961':[stat]).
domain_replace('controversy':'C0680243':[socb],'controversy':'C0680243':[inpr]).
domain_replace('Weekly':'C0332174':[tmco],'Weekly':'C0332174':[time]).
domain_replace('Meta-Analysis':'C0920317':[inpr],'Meta-Analysis':'C0920317':[resa]).
domain_replace('skills':'C0678856':[inbe],'skills':'C0678856':[cgtn]).
domain_replace('hazard':'C0598697':[qlco],'hazard':'C0598697':[risk]).
domain_replace('Benefits':'C0814225':[qnco],'Benefits':'C0814225':[bene]).
domain_replace('Short-term':'C0443303':[tmco],'short-term':'C0443303':[time]).
domain_replace('treatment options':'C0683525':[inpr],'treatment options':'C0683525':[topp,optn]).
domain_replace('Options':'C1518601':[ftcn],'Options':'C1518601':[optn]).
domain_replace('Alternative':'C1523987':[cnce],'Alternative':'C1523987':[optn]).
domain_replace('Mean Survival Time':'C0086595':[qnco],'Mean Survival Time':'C0086595':[time,stat]).
domain_replace('month':'C0439231':[tmco],'month':'C0439231':[time]).
domain_replace('per second':'C0565930':[qnco],'per second':'C0565930':[stat]).
domain_replace('intervention program':'C0599917':[topp],'intervention program':'C0599917':[topp,typg]).
domain_replace('Frequent':'C0332183':[tmco],'Frequent':'C0332183':[time]).
domain_replace('Program Evaluation':'C0033336':[ocac],'Program Evaluation':'C0033336':[infc]).
domain_replace('Distress':'C0231303':[menp],'Distress':'C0231303':[ment]).
domain_replace('Effect':'C1280500':[qlco],'Effect':'C1280500':[rslt]).
domain_replace('Duration':'C0449238':[tmco],'Duration':'C0449238':[time]).
% ---Wrong type name for "C0080105" used in Replace---------
% ---Correct type is "idcn,popg"---------
% domain_replace('Research Subject':'C0080105':[humn,idcn,popg],'Research Subject':'C0080105':[popg,humn]).
domain_replace('Longterm Effects':'C0023983':[phpr],'Longterm Effects':'C0023983':[rslt,phpr]).
domain_replace('Anxiety':'C0003467':[menp],'Anxiety':'C0003467':[ment]).
domain_replace('Depressed mood':'C0344315':[fndg],'Depressed mood':'C0344315':[ment]).
domain_replace('Awareness':'C0004448':[menp],'Awareness':'C0004448':[cgpr]).
domain_replace('search':'C1552603':[idcn],'search':'C1552603':[acty,mcha]).
domain_replace('Cessation of smoking':'C0085134':[inbe],'Cessation of smoking':'C0085134':[inbe,topp]).
domain_replace('Relapse':'C0035020':[phpr],'Relapse':'C0035020':[fndg,phpr]).
domain_replace('Disease susceptibility':'C0012655':[clna],'Predisposition':'C0012655':[clna,atti]).
domain_replace('perceived risk':'C0814102':[qlco],'perceived risk':'C0814102':[suju]).
domain_replace('Registers':'C0600375':[inpr],'Registers':'C0600375':[infc]).
domain_replace('Problem Solving':'C0033211':[menp],'Problem Solving':'C0033211':[cgpr,mcha]).
domain_replace('Decision Aids':'C0086104':[inpr],'Decision Aids':'C0086104':[infc,mnob]).
domain_replace('Information, health':'C0850397':[hlca],'Health information':'C0850397':[infc]).
domain_replace('Counseling, health':'C0850252':[fndg],'Health counseling':'C0850252':[hlca,acty]).
% info_domain_concept clauses are of the form (NewSynonym,PreferredName:CUI:SemanticTypes).
% This predicate covers two types of concepts.
% (1) Concepts already in UMLS that we want to add a new synonym for, so that we can map a new string to the concept.
% (2) Entirely new domain concepts not found in UMLS.
% The first argument is the string that we want to map and should be in lowercase.
% The following clauses cover the concepts of type (1).
% The following clauses cover the concepts of type (1).
domain_concept('personal health information','Health information':'C0850397':[infc]).
domain_concept('health decision aids','Decision Aids':'C0086104':[infc,mnob]).
domain_concept('health counseling session','Health counseling':'C0850252':[hlca,acty]).
domain_concept('problem solving process','Problem Solving':'C0033211':[cgpr,mcha]).
domain_concept('cerebral insult','Cerebrovascular accident':'C0038454':[dsyn]).
domain_concept('upper body','Trunk structure':'C0460005':[blor]).
domain_concept('leisure-time activities','Leisure Activities':'C0023292':[dora]).
domain_concept('full-term pregnancy','Term pregnancy':'C0232991':[orgf]).
domain_concept('alcohol','Alcoholic Beverages':'C0001967':[food]).
domain_concept('non-drinker','Current non-drinker of alcohol':'C0457801':[humn,popg]).
domain_concept('nondrinker','Current non-drinker of alcohol':'C0457801':[humn,popg]).
domain_concept('strenuous physical activity','Strenuous Exercise':'C1514989':[dora]).
domain_concept('vigorous physical activity','Strenuous Exercise':'C1514989':[dora]).
domain_concept('vigorous activity','Strenuous Exercise':'C1514989':[dora]).
domain_concept('strenuous activity','Strenuous Exercise':'C1514989':[dora]).
domain_concept('intensity exercise','Strenuous Exercise':'C1514989':[dora]).
domain_concept('mortality','Cessation of life':'C0011065':[orgf]).
domain_concept('median survival','Mean Survival Time':'C0086595':[time,stat]).
domain_concept('duration of survival','Mean Survival Time':'C0086595':[time,stat]).
domain_concept('recreational physical activity','Leisure physical activity':'C0336910':[dora]).
domain_concept('long-term effects','Longterm Effects':'C0023983':[rslt,phpr]).
% The following clauses cover the concepts of type (2).
domain_concept('food and drug administration','Food and Drug Administration':'N0000001':[orgt]).
domain_concept('fda','Food and Drug Administration':'N0000001':[orgt]).
domain_concept('active leisure','Active leisure':'N0000002':[dora]).
domain_concept('reference period','Reference period':'N0000003':[time]).
domain_concept('postmenopausal','postmenopausal':'N0000004':[time]).
domain_concept('literature search','literature search':'N0000005':[mcha]).
domain_concept('informed decision','informed decision':'N0000006':[cgpr]).
domain_concept('pros and cons','pros and cons':'N0000007':[risk]).
domain_concept('possibility','possibility':'N0000008':[stat]).
domain_concept('likelihood','possibility':'N0000008':[stat]).
domain_concept('program','program':'N0000009':[typg]).
domain_concept('sccip','sccip':'N0000010':[typg,topp]).
domain_concept('Surviving cancer competently intervention program','sccip':'N0000010':[typg,topp]).
domain_concept('consequence','consequence':'N0000011':[rslt]).
domain_concept('strategy','strategy':'N0000012':[tech,cgpr]).
domain_concept('veterans health administration','Veterans Health Administration':'N0000013':[orgt]).
domain_concept('vha','Veterans Health Administration':'N0000013':[orgt]).
domain_concept('positive mental imagery','positive mental imagery':'N0000014':[topp]).
domain_concept('coping strategies','coping strategies':'N0000015':[cgtn]).
domain_concept('coping skills','coping strategies':'N0000015':[cgtn]).
domain_concept('methods of coping','coping strategies':'N0000015':[cgtn]).
domain_concept('coping methods','coping strategies':'N0000015':[cgtn]).
domain_concept('active-cognitive coping','coping strategies':'N0000015':[cgtn]).
domain_concept('active-behavioral coping','coping strategies':'N0000015':[cgtn]).
domain_concept('threat','threat':'N0000016':[risk]).
domain_concept('problem','problem':'N0000017':[issu]).
domain_concept('issue','issue':'N0000018':[issu]).
domain_concept('concern','issue':'N0000018':[issu]).
domain_concept('choice','choice':'N0000019':[optn]).
domain_concept('assumption','assumption':'N0000020':[cgpr]).
domain_concept('pressuposition','pressuposition':'N0000021':[cgpr]).
domain_concept('premise','premise':'N0000022':[cgtn]).
domain_concept('perceived value','perceived value':'N0000023':[suju]).
domain_concept('perceived benefit','perceived benefit':'N0000024':[suju]).
domain_concept('value','value':'N0000025':[bene]).
domain_concept('positive aspects','value':'N0000025':[bene]).
domain_concept('advantage','advantage':'N0000026':[bene]).
domain_concept('pros','advantage':'N0000026':[bene]).
domain_concept('national institutes of health','National Institutes of Health':'N0000027':[orgt]).
domain_concept('nih','National Institutes of Health':'N0000027':[orgt]).
