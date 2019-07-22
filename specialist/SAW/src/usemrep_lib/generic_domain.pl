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

% file:	    generic_domain.pl
% module:   generic_domain.pl
:- module(generic_domain, [
	domain_name/1,
	domain_concept/2,
	domain_replace/2,
	domain_semtype/3
]).
domain_name(generic).
% The existing UMLS MT concepts that we would like to update by changing the preferred name, semantic type(s)
% The existing UMLS MT concepts that we would like to update by changing the preferred name, semantic type(s)
domain_replace('Attribute':'C0449234':[idcn],'Attribute':'C0449234':[clna]).
domain_replace('Cigarette smoker':'C0337667':[inbe],'Cigarette smoker':'C0337667':[popg,humn]). % 11/16/2017 mod ([lexmatch([cigarette]),inputmatch(['Cigarette']),tag(noun),bases([cigarette])]),  head([lexmatch([smoker]),inputmatch([smoker]),tag(noun),bases([smoker])]) ],
domain_replace('Smoker':'C0337664':[fndg],'Smoker':'C0337664':[popg,humn]).
domain_replace('Heavy smoker':'C0857118':[socb],'Heavy smoker':'C0857118':[popg,humn]). % 11/16/2017 lexmatch([heavy]),inputmatch(['Heavy']),tag(adj),bases([heavy])]),  head([lexmatch([smoker]),inputmatch([smoker]),tag(noun),bases([smoker])]) ],
domain_replace('Cigarette Smoking':'C0700219':[dora],'Cigarette Smoking':'C0700219':[inbe,socb]). % 11/16/2017 mod ([lexmatch([cigarette]),inputmatch(['Cigarette']),tag(noun),bases([cigarette])]),  head([lexmatch([smoking]),inputmatch(['Smoking']),tag(noun),bases([smoking])]) ],
domain_replace('Non-smoker':'C0337672':[fndg],'Non-smoker':'C0337672':[popg,humn]). % 11/16/2017 GOOD!  head([lexmatch(['non-smoker']),inputmatch(['Non',-,smoker]),tag(noun),bases([non,-,smoker])]) ],
domain_replace('Chain smoker':'C0459849':[fndg],'Chain smoker':'C0459849':[popg,humn]). % 11/16/2017 mod([lexmatch([chain]),inputmatch(['Chain']),tag(noun),bases([chain])]),  head([lexmatch([smoker]),inputmatch([smoker]),tag(noun),bases([smoker])]) ],
domain_replace('Continuance of life':'C0038952':[acty],'Survival':'C0038952':[orgf]). % 11/16/2017 Two heads: continuance & life
domain_replace('Medical care, unspecified':'C0496675':[fndg],'Medical care':'C0496675':[topp]). % 11/16/2017 mod([lexmatch([medical]),inputmatch(['Medical']),tag(adj),bases([medical])]), head([lexmatch([care]),inputmatch([care]),tag(noun),bases([care])]),punc([inputmatch([',']),bases([','])]) ],[head([lexmatch([unspecified]),inputmatch([unspecified]),tag(adj),bases([unspecified])])
domain_replace('Subgroup':'C1515021':[clas],'Subgroup':'C1515021':[grup,popg]). 
domain_replace('Health education':'C0018701':[hlca],'Health education':'C0018701':[edac]). % 11/16/2017 Mod: health, Head: education
domain_replace('Gene Products, tax':'C0079407':[aapp,bacs],'Tax Protein':'C0079407':[aapp,bacs]). % 05/2017 block mapping  tax to Taxes in exceptions file, & add it in next section as a synonym of this entry; % 11/16/2017 head: bases([gene,products]); + head([lexmatch([tax])||UMLS: C0033684|Proteins|aapp,bacs|||Gene Products
domain_replace('Groups':'C0441833':[inpr],'Groups':'C0441833':[popg]).
domain_replace('Forecast of outcome':'C0033325':[hlca],'Outcome prediction':'C0033325':[lbtr]). 
domain_replace('result':'C1274040':[ftcn],'result':'C1274040':[lbtr]). 
domain_replace('Body Fat Distribution':'C0424621':[fndg],'Body Fat Distribution':'C0424621':[orga,clna]). % 09/13/2016
domain_replace('Probable diagnosis':'C0332148':[qlco],'Probable diagnosis':'C0332148':[diap]).
domain_replace('subject':'C1550501':[idcn],'subject':'C1550501':[humn]).
domain_replace('GROWTH HORMONE TREATMENT':'C0744483':[dsyn],'Growth Hormone Treatment':'C0744483':[topp]).
domain_replace('disease palliation':'C0679251':[fndg],'disease palliation':'C0679251':[topp]).
domain_replace('Maximum':'C0806909':[fndg],'Maximum':'C0806909':[qnco]).
domain_replace('Thrombolysis':'C0520997':[clna],'Thrombolysis':'C0520997':[topp]).
domain_replace('Injection, intratympanic':'C1555371':[ftcn],'Intratympanic injection':'C1555371':[topp]).
domain_replace('Skier':'C0425040':[fndg],'Skier':'C0425040':[humn]).
domain_replace('Test Result':'C0456984':[fndg],'Test Result':'C0456984':[lbtr]).
domain_replace('Cream':'C1378128':[bodm],'Cream':'C1378128':[mnob,food]).
domain_replace('Milk':'C0026131':[bdsu],'Milk':'C0026131':[food,bdsu]).
domain_replace('Disposition':'C0743223':[idcn],'Disposition':'C0743223':[inbe]).
domain_replace('Motor function':'C0234130':[ftcn],'Motor function':'C0234130':[orgf]).
domain_replace('CORTICOSTEROID USE':'C0239126':[fndg],'Corticosteroid use':'C0239126':[topp]).
domain_replace('Reporting':'C0700287':[hlca],'Reporting':'C0700287':[acty]).
domain_replace('Cost Analysis':'C0010171':[ocac],'Cost Analysis':'C0010171':[resa]).
domain_replace('Prior Therapy':'C1514463':[clna],'Prior Therapy':'C1514463':[topp]).
domain_replace('Prior Chemotherapy':'C1514457':[clna],'Prior Chemotherapy':'C1514457':[topp]).
domain_replace('Reaction':'C0443286':[ftcn],'Reaction':'C0443286':[inbe,phpr]).
domain_replace('response':'C0871261':[clna],'response':'C0871261':[inbe,phpr]).
domain_replace('Longitudinal Studies':'C0023981':[qnco,resa],'Longitudinal Studies':'C0023981':[resa]).
domain_replace('Surgical intervention':'C0549433':[fndg],'Surgical intervention':'C0549433':[topp]).
domain_replace('Cognitive function: planning':'C0032074':[menp],'Plan':'C0032074':[hlca]).
domain_replace('Disease Grade 2':'C1522446':[qlco],'Disease Grade 2':'C1522446':[dsyn]).
domain_replace('H/O: surgery NOS':'C0455610':[fndg],'prior surgery':'C0455610':[topp]).
domain_replace('Etiologic Agent':'C1254373':[sbst],'Etiologic Agent':'C1254373':[orgm]).
domain_replace('Health':'C0018684':[idcn],'Health':'C0018684':[orgf]).
domain_replace('Resting state':'C0679218':[fndg],'Resting state':'C0679218':[orgf]).
domain_replace('Imagery':'C0175631':[menp],'Imagery':'C0175631':[topp]).
domain_replace('Calories':'C0439259':[qnco],'Calorie':'C0439259':[food,qnco]).
domain_replace('premenopausal':'C0279752':[tmco],'premenopausal':'C0279752':[tmco,orga]).
domain_replace('Combined result':'C1548228':[inpr],'Combined result':'C1548228':[lbtr]).
domain_replace('Preliminary results':'C1548161':[idcn],'Preliminary results':'C1548161':[lbtr]).
domain_replace('research outcome':'C0683954':[fndg],'research outcome':'C0683954':[lbtr]).
domain_replace('Finding':'C0243095':[ftcn],'Finding':'C0243095':[lbtr,fndg]).
domain_replace('Injury due to exposure to external cause':'C0274281':[inpo],'Exposure':'C0274281':[inpo]).
domain_replace('Extraction':'C0185115':[topp],'Extraction':'C0185115':[mcha,acty,topp]).
domain_replace('United States Department of Veterans Affairs':'C0041735':[orgt],'Department of Veterans Affairs':'C0041735':[orgt]).
domain_replace('Hippa':'C1193480':[invt],'HIPPA':'C1193480':[rnlw,mnob]).
domain_replace('Division (procedure)':'C1293097':[topp],'Section':'C1293097':[phpr]).
domain_replace('Patient Data Management Systems':'C0180292':[inpr,medd],'Patient Data Management Systems':'C0180292':[mnob]). % 11/17/2017: This one is one whole entry in the Lexicon, and in the plural
domain_replace('data mining':'C1328866':[mcha,ocac],'data mining':'C1328866':[mcha]).
domain_replace('GOLD STANDARD':'C0150110':[qlco],'Gold standard':'C0150110':[mcha]).
domain_replace('Reference Standards':'C0034925':[qnco],'Reference Standards':'C0034925':[mcha]).
domain_replace('Patient Outcome':'C1547647':[idcn],'Patient Outcome':'C1547647':[lbtr]).
domain_replace('Health Outcomes':'C1550208':[inpr],'Health Outcomes':'C1550208':[lbtr]).
domain_replace('Task analysis':'C0556033':[topp],'Task analysis':'C0556033':[resa]).
domain_replace('Data Analysis':'C0010992':[ocac],'Data Analysis':'C0010992':[resa]).
domain_replace('Documentation':'C0920316':[inpr],'Documentation':'C0920316':[resa]).
domain_replace('Information Retrieval Systems':'C0021422':[inpr,mcha],'Information Retrieval Systems':'C0021422':[mcha,mnob]).
domain_replace('Data Sources':'C0011001':[qnco],'Data Sources':'C0011001':[mnob]). % 11/17/2017: This one is one whole entry in the Lexicon, and in the plural
domain_replace('algorithm':'C0002045':[inpr],'algorithm':'C0002045':[mcha]).
domain_replace('Radiology report':'C1299496':[inpr],'Radiology report':'C1299496':[lbtr]).
domain_replace('expert system software':'C0681435':[inpr],'expert system software':'C0681435':[mnob]).
domain_replace('Foundations':'C0016617':[orgt],'Foundations':'C0016617':[mnob,orgt]).
domain_replace('Programs [Publication Type]':'C0376691':[inpr],'Programs':'C0376691':[mnob]).
domain_replace('Initiative':'C0424093':[menp],'Initiative':'C0424093':[mnob]).
domain_replace('Publications':'C0034036':[inpr,mnob],'Publications':'C0034036':[mnob]).
domain_replace('Guidelines':'C0162791':[inpr],'Guidelines':'C0162791':[mnob]).
domain_replace('Logistic Models':'C0023965':[inpr,qnco],'Logistic Models':'C0023965':[mcha]). % 11/10/2016 deleted resa
domain_replace('error':'C0743559':[qlco],'error':'C0743559':[fndg,lbtr]).
domain_replace('Computer Applications':'C0870325':[ftcn],'Computer Applications':'C0870325':[mnob]).
domain_replace('Computer software':'C0037585':[inpr,mnob],'Computer software':'C0037585':[mnob]).
domain_replace('Medical center':'C0565990':[hcro,mnob],'Medical center':'C0565990':[orgt,hcro]).
domain_replace('Gleason grading system for prostatic cancer':'C0332326':[clas],'Gleason score':'C0332326':[lbtr]).
domain_replace('Observation parameter':'C0449381':[fndg],'parameter':'C0449381':[lbtr]).
domain_replace('mental health':'C0025353':[idcn],'mental health':'C0025353':[orgf]).
domain_replace('Retrieval':'C1514918':[hlca],'Retrieval':'C1514918':[mcha]).
domain_replace('Architecture':'C0003737':[ocdi],'Architecture':'C0003737':[mnob]).
domain_replace('System':'C0449913':[ftcn],'System':'C0449913':[mnob]).
domain_replace('clinical document':'C1551343':[idcn],'clinical document':'C1551343':[mnob]).
domain_replace('Document type':'C1547673':[clas],'Document type':'C1547673':[mnob]).
domain_replace('Documents':'C1301746':[inpr],'Documents':'C1301746':[mnob]).
domain_replace('Experimental Laboratories':'C0237699':[mnob],'Experimental Laboratories':'C0237699':[orgt]).
domain_replace('electronic data':'C1553520':[idcn],'electronic data':'C1553520':[mnob]).
domain_replace('Classification':'C0008902':[clas],'Classification':'C0008902':[mcha]).
domain_replace('Transcript':'C1519595':[nnon],'Transcript':'C1519595':[mnob]).
domain_replace('Logistics':'C0242415':[ocac],'Logistics':'C0242415':[mcha]).
domain_replace('Decision Support Systems, Clinical':'C0525070':[inpr],'Clinical Decision Support Systems':'C0525070':[mnob]). % 11/17/2017: Here too, entry is in the plural just like it is here
domain_replace('Decision Support Systems':'C0870393':[inpr,mnob],'Decision Support Systems':'C0870393':[mnob]). % 11/17/2017: Here too, entry is in the plural just like it is here
domain_replace('roentgenographic':'C0034571':[ftcn],'X - ray':'C0034571':[diap]).
domain_replace('Imaging modality':'C1275506':[ftcn],'Imaging modality':'C1275506':[diap]).
domain_replace('Does hit':'C0596020':[fndg],'HITS':'C0596020':[mcha,acty]).
domain_replace('Identification (Psychology)':'C0020792':[menp],'Identification':'C0020792':[acty,diap]).
domain_replace('Techniques':'C0449851':[ftcn],'Techniques':'C0449851':[mnob]).
domain_replace('Mechanisms':'C0441712':[ftcn],'Mechanisms':'C0441712':[mnob]).
domain_replace('Specificity':'C0037791':[qnco],'Specificity':'C0037791':[mnob]).
domain_replace('Antimicrobial susceptibility':'C0427965':[fndg],'Sensitivity':'C0427965':[orga]).
domain_replace('Weight':'C0043100':[qnco],'Weight':'C0043100':[orga,clna]).
domain_replace('Body mass index':'C1305855':[clna],'Body mass index':'C1305855':[orga,clna]).
domain_replace('Intake':'C1512806':[ftcn],'Intake':'C1512806':[orgf,biof]).
domain_replace('essential nutrients':'C0684140':[chvf],'essential nutrients':'C0684140':[sbst,food]). % 11/17/2017: Here too, entry is in the plural just like it is here
domain_replace('Nutrition, Trace Elements Research Activity':'C1522501':[resa],'micronutrient':'C1522501':[sbst,food]).
domain_replace('Health Surveys':'C0018762':[resa],'Health Surveys':'C0018762':[resa]). % 11/17/2017: And here too
domain_replace('Current non-drinker of alcohol':'C0457801':[socb],'Current non-drinker of alcohol':'C0457801':[humn,popg]).
domain_replace('Recommendation':'C0034866':[idcn],'Recommendation':'C0034866':[hlca]).
domain_replace('Meta-Analysis':'C0920317':[inpr],'Meta-Analysis':'C0920317':[resa]).
domain_replace('treatment options':'C0683525':[inpr],'treatment options':'C0683525':[topp]).
domain_replace('intervention program':'C0599917':[topp],'intervention program':'C0599917':[topp]).
domain_replace('Program Evaluation':'C0033336':[ocac],'Program Evaluation':'C0033336':[resa]).
domain_replace('Research Subject':'C0080105':[idcn,popg],'Research Subject':'C0080105':[popg,humn]).
domain_replace('search':'C1552603':[idcn],'search':'C1552603':[acty,mcha]).
domain_replace('Cessation of smoking':'C0085134':[inbe],'Cessation of smoking':'C0085134':[inbe,hlca]).
domain_replace('Relapse':'C0035020':[phpr],'Relapse':'C0035020':[fndg,phpr]).
domain_replace('Disease susceptibility':'C0012655':[clna],'Predisposition':'C0012655':[clna]).
domain_replace('Decision Aids':'C0086104':[inpr],'Decision Aids':'C0086104':[mnob]).
domain_replace('Information, health':'C0850397':[hlca],'Health information':'C0850397':[mnob]).
domain_replace('Counseling, health':'C0850252':[fndg],'Health counseling':'C0850252':[hlca]).
domain_replace('Reference':'C1514811':[idcn],'Reference':'C1514811':[mnob]).
domain_replace('activity level':'C0683317':[fndg],'activity level':'C0683317':[orgf,orga]).
domain_replace('Body mass index procedure':'C0005893':[diap],'Body mass index':'C0005893':[clna]).
domain_replace('trend':'C1521798':[tmco],'trend':'C1521798':[phpr]).
domain_replace('Records':'C0034869':[inpr],'Records':'C0034869':[mnob]).
domain_replace('Living Wills':'C0023914':[rnlw],'Living Wills':'C0023914':[mnob,rnlw]).
domain_replace('Hemophilia - specialty':'C1321589':[bmod],'Hemophilia':'C1321589':[dsyn]).
domain_replace('Physical Fitness':'C0031812':[idcn],'Physical Fitness':'C0031812':[orga,clna]).
domain_replace('Hospital Administration':'C0019948':[hlca],'Hospital Administration':'C0019948':[ocdi]).
domain_replace('Child Custody':'C0008069':[idcn],'Child Custody':'C0008069':[gora]).
domain_replace('Mann-Whitney U Test':'C0242927':[qnco],'Mann-Whitney U Test':'C0242927':[lbtr]).
domain_replace('Dental Hygiene':'C0029164':[inbe],'Dental Hygiene':'C0029164':[inbe,hlca]).
domain_replace('Pain Management, Registered Nurse - NUCCProviderCodes':'C1556002':[inpr],'Pain Management':'C1556002':[topp]).
domain_replace('Patient education (procedure)':'C0030688':[edac],'Patient education':'C0030688':[edac]).
domain_replace('Anxiety, Separation':'C0003477':[dsyn],'Separation Anxiety':'C0003477':[mobd]).
domain_replace('Criminal Justice':'C0086072':[idcn],'Criminal Justice':'C0086072':[gora]).
domain_replace('Clinical Nurse Specialist - Long Term Care':'C1552238':[prog],'Long Term Care':'C1552238':[hlca]).
domain_replace('Health Services Accessibility':'C0018748':[qlco],'Health Services Accessibility':'C0018748':[mnob]).
domain_replace('Information':'C1533716':[idcn],'Information':'C1533716':[mnob]).
domain_replace('Low Birth Weights':'C0024032':[fndg],'Low Birth Weights':'C0024032':[orga,fndg]). % 11/17/2017: And here 
domain_replace('Eidetic Imagery':'C0013727':[menp],'Eidetic Imagery':'C0013727':[topp]).
domain_replace('Psyche structure':'C0229992':[menp],'Mind':'C0229992':[bpoc]).
domain_replace('Exercise, Aerobic':'C0001701':[topp],'Aerobic Exercise':'C0001701':[dora]).
domain_replace('Marihuana':'C0024808':[orch,phsu],'Marihuana':'C0024808':[hops,orch,phsu]).
domain_replace('Tobacco':'C0040329':[hops,orch,phsu],'Tobacco':'C0040329':[hops,orch]).
domain_replace('Encounter due to tobacco use':'C0040335':[fndg],'Tobacco use':'C0040335':[inbe]).
domain_replace('Holistic Health':'C0019844':[idcn],'Holistic Health':'C0019844':[bmod]).
domain_replace('employee health insurance':'C0681104':[qnco],'employee health insurance':'C0681104':[mnob]).
domain_replace('Property':'C0871161':[qlco],'Property':'C0871161':[mnob]).
domain_replace('motivational interviewing':'C0683474':[topp],'motivational interviewing':'C0683474':[mnob]).
domain_replace('Sleep, REM':'C0037322':[menp],'REM Sleep':'C0037322':[orgf,biof]).
domain_replace('Neologism':'C0233647':[sosy],'Neologism':'C0233647':[lang]).
domain_replace('Body Size':'C0005901':[orga,qnco],'Body Size':'C0005901':[orga,clna]).
domain_replace('Oral health':'C0029162':[idcn],'Oral health':'C0029162':[hlca,orgf]).
domain_replace('Sustained attention':'C0589099':[menp],'Sustained attention':'C0589099':[acty]).
domain_replace('Blood groups':'C0005810':[inpr],'Blood groups':'C0005810':[orga]). % 11/17/2017: This one also
domain_replace('Sleep Deprivation':'C0037316':[fndg],'Sleep Deprivation':'C0037316':[sosy,dsyn]).
domain_replace('Information Dissemination':'C0021417':[socb],'Information Dissemination':'C0021417':[acty,socb]).
domain_replace('Professional Organization or Group':'C1522486':[prog],'Professional Organization or Group':'C1522486':[orgt]).
domain_replace('Social Interaction':'C0037420':[fndg],'Social Interaction':'C0037420':[socb,inbe]).
domain_replace('Smokeless Tobacco':'C0040338':[sbst],'Smokeless Tobacco':'C0040338':[hops,sbst]).
domain_replace('Oral Tobacco':'C0008038':[sbst],'Oral Tobacco':'C0008038':[hops,sbst]).
domain_replace('Aggressive behavior':'C0001807':[mobd],'Aggressive behavior':'C0001807':[mobd,inbe]).
domain_replace('Ability to perform cognitive activity':'C0392334':[menp],'Cognitive Functioning':'C0392334':[orgf]).
domain_replace('Audiotapes':'C0004295':[mnob],'Audiotapes':'C0004295':[mnob]).
domain_replace('Alexithymia':'C0002020':[sosy],'Alexithymia':'C0002020':[mobd,sosy]).
domain_replace('Medicare':'C0018717':[rnlw],'Medicare':'C0018717':[mnob,rnlw]).
domain_replace('Government Programs':'C0018106':[gora],'Government Programs':'C0018106':[mnob,gora]).
domain_replace('Information Resources Management':'C0687753':[ocac],'Information Resources Management':'C0687753':[ocac,mnob]).
domain_replace('Social Demonstrations':'C0237560':[socb],'Social Demonstrations':'C0237560':[socb,evnt]).
domain_replace('Video Games':'C0042649':[mnob],'Video Games':'C0042649':[dora,mnob]).
domain_replace('emotional development':'C0679103':[menp],'emotional development':'C0679103':[orgf]).
domain_replace('Community Mental Health Centers':'C0009474':[hcro,mnob],'Community Mental Health Centers':'C0009474':[hcro,orgt]). % Also plural
domain_replace('Community Mental Health Services':'C0009475':[hlca],'Community Mental Health Services':'C0009475':[hcro,orgt]).
domain_replace('Acoustics':'C0001166':[npop],'Acoustics':'C0001166':[ocdi,phpr]).
domain_replace('Encounter due to palliative Care':'C0700049':[fndg],'Palliative Care':'C0700049':[topp,hlca]).
domain_replace('Self Esteem':'C0036597':[inbe,menp],'Self Esteem':'C0036597':[inbe]).
domain_replace('medical malpractice':'C0680560':[qlco],'medical malpractice':'C0680560':[acty]).
domain_replace('Body Weight':'C0005910':[orga],'Body Weight':'C0005910':[orga,clna]).
domain_replace('Executive Functioning':'C0935584':[menp],'Executive Functioning':'C0935584':[orgf,biof]).
domain_replace('Body Weight decreased':'C0043096':[fndg],'Body Weight decreased':'C0043096':[orgf,orga]).
domain_replace('Walk-in clinic (environment)':'C0338034':[hcro,mnob],'Walk-in clinic':'C0338034':[hcro,orgt]).
domain_replace('Epistemology':'C0376553':[inpr],'Epistemology':'C0376553':[ocdi]).
domain_replace('Systemic arterial pressure':'C1272641':[fndg],'Systemic arterial pressure':'C1272641':[orgf]).
domain_replace('Malpractice':'C0024650':[qlco],'Malpractice':'C0024650':[acty]).
domain_replace('User-Computer Interface':'C0042117':[inpr],'User-Computer Interface':'C0042117':[mcha]).
domain_replace('Overcorrection':'C0871026':[topp],'Overcorrection':'C0871026':[acty]).
domain_replace('cell body (neuron)':'C0599444':[celc],'cell body':'C0599444':[celc]).
domain_replace('Physical shape':'C0700329':[fndg],'Physical shape':'C0700329':[orga,clna]).
domain_replace('organizational behavior':'C0679152':[socb],'organizational behavior':'C0679152':[socb,inbe]).
domain_replace('Physical Development':'C0871079':[npop],'Physical Development':'C0871079':[orgf,npop]).
domain_replace('Functional behavioral assessment':'C0558024':[hlca],'Functional behavioral assessment':'C0558024':[diap,hlca]).
domain_replace('home visiting program':'C0681141':[shro],'home visiting program':'C0681141':[hlca]).
domain_replace('Daily Activities':'C0871707':[dora],'Daily Activities':'C0871707':[dora,acty]).
domain_replace('dysfunctional family':'C0687115':[mobd],'dysfunctional family':'C0687115':[famg,mobd]).
domain_replace('Body Height':'C0005890':[orga,qnco],'Body Height':'C0005890':[orga]).
domain_replace('Weight Gain':'C0043094':[fndg],'Weight Gain':'C0043094':[sosy,orga]).
domain_replace('Private Sector':'C0033176':[popg],'Private Sector':'C0033176':[orgt,popg]).
domain_replace('Food Safety':'C1456535':[hcpp],'Food Safety':'C1456535':[hlca]).
domain_replace('young adulthood':'C0680085':[aggp],'young adulthood':'C0680085':[tmco,aggp]). % it does not work
domain_replace('Adulthood':'C0700597':[tmco],'Adulthood':'C0700597':[tmco,aggp]).
domain_replace('Prospective Studies':'C0033522':[qnco,resa],'Prospective Studies':'C0033522':[resa]). % Also plural in the Lexicon
domain_replace('Dietary Assessment':'C0814244':[inpr],'Dietary Assessment':'C0814244':[hlca,resa]). % 08/10/2016
domain_replace('Energy Intake':'C0006777':[qnco],'Energy Intake':'C0006777':[orgf]).
domain_replace('Adult':'C0001675':[aggp],'Adult':'C0001675':[popg]). % 08/16/16
domain_replace('experience (practice)':'C0237607':[menp],'Practice':'C0237607':[acty]). % 08/16/16
domain_replace('Medical follow-up':'C0420318':[fndg],'Medical follow-up':'C0420318':[hlca]). % 08/18/16
domain_replace('Maine':'C0024497':[geoa],'Maine':'C0024497':[idcn]). % 08/19/16 to avoid FPs with ISA
domain_replace('Massachusetts':'C0024874':[geoa],'Massachusetts':'C0024874':[idcn]). % 08/19/16 to avoid FPs with ISA
domain_replace('New Hampshire':'C0027969':[geoa],'New Hampshire':'C0027969':[idcn]). % 08/19/16 to avoid FPs with ISA
domain_replace('Wisconsin':'C0043193':[geoa],'Wisconsin':'C0043193':[idcn]). % 08/19/16 to avoid FPs with ISA
domain_replace('Waist circumference':'C0455829':[fndg],'Waist circumference':'C0455829':[orga,fndg]). % 08/22/2016
domain_replace('Author':'C0221192':[idcn,prog],'Author':'C0221192':[prog]). % 09/09/2016
domain_replace('statistical tests':'C0237913':[inpr],'statistical tests':'C0237913':[resa]). % 09/07/2016
domain_replace('HORMONE REPLACEMENT':'C0745034':[dsyn],'hormone replacement':'C0745034':[topp]). % 09/12/2016
domain_replace('Relations':'C0869014':[socb],'Relations':'C0869014':[socb,phpr]). % 09/13/2016
domain_replace('Relationships':'C0439849':[qlco],'Relationships':'C0439849':[phpr,socb]). % 09/13/2016
domain_replace('Anticarcinogenic Agents':'C0085169':[phsu],'Anticarcinogenic Agents':'C0085169':[sbst,phsu]). % 09/13/2016
domain_replace('Agent':'C0450442':[chvf],'Agent':'C0450442':[phsu,orgm,sbst]). % 11/03/2016
domain_replace('Thinness':'C0039870':[fndg],'Thinness':'C0039870':[fndg,clna]). % 09/13/2016
domain_replace('Fruit intake':'C0556227':[fndg],'Fruit intake':'C0556227':[orgf,inbe,hlca]). % 09/14/2016
domain_replace('Retrospective Studies':'C0035363':[qnco],'Retrospective Studies':'C0035363':[resa]). % 09/14/2016
domain_replace('Events':'C0441471':[evnt],'Events':'C0441471':[evnt,fndg]). % 09/15/2016
domain_replace('Hip circumference':'C0562350':[fndg],'Hip circumference':'C0562350':[orga,clna]). % 09/15/2016
domain_replace('Waist-Hip Ratio':'C0205682':[orga,qnco],'Waist-Hip Ratio':'C0205682':[orga,clna]). % 09/15/2016
domain_replace('Condition':'C0348080':[qlco],'Condition':'C0348080':[fndg,sosy]). % 09/15/2016
domain_replace('Percent (qualifier value)':'C0439165':[qnco],'Percentage':'C0439165':[qnco]). % 09/15/2016
domain_replace('Community':'C0009462':[geoa],'Community':'C0009462':[popg]). % 09/15/2016
domain_replace('{PROVIDER}':'C1441436':[hlca],'Provider':'C1441436':[prog,orgt]). % 09/16/2016
domain_replace('Self Efficacy':'C0600564':[menp],'Self Efficacy':'C0600564':[inbe]). % 09/16/2016
domain_replace('Effectiveness':'C1280519':[qlco],'Effectiveness':'C1280519':[resa]). % 09/16/2016
domain_replace('Folate intake':'C0564428':[fndg],'Folate intake':'C0564428':[hlca]). % 09/21/2016
domain_replace('Vitamin intake':'C0518041':[fndg],'Vitamin intake':'C0518041':[hlca]). % 09/21/2016
domain_replace('Mineral intake':'C0518042':[fndg],'Mineral intake':'C0518042':[hlca,orgf]). % 09/21/2016
domain_replace('Iron intake':'C0518043':[fndg],'Iron intake':'C0518043':[hlca,orgf]). % 09/21/2016
domain_replace('Zinc intake':'C0564449':[fndg],'Zinc intake':'C0564449':[hlca,orgf]). % 09/21/2016
domain_replace('Supplement':'C1549514':[idcn],'Supplement':'C1549514':[phsu,vita]). % 09/21/2016
domain_replace('Fish intake':'C0556216':[fndg],'Fish intake':'C0556216':[inbe,orgf]). % 09/21/2016
domain_replace('Lymph':'C0024202':[bdsu],'Lymphatic fluid':'C0024202':[bdsu]). % 10/06/2016
domain_replace('Care plan':'C0178916':[inpr],'Care plan':'C0178916':[hlca]). % 10/07/2016
domain_replace('Multivariate Analysis':'C0026777':[qnco],'Multivariate Analysis':'C0026777':[resa]). % 10/26/2016
domain_replace('Tumor size':'C0475440':[spco],'Tumor size':'C0475440':[clna]). % 10/26/2016
domain_replace('Disease-Free Survival':'C0242793':[qnco],'Disease-Free Survival':'C0242793':[orgf]). % 10/26/2016
domain_replace('Resident':'C1549439':[inpr],'Resident':'C1549439':[popg,humn]). % 10/31/2016
domain_replace('retirement community':'C1550449':[mnob],'retirement community':'C1550449':[popg,aggp]). % 10/31/2016
domain_replace('Daily drinker':'C0556339':[fndg],'Daily drinker':'C0556339':[popg,humn]). % 10/31/2016
domain_replace('Beta carotene measurement':'C0696105':[lbpr],'Beta carotene level':'C0696105':[lbpr,lbtr]). % 10/31/2016
domain_replace('Cholesterol':'C0008377':[bacs,strd],'Cholesterol':'C0008377':[bacs,strd,sbst]). % 10/31/2016
domain_replace('Egg Food Product':'C0013710':[food],'Egg':'C0013710':[food]). % 11/03/2016
domain_replace('Diet, Fat-Restricted':'C0242970':[topp],'Low-Fat Diet':'C0242970':[food,topp]). % 11/03/2016
domain_replace('Support Groups':'C0036606':[shro],'Support Groups':'C0036606':[popg]). % 11/07/2016
domain_replace('Small':'C0700321':[qnco],'Small':'C0700321':[qlco]). % 11/07/2016
domain_replace('Theoretical model':'C0026350':[inpr],'Theoretical model':'C0026350':[resa]). % 11/07/2016
domain_replace('Survival Analysis':'C0038953':[qnco],'Survival Analysis':'C0038953':[resa]). % 11/07/2016
domain_replace('control':'C0243148':[ftcn],'controls':'C0243148':[popg]). % 11/07/2016
domain_replace('Postmenopause':'C0206159':[orgf],'Postmenopause':'C0206159':[orgf,tmco]). % 11/07/2016
domain_replace('/period':'C0439531':[tmco],'Period':'C0439531':[tmco]). % 11/07/2016
domain_replace('disease mortality':'C0681679':[qnco],'disease mortality':'C0681679':[orgf]). % 11/08/2016
domain_replace('FAT INTAKE':'C0489488':[orga],'fat intake':'C0489488':[orgf,inbe]). % 11/09/2016
domain_replace('Cohort Studies':'C0009247':[qnco],'Cohort Studies':'C0009247':[resa]). % 11/09/2016
domain_replace('Drivers of Vehicles':'C0684312':[popg],'Drivers':'C0684312':[popg]). % 11/09/2016
domain_replace('Female':'C0015780':[orga],'Female':'C0015780':[orga,popg]). % 11/09/2016
domain_replace('student':'C0038492':[prog],'student':'C0038492':[popg,humn]). % 11/09/2016
domain_replace('Participant':'C0679646':[popg],'Participant':'C0679646':[popg,humn]). % 11/10/2016
domain_replace('nonparticipant':'C0683602':[popg],'nonparticipant':'C0683602':[popg,humn]). % 11/10/2016
domain_replace('Data':'C1511726':[idcn],'Data':'C1511726':[mnob]). % 04/26/2016
domain_replace('Medical History':'C0262926':[fndg],'Medical History':'C0262926':[resa]). 
domain_replace('Feeding behaviors':'C0015745':[inbe],'Feeding behaviors':'C0015745':[inbe,socb]). % 11/10/2016
domain_replace('Human cells':'C0427861':[lbtr],'Human cells':'C0427861':[cell]). % 04/26/2016
domain_replace('TEMPERATE CLIMATE':'C0241334':[fndg],'Temperate climate':'C0241334':[npop]). % 04/26/2016
domain_replace('Does dusting':'C0563970':[fndg],'Dust':'C0563970':[mnob]). % 05/01/2017
domain_replace('Monocyte production':'C0312842':[ortf],'Monocyte maturation':'C0312842':[celf]). % 05/01/2017
domain_replace('Integumentary system':'C0037267':[bdsy],'skin':'C0037267':[tisu]). % 05/04/2017
domain_replace('Vaginal Discharge':'C0227791':[dsyn],'Vaginal Discharge':'C0227791':[bdsu]). % 05/05/2017
domain_replace('Synthesis':'C0869032':[phpr],'Synthesis':'C0869032':[phpr,orgf]). % 05/05/2017
domain_replace('geographic population':'C0032659':[qnco],'geographic population':'C0032659':[popg]). % 05/08/2017
domain_replace('glucose uptake':'C0599781':[ortf],'glucose uptake':'C0599781':[celf]). % 05/16/2017 -it's already a celf in 2015 Metathesaurus version
domain_replace('uptake':'C0243144':[phsf],'uptake':'C0243144':[phsf,ortf,celf,genf]). % 05/16/2017
domain_replace('Blood Glucose':'C0005802':[carb],'Blood Glucose':'C0005802':[lbpr]). % 10/10/2017
domain_replace('Symptomatic':'C0231220':[ftcn],'Symptomatic':'C0231220':[fndg]). % 05/18/2017
domain_replace('Diagnosis':'C0011900':[hlca],'Diagnosis':'C0011900':[hlca,diap]). % 05/22/2017
domain_replace('Tablets':'C0039225':[bodm],'Tablets':'C0039225':[phsu]). % 05/23/2017 entry source: GS
domain_replace('plasma drug concentration':'C0683150':[qnco],'plasma drug concentration':'C0683150':[lbtr]). % 05/23/2017 entry source: GS
domain_replace('Luteal Phase':'C0024153':[tmco],'Luteal Phase':'C0024153':[ortf,tmco]). % 05/23/2017 entry source: GS
domain_replace('Kinetics':'C0022702':[idcn],'Kinetics':'C0022702':[npop]). % 05/23/2017 entry source: GS
domain_replace('Pharmacokinetic':'C0031328':[ftcn],'Pharmacokinetic':'C0031328':[phsf,biof]). % 05/23/2017 entry source: GS
domain_replace('immunoreaction':'C0872193':[orgf],'immunoreaction':'C0872193':[orgf,ortf]). % GR 05/25/2017 as per GS - PMID 15592935 Vascular endothelium, erythrocytes, smooth muscle and inflammatory cells (except macrophages) in the allergic patients exhibited stronger HO-1 immunoreaction compared to the control.
domain_replace('Lymphocyte Activation':'C0024262':[celf],'Lymphocyte Activation':'C0024262':[celf,phsf]). % 06/01/2017 source: GENIA 7510689_S14 These findings highlight the role of thrombin as a potential regulator of T lymphocyte activation.
domain_replace('Trans-Activation (Genetics)':'C0040624':[genf],'Transactivation':'C0040624':[genf]). % 06/01/2017 source: GENIA 9211847_S3 The retinoblastoma susceptibility gene product suppressed the transactivation of various promoters.
domain_replace('degradation':'C0243125':[ftcn],'degradation':'C0243125':[moft]). % 06/02/2017 source: GENIA 9710600_S3 Tax expression promotes N-terminal phosphorylation and degradation of IkappaB alpha.
domain_replace('definition':'C1550452':[idcn],'definition':'C1550452':[resa]). % 06/08/2017 
domain_replace('Report (document)':'C0684224':[inpr],'Report':'C0684224':[resa]). % 06/08/2017 
domain_replace('density':'C0178587':[npop],'density':'C0178587':[npop,lbtr,orga,qnco]). % 06/13/2017 
domain_replace('Heterosexuality':'C0019421':[socb],'Heterosexuality':'C0019421':[socb,inbe]). % 06/13/2017 
domain_replace('Homosexuality':'C0019900':[socb],'Homosexuality':'C0019900':[socb,inbe]). % 06/14/2017 
domain_replace('Dimerization':'C0376525':[npop],'Dimerization':'C0376525':[npop,moft]). % 06/22/2017 
domain_replace('Regulation':'C0851285':[gora],'Regulation':'C0851285':[genf,celf,phsf]). % 06/28/2017
domain_replace('Proliferation':'C0334094':[ftcn],'Proliferation':'C0334094':[phpr]). % 06/28/2017
domain_replace('Bisexuality':'C0005639':[socb],'Bisexuality':'C0005639':[socb,inbe]). % 06/29/2017 
domain_replace('Immunoreactivity':'C0597879':[moft],'Immunoreactivity':'C0597879':[moft,orgf]). % 05/25/2017 entry source: GS 
domain_replace('[V]Follow-up examination':'C0260832':[fndg],'Follow-up examination':'C0260832':[hlca]). % 09/22/2017 
domain_replace('Fused structure':'C0332466':[ftcn],'Fused structure':'C0332466':[topp,ftcn]). % 10/02/2017 
domain_replace('Energy Metabolism':'C0014272':[phsf],'Energy Metabolism':'C0014272':[phsf,orgf]). % 10/02/2017 
domain_replace('User':'C1548600':[idcn],'User':'C1548600':[popg,humn]). % 10/02/2017 
domain_replace('Carrier of disorder':'C0560175':[fndg],'carrier':'C0560175':[podg,fndg]). % 10/03/2017 
domain_replace('Specimen':'C0370003':[sbst],'Specimen':'C0370003':[sbst,tisu]). % 10/05/2017 
domain_replace('EUROPEAN, NORTHERN':'C0239310':[fndg],'North European':'C0239310':[popg]). % 10/05/2017 
domain_replace('Not significant':'C1273937':[fndg],'Not significant':'C1273937':[qlco]). % 10/06/2017 
domain_replace('Prolonged periods':'C0425945':[fndg],'Prolonged periods':'C0425945':[tmco]). % 10/06/2017 
domain_replace('prophylactics use':'C0679782':[inbe],'prophylactic use':'C0679782':[hlca,inbe]). % 10/10/2017 
domain_replace('Sedation':'C0600097':[sosy],'Sedation':'C0600097':[topp]). % 10/11/2017 
domain_replace('Lack of sensation':'C0278134':[sosy],'anesthesia':'C0278134':[topp]). % 10/11/2017 
domain_replace('innate immune response':'C1155265':[patf],'innate immune response':'C1155265':[orgf,patf]). % 10/16/2017 
domain_replace('Asthma severity':'C0581122':[hlca],'Asthma severity':'C0581122':[fndg,clas]). % 10/17/2017 
domain_replace('Laparoscopy':'C0031150':[diap],'Laparoscopy':'C0031150':[diap,topp]). % 10/18/2017 
domain_replace('Laparoscopic approach':'C0393360':[spco],'Laparoscopic approach':'C0393360':[topp]). % 10/18/2017 
domain_replace('Ultrafine':'C0077801':[orch],'ultrafine':'C0077801':[qlco]). % 10/19/2017 
domain_replace('particle':'C0597177':[chvs],'particle':'C0597177':[hops,sbst]). % 10/19/2017  (like dust)
domain_replace('Linear Models':'C0023732':[qnco],'Linear Models':'C0023732':[mcha,resa]). % 10/19/2017 
domain_replace('Employed':'C0557351':[fndg],'Employed':'C0557351':[qlco]). % 10/26/2017 
domain_replace('Passive Cutaneous Anaphylaxis':'C0030625':[lbpr],'Passive Cutaneous Anaphylaxis':'C0030625':[lbpr,fndg]). % 11/28/2017 
domain_replace('Very slow':'C0443347':[fndg],'Very slow':'C0443347':[qlco]). % 11/30/2017 
domain_replace('Delayed Auditory Feedback':'C0870399':[fndg],'Delayed Auditory Feedback':'C0870399':[topp]). % 12/06/2017 
domain_replace('Auditory Feedback':'C0596134':[menp],'Auditory Feedback':'C0596134':[topp]). % 12/06/2017 
domain_replace('Auditory Processing Disorder, Central':'C0751257':[mobd],'Auditory Processing Disorder, Central':'C0751257':[dsyn]). % 12/06/2017 
domain_replace('Auditory Perceptual Disorders':'C0004310':[mobd],'Auditory Perceptual Disorders':'C0004310':[dsyn,mobd]). % 12/06/2017 
domain_replace('Human plasma':'C0544357':[phsu],'Human plasma':'C0544357':[phsu,bdsu]). % 12/06/2017 
domain_replace('Large tumor':'C0475278':[qlco],'Large tumor':'C0475278':[neop]). % 12/18/2017
domain_replace('Drug vehicle':'C0042444':[bodm],'Drug vehicle':'C0042444':[medd,clnd]). % GR 01/16/2018
domain_replace('Developing Countries':'C0011750':[qlco],'Developing Countries':'C0011750':[geoa]). % GR 01/18/2018
domain_replace('Cardiac index':'C0428776':[fndg],'Cardiac index':'C0428776':[lbtr]). % GR 01/18/2018
domain_replace('Ergometry':'C0085143':[hlca],'Ergometry':'C0085143':[lbtr,hlca]). % GR Jan 2018
% domain_replace('':'':[],'':'':[]).% ---Wrong Concept name for "" in Replace---------
% ---Correct Concept name is "null"---------
% domain_replace('':'':[],'':'':[]).
% This predicate covers two types of concepts.
% (1) Concepts already in UMLS that we want to add a new synonym for, so that we can map a new string to the concept.
% (2) Entirely new domain concepts not found in UMLS.
% The first argument is the string that we want to map and should be in lowercase.
% The following clauses cover the concepts of type (1).
domain_concept('group of antibody','Antibodies':'C0003241':[aapp,imft]). % GR Jan 2018
domain_concept('group of autoantibody','Autoantibodies':'C0004358':[aapp,imft]). % GR Jan 2018
domain_concept('prophylactic therapy','Prophylactic treatment':'C0199176':[topp]). % GR Jan 2018
domain_concept('vanishing testis syndrome','Testicular regression syndrome':'C0266427':[cgab,dsyn]). % GR Jan 2018
domain_concept('allograft recipient','Recipient, Transplant':'C0376387':[podg]). % GR Jan 2018 
domain_concept('vehicle','Drug vehicle':'C0042444':[medd,clnd]). % GR Jan 2018
domain_concept('valvuloplasty of the mitral valve','Repair of mitral valve':'C0396849':[topp]). % 12/13/2017 
domain_concept('Computed Tomography','X-Ray Computed Tomography':'C0040405':[diap]). % 12/12/2017 
domain_concept('delayed sidetone','Delayed Auditory Feedback':'C0870399':[topp]). % 12/06/2017 
domain_concept('locus','genetic locus':'C0678933':[gngm]). % 05/17/2017 entry source: GS 15634358 The evolution of ray pattern has involved allelic variation at multiple loci.
domain_concept('loci','genetic locus':'C0678933':[gngm]). % 05/17/2017 entry source: GS 15634358 The evolution of ray pattern has involved allelic variation at multiple loci.
domain_concept('liver transplantation','Transplantation of liver':'C0023911':[topp]). % 10/16/2017 
domain_concept('Caucasian population','Caucasoid Race':'C0007457':[popg]). % 10/19/2017 
domain_concept('paediatric admission','Hospital admission':'C0184666':[hlca]). % 10/19/2017 
domain_concept('newborn baby','Infant, Newborn':'C0021289':[aggp]). % 10/19/2017 
domain_concept('preterm newborn','Infant, Premature':'C0021294':[aggp]). % 10/19/2017 
domain_concept('newborn','Infant, Newborn':'C0021289':[aggp]). % 10/19/2017 
domain_concept('motor disturbance','Motor dysfunction':'C0234131':[sosy]). % 10/19/2017 
domain_concept('motor impairment','Motor dysfunction':'C0234131':[sosy]). % 10/19/2017 
domain_concept('transplant','Transplantation':'C0040732':[topp]). % 06/23/2017 
domain_concept('tumoral mass','Neoplasms':'C0027651':[neop]). % 11/16/2017 
domain_concept('passive cutaneous anaphylaxis reaction','Passive Cutaneous Anaphylaxis':'C0030625':[lbpr,fndg]). % 11/28/2017 
domain_concept('heterosexual contact','Heterosexuality':'C0019421':[socb,inbe]). % 06/13/2017 
domain_concept('homosexual contact','Homosexuality':'C0019900':[socb,inbe]). % 06/13/2017 
domain_concept('bisexual contact','Bisexuality':'C0005639':[socb,inbe]). % 06/29/2017 
domain_concept('pharmacokinetic process','Pharmacokinetic':'C0031328':[phsf,biof]). % 05/23/2017 entry source: GS
domain_concept('behavioral disturbance','Abnormal behavior':'C0233514':[mobd]). % 10/16/2017 
domain_concept('antiplatelet','Antiplatelet Agents':'C0085826':[phsu]). % 10/17/2017 
domain_concept('menstrual cycle rhythm','Menstrual cycle':'C0025329':[orgf]). % 05/23/2017 entry source: GS
% domain_concept('tablet dose','Tablets':'C0039225':[phsu]).domain_concept('tablet dose','Tablets':'C0039225':[phsu]). % 05/23/2017 entry source: GS but commented out as per Halil
domain_concept('anti-rejection drug','Immunosuppressive Agents':'C0021081':[phsu]). % 05/23/2017 entry source: GS
domain_concept('immunosuppressive reagent','Immunosuppressive Agents':'C0021081':[phsu]). % 05/23/2017 entry source: GS
domain_concept('immunosuppressant drug','Immunosuppressive Agents':'C0021081':[phsu]). % 05/23/2017 entry source: GS
domain_concept('survival','Survival':'C0038952':[orgf]). % 05/23/2017 entry source: GS
domain_concept('immune reaction','immunoreaction':'C0872193':[orgf,ortf]). % 05/25/2017 entry source: GS - PMID 15592935
domain_concept('immunologic reaction','immunoreaction':'C0872193':[orgf,ortf]). % 05/25/2017 entry source: GS - PMID 15592935
domain_concept('immunologic response','Immune response':'C0301872':[orgf]). % 05/25/2017 entry source: GS - PMID 15592935
domain_concept('immune reactivity','Immunoreactivity':'C0597879':[moft,orgf]). % 05/25/2017 entry source: GS 
domain_concept('tax','Tax Protein':'C0079407':[aapp,bacs]). % GR 04/26/2017
domain_concept('sample','Specimen':'C0370003':[sbst,tisu]). % 05/22/2017
domain_concept('control patient','Patients':'C0030705':[podg]). % 05/16/2017
domain_concept('pathogen','Pathogenic organism':'C0450254':[orgm]). % 05/08/2017
domain_concept('bacterial strain','Bacteria':'C0004611':[bact]). % 05/05/2017
domain_concept('tumor spread','tumor growth':'C0598934':[neop]). % 05/15/2017
domain_concept('spread of the tumor','tumor growth':'C0598934':[neop]). % 05/15/2017
domain_concept('tumor proliferation','tumor growth':'C0598934':[neop]). % 05/15/2017
domain_concept('proliferation of the tumor','tumor growth':'C0598934':[neop]). % 05/15/2017
domain_concept('tumor propagation','tumor growth':'C0598934':[neop]). % 05/15/2017
domain_concept('propagation of the tumor','tumor growth':'C0598934':[neop]). % 05/15/2017
domain_concept('disease modifying antirheumatic medication','Antirheumatic Drugs, Disease-Modifying':'C0242708':[phsu]). % 05/17/2017
domain_concept('hippocampal area','Entire hippocampus':'C1284077':[bpoc]). % GR 10/10/2017
domain_concept('cardiac surgery','Cardiac Surgery procedures':'C0018821':[topp]). % GR 10/10/2017
domain_concept('stenting','Percutaneous coronary intervention':'C1532338':[topp]). % GR 10/10/2017
domain_concept('deleterious effect','Adverse effects':'C0879626':[patf]). % GR 10/11/2017
domain_concept('adverse outcome','Adverse event':'C0877248':[fndg]). % GR 10/24/2017
domain_concept('wt/ht2','Body mass index':'C0005893':[clna]). % 11/07/2016
domain_concept('current body mass index','Body mass index':'C0005893':[clna]). % 11/08/2016
domain_concept('body mass index (wt/ht2)','Body mass index':'C0005893':[clna]). % 11/14/2016
domain_concept('current BMI','Body mass index':'C1305855':[orga,clna]). % 11/08/2016
domain_concept('after menopause','Postmenopause':'C0206159':[orgf,tmco]). % 11/07/2016
domain_concept('fruit consumption','Fruit intake':'C0556227':[orgf,inbe,hlca]). % 09/14/2016
domain_concept('food high in fat','Fatty food':'C0453819':[food]). % 08/18/2016
domain_concept('diet high in fat','Fatty food':'C0453819':[food]). % 10/27/2016
domain_concept('high in fat','Fatty food':'C0453819':[food]). % 10/27/2016 to keep under observatoin
domain_concept('diet high in complex carbohydrate','Complex carbohydrate food':'C0453805':[food]). % 11/03/2016
domain_concept('food high in complex carbohydrate','Complex carbohydrate food':'C0453805':[food]). % 11/03/2016
domain_concept('dietary assessment instrument','Dietary Assessment':'C0814244':[hlca,resa]). % 08/10/2016
domain_concept('excessive body weight','Overweight':'C0497406':[sosy]). % 08/10/2016
domain_concept('excess weight','Overweight':'C0497406':[sosy]). % 09/12/2016
domain_concept('personal health information','Health information':'C0850397':[mnob]).
domain_concept('health decision aid','Decision Aids':'C0086104':[mnob]).
domain_concept('health counseling session','Health counseling':'C0850252':[hlca]).
domain_concept('cerebral insult','Cerebrovascular accident':'C0038454':[dsyn]).
domain_concept('upper body','Trunk structure':'C0460005':[blor]).
domain_concept('leisure-time activity','Leisure Activities':'C0023292':[dora]).
domain_concept('leisure time activity','Leisure Activities':'C0023292':[dora]). % 09/13/2016
domain_concept('full-term pregnancy','Term pregnancy':'C0232991':[orgf]).
domain_concept('alcohol','Alcoholic Beverages':'C0001967':[food]).
domain_concept('non-drinker','Current non-drinker of alcohol':'C0457801':[humn,popg]).
domain_concept('nondrinker','Current non-drinker of alcohol':'C0457801':[humn,popg]).
domain_concept('strenuous physical activity','Strenuous Exercise':'C1514989':[dora]).
domain_concept('strenuous physical','Strenuous Exercise':'C1514989':[dora]). % 11/09/2016
domain_concept('vigorous physical activity','Strenuous Exercise':'C1514989':[dora]).
domain_concept('vigorous activity','Strenuous Exercise':'C1514989':[dora]).
domain_concept('strenuous activity','Strenuous Exercise':'C1514989':[dora]).
domain_concept('high-intensity exercise','Strenuous Exercise':'C1514989':[dora]). % 08/17/16
domain_concept('intensity exercise','Strenuous Exercise':'C1514989':[dora]).
domain_concept('mood disturbance','Mood Disorders':'C0525045':[mobd]). % 11/07/2016
domain_concept('mood alteration','Mood swings':'C0085633':[mobd]). % 11/11/2016
domain_concept('mood change','Mood swings':'C0085633':[mobd]). % 11/11/2016
domain_concept('median follow-up','Follow-up visit':'C0589121':[hlca]). % 11/03/2016
% domain_concept('mortality','Cessation of life':'C0011065':[orgf]).domain_concept('mortality','Cessation of life':'C0011065':[orgf]). % 11/08/2016 I changed this entry
% domain_concept('recreational physical activity','Leisure physical activity':'C0336910':[dora]).domain_concept('recreational physical activity','Leisure physical activity':'C0336910':[dora]). % 08/10/2016
domain_concept('leisure time physical activity','Leisure physical activity':'C0336910':[dora]). % 09/12/2016
domain_concept('client education','Patient education':'C0030688':[edac]).
domain_concept('adjunctive therapy','adjuvant therapy':'C0677850':[topp]).
domain_concept('adjunct therapy','adjuvant therapy':'C0677850':[topp]).
domain_concept('ambulatory care','ambulatory care services':'C0002423':[hlca]).
domain_concept('migration of nerve cells','neuronal migration':'C1326504':[celf]). % 11/17/2017 also in the plural in the Lexicon
domain_concept('hemodialysis','Dialysis procedure':'C0011946':[topp]).
domain_concept('mirror neuron','Neurons':'C0027882':[cell]).
domain_concept('hospital discharge','Facility Discharge':'C0870539':[hlca]). % GR 04/26/2017
domain_concept('discharge','Facility Discharge':'C0870539':[hlca]). % GR 05/05/2017
domain_concept('history','Medical History':'C0262926':[resa]).
domain_concept('family history','Medical History':'C0262926':[resa]). % GR 04/26/2017
domain_concept('activity of daily living','Daily Activities':'C0871707':[dora,acty]).
domain_concept('increased weight','Weight Gain':'C0043094':[sosy,orga]).
domain_concept('network of hospital','Hospitals':'C0019994':[hcro,mnob]).
domain_concept('hospital network','Hospitals':'C0019994':[hcro,mnob]). % 09/09/16
domain_concept('ors','Odds Ratio':'C0028873':[qnco]).
domain_concept('adjusted odds ratio','Odds Ratio':'C0028873':[qnco]). % 09/12/2016
domain_concept('lack of physical activity','Sedentary':'C0205254':[fndg]). % 08/10/16
domain_concept('physical inactivity','Sedentary':'C0205254':[fndg]). % 11/07/2016
domain_concept('beta carotene supplement','Beta Carotene':'C0053396':[orch,phsu,vita]). % 08/16/16
domain_concept('total calorie intake','Energy Intake':'C0006777':[orgf]). % 08/17/16
domain_concept('caloric intake','Energy Intake':'C0006777':[orgf]). % 09/12/2016
domain_concept('medical visit','Follow-up visit':'C0589121':[hlca]). % 08/18/2016
domain_concept('we','Author':'C0221192':[prog]). % 08/22/2016
domain_concept('standard definition','definition':'C1550452':[resa]). % 09/09/2016
domain_concept('report','Report':'C0684224':[resa]). % 09/09/2016
domain_concept('lean','Thinness':'C0039870':[fndg,clna]). % 09/13/2016
domain_concept('development of colon cancer','Malignant tumor of colon':'C0007102':[neop]). % 09/14/2016
domain_concept('development of endometrial cancer','Malignant neoplasm of endometrium':'C0007103':[neop]). % 09/15/2016
domain_concept('per cent','Percentage':'C0439165':[qnco]). % 09/15/2016
domain_concept('certain form of cancer','Malignant Neoplasms':'C0006826':[neop]). % 09/15/2016
domain_concept('form of cancer','Malignant Neoplasms':'C0006826':[neop]). % 11/03/2016
domain_concept('breast cancer event','Malignant neoplasm of breast':'C0006142':[neop]). % 09/30/2016
domain_concept('breast event','Malignant neoplasm of breast':'C0006142':[neop]). % 09/30/2016
domain_concept('diet low in vegetable and animal fat','Low-Fat Diet':'C0242970':[food,topp]). % 11/03/2016
domain_concept('diet low in plant and animal fat','Low-Fat Diet':'C0242970':[food,topp]). % 11/14/2016
domain_concept('diet low in animal fat','Low-Fat Diet':'C0242970':[food,topp]). % 11/03/2016
domain_concept('diet low in fat','Low-Fat Diet':'C0242970':[food,topp]). % 11/03/2016
domain_concept('low fat diet','Low-Fat Diet':'C0242970':[food,topp]). % 11/03/2016
domain_concept('detoxification enzyme','Enzymes':'C0014442':[enzy,orch]). % 11/03/2016
domain_concept('allium compound','allyl sulfide':'C0051235':[orch,phsu]). % 11/07/2016
domain_concept('metabolic alteration','METABOLIC DISTURBANCE':'C0746556':[dsyn]). % 11/07/2016
domain_concept('cross-sectional data','Data':'C1511726':[mnob]). % 11/08/2016
domain_concept('patient population','Patients':'C0030705':[podg]). % 11/08/2016
domain_concept('minority of patient','Patients':'C0030705':[podg]). % 11/08/2016
domain_concept('body fat accumulation','Obesity':'C0028754':[dsyn]). % 11/09/2016
domain_concept('excess fat accumulation','Obesity':'C0028754':[dsyn]). % 11/09/2016
domain_concept('female nurse','Nurses':'C0028661':[prog]). % 11/09/2016
domain_concept('male and female nurse','Nurses':'C0028661':[prog]). % 11/11/2016
domain_concept('female and male nurse','Nurses':'C0028661':[prog]). % 11/11/2016
domain_concept('licensed driver','Drivers':'C0684312':[popg]). % 11/09/2016
domain_concept('peer support group','Support Groups':'C0036606':[popg]). % 11/10/2016
domain_concept('cancer peer support group','Support Groups':'C0036606':[popg]). 
domain_concept('cancer peer support','Support Groups':'C0036606':[popg]). 
domain_concept('cell adherence','Cell Adhesion':'C0007577':[celf]). % GR 04/27/2017
domain_concept('autoregulatory loop','regulatory loop':'C0678662':[phsf]). % GR 05/02/2017
domain_concept('self-regulatory loop','regulatory loop':'C0678662':[phsf]). % GR 05/02/2017
domain_concept('triplet expansion','Trinucleotide Repeat Expansion':'C0524894':[comd]). % GR 05/05/2017
domain_concept('treated culture','Laboratory culture':'C0430400':[lbpr]). % GR 05/05/2017
domain_concept('allelic variation','Mutation':'C0026882':[genf]). % GR 05/18/2017 source: GS
domain_concept('c. briggsae','Caenorhabditis briggsae':'C0997972':[invt]). % GR 05/18/2017
domain_concept('c. briggsae homolog','Caenorhabditis briggsae':'C0997972':[invt]). % GR 05/18/2017
domain_concept('transactivation function','Transactivation':'C0040624':[genf]). % GR 06/01/2017 GENIA 9211847_S5 Mutation of Ser184 also diminished transactivation function in B cells.
domain_concept('platelet','Blood Platelets':'C0005821':[cell]). % GR 06/13/2017 GENIA 9170401_S2 Activated platelets tether and activate myeloid leukocytes.
domain_concept('radiofrequency energy','radiofrequency':'C0920725':[npop]). % GR 06/14/2017
domain_concept('lumbar fusion','Spinal Fusion':'C0037935':[topp]). % GR 10/02/2017
domain_concept('comorbid anxiety disorder','Anxiety Disorders':'C0003469':[mobd]). % GR 10/04/2017
domain_concept('comorbid anxiety','Anxiety Disorders':'C0003469':[mobd]). % GR 10/04/2017
domain_concept('laparoscopic management','Laparoscopic approach':'C0393360':[topp]). % GR 10/18/2017
domain_concept('dyskinetic behavior','Dyskinetic syndrome':'C0013384':[dsyn]). % GR 10/19/2017
domain_concept('anti-coagulation','anticoagulation':'C0003281':[topp]). % GR 10/23/2017
domain_concept('sperm','sperm cell':'C0037868':[cell]). % GR 11/27/2017
domain_concept('predictive genetic testing','Genetic screening method':'C0679560':[lbpr]). % GR 11/28/2017
domain_concept('renal cortical atrophy','Atrophy of kidney':'C0341698':[acab]). % GR 12/13/2017
% domain_concept('','':'':[]).domain_concept('','':'':[]).
domain_concept('food and drug administration','Food and Drug Administration':'G0000001':[orgt]). 
domain_concept('fda','Food and Drug Administration':'G0000001':[orgt]).
domain_concept('weight for height','Weight for height':'G0000002':[clna,orga]). % GR 05/05/2017
domain_concept('vegetable and fruit consumption','vegetable and fruit consumption':'G0000003':[inbe,orgf,hlca]). % 11/07/2016
domain_concept('vegetable and fruit intake','vegetable and fruit consumption':'G0000003':[inbe,orgf,hlca]). % 11/07/2016
domain_concept('consumption of fruit and vegetable','vegetable and fruit consumption':'G0000003':[inbe,orgf,hlca]). % 11/07/2016
domain_concept('consumption of vegetable and fruit','vegetable and fruit consumption':'G0000003':[inbe,orgf,hlca]). % 11/07/2016
domain_concept('fruit and vegetable consumption','vegetable and fruit consumption':'G0000003':[inbe,orgf,hlca]). % 11/07/2016
domain_concept('intake of fruit and vegetable','vegetable and fruit consumption':'G0000003':[inbe,orgf,hlca]). % GR 10/23/2017
domain_concept('postmenopausal woman','postmenopausal woman':'G0000004':[popg,humn]).
domain_concept('control subject','control subject':'G0000005':[popg,humn]). % GR 05/09/2017
domain_concept('age-matched control','control subject':'G0000005':[popg,humn]). % GR 10/03/2017
domain_concept('aberrant function','aberrant function':'G0000006':[fndg]). % GR 05/09/2017
domain_concept('adjuvant arthritis','adjuvant arthritis':'G0000007':[dsyn]). % 04/26/2017
domain_concept('clinical disease activity','clinical disease activity':'G0000008':[dsyn]). % 04/26/2017
domain_concept('secretion','secretion':'G0000009':[celf,orgf,bdsu]). % 04/27/2017
domain_concept('monocyte adhesion','monocyte adhesion':'G0000010':[celf]). % GR 04/27/2017 GENIA
domain_concept('monocyte adherence','monocyte adhesion':'G0000010':[celf]). % GR 04/27/2017 GENIA
domain_concept('erythroid precursor','erythroid precursor':'G0000011':[cell]). % 05/01/2017
domain_concept('cell differentiation','cell differentiation':'G0000012':[celf]). % 05/01/2017
domain_concept('cellular differentiation','cell differentiation':'G0000012':[celf]). % 05/01/2017 Each drug participated in a different set of interactions within the cavity; however, a number of common contacts were observed with residues also involved in fatty acid binding.
domain_concept('residue','residue':'G0000013':[sbst]). % 05/19/2017 source: GS
domain_concept('blood vessel remodeling','blood vessel remodeling':'G0000014':[ortf]). % 05/19/2017 GS PMID 15750263
domain_concept('vascular remodeling','blood vessel remodeling':'G0000014':[ortf]). % 05/19/2017
domain_concept('organ allograft survival','organ allograft survival':'G0000015':[ortf]). % 05/23/2017 - source: GS
domain_concept('allograft survival','organ allograft survival':'G0000015':[ortf]). % 05/23/2017 - source: GS
domain_concept('feedback loop','feedback loop':'G0000016':[biof]). % 05/02/2017
domain_concept('strategy','strategy':'G0000017':[resa]). % 05/04/2017
domain_concept('active leisure','Active leisure':'G0000018':[dora]). 
domain_concept('expansion bias','expansion bias':'G0000019':[comd]). % 05/05/2017
domain_concept('repeat expansion','repeat expansion':'G0000020':[genf,comd]). % 05/05/2017
domain_concept('immunological staining','immunological staining':'G0000021':[lbpr]). % 05/05/2017 - Source GS - PMID 15473914
domain_concept('immunostaining','immunological staining':'G0000021':[lbpr]). % 05/05/2017 - Source GS - PMID 15473914
domain_concept('field population','field population':'G0000022':[invt,fngs]). % 05/08/2017
domain_concept('larva population','larva population':'G0000023':[invt]). % 05/08/2017
domain_concept('aberrant auditory processing','aberrant auditory processing':'G0000024':[fndg]). % 05/09/2017
domain_concept('preclinical evaluation','preclinical evaluation':'G0000025':[hlca]). % 06/29/2017 Source: GS PMID 12368048
domain_concept('literature search','literature search':'G0000026':[mcha]). 
domain_concept('national institute of health','National Institutes of Health':'G0000027':[orgt]). % 11/17/2017 Plural in the Lexicon
domain_concept('nih','National Institutes of Health':'G0000027':[orgt]).
domain_concept('atypical anatomy','atypical anatomy':'G0000028':[anab]). % 05/09/2017
domain_concept('anomalous anatomy','atypical anatomy':'G0000028':[anab]). % 05/09/2017
domain_concept('home care','Home care':'G0000029':[hlca]).
domain_concept('short period','short period':'G0000030':[tmco]). % 04/26/2017
domain_concept('shorter period','short period':'G0000030':[tmco]).
domain_concept('continuity of care','Continuity of care':'G0000031':[hlca]).
domain_concept('quality of care','Quality of care':'G0000032':[hlca]).
domain_concept('szondi test','Szondi test':'G0000033':[diap]).
domain_concept('social work','social work':'G0000034':[ocac,ocdi,acty]).
domain_concept('occupational health','occupational health':'G0000035':[ocac,ocdi]).
domain_concept('treatment compliance','treatment compliance':'G0000036':[hlca,inbe]).
domain_concept('treatment adherence','treatment compliance':'G0000036':[hlca,inbe]).
domain_concept('patient compliance','patient compliance':'G0000037':[hlca,inbe]).
domain_concept('patient adherence','patient compliance':'G0000037':[hlca,inbe]).
domain_concept('compliance','patient compliance':'G0000037':[hlca,inbe]).
domain_concept('vibration','vibration':'G0000038':[phpr]).
domain_concept('gadolinium dtpa','Gadolinium DTPA':'G0000039':[irda,orch]). % 05/10/2017
domain_concept('gddtpa','Gadolinium DTPA':'G0000039':[irda,orch]). % 05/10/2017
domain_concept('respite care','Respite care':'G0000040':[hlca]).
domain_concept('case management','case management':'G0000041':[hlca]).
domain_concept('outreach program','Outreach program':'G0000042':[mnob]).
domain_concept('preventive medicine','Preventive Medicine':'G0000043':[bmod]).
domain_concept('tobacco smoking','Tobacco smoking':'G0000044':[inbe,socb]).
domain_concept('oncology','Oncology':'G0000045':[bmod]).
domain_concept('community health','Community Health':'G0000046':[grpa]).
domain_concept('interpersonal compatibility','Interpersonal compatibility':'G0000047':[socb,phpr]).
domain_concept('sin','sin':'G0000048':[inbe]).
domain_concept('in vitro monitoring','in vitro monitoring':'G0000049':[lbpr]).
domain_concept('in vivo detection','in vivo detection':'G0000050':[lbpr]).
domain_concept('in vivo tracking','in vivo tracking':'G0000051':[lbpr]).
domain_concept('promoter activation','promoter activation':'G0000052':[celf]). % 05/31/2017 source: GENIA  9862666_S3 IL-2 promoter activation in Jurkat T cells stimulated with superantigen presented by Raji B cells requires CD28 activation.
domain_concept('tax expression','tax expression':'G0000053':[genf]). % 05/31/2017 source: GENIA  9710600_S3 Tax expression promotes N-terminal phosphorylation and degradation of IkappaB alpha, a principal cytoplasmic inhibitor of NF-kappaB. 
domain_concept('priming','priming':'G0000054':[celf,phsf]). % 05/31/2017 source: GENIA  9916709_S1 T cell priming enhances IL-4 gene expression by increasing nuclear factor of activated T cells. 
domain_concept('sialoadhesin expression','sialoadhesin expression':'G0000055':[genf]). % 06/01/2017 source: GENIA  8816424_S11 The regulation of sialoadhesin expression was mediated by the GC receptor, and not by mineralocorticoid receptor, as shown by inhibition experiments with specific antagonists.
domain_concept('patient resistance','Patient resistance':'G0000056':[inbe]).
domain_concept('ptir267','ptir267':'G0000057':[irda]). % 05/15/2017
domain_concept('Immuno-histochemical staining','Immuno-histochemical staining':'G0000058':[lbpr]). % 05/16/2017
domain_concept('Immunohistochemical staining','Immuno-histochemical staining':'G0000058':[lbpr]). 
domain_concept('mean survival time','mean survival time':'G0000059':[tmco]). % 05/17/2017
domain_concept('survival time','survival time':'G0000060':[tmco]). % 05/17/2017
domain_concept('heterodimerization','heterodimerization':'G0000061':[npop,moft]). % 06/22/2017 GENIA 
domain_concept('homodimerization','homodimerization':'G0000062':[npop,moft]). % 06/22/2017 GENIA 
domain_concept('denial','denial':'G0000063':[inbe]).
domain_concept('medical condition','medical condition':'G0000064':[fndg,sosy]). % 08/10/2016
domain_concept('aftercare','aftercare':'G0000065':[hlca]).
domain_concept('occipitocervical fusion','occipitocervical fusion':'G0000066':[topp]). % GR 10/02/2017
domain_concept('cervical fusion','cervical fusion':'G0000067':[topp]). % GR 10/02/2017
domain_concept('treatment refusal','treatment refusal':'G0000068':[inbe]).
domain_concept('transplant tolerance','transplant tolerance':'G0000069':[orgf]). % 05/24/2017 source: GS - PMID 12368048
domain_concept('dendritic cell population','dendritic cell population':'G0000070':[cell]). % 06/29/2017 source: GS - PMID 12368048
domain_concept('dc population','dendritic cell population':'G0000070':[cell]). % 06/29/2017 source: GS - PMID 12368048
domain_concept('physical exercise','physical exercise':'G0000071':[dora]).
domain_concept('cell population','cell population':'G0000072':[cell]). % 05/23/2017 source: GS
domain_concept('transplant model','transplant model':'G0000073':[resd]). % 05/23/2017 source: GS
domain_concept('donor-derived dendritic cells','donor-derived dendritic cells':'G0000074':[cell]). % 05/23/2017 source: GS (Plural in the Lexicon)
domain_concept('donor-derived dc','donor-derived dendritic cells':'G0000074':[cell]). % 05/23/2017 source: GS
domain_concept('allo-antigen tolerance','allo-antigen tolerance':'G0000075':[ortf,orgf]). % 05/23/2017 source: GS
domain_concept('allo-antigen specific tolerance','allo-antigen tolerance':'G0000075':[ortf,orgf]). % 05/23/2017 source: GS
domain_concept('habituation','habituation':'G0000076':[phpr,inbe,ortf]).
domain_concept('il-4 gene expression','IL-4 gene expression':'G0000077':[genf]). % 06/01/2017 source: GENIA -9916709_S1 T cell priming enhances IL-4 gene expression by increasing nuclear factor of activated T cells.
domain_concept('adhesion molecule expression','adhesion molecule expression':'G0000078':[genf]). % 06/01/2017 source: GENIA -9277478_S8 Decreased adhesion molecule expression was associated with a reduction of monocytic cell adhesion.
domain_concept('cross coupling reaction','cross coupling reaction':'G0000079':[moft]). % 06/01/2017  A nickel-catalyzed cross-coupling between (hetero)arylborons and unactivated 1-bromo-1,1-difluoroalkanes has been developed.  
domain_concept('cross coupling','cross coupling':'G0000080':[moft]). % 06/01/2017  A nickel-catalyzed cross-coupling between (hetero)arylborons and unactivated 1-bromo-1,1-difluoroalkanes has been developed.  
domain_concept('cross-coupling','cross coupling':'G0000080':[moft]).
domain_concept('ubiquination','ubiquination':'G0000081':[moft]). % 06/02/2017 GENIA 9095577_S6 This transcription factor is activated via the selective phosphorylation, ubiquination and degradation of its inhibitor protein I-kB. 
domain_concept('ubiquitination','ubiquination':'G0000081':[moft]). 
domain_concept('ubiquitylation','ubiquination':'G0000081':[moft]). 
domain_concept('physiological change','physiological change':'G0000082':[phsf,orgf]). % 05/23/2017 source: GS
domain_concept('lifestyle change','lifestyle change':'G0000083':[inbe,socb,orgf]).
domain_concept('life change','life change':'G0000084':[orgf,inbe]).
domain_concept('life transition','life change':'G0000084':[orgf,inbe]).
domain_concept('hormonal fluctuation','hormonal fluctuation':'G0000085':[phsf,orgf]). % 05/23/2017 source: GS
domain_concept('menstrual cycle phase','menstrual cycle phase':'G0000086':[orgf,biof,tmco]). % 05/23/2017 source: GS
domain_concept('ctg repeat','CTG repeat':'G0000087':[bacs,nnon]). % 05/23/2017 source: GS
domain_concept('ctg repeat instability','CTG repeat instability':'G0000088':[comd]). % 05/23/2017 source: GS
domain_concept('mutagenic stress','mutagenic stress':'G0000089':[patf,fndg]). % 05/23/2017 source: GS
domain_concept('adult day care','Adult day care':'G0000090':[hlca]).
domain_concept('partial hospitalization','Partial Hospitalization':'G0000091':[hlca]).
domain_concept('postmenopausal hormone','Postmenopausal hormone':'G0000092':[horm,phsu]). % 08/10/2016
domain_concept('mental health program','Mental Health Program':'G0000093':[hlca]).
domain_concept('paroxysmal sleep','Paroxysmal Sleep':'G0000094':[dsyn]).
domain_concept('prolonged seizure','Prolonged Seizure':'G0000095':[sosy]).
% domain_concept('dopamine transporter density','dopamine transporter density':'G0000096':[lbtr]).domain_concept('dopamine transporter density','dopamine transporter density':'G0000096':[lbtr]). % 05/24/2017 source: GS - PMID 15473914 Nice entry but it'd be overfitting
domain_concept('stereological quantification','Stereological quantification':'G0000097':[lbpr]). % 05/24/2017 source: GS - PMID 15473914
domain_concept('stereological counting technique','Stereological quantification':'G0000097':[lbpr]). % 05/24/2017 source: GS - PMID 15473914
domain_concept('stereological counting','Stereological quantification':'G0000097':[lbpr]). % 05/24/2017 source: GS - PMID 15473914
domain_concept('stereological cell counting','Stereological quantification':'G0000097':[lbpr]). % 05/24/2017 source: GS - PMID 15473914
domain_concept('immunofluorescence','immunofluorescence':'G0000098':[lbpr]). 
domain_concept('atypical planum temporale','atypical planum temporale':'G0000099':[anab,fndg]). % 05/24/2017 source: GS - PMID 15534249 Should we have this? It'd be nice to have a rule as to which premodifiers change the semantic type of a concept
domain_concept('planum temporale','planum temporale':'G0000100':[bpoc]). % 05/24/2017 source: GS - PMID 15534249
domain_concept('transgressive segregation','transgressive segregation':'G0000101':[genf]). % 05/25/2017 source: GS - PMID 15601559
domain_concept('test trial','test trial':'G0000102':[resa]). % 05/26/2017 source: GS - PMID 15765262 PA final test trial with no injection assessed final place preference.
domain_concept('il-2 production','IL-2 production':'G0000103':[genf]). % 05/26/2017 source: GENIA - 10229820_S1 Fibroblast growth factor-1 (FGF-1) enhances IL-2 production.
domain_concept('hiv production','HIV production':'G0000104':[genf]). % 05/26/2017 source: GENIA - 7888116_S7 HIV production by PMA-stimulated T-cell lines was inhibited by these retinoids.
domain_concept('encoding','encoding':'G0000105':[acty,genf]).
domain_concept('coexpression','coexpression':'G0000106':[genf]). % 05/26/2017 source: GENIA - 8605348_S1   9751756_S8 Coexpression of the interleukin-13 and interleukin-4 genes correlates with their physical linkage in the cytokine gene cluster on human chromosome 5q23-31. Coexpression of both PEBP2betaMYH11 and activated NRAS induced a more severe phenotype ...
domain_concept('co-expression','coexpression':'G0000106':[genf]). 
domain_concept('deregulation','deregulation':'G0000107':[comd]). % 06/05/2017 source: GENIA - 28371260_S1 The IgH 3' enhancer  is part of a locus control region (LCR) that is involved in deregulation of the c-myc oncogene as a result of translocation into the IgH locus.
domain_concept('neutrophil adhesion','neutrophil adhesion':'G0000108':[celf]). % 05/31/2017 source: GENIA - 9400372_S5 E-selectin plays an important role in mediating neutrophil adhesion.  Platelet activating factor (PAF) contributed to cell adhesion.
domain_concept('neutrophil adherence','neutrophil adhesion':'G0000108':[celf]). 
domain_concept('high bmi','high bmi':'G0000109':[fndg,clna]). % 08/10/2016
% domain_concept('higher current bmi','high bmi':'G0000109':[fndg,clna]).domain_concept('higher current bmi','high bmi':'G0000109':[fndg,clna]). % 09/15/2016
domain_concept('elevated bmi','high bmi':'G0000109':[fndg,clna]). % 09/13/2016
domain_concept('elevated quetelet index','high bmi':'G0000109':[fndg,clna]). % 09/13/2016
domain_concept('high current bmi','high bmi':'G0000109':[fndg,clna]). % 09/13/2016
domain_concept('promoter','promoter':'G0000110':[bacs,nnon]). % 05/31/2017 source: GENIA -9115242_S2  The AP-1 site and a distal promoter element regulate transcriptional induction of collagenase-1/.
domain_concept('promoter element','promoter':'G0000110':[bacs,nnon]). % 05/31/2017 source: GENIA -9115242_S2  The AP-1 site and a distal promoter element regulate transcriptional induction of collagenase-1/.
domain_concept('il-2 transcription','il-2 transcription':'G0000111':[genf]). % 05/31/2017 source: GENIA -10224109_S4 In Jurkat T-cells, PG490 inhibits PMA/Iono-stimulated IL-2 transcription.
domain_concept('transcription from the il-2 promoter','il-2 transcription':'G0000111':[genf]). % 05/31/2017 source: GENIA -8186461_S4 Overexpression of calcineurin (CN), a Ca2+/calmodulin-dependent protein phosphatase, can stimulate transcription from the IL-2 promoter through the NF-AT-binding site.
domain_concept('gene construct','gene construct':'G0000112':[medd,resd]). % 05/31/2017 source: GENIA -9108409_S7 Human IL-5 promoter/enhancer-luciferase gene construct transfected to T-cell clones was transcribed on either TCR or IL-2R stimulation and was clearly downregulated by dexamethasone. A gene construct containing VEGF was cloned in the pFastBac-HTA vector, followed by transformation in DH10BAC.
domain_concept('hiv-1 expression','HIV-1 expression':'G0000113':[genf]). % 05/31/2017 source: GENIA -10454636_S4 We have investigated the regulation of HIV-1 expression by dopamine in Jurkat T cells and in primary blood mononuclear cells (PBMC).
domain_concept('hiv-1 gene expression','HIV-1 expression':'G0000113':[genf]). % 06/01/2017 source: GENIA -8062448_S1 Superantigens activate HIV-1 gene expression in monocytic cells.
domain_concept('transendothelial migration','transendothelial migration':'G0000114':[celf,ortf]). % 05/31/2017 source: GENIA -10082134_S10 The transendothelial migration of monocytes was inhibited by an antibody to PECAM-1.
domain_concept('cancer survival','cancer survival':'G0000115':[orgf]). % 08/10/2016
domain_concept('transcriptional induction','transcriptional induction':'G0000116':[genf]). % 05/31/2017 source: GENIA -1315834_S9 Although IFN-gamma alone does not induce ISG expression, IFN-gamma pretreatment markedly increases and hastens ISG expression and transcriptional induction.
domain_concept('longitudinal tracking','longitudinal tracking':'G0000117':[resa]). % 08/10/2016
domain_concept('adolescent year','adolescent year':'G0000118':[tmco]). % 08/10/2016
domain_concept('work activity','work activity':'G0000119':[acty,ocac]). % 09/13/2016
domain_concept('activity at work','work activity':'G0000119':[acty,ocac]). % 09/13/2016
domain_concept('pre- or postmenopausal breast cancer','pre- or postmenopausal breast cancer':'G0000120':[neop]). % 08/12/2016
domain_concept('pre- and postmenopausal breast cancer','pre- and postmenopausal breast cancer':'G0000121':[neop]). % 08/22/2016
domain_concept('good nutrition','good nutrition':'G0000122':[food]). % 08/16/16
domain_concept('nutrition','nutrition':'G0000123':[food,topp]). % 08/16/16
domain_concept('telephone dialing','telephone dialing':'G0000124':[acty]). % 08/16/16
domain_concept('enrollee','enrollee':'G0000125':[popg,humn]). % 08/17/16
domain_concept('diet high in fat','diet high in fat':'G0000126':[food]). % 08/18/2016
domain_concept('epidemiologic work','epidemiologic work':'G0000127':[resa,ocac]). % 08/18/2016
domain_concept('hiv replication','hiv replication':'G0000128':[genf]). % 05/31/2017 source: GENIA -8543841_S4  Although IL-10 alone did not induce HIV-1 replication, in the presence of TNF-alpha, IL-10 markedly enhanced virion production from a chronically infected promonocytic cell line (U1) and in acutely infected monocyte-derived macrophages.
domain_concept('viral replication','viral replication':'G0000129':[genf]). % 05/31/2017 source: GENIA -7594489_S1 Triggering of complement receptors CR1 (CD35) and CR3 (CD11b/CD18) induces nuclear translocation of NF-kappa B (p50/p65) in human monocytes and enhances viral replication in HIV-infected monocytic cells.
domain_concept('resynthesis','resynthesis':'G0000130':[genf]). % 06/01/2017 source: GENIA -10102628_S7 The nuclear pool of I(kappa)B(alpha) was partially independent of the resynthesis of the protein.
domain_concept('reduce alcohol consumption','reduce alcohol consumption':'G0000131':[inbe,hlca]). % 08/19/2016
domain_concept('investigation','investigation':'G0000132':[resa]). % 08/22/2016
domain_concept('protein-dna interaction','protein-DNA interaction':'G0000133':[moft]). % 06/01/2017 source: GENIA -1782151_S3 To further characterize the protein-DNA interactions mediating IFN-beta induction.
domain_concept('high waist-hip ratio','high waist-hip ratio':'G0000134':[orga,clna]). % 08/22/2016
domain_concept('high whr','high waist-hip ratio':'G0000134':[orga,clna]). % 08/22/2016
domain_concept('upper-body fat accumulation','high waist-hip ratio':'G0000134':[orga,clna]). % 08/22/2016
domain_concept('upper-body type of fat accumulation','high waist-hip ratio':'G0000134':[orga,clna]). % 08/22/2016
domain_concept('upper body fat accumulation','high waist-hip ratio':'G0000134':[orga,clna]). % 11/09/2016
domain_concept('upper body type of fat accumulation','high waist-hip ratio':'G0000134':[orga,clna]). % 11/09/2016
domain_concept('low waist-height ratio','low waist-hip ratio':'G0000135':[orga,clna]). % 08/22/2016
domain_concept('low whr','low waist-hip ratio':'G0000135':[orga,clna]). % 08/22/2016
domain_concept('lower-body fat accumulation','low waist-hip ratio':'G0000135':[orga,clna]). % 08/22/2016
domain_concept('lower-body type of fat accumulation','low waist-hip ratio':'G0000135':[orga,clna]). % 08/22/2016
domain_concept('lower body fat accumulation','low waist-hip ratio':'G0000135':[orga,clna]). % 11/09/2016
domain_concept('lower body type of fat accumulation','low waist-hip ratio':'G0000135':[orga,clna]). % 11/09/2016
domain_concept('fat','fat':'G0000136':[lipd]). % 09/12/2016
domain_concept('stress hormone','stress hormone':'G0000137':[horm]). % 06/02/2017 source: GENIA 28404850_S3 To further characterize the protein-DNA interactions mediating IFN-beta induction.
domain_concept('nucleus','nucleus':'G0000138':[celc]). % 06/02/2017 *not in the UMLS* source: GENIA 7878466_S4  This liberates NF-kappa B to translocate to the nucleus and initiate transcription of genes important for the defense of the organism.
domain_concept('nuclei','nucleus':'G0000138':[celc]). % 06/02/2017 *not in the UMLS* source: GENIA 8164666_S9  Nuclei from GATA-1- mutant embryonic stem cells can still be reprogrammed to express their globin genes in erythroid heterokaryons. We do need this entry
domain_concept('premenopausal breast cancer','premenopausal breast cancer':'G0000139':[neop]). % 08/22/2016 - 09/12/2016
domain_concept('postmenopausal breast cancer','postmenopausal breast cancer':'G0000140':[neop]). % 08/22/2016 - 09/12/2016
domain_concept('all-cause mortality','all-cause mortality':'G0000141':[orgf]). % 09/07/2016
domain_concept('cotransfection','cotransfection':'G0000142':[genf,mbrt]). % 06/05/2017 source: GENIA 8657101_S8 ICSAT was demonstrated to possess repressive function over the gene activation induced by IFN stimulation or by IRF-1 cotransfection.
domain_concept('myotonic dystrophy type 1','myotonic dystrophy type 1':'G0000143':[dsyn]). % 06/26/2017 source: GS
domain_concept('myotonic dystrophy 1','myotonic dystrophy type 1':'G0000143':[dsyn]). % 06/26/2017 source: GS
domain_concept('transduction','transduction':'G0000144':[celf,genf]). % 06/05/2017 source: GENIA 7823943_S2 The intracellular dioxin receptor mediates signal transduction by dioxin and functions as a ligand-activated transcription factor.
domain_concept('overexpression','overexpression':'G0000145':[genf]). % 06/05/2017 source: GENIA 9052839_S6 Overexpression of TRAMP leads to two major responses, NF-kappaB activation and apoptosis.
domain_concept('over-expression','overexpression':'G0000145':[genf]). % 06/05/2017 source: GENIA 9218843_S9 In addition, over-expression of STAT2 by transfection of the cDNA prevented apoptosis of the T cell clones. 
domain_concept('history of smoking','history of smoking':'G0000146':[inbe]). % 09/09/2016
domain_concept('smoking history','history of smoking':'G0000146':[inbe]). % 09/09/2016
domain_concept('quitter','quitter':'G0000147':[popg,humn]). % 09/09/2016
domain_concept('quit history','quit history':'G0000148':[inbe]). % 09/09/2016
domain_concept('supplement','supplement':'G0000149':[vita]). % 09/09/2016
domain_concept('homodimer','homodimer':'G0000150':[chvs]). % 06/13/2017 GR GENIA Stat proteins form homo- or heterodimers, translocate to the nucleus, and induce transcription from responsive genes.
domain_concept('heterodimer','heterodimer':'G0000151':[chvs]). % 06/13/2017 GR GENIA Stat proteins form homo- or heterodimers, translocate to the nucleus, and induce transcription from responsive genes.
domain_concept('sccip','sccip':'G0000152':[topp]). 
domain_concept('Surviving cancer competently intervention program','sccip':'G0000152':[topp]). 
domain_concept('maintenance of cessation','maintenance of cessation':'G0000153':[inbe,hlca,topp]). % 09/09/2016
domain_concept('quit attempt','quit attempt':'G0000154':[inbe,hlca]). % 09/09/2016
domain_concept('behavioral pattern','behavioral pattern':'G0000155':[inbe,socb]). % 09/09/2016
domain_concept('virion production','virion production':'G0000156':[celf]). % 06/21/2017 source: GENIA  7594489_S1 Although IL-10 alone did not induce HIV-1 replication, in the presence of TNF-alpha, IL-10 markedly enhanced virion production from a chronically infected promonocytic cell line (U1) and in acutely infected monocyte-derived macrophages. 
domain_concept('production of virion','virion production':'G0000156':[celf]). % 06/21/2017 source: GENIA
domain_concept('myotonic dystrophy type 2','myotonic dystrophy type 2':'G0000157':[dsyn]). % 06/26/2017 source: GS
domain_concept('myotonic dystrophy 2','myotonic dystrophy type 2':'G0000157':[dsyn]). % 06/26/2017 source: GS
domain_concept('gene product','gene product':'G0000158':[bacs,nnon,aapp]). % 06/23/2017 source: GENIA
domain_concept('expression','expression':'G0000159':[genf]). % 06/26/2017 source: GENIA
domain_concept('public health','public health':'G0000160':[ocdi]). % 09/09/2016
domain_concept('long-term maintenance','long-term maintenance':'G0000161':[inbe,acty]). % 09/09/2016
domain_concept('exposure to ovarian hormone','exposure to ovarian hormone':'G0000162':[inpo]). % 09/12/2016 Plural in the Lexicon
domain_concept('skinfold measurement','skinfold measurement':'G0000163':[lbtr]). % 09/12/2016
domain_concept('unchanged weight','unchanged weight':'G0000164':[orga,clna]). % 09/12/2016
domain_concept('exercise habit','exercise habit':'G0000165':[inbe,dora]). % 09/12/2016
domain_concept('female','female':'G0000166':[popg,humn]). % 09/12/2016
domain_concept('male','male':'G0000167':[popg,humn]). % 10/05/2017
domain_concept('dietary intake of calorie','dietary intake of calorie':'G0000168':[orgf]). % 09/12/2016
% domain_concept('association','association':'G0000169':[phpr]).domain_concept('association','association':'G0000169':[phpr]). % 09/13/2016 - but FP's
domain_concept('nuclear factor of activated t cell','nuclear factor of activated T cell':'G0000170':[aapp]). % GR 06/28/2017, GENIA: The hypothesis that the nuclear factor of activated T cells (NF-AT) may influence HIV-1 replication is therefore compelling 
domain_concept('t cell nuclear factor','nuclear factor of activated T cell':'G0000170':[aapp]). % GR 06/28/2017, GENIA: The hypothesis that the nuclear factor of activated T cells (NF-AT) may influence HIV-1 replication is therefore compelling 
domain_concept('nf-at','nuclear factor of activated T cell':'G0000170':[aapp]). % GR 06/28/2017, GENIA: The hypothesis that the nuclear factor of activated T cells (NF-AT) may influence HIV-1 replication is therefore compelling 
domain_concept('nfat','nuclear factor of activated T cell':'G0000170':[aapp]). % GR 06/28/2017, GENIA: The hypothesis that the nuclear factor of activated T cells (NF-AT) may influence HIV-1 replication is therefore compelling 
domain_concept('interrelation','interrelation':'G0000171':[phpr]). % 09/13/2016
domain_concept('food intake','food intake':'G0000172':[orgf]). % 09/13/2016
domain_concept('intake of food','food intake':'G0000172':[orgf]). % 09/13/2016
domain_concept('premenopausal women','premenopausal women':'G0000173':[popg,humn]). % 09/13/2016
domain_concept('postmenopausal','postmenopausal':'G0000174':[tmco]). % 09/13/2016
domain_concept('hormonal concentration','hormonal concentration':'G0000175':[orgf]). % 09/13/2016
domain_concept('hormone concentration','hormonal concentration':'G0000175':[orgf]). % 09/13/2016
domain_concept('random-effect model','random-effect model':'G0000176':[resd,resa]). % 09/14/2016
domain_concept('random effect model','random-effect model':'G0000176':[resd,resa]). % 09/14/2016
domain_concept('future research direction','future research direction':'G0000177':[resa]). % 09/14/2016
domain_concept('p for trend','p for trend':'G0000178':[qnco]). % 09/14/2016
domain_concept('study population','study population':'G0000179':[popg]). % 09/14/2016
domain_concept('length of survival','length of survival':'G0000180':[orgf,tmco]). % 09/14/2016  orgf added on 11/10/2016 
domain_concept('abdominal adiposity','abdominal adiposity':'G0000181':[orga,clna]). % 09/15/2016
domain_concept('abdominal distribution of fat','abdominal adiposity':'G0000181':[orga,clna]). % 09/15/2016
domain_concept('abdominal distribution of obesity','abdominal adiposity':'G0000181':[orga,clna]). % 09/15/2016
domain_concept('abdominal fat accumulation','abdominal adiposity':'G0000181':[orga,clna]). % GR 10/10/2017
domain_concept('nonuser','nonuser':'G0000182':[popg,humn]). % GR 10/02/2017
domain_concept('non-user','nonuser':'G0000182':[popg,humn]). % GR 10/02/2017
domain_concept('nuclear factor','nuclear factor':'G0000183':[aapp]). % GR 06/28/2017, GENIA
domain_concept('more educated woman','more educated woman':'G0000184':[popg]). % 09/15/2016
domain_concept('excessive calorie intake','excessive calorie intake':'G0000185':[orgf,inbe,fndg]). % 09/15/2016 on 11/10 I added fndg to this set
domain_concept('excessive calorie consumption','excessive calorie intake':'G0000185':[orgf,inbe,fndg]). % 09/15/2016 on 11/10 I added fndg to this set
domain_concept('excess calorie','excess calorie':'G0000186':[fndg]). % 11/07/2016 moved this up
domain_concept('excess in calorie','excess calorie':'G0000186':[fndg]). % 09/15/2016
domain_concept('excessive calorie','excess calorie':'G0000186':[fndg]). % 09/15/2016
domain_concept('weight maintenance','weight maintenance':'G0000187':[inbe,clna]). % 09/15/2016
domain_concept('avoiding adult weight gain','weight maintenance':'G0000187':[inbe,clna]). % 09/15/2016
domain_concept('avoid weight gain','weight maintenance':'G0000187':[inbe,clna]). % 09/15/2016
domain_concept('avoid being overweight','weight maintenance':'G0000187':[inbe,clna]). % 09/15/2016
domain_concept('nondiabetic','nondiabetic':'G0000188':[popg]). % 10/02/2017
domain_concept('best','best':'G0000189':[qlco]). % 09/15/2016
domain_concept('angiotensin receptor blocker','angiotensin receptor blocker':'G0000190':[aapp]). % 10/02/2017
domain_concept('macroalbuminuria','macroalbuminuria':'G0000191':[fndg]). % 10/02/2017
domain_concept('microalbuminuria','microalbuminuria':'G0000192':[fndg]). % 10/02/2017
domain_concept('dietary fat consumption','dietary fat consumption':'G0000193':[orgf,inbe]). % 09/15/2016
domain_concept('age at menarche','age at menarche':'G0000194':[orga,clna]). % 09/16/2016
domain_concept('smoking control activity','smoking control activity':'G0000195':[hlca]). % 09/16/2016
domain_concept('laboratory investigation','laboratory investigation':'G0000196':[lbpr,resa]). % GR 10/03/2017
domain_concept('nutrition information','nutrition information':'G0000197':[edac]). % 09/16/2016
domain_concept('endocrine abnormality','endocrine abnormality':'G0000198':[acab,fndg]). % 10/03/2017
domain_concept('community board','community board':'G0000199':[orgt]). % 09/16/2016
domain_concept('successful smoking cessation','successful smoking cessation':'G0000200':[inbe,hlca]). % 09/16/2016
domain_concept('successful cessation','successful smoking cessation':'G0000200':[inbe,hlca]). % 09/16/2016
domain_concept('follow-up period','follow-up period':'G0000201':[tmco]). % 09/16/2016
domain_concept('slip','slip':'G0000202':[inbe]). % 09/16/2016
domain_concept('younger age','younger age':'G0000203':[orga,clna]). % 09/16/2016
domain_concept('health care provider','health care provider':'G0000204':[prog]). % 09/16/2016
domain_concept('health care worker','health care provider':'G0000204':[prog]). % 09/16/2016
domain_concept('health provider','health care provider':'G0000204':[prog]). % 09/16/2016
domain_concept('health worker','health care provider':'G0000204':[prog]). % 09/16/2016
domain_concept('customer','customer':'G0000205':[popg]). % GR 10/10/2017
domain_concept('tumor response','tumor response':'G0000206':[orgf]). % 10/26/2016
domain_concept('vascular event','vascular event':'G0000207':[dsyn]). % GR 10/11/2017
domain_concept('low salt intake','low salt intake':'G0000208':[hlca]). % GR 10/11/2017
domain_concept('fasting glucose level','fasting glucose level':'G0000209':[lbpr,lbtr]). % GR 10/17/2017
domain_concept('fasting glucose (fg) level','fasting glucose level':'G0000209':[lbpr,lbtr]). % GR 10/17/2017
domain_concept('fg level','fasting glucose level':'G0000209':[lbpr,lbtr]). % GR 10/17/2017
domain_concept('fasting glucose','fasting glucose level':'G0000209':[lbpr,lbtr]). % GR 10/17/2017
domain_concept('fasting insulin level','fasting insulin level':'G0000210':[lbpr,lbtr]). % GR 10/18/2017
domain_concept('fasting insulin','fasting insulin level':'G0000210':[lbpr,lbtr]). % GR 10/18/2017
domain_concept('cancer-free survival','cancer-free survival':'G0000211':[orgf]). % 10/26/2016
domain_concept('male partner','male partner':'G0000212':[humn,famg]). % 10/26/2016
domain_concept('female partner','female partner':'G0000213':[humn,famg]). % 10/26/2016
domain_concept('pilot result','pilot result':'G0000214':[lbtr]). % 10/26/2016
domain_concept('pilot program','pilot program':'G0000215':[mnob]). % 10/26/2016
domain_concept('statistical significance','statistical significance':'G0000216':[lbtr]). % 10/26/2016
domain_concept('acute pathology','acute pathology':'G0000217':[patf,fndg]). % GR 10/17/2017
domain_concept('chronic condition','chronic condition':'G0000218':[fndg]). % GR 10/17/2017
domain_concept('femoral neck','femoral neck':'G0000219':[bpoc]). % 10/26/2016
domain_concept('mitochondrial function','mitochondrial function':'G0000220':[celf,moft]). % GR 10/17/2017
domain_concept('treatment group','treatment group':'G0000221':[grup]). % GR 10/17/2017
domain_concept('noncarrier','noncarrier':'G0000222':[popg]). % GR 10/18/2017
domain_concept('overall survival','overall survival':'G0000223':[orgf]). % 10/26/2016
domain_concept('date of diagnosis','date of diagnosis':'G0000224':[tmco]). % 11/07/2016
domain_concept('morbidity','morbidity':'G0000225':[phpr]). % 10/06/2016
domain_concept('outcome measurement','outcome measurement':'G0000226':[lbtr]). % 10/27/2016
domain_concept('implementation','implementation':'G0000227':[acty]). % 10/27/2016
domain_concept('dietary behavior','dietary behavior':'G0000228':[inbe,socb]). % 10/27/2016 and socb added on 11/10/2016
domain_concept('neuronal deterioration','neuronal deterioration':'G0000229':[comd]). % GR 10/18/2017
domain_concept('neuronal functional deterioration','neuronal deterioration':'G0000229':[comd]). % GR 10/18/2017
domain_concept('at time of diagnosis','at time of diagnosis':'G0000230':[tmco]). % 10/31/2016
domain_concept('at diagnosis','at time of diagnosis':'G0000230':[tmco]). % 10/31/2016
domain_concept('at the time of diagnosis','at time of diagnosis':'G0000230':[tmco]). 
domain_concept('common genetic variant','common genetic variant':'G0000231':[nusq, genf]). % GR 10/18/2017
domain_concept('common genetic variation','common genetic variant':'G0000231':[nusq, genf]). % GR 10/18/2017
domain_concept('rare genetic variant','rare genetic variant':'G0000232':[genf,comd]). % GR 10/18/2017
domain_concept('rare genetic variation','rare genetic variant':'G0000232':[genf,comd]). % GR 10/18/2017
domain_concept('functional genetic variant','functional genetic variant':'G0000233':[nusq, genf]). % GR 10/18/2017
domain_concept('functional genetic variation','functional genetic variant':'G0000233':[nusq, genf]). % GR 10/18/2017
domain_concept('physical activity at work','physical activity at work':'G0000234':[dora,acty]). % 10/31/2016
domain_concept('intention to quit','intention to quit':'G0000235':[inbe]). % 10/31/2016
domain_concept('parasympathetic blockade','parasympathetic blockade':'G0000236':[topp]). % GR 10/19/2017
domain_concept('parasympathetic nerve blockade','parasympathetic blockade':'G0000236':[topp]). % GR 10/19/2017
domain_concept('high in complex carbohydrate','high in complex carbohydrate':'G0000237':[qlco]). % 11/03/2016
domain_concept('risk score','risk score':'G0000238':[qnco]). % GR 10/19/2017
domain_concept('et-1','ET-1':'G0000239':[aapp,bacs]). % GR 10/26/2017
domain_concept('surgical guidance','surgical guidance':'G0000240':[hlca]). % 10/26/2016
domain_concept('breast density','breast density':'G0000241':[orga,clna]). % 10/26/2016
domain_concept('physical activity history','physical activity history':'G0000242':[inbe]). % 11/03/2016
domain_concept('nitrosamine formation','nitrosamine formation':'G0000243':[orgf]). % 11/03/2016
domain_concept('further investigation','further investigation':'G0000244':[resa]). % 11/03/2016
domain_concept('far investigation','further investigation':'G0000244':[resa]). % 11/21/2017 Due to results of R run
domain_concept('future research','future research':'G0000245':[resa]). % 11/03/2016
domain_concept('final section','final section':'G0000246':[mnob]). % 11/03/2016
domain_concept('pd-l1','PD-L1':'G0000247':[imft,phsu]). % GR 10/26/2017
domain_concept('abdominal girth','abdominal girth':'G0000248':[clna,orga]). % 11/04/2016
domain_concept('waist girth','abdominal girth':'G0000248':[clna,orga]). % 11/04/2016
% domain_concept('waist','abdominal girth':'G0000248':[clna,orga]).domain_concept('waist','abdominal girth':'G0000248':[clna,orga]). % 11/04/2016
domain_concept('sex','sex':'G0000249':[orga,clna]). % 11/04/2016
domain_concept('initiation and promotion of cancer','initiation and promotion of cancer':'G0000250':[neop]). % 11/04/2016
domain_concept('promotion of cancer','promotion of cancer':'G0000251':[neop]). % 11/04/2016
domain_concept('meat consumption','meat consumption':'G0000252':[orgf,inbe]). % 11/04/2016
domain_concept('intake of red meat','meat consumption':'G0000252':[orgf,inbe]). % 11/04/2016
domain_concept('salt use','salt use':'G0000253':[inbe]). % 11/04/2016
domain_concept('vegetable fat','vegetable fat':'G0000254':[lipd]). % 11/04/2016
domain_concept('general category of intervention','general category of intervention':'G0000255':[hlca]). % 11/07/2016
domain_concept('psychoeducational care','psychoeducational care':'G0000256':[hlca,edac]). % 11/07/2016
domain_concept('valvuloplasty technique','valvuloplasty technique':'G0000257':[topp]). % 12/13/2017
domain_concept('valvuloplasty','valvuloplasty':'G0000258':[topp]). % 12/13/2017
domain_concept('glass bead test','glass bead test':'G0000259':[lbpr]). % 12/13/2017
domain_concept('abnormality','abnormality':'G0000260':[fndg,acab]). % 12/14/2017
domain_concept('pacemaker','pacemaker':'G0000261':[medd]). % 12/14/2017
domain_concept('pacer','pacemaker':'G0000261':[medd]). % 12/14/2017
domain_concept('vigor','vigor':'G0000262':[clna,orga]). % 11/07/2016
domain_concept('hormonal receptor status','hormonal receptor status':'G0000263':[clna]). % 11/07/2016
domain_concept('reference period','Reference period':'G0000264':[tmco]). % 11/07/2016
domain_concept('two-year time period','two-year time period':'G0000265':[tmco]). % 11/07/2016
domain_concept('two-year period','two-year time period':'G0000265':[tmco]). % 11/07/2016
domain_concept('arm volumen increase','arm volume increase':'G0000266':[fndg,lbtr]). % 10/26/2016
domain_concept('nutrition education','nutrition education':'G0000267':[edac]). % 11/08/2016
domain_concept('molecular subgroup','molecular subgroup':'G0000268':[clna]). % 11/27/2017
domain_concept('family functioning','family functioning':'G0000269':[socb]). % 11/08/2016
domain_concept('mortality','mortality':'G0000270':[orgf]). % 11/08/2016
domain_concept('pharmacy resident','pharmacy resident':'G0000271':[prog]). % 11/27/2017
domain_concept('vastus lateralis','vastus lateralis':'G0000272':[tisu]). % 11/28/2017
domain_concept('morphology','morphology':'G0000273':[ocdi,orga]). % 11/09/2016
domain_concept('donor-derived','donor-derived':'G0000274':[qlco]). % 05/23/2017 Source: GS - to avoid mapping donor to a human
domain_concept('play','Play':'G0000275':[dora,mnob]). % 11/09/2016
domain_concept('before and after menopause','before and after menopause':'G0000276':[tmco]). % 11/09/2016
domain_concept('sex hormone level','sex hormone level':'G0000277':[clna,orga,lbtr]). % 11/09/2016
domain_concept('at baseline','at baseline':'G0000278':[tmco]). % 11/09/2016
domain_concept('usage trend','usage trend':'G0000279':[phpr,socb]). % 11/09/2016
domain_concept('black and white adult','black and white adult':'G0000280':[popg,humn]). % 11/10/2016
domain_concept('white and black adult','white and black adult':'G0000281':[popg,humn]). % 11/10/2016
domain_concept('cardiovascular risk','cardiovascular risk':'G0000282':[fndg]). % 10/02/2017
domain_concept('cardiovascular disease risk','cardiovascular risk':'G0000282':[fndg]). % 10/02/2017
domain_concept('health risk','health risk':'G0000283':[fndg]). % 10/02/2017
domain_concept('veteran health administration','Veteran Health Administration':'G0000284':[orgt]). % 11/14/2016
domain_concept('vha','Veteran Health Administration':'G0000284':[orgt]). % 11/14/2016
domain_concept('positive mental imagery','positive mental imagery':'G0000285':[topp]). % 11/14/2016
domain_concept('coping strategy','coping strategy':'G0000286':[hlca]). % 11/14/2016
domain_concept('coping skill','coping strategy':'G0000286':[hlca]). % 11/14/2016
domain_concept('method of coping','coping strategy':'G0000286':[hlca]). % 11/14/2016
domain_concept('coping method','coping strategy':'G0000286':[hlca]). % 11/14/2016
domain_concept('cognitive coping','coping strategy':'G0000286':[hlca]). % 11/14/2016
domain_concept('behavioral coping','coping strategy':'G0000286':[hlca]). % 11/14/2016
domain_concept('active-cognitive coping','coping strategy':'G0000286':[hlca]). % 11/14/2016
domain_concept('active-behavioral coping','coping strategy':'G0000286':[hlca]). % 11/14/2016
domain_concept('wrist girth','Wrist girth':'G0000287':[clna,orga]). % 11/14/2016
domain_concept('midarm girth','Midarm girth':'G0000288':[clna,orga]). % 11/14/2016
domain_concept('mid-arm girth','Midarm girth':'G0000288':[clna,orga]). % 11/14/2016
domain_concept('midarm','Midarm girth':'G0000288':[clna,orga]). % 11/04/2016
domain_concept('neurology','Neurology':'G0000289':[bmod]). % 11/03/2016
domain_concept('continuum of care','Continuum of Care':'G0000290':[hlca]). % 11/04/2016
domain_concept('texting','Texting':'G0000291':[acty,inbe]). % 11/04/2016
domain_concept('family medicine','Family Medicine':'G0000292':[bmod]). % 11/04/2016
domain_concept('drug education','Drug Education':'G0000293':[edac]). % 11/04/2016
domain_concept('social norm','Social Norm':'G0000294':[socb]). % 11/04/2016
domain_concept('acceptance and commitment therapy','Acceptance and Commitment Therapy':'G0000295':[topp]). % 11/04/2016
domain_concept('undifferentiated schizophrenia','Undifferentiated Schizophrenia':'G0000296':[mobd]). % 11/04/2016
domain_concept('distraction','distraction':'G0000297':[evnt,socb,acty]). % 11/04/2016
domain_concept('need assessment','Need Assessment':'G0000298':[resa]). % 11/04/2016
domain_concept('occupational physical activity','Occupational physical activity':'G0000299':[dora,acty]). % 11/04/2016
domain_concept('nursing','Nursing':'G0000300':[ocac]). % 11/04/2016
domain_concept('healthy lifestyle','healthy lifestyle':'G0000301':[inbe,socb]). % 09/07/2016
domain_concept('healthy life style','healthy lifestyle':'G0000301':[inbe,socb]). % 09/07/2016
domain_concept('lean tissue','lean tissue':'G0000302':[tisu]). % 09/07/2016
domain_concept('lowest quartile','lowest quartile':'G0000303':[popg]). % 09/07/2016 changed from stat
domain_concept('highest quartile','highest quartile':'G0000304':[popg]). % 09/07/2016
domain_concept('quartile','quartile':'G0000305':[popg]). % 09/07/2016 Removed stat on 09/15/2016
domain_concept('ontogeny','Ontogeny':'G0000306':[ocdi]).
domain_concept('biomedicine','Biomedicine':'G0000307':[bmod]).
domain_concept('psyche','Psyche':'G0000308':[bpoc]).
domain_concept('baseball','Baseball':'G0000309':[dora]).
domain_concept('crowdsourcing','crowdsourcing':'G0000310':[evnt,socb,acty]). % GR 10/02/2017
domain_concept('genesis','genesis':'G0000311':[phpr,patf]). % GR 10/03/2017
domain_concept('fractionated atrial electrogram','fractionated atrial electrogram':'G0000312':[lbtr]). % GR 11/30/2017
domain_concept('fractionated electrogram','fractionated atrial electrogram':'G0000312':[lbtr]). % GR 11/30/2017
domain_concept('frequency-altered feedback','frequency-altered feedback':'G0000313':[topp]). % GR 12/06/2017
domain_concept('gram-negative organism','gram-negative organism':'G0000314':[orgm]). % GR 12/06/2017
domain_concept('gram-positive organism','gram-positive organism':'G0000315':[orgm]). % GR 12/06/2017
domain_concept('biomineralization','biomineralization':'G0000316':[orgf,biof]). % GR 12/06/2017
domain_concept('spinal cord compression','spinal cord compression':'G0000317':[fndg]). % GR Jan 2018
domain_concept('cord compression','spinal cord compression':'G0000317':[fndg]). % GR Jan 2018
domain_concept('vertical translocation','vertical translocation':'G0000318':[fndg]). % GR Jan 2018
domain_concept('pretreatment','pretreatment':'G0000319':[topp]). % GR Jan 2018
