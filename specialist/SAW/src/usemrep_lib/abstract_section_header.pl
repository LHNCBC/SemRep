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

% File:		abstract_section_header.pl
% Module:	abstract_section_header
% Author:	DW
% Purpose:	Section headers in abstracts for SemRep processing


:- module( abstract_section_header, [
		valid_abstract_section_header/2
]).
% the second argument is the frequency of occurrence across all abstracts in 
% /nfsvol/nls/MEDLINE_Baseline_Repository/2005/*.xml

valid_abstract_section_header('RESULTS',		2950589).
valid_abstract_section_header('METHODS',		1986937).
valid_abstract_section_header('CONCLUSIONS',		1771420).
valid_abstract_section_header('CONCLUSION',		1358910).
valid_abstract_section_header('BACKGROUND',		1275662).
valid_abstract_section_header('OBJECTIVE',		945817).
valid_abstract_section_header('PURPOSE',		396068).
valid_abstract_section_header('OBJECTIVES',		341368).
valid_abstract_section_header('DESIGN',		238348).
valid_abstract_section_header('MATERIALS AND METHODS',		229405).
valid_abstract_section_header('INTRODUCTION',		184895).
valid_abstract_section_header('METHOD',		169412).
valid_abstract_section_header('AIM',		160903).
valid_abstract_section_header('SETTING',		146530).
valid_abstract_section_header('PATIENTS AND METHODS',		99475).
valid_abstract_section_header('STUDY DESIGN',		98846).
valid_abstract_section_header('AIMS',		90052).
valid_abstract_section_header('METHODS AND RESULTS',		77313).
valid_abstract_section_header('MATERIAL AND METHODS',		77073).
valid_abstract_section_header('DISCUSSION',		74631).
valid_abstract_section_header('PARTICIPANTS',		62515).
valid_abstract_section_header('MAIN OUTCOME MEASURES',		61348).
valid_abstract_section_header('PATIENTS',		47239).
valid_abstract_section_header('FINDINGS',		37489).
valid_abstract_section_header('INTERVENTIONS',		37265).
valid_abstract_section_header('SUMMARY',		36941).
valid_abstract_section_header('BACKGROUND AND PURPOSE',		33155).
valid_abstract_section_header('CONTEXT',		31434).
valid_abstract_section_header('SUBJECTS',		30145).
valid_abstract_section_header('RESULT',		26421).
valid_abstract_section_header('RECENT FINDINGS',		25909).
valid_abstract_section_header('PURPOSE OF REVIEW',		25796).
valid_abstract_section_header('INTERVENTION',		24012).
valid_abstract_section_header('BACKGROUND/AIMS',		22098).
valid_abstract_section_header('MEASUREMENTS',		21993).
valid_abstract_section_header('METHODOLOGY',		20115).
valid_abstract_section_header('INTERPRETATION',		20088).
valid_abstract_section_header('MEASUREMENTS AND MAIN RESULTS',		19498).
valid_abstract_section_header('MAIN RESULTS',		19364).
valid_abstract_section_header('LEVEL OF EVIDENCE',		18811).
valid_abstract_section_header('BACKGROUND AND OBJECTIVES',		18440).
valid_abstract_section_header('CASE REPORT',		18339).
valid_abstract_section_header('TRIAL REGISTRATION',		18066).
valid_abstract_section_header('SUBJECTS AND METHODS',		17921).
valid_abstract_section_header('DATA SOURCES',		17799).
valid_abstract_section_header('LIMITATIONS',		16970).
valid_abstract_section_header('METHODS AND MATERIALS',		16256).
valid_abstract_section_header('BACKGROUND AND OBJECTIVE',		14946).
valid_abstract_section_header('BACKGROUND AND AIMS',		14885).
valid_abstract_section_header('SIGNIFICANCE',		14203).
valid_abstract_section_header('RATIONALE',		14139).
valid_abstract_section_header('METHODOLOGY/PRINCIPAL FINDINGS',		13274).
valid_abstract_section_header('DATA COLLECTION AND ANALYSIS',		13211).
valid_abstract_section_header('SELECTION CRITERIA',		12957).
valid_abstract_section_header('SUMMARY OF BACKGROUND DATA',		12766).
valid_abstract_section_header('RESEARCH DESIGN AND METHODS',		12287).
valid_abstract_section_header('DESIGN AND METHODS',		12062).
valid_abstract_section_header('BACKGROUND & AIMS',		12059).
valid_abstract_section_header('EXPERIMENTAL DESIGN',		11830).
valid_abstract_section_header('MAIN OUTCOME MEASURE',		11526).
valid_abstract_section_header('STUDY OBJECTIVE',		11421).
valid_abstract_section_header('CASE PRESENTATION',		10884).
valid_abstract_section_header('CONCLUSIONS/SIGNIFICANCE',		10884).
valid_abstract_section_header('ANIMALS',		10731).
valid_abstract_section_header('CLINICAL RELEVANCE',		10597).
valid_abstract_section_header('MAIN OUTCOME MEASURE(S)',		10377).
valid_abstract_section_header('IMPORTANCE',		10266).
valid_abstract_section_header('IMPLICATIONS',		9924).
valid_abstract_section_header('CONCLUSION(S)',		9771).
valid_abstract_section_header('OBJECT',		9630).
valid_abstract_section_header('INTERVENTION(S)',		9598).
valid_abstract_section_header('MATERIAL AND METHOD',		9535).
valid_abstract_section_header('RESULT(S)',		9412).
valid_abstract_section_header('HYPOTHESIS',		9257).
valid_abstract_section_header('AUTHORS\' CONCLUSIONS',		9226).
valid_abstract_section_header('KEY RESULTS',		9056).
valid_abstract_section_header('SEARCH STRATEGY',		8411).
valid_abstract_section_header('PATIENT(S)',		8290).
valid_abstract_section_header('OUTCOME MEASURES',		8256).
valid_abstract_section_header('DATA SYNTHESIS',		7639).
valid_abstract_section_header('SIGNIFICANCE AND IMPACT OF THE STUDY',		7565).
valid_abstract_section_header('PROCEDURE',		7458).
valid_abstract_section_header('RESULTS AND CONCLUSIONS',		7362).
valid_abstract_section_header('CONCLUSIONS AND CLINICAL RELEVANCE',		7328).
valid_abstract_section_header('DESIGN AND SETTING',		7312).
valid_abstract_section_header('MOTIVATION',		7292).
valid_abstract_section_header('CASE',		6945).
valid_abstract_section_header('DESIGN, SETTING, AND PARTICIPANTS',		6828).
valid_abstract_section_header('STUDY SELECTION',		6825).
valid_abstract_section_header('BACKGROUND AND AIM',		6767).
valid_abstract_section_header('AREAS COVERED',		6565).
valid_abstract_section_header('EXPERT OPINION',		6252).
valid_abstract_section_header('AVAILABILITY',		6146).
valid_abstract_section_header('STUDY OBJECTIVES',		6062).
valid_abstract_section_header('STUDY DESIGN AND METHODS',		6025).
valid_abstract_section_header('AIM OF THE STUDY',		5918).
valid_abstract_section_header('METHODS/DESIGN',		5896).
valid_abstract_section_header('PROCEDURES',		5382).
valid_abstract_section_header('MEASUREMENTS AND RESULTS',		5369).
valid_abstract_section_header('FUNDING',		5365).
valid_abstract_section_header('KEY FINDINGS',		5290).
valid_abstract_section_header('BACKGROUND/AIM',		5185).
valid_abstract_section_header('CONCLUSIONS AND RELEVANCE',		5158).
valid_abstract_section_header('DATA EXTRACTION',		4994).
valid_abstract_section_header('SEARCH METHODS',		4907).
valid_abstract_section_header('BACKGROUND/PURPOSE',		4798).
valid_abstract_section_header('AIMS/HYPOTHESIS',		4509).
valid_abstract_section_header('PRINCIPAL FINDINGS',		4497).
valid_abstract_section_header('RATIONALE AND OBJECTIVES',		4472).
valid_abstract_section_header('RESULTS AND CONCLUSION',		4397).
valid_abstract_section_header('RESULTS AND DISCUSSION',		4375).
valid_abstract_section_header('OBJECTIVES/HYPOTHESIS',		4343).
valid_abstract_section_header('POPULATION',		4288).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS',		4263).
valid_abstract_section_header('PRACTICE IMPLICATIONS',		4141).
valid_abstract_section_header('CASE DESCRIPTION',		4115).
valid_abstract_section_header('AIMS AND OBJECTIVES',		4035).
valid_abstract_section_header('DISCUSSION AND CONCLUSION',		4013).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL RELEVANCE',		3984).
valid_abstract_section_header('RELEVANCE TO CLINICAL PRACTICE',		3966).
valid_abstract_section_header('PATIENTS AND METHOD',		3965).
valid_abstract_section_header('METHODS AND FINDINGS',		3956).
valid_abstract_section_header('MAIN OUTCOMES AND MEASURES',		3868).
valid_abstract_section_header('SAMPLE',		3848).
valid_abstract_section_header('MEASURES',		3740).
valid_abstract_section_header('PROBLEM',		3734).
valid_abstract_section_header('CONCLUSIONS/INTERPRETATION',		3671).
valid_abstract_section_header('MATERIAL/METHODS',		3540).
valid_abstract_section_header('EXPERIMENTAL APPROACH',		3461).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE',		3298).
valid_abstract_section_header('MAIN OUTCOME MEASUREMENTS',		3168).
valid_abstract_section_header('KEY POINTS',		3008).
valid_abstract_section_header('CLINICAL SIGNIFICANCE',		2997).
valid_abstract_section_header('CLINICAL IMPLICATIONS',		2985).
valid_abstract_section_header('OUTCOMES',		2895).
valid_abstract_section_header('TRIAL REGISTRATION NUMBER',		2867).
valid_abstract_section_header('RESEARCH DESIGN',		2822).
valid_abstract_section_header('STUDY DESIGN AND SETTING',		2822).
valid_abstract_section_header('SETTINGS',		2761).
valid_abstract_section_header('SUMMARY BACKGROUND DATA',		2689).
valid_abstract_section_header('MATERIALS & METHODS',		2651).
valid_abstract_section_header('BACKGROUND/OBJECTIVES',		2596).
valid_abstract_section_header('REVIEWER\'S CONCLUSIONS',		2583).
valid_abstract_section_header('CONTACT',		2547).
valid_abstract_section_header('PURPOSE OF THE STUDY',		2524).
valid_abstract_section_header('QUESTIONS/PURPOSES',		2511).
valid_abstract_section_header('SUPPLEMENTARY INFORMATION',		2500).
valid_abstract_section_header('AVAILABILITY AND IMPLEMENTATION',		2432).
valid_abstract_section_header('BACKGROUND CONTEXT',		2396).
valid_abstract_section_header('STATISTICAL ANALYSIS',		2385).
valid_abstract_section_header('STATEMENT OF PROBLEM',		2374).
valid_abstract_section_header('BACKGROUND & OBJECTIVE',		2365).
valid_abstract_section_header('BACKGROUND AND STUDY AIMS',		2273).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION',		2268).
valid_abstract_section_header('MAIN METHODS',		2265).
valid_abstract_section_header('SETTINGS AND DESIGN',		2238).
valid_abstract_section_header('DISCUSSION AND CONCLUSIONS',		2183).
valid_abstract_section_header('MATERIAL',		2117).
valid_abstract_section_header('OBSERVATIONS',		2038).
valid_abstract_section_header('INTRODUCTION AND OBJECTIVES',		1969).
valid_abstract_section_header('SETTING AND PARTICIPANTS',		1940).
valid_abstract_section_header('MATERIALS',		1916).
valid_abstract_section_header('PATIENT',		1900).
valid_abstract_section_header('GENERAL SIGNIFICANCE',		1888).
valid_abstract_section_header('REVIEW METHODS',		1879).
valid_abstract_section_header('PURPOSE/OBJECTIVES',		1858).
valid_abstract_section_header('SCOPE',		1855).
valid_abstract_section_header('PERSPECTIVE',		1852).
valid_abstract_section_header('BACKGROUND & OBJECTIVES',		1839).
valid_abstract_section_header('SUBJECTS/METHODS',		1823).
valid_abstract_section_header('CASE SUMMARY',		1803).
valid_abstract_section_header('CLINICAL PRESENTATION',		1780).
valid_abstract_section_header('MAIN MEASURES',		1734).
valid_abstract_section_header('MATERIALS AND METHOD',		1731).
valid_abstract_section_header('ANALYSIS',		1726).
valid_abstract_section_header('PREMISE OF THE STUDY',		1717).
valid_abstract_section_header('PRESENTATION OF CASE',		1714).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, & MEASUREMENTS',		1712).
valid_abstract_section_header('PLACE AND DURATION OF STUDY',		1703).
valid_abstract_section_header('RESEARCH METHODS AND PROCEDURES',		1699).
valid_abstract_section_header('IMPACT',		1691).
valid_abstract_section_header('DEVELOPMENT',		1667).
valid_abstract_section_header('CONCLUSION/SIGNIFICANCE',		1658).
valid_abstract_section_header('INTRODUCTION AND HYPOTHESIS',		1653).
valid_abstract_section_header('SAMPLE POPULATION',		1624).
valid_abstract_section_header('EVIDENCE ACQUISITION',		1602).
valid_abstract_section_header('BACKGROUND AND AIM OF THE STUDY',		1585).
valid_abstract_section_header('PRACTICAL IMPLICATIONS',		1575).
valid_abstract_section_header('DATA SOURCE',		1571).
valid_abstract_section_header('CASE REPORTS',		1555).
valid_abstract_section_header('KEY MESSAGE',		1542).
valid_abstract_section_header('CONCLUSIONS AND CLINICAL IMPORTANCE',		1519).
valid_abstract_section_header('REASONS FOR PERFORMING STUDY',		1519).
valid_abstract_section_header('GOAL',		1503).
valid_abstract_section_header('METHOD OF STUDY',		1498).
valid_abstract_section_header('APPROACH',		1488).
valid_abstract_section_header('Results',		1472).
valid_abstract_section_header('INVESTIGATIONS',		1457).
valid_abstract_section_header('DESCRIPTION',		1453).
valid_abstract_section_header('STUDY QUESTION',		1438).
valid_abstract_section_header('AIMS AND BACKGROUND',		1431).
valid_abstract_section_header('LIMITATION',		1428).
valid_abstract_section_header('RESULTADOS',		1425).
valid_abstract_section_header('STUDY DESIGN/MATERIALS AND METHODS',		1423).
valid_abstract_section_header('IMPLICATIONS FOR NURSING PRACTICE',		1409).
valid_abstract_section_header('RESULTS AND LIMITATIONS',		1380).
valid_abstract_section_header('SIGNIFICANCE STATEMENT',		1376).
valid_abstract_section_header('STATISTICAL ANALYSIS USED',		1360).
valid_abstract_section_header('SUMMARY ANSWER',		1353).
valid_abstract_section_header('INTERPRETATION AND CONCLUSIONS',		1351).
valid_abstract_section_header('STUDY SELECTION AND DATA EXTRACTION',		1340).
valid_abstract_section_header('PATIENT SAMPLE',		1334).
valid_abstract_section_header('DESIGN AND PATIENTS',		1324).
valid_abstract_section_header('METHOD AND MATERIALS',		1320).
valid_abstract_section_header('MAIN RESULTS AND THE ROLE OF CHANCE',		1317).
valid_abstract_section_header('WIDER IMPLICATIONS OF THE FINDINGS',		1317).
valid_abstract_section_header('PATIENTS/METHODS',		1310).
valid_abstract_section_header('STUDY',		1306).
valid_abstract_section_header('IN CONCLUSION',		1299).
valid_abstract_section_header('WHAT IS KNOWN ALREADY',		1299).
valid_abstract_section_header('OUTCOME',		1275).
valid_abstract_section_header('CLINICAL FEATURES',		1260).
valid_abstract_section_header('ORIGINALITY/VALUE',		1256).
valid_abstract_section_header('METHODS/RESULTS',		1254).
valid_abstract_section_header('DESIGN/METHODOLOGY/APPROACH',		1253).
valid_abstract_section_header('TAKE HOME MESSAGE',		1242).
valid_abstract_section_header('LIMITATIONS, REASONS FOR CAUTION',		1204).
valid_abstract_section_header('DESIGN, SETTING, AND PATIENTS',		1201).
valid_abstract_section_header('TREATMENT',		1192).
valid_abstract_section_header('ETHICS AND DISSEMINATION',		1187).
valid_abstract_section_header('KEY WORDS',		1184).
valid_abstract_section_header('PURPOSES',		1169).
valid_abstract_section_header('STUDY SAMPLE',		1167).
valid_abstract_section_header('BACKGROUND DATA',		1164).
valid_abstract_section_header('BACKGROUND/OBJECTIVE',		1150).
valid_abstract_section_header('CONCLUSIONS & INFERENCES',		1149).
valid_abstract_section_header('METHODS AND ANALYSIS',		1149).
valid_abstract_section_header('DESIGN, SETTING AND PARTICIPANTS',		1144).
valid_abstract_section_header('GOALS',		1144).
valid_abstract_section_header('OBSERVATION',		1140).
valid_abstract_section_header('OPINION STATEMENT',		1139).
valid_abstract_section_header('DIAGNOSIS',		1137).
valid_abstract_section_header('CLINICAL QUESTION/LEVEL OF EVIDENCE',		1128).
valid_abstract_section_header('OBJETIVO',		1127).
valid_abstract_section_header('IMPORTANCE OF THE FIELD',		1122).
valid_abstract_section_header('WHAT THE READER WILL GAIN',		1120).
valid_abstract_section_header('IMPLICATIONS FOR NURSING',		1117).
valid_abstract_section_header('ABSTRACT',		1110).
valid_abstract_section_header('STUDY DESIGN, SIZE, DURATION',		1110).
valid_abstract_section_header('DESIGN AND METHOD',		1106).
valid_abstract_section_header('EVIDENCE SYNTHESIS',		1106).
valid_abstract_section_header('PATIENTS OR OTHER PARTICIPANTS',		1105).
valid_abstract_section_header('CASES',		1098).
valid_abstract_section_header('METHODS AND PROCEDURES',		1094).
valid_abstract_section_header('AREAS COVERED IN THIS REVIEW',		1091).
valid_abstract_section_header('APPROACH AND RESULTS',		1090).
valid_abstract_section_header('FINANCIAL DISCLOSURE',		1087).
valid_abstract_section_header('INCLUSION CRITERIA',		1085).
valid_abstract_section_header('INTERPRETATION & CONCLUSION',		1072).
valid_abstract_section_header('PRIMARY OBJECTIVE',		1057).
valid_abstract_section_header('RECOMMENDATIONS',		1054).
valid_abstract_section_header('SETTING & PARTICIPANTS',		1054).
valid_abstract_section_header('PARTICIPANTS AND METHODS',		1051).
valid_abstract_section_header('OBJECTIVE AND DESIGN',		1047).
valid_abstract_section_header('CLINICAL CASE',		1043).
valid_abstract_section_header('BACKGROUND AND METHODS',		1027).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTING, METHODS',		1015).
valid_abstract_section_header('MAIN RESEARCH VARIABLES',		1004).
valid_abstract_section_header('RESULTS/CONCLUSIONS',		1004).
valid_abstract_section_header('BIOLOGICAL SIGNIFICANCE',		983).
valid_abstract_section_header('TYPE OF STUDY',		979).
valid_abstract_section_header('DESIGN/METHODS',		958).
valid_abstract_section_header('OBJECTIVE AND IMPORTANCE',		957).
valid_abstract_section_header('PATIENT AND METHODS',		956).
valid_abstract_section_header('TITLE',		948).
valid_abstract_section_header('TYPE OF STUDY/LEVEL OF EVIDENCE',		946).
valid_abstract_section_header('DESIGN AND PARTICIPANTS',		932).
valid_abstract_section_header('DATA EXTRACTION AND SYNTHESIS',		915).
valid_abstract_section_header('STUDY DESIGN/SETTING',		902).
valid_abstract_section_header('INTERPRETATION & CONCLUSIONS',		887).
valid_abstract_section_header('INTRODUCTION AND AIMS',		887).
valid_abstract_section_header('OUTCOME MEASURE',		875).
valid_abstract_section_header('CONCLUSION AND CLINICAL RELEVANCE',		874).
valid_abstract_section_header('BACKGROUND AIMS',		865).
valid_abstract_section_header('APPLICATION',		855).
valid_abstract_section_header('KEYWORDS',		853).
valid_abstract_section_header('POTENTIAL RELEVANCE',		853).
valid_abstract_section_header('PRACTICAL APPLICATION',		849).
valid_abstract_section_header('QUESTION',		847).
valid_abstract_section_header('CONCLUSION/INTERPRETATION',		844).
valid_abstract_section_header('METHOD AND RESULTS',		835).
valid_abstract_section_header('IMPLICATIONS FOR NURSING MANAGEMENT',		832).
valid_abstract_section_header('RESULTS/CONCLUSION',		828).
valid_abstract_section_header('ADVANCES IN KNOWLEDGE',		815).
valid_abstract_section_header('FROM THE CLINICAL EDITOR',		807).
valid_abstract_section_header('AIM OF STUDY',		804).
valid_abstract_section_header('DESIGN OF STUDY',		800).
valid_abstract_section_header('MATERIALS/METHODS',		792).
valid_abstract_section_header('MAIN FINDINGS',		782).
valid_abstract_section_header('STATEMENT OF SIGNIFICANCE',		778).
valid_abstract_section_header('CONTEXT AND OBJECTIVE',		765).
valid_abstract_section_header('EVALUATION',		762).
valid_abstract_section_header('METHODOLOGY AND PRINCIPAL FINDINGS',		750).
valid_abstract_section_header('Objective',		736).
valid_abstract_section_header('TREATMENT AND COURSE',		725).
valid_abstract_section_header('STUDY POPULATION',		724).
valid_abstract_section_header('OBJECTIVE AND METHODS',		723).
valid_abstract_section_header('EVIDENCE',		720).
valid_abstract_section_header('RATIONALE, AIMS AND OBJECTIVES',		720).
valid_abstract_section_header('PRIMARY FUNDING SOURCE',		715).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME MEASURES',		711).
valid_abstract_section_header('BACKGROUNDS',		709).
valid_abstract_section_header('NEW METHOD',		700).
valid_abstract_section_header('OBJECTIF',		699).
valid_abstract_section_header('MEASUREMENT',		695).
valid_abstract_section_header('SURGICAL TECHNIQUE',		690).
valid_abstract_section_header('COMMENTS',		683).
valid_abstract_section_header('FINANCIAL DISCLOSURE(S)',		671).
valid_abstract_section_header('SIGNIFICANCE OF RESULTS',		670).
valid_abstract_section_header('SETTING/PARTICIPANTS',		665).
valid_abstract_section_header('FUTURE DIRECTIONS',		662).
valid_abstract_section_header('STUDY FUNDING/COMPETING INTERESTS',		662).
valid_abstract_section_header('AIMS OF THE STUDY',		659).
valid_abstract_section_header('RELEVANCE',		656).
valid_abstract_section_header('INDICATIONS',		651).
valid_abstract_section_header('LEARNING OBJECTIVES',		650).
valid_abstract_section_header('OBJECTIVES AND METHODS',		645).
valid_abstract_section_header('PATIENT SUMMARY',		644).
valid_abstract_section_header('WHAT IS KNOWN AND OBJECTIVE',		644).
valid_abstract_section_header('METHODS & PROCEDURES',		642).
valid_abstract_section_header('OUTCOME MEASUREMENTS AND STATISTICAL ANALYSIS',		634).
valid_abstract_section_header('Importance',		631).
valid_abstract_section_header('MAIN OUTCOMES AND RESULTS',		631).
valid_abstract_section_header('INTRODUCTION AND OBJECTIVE',		630).
valid_abstract_section_header('DATA COLLECTION',		627).
valid_abstract_section_header('Conclusions and Relevance',		623).
valid_abstract_section_header('SETTING AND DESIGN',		623).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS',		620).
valid_abstract_section_header('PATIENTS AND PARTICIPANTS',		619).
valid_abstract_section_header('WHAT IS NEW AND CONCLUSION',		619).
valid_abstract_section_header('MANAGEMENT',		614).
valid_abstract_section_header('MAIN MEASUREMENTS',		612).
valid_abstract_section_header('METHODS AND MATERIAL',		612).
valid_abstract_section_header('RECENT ADVANCES',		612).
valid_abstract_section_header('TECHNIQUE',		612).
valid_abstract_section_header('CRITICAL ISSUES',		609).
valid_abstract_section_header('PARTICIPANTS AND SETTING',		608).
valid_abstract_section_header('OUTCOMES & RESULTS',		607).
valid_abstract_section_header('ASSESSMENT',		597).
valid_abstract_section_header('MAIN OUTCOME',		596).
valid_abstract_section_header('DESIGN AND SUBJECTS',		593).
valid_abstract_section_header('POPULATION AND METHODS',		593).
valid_abstract_section_header('Methods',		591).
valid_abstract_section_header('INNOVATION',		589).
valid_abstract_section_header('STATISTICS',		589).
valid_abstract_section_header('VIRTUAL SLIDES',		582).
valid_abstract_section_header('ANIMAL(S)',		580).
valid_abstract_section_header('METHODS AND OUTCOMES',		576).
valid_abstract_section_header('PATIENTS/PARTICIPANTS',		576).
valid_abstract_section_header('EXPOSURES',		574).
valid_abstract_section_header('MAIN OUTCOMES MEASURES',		566).
valid_abstract_section_header('MAJOR CONCLUSIONS',		565).
valid_abstract_section_header('SOURCES',		559).
valid_abstract_section_header('ABBREVIATIONS',		556).
valid_abstract_section_header('Conclusions',		555).
valid_abstract_section_header('PEDOT',		554).
valid_abstract_section_header('CONTRAINDICATIONS',		551).
valid_abstract_section_header('PATIENTS & METHODS',		551).
valid_abstract_section_header('METHODS AND PATIENTS',		545).
valid_abstract_section_header('DATA SOURCES/STUDY SETTING',		544).
valid_abstract_section_header('INTERVENTION AND OUTCOME',		536).
valid_abstract_section_header('DISCUSSION/CONCLUSION',		535).
valid_abstract_section_header('Main Outcomes and Measures',		529).
valid_abstract_section_header('CLINICAL TRIALS REGISTRATION',		528).
valid_abstract_section_header('METHODS AND DESIGN',		524).
valid_abstract_section_header('OBJECTIVE/HYPOTHESIS',		520).
valid_abstract_section_header('SUBJECT',		520).
valid_abstract_section_header('SPONSORSHIP',		510).
valid_abstract_section_header('HISTORY AND CLINICAL FINDINGS',		509).
valid_abstract_section_header('CLINICAL FINDINGS',		507).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION NUMBER',		506).
valid_abstract_section_header('OBJECTIVE(S)',		505).
valid_abstract_section_header('SUBJECTS AND METHOD',		504).
valid_abstract_section_header('HYPOTHESIS/OBJECTIVES',		503).
valid_abstract_section_header('Design, Setting, and Participants',		502).
valid_abstract_section_header('CONCLUSIONS AND SIGNIFICANCE',		496).
valid_abstract_section_header('MAIN CONCLUSION',		495).
valid_abstract_section_header('KEY CONCLUSIONS',		492).
valid_abstract_section_header('TOPIC',		491).
valid_abstract_section_header('LEARNING OUTCOMES',		490).
valid_abstract_section_header('MAIN OUTCOMES',		490).
valid_abstract_section_header('IMPLICATIONS FOR REHABILITATION',		488).
valid_abstract_section_header('BULGULAR',		487).
valid_abstract_section_header('CONTEXTE',		486).
valid_abstract_section_header('RESEARCH LIMITATIONS/IMPLICATIONS',		482).
valid_abstract_section_header('METHODS AND MEASURES',		473).
valid_abstract_section_header('SCOPE OF REVIEW',		473).
valid_abstract_section_header('PREDICTOR',		469).
valid_abstract_section_header('Purpose',		469).
valid_abstract_section_header('IMPLICATIONS FOR CANCER SURVIVORS',		466).
valid_abstract_section_header('METHODS AND STUDY DESIGN',		466).
valid_abstract_section_header('POSTOPERATIVE MANAGEMENT',		465).
valid_abstract_section_header('CONCLUSION AND DISCUSSION',		460).
valid_abstract_section_header('CONCLUSIONS & IMPLICATIONS',		458).
valid_abstract_section_header('MATERIAL & METHODS',		458).
valid_abstract_section_header('STATISTICAL ANALYSES PERFORMED',		455).
valid_abstract_section_header('PERSPECTIVES',		451).
valid_abstract_section_header('CONTROL',		446).
valid_abstract_section_header('TREATMENT AND OUTCOME',		446).
valid_abstract_section_header('REVIEWERS\' CONCLUSIONS',		445).
valid_abstract_section_header('SETTING AND PATIENTS',		445).
valid_abstract_section_header('EXCLUSION CRITERIA',		443).
valid_abstract_section_header('SUBJECTS AND SETTING',		443).
valid_abstract_section_header('CONTENT',		441).
valid_abstract_section_header('METHODS/PRINCIPAL FINDINGS',		438).
valid_abstract_section_header('SETTING AND SUBJECTS',		438).
valid_abstract_section_header('PATIENTS AND INTERVENTIONS',		426).
valid_abstract_section_header('IMPACT ON INDUSTRY',		425).
valid_abstract_section_header('HISTORY AND ADMISSION FINDINGS',		421).
valid_abstract_section_header('TARGET AUDIENCE',		421).
valid_abstract_section_header('DATA',		419).
valid_abstract_section_header('OVERVIEW',		412).
valid_abstract_section_header('SUBJECTS/SETTING',		410).
valid_abstract_section_header('AIMS AND METHODS',		408).
valid_abstract_section_header('KEY MESSAGES',		408).
valid_abstract_section_header('OVERVIEW OF LITERATURE',		406).
valid_abstract_section_header('RESULTS AND INTERPRETATION',		405).
valid_abstract_section_header('PURPOSE OF INVESTIGATION',		395).
valid_abstract_section_header('SYSTEMATIC REVIEW REGISTRATION',		395).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR PRACTICE',		394).
valid_abstract_section_header('CLASSIFICATION OF EVIDENCE',		392).
valid_abstract_section_header('COMPARISON WITH EXISTING METHODS',		388).
valid_abstract_section_header('HYPOTHESES',		386).
valid_abstract_section_header('RESULT AND CONCLUSION',		383).
valid_abstract_section_header('STUDY PARTICIPANTS',		383).
valid_abstract_section_header('DATA AND METHODS',		382).
valid_abstract_section_header('DATA COLLECTION/EXTRACTION METHODS',		382).
valid_abstract_section_header('STATISTICAL ANALYSES',		382).
valid_abstract_section_header('Conclusion',		380).
valid_abstract_section_header('LESSONS LEARNED',		379).
valid_abstract_section_header('THE AIM OF THE STUDY',		376).
valid_abstract_section_header('BACKGROUND INFORMATION',		375).
valid_abstract_section_header('SOURCES OF DATA',		373).
valid_abstract_section_header('EXEGESIS',		372).
valid_abstract_section_header('CONCLUSIONES',		366).
valid_abstract_section_header('DESIGN AND SAMPLE',		362).
valid_abstract_section_header('PRACTITIONER SUMMARY',		358).
valid_abstract_section_header('AIM AND METHODS',		353).
valid_abstract_section_header('DESIGN/SETTING',		352).
valid_abstract_section_header('ZUSAMMENFASSUNG',		352).
valid_abstract_section_header('PATIENTS OR PARTICIPANTS',		351).
valid_abstract_section_header('THE AIM',		346).
valid_abstract_section_header('DATA ANALYSIS',		344).
valid_abstract_section_header('STUDY FUNDING/COMPETING INTEREST(S)',		344).
valid_abstract_section_header('CASE OUTLINE',		340).
valid_abstract_section_header('MEASUREMENT AND MAIN RESULTS',		339).
valid_abstract_section_header('STUDY AIM',		335).
valid_abstract_section_header('LOCATION',		332).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION URL',		330).
valid_abstract_section_header('PRIMARY OUTCOME',		330).
valid_abstract_section_header('CITATION',		329).
valid_abstract_section_header('METHODS OF STUDY SELECTION',		327).
valid_abstract_section_header('MAIN OUTCOME MEASUREMENT',		326).
valid_abstract_section_header('CLINICAL REHABILITATION IMPACT',		325).
valid_abstract_section_header('THEORY AND METHODS',		325).
valid_abstract_section_header('AIMS/INTRODUCTION',		324).
valid_abstract_section_header('EDUCATIONAL OBJECTIVES',		324).
valid_abstract_section_header('DECLARATION OF INTEREST',		323).
valid_abstract_section_header('MAIN MESSAGE',		323).
valid_abstract_section_header('PATIENTS AND OTHER PARTICIPANTS',		322).
valid_abstract_section_header('LEVELS OF EVIDENCE',		321).
valid_abstract_section_header('SUBJECT AND METHODS',		320).
valid_abstract_section_header('AREAS OF AGREEMENT',		319).
valid_abstract_section_header('AIM AND OBJECTIVES',		316).
valid_abstract_section_header('PREDICTORS',		313).
valid_abstract_section_header('SUMMARY OF REVIEW',		313).
valid_abstract_section_header('WHAT THIS STUDY ADDS',		313).
valid_abstract_section_header('GOALS OF WORK',		312).
valid_abstract_section_header('CONFLICT OF INTEREST',		309).
valid_abstract_section_header('BACKGROUND AND AIMS OF THE STUDY',		308).
valid_abstract_section_header('CLINICAL PICTURE',		308).
valid_abstract_section_header('PATIENTS AND DESIGN',		308).
valid_abstract_section_header('ANSWER',		307).
valid_abstract_section_header('EVIDENCE REVIEW',		307).
valid_abstract_section_header('EXPOSURE',		307).
valid_abstract_section_header('ISSUE ADDRESSED',		304).
valid_abstract_section_header('PRIMARY OUTCOME MEASURES',		304).
valid_abstract_section_header('AIMS/BACKGROUND',		296).
valid_abstract_section_header('AREAS OF CONTROVERSY',		296).
valid_abstract_section_header('SOURCE',		294).
valid_abstract_section_header('CURRENT KNOWLEDGE AND KEY POINTS',		293).
valid_abstract_section_header('BACKGROUND AND DESIGN',		292).
valid_abstract_section_header('OUTCOMES & MEASUREMENTS',		292).
valid_abstract_section_header('DESIGN AND SETTINGS',		291).
valid_abstract_section_header('HISTORIQUE',		290).
valid_abstract_section_header('CASE HISTORY',		286).
valid_abstract_section_header('SCIENTIFIC SIGNIFICANCE',		285).
valid_abstract_section_header('VALUES',		285).
valid_abstract_section_header('COMMENT',		284).
valid_abstract_section_header('CONTROL GROUP',		283).
valid_abstract_section_header('OBJECTS',		281).
valid_abstract_section_header('REGISTRATION NUMBER',		279).
valid_abstract_section_header('TRIAL REGISTRY',		278).
valid_abstract_section_header('TWEETABLE ABSTRACT',		278).
valid_abstract_section_header('DATA SOURCES AND STUDY SELECTION',		276).
valid_abstract_section_header('PURPOSE AND METHODS',		276).
valid_abstract_section_header('TABULATION, INTEGRATION, AND RESULTS',		272).
valid_abstract_section_header('APPLICATIONS',		271).
valid_abstract_section_header('WHAT IS ALREADY KNOWN ABOUT THIS SUBJECT',		270).
valid_abstract_section_header('PATIENTS AND MEASUREMENTS',		269).
valid_abstract_section_header('DISCUSSIONS',		268).
valid_abstract_section_header('QUALITY OF EVIDENCE',		268).
valid_abstract_section_header('REVIEW SUMMARY',		268).
valid_abstract_section_header('STATEMENT OF THE PROBLEM',		268).
valid_abstract_section_header('OBJECTIVE/BACKGROUND',		267).
valid_abstract_section_header('Results:',		267).
valid_abstract_section_header('METHODS?AND?RESULTS',		265).
valid_abstract_section_header('PATIENTS, METHODS',		265).
valid_abstract_section_header('AIM AND OBJECTIVE',		259).
valid_abstract_section_header('GROWING POINTS',		258).
valid_abstract_section_header('EBM RATING',		256).
valid_abstract_section_header('MAIN OUTCOME MEASURES AND RESULTS',		255).
valid_abstract_section_header('DIAGNOSIS, TREATMENT AND COURSE',		254).
valid_abstract_section_header('KEY ISSUES',		254).
valid_abstract_section_header('OPTIONS',		254).
valid_abstract_section_header('RECOMMENDATIONS AND PERSPECTIVES',		254).
valid_abstract_section_header('PATIENTS, PARTICIPANTS',		252).
valid_abstract_section_header('BACKGROUND AND STUDY AIM',		251).
valid_abstract_section_header('SUMMARY OF THE FINDINGS',		251).
valid_abstract_section_header('BACKGROUND AND IMPORTANCE',		250).
valid_abstract_section_header('IMPLICATIONS FOR RESEARCH',		250).
valid_abstract_section_header('PATIENTS AND SETTING',		250).
valid_abstract_section_header('HISTORY',		249).
valid_abstract_section_header('NEW FINDINGS',		249).
valid_abstract_section_header('BACKGROUNDS/AIMS',		246).
valid_abstract_section_header('CONTROLS',		245).
valid_abstract_section_header('FUTURE PROSPECTS AND PROJECTS',		244).
valid_abstract_section_header('AREAS TIMELY FOR DEVELOPING RESEARCH',		243).
valid_abstract_section_header('CONCLUSIONS AND POTENTIAL RELEVANCE',		243).
valid_abstract_section_header('CONCLUSIONS/IMPLICATIONS FOR PRACTICE',		243).
valid_abstract_section_header('SAMPLING AND METHODS',		243).
valid_abstract_section_header('METHODS & RESULTS',		241).
valid_abstract_section_header('CASE STUDY',		238).
valid_abstract_section_header('PRIMARY OUTCOME MEASURE',		238).
valid_abstract_section_header('RECOMMENDATION',		235).
valid_abstract_section_header('STUDY DESIGN/METHODS',		235).
valid_abstract_section_header('CLINICAL PRESENTATION AND INTERVENTION',		233).
valid_abstract_section_header('LAY ABSTRACT',		233).
valid_abstract_section_header('SECONDARY OUTCOMES',		233).
valid_abstract_section_header('PROBLEM/CONDITION',		232).
valid_abstract_section_header('CONTENTS',		230).
valid_abstract_section_header('THERAPY',		230).
valid_abstract_section_header('SIGNIFICANCE AND IMPACT OF STUDY',		227).
valid_abstract_section_header('CONCLUSION AND CLINICAL IMPORTANCE',		226).
valid_abstract_section_header('PURPOSE OF STUDY',		226).
valid_abstract_section_header('CONSENSUS PROCESS',		225).
valid_abstract_section_header('DISCUSSION AND EVALUATION',		224).
valid_abstract_section_header('KEY PRACTITIONER MESSAGE',		224).
valid_abstract_section_header('SAMPLES',		223).
valid_abstract_section_header('COMPLICATIONS',		222).
valid_abstract_section_header('LEARNING POINTS',		221).
valid_abstract_section_header('RESULTS & CONCLUSION',		221).
valid_abstract_section_header('ANIMALS STUDIED',		219).
valid_abstract_section_header('LEVEL OF EVIDENCE IV',		219).
valid_abstract_section_header('Conclusion:',		218).
valid_abstract_section_header('STATE OF THE ART',		218).
valid_abstract_section_header('WHAT IS KNOWN',		218).
valid_abstract_section_header('PARTICIPANTS/SETTING',		215).
valid_abstract_section_header('CONCLUSION AND RECOMMENDATIONS',		214).
valid_abstract_section_header('METHODS/MATERIALS',		214).
valid_abstract_section_header('RATIONALE AND OBJECTIVE',		213).
valid_abstract_section_header('SUPPLEMENTAL MATERIAL',		213).
valid_abstract_section_header('TYPE OF PARTICIPANTS',		213).
valid_abstract_section_header('ETHICAL CONSIDERATIONS',		211).
valid_abstract_section_header('STUDY SETTING',		211).
valid_abstract_section_header('VALIDATION',		211).
valid_abstract_section_header('APPLICATIONS/CONCLUSIONS',		210).
valid_abstract_section_header('CONCLUSION AND SIGNIFICANCE',		210).
valid_abstract_section_header('LEVEL OF EVIDENCE V',		208).
valid_abstract_section_header('CLINICAL CASES',		207).
valid_abstract_section_header('BACKGROUND & AIM',		206).
valid_abstract_section_header('NAD(P)H',		205).
valid_abstract_section_header('REPORT',		205).
valid_abstract_section_header('STUDY REGISTRATION',		205).
valid_abstract_section_header('LESSONS LEARNT',		203).
valid_abstract_section_header('KEY CONCLUSIONS AND IMPLICATIONS FOR PRACTICE',		202).
valid_abstract_section_header('REVIEW',		201).
valid_abstract_section_header('BACKGROUND, AIM, AND SCOPE',		199).
valid_abstract_section_header('METHODOLOGICAL QUALITY',		199).
valid_abstract_section_header('OBJECTIFS',		199).
valid_abstract_section_header('QUESTIONS UNDER STUDY',		199).
valid_abstract_section_header('BASIC RESEARCH DESIGN',		198).
valid_abstract_section_header('Objective:',		198).
valid_abstract_section_header('PURPOSE/OBJECTIVE',		198).
valid_abstract_section_header('REVIEWERS',		198).
valid_abstract_section_header('ISSUES',		196).
valid_abstract_section_header('DESIGN AND MEASUREMENTS',		195).
valid_abstract_section_header('SUMMARY AND CONCLUSION',		195).
valid_abstract_section_header('INTERPRETATION AND CONCLUSION',		194).
valid_abstract_section_header('SETTING AND SAMPLE POPULATION',		194).
valid_abstract_section_header('METHODS AND SUBJECTS',		193).
valid_abstract_section_header('INTRODUCTION AND AIM',		191).
valid_abstract_section_header('TRANSLATIONAL RELEVANCE',		191).
valid_abstract_section_header('OBJECTIVE/METHODS',		190).
valid_abstract_section_header('DESCRIPTION OF SYSTEM',		188).
valid_abstract_section_header('INSTRUMENTS',		188).
valid_abstract_section_header('CONCLUSIONS AND SCIENTIFIC SIGNIFICANCE',		187).
valid_abstract_section_header('RESULTS:',		187).
valid_abstract_section_header('PARTICIPANT',		186).
valid_abstract_section_header('TARGET POPULATION',		186).
valid_abstract_section_header('WNIOSKI',		186).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS',		185).
valid_abstract_section_header('METHOD/DESIGN',		185).
valid_abstract_section_header('INTERPRETATIONS',		184).
valid_abstract_section_header('PATIENT AND METHOD',		184).
valid_abstract_section_header('AIMS AND OBJECTIVE',		183).
valid_abstract_section_header('DISCUSSION/CONCLUSIONS',		183).
valid_abstract_section_header('MEASUREMENT AND RESULTS',		183).
valid_abstract_section_header('METHODS AND PRINCIPAL FINDINGS',		183).
valid_abstract_section_header('PRACTITIONER POINTS',		183).
valid_abstract_section_header('SETTING/SUBJECTS',		183).
valid_abstract_section_header('WYNIKI',		183).
valid_abstract_section_header('DESIGN, SETTING AND PATIENTS',		181).
valid_abstract_section_header('ELIGIBILITY CRITERIA',		180).
valid_abstract_section_header('PRINCIPAL RESULTS',		180).
valid_abstract_section_header('FOLLOW-UP',		179).
valid_abstract_section_header('IMPLICATION',		179).
valid_abstract_section_header('VARIABLES',		179).
valid_abstract_section_header('OBJECTIVE AND BACKGROUND',		178).
valid_abstract_section_header('SYNTHESIS',		176).
valid_abstract_section_header('MAIN EXPOSURE',		174).
valid_abstract_section_header('STUDY APPRAISAL AND SYNTHESIS METHODS',		174).
valid_abstract_section_header('TYPES OF STUDIES REVIEWED',		174).
valid_abstract_section_header('DATABASE',		173).
valid_abstract_section_header('NEW INFORMATION',		172).
valid_abstract_section_header('PATIENTS AND RESULTS',		172).
valid_abstract_section_header('METHODOLOGY/PRINCIPAL FINDING',		171).
valid_abstract_section_header('OBJETIVOS',		171).
valid_abstract_section_header('LITERATURE REVIEW',		170).
valid_abstract_section_header('RESEARCH APPROACH',		170).
valid_abstract_section_header('MATERIAL/PATIENTS AND METHODS',		169).
valid_abstract_section_header('METHODOLOGY/APPROACH',		168).
valid_abstract_section_header('RESULTS/FINDINGS',		168).
valid_abstract_section_header('TESTING THE HYPOTHESIS',		168).
valid_abstract_section_header('COMPARISON WITH EXISTING METHOD(S)',		167).
valid_abstract_section_header('METHODOLOGIC APPROACH',		167).
valid_abstract_section_header('SOURCE OF FUNDING',		166).
valid_abstract_section_header('PRACTICAL APPLICATIONS',		165).
valid_abstract_section_header('OBJECTIVE AND METHOD',		164).
valid_abstract_section_header('PRACTICE DESCRIPTION',		164).
valid_abstract_section_header('PROGRAM DESCRIPTION',		164).
valid_abstract_section_header('REGISTRATION',		163).
valid_abstract_section_header('CLINICAL QUESTION',		162).
valid_abstract_section_header('ANIMALS AND METHODS',		161).
valid_abstract_section_header('BASIC PROCEDURES',		161).
valid_abstract_section_header('PATIENTS AND INTERVENTION',		161).
valid_abstract_section_header('PRACTICAL RELEVANCE',		161).
valid_abstract_section_header('PRESENTATION OF THE HYPOTHESIS',		161).
valid_abstract_section_header('PRACTICE INNOVATION',		160).
valid_abstract_section_header('IMPLICATIONS OF THE HYPOTHESIS',		159).
valid_abstract_section_header('MATERIA? I METODY',		159).
valid_abstract_section_header('PARTICIPANTS AND CONTROLS',		159).
valid_abstract_section_header('CASE SERIES',		157).
valid_abstract_section_header('KEY LEARNING POINTS',		157).
valid_abstract_section_header('QUESTIONS',		157).
valid_abstract_section_header('RISULTATI',		157).
valid_abstract_section_header('SUMMARY AND BACKGROUND DATA',		157).
valid_abstract_section_header('EXPERIMENTS',		156).
valid_abstract_section_header('CONCLUSIONS AND RECOMMENDATIONS',		155).
valid_abstract_section_header('METHODOLOGY/FINDINGS',		155).
valid_abstract_section_header('PRINCIPAL CONCLUSIONS',		155).
valid_abstract_section_header('SAMPLE AND METHODS',		155).
valid_abstract_section_header('SUBJECTS AND DESIGN',		155).
valid_abstract_section_header('CASE PRESENTATIONS',		153).
valid_abstract_section_header('ISSUE',		153).
valid_abstract_section_header('LAY SUMMARY',		152).
valid_abstract_section_header('METHODS:',		152).
valid_abstract_section_header('CONCLUSION AND RELEVANCE',		151).
valid_abstract_section_header('DISCUSSION-CONCLUSION',		150).
valid_abstract_section_header('PURPOSE/BACKGROUND',		150).
valid_abstract_section_header('PURPOSE/METHODS',		150).
valid_abstract_section_header('CASE DESCRIPTIONS',		149).
valid_abstract_section_header('FINDING',		149).
valid_abstract_section_header('FINDINGS AND CONCLUSIONS',		149).
valid_abstract_section_header('Methods:',		149).
valid_abstract_section_header('OUTCOMES MEASURED',		149).
valid_abstract_section_header('SOURCES OF INFORMATION',		149).
valid_abstract_section_header('IMPLEMENTATION',		148).
valid_abstract_section_header('IMPLICATIONS FOR CASE MANAGEMENT PRACTICE',		148).
valid_abstract_section_header('MEDICATION',		148).
valid_abstract_section_header('AIM/HYPOTHESIS',		147).
valid_abstract_section_header('LINKED ARTICLES',		147).
valid_abstract_section_header('OBJECTIVE OF THE STUDY',		146).
valid_abstract_section_header('MEASUREMENTS AND FINDINGS',		145).
valid_abstract_section_header('RESEARCH QUESTION',		145).
valid_abstract_section_header('PATIENT POPULATION',		144).
valid_abstract_section_header('ELECTRONIC SUPPLEMENTARY MATERIAL',		143).
valid_abstract_section_header('REPORTING PERIOD COVERED',		143).
valid_abstract_section_header('ANALYSES',		142).
valid_abstract_section_header('MATERIAL OF STUDY',		141).
valid_abstract_section_header('Objectives',		141).
valid_abstract_section_header('PRINCIPLES',		141).
valid_abstract_section_header('SURGICAL PROCEDURE',		141).
valid_abstract_section_header('CONCLUSIONS/IMPLICATIONS',		140).
valid_abstract_section_header('PURPOSE OF THE REVIEW',		140).
valid_abstract_section_header('REASON FOR PERFORMING STUDY',		140).
valid_abstract_section_header('RESULTS & DISCUSSION',		138).
valid_abstract_section_header('TECHNICAL CONSIDERATIONS',		138).
valid_abstract_section_header('CASE DETAILS',		137).
valid_abstract_section_header('CONCLUSIONI',		137).
valid_abstract_section_header('DATA SELECTION',		136).
valid_abstract_section_header('SETTING & POPULATION',		136).
valid_abstract_section_header('DESIGN, SETTING AND SUBJECTS',		135).
valid_abstract_section_header('MAIN MEASUREMENTS AND RESULTS',		135).
valid_abstract_section_header('PRACTICAL RECOMMENDATIONS',		135).
valid_abstract_section_header('Background/Aims',		134).
valid_abstract_section_header('OBJECTIVES AND DESIGN',		134).
valid_abstract_section_header('STUDY TYPE',		134).
valid_abstract_section_header('BACKGROUND AND HYPOTHESIS',		133).
valid_abstract_section_header('RESEARCH QUESTIONS',		133).
valid_abstract_section_header('VIDEO ABSTRACT',		133).
valid_abstract_section_header('ASSESSMENT OF RISK FACTORS',		132).
valid_abstract_section_header('LEVEL OF CLINICAL EVIDENCE',		132).
valid_abstract_section_header('SUMMARY AND CONCLUSIONS',		132).
valid_abstract_section_header('DATA IDENTIFICATION',		131).
valid_abstract_section_header('PRIMARY OUTCOMES',		131).
valid_abstract_section_header('RESULTS/DISCUSSION',		131).
valid_abstract_section_header('THERAPY AND OUTCOME',		131).
valid_abstract_section_header('EVIDENCE BASE',		130).
valid_abstract_section_header('Method',		130).
valid_abstract_section_header('OUTCOME MEASUREMENTS',		130).
valid_abstract_section_header('ANALYTICAL TECHNIQUES',		128).
valid_abstract_section_header('REVIEW QUESTION/OBJECTIVE',		128).
valid_abstract_section_header('THE RESULTS',		128).
valid_abstract_section_header('HISTORY AND SIGNS',		127).
valid_abstract_section_header('DESIGN, SETTING, AND SUBJECTS',		126).
valid_abstract_section_header('GRAPHICAL ABSTRACT',		125).
valid_abstract_section_header('RESULTS AND DISCUSSIONS',		125).
valid_abstract_section_header('TIME HORIZON',		125).
valid_abstract_section_header('Background',		124).
valid_abstract_section_header('DISCLOSURES',		124).
valid_abstract_section_header('INTERVENTIONS AND MEASUREMENTS',		124).
valid_abstract_section_header('MESSAGE',		124).
valid_abstract_section_header('METHODOLOGY AND RESULTS',		124).
valid_abstract_section_header('METHODS/FINDINGS',		124).
valid_abstract_section_header('NEXT STEPS',		124).
valid_abstract_section_header('OUTCOMES AND RESULTS',		124).
valid_abstract_section_header('STUDY DESIGN, SIZE AND DURATION',		124).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA',		124).
valid_abstract_section_header('DESIGNS',		123).
valid_abstract_section_header('ARTICLE TITLE AND BIBLIOGRAPHIC INFORMATION',		122).
valid_abstract_section_header('ACHIEVEMENTS',		121).
valid_abstract_section_header('BACKGROUND AND AIM OF THE WORK',		121).
valid_abstract_section_header('BACKGROUND AND THE PURPOSE OF THE STUDY',		120).
valid_abstract_section_header('CLINICAL CHALLENGES',		120).
valid_abstract_section_header('HINTERGRUND',		120).
valid_abstract_section_header('Resultados:',		120).
valid_abstract_section_header('TYPE OF STUDY/DESIGN',		120).
valid_abstract_section_header('COMMENTARY',		119).
valid_abstract_section_header('EPIDEMIOLOGY',		119).
valid_abstract_section_header('METHODOLOGY AND FINDINGS',		118).
valid_abstract_section_header('SUMMARY BACKGROUND',		118).
valid_abstract_section_header('AIM AND BACKGROUND',		117).
valid_abstract_section_header('DESIGN, PARTICIPANTS AND SETTING',		117).
valid_abstract_section_header('RELEVANT CHANGES',		117).
valid_abstract_section_header('SELECTION CRITERIA FOR STUDIES',		117).
valid_abstract_section_header('CASE CHARACTERISTICS',		116).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION INFORMATION',		116).
valid_abstract_section_header('INTRODUCTION AND DEVELOPMENT',		116).
valid_abstract_section_header('LOCAL SETTING',		116).
valid_abstract_section_header('MAIN OUTCOME AND MEASURE',		116).
valid_abstract_section_header('MAIN VARIABLES',		116).
valid_abstract_section_header('METHODS AND SAMPLE',		116).
valid_abstract_section_header('SUBJECT AND METHOD',		116).
valid_abstract_section_header('TEACHING POINTS',		116).
valid_abstract_section_header('CLINICAL TRIAL NUMBER',		115).
valid_abstract_section_header('MAIN OBSERVATIONS',		115).
valid_abstract_section_header('METHODOLOGY/PRINCIPLE FINDINGS',		115).
valid_abstract_section_header('PERFORMANCE',		115).
valid_abstract_section_header('Interventions',		114).
valid_abstract_section_header('KEY RESULTS AND CONCLUSIONS',		114).
valid_abstract_section_header('LINKING EVIDENCE TO ACTION',		114).
valid_abstract_section_header('METHODS OF STUDY',		114).
valid_abstract_section_header('RESEARCH METHODS',		114).
valid_abstract_section_header('RESULTS OF BASE-CASE ANALYSIS',		114).
valid_abstract_section_header('THEORY',		114).
valid_abstract_section_header('BACKGROUNDS AND AIMS',		113).
valid_abstract_section_header('BENEFITS, HARMS, AND COSTS',		113).
valid_abstract_section_header('EXPERIMENTAL MATERIAL',		113).
valid_abstract_section_header('PURPOSE/HYPOTHESIS',		113).
valid_abstract_section_header('STATE OF ART',		113).
valid_abstract_section_header('CONCLUSIONS:',		112).
valid_abstract_section_header('DESIGN/SETTING/PARTICIPANTS',		112).
valid_abstract_section_header('STANDARD RADIOLOGICAL METHODS',		112).
valid_abstract_section_header('AREA COVERED',		111).
valid_abstract_section_header('MEASUREMENTS/MAIN RESULTS',		111).
valid_abstract_section_header('PRINCIPLE FINDINGS',		111).
valid_abstract_section_header('AIM AND METHOD',		110).
valid_abstract_section_header('IMPLICATIONS FOR NURSING AND HEALTH POLICY',		110).
valid_abstract_section_header('INDEPENDENT VARIABLES',		110).
valid_abstract_section_header('RESULTS OF SENSITIVITY ANALYSIS',		110).
valid_abstract_section_header('BACKGROUND, AIM AND SCOPE',		109).
valid_abstract_section_header('ELIGIBILITY CRITERIA FOR SELECTING STUDIES',		109).
valid_abstract_section_header('COMPARISON WITH EXISTING METHOD',		108).
valid_abstract_section_header('DESIGN AND INTERVENTIONS',		108).
valid_abstract_section_header('INNOVATION AND CONCLUSION',		108).
valid_abstract_section_header('PUBLIC HEALTH ACTION',		108).
valid_abstract_section_header('SETTINGS/LOCATION',		108).
valid_abstract_section_header('AIM OF THE REVIEW',		107).
valid_abstract_section_header('ERGEBNISSE',		107).
valid_abstract_section_header('HYPOTHESIS/PURPOSE',		107).
valid_abstract_section_header('NEW OR UNIQUE INFORMATION PROVIDED',		107).
valid_abstract_section_header('BACKGROUND AND RATIONALE',		106).
valid_abstract_section_header('CASE-REPORT',		106).
valid_abstract_section_header('KEY CLINICAL MESSAGE',		106).
valid_abstract_section_header('Objetivo:',		106).
valid_abstract_section_header('PARTICIPANTS AND RESEARCH CONTEXT',		106).
valid_abstract_section_header('SYNOPSIS',		106).
valid_abstract_section_header('AIM OF THIS STUDY',		104).
valid_abstract_section_header('CASE-DIAGNOSIS/TREATMENT',		104).
valid_abstract_section_header('CLINICAL/METHODICAL ISSUE',		104).
valid_abstract_section_header('PATIENTS AND MATERIALS',		104).
valid_abstract_section_header('REPORTING PERIOD',		104).
valid_abstract_section_header('RESULTS OF DATA SYNTHESIS',		104).
valid_abstract_section_header('CONSEQUENCES',		103).
valid_abstract_section_header('GOAL OF THIS STUDY',		103).
valid_abstract_section_header('IMPLICATIONS FOR FURTHER RESEARCH',		103).
valid_abstract_section_header('Method:',		103).
valid_abstract_section_header('PROTOCOL',		103).
valid_abstract_section_header('DESCRIPTION OF STUDY',		102).
valid_abstract_section_header('EFFECTS OF CHANGE',		102).
valid_abstract_section_header('EFFICACY',		102).
valid_abstract_section_header('EXPERIMENTAL DESIGN AND RESULTS',		102).
valid_abstract_section_header('METHOD AND MATERIAL',		102).
valid_abstract_section_header('STUDY GROUP',		102).
valid_abstract_section_header('TOXICITY',		102).
valid_abstract_section_header('TESTING',		101).
valid_abstract_section_header('DESIGN AND RESULTS',		100).
valid_abstract_section_header('DESIGN OF THE STUDY',		99).
valid_abstract_section_header('MAIN OUTCOME AND RESULTS',		99).
valid_abstract_section_header('SOURCE OF DATA',		99).
valid_abstract_section_header('TYPES OF PARTICIPANTS',		99).
valid_abstract_section_header('INVESTIGATION',		98).
valid_abstract_section_header('MAIN EXPOSURES',		98).
valid_abstract_section_header('SETTINGS AND PARTICIPANTS',		98).
valid_abstract_section_header('SUMMARY OF FINDINGS',		98).
valid_abstract_section_header('DESIGN/PARTICIPANTS',		97).
valid_abstract_section_header('MATERIALS AND RESULTS',		97).
valid_abstract_section_header('PROCESS',		97).
valid_abstract_section_header('VARIABLES MEASURED',		97).
valid_abstract_section_header('BACKGROUND/RATIONALE',		96).
valid_abstract_section_header('SECONDARY OUTCOME MEASURES',		96).
valid_abstract_section_header('AIM OF THE WORK',		95).
valid_abstract_section_header('AIMS/METHODS',		95).
valid_abstract_section_header('CONCLUSIONS AND CLINICAL IMPLICATIONS',		95).
valid_abstract_section_header('DIFFERENTIAL DIAGNOSIS',		95).
valid_abstract_section_header('RESEARCH OBJECTIVE',		95).
valid_abstract_section_header('FINDINGS/CONCLUSIONS',		94).
valid_abstract_section_header('STUDY LIMITATIONS',		94).
valid_abstract_section_header('Materials and Methods',		93).
valid_abstract_section_header('STATISTICAL METHODS',		93).
valid_abstract_section_header('STUDY DESIGN AND SUBJECTS',		93).
valid_abstract_section_header('THE PROBLEM',		93).
valid_abstract_section_header('CONTRIBUTING REVIEWERS',		92).
valid_abstract_section_header('DESIGN, PATIENTS AND MEASUREMENTS',		92).
valid_abstract_section_header('HIGHLIGHTS',		92).
valid_abstract_section_header('STRENGTH OF RECOMMENDATION GRADE',		92).
valid_abstract_section_header('STUDY HYPOTHESIS',		92).
valid_abstract_section_header('Trial Registration',		92).
valid_abstract_section_header('EXPERIMENTAL',		91).
valid_abstract_section_header('Exposures',		91).
valid_abstract_section_header('PACS',		91).
valid_abstract_section_header('PARTICIPANTS AND INTERVENTIONS',		91).
valid_abstract_section_header('PATIENTS, SUBJECTS AND METHODS',		91).
valid_abstract_section_header('PROGNOSIS',		91).
valid_abstract_section_header('HISTOLOGY',		90).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH POLICIES',		90).
valid_abstract_section_header('INTRODUCTION/PURPOSE',		90).
valid_abstract_section_header('METHODICAL INNOVATIONS',		90).
valid_abstract_section_header('OBJECTIVE OF PROGRAM',		90).
valid_abstract_section_header('OBJECTIVES AND BACKGROUND',		90).
valid_abstract_section_header('RELEVANCE TO PRACTICE',		90).
valid_abstract_section_header('STUDY OUTCOMES',		90).
valid_abstract_section_header('TRIAL DESIGN',		90).
valid_abstract_section_header('COPYRIGHT AND USAGE',		89).
valid_abstract_section_header('PATIENTS, MATERIALS AND METHODS',		89).
valid_abstract_section_header('STUDY SUBJECTS',		89).
valid_abstract_section_header('TRIALS REGISTRATION',		89).
valid_abstract_section_header('CONCLUSION AND RECOMMENDATION',		88).
valid_abstract_section_header('KEY MEASURES FOR IMPROVEMENT',		88).
valid_abstract_section_header('SAMPLE SIZE',		88).
valid_abstract_section_header('STRATEGIES FOR CHANGE',		88).
valid_abstract_section_header('SYMPTOMS',		88).
valid_abstract_section_header('DATA COLLECTION METHODS',		87).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, AND MEASUREMENTS',		87).
valid_abstract_section_header('REASONS FOR PERFORMING THE STUDY',		87).
valid_abstract_section_header('TITRE',		87).
valid_abstract_section_header('TRIAL REGISTRATION NUMBERS',		87).
valid_abstract_section_header('ANIMAL',		86).
valid_abstract_section_header('ANTECEDENTES',		86).
valid_abstract_section_header('DISCUSSION - CONCLUSION',		86).
valid_abstract_section_header('ORGANIZING CONSTRUCT',		86).
valid_abstract_section_header('PRACTICE IMPLICATION',		86).
valid_abstract_section_header('SOURCES USED',		86).
valid_abstract_section_header('STATEMENT OF CONTRIBUTION',		86).
valid_abstract_section_header('BACKGROUND/METHODS',		85).
valid_abstract_section_header('CONCLUSIONS/IMPORTANCE',		85).
valid_abstract_section_header('GOAL, SCOPE AND BACKGROUND',		85).
valid_abstract_section_header('MAIN MESSAGES',		85).
valid_abstract_section_header('SAFETY',		84).
valid_abstract_section_header('CONTEXT AND OBJECTIVES',		83).
valid_abstract_section_header('GUIDELINES',		83).
valid_abstract_section_header('LEARNING OBJECTIVE',		83).
valid_abstract_section_header('LITERATURE FINDINGS',		83).
valid_abstract_section_header('MAIN MEASURE',		83).
valid_abstract_section_header('MARCO DE REFERENCIA',		83).
valid_abstract_section_header('MATERIALS AND METHODOLOGY',		83).
valid_abstract_section_header('TAXONOMY',		83).
valid_abstract_section_header('AUDIENCE',		82).
valid_abstract_section_header('BACKGROUND, AIMS',		82).
valid_abstract_section_header('BENEFITS, HARMS AND COSTS',		82).
valid_abstract_section_header('CASE SUMMARIES',		82).
valid_abstract_section_header('CONTEXT/OBJECTIVE',		82).
valid_abstract_section_header('DATA EXTRACTION METHODS',		82).
valid_abstract_section_header('INTRODUCTION AND PURPOSE',		82).
valid_abstract_section_header('MAJOR FINDINGS',		82).
valid_abstract_section_header('METHODS & MATERIALS',		82).
valid_abstract_section_header('METHODS/PATIENTS',		82).
valid_abstract_section_header('FVIII',		81).
valid_abstract_section_header('LEVEL OF EVIDENCE III',		81).
valid_abstract_section_header('OUTCOMES MEASURES',		81).
valid_abstract_section_header('SUMMARY OF THE BACKGROUND DATA',		81).
valid_abstract_section_header('ANIMALS OR SAMPLE POPULATION',		80).
valid_abstract_section_header('BACKGROUND AND METHOD',		80).
valid_abstract_section_header('DESIGN/PATIENTS',		80).
valid_abstract_section_header('OBJECTIVES/METHODS',		80).
valid_abstract_section_header('PATIENT FINDINGS',		80).
valid_abstract_section_header('PRIMARY OBJECTIVES',		80).
valid_abstract_section_header('PUBLIC HEALTH ACTIONS',		80).
valid_abstract_section_header('Resultados',		80).
valid_abstract_section_header('SCHLUSSFOLGERUNG',		80).
valid_abstract_section_header('EXPERIENCE',		79).
valid_abstract_section_header('IMPLICATIONS FOR RESEARCH/PRACTICE',		79).
valid_abstract_section_header('PROGRAM EVALUATION',		79).
valid_abstract_section_header('PURPOSE OF THE RESEARCH',		79).
valid_abstract_section_header('ETIOLOGY',		78).
valid_abstract_section_header('INSTRUMENT',		78).
valid_abstract_section_header('INTRODUCTION/BACKGROUND',		78).
valid_abstract_section_header('ISSUES AND PURPOSE',		78).
valid_abstract_section_header('OVERALL ARTICLE OBJECTIVE',		78).
valid_abstract_section_header('PARTICIPANTS/METHODS',		78).
valid_abstract_section_header('POPULATION STUDIED',		78).
valid_abstract_section_header('DEFINITION',		77).
valid_abstract_section_header('IMPLICATION FOR PRACTICE',		77).
valid_abstract_section_header('MAIN OUTCOME AND MEASURES',		77).
valid_abstract_section_header('SOLUTION',		77).
valid_abstract_section_header('STUDY DESIGN AND SETTINGS',		77).
valid_abstract_section_header('CLINICALTRIALSGOV IDENTIFIER',		76).
valid_abstract_section_header('AIMS OF STUDY',		75).
valid_abstract_section_header('ANIMAL STUDIED',		75).
valid_abstract_section_header('DESIGN, SETTINGS, AND PARTICIPANTS',		75).
valid_abstract_section_header('PATIENT DESCRIPTION',		75).
valid_abstract_section_header('PURPOSE/AIM',		75).
valid_abstract_section_header('QUESTION UNDER STUDY',		75).
valid_abstract_section_header('UNIQUENESS',		75).
valid_abstract_section_header('CHIEF OUTCOME MEASURES',		74).
valid_abstract_section_header('INTRODUZIONE',		74).
valid_abstract_section_header('MODEL',		74).
valid_abstract_section_header('PARTICIPANTS AND DESIGN',		74).
valid_abstract_section_header('RISK FACTORS',		74).
valid_abstract_section_header('CONCLUSIONS/RECOMMENDATIONS',		73).
valid_abstract_section_header('LARGE SCALE DATA',		73).
valid_abstract_section_header('MAIN OUTCOME VARIABLE',		73).
valid_abstract_section_header('METHODS AND PARTICIPANTS',		73).
valid_abstract_section_header('STATISTICAL ANALYSIS PERFORMED',		73).
valid_abstract_section_header('SUBJECTS AND MEASUREMENTS',		73).
valid_abstract_section_header('THE AIM OF THIS STUDY',		73).
valid_abstract_section_header('WHAT IS NEW AND CONCLUSIONS',		73).
valid_abstract_section_header('BACKGROUND AND OVERVIEW',		72).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS AND INTERVENTIONS',		72).
valid_abstract_section_header('INTRODUCTION/OBJECTIVE',		72).
valid_abstract_section_header('LITERATURE SEARCH',		72).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOMES',		72).
valid_abstract_section_header('PRIMARY PRACTICE SETTING',		72).
valid_abstract_section_header('BACKGROUND AND METHODOLOGY',		71).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS',		71).
valid_abstract_section_header('DEVELOPMENT AND CONCLUSIONS',		71).
valid_abstract_section_header('KNOWLEDGE TRANSLATION',		71).
valid_abstract_section_header('OBJETIVE',		71).
valid_abstract_section_header('PARTICIPANTS AND INTERVENTION',		71).
valid_abstract_section_header('PREVENTION',		71).
valid_abstract_section_header('DESIGN/SUBJECTS',		70).
valid_abstract_section_header('INTERVENTIONS AND MAIN OUTCOME MEASURES',		70).
valid_abstract_section_header('STUDY SELECTIONS',		70).
valid_abstract_section_header('CASES REPORT',		69).
valid_abstract_section_header('HISTORY AND FINDINGS',		69).
valid_abstract_section_header('STUDY AIMS',		69).
valid_abstract_section_header('SUBJECTS AND INTERVENTIONS',		69).
valid_abstract_section_header('BACKGROUD',		68).
valid_abstract_section_header('CONCLUSION/IMPLICATIONS',		68).
valid_abstract_section_header('MAIN CONCLUSIONS',		68).
valid_abstract_section_header('PRIMARY PRACTICE SETTING(S)',		68).
valid_abstract_section_header('PURPOSE AND EXPERIMENTAL DESIGN',		68).
valid_abstract_section_header('DESIGN AND INTERVENTION',		67).
valid_abstract_section_header('PRINCIPAL OBSERVATIONS',		67).
valid_abstract_section_header('RESULTS & CONCLUSIONS',		67).
valid_abstract_section_header('SETTINGS & PARTICIPANTS',		67).
valid_abstract_section_header('SOCIAL IMPLICATIONS',		67).
valid_abstract_section_header('STARTING POINT',		67).
valid_abstract_section_header('SUMMARY OF KEY POINTS',		67).
valid_abstract_section_header('SUMMARY OF REPORT',		67).
valid_abstract_section_header('AUTHOR\'S CONCLUSIONS',		66).
valid_abstract_section_header('COMMENTARY ON',		66).
valid_abstract_section_header('MATERIAL AND METHODOLOGY',		66).
valid_abstract_section_header('METHOD AND PATIENTS',		66).
valid_abstract_section_header('METHODS AND MEASUREMENTS',		66).
valid_abstract_section_header('PARTICIPANT(S)',		66).
valid_abstract_section_header('PHENOMENON OF INTEREST',		66).
valid_abstract_section_header('PROJECT',		66).
valid_abstract_section_header('RECENT DEVELOPMENTS',		66).
valid_abstract_section_header('SUBJECTS, MATERIALS AND METHODS',		66).
valid_abstract_section_header('SUMMARY OF BACKGROUND',		66).
valid_abstract_section_header('TRIAL REGISTRATIONS',		66).
valid_abstract_section_header('CLINICAL NEED',		65).
valid_abstract_section_header('CONCLUSION:',		65).
valid_abstract_section_header('DATA SOURCES AND EXTRACTION',		65).
valid_abstract_section_header('OBJECTIVES:',		65).
valid_abstract_section_header('SETTING AND POPULATION',		65).
valid_abstract_section_header('STUDY METHODS',		65).
valid_abstract_section_header('WHAT IS NEW',		65).
valid_abstract_section_header('ACCESSIBLE SUMMARY',		64).
valid_abstract_section_header('ANIMALS, MATERIALS AND METHODS',		64).
valid_abstract_section_header('BACKROUND',		64).
valid_abstract_section_header('END POINT',		64).
valid_abstract_section_header('METHOD(S)',		64).
valid_abstract_section_header('METHODS AND MAIN RESULTS',		64).
valid_abstract_section_header('PATHOPHYSIOLOGY',		64).
valid_abstract_section_header('STUDY DESIGN/PATIENTS AND METHODS',		64).
valid_abstract_section_header('STUDY PERIOD',		64).
valid_abstract_section_header('DATA EXTRACTION AND ANALYSIS',		63).
valid_abstract_section_header('EXPECTED RESULTS',		63).
valid_abstract_section_header('HOST RANGE',		63).
valid_abstract_section_header('MAIN OUTCOMES MEASURE(S)',		63).
valid_abstract_section_header('PRESENTATION',		63).
valid_abstract_section_header('PURPOSE/QUESTION',		63).
valid_abstract_section_header('REVIEWER',		63).
valid_abstract_section_header('ANIMAL POPULATION',		62).
valid_abstract_section_header('BACKGROUND AND GOALS',		62).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS OF KEY FINDINGS',		62).
valid_abstract_section_header('DATA SOURCES AND STUDY SETTING',		62).
valid_abstract_section_header('DESIGN/METHOD',		62).
valid_abstract_section_header('DISEASE OVERVIEW',		62).
valid_abstract_section_header('INTRODUCTION AND BACKGROUND',		62).
valid_abstract_section_header('METHODOLOGY AND SAMPLE',		62).
valid_abstract_section_header('OBJECTIVES AND STUDY DESIGN',		62).
valid_abstract_section_header('STUDY SELECTION/DATA EXTRACTION',		62).
valid_abstract_section_header('AIM/BACKGROUND',		61).
valid_abstract_section_header('BACKGROUND AND PURPOSES',		61).
valid_abstract_section_header('DESCRIPTION OF TECHNIQUE',		61).
valid_abstract_section_header('DISSEMINATION',		61).
valid_abstract_section_header('ENDPOINTS',		61).
valid_abstract_section_header('GOAL OF THE STUDY',		61).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH CARE PROVISION AND USE',		61).
valid_abstract_section_header('LESSONS',		61).
valid_abstract_section_header('POPULATION AND METHOD',		61).
valid_abstract_section_header('SCOPO',		61).
valid_abstract_section_header('SPONSORS',		61).
valid_abstract_section_header('STUDY FUNDING AND COMPETING INTERESTS',		61).
valid_abstract_section_header('VALIDITY',		61).
valid_abstract_section_header('ACTIONS TAKEN',		60).
valid_abstract_section_header('BOTTOM LINE',		60).
valid_abstract_section_header('COMMUNITY CONTEXT',		60).
valid_abstract_section_header('EVALUATION METHOD',		60).
valid_abstract_section_header('MATERIALS/PATIENTS AND METHODS',		60).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTING AND METHODS',		60).
valid_abstract_section_header('PATHOGENESIS',		60).
valid_abstract_section_header('SPONSOR',		60).
valid_abstract_section_header('SUBJECTS & METHODS',		60).
valid_abstract_section_header('SUBJECTS/PATIENTS',		60).
valid_abstract_section_header('BACKGROUND AND RESEARCH OBJECTIVE',		59).
valid_abstract_section_header('CLINICAL SETTING',		59).
valid_abstract_section_header('CLINICAL TRIAL REGISTRY',		59).
valid_abstract_section_header('CLINICALTRIAL',		59).
valid_abstract_section_header('MAIN OUTCOME VARIABLES',		59).
valid_abstract_section_header('MAIN RESULT',		59).
valid_abstract_section_header('MATERIALI E METODI',		59).
valid_abstract_section_header('METHOD AND FINDINGS',		59).
valid_abstract_section_header('OBJECTIVE/METHOD',		59).
valid_abstract_section_header('CONCLUSIONS AND DISCUSSION',		58).
valid_abstract_section_header('CONCLUSIONS/INTERPRETATIONS',		58).
valid_abstract_section_header('DESIGN AND MAIN OUTCOME MEASURES',		58).
valid_abstract_section_header('PLACE',		58).
valid_abstract_section_header('PREMISE OF STUDY',		58).
valid_abstract_section_header('PROBLEM ADDRESSED',		58).
valid_abstract_section_header('RESEARCH DESIGN AND SUBJECTS',		58).
valid_abstract_section_header('WHAT IS ALREADY KNOWN',		58).
valid_abstract_section_header('EVIDENCE AND INFORMATION SOURCES',		57).
valid_abstract_section_header('GENETIC TOXICOLOGY',		57).
valid_abstract_section_header('MEAN OUTCOME MEASURES',		57).
valid_abstract_section_header('METHODOLOGY/RESULTS',		57).
valid_abstract_section_header('OBJECTIVE OF REVIEW',		57).
valid_abstract_section_header('RATIONAL',		57).
valid_abstract_section_header('RESEARCH METHOD/DESIGN',		57).
valid_abstract_section_header('STUDY PURPOSE',		57).
valid_abstract_section_header('CLINICAL IMPLICATION',		56).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR PRACTICE',		56).
valid_abstract_section_header('CONCLUSIONS AND PRACTICE IMPLICATIONS',		56).
valid_abstract_section_header('DISCUSSION & CONCLUSION',		56).
valid_abstract_section_header('MAIN CONTRIBUTION',		56).
valid_abstract_section_header('OBSERVATION PROCEDURES',		56).
valid_abstract_section_header('PRINCIPAL CONCLUSION',		56).
valid_abstract_section_header('REVIEW METHOD',		56).
valid_abstract_section_header('CLINICALTRIALSGOV',		55).
valid_abstract_section_header('DESIGN/SETTING/PATIENTS',		55).
valid_abstract_section_header('GOV IDENTIFIER',		55).
valid_abstract_section_header('RESULT AND DISCUSSION',		55).
valid_abstract_section_header('STUDY DESIGN, SAMPLES/MATERIALS, METHODS',		55).
valid_abstract_section_header('TYPE OF REVIEW',		55).
valid_abstract_section_header('CLINICAL REPORT',		54).
valid_abstract_section_header('CONCLUSIONS / IMPLICATIONS FOR PRACTICE',		54).
valid_abstract_section_header('CONDENSED ABSTRACT',		54).
valid_abstract_section_header('MAIN OUTCOMES MEASURE',		54).
valid_abstract_section_header('METHODS AND METHODS',		54).
valid_abstract_section_header('METHODS AND RESULT',		54).
valid_abstract_section_header('ORGANIZING FRAMEWORK',		54).
valid_abstract_section_header('OUTCOME VARIABLES',		54).
valid_abstract_section_header('PARTICIPANTS AND MEASUREMENTS',		54).
valid_abstract_section_header('PATIENTS, MATERIAL AND METHODS',		54).
valid_abstract_section_header('PROBLEM BEING ADDRESSED',		54).
valid_abstract_section_header('REZULTATI',		54).
valid_abstract_section_header('SAMPLE/SETTING',		54).
valid_abstract_section_header('SETTING/PATIENTS',		54).
valid_abstract_section_header('STUDIES',		54).
valid_abstract_section_header('CONCLUSIONS AND PRACTICAL IMPLICATIONS',		53).
valid_abstract_section_header('ISSUES ADDRESSED',		53).
valid_abstract_section_header('MAJOR CONCLUSION',		53).
valid_abstract_section_header('MEASURE',		53).
valid_abstract_section_header('METHODEN',		53).
valid_abstract_section_header('RESEARCH DESIGN AND METHOD',		53).
valid_abstract_section_header('SETTING/LOCATION',		53).
valid_abstract_section_header('TREATMENTS',		53).
valid_abstract_section_header('APPROCHE',		52).
valid_abstract_section_header('DATA COLLECTED',		52).
valid_abstract_section_header('ENFOQUE',		52).
valid_abstract_section_header('IMPLICATIONS FOR PUBLIC HEALTH PRACTICE',		52).
valid_abstract_section_header('INTRODUCTION AND METHODS',		52).
valid_abstract_section_header('INTRODUCTION/AIM',		52).
valid_abstract_section_header('MAIN OBSERVATION',		52).
valid_abstract_section_header('METHODS AND PROCEDURE',		52).
valid_abstract_section_header('OBJECTIVE AND STUDY DESIGN',		52).
valid_abstract_section_header('PATIENT GROUP',		52).
valid_abstract_section_header('POPULATION OR SAMPLE',		52).
valid_abstract_section_header('RECOMMENDATIONS AND OUTLOOK',		52).
valid_abstract_section_header('RESPONSE',		52).
valid_abstract_section_header('CASE DESCRIPTION AND METHODS',		51).
valid_abstract_section_header('DATA SUMMARY',		51).
valid_abstract_section_header('DESIGNS AND METHODS',		51).
valid_abstract_section_header('LECCIONES APRENDIDAS',		51).
valid_abstract_section_header('LIMITATIONS AND REASONS FOR CAUTION',		51).
valid_abstract_section_header('MARCO REGIONAL',		51).
valid_abstract_section_header('PRINCIPAL FINDING',		51).
valid_abstract_section_header('REFERENCE TEST',		51).
valid_abstract_section_header('SPECIMENS',		51).
valid_abstract_section_header('STUDY DESIGN AND METHOD',		51).
valid_abstract_section_header('SUBJECTS AND SETTINGS',		51).
valid_abstract_section_header('THE AIMS OF OUR STUDY WERE',		51).
valid_abstract_section_header('CAMBIOS IMPORTANTES',		50).
valid_abstract_section_header('CHANGEMENTS SIGNIFICATIFS',		50).
valid_abstract_section_header('CLINICAL TRIALS REGISTRATION NUMBER',		50).
valid_abstract_section_header('CONCLUSIONS AND GENERAL SIGNIFICANCE',		50).
valid_abstract_section_header('DESCRIPTION OF THE SYSTEM',		50).
valid_abstract_section_header('ENVIRONNEMENT LOCAL',		50).
valid_abstract_section_header('GOALS AND BACKGROUND',		50).
valid_abstract_section_header('MAIN FEATURES',		50).
valid_abstract_section_header('METHODOLOGY/ PRINCIPAL FINDINGS',		50).
valid_abstract_section_header('METODE',		50).
valid_abstract_section_header('PATIENTS-METHODS',		50).
valid_abstract_section_header('QUESTION/PURPOSES',		50).
valid_abstract_section_header('RECOMMENDATION AND OUTLOOK',		50).
valid_abstract_section_header('RESEARCH METHOD',		50).
valid_abstract_section_header('RISK STRATIFICATION',		50).
valid_abstract_section_header('BACKGROUND AND CONTEXT',		49).
valid_abstract_section_header('BACKGROUND:',		49).
valid_abstract_section_header('CLINICAL TRIAL',		49).
valid_abstract_section_header('CONCLUSIONS, SIGNIFICANCE AND IMPACT OF THE STUDY',		49).
valid_abstract_section_header('MAIN OBJECTIVE',		49).
valid_abstract_section_header('PATIENT CHARACTERISTICS',		49).
valid_abstract_section_header('SUBJECT(S)',		49).
valid_abstract_section_header('THE AIM OF STUDY',		49).
valid_abstract_section_header('CONTACTS',		48).
valid_abstract_section_header('DATA SOURCE/STUDY SETTING',		48).
valid_abstract_section_header('DESIGN, SETTING, AND POPULATION',		48).
valid_abstract_section_header('FINDINGS AND DISCUSSION',		48).
valid_abstract_section_header('INDICATION',		48).
valid_abstract_section_header('MATERIAL-METHODS',		48).
valid_abstract_section_header('MESURES',		48).
valid_abstract_section_header('METODI',		48).
valid_abstract_section_header('PRESENTATION OF THE CASE',		48).
valid_abstract_section_header('RESULTS AND RECOMMENDATIONS',		48).
valid_abstract_section_header('SELECTION OF STUDIES',		48).
valid_abstract_section_header('SETTING AND METHODS',		48).
valid_abstract_section_header('TECHNICAL NOTE',		48).
valid_abstract_section_header('VETERINARY DATA SYNTHESIS',		48).
valid_abstract_section_header('DATA SOURCES AND METHODS',		47).
valid_abstract_section_header('DESIGN & METHODS',		47).
valid_abstract_section_header('FINDINGS/RESULTS',		47).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE/RESEARCH',		47).
valid_abstract_section_header('INDEX TEST',		47).
valid_abstract_section_header('Objetivo',		47).
valid_abstract_section_header('RISK-ADAPTED THERAPY',		47).
valid_abstract_section_header('TRIAL NUMBER',		47).
valid_abstract_section_header('AIM OF WORK',		46).
valid_abstract_section_header('BACKGOUND',		46).
valid_abstract_section_header('CONCLUSION/CLINICAL RELEVANCE',		46).
valid_abstract_section_header('DESCRIPTIVE DATA',		46).
valid_abstract_section_header('DIAGNOSTICS',		46).
valid_abstract_section_header('Data Sources',		46).
valid_abstract_section_header('EXCLUSIONS',		46).
valid_abstract_section_header('FINANCIAL DISCLOSURES',		46).
valid_abstract_section_header('INFORMATION SOURCES',		46).
valid_abstract_section_header('Introduction:',		46).
valid_abstract_section_header('JUSTIFICATION',		46).
valid_abstract_section_header('MAIN OUTCOMES MEASURED',		46).
valid_abstract_section_header('OBSERVATIONS AND RESULTS',		46).
valid_abstract_section_header('RESEARCH',		46).
valid_abstract_section_header('STUDY FINDING',		46).
valid_abstract_section_header('AIMS/OBJECTIVES',		45).
valid_abstract_section_header('ASSESSMENTS',		45).
valid_abstract_section_header('BENEFITS',		45).
valid_abstract_section_header('CONCEPT',		45).
valid_abstract_section_header('Conclusions:',		45).
valid_abstract_section_header('FINDINGS AND CONCLUSION',		45).
valid_abstract_section_header('FINDINGS AND OUTCOMES',		45).
valid_abstract_section_header('HUMAN DATA SYNTHESIS',		45).
valid_abstract_section_header('INTERPRETATION/CONCLUSION',		45).
valid_abstract_section_header('KEY CONCLUSION',		45).
valid_abstract_section_header('MAIN OUTCOME MEASURES(S)',		45).
valid_abstract_section_header('MATERIAL AND RESULTS',		45).
valid_abstract_section_header('MATERIALS, METHODS AND RESULTS',		45).
valid_abstract_section_header('METHOD AND DESIGN',		45).
valid_abstract_section_header('METHOD/RESULTS',		45).
valid_abstract_section_header('OUTLOOK',		45).
valid_abstract_section_header('PURPOSE AND METHOD',		45).
valid_abstract_section_header('PURPOSE OF WORK',		45).
valid_abstract_section_header('PURPOSE/METHOD',		45).
valid_abstract_section_header('Primary Funding Source',		45).
valid_abstract_section_header('QUESTIONS/PURPOSE',		45).
valid_abstract_section_header('STANDARDS',		45).
valid_abstract_section_header('VIEWPOINT',		45).
valid_abstract_section_header('CONCLUSIONS/ SIGNIFICANCE',		44).
valid_abstract_section_header('DEFINITIONS',		44).
valid_abstract_section_header('FUTURE WORK',		44).
valid_abstract_section_header('METHOD OF APPROACH',		44).
valid_abstract_section_header('NO LEVEL ASSIGNED',		44).
valid_abstract_section_header('RESPONDENTS',		44).
valid_abstract_section_header('RESULTS AND ANALYSIS',		44).
valid_abstract_section_header('REVIEW STRATEGY',		44).
valid_abstract_section_header('SAMPLE AND SETTING',		44).
valid_abstract_section_header('SECONDARY OBJECTIVES',		44).
valid_abstract_section_header('STUDY DESIGN/MATERIAL AND METHODS',		44).
valid_abstract_section_header('SUMMARY OBJECTIVES',		44).
valid_abstract_section_header('SUMMARY STATEMENT',		44).
valid_abstract_section_header('DESIGN SETTING AND PARTICIPANTS',		43).
valid_abstract_section_header('MATERIAL OR SUBJECTS',		43).
valid_abstract_section_header('PARTICIPANTS AND SETTINGS',		43).
valid_abstract_section_header('PARTICIPANTS, SETTING, METHODS',		43).
valid_abstract_section_header('PATIENTS/MATERIAL AND METHODS',		43).
valid_abstract_section_header('RESULTS OF DATA ANALYSIS',		43).
valid_abstract_section_header('SECONDARY OBJECTIVE',		43).
valid_abstract_section_header('STUDY FUNDING/COMPETING INTEREST',		43).
valid_abstract_section_header('SURGERY',		43).
valid_abstract_section_header('USEFUL WEBSITES',		43).
valid_abstract_section_header('APPLICATION TO CLINICAL PRACTICE',		42).
valid_abstract_section_header('BASIS',		42).
valid_abstract_section_header('DESIGN AND DATA SOURCES',		42).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS AND MEASUREMENTS',		42).
valid_abstract_section_header('FINAL DIAGNOSIS',		42).
valid_abstract_section_header('PLACE IN THERAPY',		42).
valid_abstract_section_header('POLICY IMPLICATIONS',		42).
valid_abstract_section_header('PROSPERO REGISTRATION NUMBER',		42).
valid_abstract_section_header('PURPOSE/OBJECTIVE(S)',		42).
valid_abstract_section_header('SCHLUSSFOLGERUNGEN',		42).
valid_abstract_section_header('SETTINGS AND DESIGNS',		42).
valid_abstract_section_header('STRENGTH OF RECOMMENDATION',		42).
valid_abstract_section_header('STUDY VARIABLES',		42).
valid_abstract_section_header('Study Selection',		42).
valid_abstract_section_header('WHAT IS KNOWN AND WHAT THIS PAPER ADDS',		42).
valid_abstract_section_header('BACKGROUND & PURPOSE',		41).
valid_abstract_section_header('Background:',		41).
valid_abstract_section_header('CASES PRESENTATION',		41).
valid_abstract_section_header('CLASSIFICATION',		41).
valid_abstract_section_header('CLINICAL SCENARIO',		41).
valid_abstract_section_header('DEPENDENT VARIABLES',		41).
valid_abstract_section_header('INVESTIGATIONS AND DIAGNOSIS',		41).
valid_abstract_section_header('LIMITATIONS, REASON FOR CAUTION',		41).
valid_abstract_section_header('MAIN RESEARCH VARIABLE',		41).
valid_abstract_section_header('PATIENTS/MATERIALS/METHODS',		41).
valid_abstract_section_header('PRESENTATION OF CASES',		41).
valid_abstract_section_header('STRUCTURED DIGITAL ABSTRACT',		41).
valid_abstract_section_header('STUDY DESIGN AND PATIENTS',		41).
valid_abstract_section_header('STUDY DESIGNS',		41).
valid_abstract_section_header('STUDY INCLUSION AND EXCLUSION CRITERIA',		41).
valid_abstract_section_header('VARIABLES OF INTEREST',		41).
valid_abstract_section_header('BACKGROUND, AIMS AND SCOPE',		40).
valid_abstract_section_header('CLINICAL RESULTS',		40).
valid_abstract_section_header('DISEASE SYMPTOMS',		40).
valid_abstract_section_header('IN VITRO STUDIES',		40).
valid_abstract_section_header('MATERIAL/METHOD',		40).
valid_abstract_section_header('MEASURES OF OUTCOME',		40).
valid_abstract_section_header('OBJECTIVE/DESIGN',		40).
valid_abstract_section_header('OBJECTIVES/BACKGROUND',		40).
valid_abstract_section_header('PURPOSE OF THE REPORT',		40).
valid_abstract_section_header('SCOPE OF THE REVIEW',		40).
valid_abstract_section_header('TYPES OF STUDIES',		40).
valid_abstract_section_header('WHAT IS KNOWN AND OBJECTIVES',		40).
valid_abstract_section_header('BACKGROUND AND AIM OF STUDY',		39).
valid_abstract_section_header('BASIC METHODS',		39).
valid_abstract_section_header('DISCLAIMER',		39).
valid_abstract_section_header('ETHICS',		39).
valid_abstract_section_header('EXPERIMENTAL VARIABLE',		39).
valid_abstract_section_header('INTERPRETATIONS AND CONCLUSIONS',		39).
valid_abstract_section_header('MAIN POINTS',		39).
valid_abstract_section_header('MATERIALS AND SURGICAL TECHNIQUE',		39).
valid_abstract_section_header('PARTICIPANTS AND METHOD',		39).
valid_abstract_section_header('RESEARCH METHODOLOGY',		39).
valid_abstract_section_header('SHORT SUMMARY',		39).
valid_abstract_section_header('AIM(S)',		38).
valid_abstract_section_header('BACKGROUNDS AND OBJECTIVES',		38).
valid_abstract_section_header('CLINICAL PROCEDURE',		38).
valid_abstract_section_header('CLINICAL TRIALS',		38).
valid_abstract_section_header('CONCLUSION AND IMPLICATION',		38).
valid_abstract_section_header('CONDENSATION',		38).
valid_abstract_section_header('Conclusiones',		38).
valid_abstract_section_header('Design',		38).
valid_abstract_section_header('ETHNOPHARMACOLOGY RELEVANCE',		38).
valid_abstract_section_header('EXAMINATIONS',		38).
valid_abstract_section_header('INTRODUCTIONS',		38).
valid_abstract_section_header('METHODS AND MAIN OUTCOME MEASURES',		38).
valid_abstract_section_header('RESEARCH OBJECTIVES',		38).
valid_abstract_section_header('SETTING AND SAMPLE',		38).
valid_abstract_section_header('SUBJECT/METHODS',		38).
valid_abstract_section_header('SUBJECTS AND INTERVENTION',		38).
valid_abstract_section_header('AIM/OBJECTIVE',		37).
valid_abstract_section_header('CAPSULE',		37).
valid_abstract_section_header('COSTS',		37).
valid_abstract_section_header('DATA RESOURCES',		37).
valid_abstract_section_header('DATA SYNTHESIS AND CONCLUSIONS',		37).
valid_abstract_section_header('DURATION',		37).
valid_abstract_section_header('EXPERIMENTAL PROCEDURES',		37).
valid_abstract_section_header('GOAL OF WORK',		37).
valid_abstract_section_header('INDEX TESTS',		37).
valid_abstract_section_header('KEY FINDING',		37).
valid_abstract_section_header('MAIN BODY',		37).
valid_abstract_section_header('MAIN VARIABLES OF INTEREST',		37).
valid_abstract_section_header('MEASUREMENTS/RESULTS',		37).
valid_abstract_section_header('METODO',		37).
valid_abstract_section_header('NEW METHODS',		37).
valid_abstract_section_header('OBJECTIVES OF THE STUDY',		37).
valid_abstract_section_header('OBJETIVES',		37).
valid_abstract_section_header('PATHOLOGICAL FINDINGS',		37).
valid_abstract_section_header('PATIENT SELECTION',		37).
valid_abstract_section_header('PURPOSE AND BACKGROUND',		37).
valid_abstract_section_header('PURPOSE:',		37).
valid_abstract_section_header('RATIONAL AND OBJECTIVES',		37).
valid_abstract_section_header('STUDY DESIGN AND PARTICIPANTS',		37).
valid_abstract_section_header('BACKGROUNDS & AIMS',		36).
valid_abstract_section_header('CLINICAL TRIAL INFORMATION',		36).
valid_abstract_section_header('CONCLUSION AND CLINICAL IMPLICATIONS',		36).
valid_abstract_section_header('DESIGN, SETTINGS, PARTICIPANTS, & MEASUREMENTS',		36).
valid_abstract_section_header('MAIN OUTCOME MEASUREMENT(S)',		36).
valid_abstract_section_header('MAIN RESULTS AND ROLE OF CHANCE',		36).
valid_abstract_section_header('METHODOLOGICAL PROCEDURES',		36).
valid_abstract_section_header('METHODS AND ANALYSES',		36).
valid_abstract_section_header('OBJECTIVE AND PARTICIPANTS',		36).
valid_abstract_section_header('OUTCOMES OF INTEREST',		36).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTINGS, METHODS',		36).
valid_abstract_section_header('PURPOSES AND CLINICAL RELEVANCE',		36).
valid_abstract_section_header('RESULTS(S)',		36).
valid_abstract_section_header('SAMPLE AND METHOD',		36).
valid_abstract_section_header('AIM OF THE PAPER',		35).
valid_abstract_section_header('CASE 2',		35).
valid_abstract_section_header('CLINICAL IMPORTANCE',		35).
valid_abstract_section_header('CLINICAL TRIALS NUMBER',		35).
valid_abstract_section_header('DESIGN, MATERIALS AND METHODS',		35).
valid_abstract_section_header('INTERVENTION AND MEASUREMENTS',		35).
valid_abstract_section_header('METHOD AND RESULT',		35).
valid_abstract_section_header('METODOS',		35).
valid_abstract_section_header('Observations',		35).
valid_abstract_section_header('PATIENT AND RESULTS',		35).
valid_abstract_section_header('PERIOD',		35).
valid_abstract_section_header('SETTINGS AND SUBJECTS',		35).
valid_abstract_section_header('SUBJECTS AND MATERIALS',		35).
valid_abstract_section_header('SYSTEMATIC REVIEW REGISTRATION NUMBER',		35).
valid_abstract_section_header('THE PURPOSE OF THE STUDY',		35).
valid_abstract_section_header('ACQUISITION OF EVIDENCE',		34).
valid_abstract_section_header('BACKGROUND & PROBLEMS',		34).
valid_abstract_section_header('BACKGROUND AND SETTING',		34).
valid_abstract_section_header('BACKGROUND AND SIGNIFICANCE',		34).
valid_abstract_section_header('CLINICAL CARE RELEVANCE',		34).
valid_abstract_section_header('CLINICAL ISSUE',		34).
valid_abstract_section_header('COURSE',		34).
valid_abstract_section_header('DIAGNOSIS AND TREATMENT',		34).
valid_abstract_section_header('INSTRUMENTATION',		34).
valid_abstract_section_header('INTRODUCTION/OBJECTIVES',		34).
valid_abstract_section_header('Limitation',		34).
valid_abstract_section_header('MAJOR OUTCOME MEASURES',		34).
valid_abstract_section_header('OPERATIVE TECHNIQUE',		34).
valid_abstract_section_header('OPINION/FEEDBACK',		34).
valid_abstract_section_header('PATIENTS AND STUDY DESIGN',		34).
valid_abstract_section_header('RESOLUTION',		34).
valid_abstract_section_header('REVIEW OF THE LITERATURE',		34).
valid_abstract_section_header('SELECTION PROCEDURES',		34).
valid_abstract_section_header('STUDY POPULATION AND METHODS',		34).
valid_abstract_section_header('SUMMARY OBJECTIVE',		34).
valid_abstract_section_header('ACCME ACCREDITATION',		33).
valid_abstract_section_header('BACKGROUND AND GOAL',		33).
valid_abstract_section_header('BACKGROUND TO THE DEBATE',		33).
valid_abstract_section_header('CONCLUSIONS & CLINICAL RELEVANCE',		33).
valid_abstract_section_header('DATA ABSTRACTION',		33).
valid_abstract_section_header('DESIGN AND OUTCOME MEASURES',		33).
valid_abstract_section_header('INITIAL ASSESSMENT',		33).
valid_abstract_section_header('INTERVENTION AND OUTCOMES',		33).
valid_abstract_section_header('LIMITS',		33).
valid_abstract_section_header('PATIENTS AND MAIN OUTCOME MEASURES',		33).
valid_abstract_section_header('SPECIALTY',		33).
valid_abstract_section_header('STRATEGY',		33).
valid_abstract_section_header('VIRTUAL SLIDE',		33).
valid_abstract_section_header('ADVANCES IN KNOWLEDGE AND IMPLICATIONS FOR PATIENT CARE',		32).
valid_abstract_section_header('CC/CT',		32).
valid_abstract_section_header('CLINICAL STUDIES',		32).
valid_abstract_section_header('DATA SOURCES AND REVIEW METHODS',		32).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS & MEASUREMENTS',		32).
valid_abstract_section_header('DISCUSSIONS AND CONCLUSIONS',		32).
valid_abstract_section_header('EXECUTIVE SUMMARY',		32).
valid_abstract_section_header('EXPERIMENT',		32).
valid_abstract_section_header('IMPLICATION FOR NURSING MANAGEMENT',		32).
valid_abstract_section_header('IMPLICATIONS FOR FUTURE RESEARCH',		32).
valid_abstract_section_header('ISRCTN',		32).
valid_abstract_section_header('SUBJECTS/SETTINGS',		32).
valid_abstract_section_header('AIMS/HYPOTHESES',		31).
valid_abstract_section_header('CHOICE OF SOLUTION',		31).
valid_abstract_section_header('CONCLUSIONS FOR PRACTICE',		31).
valid_abstract_section_header('DATA EXTRACTION AND DATA SYNTHESIS',		31).
valid_abstract_section_header('DESIGN & SETTING',		31).
valid_abstract_section_header('END POINTS',		31).
valid_abstract_section_header('FUNDING, COMPETING INTERESTS, DATA SHARING',		31).
valid_abstract_section_header('HYPOTHESIS/OBJECTIVE',		31).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH POLICY',		31).
valid_abstract_section_header('INFECTION',		31).
valid_abstract_section_header('INTERVENTION AND MAIN OUTCOME MEASURES',		31).
valid_abstract_section_header('INTERVENTIONS(S)',		31).
valid_abstract_section_header('INTERVENTIONS/METHODS',		31).
valid_abstract_section_header('MAIN COMPONENTS OF PROGRAM',		31).
valid_abstract_section_header('MATERIAL, METHODS AND RESULTS',		31).
valid_abstract_section_header('MATERIALS, SETTING, METHODS',		31).
valid_abstract_section_header('METHOD AND ANALYSIS',		31).
valid_abstract_section_header('OBSERVATION PROCEDURE',		31).
valid_abstract_section_header('OUTCOME MEASUREMENT',		31).
valid_abstract_section_header('OUTCOMES ASSESSMENT',		31).
valid_abstract_section_header('PATIENT/METHODS',		31).
valid_abstract_section_header('PATIENTS AND CONTROLS',		31).
valid_abstract_section_header('PROSPERO',		31).
valid_abstract_section_header('PURPOSE AND DESIGN',		31).
valid_abstract_section_header('SUMMARY OF CASE',		31).
valid_abstract_section_header('TARGET',		31).
valid_abstract_section_header('AIM OF DATABASE',		30).
valid_abstract_section_header('AIMS AND METHOD',		30).
valid_abstract_section_header('BASIC/CLINICAL SCIENCE ADVANCES',		30).
valid_abstract_section_header('CEL',		30).
valid_abstract_section_header('DATA RETRIEVAL',		30).
valid_abstract_section_header('EXPERIMENTAL APPROACHES',		30).
valid_abstract_section_header('LEVEL OF EVIDENCE II',		30).
valid_abstract_section_header('MAIN OUTCOME MEASURED',		30).
valid_abstract_section_header('MAIN VARIABLES STUDIED',		30).
valid_abstract_section_header('MATERIAL & METHOD',		30).
valid_abstract_section_header('MEASUREMENTS AND METHODS',		30).
valid_abstract_section_header('OBJECTIVE AND RATIONALE',		30).
valid_abstract_section_header('OBJECTIVE/AIM',		30).
valid_abstract_section_header('OUTLINE OF CASES',		30).
valid_abstract_section_header('POPULATIONS',		30).
valid_abstract_section_header('PRINCIPAL MEASUREMENTS',		30).
valid_abstract_section_header('PROTOCOL REGISTRATION',		30).
valid_abstract_section_header('QUALITY IMPROVEMENT PLAN',		30).
valid_abstract_section_header('RESEARCH DESIGN/METHODS',		30).
valid_abstract_section_header('SCOPE AND CONCLUSIONS',		30).
valid_abstract_section_header('STUDY ANSWER AND LIMITATIONS',		30).
valid_abstract_section_header('THE TITLE COMPOUND (SYSTEMATIC NAME',		30).
valid_abstract_section_header('ADVANTAGES',		29).
valid_abstract_section_header('ARGUMENT',		29).
valid_abstract_section_header('BACKGROUND AND PURPOSE OF THE STUDY',		29).
valid_abstract_section_header('BACKGROUND AND SCOPE',		29).
valid_abstract_section_header('BACKGROUND/STUDY CONTEXT',		29).
valid_abstract_section_header('CASE PRESENTATION AND INTERVENTION',		29).
valid_abstract_section_header('CEL PRACY',		29).
valid_abstract_section_header('CHALLENGES',		29).
valid_abstract_section_header('CLINICAL COURSE',		29).
valid_abstract_section_header('CLINICAL TRIALS REGISTRY',		29).
valid_abstract_section_header('CONCLUSION AND PRACTICE IMPLICATIONS',		29).
valid_abstract_section_header('CONCLUSION AND SCIENTIFIC SIGNIFICANCE',		29).
valid_abstract_section_header('CONCLUSIONS AND IMPACT',		29).
valid_abstract_section_header('CONCLUSIONS AND PERSPECTIVES',		29).
valid_abstract_section_header('DESIGN, PARTICIPANTS, AND SETTING',		29).
valid_abstract_section_header('EVIDENCE ACQUISITIONS',		29).
valid_abstract_section_header('Evidence Review',		29).
valid_abstract_section_header('FUTURE PROSPECTS',		29).
valid_abstract_section_header('Findings',		29).
valid_abstract_section_header('HYPOTHESIS/PROBLEM',		29).
valid_abstract_section_header('INCLUSION CRITERIA TYPES OF PARTICIPANTS',		29).
valid_abstract_section_header('INTRODUCTION AND METHOD',		29).
valid_abstract_section_header('INTRODUCTION OR BACKGROUND',		29).
valid_abstract_section_header('KEYS TO SUCCESS',		29).
valid_abstract_section_header('LEVEL III',		29).
valid_abstract_section_header('Level of Evidence',		29).
valid_abstract_section_header('MATERIALS, METHODS, AND RESULTS',		29).
valid_abstract_section_header('Measurements',		29).
valid_abstract_section_header('OBIETTIVO',		29).
valid_abstract_section_header('OBJECTIVE:',		29).
valid_abstract_section_header('OUTCOME MEASURES AND RESULTS',		29).
valid_abstract_section_header('Objetivos',		29).
valid_abstract_section_header('POPULATION AND SETTING',		29).
valid_abstract_section_header('PRIMARY PRACTICE SETTINGS',		29).
valid_abstract_section_header('PROBLEMS',		29).
valid_abstract_section_header('SEARCH STRATEGIES',		29).
valid_abstract_section_header('SUBJECTS, MATERIAL AND METHODS',		29).
valid_abstract_section_header('Setting',		29).
valid_abstract_section_header('TESTS',		29).
valid_abstract_section_header('THE OBJECTIVE',		29).
valid_abstract_section_header('THE TECHNOLOGY',		29).
valid_abstract_section_header('WIDER IMPLICATIONS',		29).
valid_abstract_section_header('DIAGNOSES',		28).
valid_abstract_section_header('Data Extraction and Synthesis',		28).
valid_abstract_section_header('EXPERIMENTAL INTERVENTIONS',		28).
valid_abstract_section_header('IMPLICATIONS STATEMENT',		28).
valid_abstract_section_header('MEASURES AND RESULTS',		28).
valid_abstract_section_header('METHODS AND OBJECTIVES',		28).
valid_abstract_section_header('METHODS/ANALYSIS',		28).
valid_abstract_section_header('Objectives:',		28).
valid_abstract_section_header('PATIENTS, METHODS AND RESULTS',		28).
valid_abstract_section_header('PRELIMINARY STUDIES',		28).
valid_abstract_section_header('PROPOSAL',		28).
valid_abstract_section_header('RELEVANCE FOR CLINICAL PRACTICE',		28).
valid_abstract_section_header('RESULTS AND SIGNIFICANCE',		28).
valid_abstract_section_header('STATEMENT',		28).
valid_abstract_section_header('STRATEGIES FOR IMPROVEMENT',		28).
valid_abstract_section_header('TREATMENT AND CLINICAL COURSE',		28).
valid_abstract_section_header('ANALYSIS OF RESULTS',		27).
valid_abstract_section_header('CASE 1',		27).
valid_abstract_section_header('CASES REPORTS',		27).
valid_abstract_section_header('DATA COLLECTION/EXTRACTION',		27).
valid_abstract_section_header('FACTORS',		27).
valid_abstract_section_header('FUTURE PLANS',		27).
valid_abstract_section_header('INNOVATION AND CONCLUSIONS',		27).
valid_abstract_section_header('MAIN LIMITATIONS',		27).
valid_abstract_section_header('MAIN PURPOSE',		27).
valid_abstract_section_header('NURSING IMPLICATIONS',		27).
valid_abstract_section_header('OBJECTIVES AND METHOD',		27).
valid_abstract_section_header('PATIENTS AND OTHERS PARTICIPANTS',		27).
valid_abstract_section_header('PATIENTS/SUBJECTS',		27).
valid_abstract_section_header('PERIOD COVERED',		27).
valid_abstract_section_header('PERSPECTIVES AND CONCLUSIONS',		27).
valid_abstract_section_header('PLACE AND DURATION',		27).
valid_abstract_section_header('PRACTICE GUIDELINE',		27).
valid_abstract_section_header('RESULTS AND MAJOR CONCLUSION',		27).
valid_abstract_section_header('SAMPLING',		27).
valid_abstract_section_header('SELECTED HIGHLIGHTS',		27).
valid_abstract_section_header('SETTINGS/PARTICIPANTS',		27).
valid_abstract_section_header('STUDY RESULTS',		27).
valid_abstract_section_header('TECHNIQUES',		27).
valid_abstract_section_header('WHAT THIS PAPER ADDS',		27).
valid_abstract_section_header('AIM/METHODS',		26).
valid_abstract_section_header('AREAS COVERED IN THE REVIEW',		26).
valid_abstract_section_header('CASE STUDIES',		26).
valid_abstract_section_header('CLINICAL INTRODUCTION',		26).
valid_abstract_section_header('CLINICAL TRIALS IDENTIFIER',		26).
valid_abstract_section_header('CONCLUSION/DISCUSSION',		26).
valid_abstract_section_header('CONSENSUS STATEMENT',		26).
valid_abstract_section_header('CURRENT STATUS',		26).
valid_abstract_section_header('DEMOGRAPHICS',		26).
valid_abstract_section_header('DESIGN CLASSIFICATION',		26).
valid_abstract_section_header('FACTOR',		26).
valid_abstract_section_header('GOALS OF THE WORK',		26).
valid_abstract_section_header('HOW TO CITE THIS ARTICLE',		26).
valid_abstract_section_header('KEY LIMITATIONS',		26).
valid_abstract_section_header('MEASUREMENTS AND INTERVENTIONS',		26).
valid_abstract_section_header('MISE EN CONTEXTE',		26).
valid_abstract_section_header('OBSERVATION AND RESULTS',		26).
valid_abstract_section_header('PATIENT CONCERNS',		26).
valid_abstract_section_header('PATIENTS AND METHODOLOGY',		26).
valid_abstract_section_header('PHENOMENA OF INTEREST',		26).
valid_abstract_section_header('PRECIS',		26).
valid_abstract_section_header('PREMISE',		26).
valid_abstract_section_header('PREVALENCE',		26).
valid_abstract_section_header('PROBLEM STATEMENT',		26).
valid_abstract_section_header('PURPOSE/AIM OF THE STUDY',		26).
valid_abstract_section_header('RATIONALE AND AIM',		26).
valid_abstract_section_header('RESEARCH FINDINGS',		26).
valid_abstract_section_header('SIGNIFICANCE OF THE STUDY',		26).
valid_abstract_section_header('STUDY METHOD',		26).
valid_abstract_section_header('SUMMARY OF COMMENT',		26).
valid_abstract_section_header('TRIAL REGISTRATION DATE',		26).
valid_abstract_section_header('AIMS & OBJECTIVES',		25).
valid_abstract_section_header('AIMS AND/OR HYPOTHESIS',		25).
valid_abstract_section_header('Aim:',		25).
valid_abstract_section_header('BACK GROUND',		25).
valid_abstract_section_header('CONCLUSION & INFERENCES',		25).
valid_abstract_section_header('CONCLUSION AND POTENTIAL RELEVANCE',		25).
valid_abstract_section_header('CONCLUSIONS AND INFERENCES',		25).
valid_abstract_section_header('CONFERENCE PROCESS',		25).
valid_abstract_section_header('DESIGN AND MEASURES',		25).
valid_abstract_section_header('DESIGN AND METHODOLOGY',		25).
valid_abstract_section_header('DESIGN, PATIENTS AND METHODS',		25).
valid_abstract_section_header('ENVIRONMENT',		25).
valid_abstract_section_header('HARMS',		25).
valid_abstract_section_header('INTERVENTIONS AND OUTCOME MEASURES',		25).
valid_abstract_section_header('INTRODUCTION/AIMS',		25).
valid_abstract_section_header('KEY STUDY FACTOR',		25).
valid_abstract_section_header('LABORATORY FINDINGS',		25).
valid_abstract_section_header('MAIN RESULTS AND CONCLUSIONS',		25).
valid_abstract_section_header('METHOD AND PROCEDURES',		25).
valid_abstract_section_header('METHODOLOGY/PRINCIPAL',		25).
valid_abstract_section_header('OBJECTIVES/HYPOTHESES',		25).
valid_abstract_section_header('PHYSICAL EXAMINATION',		25).
valid_abstract_section_header('SETTINGS AND PATIENTS',		25).
valid_abstract_section_header('STUDIES REVIEWED',		25).
valid_abstract_section_header('STUDY DESIGN AND RESULTS',		25).
valid_abstract_section_header('SURVEY',		25).
valid_abstract_section_header('THE CONCLUSION',		25).
valid_abstract_section_header('THERAPY AND COURSE',		25).
valid_abstract_section_header('ACTION',		24).
valid_abstract_section_header('AIM OF INVESTIGATION',		24).
valid_abstract_section_header('CONCLUDING REMARKS',		24).
valid_abstract_section_header('CONCLUSIONS/APPLICATIONS',		24).
valid_abstract_section_header('DATA AND METHOD',		24).
valid_abstract_section_header('DATA EXTRACTION/SYNTHESIS',		24).
valid_abstract_section_header('DESCRIPTION OF THE PROJECT',		24).
valid_abstract_section_header('DESIGN, SETTING, SUBJECTS',		24).
valid_abstract_section_header('ETHICAL CONSIDERATION',		24).
valid_abstract_section_header('IDENTIFICATION',		24).
valid_abstract_section_header('IN PRACTICE',		24).
valid_abstract_section_header('KEY POINTS',		24).
valid_abstract_section_header('LINKED ARTICLE',		24).
valid_abstract_section_header('MATERIAL-METHOD',		24).
valid_abstract_section_header('MATERIAL/SUBJECTS AND METHODS',		24).
valid_abstract_section_header('MATERIALS AND METHODS AND RESULTS',		24).
valid_abstract_section_header('MATERIALS-METHODS',		24).
valid_abstract_section_header('METHODS AND SETTING',		24).
valid_abstract_section_header('MOTIVATIONS',		24).
valid_abstract_section_header('OBJECTIVE OF STUDY',		24).
valid_abstract_section_header('OUTCOME & RESULTS',		24).
valid_abstract_section_header('OVERALL ARTICLE OBJECTIVES',		24).
valid_abstract_section_header('PATIENTS/METHOD',		24).
valid_abstract_section_header('PLAIN LANGUAGE SUMMARY',		24).
valid_abstract_section_header('PRIMARY ENDPOINT',		24).
valid_abstract_section_header('RECENT FINDING',		24).
valid_abstract_section_header('Racional:',		24).
valid_abstract_section_header('SPECIFIC AIM',		24).
valid_abstract_section_header('STATISTICAL METHOD',		24).
valid_abstract_section_header('STRATEGY FOR CHANGE',		24).
valid_abstract_section_header('STUDY APPRAISAL',		24).
valid_abstract_section_header('TC/CC',		24).
valid_abstract_section_header('VALIDITY AND COVERAGE',		24).
valid_abstract_section_header('ASSESSMENT OF PROBLEM',		23).
valid_abstract_section_header('BACKGROUND AND INTRODUCTION',		23).
valid_abstract_section_header('BACKGROUND/HYPOTHESIS',		23).
valid_abstract_section_header('BACKGROUND/INTRODUCTION',		23).
valid_abstract_section_header('BIAS, CONFOUNDING AND OTHER REASONS FOR CAUTION',		23).
valid_abstract_section_header('CONCLUSIONS/SIGNIFICANCES',		23).
valid_abstract_section_header('CRITICAL ISSUES AND FUTURE DIRECTIONS',		23).
valid_abstract_section_header('DATA SOURCES/SETTING',		23).
valid_abstract_section_header('DESIGN AND PROCEDURE',		23).
valid_abstract_section_header('DISCUSSIONE',		23).
valid_abstract_section_header('EQUIPMENT',		23).
valid_abstract_section_header('EXPERIMENTAL DESIGNS',		23).
valid_abstract_section_header('FINDINGS TO DATE',		23).
valid_abstract_section_header('IMPLICATIONS FOR RESEARCH AND PRACTICE',		23).
valid_abstract_section_header('INTERVENTIONS AND RESULTS',		23).
valid_abstract_section_header('MATERIALS AND SUBJECTS',		23).
valid_abstract_section_header('METHOD & PROCEDURES',		23).
valid_abstract_section_header('METHODS AND DATA',		23).
valid_abstract_section_header('PARAMETERS',		23).
valid_abstract_section_header('PATIENTS AND TREATMENT',		23).
valid_abstract_section_header('PATIENTS(S)',		23).
valid_abstract_section_header('PROBLEMS/OBJECTIVES',		23).
valid_abstract_section_header('RATIONALE AND AIMS',		23).
valid_abstract_section_header('RESEARCH STRATEGY',		23).
valid_abstract_section_header('RETENTION',		23).
valid_abstract_section_header('SCIENTIFIC BACKGROUND',		23).
valid_abstract_section_header('STUDY RATIONALE',		23).
valid_abstract_section_header('SUMMARY OF BACKGROUND INFORMATION',		23).
valid_abstract_section_header('TYPE OF THE STUDY',		23).
valid_abstract_section_header('UVOD',		23).
valid_abstract_section_header('VARIABLES STUDIED',		23).
valid_abstract_section_header('WORKING HYPOTHESIS',		23).
valid_abstract_section_header('AIMS AND METHODOLOGY',		22).
valid_abstract_section_header('AIMS OF THE PAPER',		22).
valid_abstract_section_header('CASES AND METHODS',		22).
valid_abstract_section_header('CELEM PRACY',		22).
valid_abstract_section_header('CONCLUSIONE',		22).
valid_abstract_section_header('DESCRIPTION OF SYSTEMS',		22).
valid_abstract_section_header('DESIGN, PATIENTS AND SETTING',		22).
valid_abstract_section_header('DESIGN, PATIENTS, AND METHODS',		22).
valid_abstract_section_header('DESIGN, SETTINGS, AND PATIENTS',		22).
valid_abstract_section_header('DESIGN/METHODOLOGY',		22).
valid_abstract_section_header('ETHNO-PHARMACOLOGICAL RELEVANCE',		22).
valid_abstract_section_header('FACTS FROM EAST AND WEST',		22).
valid_abstract_section_header('FOCUSED QUESTION',		22).
valid_abstract_section_header('FRAMEWORK',		22).
valid_abstract_section_header('GA/GG',		22).
valid_abstract_section_header('IN SUMMARY',		22).
valid_abstract_section_header('INCIDENCE',		22).
valid_abstract_section_header('INCLUSION AND EXCLUSION CRITERIA',		22).
valid_abstract_section_header('MAIN OUTCOMES MEASUREMENTS',		22).
valid_abstract_section_header('MCS',		22).
valid_abstract_section_header('MEASURES AND MAIN RESULTS',		22).
valid_abstract_section_header('OUTCOME VARIABLE',		22).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTING, METHOD',		22).
valid_abstract_section_header('PATIENTS AND SUBJECTS',		22).
valid_abstract_section_header('PATIENTS, MATERIAL, METHODS',		22).
valid_abstract_section_header('PROCEDURE AND RESULTS',		22).
valid_abstract_section_header('QUESTIONS UNDER STUDY/PRINCIPLES',		22).
valid_abstract_section_header('RATIONALE, AIMS AND OBJECTIVE',		22).
valid_abstract_section_header('RESULTS AND OBSERVATIONS',		22).
valid_abstract_section_header('REVIEW REGISTRATION',		22).
valid_abstract_section_header('STUDY BACKGROUND',		22).
valid_abstract_section_header('STUDY DESIGN, SIZE, AND DURATION',		22).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA, PARTICIPANTS, AND INTERVENTIONS',		22).
valid_abstract_section_header('THE PURPOSE',		22).
valid_abstract_section_header('VIEWPOINTS',		22).
valid_abstract_section_header('AIM & OBJECTIVES',		21).
valid_abstract_section_header('AIM AND DESIGN',		21).
valid_abstract_section_header('AIMS & METHODS',		21).
valid_abstract_section_header('AIMS AND HYPOTHESIS',		21).
valid_abstract_section_header('ANOVA',		21).
valid_abstract_section_header('BACKGROUND AND PROCEDURE',		21).
valid_abstract_section_header('CASE DIAGNOSIS/TREATMENT',		21).
valid_abstract_section_header('CASE RECORD',		21).
valid_abstract_section_header('CASE REPORT AND RESULTS',		21).
valid_abstract_section_header('CLINICAL MANIFESTATIONS',		21).
valid_abstract_section_header('CONCLUSIONS AND GLOBAL HEALTH IMPLICATIONS',		21).
valid_abstract_section_header('CONTEXT AND PURPOSE',		21).
valid_abstract_section_header('DATA SOURCES AND SELECTION',		21).
valid_abstract_section_header('DATABASE URL',		21).
valid_abstract_section_header('DISCUSSION AND LIMITATIONS',		21).
valid_abstract_section_header('ETHICS/DISSEMINATION',		21).
valid_abstract_section_header('EVIDENCE ACQUISITION AND SYNTHESIS',		21).
valid_abstract_section_header('FALLBERICHT',		21).
valid_abstract_section_header('FINAL CONSIDERATIONS',		21).
valid_abstract_section_header('FIRST CASE',		21).
valid_abstract_section_header('GENERALIZABILITY TO OTHER POPULATIONS',		21).
valid_abstract_section_header('IMPACT FOR HUMAN MEDICINE',		21).
valid_abstract_section_header('IMPLICATIONS FOR CLINICAL PRACTICE',		21).
valid_abstract_section_header('IMPORTANCE TO THE FIELD',		21).
valid_abstract_section_header('INDEPENDENT VARIABLE',		21).
valid_abstract_section_header('MATERIA? I METODA',		21).
valid_abstract_section_header('PARTICIPANTS AND MAIN OUTCOME MEASURES',		21).
valid_abstract_section_header('POSSIBLE COMPLICATIONS',		21).
valid_abstract_section_header('PRIMARY MEASURES',		21).
valid_abstract_section_header('RANDOMIZATION',		21).
valid_abstract_section_header('RESEARCH METHODOLOGY/DESIGN',		21).
valid_abstract_section_header('RESULTS AND METHODS',		21).
valid_abstract_section_header('SECONDARY OUTCOME MEASURE',		21).
valid_abstract_section_header('STUDY APPRAISAL AND SYNTHESIS',		21).
valid_abstract_section_header('STUDY PROTOCOL',		21).
valid_abstract_section_header('STUDY-DESIGN',		21).
valid_abstract_section_header('SURGICAL TREATMENT',		21).
valid_abstract_section_header('TAKE-HOME MESSAGE',		21).
valid_abstract_section_header('THERAPY AND CLINICAL COURSE',		21).
valid_abstract_section_header('WHERE NEXT',		21).
valid_abstract_section_header('AIMS AND DESIGN',		20).
valid_abstract_section_header('CASE REPORT AND DISCUSSION',		20).
valid_abstract_section_header('CASE SERIES SUMMARY',		20).
valid_abstract_section_header('CONCLUSION',		20).
valid_abstract_section_header('CONCLUSION & CLINICAL RELEVANCE',		20).
valid_abstract_section_header('CONCLUSIONS AND RELEVANCE TO CLINICAL PRACTICE',		20).
valid_abstract_section_header('CONCLUSIONS/LESSONS LEARNED',		20).
valid_abstract_section_header('CONCLUTION',		20).
valid_abstract_section_header('DATA SOURCES/STUDY DESIGN',		20).
valid_abstract_section_header('DESIGN AND POPULATION',		20).
valid_abstract_section_header('DISEASE SIGNS',		20).
valid_abstract_section_header('ETHICAL ISSUES',		20).
valid_abstract_section_header('EVALUATION METHODS',		20).
valid_abstract_section_header('GROUP A',		20).
valid_abstract_section_header('IMPLICATIONS FOR CASE MANAGEMENT',		20).
valid_abstract_section_header('IMPLICATIONS FOR NURSING RESEARCH',		20).
valid_abstract_section_header('LEVEL OF PROOF',		20).
valid_abstract_section_header('MAIN PROBLEM',		20).
valid_abstract_section_header('MATERIALS AND METHODS:',		20).
valid_abstract_section_header('METHODOLOGIES/PRINCIPAL FINDINGS',		20).
valid_abstract_section_header('METHODOLOGY AND PRINCIPLE FINDINGS',		20).
valid_abstract_section_header('METHODS/SUBJECTS',		20).
valid_abstract_section_header('OBJECTIVE AND SETTING',		20).
valid_abstract_section_header('OUTCOME & MEASUREMENTS',		20).
valid_abstract_section_header('Objetivo:',		20).
valid_abstract_section_header('PATIENTS/MATERIALS AND METHODS',		20).
valid_abstract_section_header('PERSPECTIVES AND CONCLUSION',		20).
valid_abstract_section_header('POPULATION/SAMPLE',		20).
valid_abstract_section_header('POTENTIAL CLINICAL RELEVANCE',		20).
valid_abstract_section_header('RATIONALE FOR THE STUDY',		20).
valid_abstract_section_header('RECOMMENDATIONS FOR CLINICAL PRACTICE',		20).
valid_abstract_section_header('RESEARCH IMPLICATIONS',		20).
valid_abstract_section_header('RESULTS AND/OR CONCLUSIONS',		20).
valid_abstract_section_header('REVIEW OF LITERATURE',		20).
valid_abstract_section_header('Resultados:',		20).
valid_abstract_section_header('Results:',		20).
valid_abstract_section_header('SELECTION PROCEDURE',		20).
valid_abstract_section_header('SPECIFIC AIMS',		20).
valid_abstract_section_header('STATE OF THE ART AND PERSPECTIVES',		20).
valid_abstract_section_header('THE CASE',		20).
valid_abstract_section_header('THE SOLUTION',		20).
valid_abstract_section_header('AG/GG',		19).
valid_abstract_section_header('ANALYTICAL APPROACH',		19).
valid_abstract_section_header('ARTICLE CHOSEN',		19).
valid_abstract_section_header('BACKGROUND AND OBJECT',		19).
valid_abstract_section_header('CASE AND METHODS',		19).
valid_abstract_section_header('CASE HISTORIES',		19).
valid_abstract_section_header('CLINICAL AND PATHOLOGICAL FINDINGS',		19).
valid_abstract_section_header('CLINICAL DATA',		19).
valid_abstract_section_header('CLINICAL MATERIAL',		19).
valid_abstract_section_header('CLINICAL OBSERVATION',		19).
valid_abstract_section_header('CLINICAL RECOMMENDATIONS',		19).
valid_abstract_section_header('COHORT',		19).
valid_abstract_section_header('CONCLUSIONS/CLINICAL RELEVANCE',		19).
valid_abstract_section_header('CONCLUSIONS/RELEVANCE',		19).
valid_abstract_section_header('CONSENSUS',		19).
valid_abstract_section_header('DATA AND SOURCES',		19).
valid_abstract_section_header('DATA SOURCES AND SYNTHESIS',		19).
valid_abstract_section_header('DATABASES',		19).
valid_abstract_section_header('DATE OF FIRST PATIENT\'S ENROLMENT',		19).
valid_abstract_section_header('DESCRIPTION OF CASE',		19).
valid_abstract_section_header('DESIGN AND MATERIALS',		19).
valid_abstract_section_header('EDUCATIONAL AIMS',		19).
valid_abstract_section_header('EXPERIMENTAL ANIMALS',		19).
valid_abstract_section_header('HYPOTHESIS AND BACKGROUND',		19).
valid_abstract_section_header('IMPLICATIONS FOR POLICY',		19).
valid_abstract_section_header('INTERVENTIONS AND OUTCOMES',		19).
valid_abstract_section_header('Intervention',		19).
valid_abstract_section_header('MATERIEL AND METHODS',		19).
valid_abstract_section_header('METHODS AND OUTCOME MEASURES',		19).
valid_abstract_section_header('Materials and methods',		19).
valid_abstract_section_header('OBJECTIVE AND CONCLUSION',		19).
valid_abstract_section_header('OBJECTIVES/AIMS',		19).
valid_abstract_section_header('ORIGINALITY',		19).
valid_abstract_section_header('PATIENTS, MATERIALS, AND METHODS',		19).
valid_abstract_section_header('PATIENTS/INTERVENTIONS',		19).
valid_abstract_section_header('POPULATION AND SAMPLE',		19).
valid_abstract_section_header('PRELIMINARY RESULTS',		19).
valid_abstract_section_header('PRIMARY STUDY OBJECTIVE',		19).
valid_abstract_section_header('PROGRAM',		19).
valid_abstract_section_header('PROJECT DESCRIPTION',		19).
valid_abstract_section_header('REFERENCE',		19).
valid_abstract_section_header('RESEARCH TOPICS',		19).
valid_abstract_section_header('RESOLUTIONS',		19).
valid_abstract_section_header('RESULTS AND IMPLICATIONS',		19).
valid_abstract_section_header('SAMPLES AND METHODS',		19).
valid_abstract_section_header('STUDY PARTICIPANTS AND METHODS',		19).
valid_abstract_section_header('SUMMARY POINTS',		19).
valid_abstract_section_header('TAXONOMIC NOVELTIES',		19).
valid_abstract_section_header('VOLUNTEERS AND METHODS',		19).
valid_abstract_section_header('WIDER IMPLICATION OF THE FINDINGS',		19).
valid_abstract_section_header('WORK METHOD',		19).
valid_abstract_section_header('A CASE REPORT',		18).
valid_abstract_section_header('ABBREVIATION',		18).
valid_abstract_section_header('ACKNOWLEDGMENTS',		18).
valid_abstract_section_header('BACKGROUND AND STUDY OBJECTIVE',		18).
valid_abstract_section_header('BACKGROUND/AIMS/METHODS',		18).
valid_abstract_section_header('BACKGROUND/GOALS',		18).
valid_abstract_section_header('BLINDING',		18).
valid_abstract_section_header('CASE ILLUSTRATION',		18).
valid_abstract_section_header('CLINICAL TRIAL REGISTRY NUMBER',		18).
valid_abstract_section_header('CONCLUDING STATEMENT',		18).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATION',		18).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR NURSING MANAGEMENT',		18).
valid_abstract_section_header('Conclusion:',		18).
valid_abstract_section_header('DESCRIPTORS',		18).
valid_abstract_section_header('DESIGN AND MEASUREMENT',		18).
valid_abstract_section_header('DESIGN, PATIENTS, AND SETTING',		18).
valid_abstract_section_header('DESIGN, SETTINGS AND PARTICIPANTS',		18).
valid_abstract_section_header('DESIGN, SUBJECTS AND METHODS',		18).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL SIGNIFICANCE',		18).
valid_abstract_section_header('EXAMPLES',		18).
valid_abstract_section_header('FOCUSED CLINICAL QUESTION',		18).
valid_abstract_section_header('GOAL OF SURGERY',		18).
valid_abstract_section_header('IN THE TITLE COMPOUND (SYSTEMATIC NAME',		18).
valid_abstract_section_header('INCLUDED STUDIES',		18).
valid_abstract_section_header('INTRODUCTON',		18).
valid_abstract_section_header('KEY PRACTITIONER MESSAGES',		18).
valid_abstract_section_header('LEVEL',		18).
valid_abstract_section_header('MAIN TEXT',		18).
valid_abstract_section_header('METHOD/PRINCIPAL FINDINGS',		18).
valid_abstract_section_header('METHODS AND FINDING',		18).
valid_abstract_section_header('OBIETTIVI',		18).
valid_abstract_section_header('OBJECT AND METHOD',		18).
valid_abstract_section_header('OUTLINE',		18).
valid_abstract_section_header('PATIENTS OR OTHERS PARTICIPANTS',		18).
valid_abstract_section_header('POSITIONING AND ANAESTHESIA',		18).
valid_abstract_section_header('PROBANDS AND METHODS',		18).
valid_abstract_section_header('RECOMMENDATION 1',		18).
valid_abstract_section_header('RECOMMENDATION 2',		18).
valid_abstract_section_header('RESULTS AND INTERPRETATIONS',		18).
valid_abstract_section_header('SETTING AND INTERVENTIONS',		18).
valid_abstract_section_header('SETTING(S)',		18).
valid_abstract_section_header('SIGNIFICANT AND IMPACT OF THE STUDY',		18).
valid_abstract_section_header('SOURCE OF INFORMATION',		18).
valid_abstract_section_header('SPECULATION',		18).
valid_abstract_section_header('STUDY DESIGN AND OBJECTIVES',		18).
valid_abstract_section_header('STUDY DESIGN/DATA COLLECTION',		18).
valid_abstract_section_header('STUDY PERSPECTIVE',		18).
valid_abstract_section_header('SUMMARY OF RESULTS',		18).
valid_abstract_section_header('Subjects and methods',		18).
valid_abstract_section_header('THE FUTURE',		18).
valid_abstract_section_header('ACKNOWLEDGEMENTS',		17).
valid_abstract_section_header('CHILDREN',		17).
valid_abstract_section_header('CLINICAL ADVANTAGES',		17).
valid_abstract_section_header('CONCLUSION & IMPLICATIONS',		17).
valid_abstract_section_header('CONCLUSIONS AND PUBLIC HEALTH IMPLICATIONS',		17).
valid_abstract_section_header('DESIGN/INTERVENTION',		17).
valid_abstract_section_header('DISCUSSIONS/CONCLUSIONS',		17).
valid_abstract_section_header('DISEASE CONTROL',		17).
valid_abstract_section_header('DISEASE MANAGEMENT',		17).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL IMPORTANCE',		17).
valid_abstract_section_header('EVIDENCE LEVEL',		17).
valid_abstract_section_header('EXAMPLE',		17).
valid_abstract_section_header('FOCUS',		17).
valid_abstract_section_header('FUTURE PERSPECTIVES',		17).
valid_abstract_section_header('GOALS OF THE STUDY',		17).
valid_abstract_section_header('GOALS/BACKGROUND',		17).
valid_abstract_section_header('HYPOTHESIS AND OBJECTIVES',		17).
valid_abstract_section_header('KEY EXPOSURE/STUDY FACTOR',		17).
valid_abstract_section_header('KEY POINTS',		17).
valid_abstract_section_header('LITERATURE REVIEWED',		17).
valid_abstract_section_header('MAIN METHOD',		17).
valid_abstract_section_header('MATERIAL AND SUBJECTS',		17).
valid_abstract_section_header('MATERIAL AND TREATMENT',		17).
valid_abstract_section_header('MATERIAL METHODS',		17).
valid_abstract_section_header('OUTCOME MEASURED',		17).
valid_abstract_section_header('PATIENT PRESENTATION',		17).
valid_abstract_section_header('PATIENTEN UND METHODEN',		17).
valid_abstract_section_header('POLICY POINTS',		17).
valid_abstract_section_header('PRETREATMENT RECORDS',		17).
valid_abstract_section_header('PRINCIPLE',		17).
valid_abstract_section_header('PROBLEM IDENTIFICATION',		17).
valid_abstract_section_header('PUBLIC HEALTH IMPLICATIONS',		17).
valid_abstract_section_header('RESULT AND CONCLUSIONS',		17).
valid_abstract_section_header('STANDARD TREATMENT',		17).
valid_abstract_section_header('STUDY AND DESIGN',		17).
valid_abstract_section_header('STUDY FUNDING',		17).
valid_abstract_section_header('STUDY GOAL',		17).
valid_abstract_section_header('SUBJECTIVE',		17).
valid_abstract_section_header('SUBJECTS AND METHODOLOGY',		17).
valid_abstract_section_header('SUBJECTS AND RESULTS',		17).
valid_abstract_section_header('SUMMARY/CONCLUSIONS',		17).
valid_abstract_section_header('THEORETICAL FRAMEWORK',		17).
valid_abstract_section_header('TO CLAIM CME CREDITS',		17).
valid_abstract_section_header('TYPES OF INTERVENTIONS',		17).
valid_abstract_section_header('VIDEO ABSTRACT AVAILABLE',		17).
valid_abstract_section_header('WORK RESULTS',		17).
valid_abstract_section_header('AREA COVERED IN THIS REVIEW',		16).
valid_abstract_section_header('BACKGROUNDS AND AIM',		16).
valid_abstract_section_header('BACKGROUND?',		16).
valid_abstract_section_header('BASELINE DATA',		16).
valid_abstract_section_header('CAPSULE SUMMARY',		16).
valid_abstract_section_header('CASE DEFINITION',		16).
valid_abstract_section_header('CASE DISCUSSION',		16).
valid_abstract_section_header('CASE OUTLINES',		16).
valid_abstract_section_header('CASE-REPORTS',		16).
valid_abstract_section_header('CONCLUSION AND PERSPECTIVES',		16).
valid_abstract_section_header('CONCLUSIONS',		16).
valid_abstract_section_header('CONCLUSIONS?',		16).
valid_abstract_section_header('CONLUSIONS',		16).
valid_abstract_section_header('DATA COLLECTION/ANALYSIS',		16).
valid_abstract_section_header('DATABASES AND DATA TREATMENT',		16).
valid_abstract_section_header('DESIGN SETTING',		16).
valid_abstract_section_header('DESIGN, PARTICIPANTS, AND INTERVENTION',		16).
valid_abstract_section_header('DESIGN, SETTING',		16).
valid_abstract_section_header('Data Extraction',		16).
valid_abstract_section_header('EDUCATIONAL OBJECTIVE',		16).
valid_abstract_section_header('ENHANCED VERSION',		16).
valid_abstract_section_header('EVALUATIONS',		16).
valid_abstract_section_header('EXPERIMENTAL INTERVENTION',		16).
valid_abstract_section_header('Exposure',		16).
valid_abstract_section_header('FEATURES',		16).
valid_abstract_section_header('GLOBAL IMPORTANCE',		16).
valid_abstract_section_header('ILLUSTRATIVE CASE',		16).
valid_abstract_section_header('LEVEL OF EVIDENCE I',		16).
valid_abstract_section_header('LIMITATIONS AND CONCLUSIONS',		16).
valid_abstract_section_header('LIMITATIONS OF THE STUDY',		16).
valid_abstract_section_header('LITERATURE SURVEY',		16).
valid_abstract_section_header('MAIN MEASUREMENT',		16).
valid_abstract_section_header('MAJOR RESULTS',		16).
valid_abstract_section_header('MATERIAL AND PATIENTS',		16).
valid_abstract_section_header('MEDICAL HISTORY',		16).
valid_abstract_section_header('METHODS & FINDINGS',		16).
valid_abstract_section_header('METHODS/DESIGNS',		16).
valid_abstract_section_header('METHODS/STUDY DESIGN',		16).
valid_abstract_section_header('METHODS?',		16).
valid_abstract_section_header('OBJECTIVE AND PATIENTS',		16).
valid_abstract_section_header('OBJECTIVE?',		16).
valid_abstract_section_header('OPERATIONS',		16).
valid_abstract_section_header('OUTCOME MEASURES AND STATISTICAL ANALYSIS',		16).
valid_abstract_section_header('OVERALL STRENGTH OF EVIDENCE',		16).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTING, AND METHODS',		16).
valid_abstract_section_header('PATIENTS AND CONTROL SUBJECTS',		16).
valid_abstract_section_header('PATIENTS/INTERVENTION',		16).
valid_abstract_section_header('PRACTICE',		16).
valid_abstract_section_header('PREDICTOR VARIABLES',		16).
valid_abstract_section_header('PRINCIPAL FINDINGS/CONCLUSIONS',		16).
valid_abstract_section_header('PURPOSE OF THIS REVIEW',		16).
valid_abstract_section_header('RATIONALE/OBJECTIVES',		16).
valid_abstract_section_header('RESULT/CONCLUSION',		16).
valid_abstract_section_header('RESULTS?',		16).
valid_abstract_section_header('Racional:',		16).
valid_abstract_section_header('STATE OF KNOWLEDGE',		16).
valid_abstract_section_header('SUMMARY INTRODUCTION',		16).
valid_abstract_section_header('THE AIM OF THE WORK',		16).
valid_abstract_section_header('TOLERABILITY',		16).
valid_abstract_section_header('TRANSMISSION',		16).
valid_abstract_section_header('VOLUNTEERS',		16).
valid_abstract_section_header('ANATOMY',		15).
valid_abstract_section_header('ANIMALS AND INTERVENTIONS',		15).
valid_abstract_section_header('BACKGROUND PURPOSE',		15).
valid_abstract_section_header('BACKGROUND/PURPOSE(S)',		15).
valid_abstract_section_header('BASIC PROCEDURE',		15).
valid_abstract_section_header('BRIEF DESCRIPTION',		15).
valid_abstract_section_header('Background:',		15).
valid_abstract_section_header('CHILDREN AND METHODS',		15).
valid_abstract_section_header('CLINICAL CONSIDERATIONS',		15).
valid_abstract_section_header('CLINICAL OBSERVATIONS',		15).
valid_abstract_section_header('CONCLUSIONS(S)',		15).
valid_abstract_section_header('CURRENT SITUATION',		15).
valid_abstract_section_header('DATE SOURCES',		15).
valid_abstract_section_header('DESCRIPTION OF PROGRAM',		15).
valid_abstract_section_header('DESIGN METHODS',		15).
valid_abstract_section_header('DESIGN, PARTICIPANTS',		15).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS, AND INTERVENTION',		15).
valid_abstract_section_header('DESIGN, SUBJECTS AND MEASUREMENTS',		15).
valid_abstract_section_header('DESIGN/INTERVENTIONS',		15).
valid_abstract_section_header('DESIGN/SETTING/SUBJECTS',		15).
valid_abstract_section_header('EXAMINATION',		15).
valid_abstract_section_header('EXPECTED OUTCOMES',		15).
valid_abstract_section_header('EXPERIMENT DESIGN',		15).
valid_abstract_section_header('FUTURE',		15).
valid_abstract_section_header('GROUP B',		15).
valid_abstract_section_header('IMPLICATIONS AND CONCLUSIONS',		15).
valid_abstract_section_header('INTERVENTION AND TECHNIQUE',		15).
valid_abstract_section_header('INTERVENTIONS AND OUTCOME',		15).
valid_abstract_section_header('INTRODUCTION AND CLINICAL CASE',		15).
valid_abstract_section_header('KEY CONCLUSION AND IMPLICATIONS FOR PRACTICE',		15).
valid_abstract_section_header('KEY ISSUE',		15).
valid_abstract_section_header('LIMITATIONS REASONS FOR CAUTION',		15).
valid_abstract_section_header('MAIN OUTCOME MEASUREMENTS AND RESULTS',		15).
valid_abstract_section_header('MEAN OUTCOME MEASURE',		15).
valid_abstract_section_header('MEASUREMENTS AND MAIN RESULT',		15).
valid_abstract_section_header('MECHANISMS',		15).
valid_abstract_section_header('METHOD AND SAMPLE',		15).
valid_abstract_section_header('METHOD/RESULT',		15).
valid_abstract_section_header('METHODS/SEARCH STRATEGY',		15).
valid_abstract_section_header('Methods:',		15).
valid_abstract_section_header('NULL HYPOTHESIS',		15).
valid_abstract_section_header('OBJECTIVES AND HYPOTHESIS',		15).
valid_abstract_section_header('OBJECTIVES/DESIGN',		15).
valid_abstract_section_header('OUTCOME MEASURE(S)',		15).
valid_abstract_section_header('Objetivos:',		15).
valid_abstract_section_header('PARTICIPANTS AND CONTEXT',		15).
valid_abstract_section_header('PARTICIPANTS AND OUTCOME MEASURES',		15).
valid_abstract_section_header('PATIENTS/SETTING',		15).
valid_abstract_section_header('POPULATION, MATERIAL AND METHODS',		15).
valid_abstract_section_header('PREFACE',		15).
valid_abstract_section_header('PURPOSE AND OBJECTIVES',		15).
valid_abstract_section_header('Participants',		15).
valid_abstract_section_header('QUALITY PROBLEM',		15).
valid_abstract_section_header('RESULTS AND FINDINGS',		15).
valid_abstract_section_header('RESULTS/OUTCOMES',		15).
valid_abstract_section_header('Results.',		15).
valid_abstract_section_header('SAMPLING AND METHOD',		15).
valid_abstract_section_header('SEARCH PROTOCOL',		15).
valid_abstract_section_header('SELECTION',		15).
valid_abstract_section_header('STATE OF ART AND PERSPECTIVES',		15).
valid_abstract_section_header('STATEMENT OF PURPOSE',		15).
valid_abstract_section_header('STUDY OUTCOME',		15).
valid_abstract_section_header('STUDY REGISTRATION NUMBER',		15).
valid_abstract_section_header('SYNTHESIS OF EVIDENCE',		15).
valid_abstract_section_header('Aim:',		14).
valid_abstract_section_header('BACKGROUND AND STUDY OBJECTIVES',		14).
valid_abstract_section_header('BACKGROUND SUMMARY',		14).
valid_abstract_section_header('BASIC REMARKS',		14).
valid_abstract_section_header('CASE HISTORY AND CLINICAL FINDINGS',		14).
valid_abstract_section_header('CASO CLINICO',		14).
valid_abstract_section_header('CLINICAL ASPECTS',		14).
valid_abstract_section_header('CONCLUSION AND GENERAL SIGNIFICANCE',		14).
valid_abstract_section_header('DATA EVALUATION',		14).
valid_abstract_section_header('DATA SOURCES, STUDY SELECTION, AND DATA EXTRACTION',		14).
valid_abstract_section_header('DATA SOURCES/STUDY SELECTION',		14).
valid_abstract_section_header('DATABASES USED',		14).
valid_abstract_section_header('DESCRIPTION OF THE CASE',		14).
valid_abstract_section_header('DESIGN AND ANALYSIS',		14).
valid_abstract_section_header('DESIGN AND STUDY POPULATION',		14).
valid_abstract_section_header('DESIGN, PATIENTS',		14).
valid_abstract_section_header('DESIGN, PATIENTS, AND INTERVENTIONS',		14).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, AND INTERVENTIONS',		14).
valid_abstract_section_header('DIAGNOSTIC METHODS',		14).
valid_abstract_section_header('Data Synthesis',		14).
valid_abstract_section_header('EDITOR\'S NOTE',		14).
valid_abstract_section_header('EFFECTIVENESS',		14).
valid_abstract_section_header('FACILITY',		14).
valid_abstract_section_header('FUTURE RESEARCH',		14).
valid_abstract_section_header('IMPLICATION FOR NURSING PRACTICE',		14).
valid_abstract_section_header('IMPLICATION OF THE HYPOTHESIS',		14).
valid_abstract_section_header('IMPLICATIONS FOR PUBLIC HEALTH',		14).
valid_abstract_section_header('IMPORTANT FINDINGS',		14).
valid_abstract_section_header('INTERVENTION AND RESULTS',		14).
valid_abstract_section_header('KEY CONCEPTS',		14).
valid_abstract_section_header('KEY QUESTIONS AND ANSWERS',		14).
valid_abstract_section_header('LIMITATION, REASONS FOR CAUTION',		14).
valid_abstract_section_header('MAIN OUTCOME PARAMETERS',		14).
valid_abstract_section_header('MATERIALS METHODS',		14).
valid_abstract_section_header('MATERIALS OR SUBJECTS',		14).
valid_abstract_section_header('MEASUREMENT AND FINDINGS',		14).
valid_abstract_section_header('MECHANISM OF ACTION',		14).
valid_abstract_section_header('METHOD AND SUBJECTS',		14).
valid_abstract_section_header('METHODS AND AIMS',		14).
valid_abstract_section_header('METHODS AND INTERVENTIONS',		14).
valid_abstract_section_header('METHODS DESIGN',		14).
valid_abstract_section_header('METHODS USED',		14).
valid_abstract_section_header('METHODS-RESULTS',		14).
valid_abstract_section_header('Material and Methods:',		14).
valid_abstract_section_header('OBJECTIVE/PURPOSE',		14).
valid_abstract_section_header('OBJECTIVES/AIM',		14).
valid_abstract_section_header('PARTICIPANTS/INTERVENTIONS',		14).
valid_abstract_section_header('PATIENT POPULATION AND METHODS',		14).
valid_abstract_section_header('PATIENTS AND MATERIAL',		14).
valid_abstract_section_header('PATIENTS AND SETTINGS',		14).
valid_abstract_section_header('PATIENTS, METHOD',		14).
valid_abstract_section_header('PERIOD OF STUDY',		14).
valid_abstract_section_header('POPULATIONS AND METHODS',		14).
valid_abstract_section_header('PRACTICE OR POLICY',		14).
valid_abstract_section_header('PRACTICE PATTERN EXAMINED',		14).
valid_abstract_section_header('PRIMARY RESULTS',		14).
valid_abstract_section_header('PRINCIPLE CONCLUSIONS',		14).
valid_abstract_section_header('PROSPECT',		14).
valid_abstract_section_header('Patients',		14).
valid_abstract_section_header('QUALITY PROBLEM OR ISSUE',		14).
valid_abstract_section_header('RANDOMISATION',		14).
valid_abstract_section_header('REPORTS',		14).
valid_abstract_section_header('RESEARCH DESIGN, SUBJECTS, AND MEASURES',		14).
valid_abstract_section_header('RESEARCH METHODS AND PROCEDURE',		14).
valid_abstract_section_header('RESULTS, CONCLUSION',		14).
valid_abstract_section_header('REVIEW RESULTS',		14).
valid_abstract_section_header('RT-PCR',		14).
valid_abstract_section_header('SETTING AND INTERVENTION',		14).
valid_abstract_section_header('SETTING/SAMPLE',		14).
valid_abstract_section_header('SIGNIFICANCE AND IMPACTS OF THE STUDY',		14).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA, PARTICIPANTS AND INTERVENTIONS',		14).
valid_abstract_section_header('SUBJECTS AND MEASURES',		14).
valid_abstract_section_header('SUBJECTS AND STUDY DESIGN',		14).
valid_abstract_section_header('SUBJECTS/PATIENTS AND METHODS',		14).
valid_abstract_section_header('SYSTEM DESCRIPTION',		14).
valid_abstract_section_header('TABULATION, INTEGRATION AND RESULTS',		14).
valid_abstract_section_header('TRIAL REGISTRATION ISRCTN',		14).
valid_abstract_section_header('WHAT\'S NEW',		14).
valid_abstract_section_header('AIM AND SCOPE',		13).
valid_abstract_section_header('AIM OF PAPER',		13).
valid_abstract_section_header('AIM/PURPOSE',		13).
valid_abstract_section_header('AUDIT',		13).
valid_abstract_section_header('BACKGROUND AND AIM OF WORK',		13).
valid_abstract_section_header('BACKGROUND/AIM OF STUDY',		13).
valid_abstract_section_header('CASE RESULTS',		13).
valid_abstract_section_header('CLINICAL CONTEXT',		13).
valid_abstract_section_header('CLINICAL DESCRIPTION',		13).
valid_abstract_section_header('CLINICAL FINDINGS AND DIAGNOSIS',		13).
valid_abstract_section_header('CLINICAL MATERIALS AND METHODS',		13).
valid_abstract_section_header('CLINICAL SYMPTOMS',		13).
valid_abstract_section_header('CLINICALTRIALSGOV NUMBER',		13).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR NURSING AND HEALTH POLICY',		13).
valid_abstract_section_header('CONCLUSION AND RELEVANCE TO CLINICAL PRACTICE',		13).
valid_abstract_section_header('CONCLUSIONS AND CLINICAL SIGNIFICANCE',		13).
valid_abstract_section_header('CONCLUSIONS AND LIMITATIONS',		13).
valid_abstract_section_header('CONCLUTIONS',		13).
valid_abstract_section_header('CONTEXT AND AIM',		13).
valid_abstract_section_header('CONTEXT AND AIMS',		13).
valid_abstract_section_header('Conclusions.',		13).
valid_abstract_section_header('DESCRIPTOR',		13).
valid_abstract_section_header('DESIGN AND PATIENT',		13).
valid_abstract_section_header('DESIGN AND STUDY PARTICIPANTS',		13).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, AND INTERVENTION',		13).
valid_abstract_section_header('DIAGNOSTIC PROCEDURES',		13).
valid_abstract_section_header('DIAGNOSTIC WORK-UP',		13).
valid_abstract_section_header('DISCUSSION AND SUMMARY',		13).
valid_abstract_section_header('DISCUSSION:',		13).
valid_abstract_section_header('ETHICAL ISSUES AND APPROVAL',		13).
valid_abstract_section_header('EXPERIMENTAL METHODS',		13).
valid_abstract_section_header('FINDINGS/CONCLUSION',		13).
valid_abstract_section_header('IMPLICATION FOR RESEARCH',		13).
valid_abstract_section_header('IMPLICATION STATEMENT',		13).
valid_abstract_section_header('INFANTS AND METHODS',		13).
valid_abstract_section_header('INTRODUCTION AND AIM OF THE STUDY',		13).
valid_abstract_section_header('INTRODUCTION AND RATIONALE',		13).
valid_abstract_section_header('KEY RECOMMENDATIONS',		13).
valid_abstract_section_header('MAIN EXPOSURE MEASURES',		13).
valid_abstract_section_header('MAIN OBJECTIVES',		13).
valid_abstract_section_header('MAIN OUTCOME MEASURE AND RESULTS',		13).
valid_abstract_section_header('MATERIAL, METHODS',		13).
valid_abstract_section_header('MATERIALS & METHOD',		13).
valid_abstract_section_header('MATERIALS AND PATIENTS',		13).
valid_abstract_section_header('METHOD AND PROCEDURE',		13).
valid_abstract_section_header('METHODOLOGICAL DESIGN AND JUSTIFICATION',		13).
valid_abstract_section_header('METHODS AND POPULATION',		13).
valid_abstract_section_header('OBJECTIVE AND HYPOTHESIS',		13).
valid_abstract_section_header('OBJECTIVES AND AIMS',		13).
valid_abstract_section_header('OUTCOME AND RESULTS',		13).
valid_abstract_section_header('PATIENT REPORT',		13).
valid_abstract_section_header('PATIENTS AND OUTCOME MEASURES',		13).
valid_abstract_section_header('PATIENTS OR MATERIALS',		13).
valid_abstract_section_header('PATIENTS, METHODS, AND RESULTS',		13).
valid_abstract_section_header('PRIMARY ARGUMENT',		13).
valid_abstract_section_header('PURPOSE AND PATIENTS AND METHODS',		13).
valid_abstract_section_header('PURPOSE OF RESEARCH',		13).
valid_abstract_section_header('PURPOSE OF THIS STUDY',		13).
valid_abstract_section_header('PURPOSE/INTRODUCTION',		13).
valid_abstract_section_header('RECOMMENDATION AND PERSPECTIVE',		13).
valid_abstract_section_header('REFERENCE TESTS',		13).
valid_abstract_section_header('RESEARCH DESIGN AND PARTICIPANTS',		13).
valid_abstract_section_header('RESEARCH IN CONTEXT',		13).
valid_abstract_section_header('RESEARCH METHOD AND PROCEDURES',		13).
valid_abstract_section_header('REVIEW OBJECTIVE',		13).
valid_abstract_section_header('SIGNIFICANCES',		13).
valid_abstract_section_header('STATISTICS ANALYSIS',		13).
valid_abstract_section_header('STRUCTURE',		13).
valid_abstract_section_header('STUDY ANSWER',		13).
valid_abstract_section_header('STUDY DESIGN AND OBJECTIVE',		13).
valid_abstract_section_header('STUDY DESIGN, MATERIALS AND METHODS',		13).
valid_abstract_section_header('STUDY ELIGIBILITY',		13).
valid_abstract_section_header('STUDY FINDINGS',		13).
valid_abstract_section_header('STUDY SELECTION AND DATA ABSTRACTION',		13).
valid_abstract_section_header('SUBJECTS/DESIGN',		13).
valid_abstract_section_header('SUMMARY AND BACKGROUND',		13).
valid_abstract_section_header('SUMMARY OF RECOMMENDATIONS',		13).
valid_abstract_section_header('SUMMARY OF THE EVIDENCE',		13).
valid_abstract_section_header('TRAIL REGISTRATION',		13).
valid_abstract_section_header('TREATMENT INNOVATIONS',		13).
valid_abstract_section_header('TRIAL REGISTER',		13).
valid_abstract_section_header('TRIAL REGISTRATION NO',		13).
valid_abstract_section_header('TRIAL STATUS',		13).
valid_abstract_section_header('AIM AND METHODOLOGY',		12).
valid_abstract_section_header('AIM, PATIENTS AND METHODS',		12).
valid_abstract_section_header('AIM/OBJECTIVES',		12).
valid_abstract_section_header('ANTECEDENTS',		12).
valid_abstract_section_header('ARGUMENTS',		12).
valid_abstract_section_header('BACKGROUND, MATERIAL AND METHODS',		12).
valid_abstract_section_header('BACKGROUNDS AND OBJECTIVE',		12).
valid_abstract_section_header('BACKGROUNDS AND PURPOSE',		12).
valid_abstract_section_header('CADAVERS',		12).
valid_abstract_section_header('CAPSULE ABSTRACT',		12).
valid_abstract_section_header('CASE MANAGEMENT',		12).
valid_abstract_section_header('CASE MATERIAL',		12).
valid_abstract_section_header('CASUISTIC AND METHODS',		12).
valid_abstract_section_header('CHIEF COMPLAINT',		12).
valid_abstract_section_header('CLINIC CASE',		12).
valid_abstract_section_header('CLINICAL FEATURE',		12).
valid_abstract_section_header('CLINICAL POTENTIAL',		12).
valid_abstract_section_header('CLINICAL VALUE',		12).
valid_abstract_section_header('CONDITIONS',		12).
valid_abstract_section_header('CONLUSION',		12).
valid_abstract_section_header('CONSLUSION',		12).
valid_abstract_section_header('CONTEXT AND OBJECTIVE:',		12).
valid_abstract_section_header('CONTEXT/OBJECTIVES',		12).
valid_abstract_section_header('DESIGN AND SETTING:',		12).
valid_abstract_section_header('DESIGN STUDY',		12).
valid_abstract_section_header('DESIGN, INTERVENTIONS, AND MAIN OUTCOME MEASURES',		12).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, MEASUREMENTS',		12).
valid_abstract_section_header('DESIGN/MEASUREMENTS',		12).
valid_abstract_section_header('DESIGN/PATIENTS/MEASUREMENTS',		12).
valid_abstract_section_header('DESIGN/SETTING/PARTICIPANTS/MEASUREMENTS',		12).
valid_abstract_section_header('DIAGNOSIS AND THERAPY',		12).
valid_abstract_section_header('DIAGNOSIS, TREATMENT AND CLINICAL COURSE',		12).
valid_abstract_section_header('DISCUSSIONS AND CONCLUSION',		12).
valid_abstract_section_header('ECONOMIC ANALYSIS',		12).
valid_abstract_section_header('ENZYMES',		12).
valid_abstract_section_header('ETHNOBOTANICAL RELEVANCE',		12).
valid_abstract_section_header('EVIDENCE-BASED ANALYSIS METHODS',		12).
valid_abstract_section_header('EVOLUTION',		12).
valid_abstract_section_header('EXPERIMENTAL PROTOCOL',		12).
valid_abstract_section_header('EXPERIMENTAL STUDIES',		12).
valid_abstract_section_header('EXTRACTION METHODS',		12).
valid_abstract_section_header('FOLLOW UP',		12).
valid_abstract_section_header('FUNDAMENTALS',		12).
valid_abstract_section_header('GENETICS',		12).
valid_abstract_section_header('HISTOPATHOLOGY',		12).
valid_abstract_section_header('HYPOTHESIS/BACKGROUND',		12).
valid_abstract_section_header('IMPLICATION FOR NURSING AND HEALTH POLICY',		12).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE AND RESEARCH',		12).
valid_abstract_section_header('IN CLINICAL PRACTICE',		12).
valid_abstract_section_header('INTENTION, GOAL, SCOPE, BACKGROUND',		12).
valid_abstract_section_header('INTERVENTION AND MAIN OUTCOME MEASURE',		12).
valid_abstract_section_header('MAIN OUTCOME RESULTS',		12).
valid_abstract_section_header('MATERIALS/SUBJECTS AND METHODS',		12).
valid_abstract_section_header('MATHERIALS AND METHODS',		12).
valid_abstract_section_header('MEASUREMENTS AND MAIN FINDINGS',		12).
valid_abstract_section_header('METHOD & RESULTS',		12).
valid_abstract_section_header('METHOD AND PARTICIPANTS',		12).
valid_abstract_section_header('METHODOLOGY / PRINCIPAL FINDINGS',		12).
valid_abstract_section_header('METHODS AND DISCUSSION',		12).
valid_abstract_section_header('METHODS OF ANALYSIS',		12).
valid_abstract_section_header('METHODS:',		12).
valid_abstract_section_header('NAMEN',		12).
valid_abstract_section_header('OPTIONS AND OUTCOMES',		12).
valid_abstract_section_header('OTHER PARTICIPANTS',		12).
valid_abstract_section_header('OUTCOME PARAMETERS',		12).
valid_abstract_section_header('Objective:',		12).
valid_abstract_section_header('PARTICIPANTS AND MEASURES',		12).
valid_abstract_section_header('PARTICIPANTS/PATIENTS',		12).
valid_abstract_section_header('PARTICIPATION',		12).
valid_abstract_section_header('PATIENT/PARTICIPANTS',		12).
valid_abstract_section_header('PATIENTINNEN UND METHODEN',		12).
valid_abstract_section_header('PERSPECTIVE (VALUES)',		12).
valid_abstract_section_header('PERSPECTIVES AND PROJECTS',		12).
valid_abstract_section_header('REGISTRATION DETAILS',		12).
valid_abstract_section_header('REPORT OF CASES',		12).
valid_abstract_section_header('RESEARCH AND DESIGN METHODS',		12).
valid_abstract_section_header('RESEARCH DESIGNS AND METHODS',		12).
valid_abstract_section_header('RESULT & CONCLUSION',		12).
valid_abstract_section_header('RESULTS AND COMMENTS',		12).
valid_abstract_section_header('RESULTS/OUTCOME',		12).
valid_abstract_section_header('RESULTS:',		12).
valid_abstract_section_header('SAMPLE AND METHODOLOGY',		12).
valid_abstract_section_header('SETTING AND STUDY PARTICIPANTS',		12).
valid_abstract_section_header('SETTING, PARTICIPANTS',		12).
valid_abstract_section_header('SETTING/ PARTICIPANTS',		12).
valid_abstract_section_header('SIDE EFFECTS',		12).
valid_abstract_section_header('SIGNIFICANCE AND IMPACT',		12).
valid_abstract_section_header('SIGNIFICANCE OF THE RESEARCH',		12).
valid_abstract_section_header('SITUATION',		12).
valid_abstract_section_header('SOFTWARE',		12).
valid_abstract_section_header('SPECIFIC OBJECTIVES',		12).
valid_abstract_section_header('STATISTICAL TESTS',		12).
valid_abstract_section_header('SUBJECT OBJECTIVE',		12).
valid_abstract_section_header('SUBJECTS AND PATIENTS',		12).
valid_abstract_section_header('SYNTHESIS OF THE EVIDENCE',		12).
valid_abstract_section_header('THERAPEUTIC OPTIONS',		12).
valid_abstract_section_header('TREATMENT AND METHODS',		12).
valid_abstract_section_header('TYPES OF OUTCOME MEASURES',		12).
valid_abstract_section_header('WHY THIS MATTERS TO ME',		12).
valid_abstract_section_header('ACKNOWLEDGEMENT',		11).
valid_abstract_section_header('ANAMNESIS',		11).
valid_abstract_section_header('ANIMAL OR SAMPLE POPULATION',		11).
valid_abstract_section_header('ANIMAL STUDIES',		11).
valid_abstract_section_header('AUTHORS\' CONCLUSION',		11).
valid_abstract_section_header('BACKGROUND & METHODS',		11).
valid_abstract_section_header('CLINICAL HISTORY',		11).
valid_abstract_section_header('CLINICAL OUTCOME',		11).
valid_abstract_section_header('CLINICAL PRESENTATIONS',		11).
valid_abstract_section_header('CLINICAL SIGNS',		11).
valid_abstract_section_header('CONCEPTUAL FRAMEWORK',		11).
valid_abstract_section_header('CONCLUSION AND CLINICAL REHABILITATION IMPACT',		11).
valid_abstract_section_header('CONCLUSION AND IMPACT',		11).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR NURSING MANAGEMENT',		11).
valid_abstract_section_header('CONCLUSIONS & IMPLICATIONS FOR NURSING',		11).
valid_abstract_section_header('CONCLUSIONS AND INTERPRETATION',		11).
valid_abstract_section_header('CONSIDERATIONS',		11).
valid_abstract_section_header('CURRENT KNOWLEDGE',		11).
valid_abstract_section_header('Clinical Question',		11).
valid_abstract_section_header('Conclusions:',		11).
valid_abstract_section_header('DATA COLLECTION/EXTRACTION METHOD',		11).
valid_abstract_section_header('DATA SOURCE AND SELECTION',		11).
valid_abstract_section_header('DATA SOURCE AND STUDY SELECTION',		11).
valid_abstract_section_header('DATA SYNTHESIS AND CONCLUSION',		11).
valid_abstract_section_header('DESCRIPTION OF CASES',		11).
valid_abstract_section_header('DESIGN, MATERIALS, AND METHODS',		11).
valid_abstract_section_header('DISCUSSION AND RESULTS',		11).
valid_abstract_section_header('HISTORY AND PHYSICAL EXAMINATION',		11).
valid_abstract_section_header('HISTORY OF PRESENT ILLNESS',		11).
valid_abstract_section_header('HYPOTHESIS/INTRODUCTION',		11).
valid_abstract_section_header('IMPLICATIONS FOR NURSING POLICY',		11).
valid_abstract_section_header('INTERVENTIONS AND MAIN RESULTS',		11).
valid_abstract_section_header('INTRODUCTION AND GOALS',		11).
valid_abstract_section_header('JEL CLASSIFICATION',		11).
valid_abstract_section_header('KEY CONCLUSIONS AND IMPLICATIONS',		11).
valid_abstract_section_header('KEY RISK/STUDY FACTOR',		11).
valid_abstract_section_header('Limitations',		11).
valid_abstract_section_header('MAIN OUTCOMES AND MEASURE',		11).
valid_abstract_section_header('MATERIALS AND TREATMENT',		11).
valid_abstract_section_header('MEASUREMENTS & RESULTS',		11).
valid_abstract_section_header('MEASUREMENTS AND MAIN OUTCOMES',		11).
valid_abstract_section_header('METHOD/MATERIALS',		11).
valid_abstract_section_header('NEW TECHNOLOGY',		11).
valid_abstract_section_header('OBJECTIVE AND AIM',		11).
valid_abstract_section_header('OBJECTIVE AND SUBJECTS',		11).
valid_abstract_section_header('OBJEVTIVE',		11).
valid_abstract_section_header('ONGOING ISSUES',		11).
valid_abstract_section_header('ORGANIZING CONSTRUCTS',		11).
valid_abstract_section_header('OTHER MEASUREMENTS',		11).
valid_abstract_section_header('PARTICIPANTS OR SAMPLES',		11).
valid_abstract_section_header('PATIENTS AND TECHNIQUES',		11).
valid_abstract_section_header('PATIENTS SAMPLE',		11).
valid_abstract_section_header('PATIENTS, DESIGN, AND SETTING',		11).
valid_abstract_section_header('PRESENTATION OF A CASE',		11).
valid_abstract_section_header('PRIMARY MEASUREMENTS',		11).
valid_abstract_section_header('PROGRESS',		11).
valid_abstract_section_header('PROPOSE',		11).
valid_abstract_section_header('PROTOCOL REGISTRATION NUMBER',		11).
valid_abstract_section_header('PURPOSE OF THE WORK',		11).
valid_abstract_section_header('RACIONAL',		11).
valid_abstract_section_header('RESULTS AND COMPARISON WITH EXISTING METHODS',		11).
valid_abstract_section_header('SAMPLE(S)',		11).
valid_abstract_section_header('SETTING AND METHOD',		11).
valid_abstract_section_header('SETTING AND OBJECTIVE',		11).
valid_abstract_section_header('SETTINGS AND METHODS',		11).
valid_abstract_section_header('STATISTICAL ANALYSIS AND RESULTS',		11).
valid_abstract_section_header('STUDY DESIGN AND MAIN OUTCOME MEASURES',		11).
valid_abstract_section_header('STUDY UNITS',		11).
valid_abstract_section_header('SUBJECTS (MATERIALS) AND METHODS',		11).
valid_abstract_section_header('SUBJECTS AND MAIN OUTCOME MEASURES',		11).
valid_abstract_section_header('SUBJECTS/SAMPLES',		11).
valid_abstract_section_header('TOOLS',		11).
valid_abstract_section_header('TRAIL REGISTRATION NUMBER',		11).
valid_abstract_section_header('TRANSLATION TO HEALTH EDUCATION PRACTICE',		11).
valid_abstract_section_header('TRIALS',		11).
valid_abstract_section_header('TYPE OF STUDIES REVIEWED',		11).
valid_abstract_section_header('UNIQUE INFORMATION PROVIDED',		11).
valid_abstract_section_header('AIMS AND SCOPE',		10).
valid_abstract_section_header('AIMS OF THE REVIEW',		10).
valid_abstract_section_header('AIMS/OBJECTIVE',		10).
valid_abstract_section_header('ANALYSIS AND RESULTS',		10).
valid_abstract_section_header('ASSESSMENT OF QUALITY',		10).
valid_abstract_section_header('BACKGROUND AND MOTIVATION',		10).
valid_abstract_section_header('BACKGROUND AND RESEARCH OBJECTIVES',		10).
valid_abstract_section_header('BACKGROUND OF THE STUDY',		10).
valid_abstract_section_header('BACKGROUND/PURPOSES',		10).
valid_abstract_section_header('CASE REVIEW',		10).
valid_abstract_section_header('CLINICAL APPLICATION',		10).
valid_abstract_section_header('CLINICAL BOTTOM LINE',		10).
valid_abstract_section_header('CLINICAL PROBLEM',		10).
valid_abstract_section_header('CLINICAL STUDY',		10).
valid_abstract_section_header('CLINICAL UTILITY',		10).
valid_abstract_section_header('COMMENTS AND CONCLUSION',		10).
valid_abstract_section_header('COMPARISON TO EXISTING METHODS',		10).
valid_abstract_section_header('CONCLUSION AND CLINICAL SIGNIFICANCE',		10).
valid_abstract_section_header('CONCLUSION/IMPLICATIONS FOR PRACTICE',		10).
valid_abstract_section_header('CONCLUSION/RECOMMENDATION',		10).
valid_abstract_section_header('CONSENSUS POSITION',		10).
valid_abstract_section_header('COUNT',		10).
valid_abstract_section_header('CT/TT',		10).
valid_abstract_section_header('CURRENT DATA',		10).
valid_abstract_section_header('DATA ACQUISITION',		10).
valid_abstract_section_header('DATA SOURCE AND METHODS',		10).
valid_abstract_section_header('DEFINITION OF THE PROBLEM',		10).
valid_abstract_section_header('DESCRIPTION OF THE PROJECT/INNOVATION',		10).
valid_abstract_section_header('DESCRIPTIONS',		10).
valid_abstract_section_header('DESIGN AND PROCEDURES',		10).
valid_abstract_section_header('DESIGN, PARTICIPANTS, AND METHODS',		10).
valid_abstract_section_header('DESIGN, PATIENTS, MEASUREMENTS',		10).
valid_abstract_section_header('DESIGN, SUBJECTS AND SETTING',		10).
valid_abstract_section_header('DISCLOSURE',		10).
valid_abstract_section_header('DISCUSSION AND RECOMMENDATIONS',		10).
valid_abstract_section_header('EQUIPMENT AND METHODS',		10).
valid_abstract_section_header('ETHNO PHARMACOLOGICAL RELEVANCE',		10).
valid_abstract_section_header('GOAL OF STUDY',		10).
valid_abstract_section_header('GROWING POINTS AND AREAS TIMELY FOR DEVELOPING RESEARCH',		10).
valid_abstract_section_header('IMPLICATIONS/CONCLUSIONS',		10).
valid_abstract_section_header('INSTRUCTION',		10).
valid_abstract_section_header('INTERPRETATION/CONCLUSIONS',		10).
valid_abstract_section_header('INTERVENTION/METHODS',		10).
valid_abstract_section_header('INTRODUCTION/HYPOTHESIS',		10).
valid_abstract_section_header('LITERATURE',		10).
valid_abstract_section_header('MAIN OUTCOME AND MEASUREMENTS',		10).
valid_abstract_section_header('MAIN RECOMMENDATIONS',		10).
valid_abstract_section_header('MAIN STUDY MEASURES',		10).
valid_abstract_section_header('MATERIAL METHOD',		10).
valid_abstract_section_header('MATERIALS OF STUDY',		10).
valid_abstract_section_header('MEASURMENTS',		10).
valid_abstract_section_header('MECHANISMS OF TOXICITY',		10).
valid_abstract_section_header('METHOD AND CLINICAL MATERIAL',		10).
valid_abstract_section_header('METHODOLOGY/MAIN FINDINGS',		10).
valid_abstract_section_header('OBJECTION',		10).
valid_abstract_section_header('OBJECTIVE AND RESULTS',		10).
valid_abstract_section_header('OBJECTIVE/INTRODUCTION',		10).
valid_abstract_section_header('OBJECTIVES AND AIM',		10).
valid_abstract_section_header('OBJECTIVES AND RATIONALE',		10).
valid_abstract_section_header('OBJECTIVES/PURPOSES',		10).
valid_abstract_section_header('OBJECTS AND METHODS',		10).
valid_abstract_section_header('OUTCOMES AND MEASUREMENTS',		10).
valid_abstract_section_header('Objectives.',		10).
valid_abstract_section_header('PARTICIPANTS/MATERIAL, SETTING, METHODS',		10).
valid_abstract_section_header('PARTICIPANTS/SETTINGS',		10).
valid_abstract_section_header('PATIENT AND INTERVENTION',		10).
valid_abstract_section_header('PHARMACOKINETICS',		10).
valid_abstract_section_header('PHYSICAL PROPERTIES',		10).
valid_abstract_section_header('PREDICTOR OR FACTOR',		10).
valid_abstract_section_header('PRESENTATION OF HYPOTHESIS',		10).
valid_abstract_section_header('PRIMARY OUTCOME VARIABLE',		10).
valid_abstract_section_header('PRINCIPLE RESULTS',		10).
valid_abstract_section_header('PURPOSE AND OBJECTIVE',		10).
valid_abstract_section_header('PURPOSE, PATIENTS, AND METHODS',		10).
valid_abstract_section_header('PURPOSE/AIMS',		10).
valid_abstract_section_header('RECENT PROGRESS',		10).
valid_abstract_section_header('REPORT OF A CASE',		10).
valid_abstract_section_header('RESEARCH AND METHODS',		10).
valid_abstract_section_header('RESEARCH LIMITATIONS',		10).
valid_abstract_section_header('RESEARCH METHODS & PROCEDURES',		10).
valid_abstract_section_header('RESOURCES',		10).
valid_abstract_section_header('RESULTS AND STATISTICS',		10).
valid_abstract_section_header('RESULTS OF THE STUDY',		10).
valid_abstract_section_header('RESULTS-CONCLUSIONS',		10).
valid_abstract_section_header('SECOND CASE',		10).
valid_abstract_section_header('SIGNIFICANCE/CONCLUSIONS',		10).
valid_abstract_section_header('STATEMENT OF CONCLUSIONS',		10).
valid_abstract_section_header('STATEMENT OF PROBLEMS',		10).
valid_abstract_section_header('STUDY DESIGN, PATIENTS, AND METHODS',		10).
valid_abstract_section_header('STUDY DESIGN, SIZE DURATION',		10).
valid_abstract_section_header('STUDY DESIGN/METHOD',		10).
valid_abstract_section_header('STUDY QUESTIONS',		10).
valid_abstract_section_header('STUDY SELECTION AND EXTRACTION',		10).
valid_abstract_section_header('SUBJECTS AND OUTCOME MEASURES',		10).
valid_abstract_section_header('SUBJECTS, PARTICIPANTS',		10).
valid_abstract_section_header('SUBJECTS/PARTICIPANTS',		10).
valid_abstract_section_header('SYNTHESIS METHODS',		10).
valid_abstract_section_header('THE TECHNOLOGY BEING REVIEWED',		10).
valid_abstract_section_header('THEORETICAL BACKGROUND',		10).
valid_abstract_section_header('TRIALS REGISTRATION NUMBER',		10).
valid_abstract_section_header('TYPES OF INTERVENTION(S)/PHENOMENA OF INTEREST',		10).
valid_abstract_section_header('VALUE/ORIGINALITY',		10).
valid_abstract_section_header('ACTIVITIES',		9).
valid_abstract_section_header('AG/AA',		9).
valid_abstract_section_header('AIM/METHOD',		9).
valid_abstract_section_header('AIMS AND DEVELOPMENT',		9).
valid_abstract_section_header('AIMS OF THIS STUDY',		9).
valid_abstract_section_header('AIMS/PURPOSE',		9).
valid_abstract_section_header('ANALYTIC VALIDITY',		9).
valid_abstract_section_header('ANIMALS, METHODS',		9).
valid_abstract_section_header('APPLICATIONS/CONCLUSION',		9).
valid_abstract_section_header('ASSESSMENT OF METHODOLOGICAL QUALITY',		9).
valid_abstract_section_header('BACKGORUND',		9).
valid_abstract_section_header('BACKGROUND AND STUDY PURPOSE',		9).
valid_abstract_section_header('BASIC PROBLEM AND OBJECTIVE',		9).
valid_abstract_section_header('Bottom Line',		9).
valid_abstract_section_header('CALCULATION',		9).
valid_abstract_section_header('CASES DESCRIPTION',		9).
valid_abstract_section_header('CLINICAL MATERIAL AND METHODS',		9).
valid_abstract_section_header('CLINICAL VALIDITY',		9).
valid_abstract_section_header('CONCLUSION AND PERSPECTIVE',		9).
valid_abstract_section_header('CONCLUSION/RECOMMENDATIONS',		9).
valid_abstract_section_header('CONCLUSIONS/DISCUSSION',		9).
valid_abstract_section_header('CONCLUSIONS/SUMMARY',		9).
valid_abstract_section_header('CRITERIA',		9).
valid_abstract_section_header('CT/CC',		9).
valid_abstract_section_header('DATA ANALYSES',		9).
valid_abstract_section_header('DATA SOURCES/STUDY SETTINGS',		9).
valid_abstract_section_header('DESCRIPTION OF INSTRUMENTATION',		9).
valid_abstract_section_header('DESIGN & PARTICIPANTS',		9).
valid_abstract_section_header('DESIGN AND OBJECTIVE',		9).
valid_abstract_section_header('DESIGN, PARTICIPANTS AND MEASUREMENTS',		9).
valid_abstract_section_header('DESIGN, SETTING, AND PATIENT',		9).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, AND MEASURES',		9).
valid_abstract_section_header('DESIGN, SUBJECTS, AND INTERVENTION',		9).
valid_abstract_section_header('DESIGN/PARTICIPANTS/SETTING',		9).
valid_abstract_section_header('DESIGN/SAMPLE',		9).
valid_abstract_section_header('DESIGN:',		9).
valid_abstract_section_header('DIAGNOSIS, THERAPY AND CLINICAL COURSE',		9).
valid_abstract_section_header('DIAGNOSIS, THERAPY AND COURSE',		9).
valid_abstract_section_header('DIAGNOSTIC',		9).
valid_abstract_section_header('DIAGNOSTIC TEST',		9).
valid_abstract_section_header('DISCUSION',		9).
valid_abstract_section_header('DISCUSSION AND IMPLICATIONS',		9).
valid_abstract_section_header('ELIGIBILITY CRITERIA FOR STUDY SELECTION',		9).
valid_abstract_section_header('ETHICAL APPROVAL',		9).
valid_abstract_section_header('ETHNOPHARMACOLOGIC RELEVANCE',		9).
valid_abstract_section_header('EVIDENCE SUMMARY',		9).
valid_abstract_section_header('EXEGESE',		9).
valid_abstract_section_header('EXPERIENCE AND RESULTS',		9).
valid_abstract_section_header('EXPERIMENTAL MATERIALS',		9).
valid_abstract_section_header('EXPERIMENTAL PROCEDURE',		9).
valid_abstract_section_header('EXPERIMENTAL VARIABLES',		9).
valid_abstract_section_header('FAMILY HISTORY',		9).
valid_abstract_section_header('FUNDING DETAILS',		9).
valid_abstract_section_header('GOV NUMBER',		9).
valid_abstract_section_header('GUIDELINE QUESTION',		9).
valid_abstract_section_header('HEALTH POLITICAL BACKGROUND',		9).
valid_abstract_section_header('HYPERTENSION',		9).
valid_abstract_section_header('ILLUSTRATIVE CASES',		9).
valid_abstract_section_header('IMAGING',		9).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH CARE PROVISION',		9).
valid_abstract_section_header('IMPLICATIONS FOR NURSE MANAGERS',		9).
valid_abstract_section_header('INCLUSION',		9).
valid_abstract_section_header('INFERENCE',		9).
valid_abstract_section_header('INTERVENTION AND METHODS',		9).
valid_abstract_section_header('INTERVENTION AND OUTCOME MEASURES',		9).
valid_abstract_section_header('MAIN FINDING',		9).
valid_abstract_section_header('MAIN OUTCOME(S) AND MEASURE(S)',		9).
valid_abstract_section_header('MATERIALS AND MATERIALS',		9).
valid_abstract_section_header('MEAN OUTCOME MEASURE(S)',		9).
valid_abstract_section_header('MEASUREMENTS AND ANALYSIS',		9).
valid_abstract_section_header('METHOD/PATIENTS',		9).
valid_abstract_section_header('METHODOLOGIES',		9).
valid_abstract_section_header('METHODS & STUDY DESIGN',		9).
valid_abstract_section_header('METHODS AND APPROACH',		9).
valid_abstract_section_header('METHODS AND TECHNIQUE',		9).
valid_abstract_section_header('METHODS/SAMPLE',		9).
valid_abstract_section_header('MICROBIOLOGICAL PROPERTIES',		9).
valid_abstract_section_header('MINI-ABSTRACT',		9).
valid_abstract_section_header('MODEL, PERSPECTIVE, & TIMEFRAME',		9).
valid_abstract_section_header('MORTALITY',		9).
valid_abstract_section_header('Main Outcome and Measures',		9).
valid_abstract_section_header('Material and Methods',		9).
valid_abstract_section_header('Method:',		9).
valid_abstract_section_header('OBJECTIVE AND BACKGROUND DATA',		9).
valid_abstract_section_header('OBJECTIVE, DESIGN, AND SETTING',		9).
valid_abstract_section_header('OUTCOME ASSESSMENT',		9).
valid_abstract_section_header('PARTICIPANTS AND/OR CONTROLS',		9).
valid_abstract_section_header('PARTICIPANTS, DESIGN, AND SETTING',		9).
valid_abstract_section_header('PARTICIPANTS, SETTING AND METHODS',		9).
valid_abstract_section_header('PARTICIPANTS/INTERVENTION',		9).
valid_abstract_section_header('PATIENT CASE',		9).
valid_abstract_section_header('POSTOPERATIVE CARE',		9).
valid_abstract_section_header('PRINCIPAL FINDINGS AND CONCLUSIONS',		9).
valid_abstract_section_header('PRINCIPALS',		9).
valid_abstract_section_header('PROBLEM STATEMENT AND BACKGROUND',		9).
valid_abstract_section_header('PROGRAM DESIGN',		9).
valid_abstract_section_header('PURPOSE AND CLINICAL RELEVANCE',		9).
valid_abstract_section_header('PURPOSE AND SETTING',		9).
valid_abstract_section_header('QUALIFYING STATEMENTS',		9).
valid_abstract_section_header('RATIONAL, AIMS AND OBJECTIVES',		9).
valid_abstract_section_header('RATIONALE, AIM AND OBJECTIVE',		9).
valid_abstract_section_header('RATIONALE, AIMS, AND OBJECTIVES',		9).
valid_abstract_section_header('RECOMMENDATION 3',		9).
valid_abstract_section_header('REFERENCE/CITATION',		9).
valid_abstract_section_header('RELEVANCE TO CLINICAL OR PROFESSIONAL PRACTICE',		9).
valid_abstract_section_header('REMARKS',		9).
valid_abstract_section_header('RESEARCH DESIGN AND METHODS AND RESULTS',		9).
valid_abstract_section_header('RESULTADO',		9).
valid_abstract_section_header('RESULTS AND COMPLICATIONS',		9).
valid_abstract_section_header('RESULTS OF SENSITIVITY ANALYSES',		9).
valid_abstract_section_header('RESUTLS',		9).
valid_abstract_section_header('REVIEW QUESTION',		9).
valid_abstract_section_header('REVIEW REGISTRATION NUMBER',		9).
valid_abstract_section_header('SCOPE AND PURPOSE',		9).
valid_abstract_section_header('SEARCH METHOD',		9).
valid_abstract_section_header('SEARCH STRATEGY AND SELECTION CRITERIA',		9).
valid_abstract_section_header('SETTING/DESIGN',		9).
valid_abstract_section_header('SETTING/PARTICIPANTS/RESOURCES',		9).
valid_abstract_section_header('SETTING/POPULATION',		9).
valid_abstract_section_header('STUDY DESIGN SIZE, DURATION',		9).
valid_abstract_section_header('STUDY DESIGN/RESULTS',		9).
valid_abstract_section_header('STUDY DESIGNS/MATERIALS AND METHODS',		9).
valid_abstract_section_header('STUDY FACTORS',		9).
valid_abstract_section_header('STUDY GOALS',		9).
valid_abstract_section_header('STUDY SETTING/DATA SOURCES',		9).
valid_abstract_section_header('STUDY SITE',		9).
valid_abstract_section_header('SUMMARY OF EVIDENCE',		9).
valid_abstract_section_header('SYSTEMATIC REVIEW METHODOLOGY',		9).
valid_abstract_section_header('TASK FORCE RECOMMENDATIONS',		9).
valid_abstract_section_header('THE AIM OF THE PAPER',		9).
valid_abstract_section_header('THERAPEUTIC IMPLICATIONS',		9).
valid_abstract_section_header('TOXICOKINETICS',		9).
valid_abstract_section_header('TREATMENT PLAN',		9).
valid_abstract_section_header('TRIAL REGISTRATION INFORMATION',		9).
valid_abstract_section_header('TRIAL SELECTION',		9).
valid_abstract_section_header('VACCINATION RECOMMENDATIONS',		9).
valid_abstract_section_header('WHAT THE READERS WILL GAIN',		9).
valid_abstract_section_header('WNIOSKI:',		9).
valid_abstract_section_header('WYNIKI:',		9).
valid_abstract_section_header('ACCREDITATION',		8).
valid_abstract_section_header('AETIOLOGY',		8).
valid_abstract_section_header('AIM OF THE RESEARCH',		8).
valid_abstract_section_header('AMS SUBJECT CLASSIFICATION',		8).
valid_abstract_section_header('AREAS OF AGREEMENT AND CONTROVERSY',		8).
valid_abstract_section_header('BACKGROUND AND PURPOSE OF STUDY',		8).
valid_abstract_section_header('BACKGROUND CONTENT',		8).
valid_abstract_section_header('BACKGROUND DATA AND OBJECTIVE',		8).
valid_abstract_section_header('BACKGROUND/CONTEXT',		8).
valid_abstract_section_header('BACKGROUND/METHOD',		8).
valid_abstract_section_header('BACKGROUND/SIGNIFICANCE',		8).
valid_abstract_section_header('BASIC PROBLEM AND OBJECTIVE OF STUDY',		8).
valid_abstract_section_header('BASIC RESEARCH DESIGN AND PARTICIPANTS',		8).
valid_abstract_section_header('CASE OR SERIES SUMMARY',		8).
valid_abstract_section_header('CASE OUTCOME',		8).
valid_abstract_section_header('CASE REPORT AND METHODS',		8).
valid_abstract_section_header('CASUISTIC AND METHOD',		8).
valid_abstract_section_header('CHALLENGES AND LESSONS LEARNED',		8).
valid_abstract_section_header('CLINICAL INVESTIGATIONS',		8).
valid_abstract_section_header('COMMENTS AND CONCLUSIONS',		8).
valid_abstract_section_header('CONCLUSION AND IMPLICATION FOR PRACTICE',		8).
valid_abstract_section_header('CONCLUSION AND OUTLOOK',		8).
valid_abstract_section_header('CONCLUSIONS/PRACTICE IMPLICATIONS',		8).
valid_abstract_section_header('CONCLUSIONS:',		8).
valid_abstract_section_header('CONTEXT OF CASE',		8).
valid_abstract_section_header('CONTEXT/BACKGROUND',		8).
valid_abstract_section_header('CONTEXTUAL ISSUES',		8).
valid_abstract_section_header('CRITICAL APPRAISAL',		8).
valid_abstract_section_header('DATA COLLECTION AND SYNTHESIS',		8).
valid_abstract_section_header('DATA SELECTION AND EXTRACTION',		8).
valid_abstract_section_header('DATA SET',		8).
valid_abstract_section_header('DATA SOURCES AND DATA EXTRACTION',		8).
valid_abstract_section_header('DATA SOURCES, EXTRACTION, AND SYNTHESIS',		8).
valid_abstract_section_header('DATA SOURCES/SYNTHESIS',		8).
valid_abstract_section_header('DESIGN AND STUDY SAMPLE',		8).
valid_abstract_section_header('DESIGN, MATERIAL AND METHODS',		8).
valid_abstract_section_header('DESIGN, PARTICIPANTS, MEASUREMENTS',		8).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS AND INTERVENTION',		8).
valid_abstract_section_header('DESIGN, SUBJECTS AND INTERVENTION',		8).
valid_abstract_section_header('DISCUSSION, CONCLUSION',		8).
valid_abstract_section_header('DURATION OF ACTIVE TREATMENT',		8).
valid_abstract_section_header('EDUCATIONAL VALUE',		8).
valid_abstract_section_header('EMPLACEMENT',		8).
valid_abstract_section_header('ENDORSEMENT',		8).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL EVIDENCE',		8).
valid_abstract_section_header('EXAMINEES AND METHODS',		8).
valid_abstract_section_header('EXPERIENCES',		8).
valid_abstract_section_header('EXPERIMENTAL SUBJECTS',		8).
valid_abstract_section_header('FUTURE WORK RECOMMENDATIONS',		8).
valid_abstract_section_header('Fundamento:',		8).
valid_abstract_section_header('GOALS OF THIS STUDY',		8).
valid_abstract_section_header('HINTERGRUND UND ZIELE',		8).
valid_abstract_section_header('HISTORY AND CLINICAL PRESENTATION',		8).
valid_abstract_section_header('HYPOTHESES/PURPOSE',		8).
valid_abstract_section_header('IMPLICATION FOR CANCER SURVIVORS',		8).
valid_abstract_section_header('IMPLICATIONS FOR CM PRACTICE',		8).
valid_abstract_section_header('IMPLICATIONS FOR PATIENT CARE',		8).
valid_abstract_section_header('INDICATIONS FOR USE',		8).
valid_abstract_section_header('INNOVATIONS',		8).
valid_abstract_section_header('INRODUCTION',		8).
valid_abstract_section_header('INTRODUCTION & OBJECTIVES',		8).
valid_abstract_section_header('INTRODUCTION AND MATERIAL',		8).
valid_abstract_section_header('INTRODUCTION/METHODS',		8).
valid_abstract_section_header('INTRODUCTION:',		8).
valid_abstract_section_header('INVESTIGATED GROUP',		8).
valid_abstract_section_header('KEY FINDINGS AND IMPLICATIONS',		8).
valid_abstract_section_header('KEY LEARNING POINT',		8).
valid_abstract_section_header('KEY MEASURES',		8).
valid_abstract_section_header('LABORATORY TESTS',		8).
valid_abstract_section_header('LESSONS LEARNED/NEXT STEPS',		8).
valid_abstract_section_header('MAIN OUTCOME MEASURES AND METHODS',		8).
valid_abstract_section_header('MAIN TOPICS',		8).
valid_abstract_section_header('MAJOR POINTS',		8).
valid_abstract_section_header('MATERIAL UND METHODEN',		8).
valid_abstract_section_header('MATERIALAND METHODS',		8).
valid_abstract_section_header('MEANS AND METHODS',		8).
valid_abstract_section_header('MEASUREMENTS AND OUTCOMES',		8).
valid_abstract_section_header('MEASURES AND ANALYSIS',		8).
valid_abstract_section_header('MECHANISMS OF ACTION',		8).
valid_abstract_section_header('METHODE',		8).
valid_abstract_section_header('METHODOLOGICAL APPROACH',		8).
valid_abstract_section_header('METHODOLOGICAL DESIGN',		8).
valid_abstract_section_header('METHODOLOGY AND PRINCIPAL FINDING',		8).
valid_abstract_section_header('METHODOLOGY PRINCIPAL FINDINGS',		8).
valid_abstract_section_header('METHODOLOGY/DESIGN',		8).
valid_abstract_section_header('METHODS/LITERATURE REVIEWED',		8).
valid_abstract_section_header('METHODS/SETTING',		8).
valid_abstract_section_header('MOTIVATION AND RESULTS',		8).
valid_abstract_section_header('Materials and Methods.',		8).
valid_abstract_section_header('NUMBERS',		8).
valid_abstract_section_header('OBJECTIVE AND SUMMARY BACKGROUND DATA',		8).
valid_abstract_section_header('OBJECTIVE S',		8).
valid_abstract_section_header('OBJECTIVE/GOAL',		8).
valid_abstract_section_header('OBJECTIVE/HYPOTHESES',		8).
valid_abstract_section_header('OBJECTIVES AND SETTING',		8).
valid_abstract_section_header('OBJECTIVES OF STUDY',		8).
valid_abstract_section_header('OBJECTIVES/GOAL',		8).
valid_abstract_section_header('OBJECTIVES/STUDY DESIGN',		8).
valid_abstract_section_header('OBJECTIVO',		8).
valid_abstract_section_header('OJECTIVE',		8).
valid_abstract_section_header('OPEN PEER REVIEW',		8).
valid_abstract_section_header('ORIGINS OF INFORMATION',		8).
valid_abstract_section_header('OUTCOME MEASUREMENT AND STATISTICAL ANALYSIS',		8).
valid_abstract_section_header('PARTICIPANTES',		8).
valid_abstract_section_header('PATIENT SAMPLE AND METHODOLOGY',		8).
valid_abstract_section_header('PERSONAL EXPERIENCE',		8).
valid_abstract_section_header('PHENOMENOLOGY SHOWN',		8).
valid_abstract_section_header('POPULATION, SAMPLE, SETTING',		8).
valid_abstract_section_header('PRACTICAL ATTITUDE',		8).
valid_abstract_section_header('PREOPERATIVE COUNSELING AND INFORMED CONSENT',		8).
valid_abstract_section_header('PRIMARY VARIABLES OF INTEREST',		8).
valid_abstract_section_header('PROBLEM CONSIDERED',		8).
valid_abstract_section_header('PROBLEM/OBJECTIVE',		8).
valid_abstract_section_header('PROCEDURES AND RESULTS',		8).
valid_abstract_section_header('PROSPECTIVE STUDY',		8).
valid_abstract_section_header('PROSPECTS AND PROJECTS',		8).
valid_abstract_section_header('PROSPERO REGISTRATION',		8).
valid_abstract_section_header('PURPOSE OF ARTICLE',		8).
valid_abstract_section_header('PURPOSE OF THE INVESTIGATION',		8).
valid_abstract_section_header('PURPOSE:',		8).
valid_abstract_section_header('PURPOSES/OBJECTIVES',		8).
valid_abstract_section_header('QUESTIONS UNDER STUDY / PRINCIPLES',		8).
valid_abstract_section_header('RATIONALE AND BACKGROUND',		8).
valid_abstract_section_header('RATIONALE FOR STUDY',		8).
valid_abstract_section_header('REASON FOR PERFORMING THE STUDY',		8).
valid_abstract_section_header('RECOMMENDATION AND PERSPECTIVES',		8).
valid_abstract_section_header('RECOMMENDATIONS AND PERSPECTIVE',		8).
valid_abstract_section_header('REFERENCES',		8).
valid_abstract_section_header('RELEVANT FINDINGS',		8).
valid_abstract_section_header('RESEARCH DESIGN & METHODS',		8).
valid_abstract_section_header('RESEARCH RECOMMENDATIONS',		8).
valid_abstract_section_header('SAMPLE SIZE ESTIMATES',		8).
valid_abstract_section_header('SEARCH STRATEGY & SOURCES',		8).
valid_abstract_section_header('SERIES SUMMARY',		8).
valid_abstract_section_header('SETTING AND STUDY POPULATION',		8).
valid_abstract_section_header('SETTINGS & DESIGN',		8).
valid_abstract_section_header('SIGNIFICANCE AND IMPORTANCE OF THE STUDY',		8).
valid_abstract_section_header('SOFTWARE AVAILABILITY',		8).
valid_abstract_section_header('SOLUTIONS',		8).
valid_abstract_section_header('STRENGTH-OF-RECOMMENDATION TAXONOMY SORT',		8).
valid_abstract_section_header('STUDY AREA',		8).
valid_abstract_section_header('STUDY DESIGN AND DATA COLLECTION',		8).
valid_abstract_section_header('STUDY DESIGN AND MEASUREMENTS',		8).
valid_abstract_section_header('STUDY DESIGN AND METHODOLOGY',		8).
valid_abstract_section_header('STUDY DESIGNS AND METHODS',		8).
valid_abstract_section_header('STUDY MATERIAL',		8).
valid_abstract_section_header('STUDY OBJECT',		8).
valid_abstract_section_header('STUDY PATIENTS',		8).
valid_abstract_section_header('SUBJECTS AND PARTICIPANTS',		8).
valid_abstract_section_header('SURGICAL METHOD',		8).
valid_abstract_section_header('SURGICAL PROCEDURES',		8).
valid_abstract_section_header('SURVEY SAMPLE',		8).
valid_abstract_section_header('SURVIVAL',		8).
valid_abstract_section_header('TECHNICAL ASPECTS',		8).
valid_abstract_section_header('TESTING OF THE HYPOTHESIS',		8).
valid_abstract_section_header('THE AIM OF THIS WORK',		8).
valid_abstract_section_header('THE AIMS',		8).
valid_abstract_section_header('THERAPEUTIC MANAGEMENT',		8).
valid_abstract_section_header('VEGF-A',		8).
valid_abstract_section_header('WHAT WILL THE READER GAIN',		8).
valid_abstract_section_header('WPROWADZENIE',		8).
valid_abstract_section_header('ABSTRACT OBJECTIVE',		7).
valid_abstract_section_header('ACCESS TO DATA',		7).
valid_abstract_section_header('ADMISSION FINDINGS',		7).
valid_abstract_section_header('ADULTS',		7).
valid_abstract_section_header('ADVERSE EFFECTS',		7).
valid_abstract_section_header('AIM OF REVIEW',		7).
valid_abstract_section_header('AIM OF THE DATABASE',		7).
valid_abstract_section_header('AIMS AND PURPOSE',		7).
valid_abstract_section_header('ANAMNESIS AND CLINICAL FINDINGS',		7).
valid_abstract_section_header('ANESTHESIA',		7).
valid_abstract_section_header('APPROACHES',		7).
valid_abstract_section_header('ARTICLE SELECTION',		7).
valid_abstract_section_header('ASSESSMENT TOOLS',		7).
valid_abstract_section_header('AUTHORS CONCLUSIONS',		7).
valid_abstract_section_header('BACKGROUND AND OBJETIVE',		7).
valid_abstract_section_header('BACKGROUND AND RESULTS',		7).
valid_abstract_section_header('BACKGROUND OBJECTIVES',		7).
valid_abstract_section_header('BACKGROUND, AIMS, AND SCOPE',		7).
valid_abstract_section_header('BACKGROUND, MATERIALS AND METHODS',		7).
valid_abstract_section_header('BACKGROUND-AIM',		7).
valid_abstract_section_header('BASIC DESIGN',		7).
valid_abstract_section_header('BENEFITS, HARM AND COSTS',		7).
valid_abstract_section_header('CAUTION',		7).
valid_abstract_section_header('CLINCAL QUESTION/LEVEL OF EVIDENCE',		7).
valid_abstract_section_header('CLINICAL APPLICATIONS',		7).
valid_abstract_section_header('CLINICAL TRIAL NUMBERS',		7).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION- URL',		7).
valid_abstract_section_header('CLINICALTRIALSGOV REGISTRATION NUMBER',		7).
valid_abstract_section_header('COMMENTARIES',		7).
valid_abstract_section_header('COMPARATORS',		7).
valid_abstract_section_header('COMPARISON',		7).
valid_abstract_section_header('CONCLUSION AND MESSAGE',		7).
valid_abstract_section_header('CONCLUSION/HYPOTHESIS',		7).
valid_abstract_section_header('CONCLUSIONS & SIGNIFICANCE',		7).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR CANCER SURVIVORS',		7).
valid_abstract_section_header('CONCLUSIONS AND OUTLOOK',		7).
valid_abstract_section_header('CONCLUSIONS SIGNIFICANCE',		7).
valid_abstract_section_header('CONCLUSIONS/IMPLICATIONS FOR PRACTICE:',		7).
valid_abstract_section_header('CONCLUSIONS/LEVEL OF EVIDENCE',		7).
valid_abstract_section_header('CONCULSION',		7).
valid_abstract_section_header('CRITIQUE',		7).
valid_abstract_section_header('DATA CAPTURE',		7).
valid_abstract_section_header('DATA COLLECTION METHOD',		7).
valid_abstract_section_header('DATA IDENTIFICATION AND SELECTION',		7).
valid_abstract_section_header('DATA QUALITY',		7).
valid_abstract_section_header('DATA SOURCES AND SELECTION CRITERIA',		7).
valid_abstract_section_header('DATA SOURCES AND SETTING',		7).
valid_abstract_section_header('DATA SOURCES AND STUDY DESIGN',		7).
valid_abstract_section_header('DATA SYNTHESES',		7).
valid_abstract_section_header('DEPENDENT VARIABLE',		7).
valid_abstract_section_header('DESCRIPTION OF PROJECT',		7).
valid_abstract_section_header('DESIGN AND STUDY SUBJECTS',		7).
valid_abstract_section_header('DESIGN, SETTING, AND METHODS',		7).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, & METHODS',		7).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS, AND MAIN OUTCOME MEASURE',		7).
valid_abstract_section_header('DESIGN, SUBJECTS, AND SETTING',		7).
valid_abstract_section_header('DIAGNOSTIC TESTS',		7).
valid_abstract_section_header('ELIGIBILITY CRITERIA FOR INCLUDED STUDIES',		7).
valid_abstract_section_header('ENDPOINT',		7).
valid_abstract_section_header('EVALUATIONS/MEASUREMENTS',		7).
valid_abstract_section_header('EXPERIMENTAL DATA',		7).
valid_abstract_section_header('FEASIBILITY',		7).
valid_abstract_section_header('FINDINGS AND IMPLICATIONS',		7).
valid_abstract_section_header('FUTURE AND PROJECTS',		7).
valid_abstract_section_header('GENERAL METHODS',		7).
valid_abstract_section_header('GENERAL QUESTION',		7).
valid_abstract_section_header('GG, OR',		7).
valid_abstract_section_header('GOAL AND METHODS',		7).
valid_abstract_section_header('GOALS AND OBJECTIVES',		7).
valid_abstract_section_header('GOV IDENTIFIERS',		7).
valid_abstract_section_header('GUIDELINE QUESTIONS',		7).
valid_abstract_section_header('IMPACT OF THE STUDY',		7).
valid_abstract_section_header('INNOVATION AND IMPLICATIONS',		7).
valid_abstract_section_header('INSTRUMENTS AND METHODS',		7).
valid_abstract_section_header('INTERVENTION/OUTCOME',		7).
valid_abstract_section_header('INTRO',		7).
valid_abstract_section_header('INTRODUTION',		7).
valid_abstract_section_header('Introduction',		7).
valid_abstract_section_header('JEL CODES',		7).
valid_abstract_section_header('LESSONS AND MESSAGES',		7).
valid_abstract_section_header('LEVEL II',		7).
valid_abstract_section_header('LOCATIONS',		7).
valid_abstract_section_header('MAIN EXPOSURE MEASURE',		7).
valid_abstract_section_header('MAIN ISSUES',		7).
valid_abstract_section_header('MAIN OBSERVATIONS AND RESULTS',		7).
valid_abstract_section_header('MAIN OUTCOME FINDINGS',		7).
valid_abstract_section_header('MAIN OUTCOME MEASURES/RESULTS',		7).
valid_abstract_section_header('MAIN OUTCOME METHODS',		7).
valid_abstract_section_header('MANAGEMENT OF REFRACTORY DISEASE',		7).
valid_abstract_section_header('MEASUREMENT AND MAIN RESULT',		7).
valid_abstract_section_header('METHOD OF STUDY SELECTION',		7).
valid_abstract_section_header('METHODIK',		7).
valid_abstract_section_header('METHODOLOGICAL INNOVATIONS',		7).
valid_abstract_section_header('METHODS - STUDY SELECTION',		7).
valid_abstract_section_header('METHODS / DESIGN',		7).
valid_abstract_section_header('METHODS AND MAIN OUTCOMES',		7).
valid_abstract_section_header('METHODS, RESULTS',		7).
valid_abstract_section_header('METHODS/ACTIVITY',		7).
valid_abstract_section_header('METHODS/DESCRIPTION',		7).
valid_abstract_section_header('METHORDS',		7).
valid_abstract_section_header('METODA',		7).
valid_abstract_section_header('NEED AND PURPOSE OF REVIEW',		7).
valid_abstract_section_header('OBJECTIVE & METHODS',		7).
valid_abstract_section_header('OBJECTIVE/AIMS',		7).
valid_abstract_section_header('OBJECTIVES & METHODS',		7).
valid_abstract_section_header('OBJECTIVES AND GOAL',		7).
valid_abstract_section_header('OBJECTIVES AND RESULTS',		7).
valid_abstract_section_header('OBJECTIVES/ HYPOTHESIS',		7).
valid_abstract_section_header('OBJECTIVES/PURPOSE',		7).
valid_abstract_section_header('OBJECTVE',		7).
valid_abstract_section_header('OBSERVATIONAL PROCEDURE',		7).
valid_abstract_section_header('ORGANIZING CONSTRUCT AND METHODS',		7).
valid_abstract_section_header('ORIGINAL ARTICLE ABSTRACT',		7).
valid_abstract_section_header('ORIGINALITY/VALUE OF CHAPTER',		7).
valid_abstract_section_header('OUTCOMES MEASURE',		7).
valid_abstract_section_header('OUTCOMES/RESULTS',		7).
valid_abstract_section_header('PARTICIPANTS & SETTING',		7).
valid_abstract_section_header('PATHOLOGY',		7).
valid_abstract_section_header('PATIENT & METHODS',		7).
valid_abstract_section_header('PATIENT MATERIAL',		7).
valid_abstract_section_header('PATIENT, INTERVENTION, AND RESULTS',		7).
valid_abstract_section_header('PATIENTEN UND METHODIK',		7).
valid_abstract_section_header('PATIENTS AND MAIN OUTCOME MEASUREMENTS',		7).
valid_abstract_section_header('PATIENTS/DESIGN',		7).
valid_abstract_section_header('PERSONS',		7).
valid_abstract_section_header('PHASE I',		7).
valid_abstract_section_header('PRACTICAL IMPLICATION',		7).
valid_abstract_section_header('PREVENTIVE MEASURES',		7).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOMES MEASURES',		7).
valid_abstract_section_header('PRINCIPAL FINDINGS/METHODOLOGY',		7).
valid_abstract_section_header('PROBLEM AND OBJECTIVE',		7).
valid_abstract_section_header('PROBLEMA',		7).
valid_abstract_section_header('PROPHYLAXIS',		7).
valid_abstract_section_header('PROPOSALS',		7).
valid_abstract_section_header('PROPOSED METHOD',		7).
valid_abstract_section_header('PURPOSE AND PATIENTS',		7).
valid_abstract_section_header('PURPOSES OF THE STUDY',		7).
valid_abstract_section_header('QUALITY ASSESSMENT',		7).
valid_abstract_section_header('QUALITY ISSUE',		7).
valid_abstract_section_header('QUESTION OF THE STUDY',		7).
valid_abstract_section_header('QUESTION/PURPOSE',		7).
valid_abstract_section_header('QUESTIONS/HYPOTHESES',		7).
valid_abstract_section_header('RADIOTHERAPY',		7).
valid_abstract_section_header('RECENT STUDIES',		7).
valid_abstract_section_header('REFLECTIONS',		7).
valid_abstract_section_header('REGISTRATION ID IN IRCT',		7).
valid_abstract_section_header('REGISTRY',		7).
valid_abstract_section_header('REHABILITATION',		7).
valid_abstract_section_header('RELEVANCE AND NOVEL INFORMATION',		7).
valid_abstract_section_header('RELEVANCE TO INDUSTRY',		7).
valid_abstract_section_header('RESEARCH AIM',		7).
valid_abstract_section_header('RESEARCH IMPLICATIONS/LIMITATIONS',		7).
valid_abstract_section_header('RESEARCH PROBLEM',		7).
valid_abstract_section_header('RESULTATS',		7).
valid_abstract_section_header('RESULTS/SIGNIFICANCE',		7).
valid_abstract_section_header('RESUMEN',		7).
valid_abstract_section_header('REVIEW PROCESS',		7).
valid_abstract_section_header('RISKS',		7).
valid_abstract_section_header('SCIENTIFIC QUESTION',		7).
valid_abstract_section_header('SETTING, DESIGN, AND PATIENTS',		7).
valid_abstract_section_header('SETTINGS/SUBJECTS',		7).
valid_abstract_section_header('SIGNIFICANT OUTCOMES',		7).
valid_abstract_section_header('SPECIAL FEATURES',		7).
valid_abstract_section_header('STRATEGIES',		7).
valid_abstract_section_header('STUDY 1',		7).
valid_abstract_section_header('STUDY DESIGN & SETTING',		7).
valid_abstract_section_header('STUDY DESIGN, SETTING, AND PATIENTS',		7).
valid_abstract_section_header('STUDY POPULATIONS',		7).
valid_abstract_section_header('STUDY SAMPLES',		7).
valid_abstract_section_header('STUDY SELECTION CRITERIA',		7).
valid_abstract_section_header('STUDY SUBJECTS AND METHODS',		7).
valid_abstract_section_header('STUDY/PRINCIPLES',		7).
valid_abstract_section_header('SUBJECTS AND TREATMENT',		7).
valid_abstract_section_header('SUMMARY OF DATA',		7).
valid_abstract_section_header('TECHNOLOGY',		7).
valid_abstract_section_header('THE HYPOTHESIS',		7).
valid_abstract_section_header('THE PURPOSE OF THE RESEARCH',		7).
valid_abstract_section_header('THE PURPOSE OF THIS STUDY',		7).
valid_abstract_section_header('THERAPEUTIC STRATEGIES',		7).
valid_abstract_section_header('TRIAL REGISTER NUMBER',		7).
valid_abstract_section_header('TWEAK',		7).
valid_abstract_section_header('WHAT IS KNOWN AND BACKGROUND',		7).
valid_abstract_section_header('WIDER IMPLICATIONS OF THE FINDING',		7).
valid_abstract_section_header('ABSTARCT',		6).
valid_abstract_section_header('AIM/DESIGN',		6).
valid_abstract_section_header('APPROACH AND METHODS',		6).
valid_abstract_section_header('AREAS OF UNCERTAINTY',		6).
valid_abstract_section_header('AUSTRALIAN NEW ZEALAND CLINICAL TRIALS REGISTRY',		6).
valid_abstract_section_header('AUTHORS',		6).
valid_abstract_section_header('AVAILABILITY AND SUPPLEMENTARY INFORMATION',		6).
valid_abstract_section_header('Aim',		6).
valid_abstract_section_header('BACKGRAUND',		6).
valid_abstract_section_header('BACKGROUND',		6).
valid_abstract_section_header('BACKGROUND AND PROPOSE',		6).
valid_abstract_section_header('BACKGROUND/PURPOSE OF THE STUDY',		6).
valid_abstract_section_header('BASELINE RESULTS',		6).
valid_abstract_section_header('Background and Objectives',		6).
valid_abstract_section_header('CASES AND METHOD',		6).
valid_abstract_section_header('CAUSES',		6).
valid_abstract_section_header('CENTER CONDUCTING THE REVIEW',		6).
valid_abstract_section_header('CENTERS',		6).
valid_abstract_section_header('CHEMOTHERAPY',		6).
valid_abstract_section_header('CLINICAL ASSESSMENT',		6).
valid_abstract_section_header('CLINICAL CHARACTERISTICS',		6).
valid_abstract_section_header('CLINICAL EVIDENCE',		6).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION NO',		6).
valid_abstract_section_header('CLINICALTRIALSGOV ID',		6).
valid_abstract_section_header('COMPOSITION OF THE COMMITTEE',		6).
valid_abstract_section_header('CONCLUSION AND GLOBAL HEALTH IMPLICATIONS',		6).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS OF KEY FINDINGS',		6).
valid_abstract_section_header('CONCLUSION AND PRACTICAL IMPLICATIONS',		6).
valid_abstract_section_header('CONCLUSION OF THE OPINION',		6).
valid_abstract_section_header('CONCLUSIONS / SIGNIFICANCE',		6).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR PUBLIC HEALTH PRACTICE',		6).
valid_abstract_section_header('CONCLUSIONS AND PRACTICAL APPLICATIONS',		6).
valid_abstract_section_header('CONCLUSIONS AND RECOMMENDATION',		6).
valid_abstract_section_header('CONCLUSIONS/FINDINGS',		6).
valid_abstract_section_header('CONDITION',		6).
valid_abstract_section_header('CONFLICT-OF-INTEREST STATEMENT',		6).
valid_abstract_section_header('CONSEQUENCE',		6).
valid_abstract_section_header('CONTENT ANALYSIS OF LITERATURE',		6).
valid_abstract_section_header('CONTRAINDICATION',		6).
valid_abstract_section_header('CONTRIBUTION',		6).
valid_abstract_section_header('CREDIBILITY',		6).
valid_abstract_section_header('CURRENT SITUATION AND SALIENT POINTS',		6).
valid_abstract_section_header('D HAMILTONI',		6).
valid_abstract_section_header('DATA COLLECTION & ANALYSIS',		6).
valid_abstract_section_header('DATA SOURCE/STUDY DESIGN',		6).
valid_abstract_section_header('DATA SOURCES/DATA COLLECTION',		6).
valid_abstract_section_header('DATA/RESULTS',		6).
valid_abstract_section_header('DATA/SOURCES',		6).
valid_abstract_section_header('DATE OF FIRST PATIENT\'S ENROLLMENT',		6).
valid_abstract_section_header('DESIGN, PARTICIPANTS, AND INTERVENTIONS',		6).
valid_abstract_section_header('DESIGN, PATIENTS, AND MAIN OUTCOME MEASURES',		6).
valid_abstract_section_header('DESIGN, SETTING AND METHODS',		6).
valid_abstract_section_header('DESIGN, SETTING, AND PARTICIPANT',		6).
valid_abstract_section_header('DESIGN, SETTINGS, AND SUBJECTS',		6).
valid_abstract_section_header('DISCUSSION AND IMPLICATIONS FOR PRACTICE',		6).
valid_abstract_section_header('DONORS AND METHODS',		6).
valid_abstract_section_header('EDUCATION',		6).
valid_abstract_section_header('EMERGING AREAS FOR DEVELOPING RESEARCH',		6).
valid_abstract_section_header('ENTORNO',		6).
valid_abstract_section_header('EPIDEMIOLOGICAL DATA',		6).
valid_abstract_section_header('EPILOGUE',		6).
valid_abstract_section_header('ESSENTIAL RESULTS',		6).
valid_abstract_section_header('EUDRACT NUMBER',		6).
valid_abstract_section_header('EVALUATION DESIGN',		6).
valid_abstract_section_header('EXPERIMENTAL OBJECTIVES',		6).
valid_abstract_section_header('EXPOSURE MEASURES',		6).
valid_abstract_section_header('FACTS',		6).
valid_abstract_section_header('FOUNDATION',		6).
valid_abstract_section_header('FUTURE PROSPECT AND PROJECTS',		6).
valid_abstract_section_header('GOALS, SCOPE AND BACKGROUND',		6).
valid_abstract_section_header('HISTORIQUE ET OBJECTIF',		6).
valid_abstract_section_header('HISTORY AND FINDINGS ON ADMISSION',		6).
valid_abstract_section_header('HOW',		6).
valid_abstract_section_header('IMPACTS',		6).
valid_abstract_section_header('IMPLICATION FOR FURTHER RESEARCH',		6).
valid_abstract_section_header('IMPLICATIONS FOR NURSING AND/OR HEALTH POLICY',		6).
valid_abstract_section_header('IMPLICATIONS FOR PRACTISE',		6).
valid_abstract_section_header('IN THE FUTURE',		6).
valid_abstract_section_header('IN VIVO STUDIES',		6).
valid_abstract_section_header('INDIVIDUALS',		6).
valid_abstract_section_header('INDIVIDUALS AND METHODS',		6).
valid_abstract_section_header('INFORMATION',		6).
valid_abstract_section_header('INJURY PATTERNS',		6).
valid_abstract_section_header('INSTITUTION',		6).
valid_abstract_section_header('INTEGRATIVE SIGNIFICANCE',		6).
valid_abstract_section_header('INTERVENTION AND TESTING',		6).
valid_abstract_section_header('INTERVENTIONS AND METHODS',		6).
valid_abstract_section_header('INTERVENTIONS, MEASUREMENTS, AND MAIN RESULTS',		6).
valid_abstract_section_header('INTRODUCTION:',		6).
valid_abstract_section_header('INVESTIGATIONS, DIAGNOSIS AND TREATMENT',		6).
valid_abstract_section_header('INVESTIGATIONS, TREATMENT AND COURSE',		6).
valid_abstract_section_header('KEY ISSUE(S)',		6).
valid_abstract_section_header('KEYPOINTS',		6).
valid_abstract_section_header('LIMITATIONS AND REASON FOR CAUTION',		6).
valid_abstract_section_header('LIMITATIONS, REASONS FOR CAUTIONS',		6).
valid_abstract_section_header('LIMITES',		6).
valid_abstract_section_header('MAIN METHODS AND KEY FINDINGS',		6).
valid_abstract_section_header('MATERIAL AND SURGICAL TECHNIQUE',		6).
valid_abstract_section_header('MATERIAL, PATIENTS AND METHODS',		6).
valid_abstract_section_header('MATERIALS AND METHODS/RESULTS',		6).
valid_abstract_section_header('MATERIALS, METHODS',		6).
valid_abstract_section_header('MATERIALS/METHOD',		6).
valid_abstract_section_header('MATHERIAL AND METHODS',		6).
valid_abstract_section_header('MEDICAL TREATMENT',		6).
valid_abstract_section_header('METHDOS',		6).
valid_abstract_section_header('METHODES',		6).
valid_abstract_section_header('METHODOLOGY & PRINCIPAL FINDINGS',		6).
valid_abstract_section_header('METHODOLOGY/MAIN RESULTS',		6).
valid_abstract_section_header('METHODS AND CONCLUSIONS',		6).
valid_abstract_section_header('METHODS AND FOCUS',		6).
valid_abstract_section_header('METHODS AND MAIN FINDINGS',		6).
valid_abstract_section_header('METHODS AND STUDY POPULATION',		6).
valid_abstract_section_header('METHODS, RESULTS AND CONCLUSIONS',		6).
valid_abstract_section_header('METHODS/PRINCIPAL FINDING',		6).
valid_abstract_section_header('MODELS',		6).
valid_abstract_section_header('NEW INFORMATION PROVIDED',		6).
valid_abstract_section_header('NOTE',		6).
valid_abstract_section_header('OBJECT AND METHODS',		6).
valid_abstract_section_header('OBJECT OF THE STUDY',		6).
valid_abstract_section_header('OBJECTIVE & DESIGN',		6).
valid_abstract_section_header('OBJECTIVE AND METHODOLOGY',		6).
valid_abstract_section_header('OBJECTIVE, DESIGN AND METHODS',		6).
valid_abstract_section_header('OBJECTIVE/PATIENTS',		6).
valid_abstract_section_header('OBJECTIVES AND PATIENTS',		6).
valid_abstract_section_header('OF BACKGROUND DATA',		6).
valid_abstract_section_header('OPERATIVE PROCEDURE',		6).
valid_abstract_section_header('OUTCOME & MEASUREMENT',		6).
valid_abstract_section_header('OUTCOME MEASUREMENTS AND STATISTICAL ANALYSES',		6).
valid_abstract_section_header('OUTCOMES MEASUREMENTS AND STATISTICAL ANALYSIS',		6).
valid_abstract_section_header('OUTCOMES SUMMARY',		6).
valid_abstract_section_header('OUTPUT',		6).
valid_abstract_section_header('OUTPUTS',		6).
valid_abstract_section_header('OVERALL APPROACH TO QUALITY AND SAFETY',		6).
valid_abstract_section_header('PARTICIPANTS, DESIGN AND SETTING',		6).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTING METHODS',		6).
valid_abstract_section_header('PATIENT HISTORY',		6).
valid_abstract_section_header('PATIENT SAMPLES',		6).
valid_abstract_section_header('PATIENT/METHOD',		6).
valid_abstract_section_header('PATIENTS & METHOD',		6).
valid_abstract_section_header('PATIENTS (OR PARTICIPANTS)',		6).
valid_abstract_section_header('PATIENTS AND TECHNIQUE',		6).
valid_abstract_section_header('PATIENTS/MATERIAL AND METHOD',		6).
valid_abstract_section_header('PHARMACODYNAMIC PROPERTIES',		6).
valid_abstract_section_header('PHARMACOKINETIC PROPERTIES',		6).
valid_abstract_section_header('PHASE II',		6).
valid_abstract_section_header('POTENTIAL DIFFICULTIES',		6).
valid_abstract_section_header('PRACTICE PEARL',		6).
valid_abstract_section_header('PREOPERATIVE WORK UP',		6).
valid_abstract_section_header('PRIMARY FINDINGS',		6).
valid_abstract_section_header('PRIMARY OUTCOME VARIABLES',		6).
valid_abstract_section_header('PROGRAMME EVALUATION',		6).
valid_abstract_section_header('PUPOSE',		6).
valid_abstract_section_header('PURPOSE AND SCOPE',		6).
valid_abstract_section_header('RATIONALE AIMS AND OBJECTIVES',		6).
valid_abstract_section_header('RECENT DATA',		6).
valid_abstract_section_header('REFERENCE TEST OR OUTCOME',		6).
valid_abstract_section_header('REGULATORY STATUS',		6).
valid_abstract_section_header('REQUIREMENTS',		6).
valid_abstract_section_header('RESEARCH, DESIGN AND METHODS',		6).
valid_abstract_section_header('RESLUTS',		6).
valid_abstract_section_header('RESULTS AND OUTCOMES',		6).
valid_abstract_section_header('RESULTS/STATISTICS',		6).
valid_abstract_section_header('RESUTS',		6).
valid_abstract_section_header('Result',		6).
valid_abstract_section_header('SCIENTIFIC ABSTRACT',		6).
valid_abstract_section_header('SCOPE OF THE PROBLEM',		6).
valid_abstract_section_header('SETTING AND DESIGNS',		6).
valid_abstract_section_header('SETTING AND RESULTS',		6).
valid_abstract_section_header('SETTING AND SUBJECT',		6).
valid_abstract_section_header('SETTING, PATIENTS',		6).
valid_abstract_section_header('SIGNIFICANCE AND THE IMPACT OF THE STUDY',		6).
valid_abstract_section_header('SIGNIFICANCE OF RESEARCH',		6).
valid_abstract_section_header('SIMILAR CASES PUBLISHED',		6).
valid_abstract_section_header('SOURCES OF EVIDENCE',		6).
valid_abstract_section_header('SPECIFIC RESEARCH CHALLENGE',		6).
valid_abstract_section_header('STUDIES INCLUDED',		6).
valid_abstract_section_header('STUDY APPRAISAL AND SYNTHESIS METHOD',		6).
valid_abstract_section_header('STUDY DESIGN, SETTING AND SUBJECTS',		6).
valid_abstract_section_header('STUDY GROUP AND METHODS',		6).
valid_abstract_section_header('STUDY GROUPS',		6).
valid_abstract_section_header('STUDY IDENTIFICATION',		6).
valid_abstract_section_header('STUDY LIMITATION',		6).
valid_abstract_section_header('STUDY OBJECTIVE AND DESIGN',		6).
valid_abstract_section_header('STUDY POPULATION AND DESIGN',		6).
valid_abstract_section_header('SUBJECTS OR PARTICIPANTS',		6).
valid_abstract_section_header('SUBJECTS/INTERVENTIONS',		6).
valid_abstract_section_header('SUGGESTIONS',		6).
valid_abstract_section_header('SUMMARY OF IMPORTANT FINDINGS',		6).
valid_abstract_section_header('SUMMARYOF BACKGROUND DATA',		6).
valid_abstract_section_header('SUPPORT',		6).
valid_abstract_section_header('SURVEILLANCE',		6).
valid_abstract_section_header('Subjects and Method',		6).
valid_abstract_section_header('TC/TT',		6).
valid_abstract_section_header('TECHNICAL REPORT',		6).
valid_abstract_section_header('THE GOAL',		6).
valid_abstract_section_header('THEME',		6).
valid_abstract_section_header('THERAPEUTIC STRATEGY',		6).
valid_abstract_section_header('THERAPY AND RESULTS',		6).
valid_abstract_section_header('TIMING',		6).
valid_abstract_section_header('TOLERANCE',		6).
valid_abstract_section_header('TOOLS AND METHODS',		6).
valid_abstract_section_header('TREATMENT PROTOCOL',		6).
valid_abstract_section_header('TREATMENT RECOMMENDATIONS',		6).
valid_abstract_section_header('TREATMENT/OUTCOME',		6).
valid_abstract_section_header('TRIAL',		6).
valid_abstract_section_header('TRIAL REGISTRY NUMBER',		6).
valid_abstract_section_header('TT/TC',		6).
valid_abstract_section_header('TYPE OF PARTICIPANT',		6).
valid_abstract_section_header('TYPES OF OUTCOMES',		6).
valid_abstract_section_header('VIEWPOINT AND CONCLUSION',		6).
valid_abstract_section_header('ABSRACT',		5).
valid_abstract_section_header('ACCESS TO RESEARCH MATERIALS',		5).
valid_abstract_section_header('ACKNOWLEDGMENT',		5).
valid_abstract_section_header('ACTION STATEMENTS',		5).
valid_abstract_section_header('ACTIONS',		5).
valid_abstract_section_header('ACTIVITY',		5).
valid_abstract_section_header('ADVANCE IN KNOWLEDGE',		5).
valid_abstract_section_header('AIM & METHODS',		5).
valid_abstract_section_header('AIM AND GOALS',		5).
valid_abstract_section_header('AIM AND PURPOSE',		5).
valid_abstract_section_header('AIM OF THE STUDY AND METHODS',		5).
valid_abstract_section_header('AIM(S) OF THE STUDY',		5).
valid_abstract_section_header('AIM/INTRODUCTION',		5).
valid_abstract_section_header('AIM/OBJECTIVES/BACKGROUND',		5).
valid_abstract_section_header('AIMS AND RESULTS',		5).
valid_abstract_section_header('APPARATUS',		5).
valid_abstract_section_header('APPLICATION/CONCLUSIONS',		5).
valid_abstract_section_header('APPRAISAL AND SYNTHESIS METHODS',		5).
valid_abstract_section_header('APPROACHES AND RESULTS',		5).
valid_abstract_section_header('AUTHOR SUMMARY',		5).
valid_abstract_section_header('BACGROUND',		5).
valid_abstract_section_header('BACKGROUND OR PURPOSE',		5).
valid_abstract_section_header('BACKGROUND/AIMS AND METHODS',		5).
valid_abstract_section_header('BASIC SCIENCE ADVANCES',		5).
valid_abstract_section_header('BENEFITS, HARMS, COSTS',		5).
valid_abstract_section_header('BEST PRACTICES',		5).
valid_abstract_section_header('BIOLOGICAL MECHANISMS',		5).
valid_abstract_section_header('CA/CC',		5).
valid_abstract_section_header('CALCULATIONS',		5).
valid_abstract_section_header('CASE FINDINGS/RESULTS',		5).
valid_abstract_section_header('CASE REPORT AND LITERATURE REVIEW',		5).
valid_abstract_section_header('CASE REPORT AND METHOD',		5).
valid_abstract_section_header('CASE REPORT/METHODS',		5).
valid_abstract_section_header('CASES PRESENTATIONS',		5).
valid_abstract_section_header('CG/CC',		5).
valid_abstract_section_header('CHAPTERS',		5).
valid_abstract_section_header('CLINICAL DETAILS',		5).
valid_abstract_section_header('CLINICAL DIAGNOSIS',		5).
valid_abstract_section_header('CLINICAL IMPACT',		5).
valid_abstract_section_header('CLINICAL MANAGEMENT',		5).
valid_abstract_section_header('CLINICAL SETTINGS',		5).
valid_abstract_section_header('CLINICAL SIGNIFICANCE STATEMENT',		5).
valid_abstract_section_header('CLINICAL SUMMARY',		5).
valid_abstract_section_header('CLINICAL TRIALS REGISTRATION INFORMATION',		5).
valid_abstract_section_header('COMPARISON TO EXISTING METHOD',		5).
valid_abstract_section_header('COMPETING INTERESTS',		5).
valid_abstract_section_header('CONCLUSION/RELEVANCE',		5).
valid_abstract_section_header('CONCLUSION:',		5).
valid_abstract_section_header('CONCLUSSION',		5).
valid_abstract_section_header('CONCUSIONS',		5).
valid_abstract_section_header('CONSLUSIONS',		5).
valid_abstract_section_header('CONSORT',		5).
valid_abstract_section_header('CONSTATATIONS',		5).
valid_abstract_section_header('CONTEXT & OBJECTIVE',		5).
valid_abstract_section_header('CONTEXT:',		5).
valid_abstract_section_header('CONTROVERSIAL ISSUES',		5).
valid_abstract_section_header('COURSE AND TREATMENT',		5).
valid_abstract_section_header('COVER PICTURE',		5).
valid_abstract_section_header('DATA ANALYSIS METHOD',		5).
valid_abstract_section_header('DATA EXTRACTION METHOD',		5).
valid_abstract_section_header('DATA SOURCES AND ELIGIBILITY',		5).
valid_abstract_section_header('DATA SOURCES AND STUDY ELIGIBILITY CRITERIA',		5).
valid_abstract_section_header('DATA SYNTHESIS/METHODS',		5).
valid_abstract_section_header('DATA, SOURCES AND STUDY SELECTION',		5).
valid_abstract_section_header('DECLARATION OF INTERESTS',		5).
valid_abstract_section_header('DEPENDENT MEASURES',		5).
valid_abstract_section_header('DESCRIPTION OF POLICY PRACTICE',		5).
valid_abstract_section_header('DESCRIPTION OF TECHNOLOGY/THERAPY',		5).
valid_abstract_section_header('DESCRIPTION OF THE STUDY',		5).
valid_abstract_section_header('DESCRIPTION OF THE TECHNIQUE',		5).
valid_abstract_section_header('DESIGN AND SCOPE',		5).
valid_abstract_section_header('DESIGN AND VOLUNTEERS',		5).
valid_abstract_section_header('DESIGN SETTING AND SUBJECTS',		5).
valid_abstract_section_header('DESIGN SETTING PARTICIPANTS',		5).
valid_abstract_section_header('DESIGN, PARTICIPANTS, AND MEASURES',		5).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, AND MAIN OUTCOME MEASURES',		5).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS, INTERVENTIONS',		5).
valid_abstract_section_header('DESIGN, SETTINGS, PARTICIPANTS',		5).
valid_abstract_section_header('DESIGN/METHOD/APPROACH',		5).
valid_abstract_section_header('DESIGN/OUTCOME MEASURES',		5).
valid_abstract_section_header('DESIGN/SETTINGS/PARTICIPANTS',		5).
valid_abstract_section_header('DEVELOPING RECOMMENDATIONS',		5).
valid_abstract_section_header('DEVELOPMENT AND CONCLUSION',		5).
valid_abstract_section_header('DEVICES',		5).
valid_abstract_section_header('DIAGNOSIS AND COURSE',		5).
valid_abstract_section_header('DIAGNOSIS AND MANAGEMENT',		5).
valid_abstract_section_header('DISSEMINATION AND ETHICS',		5).
valid_abstract_section_header('ELIGIBILITY',		5).
valid_abstract_section_header('ETHNOPHARMACOLOGY',		5).
valid_abstract_section_header('EVALUATION OF THE HYPOTHESIS',		5).
valid_abstract_section_header('EVALUATION RESULTS',		5).
valid_abstract_section_header('EVIDENCE AND CONSENSUS PROCESS',		5).
valid_abstract_section_header('EXPERIMENTS AND RESULTS',		5).
valid_abstract_section_header('EXPLANATORY MEASURES',		5).
valid_abstract_section_header('Evidence Acquisition',		5).
valid_abstract_section_header('FINAL REMARKS',		5).
valid_abstract_section_header('FINDINGS AND PRACTICE IMPLICATIONS',		5).
valid_abstract_section_header('FINDINGS AND RECOMMENDATIONS',		5).
valid_abstract_section_header('FINDINGS/DISCUSSION',		5).
valid_abstract_section_header('GENOME',		5).
valid_abstract_section_header('GOAL, SCOPE, AND BACKGROUND',		5).
valid_abstract_section_header('GROUP',		5).
valid_abstract_section_header('GROUP AND METHODS',		5).
valid_abstract_section_header('GROUP OF PATIENTS AND METHODS',		5).
valid_abstract_section_header('HEADLINE RESULTS',		5).
valid_abstract_section_header('HEALTH PROBLEM',		5).
valid_abstract_section_header('HISTORICAL BACKGROUND',		5).
valid_abstract_section_header('HISTORICAL PERSPECTIVE',		5).
valid_abstract_section_header('HUMAN STUDIES',		5).
valid_abstract_section_header('IMPACT ON RESEARCH, PRACTICE, AND POLICY',		5).
valid_abstract_section_header('IMPACT ON THE INDUSTRY',		5).
valid_abstract_section_header('IMPACT ON TRAFFIC SAFETY',		5).
valid_abstract_section_header('IMPLICATIONS AND ACTION',		5).
valid_abstract_section_header('IMPLICATIONS OF CANCER SURVIVORS',		5).
valid_abstract_section_header('IMPORTANCE AND OBJECTIVE',		5).
valid_abstract_section_header('IN CONCLUSIONS',		5).
valid_abstract_section_header('IN VITRO',		5).
valid_abstract_section_header('IN VIVO',		5).
valid_abstract_section_header('INTENTION',		5).
valid_abstract_section_header('INTERVENTION(S) AND MAIN OUTCOME MEASURE(S)',		5).
valid_abstract_section_header('INTERVENTION/TECHNIQUE',		5).
valid_abstract_section_header('INTERVENTIONS AND MAIN OUTCOME MEASUREMENTS',		5).
valid_abstract_section_header('INTODUCTION',		5).
valid_abstract_section_header('INTRODUCION',		5).
valid_abstract_section_header('INTRODUCTION AND AIM OF STUDY',		5).
valid_abstract_section_header('INTRODUCTION AND CLINICAL CASES',		5).
valid_abstract_section_header('INVESTIGATION AND DIAGNOSIS',		5).
valid_abstract_section_header('INVESTIGATIONS AND TREATMENT',		5).
valid_abstract_section_header('Introduction:',		5).
valid_abstract_section_header('LARGE-SCALE DATA',		5).
valid_abstract_section_header('LIMITATIONS AND CONCLUSION',		5).
valid_abstract_section_header('MAIN CONTRIBUTIONS',		5).
valid_abstract_section_header('MAIN FINDINGS AND CONCLUSIONS',		5).
valid_abstract_section_header('MAIN INDEPENDENT VARIABLES',		5).
valid_abstract_section_header('MAIN MEASURES AND RESULTS',		5).
valid_abstract_section_header('MAIN OUTCOME CRITERIA',		5).
valid_abstract_section_header('MAIN RESEARCH CLASSIFICATIONS',		5).
valid_abstract_section_header('MAIN VARIABLES EXAMINED',		5).
valid_abstract_section_header('MAJOR CONCLUSIONS AND GENERAL SIGNIFICANCE',		5).
valid_abstract_section_header('MATERIAL AND DISCUSSION',		5).
valid_abstract_section_header('MATERIAL UND METHODS',		5).
valid_abstract_section_header('MATERIALS &METHODS',		5).
valid_abstract_section_header('MATERIALS AND INTERVENTIONS',		5).
valid_abstract_section_header('MATERIALS AND METHODS, RESULTS',		5).
valid_abstract_section_header('MATERIALS, SETTING AND METHODS',		5).
valid_abstract_section_header('MEASUREMENT & RESULTS',		5).
valid_abstract_section_header('MEASURES AND OUTCOMES',		5).
valid_abstract_section_header('MEHODS',		5).
valid_abstract_section_header('METHOD AND PATIENT',		5).
valid_abstract_section_header('METHOD SUMMARY',		5).
valid_abstract_section_header('METHODOLOGY AND MAIN FINDINGS',		5).
valid_abstract_section_header('METHODOLOGY AND PATIENTS',		5).
valid_abstract_section_header('METHODOLOGY/PRINICIPAL FINDINGS',		5).
valid_abstract_section_header('METHODS & DESIGN',		5).
valid_abstract_section_header('METHODS AND KEY RESULTS',		5).
valid_abstract_section_header('METHODS AND OBSERVATIONS',		5).
valid_abstract_section_header('METHODS OF THE REVIEW',		5).
valid_abstract_section_header('METHODS RESULTS',		5).
valid_abstract_section_header('METHODS/OBJECTIVES',		5).
valid_abstract_section_header('METHODS/PRINCIPLE FINDINGS',		5).
valid_abstract_section_header('MODEL DESCRIPTION',		5).
valid_abstract_section_header('Materials and methods.',		5).
valid_abstract_section_header('Methods',		5).
valid_abstract_section_header('OBJECTIVE AND MOTIVATION',		5).
valid_abstract_section_header('OBJECTIVE AND PURPOSE',		5).
valid_abstract_section_header('OBJECTIVE OF THE PROGRAM',		5).
valid_abstract_section_header('OBJECTIVE/DESIGN/PATIENTS',		5).
valid_abstract_section_header('OBJECTIVE/SETTING',		5).
valid_abstract_section_header('OBJECTIVES OF THE REVIEW',		5).
valid_abstract_section_header('OBJECTIVES, PATIENTS AND METHODS',		5).
valid_abstract_section_header('OPINION',		5).
valid_abstract_section_header('OPPORTUNITIES',		5).
valid_abstract_section_header('ORGANIZATION',		5).
valid_abstract_section_header('ORIGINALITY/VALUE OF THE CHAPTER',		5).
valid_abstract_section_header('OUTCOMES & MEASURES',		5).
valid_abstract_section_header('PARTICIPANT/MATERIALS, SETTING, METHODS',		5).
valid_abstract_section_header('PARTICIPANTS AND PATIENTS',		5).
valid_abstract_section_header('PARTICIPANTS, INTERVENTIONS, AND MAIN OUTCOME MEASURES',		5).
valid_abstract_section_header('PAST MEDICAL HISTORY',		5).
valid_abstract_section_header('PATIENT DATA',		5).
valid_abstract_section_header('PATIENT(S) AND ANIMAL(S)',		5).
valid_abstract_section_header('PATIENT(S) AND INTERVENTION(S)',		5).
valid_abstract_section_header('PATIENT, METHODS AND RESULTS',		5).
valid_abstract_section_header('PATIENTS AND FINDINGS',		5).
valid_abstract_section_header('PATIENTS ET METHODS',		5).
valid_abstract_section_header('PATIENTS METHODS',		5).
valid_abstract_section_header('PATIENTS SUBJECTS AND METHODS',		5).
valid_abstract_section_header('PATIENTS--METHODS',		5).
valid_abstract_section_header('PHARMACOLOGY',		5).
valid_abstract_section_header('POINTS OF CONSENSUS',		5).
valid_abstract_section_header('POPULATION AND RESULTS',		5).
valid_abstract_section_header('POSITION STATEMENT',		5).
valid_abstract_section_header('POTENTIAL INTERVENTION',		5).
valid_abstract_section_header('POWER CALCULATIONS',		5).
valid_abstract_section_header('PREDICTOR VARIABLE',		5).
valid_abstract_section_header('PREDICTORS & OUTCOMES',		5).
valid_abstract_section_header('PREMESSA',		5).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME MEASURE',		5).
valid_abstract_section_header('PRIMARY PURPOSE',		5).
valid_abstract_section_header('PRINCIPAL LABORATORY FINDINGS',		5).
valid_abstract_section_header('PROBANDS',		5).
valid_abstract_section_header('PROBLEM AND BACKGROUND',		5).
valid_abstract_section_header('PROBLEM/BACKGROUND',		5).
valid_abstract_section_header('PROGRAM OUTCOMES',		5).
valid_abstract_section_header('PROGRAMME DESCRIPTION',		5).
valid_abstract_section_header('PROJECTS',		5).
valid_abstract_section_header('PROSPECTS',		5).
valid_abstract_section_header('PROVISIONAL CLINICAL OPINION',		5).
valid_abstract_section_header('RATIONAL AND OBJECTIVE',		5).
valid_abstract_section_header('RATIONALE AND PURPOSE',		5).
valid_abstract_section_header('RECENT LITERATURE DATA',		5).
valid_abstract_section_header('RECOMMENDATIONS AND CONCLUSIONS',		5).
valid_abstract_section_header('RELEVANCE/IMPACT',		5).
valid_abstract_section_header('RELIABILITY',		5).
valid_abstract_section_header('RESEARCH DESIGN AND CONTEXT',		5).
valid_abstract_section_header('RESEARCH DESIGN, SUBJECTS, MEASURES',		5).
valid_abstract_section_header('RESEARCH SETTING',		5).
valid_abstract_section_header('RESISTANCE',		5).
valid_abstract_section_header('RESULTS OF STUDIES',		5).
valid_abstract_section_header('RESULTS, DISCUSSION',		5).
valid_abstract_section_header('RESULTS/INTERPRETATION',		5).
valid_abstract_section_header('RESULTS/SUMMARY',		5).
valid_abstract_section_header('REULTS',		5).
valid_abstract_section_header('REVIEW AND DISCUSSION',		5).
valid_abstract_section_header('REVIEW AND UPDATING',		5).
valid_abstract_section_header('REVIEW FINDINGS',		5).
valid_abstract_section_header('SCIENTIFIC SIGNIFICANCE AND FUTURE DIRECTIONS',		5).
valid_abstract_section_header('SCOPE AND METHODS',		5).
valid_abstract_section_header('SCOPE OF THE STUDY',		5).
valid_abstract_section_header('SEARCH TERMS',		5).
valid_abstract_section_header('SECONDARY PREVENTION',		5).
valid_abstract_section_header('SERIES OUTLINE',		5).
valid_abstract_section_header('SETTING & DESIGN',		5).
valid_abstract_section_header('SETTING AND OBJECTIVES',		5).
valid_abstract_section_header('SETTING AND PATIENT(S)',		5).
valid_abstract_section_header('SETTING AND TYPE OF PARTICIPANTS',		5).
valid_abstract_section_header('SETTING USA SUBJECTS',		5).
valid_abstract_section_header('SETTING, PARTICIPANTS, AND MEASUREMENTS',		5).
valid_abstract_section_header('SHORT CONCLUSION',		5).
valid_abstract_section_header('SIGNIFICANCE/CONCLUSION',		5).
valid_abstract_section_header('SOCIAL HISTORY',		5).
valid_abstract_section_header('SPONSORSHIPS',		5).
valid_abstract_section_header('STARTPOINTS',		5).
valid_abstract_section_header('STATEMENT OF RELEVANCE',		5).
valid_abstract_section_header('STATEMENTS OF THE PROBLEM',		5).
valid_abstract_section_header('STATISTIC',		5).
valid_abstract_section_header('STUDY DESIGN & METHODS',		5).
valid_abstract_section_header('STUDY DESIGN AND SIZE',		5).
valid_abstract_section_header('STUDY DESIGN/DATA COLLECTION/EXTRACTION METHODS',		5).
valid_abstract_section_header('STUDY DESIGN/PATIENT AND METHODS',		5).
valid_abstract_section_header('STUDY EXPOSURE',		5).
valid_abstract_section_header('STUDY METHODOLOGY',		5).
valid_abstract_section_header('STUDY POPULATION AND SETTING',		5).
valid_abstract_section_header('STUDY SETTING AND DESIGN',		5).
valid_abstract_section_header('SUBJECTS & SETTING',		5).
valid_abstract_section_header('SUBJECTS, MAIN OUTCOME MEASURES',		5).
valid_abstract_section_header('SUBJECTS/MATERIALS',		5).
valid_abstract_section_header('SUBJECTS/METHOD',		5).
valid_abstract_section_header('SUMMARY AND DISCUSSION',		5).
valid_abstract_section_header('SUMMARY OF CASES',		5).
valid_abstract_section_header('SUMMARY/CONCLUSION',		5).
valid_abstract_section_header('SURGICAL APPROACH',		5).
valid_abstract_section_header('SURVEY INSTRUMENT',		5).
valid_abstract_section_header('SURVEY RESULTS',		5).
valid_abstract_section_header('SURVEYS',		5).
valid_abstract_section_header('Subjects and Methods',		5).
valid_abstract_section_header('TAKE HOME MESSAGES',		5).
valid_abstract_section_header('THE AIM OF OUR STUDY',		5).
valid_abstract_section_header('THE AIM OF THIS PAPER',		5).
valid_abstract_section_header('THE AIM OF WORK',		5).
valid_abstract_section_header('THE EXAMINEES AND METHODS',		5).
valid_abstract_section_header('THE METHOD',		5).
valid_abstract_section_header('THE PURPOSE OF THIS STUDY WAS TWOFOLD',		5).
valid_abstract_section_header('THE STUDY',		5).
valid_abstract_section_header('THERAPEUTICS',		5).
valid_abstract_section_header('THESIS',		5).
valid_abstract_section_header('TREATMENT AND FURTHER COURSE',		5).
valid_abstract_section_header('TREATMENT OPTIONS',		5).
valid_abstract_section_header('TRIAL REGISTRATION NUMBER ISRCTN',		5).
valid_abstract_section_header('VALEURS',		5).
valid_abstract_section_header('VALIDATING THE RECOMMENDATIONS',		5).
valid_abstract_section_header('VIEWPOINT AND CONCLUSIONS',		5).
valid_abstract_section_header('WHAT DOES THIS STUDY ADD',		5).
valid_abstract_section_header('WHAT THIS STUDY ADDS TO EXISTING KNOWLEDGE',		5).
valid_abstract_section_header('A PRETREATMENT RECORDS',		4).
valid_abstract_section_header('AA/AT',		4).
valid_abstract_section_header('ABBREVIATED DESCRIPTION OF THE STATE OF KNOWLEDGE',		4).
valid_abstract_section_header('ABSTACT',		4).
valid_abstract_section_header('ABSTRACT BACKGROUND',		4).
valid_abstract_section_header('ACCESS TO CARE',		4).
valid_abstract_section_header('ACQUIRING OF EVIDENCE',		4).
valid_abstract_section_header('ADVERSE EVENTS',		4).
valid_abstract_section_header('AIM AND SETTING',		4).
valid_abstract_section_header('AIM OF OUR STUDY',		4).
valid_abstract_section_header('AIMS & BACKGROUND',		4).
valid_abstract_section_header('AIMS & OBJECTIVE',		4).
valid_abstract_section_header('ALLOCATION',		4).
valid_abstract_section_header('ALTERNATE APPROACH',		4).
valid_abstract_section_header('ANIMALS STUDIED AND PROCEDURES',		4).
valid_abstract_section_header('APPLICATION TO PRACTICE',		4).
valid_abstract_section_header('APPRAISAL AND SYNTHESIS',		4).
valid_abstract_section_header('APPROACH & RESULTS',		4).
valid_abstract_section_header('ARTICLE ABSTRACT',		4).
valid_abstract_section_header('AT/AA',		4).
valid_abstract_section_header('AVAILABILTY',		4).
valid_abstract_section_header('B POST-TREATMENT RECORDS',		4).
valid_abstract_section_header('BACKGROND',		4).
valid_abstract_section_header('BACKGROUD/OBEJECTIVES',		4).
valid_abstract_section_header('BACKGROUND & PROBLEM',		4).
valid_abstract_section_header('BACKGROUND AND PURPOSE:',		4).
valid_abstract_section_header('BACKGROUND AND RESEARCH QUESTION',		4).
valid_abstract_section_header('BACKGROUND TO THE STUDY',		4).
valid_abstract_section_header('BACKGROUND&AIMS',		4).
valid_abstract_section_header('BACKGROUNDS/OBJECTIVES',		4).
valid_abstract_section_header('BMI (MD',		4).
valid_abstract_section_header('CADRE',		4).
valid_abstract_section_header('CARDIOVASCULAR RISK',		4).
valid_abstract_section_header('CASE (DESCRIPTION)',		4).
valid_abstract_section_header('CASE AND METHOD',		4).
valid_abstract_section_header('CASE AND RESULTS',		4).
valid_abstract_section_header('CASE AND REVIEW',		4).
valid_abstract_section_header('CASE DIAGNOSIS',		4).
valid_abstract_section_header('CASE EXAMPLES',		4).
valid_abstract_section_header('CASE PRESENTATION AND DISCUSSION',		4).
valid_abstract_section_header('CASE REPORT 1',		4).
valid_abstract_section_header('CASE REPORT AND REVIEW',		4).
valid_abstract_section_header('CASE RESULT',		4).
valid_abstract_section_header('CASE-DIAGNOSIS',		4).
valid_abstract_section_header('CHARACTERISTICS',		4).
valid_abstract_section_header('CHEMICAL COMPOUNDS',		4).
valid_abstract_section_header('CILJ',		4).
valid_abstract_section_header('CLINIC',		4).
valid_abstract_section_header('CLINICAL AND LABORATORY TESTS',		4).
valid_abstract_section_header('CLINICAL CASE AND CONCLUSIONS',		4).
valid_abstract_section_header('CLINICAL CASE REPORT',		4).
valid_abstract_section_header('CLINICAL CASES AND CONCLUSIONS',		4).
valid_abstract_section_header('CLINICAL COURSE AND THERAPY',		4).
valid_abstract_section_header('CLINICAL EXPERIENCE',		4).
valid_abstract_section_header('CLINICAL FINDINGS/PATIENT CONCERNS',		4).
valid_abstract_section_header('CLINICAL MATERIALS',		4).
valid_abstract_section_header('CLINICAL NURSING IMPLICATIONS',		4).
valid_abstract_section_header('CLINICAL PRESENTATION AND INTERVENTIONS',		4).
valid_abstract_section_header('CLINICAL QUESTIONS/LEVEL OF EVIDENCE',		4).
valid_abstract_section_header('CLINICAL REGISTRATION NUMBER',		4).
valid_abstract_section_header('CLINICAL TRIAL IDENTIFIER',		4).
valid_abstract_section_header('CLINICAL TRIAL NO',		4).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION-URL',		4).
valid_abstract_section_header('CLINICAL USE',		4).
valid_abstract_section_header('CLINICALTRIALSGOV IDENTIFIERS',		4).
valid_abstract_section_header('COMMENTARY AND CONCLUSION',		4).
valid_abstract_section_header('COMPARISON WITH OTHER METHODS',		4).
valid_abstract_section_header('CONCEPTS',		4).
valid_abstract_section_header('CONCLUISON',		4).
valid_abstract_section_header('CONCLUSIO',		4).
valid_abstract_section_header('CONCLUSION AND INTERPRETATION',		4).
valid_abstract_section_header('CONCLUSION AND NEXT STEPS',		4).
valid_abstract_section_header('CONCLUSION, SIGNIFICANCE AND IMPACT OF THE STUDY',		4).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR PRACTISE',		4).
valid_abstract_section_header('CONCLUSIONS AND REVELANCE',		4).
valid_abstract_section_header('CONCLUSIONS, SIGNIFICANCE AND IMPACT OF STUDY',		4).
valid_abstract_section_header('CONCLUSIONS/SIGNFICANCE',		4).
valid_abstract_section_header('CONCULSIONS',		4).
valid_abstract_section_header('CONCUSION',		4).
valid_abstract_section_header('CONSTRUCTION AND CONTENT',		4).
valid_abstract_section_header('CORPUS OF EVIDENCE',		4).
valid_abstract_section_header('COST-EFFECTIVENESS',		4).
valid_abstract_section_header('CPRS-R',		4).
valid_abstract_section_header('CURRENT CONTROLLED TRIALS',		4).
valid_abstract_section_header('CURRENT RESEARCH',		4).
valid_abstract_section_header('CURRICULUM',		4).
valid_abstract_section_header('Conclusion and Relevance',		4).
valid_abstract_section_header('Conclusiones:',		4).
valid_abstract_section_header('DATA & METHODS',		4).
valid_abstract_section_header('DATA DEPOSITION',		4).
valid_abstract_section_header('DATA SOURCES, STUDY SELECTION AND DATA EXTRACTION',		4).
valid_abstract_section_header('DATA SOURCES, STUDY SELECTION, AND DATA SYNTHESIS',		4).
valid_abstract_section_header('DATABASE USED',		4).
valid_abstract_section_header('DEBATE',		4).
valid_abstract_section_header('DESCRIPTION OF CARE PRACTICE',		4).
valid_abstract_section_header('DESCRIPTION OF THE INTERVENTION',		4).
valid_abstract_section_header('DESCRIPTION OF TOPIC',		4).
valid_abstract_section_header('DESIGN AND MATERIAL',		4).
valid_abstract_section_header('DESIGN AND OBJECTIVES',		4).
valid_abstract_section_header('DESIGN AND REVIEW METHODS',		4).
valid_abstract_section_header('DESIGN SETTING AND PATIENTS',		4).
valid_abstract_section_header('DESIGN SETTING PARTICIPANTS AND MEASUREMENTS',		4).
valid_abstract_section_header('DESIGN, METHODS AND RESULTS',		4).
valid_abstract_section_header('DESIGN, PARTICIPANTS, AND MEASUREMENTS',		4).
valid_abstract_section_header('DESIGN, SETTING & PATIENTS',		4).
valid_abstract_section_header('DESIGN, SETTING, SUBJECTS, OUTCOME MEASURES',		4).
valid_abstract_section_header('DESIGN, SUBJECTS, AND METHODS',		4).
valid_abstract_section_header('DESIGN/PARTICIPANTS/MEASUREMENTS',		4).
valid_abstract_section_header('DESIGN/RESULTS',		4).
valid_abstract_section_header('DESING',		4).
valid_abstract_section_header('DIAGNOSTIC FINDINGS AND THERAPY',		4).
valid_abstract_section_header('DISCUSSION & CONCLUSIONS',		4).
valid_abstract_section_header('DISCUSSION AND CLINICAL IMPLICATIONS',		4).
valid_abstract_section_header('DISCUSSION AND DISSEMINATION',		4).
valid_abstract_section_header('DISCUSSION/RELATION TO CLINICAL PRACTICE',		4).
valid_abstract_section_header('DISCUSSSION',		4).
valid_abstract_section_header('DISTRIBUTION',		4).
valid_abstract_section_header('DOSAGE',		4).
valid_abstract_section_header('DOSAGE AND ADMINISTRATION',		4).
valid_abstract_section_header('DRUG THERAPY',		4).
valid_abstract_section_header('Design and Setting',		4).
valid_abstract_section_header('Design, Settings, and Participants',		4).
valid_abstract_section_header('EARLY RESULTS',		4).
valid_abstract_section_header('EMPIRICAL DATA',		4).
valid_abstract_section_header('ENDPOINTS AND LINKAGES TO OTHER DATA',		4).
valid_abstract_section_header('ENROLLMENT',		4).
valid_abstract_section_header('ESULTS',		4).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL RELEVANCE AND AIM OF THE STUDY',		4).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL RELEVENCE',		4).
valid_abstract_section_header('ETHNOPHARMOCOLOGICAL RELEVANCE',		4).
valid_abstract_section_header('ETIOLOGIC FACTORS',		4).
valid_abstract_section_header('EVIDENCE ACQUISTION',		4).
valid_abstract_section_header('EXPECTED IMPACT OF THE STUDY FOR PUBLIC HEALTH',		4).
valid_abstract_section_header('EXPERIMENTAL APPROACH AND KEY RESULTS',		4).
valid_abstract_section_header('EXPERIMENTAL PLAN',		4).
valid_abstract_section_header('EXPERIMENTAL SETUP',		4).
valid_abstract_section_header('EXPERT CLINICAL OPINION',		4).
valid_abstract_section_header('FINANCIAL SUPPORT',		4).
valid_abstract_section_header('FINDINGS AND INTERPRETATION',		4).
valid_abstract_section_header('FINDINGS AND OUTCOME',		4).
valid_abstract_section_header('FOREWORD',		4).
valid_abstract_section_header('FUNCTIONAL RESULTS',		4).
valid_abstract_section_header('GENERAL DESIGN',		4).
valid_abstract_section_header('GENETIC STUDIES',		4).
valid_abstract_section_header('GEOGRAPHICAL DISTRIBUTION',		4).
valid_abstract_section_header('GOALS AND METHODS',		4).
valid_abstract_section_header('GPE-PAVM',		4).
valid_abstract_section_header('GROUP OF PATIENTS',		4).
valid_abstract_section_header('HIGHLIGHTS OF THE ARTICLE',		4).
valid_abstract_section_header('HISTOPATHOLOGIC FINDINGS',		4).
valid_abstract_section_header('HOSTS',		4).
valid_abstract_section_header('HOW THE RESEARCH WAS CONDUCTED',		4).
valid_abstract_section_header('HYPOTHESIS AND AIMS',		4).
valid_abstract_section_header('IMAGING FINDINGS',		4).
valid_abstract_section_header('IMPLEMENTATION AND RESULTS',		4).
valid_abstract_section_header('IMPLICATION FOR NURSING',		4).
valid_abstract_section_header('IMPLICATIONS FOR NURSE MANAGEMENT',		4).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE AND POLICY',		4).
valid_abstract_section_header('IMPLICATIONS OF HYPOTHESIS',		4).
valid_abstract_section_header('INCLUSION/EXCLUSION CRITERIA',		4).
valid_abstract_section_header('INDICATIONS FOR SURGERY',		4).
valid_abstract_section_header('INFECTIONS',		4).
valid_abstract_section_header('INTEM',		4).
valid_abstract_section_header('INTERACTIONS',		4).
valid_abstract_section_header('INTERPRETATIONS & CONCLUSION',		4).
valid_abstract_section_header('INTERVENTION AND COMPARATOR',		4).
valid_abstract_section_header('INTERVENTION/MEASUREMENTS',		4).
valid_abstract_section_header('INTRODUCTION & AIM',		4).
valid_abstract_section_header('INVENTIONS',		4).
valid_abstract_section_header('INVESTIGATED GROUP AND METHODS',		4).
valid_abstract_section_header('KEY CONCLUSIONS AND CLINICAL IMPLICATIONS',		4).
valid_abstract_section_header('KEY METHODS',		4).
valid_abstract_section_header('KEY POINT',		4).
valid_abstract_section_header('KEY PRACTITIONERS MESSAGES',		4).
valid_abstract_section_header('L-NMMA',		4).
valid_abstract_section_header('LABORATORY ANALYSIS',		4).
valid_abstract_section_header('LABORATORY DATA',		4).
valid_abstract_section_header('LEF-LG',		4).
valid_abstract_section_header('LEVEL OF EVIDENCE:',		4).
valid_abstract_section_header('LIMITATIONS/REASONS FOR CAUTION',		4).
valid_abstract_section_header('LIST OF ABBREVIATIONS',		4).
valid_abstract_section_header('LOCALIZATION',		4).
valid_abstract_section_header('MAIN COMPONENTS OF THE PROGRAM',		4).
valid_abstract_section_header('MAIN CONCLUSIONS/SIGNIFICANCE',		4).
valid_abstract_section_header('MAIN POINTS DISCUSSED',		4).
valid_abstract_section_header('MATERIALE E METODO',		4).
valid_abstract_section_header('MATERIALS AND METHODS, AND RESULTS',		4).
valid_abstract_section_header('MATERIALS-AND-METHODS',		4).
valid_abstract_section_header('MEASUREMENTS & OUTCOMES',		4).
valid_abstract_section_header('MEHTODS',		4).
valid_abstract_section_header('METERIALS AND METHODS',		4).
valid_abstract_section_header('METHDS',		4).
valid_abstract_section_header('METHOD & MATERIALS',		4).
valid_abstract_section_header('METHOD AND DATA',		4).
valid_abstract_section_header('METHOD AND POPULATION',		4).
valid_abstract_section_header('METHOD OF REVIEW',		4).
valid_abstract_section_header('METHOD OF THE STUDY',		4).
valid_abstract_section_header('METHOD:',		4).
valid_abstract_section_header('METHODOLOGY, PRINCIPAL FINDINGS',		4).
valid_abstract_section_header('METHODS & PROCEDURE',		4).
valid_abstract_section_header('METHODS AND PATIENT',		4).
valid_abstract_section_header('METHODS AND SETTINGS',		4).
valid_abstract_section_header('METHODS/RESULT',		4).
valid_abstract_section_header('METHOS',		4).
valid_abstract_section_header('MODE OF ACTION',		4).
valid_abstract_section_header('MODEL DEVELOPMENT',		4).
valid_abstract_section_header('MOLECULAR GENETICS',		4).
valid_abstract_section_header('MOST IMPORTANT FINDINGS',		4).
valid_abstract_section_header('Methods and Results',		4).
valid_abstract_section_header('NAME OF REGISTRY',		4).
valid_abstract_section_header('NEED',		4).
valid_abstract_section_header('NOVELTY OF THE WORK',		4).
valid_abstract_section_header('NTRODUCTION',		4).
valid_abstract_section_header('OBEJECTIVES',		4).
valid_abstract_section_header('OBESITY',		4).
valid_abstract_section_header('OBJECIVES',		4).
valid_abstract_section_header('OBJECTIF DE LA REVUE',		4).
valid_abstract_section_header('OBJECTIVE, PARTICIPANTS, AND METHODS',		4).
valid_abstract_section_header('OBJECTIVE, PATIENTS AND METHODS',		4).
valid_abstract_section_header('OBJECTIVES AND CONCLUSIONS',		4).
valid_abstract_section_header('OBJECTIVES/INTRODUCTION',		4).
valid_abstract_section_header('OBJECTVES',		4).
valid_abstract_section_header('OBSERVATION & RESULTS',		4).
valid_abstract_section_header('ONCLUSION',		4).
valid_abstract_section_header('OPEN QUESTIONS',		4).
valid_abstract_section_header('OPERATION TECHNIQUE',		4).
valid_abstract_section_header('ORGANISATION/RESPONSIBILITY',		4).
valid_abstract_section_header('OSCAR',		4).
valid_abstract_section_header('OUR RESULTS',		4).
valid_abstract_section_header('OUTCOME MEASURES AND ANALYSIS',		4).
valid_abstract_section_header('OVERALL CONCLUSIONS',		4).
valid_abstract_section_header('OWN EXPERIENCES',		4).
valid_abstract_section_header('PARTICIPANTS AND/OR CONTEXTS',		4).
valid_abstract_section_header('PARTICIPANTS, SETTING',		4).
valid_abstract_section_header('PARTICIPANTS/ MATERIALS, SETTING, METHODS',		4).
valid_abstract_section_header('PARTICIPANTS/CASES',		4).
valid_abstract_section_header('PARTICIPANTS:',		4).
valid_abstract_section_header('PATHOGENICITY',		4).
valid_abstract_section_header('PATIENT GROUP AND METHODS',		4).
valid_abstract_section_header('PATIENT-SUBJECT SELECTION',		4).
valid_abstract_section_header('PATIENTS AND METHODS AND RESULTS',		4).
valid_abstract_section_header('PATIENTS, METHOD AND RESULTS',		4).
valid_abstract_section_header('PATIENTS/RESULTS',		4).
valid_abstract_section_header('PHYSICIAN QUALIFICATION',		4).
valid_abstract_section_header('PHYSIOPATHOLOGY',		4).
valid_abstract_section_header('PLACE AND DURATION OF THE STUDY',		4).
valid_abstract_section_header('POPULATION AND METHODOLOGY',		4).
valid_abstract_section_header('POST-TREATMENT RECORDS',		4).
valid_abstract_section_header('POSTOPERATIVE TREATMENT',		4).
valid_abstract_section_header('PREREQUISITES',		4).
valid_abstract_section_header('PRESENT',		4).
valid_abstract_section_header('PRESENTATION AND INTERVENTION',		4).
valid_abstract_section_header('PREVIOUS PRESENTATION',		4).
valid_abstract_section_header('PRIMARY AIM',		4).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME',		4).
valid_abstract_section_header('PRIMARY PREVENTION',		4).
valid_abstract_section_header('PRIMARY/SECONDARY OUTCOME MEASURES',		4).
valid_abstract_section_header('PRINCIPAL MEASURES',		4).
valid_abstract_section_header('PROBLEM AND METHOD OF STUDY',		4).
valid_abstract_section_header('PROFILE',		4).
valid_abstract_section_header('PROGRAM COMPONENTS',		4).
valid_abstract_section_header('PROGRAM DEVELOPMENT',		4).
valid_abstract_section_header('PROGRESS TO DATE',		4).
valid_abstract_section_header('PROPOSED APPROACH',		4).
valid_abstract_section_header('PROSPECTIVES',		4).
valid_abstract_section_header('PROVEN EFFICACY',		4).
valid_abstract_section_header('PUBLICATION INDICES',		4).
valid_abstract_section_header('PUPPOSE',		4).
valid_abstract_section_header('PURPOSE',		4).
valid_abstract_section_header('PURPOSE AND AIMS',		4).
valid_abstract_section_header('PURPOSE/HYPOTHESES',		4).
valid_abstract_section_header('QUALITY',		4).
valid_abstract_section_header('QUALITY OF LIFE',		4).
valid_abstract_section_header('RATIONALE AND HYPOTHESIS',		4).
valid_abstract_section_header('RATIONALE FOR THIS STUDY',		4).
valid_abstract_section_header('RATIONALES AND OBJECTIVES',		4).
valid_abstract_section_header('RECENT ADVANCES AND CRITICAL ISSUES',		4).
valid_abstract_section_header('RECOMMENDATION 4',		4).
valid_abstract_section_header('RECRUITMENT',		4).
valid_abstract_section_header('REGISTRATIONS',		4).
valid_abstract_section_header('RELEVANCE FOR PRACTICE',		4).
valid_abstract_section_header('REPORT OF THE CASE',		4).
valid_abstract_section_header('RESEARCH AGENDA',		4).
valid_abstract_section_header('RESEARCH AIMS',		4).
valid_abstract_section_header('RESEARCH DESIGN AND MEASURES',		4).
valid_abstract_section_header('RESEARCH DESIGN AND SETTING',		4).
valid_abstract_section_header('RESEARCH PURPOSE',		4).
valid_abstract_section_header('RESULT & DISCUSSION',		4).
valid_abstract_section_header('RESULT/CONCLUSIONS',		4).
valid_abstract_section_header('RESULT/DISCUSSION',		4).
valid_abstract_section_header('RESULTS AND INNOVATION',		4).
valid_abstract_section_header('RESULTS AND PERSPECTIVES',		4).
valid_abstract_section_header('RESULTS AND SUMMARY',		4).
valid_abstract_section_header('RESULTS RESULTS',		4).
valid_abstract_section_header('RESULTS, CONCLUSIONS',		4).
valid_abstract_section_header('RESULTS, DISCUSSION AND CONCLUSION',		4).
valid_abstract_section_header('REVIEW DATE',		4).
valid_abstract_section_header('REVIEW METHOD USED',		4).
valid_abstract_section_header('RISULTATO',		4).
valid_abstract_section_header('RSULTS',		4).
valid_abstract_section_header('RUN-APAP',		4).
valid_abstract_section_header('SCOPE AND METHODOLOGY',		4).
valid_abstract_section_header('SCOPO DEL LAVORO',		4).
valid_abstract_section_header('SCOPO DELLO STUDIO',		4).
valid_abstract_section_header('SCREENING',		4).
valid_abstract_section_header('SCREENING TOOLS',		4).
valid_abstract_section_header('SECTIONS',		4).
valid_abstract_section_header('SETTING UK PARTICIPANTS',		4).
valid_abstract_section_header('SETTING, PARTICIPANTS AND OUTCOME MEASURES',		4).
valid_abstract_section_header('SIGNIFICANCE AND CONCLUSION',		4).
valid_abstract_section_header('SIGNIFICANCE AND CONCLUSIONS',		4).
valid_abstract_section_header('SIGNIFICANT',		4).
valid_abstract_section_header('SOURCES OF FUNDING',		4).
valid_abstract_section_header('STATEMENT OF CONCLUSIONS AND RECOMMENDATIONS FOR CLINICAL PRACTICE',		4).
valid_abstract_section_header('STATUS',		4).
valid_abstract_section_header('STRENGTH OF RECOMMENDATION TAXONOMY SORT',		4).
valid_abstract_section_header('STRUCTURE OF STUDY',		4).
valid_abstract_section_header('STRUCTURED ABSTRACT',		4).
valid_abstract_section_header('STUDY DESIGN AND DATA COLLECTION METHODS',		4).
valid_abstract_section_header('STUDY DESIGN AND OUTCOME MEASURES',		4).
valid_abstract_section_header('STUDY DESIGN, SETTING, AND PARTICIPANTS',		4).
valid_abstract_section_header('STUDY FUNDING, COMPETING INTERESTS',		4).
valid_abstract_section_header('STUDY FUNDING/ COMPETING INTERESTS',		4).
valid_abstract_section_header('STUDY FUNDING/POTENTIAL COMPETING INTERESTS',		4).
valid_abstract_section_header('STUDY OBJECTIVES AND DESIGN',		4).
valid_abstract_section_header('STUDY SELECTION AND METHODS',		4).
valid_abstract_section_header('SUBJECT SELECTION',		4).
valid_abstract_section_header('SUBJECTS OF STUDY',		4).
valid_abstract_section_header('SUMMARY BACKGROUND/OBJECTIVES',		4).
valid_abstract_section_header('SUMMARY STATEMENTS',		4).
valid_abstract_section_header('SUPPLEMENTARY MATERIAL',		4).
valid_abstract_section_header('SURGICAL RELEVANCE',		4).
valid_abstract_section_header('SURGICAL TECHNIQUES',		4).
valid_abstract_section_header('SYNTHESIS OF DATA',		4).
valid_abstract_section_header('SYSTEMATIC REVIEW REGISTRATION PROSPERO',		4).
valid_abstract_section_header('TA/TT',		4).
valid_abstract_section_header('TECHNIC',		4).
valid_abstract_section_header('TERMINOLOGY',		4).
valid_abstract_section_header('TEST PERSONS AND METHODS',		4).
valid_abstract_section_header('TESTING OF HYPOTHESIS',		4).
valid_abstract_section_header('TG/GG',		4).
valid_abstract_section_header('THE MATERIAL AND METHODS',		4).
valid_abstract_section_header('THE PRIMARY OBJECTIVE',		4).
valid_abstract_section_header('THE PROGRAM',		4).
valid_abstract_section_header('THE PROJECT',		4).
valid_abstract_section_header('THE RESEARCH',		4).
valid_abstract_section_header('THEORY AND DISCUSSION',		4).
valid_abstract_section_header('THERAPEUTIC APPROACH',		4).
valid_abstract_section_header('THERAPEUTIC METHODS',		4).
valid_abstract_section_header('THERAPEUTIC MODALITIES',		4).
valid_abstract_section_header('THERAPY AND PROGNOSIS',		4).
valid_abstract_section_header('TOPICS',		4).
valid_abstract_section_header('TRANSPLANTATION',		4).
valid_abstract_section_header('TREATMENT AND PROGNOSIS',		4).
valid_abstract_section_header('TRIAL REGISTRATION ID',		4).
valid_abstract_section_header('TRIAL REGISTRATION IDENTIFIER',		4).
valid_abstract_section_header('TRIAL REGISTRATION UMIN-CTR',		4).
valid_abstract_section_header('UKCRN ID',		4).
valid_abstract_section_header('UNIQUE IDENTIFIER',		4).
valid_abstract_section_header('UPDATE',		4).
valid_abstract_section_header('USEFUL WEBSITE',		4).
valid_abstract_section_header('VACCINATION',		4).
valid_abstract_section_header('VIEWPOINTS AND CONCLUSION',		4).
valid_abstract_section_header('VIEWPOINTS AND CONCLUSIONS',		4).
valid_abstract_section_header('WIDER IMPLICATIONS OF FINDINGS',		4).
valid_abstract_section_header('2 METHODS',		3).
valid_abstract_section_header('3 RESULTS',		3).
valid_abstract_section_header('A CASE REVIEW',		3).
valid_abstract_section_header('AA/AG',		3).
valid_abstract_section_header('ABSTRACTS',		3).
valid_abstract_section_header('ACCESSIBLE ABSTRACT',		3).
valid_abstract_section_header('ACHIEVEMENTS AND PRACTICAL RECOMMENDATIONS',		3).
valid_abstract_section_header('ACQUISITION OF THE EVIDENCE',		3).
valid_abstract_section_header('ACTRN',		3).
valid_abstract_section_header('ACUTE STROKE',		3).
valid_abstract_section_header('AIM AND DEVELOPMENT',		3).
valid_abstract_section_header('AIM OF THE VIDEO',		3).
valid_abstract_section_header('AIMS & METHOD',		3).
valid_abstract_section_header('AIMS AND CONCLUSION',		3).
valid_abstract_section_header('AIMS/OBJECTIVES/BACKGROUND',		3).
valid_abstract_section_header('ANALYSE',		3).
valid_abstract_section_header('ANALYTIC APPROACH',		3).
valid_abstract_section_header('ANIMAL STUDY',		3).
valid_abstract_section_header('ANIMALS & METHODS',		3).
valid_abstract_section_header('ANTIBIOTIC THERAPY',		3).
valid_abstract_section_header('ANTICIPATED RESULTS',		3).
valid_abstract_section_header('ANTICOAGULATION',		3).
valid_abstract_section_header('AREA OF CONTROVERSY',		3).
valid_abstract_section_header('AREAS FOR TIMELY RESEARCH',		3).
valid_abstract_section_header('ARTICLE TYPE',		3).
valid_abstract_section_header('AUTHOR',		3).
valid_abstract_section_header('AUTHOR\'S CONCLUSION',		3).
valid_abstract_section_header('AUTOPSY',		3).
valid_abstract_section_header('Antecedentes',		3).
valid_abstract_section_header('BACHGROUND',		3).
valid_abstract_section_header('BACKGOUND/AIM',		3).
valid_abstract_section_header('BACKGRAOUND',		3).
valid_abstract_section_header('BACKGROUND & METHOD',		3).
valid_abstract_section_header('BACKGROUND & STUDY AIMS',		3).
valid_abstract_section_header('BACKGROUND / OBJECTIVE',		3).
valid_abstract_section_header('BACKGROUND AIM',		3).
valid_abstract_section_header('BACKGROUND AND DISCUSSION',		3).
valid_abstract_section_header('BACKGROUND AND RATIONAL',		3).
valid_abstract_section_header('BACKGROUND AND STUDY DESIGN',		3).
valid_abstract_section_header('BACKGROUND OR AIMS',		3).
valid_abstract_section_header('BACKGROUND/OBJECT',		3).
valid_abstract_section_header('BACKGROUNDS & OBJECTIVE',		3).
valid_abstract_section_header('BACKGROUNDS/OBJECTIVE',		3).
valid_abstract_section_header('BLACKGROUND',		3).
valid_abstract_section_header('BLOOD PRESSURE VARIABILITY',		3).
valid_abstract_section_header('BRACE DESCRIPTION',		3).
valid_abstract_section_header('BRIEF SUMMARY',		3).
valid_abstract_section_header('BRVO',		3).
valid_abstract_section_header('BUT DE LA REVUE',		3).
valid_abstract_section_header('C POST-RETENTION RECORDS',		3).
valid_abstract_section_header('CANCER',		3).
valid_abstract_section_header('CANDIDATE PREDICTORS',		3).
valid_abstract_section_header('CAS',		3).
valid_abstract_section_header('CASE ANALYSIS',		3).
valid_abstract_section_header('CASE AND TECHNIQUE',		3).
valid_abstract_section_header('CASE DESCRIPTION AND OUTCOMES',		3).
valid_abstract_section_header('CASE DESCRIPTION AND RESULTS',		3).
valid_abstract_section_header('CASE DETAIL',		3).
valid_abstract_section_header('CASE DIAGNOSIS AND TREATMENT',		3).
valid_abstract_section_header('CASE PRESENTATION AND COURSE',		3).
valid_abstract_section_header('CASE REPORT',		3).
valid_abstract_section_header('CASE REPORTS AND DISCUSSION',		3).
valid_abstract_section_header('CASE VIGNETTE',		3).
valid_abstract_section_header('CASE(S)',		3).
valid_abstract_section_header('CASES OUTLINE',		3).
valid_abstract_section_header('CASUISTICS',		3).
valid_abstract_section_header('CCONCLUSIONS',		3).
valid_abstract_section_header('CHALLENGE',		3).
valid_abstract_section_header('CHALLENGES AND SUCCESSES',		3).
valid_abstract_section_header('CHEMICAL COMPOUNDS STUDIED IN THIS ARTICLE',		3).
valid_abstract_section_header('CLINCIAL QUESTION/LEVEL OF EVIDENCE',		3).
valid_abstract_section_header('CLINIC REPORT',		3).
valid_abstract_section_header('CLINICAL',		3).
valid_abstract_section_header('CLINICAL AND LABORATORY FINDINGS',		3).
valid_abstract_section_header('CLINICAL APPEARANCE',		3).
valid_abstract_section_header('CLINICAL CONSEQUENCES',		3).
valid_abstract_section_header('CLINICAL FINDINGS AND TREATMENT',		3).
valid_abstract_section_header('CLINICAL HISTORY PATIENT',		3).
valid_abstract_section_header('CLINICAL NEED AND TARGET POPULATION',		3).
valid_abstract_section_header('CLINICAL QUESTIONS',		3).
valid_abstract_section_header('CLINICAL RECORD',		3).
valid_abstract_section_header('CLINICAL REPORTS',		3).
valid_abstract_section_header('CLINICAL SUBJECT AND METHODS',		3).
valid_abstract_section_header('CLINICAL TRIAL ID',		3).
valid_abstract_section_header('CLINICAL TRIAL REG NO',		3).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION NUMBERS',		3).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATIONS',		3).
valid_abstract_section_header('CLINICAL TRIALS INFORMATION',		3).
valid_abstract_section_header('CLINICAL/METHODOLOGICAL ISSUE',		3).
valid_abstract_section_header('CLINICALTRIALS',		3).
valid_abstract_section_header('COCLUSION',		3).
valid_abstract_section_header('COCNLUSIONS',		3).
valid_abstract_section_header('COMMUNICATION',		3).
valid_abstract_section_header('COMPARATOR',		3).
valid_abstract_section_header('CONCLUSINS',		3).
valid_abstract_section_header('CONCLUSION (S)',		3).
valid_abstract_section_header('CONCLUSION / SIGNIFICANCE',		3).
valid_abstract_section_header('CONCLUSION AND CLINICAL IMPLICATION',		3).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR CANCER SURVIVORS',		3).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR HEALTH POLICY',		3).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR NURSING',		3).
valid_abstract_section_header('CONCLUSION AND INFERENCES',		3).
valid_abstract_section_header('CONCLUSION/IMPLICATION',		3).
valid_abstract_section_header('CONCLUSION/SIGNIFICANCES',		3).
valid_abstract_section_header('CONCLUSIONS AND FUTURE DIRECTIONS',		3).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATION FOR PRACTICE',		3).
valid_abstract_section_header('CONCLUSIONS AND IMPORTANCE',		3).
valid_abstract_section_header('CONCLUSIONS AND MESSAGE',		3).
valid_abstract_section_header('CONCLUSIONS AND POLICY IMPLICATIONS',		3).
valid_abstract_section_header('CONCLUSIONS AND RECOMMENDATIONS FOR CLINICAL PRACTICE',		3).
valid_abstract_section_header('CONCLUSIONS AND SIGNIFICANCE OF THE STUDY',		3).
valid_abstract_section_header('CONCLUSIONS AND SUGGESTIONS',		3).
valid_abstract_section_header('CONCLUSIONS/LIMITATIONS',		3).
valid_abstract_section_header('CONCLUSIONS/RESULTS',		3).
valid_abstract_section_header('CONCLUSIONS/SIGNIFICANT FINDINGS',		3).
valid_abstract_section_header('CONCLUSTION',		3).
valid_abstract_section_header('CONLCUSION',		3).
valid_abstract_section_header('CONTENT ORGANIZATION',		3).
valid_abstract_section_header('CONTEXTS',		3).
valid_abstract_section_header('CONTRIBUTION TO THE FIELD',		3).
valid_abstract_section_header('CONTRIBUTIONS',		3).
valid_abstract_section_header('CONTROL MEASURES',		3).
valid_abstract_section_header('CONTROVERSY',		3).
valid_abstract_section_header('CORONARY ANGIOGRAPHY',		3).
valid_abstract_section_header('CURRENT KNOWLEDGE AND KEY POINT',		3).
valid_abstract_section_header('CURRENT RECOMMENDATIONS',		3).
valid_abstract_section_header('DATA ANALYZED',		3).
valid_abstract_section_header('DATA AND ANALYSIS',		3).
valid_abstract_section_header('DATA BASE',		3).
valid_abstract_section_header('DATA COLLECTION AND ANALYSES',		3).
valid_abstract_section_header('DATA EXTRACTION AND QUALITY ASSESSMENT',		3).
valid_abstract_section_header('DATA SELECTION AND DATA EXTRACTION',		3).
valid_abstract_section_header('DATA SOURCE AND SYNTHESIS',		3).
valid_abstract_section_header('DATA SOURCES AND SETTINGS',		3).
valid_abstract_section_header('DCARS',		3).
valid_abstract_section_header('DECLARATIONS OF INTEREST',		3).
valid_abstract_section_header('DESCRIPTION OF INTERVENTION',		3).
valid_abstract_section_header('DESCRIPTION OF STRATEGY',		3).
valid_abstract_section_header('DESCRIPTION OF THE EVIDENCE COLLECTION METHOD',		3).
valid_abstract_section_header('DESCRIPTION OF THE PROCESS',		3).
valid_abstract_section_header('DESCRIPTION OF TOPIC WITH RELATED EVIDENCE',		3).
valid_abstract_section_header('DESIGN AND DATA SOURCE',		3).
valid_abstract_section_header('DESIGN METHOD',		3).
valid_abstract_section_header('DESIGN SETTING PARTICIPANTS & MEASUREMENTS',		3).
valid_abstract_section_header('DESIGN SETTINGS AND PARTICIPANTS',		3).
valid_abstract_section_header('DESIGN, MATERIALS & METHODS',		3).
valid_abstract_section_header('DESIGN, METHODS, AND RESULTS',		3).
valid_abstract_section_header('DESIGN, PARTICIPANTS AND METHODS',		3).
valid_abstract_section_header('DESIGN, PATIENTS AND RESULTS',		3).
valid_abstract_section_header('DESIGN, PATIENTS, AND MAIN OUTCOME MEASURE',		3).
valid_abstract_section_header('DESIGN, SUBJECTS',		3).
valid_abstract_section_header('DESIGN/MATERIALS AND METHODS',		3).
valid_abstract_section_header('DESIGN/SETTINGS',		3).
valid_abstract_section_header('DESIGN/SETTINGS/PATIENTS',		3).
valid_abstract_section_header('DETAILS OF THE CASE',		3).
valid_abstract_section_header('DIABETES',		3).
valid_abstract_section_header('DIAGNOSIS, TREATMENT, AND COURSE',		3).
valid_abstract_section_header('DIAGNOSTIC CRITERIA',		3).
valid_abstract_section_header('DIAGNOSTIC PROBLEMS',		3).
valid_abstract_section_header('DIAGNOSTIC WORKUP',		3).
valid_abstract_section_header('DIAGNOSTICS AND THERAPY',		3).
valid_abstract_section_header('DISCUSSION CONCLUSION',		3).
valid_abstract_section_header('DISCUSSION ET CONCLUSION',		3).
valid_abstract_section_header('DISCUSSIONE E CONCLUSIONI',		3).
valid_abstract_section_header('DISCUSSIONS, CONCLUSIONS',		3).
valid_abstract_section_header('DORMANT-AF STUDY',		3).
valid_abstract_section_header('DP-PES',		3).
valid_abstract_section_header('DRUGS',		3).
valid_abstract_section_header('DUTCH TRIAL REGISTER',		3).
valid_abstract_section_header('Data analysis',		3).
valid_abstract_section_header('Description',		3).
valid_abstract_section_header('Design, Setting, and Patients',		3).
valid_abstract_section_header('Discussion',		3).
valid_abstract_section_header('ECONOMIC IMPORTANCE',		3).
valid_abstract_section_header('EDITORS\' INTRODUCTION',		3).
valid_abstract_section_header('EFFECTS',		3).
valid_abstract_section_header('EINLEITUNG',		3).
valid_abstract_section_header('ELIGIBILITY CRITERIA FOR SELECTED STUDIES',		3).
valid_abstract_section_header('ELIGIBILITY FOR SELECTING STUDIES',		3).
valid_abstract_section_header('EPIDEMIOLOGICAL STUDIES',		3).
valid_abstract_section_header('ERGEBNISSE UND SCHLUSSFOLGERUNG',		3).
valid_abstract_section_header('ERGEBNISSE UND SCHLUSSFOLGERUNGEN',		3).
valid_abstract_section_header('ERYTHROPOIETIN THERAPY',		3).
valid_abstract_section_header('ETHICS APPROVAL',		3).
valid_abstract_section_header('ETHNOPHAMACOLOGICAL RELEVANCE',		3).
valid_abstract_section_header('ETHNOPHARMACROLOGICAL RELEVANCE',		3).
valid_abstract_section_header('EUDRACT',		3).
valid_abstract_section_header('EVALUATION MECHANISMS',		3).
valid_abstract_section_header('EVENT',		3).
valid_abstract_section_header('EVIDENCE SYNTHESIS AND CONCLUSIONS',		3).
valid_abstract_section_header('EXPECTED IMPACT',		3).
valid_abstract_section_header('EXPERIENCE REPORT',		3).
valid_abstract_section_header('EXPERIENCE TO DATE',		3).
valid_abstract_section_header('EXPERIMENTAL DESIGN/RESULTS',		3).
valid_abstract_section_header('EXPERIMENTAL GROUPS',		3).
valid_abstract_section_header('EXPERIMENTAL METHOD',		3).
valid_abstract_section_header('EXPERIMENTAL STUDY',		3).
valid_abstract_section_header('EXPLORATION',		3).
valid_abstract_section_header('EXTRACTION',		3).
valid_abstract_section_header('FACILITIES AVAILABLE',		3).
valid_abstract_section_header('FAZIT',		3).
valid_abstract_section_header('FINAL REFLECTIONS',		3).
valid_abstract_section_header('FUNDING/SPONSORSHIP',		3).
valid_abstract_section_header('FUNDINGS',		3).
valid_abstract_section_header('FURTHER DEVELOPMENTS',		3).
valid_abstract_section_header('FURTHER INFORMATION',		3).
valid_abstract_section_header('FUTURE CHALLENGES',		3).
valid_abstract_section_header('FUTURE DIRECTION',		3).
valid_abstract_section_header('Fundamentos:',		3).
valid_abstract_section_header('GENETIC FACTORS',		3).
valid_abstract_section_header('GG/AG',		3).
valid_abstract_section_header('GLOBAL HEALTH IMPLICATIONS',		3).
valid_abstract_section_header('GOV REGISTRATION',		3).
valid_abstract_section_header('GOV REGISTRATION NUMBER',		3).
valid_abstract_section_header('GROWTH',		3).
valid_abstract_section_header('HEART FAILURE',		3).
valid_abstract_section_header('HF/HF',		3).
valid_abstract_section_header('HIGHLIGHT',		3).
valid_abstract_section_header('HISTORICAL ASPECTS',		3).
valid_abstract_section_header('HISTORICAL OVERVIEW',		3).
valid_abstract_section_header('HISTORY AND CLINICAL DATA',		3).
valid_abstract_section_header('HISTORY AND CLINICAL FINDING',		3).
valid_abstract_section_header('HISTORY AND EXAMINATION',		3).
valid_abstract_section_header('HISTORY AND PHYSICAL FINDINGS',		3).
valid_abstract_section_header('HOSPITAL COURSE',		3).
valid_abstract_section_header('HUMAN FACTORS ENGINEERING (HFE) ANALYSIS',		3).
valid_abstract_section_header('IMPACT OF INDUSTRY',		3).
valid_abstract_section_header('IMPACT TO INDUSTRY',		3).
valid_abstract_section_header('IMPLEMENTATION AND PERFORMANCE',		3).
valid_abstract_section_header('IMPLICATIONS AND CONCLUSION',		3).
valid_abstract_section_header('IMPLICATIONS AND CONTRIBUTION',		3).
valid_abstract_section_header('IMPLICATIONS FOR NURSING LEADERSHIP',		3).
valid_abstract_section_header('IMPLICATIONS FOR NURSING MANAGERS',		3).
valid_abstract_section_header('IMPLICATIONS FOR NURSING OR HEALTH POLICY',		3).
valid_abstract_section_header('IMPLICATIONS FOR POLICY & PRACTICE',		3).
valid_abstract_section_header('IMPLICATIONS FOR POLICY, DELIVERY, OR PRACTICE',		3).
valid_abstract_section_header('IMPLICATIONS OF KEY FINDINGS',		3).
valid_abstract_section_header('IN GENERAL',		3).
valid_abstract_section_header('IN VIVO STUDY',		3).
valid_abstract_section_header('INFECTION PROCESS',		3).
valid_abstract_section_header('INFERENCES',		3).
valid_abstract_section_header('INFORMANTS',		3).
valid_abstract_section_header('INFORMATION AND DISCUSSION',		3).
valid_abstract_section_header('INTEPRETATION',		3).
valid_abstract_section_header('INTEREST',		3).
valid_abstract_section_header('INTERPRETATION OF RESULTS',		3).
valid_abstract_section_header('INTERVENTION ESD MAIN OUTCOME MEASUREMENTS',		3).
valid_abstract_section_header('INTERVENTION/EXPOSURE',		3).
valid_abstract_section_header('INTERVENTION/RESULTS',		3).
valid_abstract_section_header('INTERVENTIONS/MEASUREMENTS',		3).
valid_abstract_section_header('INTORDUCTION',		3).
valid_abstract_section_header('INTRODUCTION & OBJECTIVE',		3).
valid_abstract_section_header('INTRODUCTION AND DESIGN',		3).
valid_abstract_section_header('INVESTIGATED SUBJECTS',		3).
valid_abstract_section_header('Introduccion',		3).
valid_abstract_section_header('KEY MESSAGES AND IMPLICATIONS',		3).
valid_abstract_section_header('KEY QUESTIONS',		3).
valid_abstract_section_header('KEYNOTE ADDRESS',		3).
valid_abstract_section_header('LABORATORY DIAGNOSIS',		3).
valid_abstract_section_header('LEARNING POINT',		3).
valid_abstract_section_header('LESSON LEARNED',		3).
valid_abstract_section_header('LESSON LEARNT',		3).
valid_abstract_section_header('LEVEL OF EVIDENCE AND STUDY TYPE',		3).
valid_abstract_section_header('LEVEL OF INCIDENCE',		3).
valid_abstract_section_header('LEVELS',		3).
valid_abstract_section_header('LITERATURE REVIEW AND DISCUSSION',		3).
valid_abstract_section_header('LITERATURE SELECTION',		3).
valid_abstract_section_header('LITERATURE SOURCES',		3).
valid_abstract_section_header('LOOKING AHEAD',		3).
valid_abstract_section_header('M-CHO',		3).
valid_abstract_section_header('MAIN CONTENTS',		3).
valid_abstract_section_header('MAIN FINDINGS AND CONCLUSION',		3).
valid_abstract_section_header('MAIN FINDINGS/DISCUSSION',		3).
valid_abstract_section_header('MAIN OUTCOMEMEASURES',		3).
valid_abstract_section_header('MAIN RESULTS AND THE ROLE OF CHANGE',		3).
valid_abstract_section_header('MAIN UPDATING',		3).
valid_abstract_section_header('MAIN VARIABLE',		3).
valid_abstract_section_header('MAIN VARIABLES AND DESCRIPTIVE DATA',		3).
valid_abstract_section_header('MAJOR OUTCOME MEASURE',		3).
valid_abstract_section_header('MAJOR OUTCOMES',		3).
valid_abstract_section_header('MAJOR THEMES',		3).
valid_abstract_section_header('MAJOR TOPICS',		3).
valid_abstract_section_header('MANAGEMENT OF PROGRESSIVE OR REFRACTORY DISEASE',		3).
valid_abstract_section_header('MATERIAL AND METHODS/RESULTS',		3).
valid_abstract_section_header('MATERIAL AND THE METHODS',		3).
valid_abstract_section_header('MATERIAL, METHODOLOGY',		3).
valid_abstract_section_header('MATERIAL, METHODS AND TREATMENT',		3).
valid_abstract_section_header('MATERIALE DI STUDIO',		3).
valid_abstract_section_header('MATERIALE E METODI',		3).
valid_abstract_section_header('MATERIALS & METHODOLOGY',		3).
valid_abstract_section_header('MATERIALS AND TMETHODS',		3).
valid_abstract_section_header('MEHTOHDS',		3).
valid_abstract_section_header('META-ANALYSIS',		3).
valid_abstract_section_header('METABOLISM',		3).
valid_abstract_section_header('METHOD & MATERIAL',		3).
valid_abstract_section_header('METHOD AND OBJECTIVE',		3).
valid_abstract_section_header('METHOD AND SCOPE',		3).
valid_abstract_section_header('METHODOLOGICAL ASPECTS',		3).
valid_abstract_section_header('METHODOLOGY & METHODS',		3).
valid_abstract_section_header('METHODOLOGY AND PARTICIPANTS',		3).
valid_abstract_section_header('METHODOLOGY AND SIGNIFICANT FINDINGS',		3).
valid_abstract_section_header('METHODOLOGY/ PRINCIPLE FINDINGS',		3).
valid_abstract_section_header('METHODOLOGY/FINDING',		3).
valid_abstract_section_header('METHODOLOGY/SIGNIFICANT FINDINGS',		3).
valid_abstract_section_header('METHODS & MATERIAL',		3).
valid_abstract_section_header('METHODS & PRINCIPAL FINDINGS',		3).
valid_abstract_section_header('METHODS / RESULTS',		3).
valid_abstract_section_header('METHODS AND INFORMATION SOURCES',		3).
valid_abstract_section_header('METHODS AND INTERVENTION',		3).
valid_abstract_section_header('METHODS AND MAIN OBSERVATIONS',		3).
valid_abstract_section_header('METHODS AND MAJOR FINDINGS',		3).
valid_abstract_section_header('METHODS AND PRINCIPAL RESULTS',		3).
valid_abstract_section_header('METHODS AND SCOPE',		3).
valid_abstract_section_header('METHODS AND SUBJECTIVE',		3).
valid_abstract_section_header('METHODS OR DESIGN',		3).
valid_abstract_section_header('METHODS OR INTERVENTIONS',		3).
valid_abstract_section_header('METHODS, RESULTS, AND CONCLUSIONS',		3).
valid_abstract_section_header('METHODS/MAIN FINDINGS',		3).
valid_abstract_section_header('METHODS/MAJOR FINDINGS',		3).
valid_abstract_section_header('METHODS/TECHNIQUE',		3).
valid_abstract_section_header('METODOLOGIA',		3).
valid_abstract_section_header('MODEL, PERSPECTIVE, & TIMELINE',		3).
valid_abstract_section_header('MOLECULAR BIOLOGY',		3).
valid_abstract_section_header('MORBIDITY',		3).
valid_abstract_section_header('MYOCARDIAL VIABILITY',		3).
valid_abstract_section_header('Main Outcome Measures',		3).
valid_abstract_section_header('Material and methods',		3).
valid_abstract_section_header('Methodology',		3).
valid_abstract_section_header('NEED AND PURPOSE',		3).
valid_abstract_section_header('NEW DATA',		3).
valid_abstract_section_header('NEW DEVELOPMENTS',		3).
valid_abstract_section_header('NEW FEATURES',		3).
valid_abstract_section_header('NEXT STEPS/FUTURE DIRECTIONS',		3).
valid_abstract_section_header('NOMENCLATURE',		3).
valid_abstract_section_header('NRSTS',		3).
valid_abstract_section_header('OBIECTIVE',		3).
valid_abstract_section_header('OBJECT & METHOD',		3).
valid_abstract_section_header('OBJECT OF STUDY',		3).
valid_abstract_section_header('OBJECTIFS DE LA REVUE',		3).
valid_abstract_section_header('OBJECTIONS',		3).
valid_abstract_section_header('OBJECTIV',		3).
valid_abstract_section_header('OBJECTIVE (S)',		3).
valid_abstract_section_header('OBJECTIVE AND PERSPECTIVES',		3).
valid_abstract_section_header('OBJECTIVE OF THE REVIEW',		3).
valid_abstract_section_header('OBJECTIVE OF WORK',		3).
valid_abstract_section_header('OBJECTIVE/METHODOLOGY',		3).
valid_abstract_section_header('OBJECTIVES, STUDY DESIGN AND MAIN OUTCOME MEASURES',		3).
valid_abstract_section_header('OBJETIVO PRINCIPAL',		3).
valid_abstract_section_header('OBJETIVO PRINCIPAL DEL ESTUDIO',		3).
valid_abstract_section_header('OBSTACLES',		3).
valid_abstract_section_header('OPERATIONAL ISSUES',		3).
valid_abstract_section_header('OPIOIDS',		3).
valid_abstract_section_header('OTHER FACTORS',		3).
valid_abstract_section_header('OUTBREAK',		3).
valid_abstract_section_header('OUTCOME MEASURES/RESULTS',		3).
valid_abstract_section_header('OUTCOMES & MEASUREMENT',		3).
valid_abstract_section_header('OUTCOMES AND ANALYSIS',		3).
valid_abstract_section_header('Objetives.',		3).
valid_abstract_section_header('PACS NUMBERS',		3).
valid_abstract_section_header('PARTICIPANTS AND SAMPLES',		3).
valid_abstract_section_header('PARTICIPANTS, SETTINGS, METHODS',		3).
valid_abstract_section_header('PARTICIPANTS/MATERIALS SETTING, METHODS',		3).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTINGS AND METHODS',		3).
valid_abstract_section_header('PARTICIPANTS/MEASUREMENTS',		3).
valid_abstract_section_header('PAT -/-',		3).
valid_abstract_section_header('PATHOGENIC HYPOTHESES',		3).
valid_abstract_section_header('PATHOLOGY FINDINGS',		3).
valid_abstract_section_header('PATIENT AND SETTING',		3).
valid_abstract_section_header('PATIENT REPORTS',		3).
valid_abstract_section_header('PATIENT SAFETY AS A LEADERSHIP AND ORGANIZATIONAL PRIORITY',		3).
valid_abstract_section_header('PATIENT SAMPLE AND METHOD',		3).
valid_abstract_section_header('PATIENT(S)/ANIMAL(S)',		3).
valid_abstract_section_header('PATIENTS AND MAIN RESULTS',		3).
valid_abstract_section_header('PATIENTS AND METODS',		3).
valid_abstract_section_header('PATIENTS AND OUTCOME',		3).
valid_abstract_section_header('PATIENTS AND THERAPY',		3).
valid_abstract_section_header('PATIENTS CHARACTERISTICS',		3).
valid_abstract_section_header('PATIENTS CONCERNS',		3).
valid_abstract_section_header('PATIENTS, MATERIAL, AND METHODS',		3).
valid_abstract_section_header('PATIENTS, MATERIALS, METHODS',		3).
valid_abstract_section_header('PATIENTS/METHODS AND RESULTS',		3).
valid_abstract_section_header('PATIENTS/SUBJECTS AND METHODS',		3).
valid_abstract_section_header('PATINETS AND METHODS',		3).
valid_abstract_section_header('PBH-LCI',		3).
valid_abstract_section_header('PERSONNEL',		3).
valid_abstract_section_header('PHASE III',		3).
valid_abstract_section_header('PHYSICAL EXAMINATION FINDINGS',		3).
valid_abstract_section_header('PHYSICAL EXAMINATION RESULTS',		3).
valid_abstract_section_header('PITFALLS',		3).
valid_abstract_section_header('PLANNING',		3).
valid_abstract_section_header('PLF-LG',		3).
valid_abstract_section_header('POINT OF VIEW',		3).
valid_abstract_section_header('POSITIVE DIAGNOSIS',		3).
valid_abstract_section_header('POST-RETENTION RECORDS',		3).
valid_abstract_section_header('POSTOPERATIVE COMPLICATIONS',		3).
valid_abstract_section_header('PRACTICE CONCLUSION',		3).
valid_abstract_section_header('PRACTICE DESCRIPTION AND INNOVATION',		3).
valid_abstract_section_header('PRACTICE POINTS',		3).
valid_abstract_section_header('PRACTICE SETTING',		3).
valid_abstract_section_header('PRESENT SITUATION',		3).
valid_abstract_section_header('PRESENT STATUS',		3).
valid_abstract_section_header('PREVIOUS MEDICAL HISTORY',		3).
valid_abstract_section_header('PRIMARY ENDPOINTS',		3).
valid_abstract_section_header('PRINCIPLE LABORATORY FINDINGS',		3).
valid_abstract_section_header('PRINCIPLE OBSERVATIONS',		3).
valid_abstract_section_header('PROBLEM AND METHOD',		3).
valid_abstract_section_header('PROCEDURES AND FINDINGS',		3).
valid_abstract_section_header('PROCEEDINGS',		3).
valid_abstract_section_header('PROGRESS SINCE ICPD',		3).
valid_abstract_section_header('PROJECT OBJECTIVES',		3).
valid_abstract_section_header('PROJECTS AND PERSPECTIVES',		3).
valid_abstract_section_header('PROSPERO REGISTRY NUMBER',		3).
valid_abstract_section_header('PURPOSE & METHODS',		3).
valid_abstract_section_header('PURPOSE AND HYPOTHESIS',		3).
valid_abstract_section_header('Purposes',		3).
valid_abstract_section_header('QUESTIONNAIRES',		3).
valid_abstract_section_header('R-ZES',		3).
valid_abstract_section_header('RATIONALE AND DESIGN',		3).
valid_abstract_section_header('RATIONALE AND METHODS',		3).
valid_abstract_section_header('RATIONALE/BACKGROUND',		3).
valid_abstract_section_header('RECOMMENDATION 5',		3).
valid_abstract_section_header('RECOMMENDATION 6',		3).
valid_abstract_section_header('RECOMMENDATIONS AND CONCLUSION',		3).
valid_abstract_section_header('RECOMMENDATIONS FOR PREVENTION',		3).
valid_abstract_section_header('REFERRAL',		3).
valid_abstract_section_header('REGISTRATION INFORMATION',		3).
valid_abstract_section_header('REGISTRATION NUMBER FOR CLINICAL TRIALS',		3).
valid_abstract_section_header('RELEVANCE FOR PATIENTS',		3).
valid_abstract_section_header('REPORT CASE',		3).
valid_abstract_section_header('REPORT OF CASE',		3).
valid_abstract_section_header('RESEARCH HIGHLIGHTS',		3).
valid_abstract_section_header('RESEARCH METHODOLOGY/PRINCIPAL FINDINGS',		3).
valid_abstract_section_header('RESEARCH OUTCOMES',		3).
valid_abstract_section_header('RESEARCH PARTICIPANTS AND METHODS',		3).
valid_abstract_section_header('RESEARCH STRATEGY AND METHODS',		3).
valid_abstract_section_header('RESUITS',		3).
valid_abstract_section_header('RESULT AND INTERPRETATION',		3).
valid_abstract_section_header('RESULTS AND CLINICAL RELEVANCE',		3).
valid_abstract_section_header('RESULTS AND COMMENT',		3).
valid_abstract_section_header('RESULTS AND COMPARISON WITH EXISTING METHOD',		3).
valid_abstract_section_header('RESULTS AND DEBATES',		3).
valid_abstract_section_header('RESULTS AND LIMITATION',		3).
valid_abstract_section_header('RESULTS AND SIGNIFICANCE OF RESULTS',		3).
valid_abstract_section_header('RESULTS, DISCUSSION AND CONCLUSIONS',		3).
valid_abstract_section_header('RESULTS-CONCLUSION',		3).
valid_abstract_section_header('RESUME',		3).
valid_abstract_section_header('REVIEW OBJECTIVES',		3).
valid_abstract_section_header('REVIEW QUESTION/OBJECTIVES',		3).
valid_abstract_section_header('REVIEW QUESTIONS',		3).
valid_abstract_section_header('REVIEW QUESTIONS/OBJECTIVES',		3).
valid_abstract_section_header('RISK ADAPTED THERAPY',		3).
valid_abstract_section_header('RISK OF BIAS ASSESSMENT',		3).
valid_abstract_section_header('ROUNDTABLE DISCUSSION',		3).
valid_abstract_section_header('SCOPE AND SOURCES',		3).
valid_abstract_section_header('SCOPE OF THIS REVIEW',		3).
valid_abstract_section_header('SCOPE OF VIEW',		3).
valid_abstract_section_header('SEARCH METHODS AND SELECTION CRITERIA',		3).
valid_abstract_section_header('SEARCH STRATEGY FOR IDENTIFICATION OF STUDIES',		3).
valid_abstract_section_header('SECONDARY',		3).
valid_abstract_section_header('SETTING / PARTICIPANTS',		3).
valid_abstract_section_header('SETTING USA PARTICIPANTS',		3).
valid_abstract_section_header('SETTING:',		3).
valid_abstract_section_header('SETTINGS & POPULATION',		3).
valid_abstract_section_header('SETTTING',		3).
valid_abstract_section_header('SEVERITY',		3).
valid_abstract_section_header('SHORT COMMUNICATION',		3).
valid_abstract_section_header('SHR-S',		3).
valid_abstract_section_header('SIGNIFICANCE PARAGRAPH',		3).
valid_abstract_section_header('SIGNIFICANT STATEMENT',		3).
valid_abstract_section_header('SIGNS AND SYMPTOMS',		3).
valid_abstract_section_header('SPECIFIC OBJECTIVE',		3).
valid_abstract_section_header('STANDARD APPROACH',		3).
valid_abstract_section_header('STATE OF THE ART AND MAIN POINTS',		3).
valid_abstract_section_header('STATEMENT OF SIGNIFICANCE OF THE STUDY',		3).
valid_abstract_section_header('STATISTICAL ANALYSES USED',		3).
valid_abstract_section_header('STATISTICAL METHOD USED',		3).
valid_abstract_section_header('STATISTICAL TEST',		3).
valid_abstract_section_header('STRENGTHS AND LIMITATIONS',		3).
valid_abstract_section_header('STUDIES UNDERTAKEN',		3).
valid_abstract_section_header('STUDY 2',		3).
valid_abstract_section_header('STUDY A',		3).
valid_abstract_section_header('STUDY APPRAISAL METHODS',		3).
valid_abstract_section_header('STUDY CONTEXT',		3).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA AND PARTICIPANTS',		3).
valid_abstract_section_header('STUDY FUNDING AND COMPETING INTEREST',		3).
valid_abstract_section_header('STUDY FUNDING/COMPLETING OF INTERESTS',		3).
valid_abstract_section_header('STUDY SELECTION AND ANALYSIS',		3).
valid_abstract_section_header('STUDY SELECTION OR ELIGIBILITY CRITERIA',		3).
valid_abstract_section_header('SUBJECTS, METHODS AND RESULTS',		3).
valid_abstract_section_header('SUMMARY ANSWER AND LIMITATIONS',		3).
valid_abstract_section_header('SUMMARY OF BACKGROUND DATE',		3).
valid_abstract_section_header('SUMMARY OF KEY FINDINGS',		3).
valid_abstract_section_header('SUMMARY OF THE DATA',		3).
valid_abstract_section_header('SUPPLEMENTARY DATA',		3).
valid_abstract_section_header('SURGICAL MANAGEMENT',		3).
valid_abstract_section_header('SURGICAL THERAPY',		3).
valid_abstract_section_header('SYNONYMS',		3).
valid_abstract_section_header('T OR',		3).
valid_abstract_section_header('TABLE OF CONTENTS',		3).
valid_abstract_section_header('TARGET GROUP',		3).
valid_abstract_section_header('TEACHING METHOD',		3).
valid_abstract_section_header('TECHNICAL FEATURES',		3).
valid_abstract_section_header('TECHNIQUE DESCRIPTION',		3).
valid_abstract_section_header('TESTING AND IMPLICATIONS OF THE HYPOTHESIS',		3).
valid_abstract_section_header('TG/TG',		3).
valid_abstract_section_header('THE AIMS OF THIS STUDY WERE',		3).
valid_abstract_section_header('THE CONCLUSIONS',		3).
valid_abstract_section_header('THE FRAMEWORK',		3).
valid_abstract_section_header('THE MATERIALS AND METHODS',		3).
valid_abstract_section_header('THE OBJECT',		3).
valid_abstract_section_header('THE PARTNERSHIP',		3).
valid_abstract_section_header('THE PATIENT',		3).
valid_abstract_section_header('THE PURPOSE OF THE PAPER',		3).
valid_abstract_section_header('THE RESULT',		3).
valid_abstract_section_header('THE STUDY\'S RATIONALE',		3).
valid_abstract_section_header('THE WAY FORWARD',		3).
valid_abstract_section_header('THEMES',		3).
valid_abstract_section_header('THEORETICAL APPROACHES',		3).
valid_abstract_section_header('THEORETICAL CONSIDERATIONS',		3).
valid_abstract_section_header('THERAPEUTIC APPLICATIONS',		3).
valid_abstract_section_header('THERAPEUTIC APPROACHES',		3).
valid_abstract_section_header('THERAPEUTIC POSSIBILITIES',		3).
valid_abstract_section_header('THERAPEUTIC USE',		3).
valid_abstract_section_header('TOPICS COVERED',		3).
valid_abstract_section_header('TRAINING',		3).
valid_abstract_section_header('TRATAMIENTO Y RESULTADO',		3).
valid_abstract_section_header('TREATMENT AND FOLLOW UP',		3).
valid_abstract_section_header('TREATMENT AND OUTCOMES',		3).
valid_abstract_section_header('TREATMENT AND PREVENTION',		3).
valid_abstract_section_header('TREATMENT AND RESULTS',		3).
valid_abstract_section_header('TREATMENT STRATEGY',		3).
valid_abstract_section_header('TRIAL REGISTRATION DRKS-ID',		3).
valid_abstract_section_header('TRIAL REGISTRATIONS NUMBER',		3).
valid_abstract_section_header('TRIALREGNO',		3).
valid_abstract_section_header('TT, OR',		3).
valid_abstract_section_header('TYPE II',		3).
valid_abstract_section_header('TYPES OF INTERVENTION(S)',		3).
valid_abstract_section_header('UNANSWERED QUESTIONS',		3).
valid_abstract_section_header('WHAT IS KNOWN ABOUT THE SUBJECT',		3).
valid_abstract_section_header('WHY THIS MATTERS TO US',		3).
valid_abstract_section_header('WIDER IMPLICATIONS OF THESE FINDINGS',		3).
valid_abstract_section_header('YEARS',		3).
valid_abstract_section_header('ZIELSETZUNG',		3).
valid_abstract_section_header('11 BACKGROUND',		2).
valid_abstract_section_header('13 RESULTS',		2).
valid_abstract_section_header('2000 MATHEMATICS SUBJECT CLASSIFICATION',		2).
valid_abstract_section_header('2010 MATHEMATICS SUBJECT CLASSIFICATION',		2).
valid_abstract_section_header('4 CONCLUSION',		2).
valid_abstract_section_header('A CASE STUDY',		2).
valid_abstract_section_header('A-DROP',		2).
valid_abstract_section_header('AA, OR',		2).
valid_abstract_section_header('ABSTARCT BACKGROUND',		2).
valid_abstract_section_header('ABSTRACT OF REVIEWED ARTICLE',		2).
valid_abstract_section_header('ACCESS',		2).
valid_abstract_section_header('ACCESSION NUMBERS',		2).
valid_abstract_section_header('ACHIEVEMENTS/PRACTICAL RECOMMENDATIONS',		2).
valid_abstract_section_header('ACKNOWLEGMENT',		2).
valid_abstract_section_header('ACTION STEPS',		2).
valid_abstract_section_header('ACTUALITIES',		2).
valid_abstract_section_header('ACTUALITIES AND STRONG POINTS',		2).
valid_abstract_section_header('ACTUALITY',		2).
valid_abstract_section_header('ADAPTED MANAGEMENT',		2).
valid_abstract_section_header('ADDITIONAL MATERIAL AVAILABLE ONLINE',		2).
valid_abstract_section_header('ADDITIONAL ONLINE MATERIAL',		2).
valid_abstract_section_header('ADDITIONAL TESTS',		2).
valid_abstract_section_header('ADDRESSING THE ISSUE',		2).
valid_abstract_section_header('ADDRESSING THE ISSUES',		2).
valid_abstract_section_header('ADJUVANT THERAPIES',		2).
valid_abstract_section_header('ADMINISTRATION',		2).
valid_abstract_section_header('ADVANCES IN KNOWLEDGE AND IMPLICATION FOR PATIENT CARE',		2).
valid_abstract_section_header('ADVANTAGES AND DISADVANTAGES',		2).
valid_abstract_section_header('AFTERCARE',		2).
valid_abstract_section_header('AGRONOMIC IMPORTANCE',		2).
valid_abstract_section_header('AIM',		2).
valid_abstract_section_header('AIM & METHOD',		2).
valid_abstract_section_header('AIM AND CONTENT',		2).
valid_abstract_section_header('AIM AND HYPOTHESIS',		2).
valid_abstract_section_header('AIM AND METHODS OF THE STUDY',		2).
valid_abstract_section_header('AIM AND/OR HYPOTHESIS',		2).
valid_abstract_section_header('AIM OF THE WORK AND METHODS',		2).
valid_abstract_section_header('AIM OF THIS PAPER',		2).
valid_abstract_section_header('AIM OF THIS REVIEW',		2).
valid_abstract_section_header('AIM OF THIS WORK',		2).
valid_abstract_section_header('AIM, METHODS',		2).
valid_abstract_section_header('AIM/QUESTION',		2).
valid_abstract_section_header('AIMS/METHOD',		2).
valid_abstract_section_header('AIMS/METHODOLOGY',		2).
valid_abstract_section_header('AIMS/RESULTS',		2).
valid_abstract_section_header('ALGORITHM',		2).
valid_abstract_section_header('ALLERGY',		2).
valid_abstract_section_header('ANALGESIA',		2).
valid_abstract_section_header('ANALYSIS AND EVIDENCE',		2).
valid_abstract_section_header('ANALYSIS METHOD',		2).
valid_abstract_section_header('ANALYSIS OF THE RESULTS',		2).
valid_abstract_section_header('ANALYTICAL METHODS',		2).
valid_abstract_section_header('ANATOMIC CONSIDERATIONS',		2).
valid_abstract_section_header('ANGIOTENSIN CONVERTING ENZYME (ACE) INHIBITORS',		2).
valid_abstract_section_header('ANGIOTENSIN CONVERTING ENZYME INHIBITORS',		2).
valid_abstract_section_header('ANGIOTENSIN II RECEPTOR ANTAGONISTS',		2).
valid_abstract_section_header('ANGIOTENSIN II RECEPTORS',		2).
valid_abstract_section_header('ANIMAL EXPERIMENTS',		2).
valid_abstract_section_header('ANIMAL MODELS',		2).
valid_abstract_section_header('ANIMALS/SUBJECTS',		2).
valid_abstract_section_header('ANTECEDENTES/OBJETIVO',		2).
valid_abstract_section_header('ANTICIPATED OUTCOMES',		2).
valid_abstract_section_header('APPLICATION OF THE MODEL',		2).
valid_abstract_section_header('APPLICATION TO MEDICAL EDUCATION',		2).
valid_abstract_section_header('APPLICATION TO TREATMENT',		2).
valid_abstract_section_header('APPRAISAL',		2).
valid_abstract_section_header('APPROACH OR DESIGN',		2).
valid_abstract_section_header('APPROACH TO ADDRESSING THE SIX IOM QUALITY AIMS',		2).
valid_abstract_section_header('AREA TIMELY FOR DEVELOPING RESEARCH',		2).
valid_abstract_section_header('AREAS COVERED IN THIS REVIEW/WHAT THE READER WILL GAIN',		2).
valid_abstract_section_header('AREAS OF CONTENTION',		2).
valid_abstract_section_header('ARTICLE SUMMARY',		2).
valid_abstract_section_header('ASPIRIN',		2).
valid_abstract_section_header('ASSESSMENT AND DATA EXTRACTION',		2).
valid_abstract_section_header('ASSESSMENT OF EXPOSURE',		2).
valid_abstract_section_header('ASSESSMENT OF PROBLEMS',		2).
valid_abstract_section_header('ASSESSMENT OF QUALITY OF EVIDENCE',		2).
valid_abstract_section_header('AUDIT STANDARDS',		2).
valid_abstract_section_header('AUSTRALIA NEW ZEALAND CLINICAL TRIALS REGISTRY NUMBER',		2).
valid_abstract_section_header('AUTHOR JUSTIFICATIONS',		2).
valid_abstract_section_header('AUTHORS SUMMARY',		2).
valid_abstract_section_header('AUTHORS\' CONCLUSIONS:',		2).
valid_abstract_section_header('AVAILABILITY/SUPPLEMENTARY INFORMATION',		2).
valid_abstract_section_header('BACKDROUND',		2).
valid_abstract_section_header('BACKGOUND AND AIM',		2).
valid_abstract_section_header('BACKGROUD AND AIM',		2).
valid_abstract_section_header('BACKGROUD AND OBJECTIVES',		2).
valid_abstract_section_header('BACKGROUND & GOALS',		2).
valid_abstract_section_header('BACKGROUND & RATIONALE',		2).
valid_abstract_section_header('BACKGROUND / AIMS',		2).
valid_abstract_section_header('BACKGROUND AIM OF THE STUDY',		2).
valid_abstract_section_header('BACKGROUND AND FINDINGS',		2).
valid_abstract_section_header('BACKGROUND AND THE AIM OF THE STUDY',		2).
valid_abstract_section_header('BACKGROUND SETTING',		2).
valid_abstract_section_header('BACKGROUND, AIM',		2).
valid_abstract_section_header('BACKGROUND-OBJECTIVE',		2).
valid_abstract_section_header('BACKGROUND/METHODOLOGY',		2).
valid_abstract_section_header('BACKGROUND/PRINCIPAL FINDINGS',		2).
valid_abstract_section_header('BACKGROUNG',		2).
valid_abstract_section_header('BACKGRUND',		2).
valid_abstract_section_header('BACTERIA AND VIRUSES',		2).
valid_abstract_section_header('BAKGROUND',		2).
valid_abstract_section_header('BASELINE CHARACTERISTICS',		2).
valid_abstract_section_header('BASIC LIFE SUPPORT',		2).
valid_abstract_section_header('BASIC PROBLEM',		2).
valid_abstract_section_header('BASIC/CLINICAL SCIENCE',		2).
valid_abstract_section_header('BASIS AND OBJECTIVE',		2).
valid_abstract_section_header('BEFORE STARTING TREATMENT',		2).
valid_abstract_section_header('BEST PRACTICE',		2).
valid_abstract_section_header('BEST PRACTICE ADVICE 1',		2).
valid_abstract_section_header('BEST PRACTICE ADVICE 2',		2).
valid_abstract_section_header('BEST PRACTICE ADVICE 3',		2).
valid_abstract_section_header('BEST PRACTICE ADVICE 4',		2).
valid_abstract_section_header('BEST PRACTICE ADVICE 5',		2).
valid_abstract_section_header('BEST PRACTICE ADVICE 6',		2).
valid_abstract_section_header('BIAS, LIMITATIONS AND GENERALIZABILITY',		2).
valid_abstract_section_header('BIOACTIVITY',		2).
valid_abstract_section_header('BIOLOGICAL VARIATION AND ANALYTICAL PERFORMANCE',		2).
valid_abstract_section_header('BREAST CANCER',		2).
valid_abstract_section_header('BULLET POINTS',		2).
valid_abstract_section_header('CA/AA',		2).
valid_abstract_section_header('CABG HR',		2).
valid_abstract_section_header('CALCIUM ANTAGONISTS',		2).
valid_abstract_section_header('CANCER DETECTION',		2).
valid_abstract_section_header('CANCER PREVENTION AND THERAPY',		2).
valid_abstract_section_header('CARDIAC MAGNETIC RESONANCE IMAGING',		2).
valid_abstract_section_header('CARDIAC POSITRON EMISSION TOMOGRAPHY',		2).
valid_abstract_section_header('CASE CLINIC',		2).
valid_abstract_section_header('CASE CONTROL STUDY',		2).
valid_abstract_section_header('CASE DESCRIPTIONS AND RESULTS',		2).
valid_abstract_section_header('CASE EXPERIENCE',		2).
valid_abstract_section_header('CASE FINDINGS',		2).
valid_abstract_section_header('CASE HISTORY AND DIAGNOSIS',		2).
valid_abstract_section_header('CASE HISTORY AND EXAMINATION',		2).
valid_abstract_section_header('CASE HISTORY AND FINDINGS',		2).
valid_abstract_section_header('CASE I',		2).
valid_abstract_section_header('CASE NOTE',		2).
valid_abstract_section_header('CASE ONE',		2).
valid_abstract_section_header('CASE PRESENTAION',		2).
valid_abstract_section_header('CASE PRESENTATION AND IDENTIFICATION',		2).
valid_abstract_section_header('CASE PRESENTATION AND LITERATURE REVIEW',		2).
valid_abstract_section_header('CASE PRESENTED',		2).
valid_abstract_section_header('CASE REPORT AND CONCLUSION',		2).
valid_abstract_section_header('CASE REPORT AND REVIEW OF LITERATURE',		2).
valid_abstract_section_header('CASE REPORT/RESULTS',		2).
valid_abstract_section_header('CASE REPORTS AND METHODS',		2).
valid_abstract_section_header('CASE REPRESENTATION',		2).
valid_abstract_section_header('CASE REVIEWS',		2).
valid_abstract_section_header('CASE SERIES PRESENTATION',		2).
valid_abstract_section_header('CASE SERIES REPORT',		2).
valid_abstract_section_header('CASE STUDY 1',		2).
valid_abstract_section_header('CASE TWO',		2).
valid_abstract_section_header('CASES AND SETTING',		2).
valid_abstract_section_header('CASES REPORT/DISCUSSION',		2).
valid_abstract_section_header('CASES SUMMARY',		2).
valid_abstract_section_header('CAUSES OF INJURY',		2).
valid_abstract_section_header('CAUTIONS',		2).
valid_abstract_section_header('CHANGE IN PRACTICE',		2).
valid_abstract_section_header('CHIEF COMPLAINTS',		2).
valid_abstract_section_header('CHINESE CLINICAL TRIALS REGISTRATION',		2).
valid_abstract_section_header('CHOICE OF SOLUTION AND IMPLEMENTATION',		2).
valid_abstract_section_header('CLASSIFICATION OF DYSPHONIAS',		2).
valid_abstract_section_header('CLINIC CASES',		2).
valid_abstract_section_header('CLINICAL AND RESEARCH IMPLICATION',		2).
valid_abstract_section_header('CLINICAL CASE REPORTS',		2).
valid_abstract_section_header('CLINICAL EXAMINATION',		2).
valid_abstract_section_header('CLINICAL EXPRESSION',		2).
valid_abstract_section_header('CLINICAL FINDINGS/DIAGNOSES',		2).
valid_abstract_section_header('CLINICAL MESSAGE',		2).
valid_abstract_section_header('CLINICAL OUTCOMES',		2).
valid_abstract_section_header('CLINICAL PATHWAY',		2).
valid_abstract_section_header('CLINICAL PRESENTATION AND RESULTS',		2).
valid_abstract_section_header('CLINICAL PRESENTATION/INTERVENTION',		2).
valid_abstract_section_header('CLINICAL REGISTRATION',		2).
valid_abstract_section_header('CLINICAL RELEVANCE STATEMENT',		2).
valid_abstract_section_header('CLINICAL RELEVANCE/APPLICATION',		2).
valid_abstract_section_header('CLINICAL RELEVANCES',		2).
valid_abstract_section_header('CLINICAL SERIES',		2).
valid_abstract_section_header('CLINICAL TRAIL REGISTRATION NUMBER',		2).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION ID NUMBER',		2).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION INFORMATION URL',		2).
valid_abstract_section_header('CLINICAL TRIALS INCLUDED',		2).
valid_abstract_section_header('CLINICAL TRIALSGOV IDENTIFIER',		2).
valid_abstract_section_header('CLINICALTRIALSGOV, NUMBER',		2).
valid_abstract_section_header('CLINICIAL RELEVANCE',		2).
valid_abstract_section_header('CLINICS',		2).
valid_abstract_section_header('CO-EXISTING DISEASES',		2).
valid_abstract_section_header('COCLUSIONS',		2).
valid_abstract_section_header('COCNLUSION',		2).
valid_abstract_section_header('COLLECTION OF DATA',		2).
valid_abstract_section_header('COMBINATION THERAPY',		2).
valid_abstract_section_header('COMPARISON TO EXISTING METHOD(S)',		2).
valid_abstract_section_header('COMPARISON WITH EXISITING METHOD(S)',		2).
valid_abstract_section_header('COMPARISON WITH EXISTING METHOD (S)',		2).
valid_abstract_section_header('COMPARISON WITH EXISTING METHODS AND CONCLUSIONS',		2).
valid_abstract_section_header('COMPETING INTEREST(S)',		2).
valid_abstract_section_header('CONCEPT OF DIABETES MELLITUS',		2).
valid_abstract_section_header('CONCEPTS/TRENDS',		2).
valid_abstract_section_header('CONCEPTUAL MODEL',		2).
valid_abstract_section_header('CONCLUIOSN',		2).
valid_abstract_section_header('CONCLUISION',		2).
valid_abstract_section_header('CONCLULSIONS',		2).
valid_abstract_section_header('CONCLUSION & DISCUSSION',		2).
valid_abstract_section_header('CONCLUSION & RECOMMENDATIONS',		2).
valid_abstract_section_header('CONCLUSION AND APPLICATION',		2).
valid_abstract_section_header('CONCLUSION AND APPLICATION TO PRACTICE',		2).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR NURSING POLICY',		2).
valid_abstract_section_header('CONCLUSION SIGNIFICANCE',		2).
valid_abstract_section_header('CONCLUSION-DISCUSSION',		2).
valid_abstract_section_header('CONCLUSION/NEXT STEPS',		2).
valid_abstract_section_header('CONCLUSION/PERSPECTIVES',		2).
valid_abstract_section_header('CONCLUSION/PRACTICE IMPLICATIONS',		2).
valid_abstract_section_header('CONCLUSIONS & IMPLICATION',		2).
valid_abstract_section_header('CONCLUSIONS & INTERFERENCES',		2).
valid_abstract_section_header('CONCLUSIONS & RECOMMENDATIONS',		2).
valid_abstract_section_header('CONCLUSIONS & RELEVANCE',		2).
valid_abstract_section_header('CONCLUSIONS / LEVEL OF EVIDENCE',		2).
valid_abstract_section_header('CONCLUSIONS / LEVEL OF EVIDENCE 3',		2).
valid_abstract_section_header('CONCLUSIONS AND APPLICATIONS TO PRACTICE',		2).
valid_abstract_section_header('CONCLUSIONS AND CLINICIAL RELEVENCE',		2).
valid_abstract_section_header('CONCLUSIONS AND COMMENTS',		2).
valid_abstract_section_header('CONCLUSIONS AND DISCUSSIONS',		2).
valid_abstract_section_header('CONCLUSIONS AND FUTURE WORK',		2).
valid_abstract_section_header('CONCLUSIONS AND HYPOTHESIS',		2).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATION OF KEY FINDINGS',		2).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR CLINICAL PRACTICE',		2).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR THE PRACTICE',		2).
valid_abstract_section_header('CONCLUSIONS AND OUTCOME',		2).
valid_abstract_section_header('CONCLUSIONS AND POTENTIAL IMPLICATIONS',		2).
valid_abstract_section_header('CONCLUSIONS AND RESULTS',		2).
valid_abstract_section_header('CONCLUSIONS AND SIGNIFICANCE AND IMPACT OF THE STUDY',		2).
valid_abstract_section_header('CONCLUSIONS AND THEIR SIGNIFICANCE',		2).
valid_abstract_section_header('CONCLUSIONS, BRIEF SUMMARY AND POTENTIAL IMPLICATIONS',		2).
valid_abstract_section_header('CONCLUSIONS, RECOMMENDATIONS AND PERSPECTIVES',		2).
valid_abstract_section_header('CONCLUSIONS/CLINICAL IMPLICATIONS',		2).
valid_abstract_section_header('CONCLUSIONS/CLINICAL IMPORTANCE',		2).
valid_abstract_section_header('CONCLUSIONS/CLINICAL SIGNIFICANCE',		2).
valid_abstract_section_header('CONCLUSIONS/FUTURE DIRECTIONS',		2).
valid_abstract_section_header('CONCLUSIONS/GENERAL SIGNIFICANCE',		2).
valid_abstract_section_header('CONCLUSIONS/IMPACT',		2).
valid_abstract_section_header('CONCLUSIONS/IMPLICATIONS FOR CANCER SURVIVORS',		2).
valid_abstract_section_header('CONCLUSIONS/PERSPECTIVES',		2).
valid_abstract_section_header('CONCLUSIOS',		2).
valid_abstract_section_header('CONCLUSON',		2).
valid_abstract_section_header('CONCLUSSIONS',		2).
valid_abstract_section_header('CONFERENCE PARTICIPANTS',		2).
valid_abstract_section_header('CONFLICT OF INTEREST STATEMENT',		2).
valid_abstract_section_header('CONFLICTS OF INTEREST/DISCLOSURES',		2).
valid_abstract_section_header('CONSENSUS METHODS',		2).
valid_abstract_section_header('CONSENSUS STATEMENTS',		2).
valid_abstract_section_header('CONSEQUENCES OF THE HYPOTHESIS',		2).
valid_abstract_section_header('CONSERVATIVE TREATMENT',		2).
valid_abstract_section_header('CONSUMER SUMMARY',		2).
valid_abstract_section_header('CONTACT HYPERSENSITIVITY STUDIES',		2).
valid_abstract_section_header('CONTENT OF REVIEW',		2).
valid_abstract_section_header('CONTENTS AND METHODS',		2).
valid_abstract_section_header('CONTENTS OF THE PROPOSAL',		2).
valid_abstract_section_header('CONTEXTO',		2).
valid_abstract_section_header('CONTEXTUAL CHALLENGES',		2).
valid_abstract_section_header('CONTROL DATA',		2).
valid_abstract_section_header('CORONARY ARTERY DISEASE',		2).
valid_abstract_section_header('CORRECTION',		2).
valid_abstract_section_header('CORTICOSTEROIDS',		2).
valid_abstract_section_header('COST ANALYSIS',		2).
valid_abstract_section_header('COUNSELING',		2).
valid_abstract_section_header('CRITERIA FOR CONSIDERING STUDIES FOR THIS REVIEW',		2).
valid_abstract_section_header('CRITICAL ANALYSIS',		2).
valid_abstract_section_header('CRITICAL APPRAISAL, DATA COLLECTION AND ANALYSIS',		2).
valid_abstract_section_header('CSFMC',		2).
valid_abstract_section_header('CTB-SAP',		2).
valid_abstract_section_header('CULTURAL ASPECTS',		2).
valid_abstract_section_header('CURRENT EVENTS',		2).
valid_abstract_section_header('CURRENT EVIDENCE',		2).
valid_abstract_section_header('CURRENT GUIDELINES',		2).
valid_abstract_section_header('CURRENT INVESTIGATIONS',		2).
valid_abstract_section_header('CURRENT KNOWLEDGE AND KEYPOINTS',		2).
valid_abstract_section_header('CURRENT POSITION AND MAJOR POINTS',		2).
valid_abstract_section_header('CURRENT SITUATION AND PROBLEMS',		2).
valid_abstract_section_header('CURRENT STATE',		2).
valid_abstract_section_header('CURRENT TREATMENT',		2).
valid_abstract_section_header('CURRICULUM DESCRIPTION',		2).
valid_abstract_section_header('CURRICULUM DESIGN',		2).
valid_abstract_section_header('Case description:',		2).
valid_abstract_section_header('Clinical Application',		2).
valid_abstract_section_header('Comentario',		2).
valid_abstract_section_header('Comments:',		2).
valid_abstract_section_header('DATA ACCESS',		2).
valid_abstract_section_header('DATA APPRAISAL AND SYNTHESIS METHODS',		2).
valid_abstract_section_header('DATA COLLECTION AND DATA ANALYSIS',		2).
valid_abstract_section_header('DATA COLLECTION PROCEDURES',		2).
valid_abstract_section_header('DATA EXTRACTION, SYNTHESIS',		2).
valid_abstract_section_header('DATA INTERPRETATIONS',		2).
valid_abstract_section_header('DATA SOURCES AND ANALYSIS',		2).
valid_abstract_section_header('DATA SOURCES/COLLECTION',		2).
valid_abstract_section_header('DATA SOURCES/SETTINGS',		2).
valid_abstract_section_header('DATA STUDIES',		2).
valid_abstract_section_header('DATA SYNTHESIS AND EXTRACTION',		2).
valid_abstract_section_header('DATA/METHODS',		2).
valid_abstract_section_header('DATABASE AND DATA TREATMENT',		2).
valid_abstract_section_header('DATABASES AND SOURCES',		2).
valid_abstract_section_header('DECISION MAKING',		2).
valid_abstract_section_header('DEFINITION AND CLASSIFICATION',		2).
valid_abstract_section_header('DEIGN',		2).
valid_abstract_section_header('DEL/DEL',		2).
valid_abstract_section_header('DEMENTIA',		2).
valid_abstract_section_header('DESARROLLO',		2).
valid_abstract_section_header('DESCRIPTION AND EVALUATION',		2).
valid_abstract_section_header('DESCRIPTION OF IMPLEMENTATION',		2).
valid_abstract_section_header('DESCRIPTION OF INTEGRATED CARE CASE',		2).
valid_abstract_section_header('DESCRIPTION OF POLICY AND PRACTICE',		2).
valid_abstract_section_header('DESCRIPTION OF THE DISORDER',		2).
valid_abstract_section_header('DESCRIPTION OF THE METHOD',		2).
valid_abstract_section_header('DESCRIPTION OF THE SIMULATOR',		2).
valid_abstract_section_header('DESCRIPTION OF THE TRAINING MODEL',		2).
valid_abstract_section_header('DESCRIPTION OF TOPIC AND RELATED EVIDENCE',		2).
valid_abstract_section_header('DESIGN & METHOD',		2).
valid_abstract_section_header('DESIGN & PATIENTS',		2).
valid_abstract_section_header('DESIGN & SUBJECTS',		2).
valid_abstract_section_header('DESIGN AND APPROACH',		2).
valid_abstract_section_header('DESIGN AND LOCATION',		2).
valid_abstract_section_header('DESIGN AND MAIN MEASURES',		2).
valid_abstract_section_header('DESIGN AND SETTING, AND PATIENTS',		2).
valid_abstract_section_header('DESIGN SETTING AND METHODS',		2).
valid_abstract_section_header('DESIGN SETTING AND PATIENT',		2).
valid_abstract_section_header('DESIGN(S)',		2).
valid_abstract_section_header('DESIGN, MEASUREMENTS AND RESULTS',		2).
valid_abstract_section_header('DESIGN, METHODS, RESULTS',		2).
valid_abstract_section_header('DESIGN, PARTICIPANTS, AND MAIN MEASURES',		2).
valid_abstract_section_header('DESIGN, SETTING & PARTICIPANTS',		2).
valid_abstract_section_header('DESIGN, SETTING AND POPULATION',		2).
valid_abstract_section_header('DESIGN, SETTING PARTICIPANTS, & MEASUREMENTS',		2).
valid_abstract_section_header('DESIGN, SETTING, AND MATERIALS',		2).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS AND METHODS',		2).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, & MEASUREMENT',		2).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, &MEASUREMENTS',		2).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS, AND INTERVENTIONS',		2).
valid_abstract_section_header('DESIGN, SUBJECTS AND OUTCOME MEASURES',		2).
valid_abstract_section_header('DESIGN/SETTING/POPULATION',		2).
valid_abstract_section_header('DESIGN/SETTING/SAMPLE',		2).
valid_abstract_section_header('DESIGN/SETTING/SUBJECTS/METHODS',		2).
valid_abstract_section_header('DETECTION',		2).
valid_abstract_section_header('DETERMINANTS',		2).
valid_abstract_section_header('DEVELOPMENT OF THE ACTIVITY AND MATERIALS',		2).
valid_abstract_section_header('DEVELOPMENTS',		2).
valid_abstract_section_header('DIAGNOSES AND INTERVENTIONS',		2).
valid_abstract_section_header('DIAGNOSES, INTERVENTIONS AND OUTCOMES',		2).
valid_abstract_section_header('DIAGNOSIS AND EPIDEMIOLOGY',		2).
valid_abstract_section_header('DIAGNOSIS, THERAPY, AND COURSE',		2).
valid_abstract_section_header('DIAGNOSIS/TREATMENT',		2).
valid_abstract_section_header('DIAGNOSTIC ASSESSMENT',		2).
valid_abstract_section_header('DIAGNOSTIC CHALLENGES',		2).
valid_abstract_section_header('DIAGNOSTIC EVALUATION',		2).
valid_abstract_section_header('DIAGNOSTIC MEASURES',		2).
valid_abstract_section_header('DIAGNOSTIC PROCEDURE',		2).
valid_abstract_section_header('DICUSSION',		2).
valid_abstract_section_header('DIFFERENTIAL DIAGNOSTICS',		2).
valid_abstract_section_header('DIFFICULTIES',		2).
valid_abstract_section_header('DISADVANTAGES',		2).
valid_abstract_section_header('DISCCUSION',		2).
valid_abstract_section_header('DISCUSSION AND COMMENTS',		2).
valid_abstract_section_header('DISCUSSION AND FINAL CONSIDERATIONS',		2).
valid_abstract_section_header('DISCUSSION AND LESSONS LEARNED',		2).
valid_abstract_section_header('DISCUSSION AND LITERATURE REVIEW',		2).
valid_abstract_section_header('DISCUSSION AND PERSPECTIVES',		2).
valid_abstract_section_header('DISCUSSION AND SCIENTIFIC SIGNIFICANCE',		2).
valid_abstract_section_header('DISCUSSION(S)',		2).
valid_abstract_section_header('DISCUSSION, CONCLUSIONS',		2).
valid_abstract_section_header('DISCUSSION-CONCLUSIONS',		2).
valid_abstract_section_header('DISCUSSION/IMPACT/RECOMMENDATIONS',		2).
valid_abstract_section_header('DISCUSSION/IMPLICATIONS',		2).
valid_abstract_section_header('DISCUSSION/SIGNIFICANCE',		2).
valid_abstract_section_header('DISCUSSON',		2).
valid_abstract_section_header('DISEASE AND TREATMENT',		2).
valid_abstract_section_header('DISKUSSION',		2).
valid_abstract_section_header('DISUSSION',		2).
valid_abstract_section_header('DIURETICS',		2).
valid_abstract_section_header('DOBUTAMINE ECHOCARDIOGRAPHY',		2).
valid_abstract_section_header('DOCUMENTATION',		2).
valid_abstract_section_header('DONOR(S)',		2).
valid_abstract_section_header('DONORS, METHODS',		2).
valid_abstract_section_header('DREAM',		2).
valid_abstract_section_header('DRUG INTERACTIONS',		2).
valid_abstract_section_header('DURATION OF TREATMENT',		2).
valid_abstract_section_header('DYSTONIA',		2).
valid_abstract_section_header('Data Source',		2).
valid_abstract_section_header('Debate',		2).
valid_abstract_section_header('Design, Setting and Participants',		2).
valid_abstract_section_header('Design, Setting, Participants',		2).
valid_abstract_section_header('E-ZES',		2).
valid_abstract_section_header('EBM LEVEL IV',		2).
valid_abstract_section_header('EDITORIAL SUMMARY',		2).
valid_abstract_section_header('EDITORS NOTE',		2).
valid_abstract_section_header('EDUCATIONAL MODEL',		2).
valid_abstract_section_header('EES/ZES',		2).
valid_abstract_section_header('EFFICACY ENDPOINTS',		2).
valid_abstract_section_header('EMERGING EVIDENCE',		2).
valid_abstract_section_header('EMPIRICAL APPLICATION',		2).
valid_abstract_section_header('EMPIRICAL STUDIES',		2).
valid_abstract_section_header('ENVIRONMENTAL FACTORS',		2).
valid_abstract_section_header('ENVIRONNEMENT',		2).
valid_abstract_section_header('ENZYME',		2).
valid_abstract_section_header('EPIDEMIOLOGY IN FRANCE',		2).
valid_abstract_section_header('ESSENTIAL HYPERTENSION',		2).
valid_abstract_section_header('ESTABLISHED FACTS',		2).
valid_abstract_section_header('ETHICS, BENEFITS AND DISSEMINATION',		2).
valid_abstract_section_header('ETHNAOPHARMACOLOGIAL RELEVANCE',		2).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL RELAVANCE',		2).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL RELEVANCE AND BACKGROUND',		2).
valid_abstract_section_header('ETHNOPHARMCOLOGICAL RELEVANCE',		2).
valid_abstract_section_header('ETIOLOGY AND DIAGNOSIS',		2).
valid_abstract_section_header('EVALUATING SUCCESS',		2).
valid_abstract_section_header('EVALUATION AND KEY ISSUES',		2).
valid_abstract_section_header('EVALUATION AND LESSONS LEARNED',		2).
valid_abstract_section_header('EVALUATION COMPONENT',		2).
valid_abstract_section_header('EVALUATION CRITERIA',		2).
valid_abstract_section_header('EVALUATION OF',		2).
valid_abstract_section_header('EVIDENCE AQUISITION',		2).
valid_abstract_section_header('EVIDENCES ACQUISITION',		2).
valid_abstract_section_header('EVIDENCES SYNTHESIS',		2).
valid_abstract_section_header('EXAMINATION FINDINGS',		2).
valid_abstract_section_header('EXEGESES',		2).
valid_abstract_section_header('EXPERIMENT APPROACH',		2).
valid_abstract_section_header('EXPERIMENTAL FINDINGS',		2).
valid_abstract_section_header('EXPERIMENTAL MODELS',		2).
valid_abstract_section_header('EXPERIMENTS AND THEORY',		2).
valid_abstract_section_header('EXPERT COMMENTARY',		2).
valid_abstract_section_header('EXPLORATIONS',		2).
valid_abstract_section_header('EXPOSURE AND OUTCOME',		2).
valid_abstract_section_header('EXPOSURES FOR OBSERVATIONAL STUDIES',		2).
valid_abstract_section_header('EXPOSURES OF INTEREST',		2).
valid_abstract_section_header('FAVORING FACTORS',		2).
valid_abstract_section_header('FEEDBACK',		2).
valid_abstract_section_header('FGR/CONTROL',		2).
valid_abstract_section_header('FGR/FF',		2).
valid_abstract_section_header('FGR/FR',		2).
valid_abstract_section_header('FIBTEM',		2).
valid_abstract_section_header('FINANCING',		2).
valid_abstract_section_header('FINDING AND CONCLUSION',		2).
valid_abstract_section_header('FINDINGS AND DIAGNOSIS',		2).
valid_abstract_section_header('FINDINGS AND SIGNIFICANCE',		2).
valid_abstract_section_header('FOLLOW-UP PERIOD',		2).
valid_abstract_section_header('FREQUENCY OF FOLLOW-UP LABORATORY TESTS',		2).
valid_abstract_section_header('FRONTLINE THERAPY',		2).
valid_abstract_section_header('FUNCTION',		2).
valid_abstract_section_header('FUNCTIONS',		2).
valid_abstract_section_header('FUNDING SOURCE',		2).
valid_abstract_section_header('FUNDING SOURCES',		2).
valid_abstract_section_header('FURTHER WORK',		2).
valid_abstract_section_header('FUTURE ASPECTS',		2).
valid_abstract_section_header('FUTURE DEVELOPMENTS',		2).
valid_abstract_section_header('FUTURE PERSPECTIVE',		2).
valid_abstract_section_header('FUTURE PROJECTS',		2).
valid_abstract_section_header('FUTURE TRENDS',		2).
valid_abstract_section_header('Final considerations',		2).
valid_abstract_section_header('G, OR',		2).
valid_abstract_section_header('GA/AA',		2).
valid_abstract_section_header('GC/GG',		2).
valid_abstract_section_header('GENERAL OBJECTIVE',		2).
valid_abstract_section_header('GENERAL PURPOSE',		2).
valid_abstract_section_header('GENERAL TREATMENT',		2).
valid_abstract_section_header('GENETICS AND ENVIRONMENT',		2).
valid_abstract_section_header('GENOMIC STRUCTURE',		2).
valid_abstract_section_header('GG/GA',		2).
valid_abstract_section_header('GLS (HR',		2).
valid_abstract_section_header('GOAL AND BACKGROUND',		2).
valid_abstract_section_header('GOAL AND OBJECTIVES',		2).
valid_abstract_section_header('GT/GG',		2).
valid_abstract_section_header('GUIDANCE',		2).
valid_abstract_section_header('GUIDELINE DEVELOPMENT',		2).
valid_abstract_section_header('HEALTH ECONOMICS',		2).
valid_abstract_section_header('HEALTH NEEDS OF RURAL AREAS',		2).
valid_abstract_section_header('HFE ANALYSIS',		2).
valid_abstract_section_header('HIGH-VALUE CARE ADVICE 1',		2).
valid_abstract_section_header('HIGH-VALUE CARE ADVICE 2',		2).
valid_abstract_section_header('HIGH-VALUE CARE ADVICE 3',		2).
valid_abstract_section_header('HIGH-VALUE CARE ADVICE 4',		2).
valid_abstract_section_header('HIGHEST AVAILABLE EVIDENCE',		2).
valid_abstract_section_header('HINTERGRUND UND FRAGESTELLUNG',		2).
valid_abstract_section_header('HISTORY AND ADMISSION DIAGNOSIS',		2).
valid_abstract_section_header('HISTORY AND EVOLUTION',		2).
valid_abstract_section_header('HISTORY AND REASON FOR ADMISSION',		2).
valid_abstract_section_header('HOSPITAL BASED',		2).
valid_abstract_section_header('HOST RANGE AND SYMPTOMS',		2).
valid_abstract_section_header('HOSTING EVENT',		2).
valid_abstract_section_header('HUMAN CHALLENGE AND CONCLUSIONS',		2).
valid_abstract_section_header('HUMAN DATA',		2).
valid_abstract_section_header('HYPERTENSION AND DIABETES',		2).
valid_abstract_section_header('ICNMD XIII',		2).
valid_abstract_section_header('IDENTIFIER',		2).
valid_abstract_section_header('III) RESULTS',		2).
valid_abstract_section_header('IMAGING MODALITIES',		2).
valid_abstract_section_header('IMMEDIATE RESULTS',		2).
valid_abstract_section_header('IMMUNOSUPPRESSION',		2).
valid_abstract_section_header('IMPACT OF DISEASE',		2).
valid_abstract_section_header('IMPLEMENTATION AND AVAILABILITY',		2).
valid_abstract_section_header('IMPLEMENTATION AND EVALUATION',		2).
valid_abstract_section_header('IMPLEMENTATION DATES AND EXPERIENCE TO DATE',		2).
valid_abstract_section_header('IMPLEMENTATION STRATEGIES',		2).
valid_abstract_section_header('IMPLEMENTING THE PROJECT',		2).
valid_abstract_section_header('IMPLICATION FOR HEALTH POLICIES',		2).
valid_abstract_section_header('IMPLICATION FOR HEALTH POLICY',		2).
valid_abstract_section_header('IMPLICATION FOR PATIENT CARE',		2).
valid_abstract_section_header('IMPLICATION OF KEY FINDINGS',		2).
valid_abstract_section_header('IMPLICATIONS FOR CANCER',		2).
valid_abstract_section_header('IMPLICATIONS FOR CANCER SURVIVORSHIP',		2).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH POLICIES, HEALTH CARE PROVISION AND USE',		2).
valid_abstract_section_header('IMPLICATIONS FOR NPS',		2).
valid_abstract_section_header('IMPLICATIONS FOR NURSING AND HEALTH POLICIES',		2).
valid_abstract_section_header('IMPLICATIONS FOR NURSING EDUCATION',		2).
valid_abstract_section_header('IMPLICATIONS FOR NURSING MANAGEMENT AND LEADERSHIP',		2).
valid_abstract_section_header('IMPLICATIONS FOR PHYSIOTHERAPY PRACTICE',		2).
valid_abstract_section_header('IMPLICATIONS FOR POLICY AND PRACTICE',		2).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE AND/OR POLICY',		2).
valid_abstract_section_header('IMPLICATIONS OF THE HYPOTHESES',		2).
valid_abstract_section_header('IMPORTANCE AND OBJECTIVES',		2).
valid_abstract_section_header('IMPORTANCE OF THE STUDY',		2).
valid_abstract_section_header('IMPORTANCE STATEMENT',		2).
valid_abstract_section_header('IMPRESSION',		2).
valid_abstract_section_header('IN ADULTS',		2).
valid_abstract_section_header('IN CHILDREN',		2).
valid_abstract_section_header('IN DIABETIC SUBJECTS',		2).
valid_abstract_section_header('IN VITRO STUDY',		2).
valid_abstract_section_header('INCLUSIVE CRITERIA',		2).
valid_abstract_section_header('INDICATORS',		2).
valid_abstract_section_header('INDUSTRIAL AREAS',		2).
valid_abstract_section_header('INFECTION CONTROL',		2).
valid_abstract_section_header('INFECTION IN CATS',		2).
valid_abstract_section_header('INJURIES',		2).
valid_abstract_section_header('INPUT',		2).
valid_abstract_section_header('INSTEAD OF CONCLUSION',		2).
valid_abstract_section_header('INSTRUCTIONS',		2).
valid_abstract_section_header('INSTRUMENT DESIGN',		2).
valid_abstract_section_header('INTENDED USERS',		2).
valid_abstract_section_header('INTENSIVE CARE',		2).
valid_abstract_section_header('INTERPRETATION AND IMPLICATIONS',		2).
valid_abstract_section_header('INTERPRETATIONS & CONCLUSIONS',		2).
valid_abstract_section_header('INTERVENTION & MAIN OUTCOME',		2).
valid_abstract_section_header('INTERVENTION (S)',		2).
valid_abstract_section_header('INTERVENTION AND CONTROL',		2).
valid_abstract_section_header('INTERVENTION AND DISCUSSION',		2).
valid_abstract_section_header('INTERVENTION OR EXPOSURE',		2).
valid_abstract_section_header('INTERVENTIONS AND EXPOSURES',		2).
valid_abstract_section_header('INTRODCTION',		2).
valid_abstract_section_header('INTRODCUTION',		2).
valid_abstract_section_header('INTRODUCATION',		2).
valid_abstract_section_header('INTRODUCCTION',		2).
valid_abstract_section_header('INTRODUCTION & AIMS',		2).
valid_abstract_section_header('INTRODUCTION & BACKGROUND',		2).
valid_abstract_section_header('INTRODUCTION & PURPOSE',		2).
valid_abstract_section_header('INTRODUCTION AND AIMS OF THE STUDY',		2).
valid_abstract_section_header('INTRODUCTION AND GOAL',		2).
valid_abstract_section_header('INTRODUCTION AND PURPOSE OF THE STUDY',		2).
valid_abstract_section_header('INTRODUCTION-AIM',		2).
valid_abstract_section_header('INVESTIGATIONAL DRUGS',		2).
valid_abstract_section_header('ISCHEMIA',		2).
valid_abstract_section_header('ISOLATES',		2).
valid_abstract_section_header('ISSUES AND CHALLENGES',		2).
valid_abstract_section_header('ISSUES AND CONCLUSIONS',		2).
valid_abstract_section_header('ISSUES AND PARTICIPANTS',		2).
valid_abstract_section_header('JEL CLASSIFICATIONS',		2).
valid_abstract_section_header('KEY ASPECTS',		2).
valid_abstract_section_header('KEY CONSIDERATIONS',		2).
valid_abstract_section_header('KEY FEATURES',		2).
valid_abstract_section_header('KEY FINDINGS AND CLINICAL RELEVANCE',		2).
valid_abstract_section_header('KEY GUIDELINES',		2).
valid_abstract_section_header('KEY MESSAGE AND RECENT FACTS',		2).
valid_abstract_section_header('KEY METHODOLOGICAL ISSUES',		2).
valid_abstract_section_header('KEY PRACTITIONERS MESSAGE',		2).
valid_abstract_section_header('KEY RESULTS AND CONCLUSION',		2).
valid_abstract_section_header('KEY SENTENCES',		2).
valid_abstract_section_header('KEY WORD',		2).
valid_abstract_section_header('KEY-FINDINGS',		2).
valid_abstract_section_header('KEYMESSAGE',		2).
valid_abstract_section_header('LABELED PROBLEM',		2).
valid_abstract_section_header('LABORATORY INVESTIGATION',		2).
valid_abstract_section_header('LASER PARAMETERS',		2).
valid_abstract_section_header('LATE OUTCOME',		2).
valid_abstract_section_header('LATEST FINDINGS',		2).
valid_abstract_section_header('LEARNING',		2).
valid_abstract_section_header('LEARNING METHODS',		2).
valid_abstract_section_header('LEARNING OBJECTIVES/OUTCOMES',		2).
valid_abstract_section_header('LEGISLATION',		2).
valid_abstract_section_header('LESSONS FROM THE FIELD',		2).
valid_abstract_section_header('LESSONS LEARNED AND CONCLUSIONS',		2).
valid_abstract_section_header('LESSONS SUBSECTIONS',		2).
valid_abstract_section_header('LEVEL OF EVEDINCE',		2).
valid_abstract_section_header('LEVEL OF EVIDENCES',		2).
valid_abstract_section_header('LEVEL OF PROOF, TYPE OF STUDY',		2).
valid_abstract_section_header('LEVEL OF STUDY',		2).
valid_abstract_section_header('LIFE CYCLE',		2).
valid_abstract_section_header('LIFESTYLE',		2).
valid_abstract_section_header('LIMITACIONES',		2).
valid_abstract_section_header('LINKING EVIDENCE TO PRACTICE',		2).
valid_abstract_section_header('LITERATURE REVIEW AIMS',		2).
valid_abstract_section_header('LITERATURE SEARCH RESULTS',		2).
valid_abstract_section_header('LONG-TERM CARE PLAN',		2).
valid_abstract_section_header('LONGHORNED BEETLES (COLEOPTERA',		2).
valid_abstract_section_header('MAIN ARGUMENT',		2).
valid_abstract_section_header('MAIN COMPONENTS',		2).
valid_abstract_section_header('MAIN CONTENT',		2).
valid_abstract_section_header('MAIN DATA',		2).
valid_abstract_section_header('MAIN FACTOR',		2).
valid_abstract_section_header('MAIN INTERVENTIONS',		2).
valid_abstract_section_header('MAIN MEASURE(S)',		2).
valid_abstract_section_header('MAIN OUTCOME MEASURE AND ANALYSIS',		2).
valid_abstract_section_header('MAIN OUTCOME MEASURES/INTERVENTIONS',		2).
valid_abstract_section_header('MAIN OUTCOME MEASURES/PRINCIPAL RESULTS',		2).
valid_abstract_section_header('MAIN OUTCOME MEASURES/STATISTICAL ANALYSES',		2).
valid_abstract_section_header('MAIN OUTCOME(S) AND MEASURES',		2).
valid_abstract_section_header('MAIN PURPOSE AND STARTING POINTS (OBJECTIVES)',		2).
valid_abstract_section_header('MAIN RESULT AND ROLE OF CHANCE',		2).
valid_abstract_section_header('MAIN RESULT AND THE ROLE OF CHANCE',		2).
valid_abstract_section_header('MAIN RESULTS AND CONCLUSION',		2).
valid_abstract_section_header('MAIN RESULTS:',		2).
valid_abstract_section_header('MAIN THESIS',		2).
valid_abstract_section_header('MAIN VARIABLES STUDIED AND MAIN OUTCOME MEASURES',		2).
valid_abstract_section_header('MAJOR CONCLUSION AND GENERAL SIGNIFICANCE',		2).
valid_abstract_section_header('MAJOR POINTS DISCUSSED',		2).
valid_abstract_section_header('MALIGNANT POTENTIAL',		2).
valid_abstract_section_header('MANAGEMENT AND OUTCOME',		2).
valid_abstract_section_header('MANAGEMENT OF OA',		2).
valid_abstract_section_header('MARKERS',		2).
valid_abstract_section_header('MATERIAL AND METODS',		2).
valid_abstract_section_header('MATERIAL AND THE METHOD',		2).
valid_abstract_section_header('MATERIAL, METHOD AND RESULTS',		2).
valid_abstract_section_header('MATERIAL, METHOD, RESULTS',		2).
valid_abstract_section_header('MATERIAL, METHODS, AND RESULTS',		2).
valid_abstract_section_header('MATERIALS (SUBJECTS) AND METHODS',		2).
valid_abstract_section_header('MATERIALS AND',		2).
valid_abstract_section_header('MATERIALS AND FINDINGS',		2).
valid_abstract_section_header('MATERIALS AND MATHODS',		2).
valid_abstract_section_header('MATERIALS AND RESEARCH METHODS',		2).
valid_abstract_section_header('MATERIALS AND SURGICAL TECHNIQUES',		2).
valid_abstract_section_header('MATERIALS ET METHODS',		2).
valid_abstract_section_header('MATERIALS, METHODS AND PROCEDURES',		2).
valid_abstract_section_header('MATERIALS, SUBJECTS, AND METHODS',		2).
valid_abstract_section_header('MATERIALSAND METHODS',		2).
valid_abstract_section_header('MATERIA?',		2).
valid_abstract_section_header('MATHEMATICS SUBJECT CLASSIFICATION',		2).
valid_abstract_section_header('MATHERIAL AND METHOD',		2).
valid_abstract_section_header('MEASUREMENT & OUTCOMES',		2).
valid_abstract_section_header('MEASUREMENTS AND MAIN METHODS',		2).
valid_abstract_section_header('MEASURES/ANALYSIS',		2).
valid_abstract_section_header('MEASURING INSTRUMENTS',		2).
valid_abstract_section_header('MECHANISM',		2).
valid_abstract_section_header('MEDICAL EDUCATION',		2).
valid_abstract_section_header('MEDICATIONS',		2).
valid_abstract_section_header('MEMBERSHIP',		2).
valid_abstract_section_header('MENSAJES CLAVE',		2).
valid_abstract_section_header('MESSAGE PRINCIPAL',		2).
valid_abstract_section_header('METHOD AND AIMS',		2).
valid_abstract_section_header('METHOD AND CONCLUSION',		2).
valid_abstract_section_header('METHOD AND CONCLUSIONS',		2).
valid_abstract_section_header('METHOD AND PRINCIPAL FINDINGS',		2).
valid_abstract_section_header('METHOD AND PURPOSE',		2).
valid_abstract_section_header('METHOD SAND RESULTS',		2).
valid_abstract_section_header('METHOD, RESULTS AND CONCLUSIONS',		2).
valid_abstract_section_header('METHOD/CASE',		2).
valid_abstract_section_header('METHOD/CASE REPORT',		2).
valid_abstract_section_header('METHOD/FINDINGS',		2).
valid_abstract_section_header('METHOD/PRINCIPLE FINDINGS',		2).
valid_abstract_section_header('METHODODOLOGY/PRINCIPAL FINDINGS',		2).
valid_abstract_section_header('METHODOLOGICAL/PRINCIPAL FINDINGS',		2).
valid_abstract_section_header('METHODOLOGY /PRINCIPAL FINDINGS',		2).
valid_abstract_section_header('METHODOLOGY AND DATA',		2).
valid_abstract_section_header('METHODOLOGY AND PRINCIPAL RESULTS',		2).
valid_abstract_section_header('METHODOLOGY/CLINICAL FINDINGS',		2).
valid_abstract_section_header('METHODOLOGY/PRINCIPLE FINDING',		2).
valid_abstract_section_header('METHODOLOGY/SIGNIFICANCE',		2).
valid_abstract_section_header('METHODS & PATIENTS',		2).
valid_abstract_section_header('METHODS & RESEARCH DESIGN',		2).
valid_abstract_section_header('METHODS (CASE REPORT)',		2).
valid_abstract_section_header('METHODS (DESIGN, SETTING, PARTICIPANTS)',		2).
valid_abstract_section_header('METHODS (STUDY DESIGN)',		2).
valid_abstract_section_header('METHODS AND AIM OF THE STUDY',		2).
valid_abstract_section_header('METHODS AND DATA SOURCES',		2).
valid_abstract_section_header('METHODS AND DATABASES',		2).
valid_abstract_section_header('METHODS AND DESIGNS',		2).
valid_abstract_section_header('METHODS AND EVALUATION OF THE HYPOTHESIS',		2).
valid_abstract_section_header('METHODS AND PRINCIPLE FINDINGS',		2).
valid_abstract_section_header('METHODS AND PURPOSE',		2).
valid_abstract_section_header('METHODS AND RESEARCH',		2).
valid_abstract_section_header('METHODS AND STUDY SETTING',		2).
valid_abstract_section_header('METHODS AND TECHNIQUES',		2).
valid_abstract_section_header('METHODS AND THE RESULTS',		2).
valid_abstract_section_header('METHODS AND TOPICS',		2).
valid_abstract_section_header('METHODS AND TRIAL DESIGN',		2).
valid_abstract_section_header('METHODS SUBJECTS',		2).
valid_abstract_section_header('METHODS USED FOR LOCATING, SELECTING, EXTRACTING AND SYNTHESIZING DATA',		2).
valid_abstract_section_header('METHODS, FINDINGS',		2).
valid_abstract_section_header('METHODS, RESULTS AND CONCLUSION',		2).
valid_abstract_section_header('METHODS, SETTING AND PARTICIPANTS',		2).
valid_abstract_section_header('METHODS-MATERIAL',		2).
valid_abstract_section_header('METHODS/ MATERIALS',		2).
valid_abstract_section_header('METHODS/APPROACH',		2).
valid_abstract_section_header('METHODS/DATA SOURCES',		2).
valid_abstract_section_header('METHODS/DISCUSSION',		2).
valid_abstract_section_header('METHODS/MAIN OUTCOME MEASURES',		2).
valid_abstract_section_header('METHODS/MAIN RESULTS',		2).
valid_abstract_section_header('METHODS/MATERIAL',		2).
valid_abstract_section_header('METHODS/OUTCOMES',		2).
valid_abstract_section_header('METHODS/PARTICIPANTS',		2).
valid_abstract_section_header('METHODS/PATIENT',		2).
valid_abstract_section_header('METHODS/PATIENTS/RESULTS',		2).
valid_abstract_section_header('METHODS/PROCEDURES',		2).
valid_abstract_section_header('METHODS/SEARCH STRATEGIES',		2).
valid_abstract_section_header('METHODS/TRIAL DESIGN',		2).
valid_abstract_section_header('METODE IN REZULTATI',		2).
valid_abstract_section_header('METODS',		2).
valid_abstract_section_header('METODY',		2).
valid_abstract_section_header('MICROSCOPY',		2).
valid_abstract_section_header('MINI SUMMARY',		2).
valid_abstract_section_header('MINING AREAS',		2).
valid_abstract_section_header('MIP (OR',		2).
valid_abstract_section_header('MODEL, PERSPECTIVE, & TIME FRAME',		2).
valid_abstract_section_header('MOLECULAR GENETIC STUDIES',		2).
valid_abstract_section_header('MONITORING',		2).
valid_abstract_section_header('MORBIDITY AND MORTALITY',		2).
valid_abstract_section_header('MORPHOLOGY',		2).
valid_abstract_section_header('MORTALITY AND MORBIDITY',		2).
valid_abstract_section_header('MOTHODS',		2).
valid_abstract_section_header('MOTIVATIONS AND RESULTS',		2).
valid_abstract_section_header('MULTIDISCIPLINARY CARE',		2).
valid_abstract_section_header('MWTHODS',		2).
valid_abstract_section_header('MYOCARDIAL STUNNING',		2).
valid_abstract_section_header('Main Outcome and Measure',		2).
valid_abstract_section_header('Methods and Materials',		2).
valid_abstract_section_header('Metodos',		2).
valid_abstract_section_header('NAME OF THE TRIAL REGISTRY',		2).
valid_abstract_section_header('NARRATIVE',		2).
valid_abstract_section_header('NEEDS',		2).
valid_abstract_section_header('NEUROIMAGING',		2).
valid_abstract_section_header('NEW AND NOTEWORTHY',		2).
valid_abstract_section_header('NEW DEVICES',		2).
valid_abstract_section_header('NEW METHOD AND RESULTS',		2).
valid_abstract_section_header('NEW THERAPEUTIC APPROACHES',		2).
valid_abstract_section_header('NEWS AND KEY POINTS',		2).
valid_abstract_section_header('NIVEAU DE PREUVE',		2).
valid_abstract_section_header('NOTE TO PRACTITIONERS',		2).
valid_abstract_section_header('NOTES',		2).
valid_abstract_section_header('NOVEL EXPERIMENTS',		2).
valid_abstract_section_header('NOVELTY',		2).
valid_abstract_section_header('NT/ND',		2).
valid_abstract_section_header('NUMBER OF SUBJECTS',		2).
valid_abstract_section_header('NUMBERS ANALYSED',		2).
valid_abstract_section_header('NURSING APPLICATION',		2).
valid_abstract_section_header('NUTRITION',		2).
valid_abstract_section_header('NUTRITIONAL INTERVENTION',		2).
valid_abstract_section_header('NUTRITIONAL SCREENING',		2).
valid_abstract_section_header('OBJCETIVE',		2).
valid_abstract_section_header('OBJECT AND DESIGN',		2).
valid_abstract_section_header('OBJECT OF RESEARCH',		2).
valid_abstract_section_header('OBJECTIU',		2).
valid_abstract_section_header('OBJECTIVE & METHOD',		2).
valid_abstract_section_header('OBJECTIVE & METHODOLOGY',		2).
valid_abstract_section_header('OBJECTIVE AND DISCUSSION',		2).
valid_abstract_section_header('OBJECTIVE AND INTERVENTION',		2).
valid_abstract_section_header('OBJECTIVE AND RESEARCH DESIGN',		2).
valid_abstract_section_header('OBJECTIVE DATA',		2).
valid_abstract_section_header('OBJECTIVE DESIGN',		2).
valid_abstract_section_header('OBJECTIVE OF THE RESEARCH',		2).
valid_abstract_section_header('OBJECTIVE/DISCUSSION',		2).
valid_abstract_section_header('OBJECTIVES AND HYPOTHESES',		2).
valid_abstract_section_header('OBJECTIVES AND STUDY',		2).
valid_abstract_section_header('OBJECTIVES OF REVIEW',		2).
valid_abstract_section_header('OBJECTIVES OF THE PROGRAM',		2).
valid_abstract_section_header('OBJECTIVES OF THE SURVEY',		2).
valid_abstract_section_header('OBJECTIVES/METHOD',		2).
valid_abstract_section_header('OBJECTIVES/OUTCOMES',		2).
valid_abstract_section_header('OBJETIVOS PRINCIPALES DEL ESTUDIO',		2).
valid_abstract_section_header('OBSERVATION AND CONCLUSION',		2).
valid_abstract_section_header('OBSERVATION/INTERVENTION',		2).
valid_abstract_section_header('OBSERVATIONS AND CONCLUSIONS',		2).
valid_abstract_section_header('OBSERVATIONS AND METHODS',		2).
valid_abstract_section_header('OCCURRENCE',		2).
valid_abstract_section_header('OF KEY POINTS',		2).
valid_abstract_section_header('ONCLUSIONS',		2).
valid_abstract_section_header('OPERATION AND RESULTS',		2).
valid_abstract_section_header('OPERATIVE FINDINGS',		2).
valid_abstract_section_header('OPERATIVE MANAGEMENT',		2).
valid_abstract_section_header('OPTIMAL CARE',		2).
valid_abstract_section_header('ORAL ANTICOAGULATION',		2).
valid_abstract_section_header('ORIGINAL POSITIONS',		2).
valid_abstract_section_header('OSSERVAZIONE PERSONALE',		2).
valid_abstract_section_header('OTHER',		2).
valid_abstract_section_header('OUR CASE',		2).
valid_abstract_section_header('OUR EXPERIENCE',		2).
valid_abstract_section_header('OUTCOME & MEASURES',		2).
valid_abstract_section_header('OUTCOME AND MEASURES',		2).
valid_abstract_section_header('OUTCOME OBJECTIVES',		2).
valid_abstract_section_header('OUTCOME/CONCLUSION',		2).
valid_abstract_section_header('OUTCOMES & OTHER MEASUREMENTS',		2).
valid_abstract_section_header('OUTCOMES AND CONCLUSION',		2).
valid_abstract_section_header('OUTCOMES AND DISCUSSION',		2).
valid_abstract_section_header('OUTCOMES AND METHODS',		2).
valid_abstract_section_header('OUTLINE OF THE MODEL',		2).
valid_abstract_section_header('OVERVIEW/LITERATURE REVIEWED',		2).
valid_abstract_section_header('OWN EXPERIENCE',		2).
valid_abstract_section_header('OWN STUDIES',		2).
valid_abstract_section_header('Outcome Measures',		2).
valid_abstract_section_header('PACJENCI I METODA',		2).
valid_abstract_section_header('PACS CODE',		2).
valid_abstract_section_header('PACS CODES',		2).
valid_abstract_section_header('PAIENTS AND METHODS',		2).
valid_abstract_section_header('PALIVIZUMAB OUTCOMES REGISTRY',		2).
valid_abstract_section_header('PAPER AIM',		2).
valid_abstract_section_header('PARADOX',		2).
valid_abstract_section_header('PARTECIPANTS',		2).
valid_abstract_section_header('PARTICIPANT SAMPLE',		2).
valid_abstract_section_header('PARTICIPANT, MATERIALS, SETTING, METHODS',		2).
valid_abstract_section_header('PARTICIPANTS & METHODS',		2).
valid_abstract_section_header('PARTICIPANTS & SETTINGS',		2).
valid_abstract_section_header('PARTICIPANTS AND DATA COLLECTION',		2).
valid_abstract_section_header('PARTICIPANTS AND EXPOSURES',		2).
valid_abstract_section_header('PARTICIPANTS AND INCLUDED STUDIES',		2).
valid_abstract_section_header('PARTICIPANTS AND PROCEDURES',		2).
valid_abstract_section_header('PARTICIPANTS, MATERIALS AND METHODS',		2).
valid_abstract_section_header('PARTICIPANTS, MATERIALS, SETTING, METHODS',		2).
valid_abstract_section_header('PARTICIPANTS, MATERIALS/METHODS',		2).
valid_abstract_section_header('PARTICIPANTS, METHODS',		2).
valid_abstract_section_header('PARTICIPANTS, SETTINGS AND METHODS',		2).
valid_abstract_section_header('PARTICIPANTS, STUDY DESIGN, AND METHODS',		2).
valid_abstract_section_header('PARTICIPANTS/EXPOSURES',		2).
valid_abstract_section_header('PATHOGENETIC MECHANISMS',		2).
valid_abstract_section_header('PATHOPHYSIOLOGICAL HYPOTHESES',		2).
valid_abstract_section_header('PATIENT (S)',		2).
valid_abstract_section_header('PATIENT 1',		2).
valid_abstract_section_header('PATIENT AND CASE REPORT',		2).
valid_abstract_section_header('PATIENT AND MATERIAL',		2).
valid_abstract_section_header('PATIENT DESCRIPTION AND RESULTS',		2).
valid_abstract_section_header('PATIENT FINDINGS AND SUMMARY',		2).
valid_abstract_section_header('PATIENT FOLLOW-UP',		2).
valid_abstract_section_header('PATIENT GROUPS',		2).
valid_abstract_section_header('PATIENT INFORMATION',		2).
valid_abstract_section_header('PATIENT OUTCOMES',		2).
valid_abstract_section_header('PATIENT SAMPLE AND OUTCOME MEASURES',		2).
valid_abstract_section_header('PATIENT, METHOD AND RESULTS',		2).
valid_abstract_section_header('PATIENTEN UND METHODE',		2).
valid_abstract_section_header('PATIENTS AND METHODS/RESULTS',		2).
valid_abstract_section_header('PATIENTS AND METHODS:',		2).
valid_abstract_section_header('PATIENTS AT RISK',		2).
valid_abstract_section_header('PATIENTS OR SUBJECTS',		2).
valid_abstract_section_header('PATIENTS, MATERIALS AND METHOD',		2).
valid_abstract_section_header('PATIENTS, MEASUREMENTS AND RESULTS',		2).
valid_abstract_section_header('PATIENTS/METHODS/RESULTS',		2).
valid_abstract_section_header('PATTERNS OF RESISTANCE',		2).
valid_abstract_section_header('PEARLS',		2).
valid_abstract_section_header('PERCEPTION',		2).
valid_abstract_section_header('PERCUTANEOUS TRANSLUMINAL CORONARY ANGIOPLASTY',		2).
valid_abstract_section_header('PERSPECTIVE AND CONCLUSION',		2).
valid_abstract_section_header('PERSPECTIVE AND CONCLUSIONS',		2).
valid_abstract_section_header('PERSPECTIVE AND PROJECTS',		2).
valid_abstract_section_header('PERSPECTIVES AND FUTURE PROJECTS',		2).
valid_abstract_section_header('PERSPECTIVES/CONCLUSION',		2).
valid_abstract_section_header('PERSPECTIVES/CONCLUSIONS',		2).
valid_abstract_section_header('PGI-I',		2).
valid_abstract_section_header('PHYTOCHEMISTRY',		2).
valid_abstract_section_header('PLACE OF STUDY',		2).
valid_abstract_section_header('PLACEMENT',		2).
valid_abstract_section_header('PLAN OF ACTION',		2).
valid_abstract_section_header('PLAQUE FORMATION',		2).
valid_abstract_section_header('PLATELET INHIBITION',		2).
valid_abstract_section_header('POINTS DISCUSSED',		2).
valid_abstract_section_header('POLICY PRACTICE',		2).
valid_abstract_section_header('POLICY/PRACTICE',		2).
valid_abstract_section_header('POPULATION AND SAMPLE SIZE',		2).
valid_abstract_section_header('POSSIBLE ROLES',		2).
valid_abstract_section_header('POSTIMPLEMENTATION RESULTS',		2).
valid_abstract_section_header('POSTOPERATIVE COURSE',		2).
valid_abstract_section_header('POSTOPERATIVE REGIMEN',		2).
valid_abstract_section_header('POSTSTROKE DEPRESSION',		2).
valid_abstract_section_header('POSTTREATMENT RECORDS',		2).
valid_abstract_section_header('POTENTIAL',		2).
valid_abstract_section_header('POTENTIAL SOLUTIONS',		2).
valid_abstract_section_header('PP-DES METHODS',		2).
valid_abstract_section_header('PP/PS',		2).
valid_abstract_section_header('PRACTICAL CONSEQUENCES',		2).
valid_abstract_section_header('PRACTICAL IMPLICATIONS AND CONCLUSIONS',		2).
valid_abstract_section_header('PRACTICAL MANAGEMENT',		2).
valid_abstract_section_header('PRACTICE AND IMPLICATIONS',		2).
valid_abstract_section_header('PRECAUTIONS',		2).
valid_abstract_section_header('PRECLINICAL PHARMACOLOGY',		2).
valid_abstract_section_header('PREDICTOR, OUTCOMES, & MEASUREMENTS',		2).
valid_abstract_section_header('PRELIMINARY EXPERIENCE',		2).
valid_abstract_section_header('PREOPERATIVE ASSESSMENT',		2).
valid_abstract_section_header('PREOPERATIVE EVALUATION',		2).
valid_abstract_section_header('PRESENT AND FUTURE',		2).
valid_abstract_section_header('PRESENT KNOWLEDGE',		2).
valid_abstract_section_header('PRESENTATION OF CASE REPORT',		2).
valid_abstract_section_header('PRESENTATION/INTERVENTION',		2).
valid_abstract_section_header('PREVENTION AND THERAPY',		2).
valid_abstract_section_header('PREVIOUS CLASSIFICATIONS OF DYSPHONIAS',		2).
valid_abstract_section_header('PREVIOUS STUDIES',		2).
valid_abstract_section_header('PRIMARY AND SECONDARY MEASURES',		2).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME MEASUREMENTS',		2).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME MEASURES AND ANALYSIS',		2).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOMES MEASURED',		2).
valid_abstract_section_header('PRIMARY DATA SOURCE',		2).
valid_abstract_section_header('PRIMARY DATA SOURCES',		2).
valid_abstract_section_header('PRIMARY DIAGNOSIS',		2).
valid_abstract_section_header('PRIMARY END-POINT',		2).
valid_abstract_section_header('PRIMARY HYPOTHESIS',		2).
valid_abstract_section_header('PRIMARY OUTCOME AND SECONDARY OUTCOME MEASURES',		2).
valid_abstract_section_header('PRIMARY OUTCOME MEASUREMENTS',		2).
valid_abstract_section_header('PRIMARY PRACTICE SETTINGS(S)',		2).
valid_abstract_section_header('PRIMARY RECOMMENDATION',		2).
valid_abstract_section_header('PRINCIPAL FINDINGS AND SIGNIFICANCE',		2).
valid_abstract_section_header('PRINCIPAL FINDINGS/SIGNIFICANCE',		2).
valid_abstract_section_header('PRINCIPAL INVESTIGATOR AND PROMOTER',		2).
valid_abstract_section_header('PRINCIPAL POINTS',		2).
valid_abstract_section_header('PRINCIPLE CONCLUSION',		2).
valid_abstract_section_header('PRINCIPLE FINDING',		2).
valid_abstract_section_header('PRINCIPLES AND METHODS',		2).
valid_abstract_section_header('PROBABILITY FUNCTION FOR DIAGNOSES',		2).
valid_abstract_section_header('PROBLEM ANALYSIS',		2).
valid_abstract_section_header('PROBLEM DESCRIPTION',		2).
valid_abstract_section_header('PROBLEM OR BACKGROUND',		2).
valid_abstract_section_header('PROCEDURES/OUTCOMES',		2).
valid_abstract_section_header('PROFESSIONAL PRESENTATION',		2).
valid_abstract_section_header('PROGNOSIS AND TREATMENT',		2).
valid_abstract_section_header('PROGRAM IMPLEMENTATION',		2).
valid_abstract_section_header('PROGRAM OVERVIEW',		2).
valid_abstract_section_header('PROGRAM PLAN AND IMPLEMENTATION',		2).
valid_abstract_section_header('PROGRAM SUMMARY',		2).
valid_abstract_section_header('PROGRAMS',		2).
valid_abstract_section_header('PROGRESSION',		2).
valid_abstract_section_header('PROJECT SUMMARY',		2).
valid_abstract_section_header('PROMISING FUTURE IMPLICATIONS',		2).
valid_abstract_section_header('PROPOSAL AND CONCLUSION',		2).
valid_abstract_section_header('PROPOSED CHANGES',		2).
valid_abstract_section_header('PROPOSED METHODS',		2).
valid_abstract_section_header('PROPOSED PROGRAM',		2).
valid_abstract_section_header('PROPOSED RECOMMENDATIONS',		2).
valid_abstract_section_header('PROPOSED SOLUTION',		2).
valid_abstract_section_header('PROPOSED SOLUTIONS',		2).
valid_abstract_section_header('PROPOSITION',		2).
valid_abstract_section_header('PROPOSITIONS',		2).
valid_abstract_section_header('PROSPERO PROTOCOL REGISTRATION NUMBER',		2).
valid_abstract_section_header('PROTOCOL NUMBER OF BRAZILIAN REGISTRY OF CLINICAL TRIALS',		2).
valid_abstract_section_header('PROTOCOL REGISTRATION IN PROSPERO',		2).
valid_abstract_section_header('PROTOCOL VERSION',		2).
valid_abstract_section_header('PURPOSE AND SOURCE',		2).
valid_abstract_section_header('PURPOSE AND SOURCES',		2).
valid_abstract_section_header('PURPOSE AND SOURCES OF INFORMATION',		2).
valid_abstract_section_header('PURPOSE OF PROGRAM',		2).
valid_abstract_section_header('PURPOSE OF REVEIW',		2).
valid_abstract_section_header('PURPOSES OF REVIEW',		2).
valid_abstract_section_header('PURPOUSE',		2).
valid_abstract_section_header('PURPUSE',		2).
valid_abstract_section_header('QUALITY OF CARE INTERVENTIONS',		2).
valid_abstract_section_header('QUANTITATIVE FINDINGS',		2).
valid_abstract_section_header('QUESTIONING',		2).
valid_abstract_section_header('RADIOLOGIC DIAGNOSIS',		2).
valid_abstract_section_header('RADIOLOGICAL ASSESSMENT',		2).
valid_abstract_section_header('RADIOPHARMACEUTICALS',		2).
valid_abstract_section_header('RATIONALE & AIM',		2).
valid_abstract_section_header('RATIONALE & OBJECTIVES',		2).
valid_abstract_section_header('RATIONALE OBJECTIVE',		2).
valid_abstract_section_header('RATIONALE OF THE STUDY',		2).
valid_abstract_section_header('RATIONALE, AIM AND OBJECTIVES',		2).
valid_abstract_section_header('RAZPRAVA',		2).
valid_abstract_section_header('REASON FOR STUDY',		2).
valid_abstract_section_header('REASONS FOR PERFORMING THIS STUDY',		2).
valid_abstract_section_header('REASONS FOR STUDY',		2).
valid_abstract_section_header('REBEC',		2).
valid_abstract_section_header('RECENT ADVANCES/CRITICAL ISSUES',		2).
valid_abstract_section_header('RECENT FINDINGS AND SUMMARY',		2).
valid_abstract_section_header('RECENT RESEARCH',		2).
valid_abstract_section_header('RECENT TRENDS',		2).
valid_abstract_section_header('RECOMMENDATIONS  LEVEL II',		2).
valid_abstract_section_header('REFERENCE AND METHOD',		2).
valid_abstract_section_header('REGARDING DIAGNOSIS',		2).
valid_abstract_section_header('REGISTERED CLINICAL TRIAL',		2).
valid_abstract_section_header('REGISTRY NUMBER',		2).
valid_abstract_section_header('REGULATION',		2).
valid_abstract_section_header('RELATION TO CLINICAL PRACTICE',		2).
valid_abstract_section_header('RELEVANCE TO CLINICAL CARE',		2).
valid_abstract_section_header('RELEVANCE TO CLINICAL NURSING',		2).
valid_abstract_section_header('RELEVANT TO CLINICAL PRACTICE',		2).
valid_abstract_section_header('REMARK',		2).
valid_abstract_section_header('RES ULTS',		2).
valid_abstract_section_header('RESEARCH AIMS AND DESIGN',		2).
valid_abstract_section_header('RESEARCH AND PRACTICE IMPLICATIONS',		2).
valid_abstract_section_header('RESEARCH CONTEXT',		2).
valid_abstract_section_header('RESEARCH DESIGN AND METHODOLOGY',		2).
valid_abstract_section_header('RESEARCH DESIGN/METHODS AND RESULTS',		2).
valid_abstract_section_header('RESEARCH DESIGN/PARTICIPANTS/CONTEXT',		2).
valid_abstract_section_header('RESEARCH DESIGN/SUBJECTS',		2).
valid_abstract_section_header('RESEARCH FOCUS',		2).
valid_abstract_section_header('RESEARCH METHOD USED',		2).
valid_abstract_section_header('RESEARCH PROCESS',		2).
valid_abstract_section_header('RESEARCH QUESTION AND DESIGN',		2).
valid_abstract_section_header('RESEARCH QUESTION/OBJECTIVES/HYPOTHESIS',		2).
valid_abstract_section_header('RESEARCH RESULTS',		2).
valid_abstract_section_header('RESOURCES AND MATERIALS',		2).
valid_abstract_section_header('RESPONSES',		2).
valid_abstract_section_header('RESULT (S)',		2).
valid_abstract_section_header('RESULT AND ANALYSIS',		2).
valid_abstract_section_header('RESULT AND OBSERVATION',		2).
valid_abstract_section_header('RESULT(S) AND CONCLUSION(S)',		2).
valid_abstract_section_header('RESULTS & INTERPRETATION',		2).
valid_abstract_section_header('RESULTS & OUTCOMES',		2).
valid_abstract_section_header('RESULTS A',		2).
valid_abstract_section_header('RESULTS AND CLINICAL COURSE',		2).
valid_abstract_section_header('RESULTS AND COMPARISON WITH EXISTING METHOD(S)',		2).
valid_abstract_section_header('RESULTS AND CONLUSION',		2).
valid_abstract_section_header('RESULTS AND DICUSSION',		2).
valid_abstract_section_header('RESULTS AND DISCUSION',		2).
valid_abstract_section_header('RESULTS AND EVALUATION',		2).
valid_abstract_section_header('RESULTS AND IMPACT ON INDUSTRY AND GOVERNMENT',		2).
valid_abstract_section_header('RESULTS AND SIGNIFICANCE OF THE RESEARCH',		2).
valid_abstract_section_header('RESULTS AND STATE OF THE ART',		2).
valid_abstract_section_header('RESULTS AND SYNTHESIS METHODS',		2).
valid_abstract_section_header('RESULTS AND ?ONCLUSION',		2).
valid_abstract_section_header('RESULTS CONCLUSION',		2).
valid_abstract_section_header('RESULTS OF ASSESSMENT',		2).
valid_abstract_section_header('RESULTS OF CLINICAL STUDIES',		2).
valid_abstract_section_header('RESULTS OF DATA REVIEW',		2).
valid_abstract_section_header('RESULTS OF LITERATURE REVIEW',		2).
valid_abstract_section_header('RESULTS RESULT',		2).
valid_abstract_section_header('RESULTS SHOWED',		2).
valid_abstract_section_header('RESULTS, AND CONCLUSIONS',		2).
valid_abstract_section_header('RESULTS/COMPLICATIONS',		2).
valid_abstract_section_header('RESULTS/EVALUATION',		2).
valid_abstract_section_header('RESULTS/IMPLICATIONS',		2).
valid_abstract_section_header('RESULTS/LESSONS LEARNED',		2).
valid_abstract_section_header('RESULTS/LITERATURE REVIEW',		2).
valid_abstract_section_header('RESULTS/METHODS',		2).
valid_abstract_section_header('RESULTS/PRINCIPAL FINDINGS',		2).
valid_abstract_section_header('RESUMO',		2).
valid_abstract_section_header('RETROSPECTIVE STUDY',		2).
valid_abstract_section_header('REVIEW METHODS AND DATA SOURCES',		2).
valid_abstract_section_header('REVIEW OBJECTIVE AND QUESTIONS',		2).
valid_abstract_section_header('RICKETTSIA TYPHI',		2).
valid_abstract_section_header('RISK EVALUATION',		2).
valid_abstract_section_header('RISK OF TOXICITY',		2).
valid_abstract_section_header('ROLES AND RESPONSIBILITIES',		2).
valid_abstract_section_header('ROOT CAUSE ANALYSIS',		2).
valid_abstract_section_header('ROUNDTABLE ASSEMBLY',		2).
valid_abstract_section_header('RUN-PLA',		2).
valid_abstract_section_header('Recommendation',		2).
valid_abstract_section_header('Resultado:',		2).
valid_abstract_section_header('Results',		2).
valid_abstract_section_header('Results of Base-Case Analysis',		2).
valid_abstract_section_header('Results of Sensitivity Analysis',		2).
valid_abstract_section_header('SAHARA-J',		2).
valid_abstract_section_header('SALVAGE THERAPY',		2).
valid_abstract_section_header('SAMPLE SIZE ESTIMATE',		2).
valid_abstract_section_header('SAMPLING TECHNIQUE',		2).
valid_abstract_section_header('SCOPE AND AIMS',		2).
valid_abstract_section_header('SCOPE AND BACKGROUND',		2).
valid_abstract_section_header('SCOPE AND FINDINGS',		2).
valid_abstract_section_header('SCREENING SCHEDULE',		2).
valid_abstract_section_header('SCREENING TESTS',		2).
valid_abstract_section_header('SEARCH',		2).
valid_abstract_section_header('SEARCH SOURCES',		2).
valid_abstract_section_header('SECONDARY CRITERIA',		2).
valid_abstract_section_header('SECONDARY HYPERPARATHYROIDISM',		2).
valid_abstract_section_header('SECONDARY PROPHYLAXIS',		2).
valid_abstract_section_header('SEE RESEARCH ARTICLE',		2).
valid_abstract_section_header('SEIZURES',		2).
valid_abstract_section_header('SESSION TITLE',		2).
valid_abstract_section_header('SETTING AND CONTEXT',		2).
valid_abstract_section_header('SETTING AND DATA SOURCES',		2).
valid_abstract_section_header('SETTING AND PARTICIPANT',		2).
valid_abstract_section_header('SETTING E PARTECIPANTI',		2).
valid_abstract_section_header('SETTING ICU PATIENTS',		2).
valid_abstract_section_header('SETTING OF THE STUDY',		2).
valid_abstract_section_header('SETTING THRESHOLDS',		2).
valid_abstract_section_header('SETTING UK POPULATION',		2).
valid_abstract_section_header('SETTING, PARTICIPANTS AND METHOD',		2).
valid_abstract_section_header('SETTING, PATIENTS AND INTERVENTIONS',		2).
valid_abstract_section_header('SETTING, PATIENTS, AND INTERVENTIONS',		2).
valid_abstract_section_header('SETTING, PATIENTS, AND MEASUREMENTS',		2).
valid_abstract_section_header('SETTING, SUBJECTS, AND METHODS',		2).
valid_abstract_section_header('SETTINGS/LOCATION, SUBJECTS, AND INTERVENTIONS',		2).
valid_abstract_section_header('SEVERAL METHODS',		2).
valid_abstract_section_header('SHORT INTRODUCTION',		2).
valid_abstract_section_header('SIGNIFICANCE OF THE CONCLUSIONS',		2).
valid_abstract_section_header('SIMULATIONS',		2).
valid_abstract_section_header('SITUATION ANALYSIS',		2).
valid_abstract_section_header('SMOKING',		2).
valid_abstract_section_header('SOURCES OF DATA, AREAS OF AGREEMENT AND CONTROVERSY',		2).
valid_abstract_section_header('SPINE',		2).
valid_abstract_section_header('STANDARD',		2).
valid_abstract_section_header('STASTICAL ANALYSIS',		2).
valid_abstract_section_header('STATE OF THE ART/PERSPECTIVES',		2).
valid_abstract_section_header('STATE OF THE PROBLEM',		2).
valid_abstract_section_header('STATE-OF-THE-ART',		2).
valid_abstract_section_header('STATEMENT OF CONCLUSION',		2).
valid_abstract_section_header('STATINS',		2).
valid_abstract_section_header('STATISTICAL',		2).
valid_abstract_section_header('STATISTICAL APPROACH',		2).
valid_abstract_section_header('STILL SUFFICIENT',		2).
valid_abstract_section_header('STRENGTH OF RECOMMENDATION TAXONOMY',		2).
valid_abstract_section_header('STRESS ECHOCARDIOGRAPHY WITH CONTRAST',		2).
valid_abstract_section_header('STRONG POINT',		2).
valid_abstract_section_header('STUDIED PARAMETERS',		2).
valid_abstract_section_header('STUDIED POPULATION',		2).
valid_abstract_section_header('STUDY AND METHODS',		2).
valid_abstract_section_header('STUDY APPRAISAL/SYNTHESIS METHODS',		2).
valid_abstract_section_header('STUDY B',		2).
valid_abstract_section_header('STUDY DESIGN AND MEASURES',		2).
valid_abstract_section_header('STUDY DESIGN PATIENTS AND MEASUREMENTS',		2).
valid_abstract_section_header('STUDY DESIGN, MEASURES AND OUTCOMES',		2).
valid_abstract_section_header('STUDY DESIGN, METHODS',		2).
valid_abstract_section_header('STUDY DESIGN, SETTING AND PARTICIPANTS',		2).
valid_abstract_section_header('STUDY DESIGN, SETTING, & PARTICIPANTS',		2).
valid_abstract_section_header('STUDY DESIGN, SIZE',		2).
valid_abstract_section_header('STUDY DESIGN, SUBJECTS AND OUTCOME MEASURES',		2).
valid_abstract_section_header('STUDY DESIGN/METHODS AND MATERIALS',		2).
valid_abstract_section_header('STUDY DESIGN/METHODS/DATA',		2).
valid_abstract_section_header('STUDY DESIGN/PATIENT AND METHOD',		2).
valid_abstract_section_header('STUDY DESIGN/SAMPLE',		2).
valid_abstract_section_header('STUDY DESIGN:',		2).
valid_abstract_section_header('STUDY DESING',		2).
valid_abstract_section_header('STUDY DURATION',		2).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA PARTICIPANTS AND INTERVENTIONS',		2).
valid_abstract_section_header('STUDY FINDING/COMPETING INTERESTS',		2).
valid_abstract_section_header('STUDY FUNDING COMPETING INTERESTS',		2).
valid_abstract_section_header('STUDY FUNDING/ COMPETING INTEREST(S)',		2).
valid_abstract_section_header('STUDY FUNDING/COMPETING INTEREST (S)',		2).
valid_abstract_section_header('STUDY IDENTIFIER',		2).
valid_abstract_section_header('STUDY II',		2).
valid_abstract_section_header('STUDY IMPLICATIONS',		2).
valid_abstract_section_header('STUDY INTERVENTION',		2).
valid_abstract_section_header('STUDY NUMBER',		2).
valid_abstract_section_header('STUDY OUTCOME(S)',		2).
valid_abstract_section_header('STUDY POPULATION AND RESULTS',		2).
valid_abstract_section_header('STUDY PROCEDURES',		2).
valid_abstract_section_header('STUDY SELECTION AND INTERVENTIONS',		2).
valid_abstract_section_header('STUDY SELECTION, DATA EXTRACTION AND SYNTHESIS',		2).
valid_abstract_section_header('STUDY SELECTION/ELIGIBILITY',		2).
valid_abstract_section_header('STUDY SELECTION/ELIGIBILITY CRITERIA',		2).
valid_abstract_section_header('STUDY SETTING AND PARTICIPANTS',		2).
valid_abstract_section_header('STUDY SOURCES',		2).
valid_abstract_section_header('STUDY STRATEGY',		2).
valid_abstract_section_header('STUDY STRUCTURE',		2).
valid_abstract_section_header('STUDY SYNTHESIS AND APPRAISAL',		2).
valid_abstract_section_header('STUDY, DESIGN, SIZE, DURATION',		2).
valid_abstract_section_header('SUBJECT & METHODS',		2).
valid_abstract_section_header('SUBJECT AND TREATMENT',		2).
valid_abstract_section_header('SUBJECT/DESIGN',		2).
valid_abstract_section_header('SUBJECTS AND RESEARCH METHODS',		2).
valid_abstract_section_header('SUBJECTS, DESIGN AND MEASUREMENT',		2).
valid_abstract_section_header('SUBJECTS, STUDY DESIGN, OUTCOME MEASURES',		2).
valid_abstract_section_header('SUBJECTS, TREATMENT AND METHODS',		2).
valid_abstract_section_header('SUBJECTS/INTERVENTION',		2).
valid_abstract_section_header('SUBJECTS/MATERIALS AND METHODS',		2).
valid_abstract_section_header('SUBSEQUENT INVESTIGATION',		2).
valid_abstract_section_header('SUCCESS FACTORS',		2).
valid_abstract_section_header('SUGGESTED SOLUTION',		2).
valid_abstract_section_header('SUGGESTION',		2).
valid_abstract_section_header('SUMARY',		2).
valid_abstract_section_header('SUMMARY FOR TABLE OF CONTENTS',		2).
valid_abstract_section_header('SUMMARY OF BACKGROUND DATA AND OBJECTIVES',		2).
valid_abstract_section_header('SUMMARY OF CONTENT',		2).
valid_abstract_section_header('SUMMARY OF FINDINGS AND CONCLUSIONS',		2).
valid_abstract_section_header('SUMMARY OF ISSUES',		2).
valid_abstract_section_header('SUMMARY OF REPORTS',		2).
valid_abstract_section_header('SUMMARY SENTENCE',		2).
valid_abstract_section_header('SUMMAY',		2).
valid_abstract_section_header('SUMMERY',		2).
valid_abstract_section_header('SUPPLEMENT',		2).
valid_abstract_section_header('SUPPORTING DATA',		2).
valid_abstract_section_header('SUPPORTING EVIDENCE',		2).
valid_abstract_section_header('SURVEY METHODS',		2).
valid_abstract_section_header('SURVEY PARTICIPANTS',		2).
valid_abstract_section_header('SURVIVAL AMONG ADULT RECIPIENTS',		2).
valid_abstract_section_header('SURVIVAL AMONG PEDIATRIC RECIPIENTS',		2).
valid_abstract_section_header('SURVIVAL RATES',		2).
valid_abstract_section_header('SYMPTOMATOLOGY',		2).
valid_abstract_section_header('SYMPTOMS AND SIGNS',		2).
valid_abstract_section_header('SYNTHESIS AND CONCLUSIONS',		2).
valid_abstract_section_header('SYNTHESIS AND FINDINGS',		2).
valid_abstract_section_header('SYNTHESIS OF RESULTS',		2).
valid_abstract_section_header('SYSTEM',		2).
valid_abstract_section_header('Selection criteria',		2).
valid_abstract_section_header('Study Selection and Data Extraction',		2).
valid_abstract_section_header('TABULATIONS, INTEGRATION, AND RESULTS',		2).
valid_abstract_section_header('TARGETS',		2).
valid_abstract_section_header('TASK FORCE SUGGESTIONS',		2).
valid_abstract_section_header('TAXONOMIC RELATIONSHIPS',		2).
valid_abstract_section_header('TECHNICAL DEVELOPMENT',		2).
valid_abstract_section_header('TECHNICAL PROCEDURE',		2).
valid_abstract_section_header('TECHNICAL SIGNIFICANCE',		2).
valid_abstract_section_header('TECHNIQUE AND RESULTS',		2).
valid_abstract_section_header('TERMS',		2).
valid_abstract_section_header('TESTED DOCUMENTS',		2).
valid_abstract_section_header('TESTS AVAILABLE',		2).
valid_abstract_section_header('THE AIM OF THE CURRENT STUDY WAS TWOFOLD',		2).
valid_abstract_section_header('THE APPROACH',		2).
valid_abstract_section_header('THE CASE HISTORY',		2).
valid_abstract_section_header('THE CASE STUDY',		2).
valid_abstract_section_header('THE CURRENT SITUATION',		2).
valid_abstract_section_header('THE GOAL OF THE STUDY',		2).
valid_abstract_section_header('THE INTERVENTION',		2).
valid_abstract_section_header('THE ISSUE',		2).
valid_abstract_section_header('THE LEVEL OF EVIDENCE',		2).
valid_abstract_section_header('THE METHODS',		2).
valid_abstract_section_header('THE MODEL',		2).
valid_abstract_section_header('THE NATURE AND AIM OF THE WORK',		2).
valid_abstract_section_header('THE NEXT STEP',		2).
valid_abstract_section_header('THE OBSERVATORY',		2).
valid_abstract_section_header('THE OUTCOMES DISSEMINATION PROJECT',		2).
valid_abstract_section_header('THE PAST',		2).
valid_abstract_section_header('THE PRESENT',		2).
valid_abstract_section_header('THE PRINCIPAL RESULTS',		2).
valid_abstract_section_header('THE PROPOSED MODEL',		2).
valid_abstract_section_header('THE RESULTS OF THE WORK',		2).
valid_abstract_section_header('THE STATEMENT OF SIGNIFICANCE',		2).
valid_abstract_section_header('THE STUDY OBJECTIVE',		2).
valid_abstract_section_header('THE WIDER CONTEXT',		2).
valid_abstract_section_header('THEORETICAL FRAME OF REFERENCE',		2).
valid_abstract_section_header('THEORY GENERATED',		2).
valid_abstract_section_header('THEORY INTO PRACTICE',		2).
valid_abstract_section_header('THERAPEUTIC CHOICE',		2).
valid_abstract_section_header('THERAPEUTIC OBJECTIVES',		2).
valid_abstract_section_header('THERAPEUTIC PERSPECTIVES',		2).
valid_abstract_section_header('THERAPEUTIC POTENTIAL',		2).
valid_abstract_section_header('THERAPEUTICAL APPROACH',		2).
valid_abstract_section_header('THERAPY AND CONCLUSION',		2).
valid_abstract_section_header('THERAPY STRUCTURE AND TECHNIQUES',		2).
valid_abstract_section_header('THESIS AND DISCUSSION',		2).
valid_abstract_section_header('THIS STUDY ADDS',		2).
valid_abstract_section_header('THIS THESIS AIMED TO',		2).
valid_abstract_section_header('THROMBUS FORMATION',		2).
valid_abstract_section_header('TIME POINTS',		2).
valid_abstract_section_header('TMS METHODS',		2).
valid_abstract_section_header('TOPIC OF THE STUDY',		2).
valid_abstract_section_header('TOPICAL HEADINGS',		2).
valid_abstract_section_header('TRACE ELEMENTS',		2).
valid_abstract_section_header('TRACKING COMPLIANCE',		2).
valid_abstract_section_header('TRANSCRIPT PROFILING',		2).
valid_abstract_section_header('TRANSLATION RELEVANCE',		2).
valid_abstract_section_header('TRANSMISSION AND PREVENTION',		2).
valid_abstract_section_header('TREATMENT REGIMENS',		2).
valid_abstract_section_header('TRIAL METHODS',		2).
valid_abstract_section_header('TRIAL NUMBERS',		2).
valid_abstract_section_header('TRIAL REGESTRATION',		2).
valid_abstract_section_header('TRIAL REGISTRATION ACTRN',		2).
valid_abstract_section_header('TRIAL REGISTRATION PROSPERO',		2).
valid_abstract_section_header('TRIAL REGISTRY NAME',		2).
valid_abstract_section_header('TRIALS REGISTRY',		2).
valid_abstract_section_header('TRIGGERS',		2).
valid_abstract_section_header('TT/TA',		2).
valid_abstract_section_header('TWO CASE REPORTS',		2).
valid_abstract_section_header('TYPE IA',		2).
valid_abstract_section_header('TYPE OF REVIEW AND SEARCH STRATEGY',		2).
valid_abstract_section_header('TYPES OF SETTING',		2).
valid_abstract_section_header('Target Population',		2).
valid_abstract_section_header('Time Horizon',		2).
valid_abstract_section_header('ULTRAMINI ABSTRACT',		2).
valid_abstract_section_header('ULTRAMINI-ABSTRACT',		2).
valid_abstract_section_header('ULTRASONOGRAPHY',		2).
valid_abstract_section_header('UNDERLYING MECHANISMS',		2).
valid_abstract_section_header('UPDATE METHODOLOGY',		2).
valid_abstract_section_header('URBAN AREAS',		2).
valid_abstract_section_header('USE OF INFORMATION TO SET AND EVALUATE QUALITY GOALS AND PRIORITIZE INITIATIVES',		2).
valid_abstract_section_header('UTILITY AND DISCUSSION',		2).
valid_abstract_section_header('VACCINES',		2).
valid_abstract_section_header('VALUE',		2).
valid_abstract_section_header('VIDEO',		2).
valid_abstract_section_header('WHAT THIS ADDS',		2).
valid_abstract_section_header('WHAT WAS DONE',		2).
valid_abstract_section_header('WHAT WAS KNOWN BEFORE',		2).
valid_abstract_section_header('WHERE',		2).
valid_abstract_section_header('WHY',		2).
valid_abstract_section_header('WHY SHOULD AN EMERGENCY PHYSICIAN BE AWARE OF THIS',		2).
valid_abstract_section_header('WIDER IMPLICATION OF THE FINDING',		2).
valid_abstract_section_header('WIDER IMPLICATIONS FOR THE FINDINGS',		2).
valid_abstract_section_header('WORKSHOP OUTCOMES',		2).
valid_abstract_section_header('WOUND HEALING',		2).
valid_abstract_section_header('ZIEL',		2).
valid_abstract_section_header(', BMI',		1).
valid_abstract_section_header(', METHODS',		1).
valid_abstract_section_header(', PEDOT',		1).
valid_abstract_section_header(', SAFETY WARNING',		1).
valid_abstract_section_header('1 BACKGROUND',		1).
valid_abstract_section_header('1 OBJECTIVE',		1).
valid_abstract_section_header('1 PRIMARY OBJECTIVES',		1).
valid_abstract_section_header('12 METHOD',		1).
valid_abstract_section_header('12 METHODS',		1).
valid_abstract_section_header('14 CONCLUSION',		1).
valid_abstract_section_header('14 CONCLUSIONS',		1).
valid_abstract_section_header('15 HIV RISK BEHAVIORS IN DRUG COURT',		1).
valid_abstract_section_header('16 HIV RISK FACTORS IN DRUG COURT',		1).
valid_abstract_section_header('17 GEOGRAPHIC RISK FOR HIV',		1).
valid_abstract_section_header('1METHOD',		1).
valid_abstract_section_header('2 MATERIAL AND METHODS',		1).
valid_abstract_section_header('2 SECONDARY OBJECTIVES',		1).
valid_abstract_section_header('2000 AMS',		1).
valid_abstract_section_header('4 SYNTHESIS',		1).
valid_abstract_section_header('A CHARGE FOR THE FUTURE',		1).
valid_abstract_section_header('A CLIMATE FOR RISK ASSESSMENT',		1).
valid_abstract_section_header('A CONCEPTUAL MODEL FOR ASSESSING THE QUALITY OF GLYCEMIC CONTROL',		1).
valid_abstract_section_header('A CONCLUSION',		1).
valid_abstract_section_header('A GRASSROOTS BENCHMARKING EXAMPLE',		1).
valid_abstract_section_header('A GROWING INTEREST IN QUALITY',		1).
valid_abstract_section_header('A HETEROGENEOUS DISEASE',		1).
valid_abstract_section_header('A LONG ACTING ANALOGUE',		1).
valid_abstract_section_header('A MANAGEMENT TOOL',		1).
valid_abstract_section_header('A MESSAGE FROM ASCO\'S PRESIDENT',		1).
valid_abstract_section_header('A MODEL FOR OTHER STATES',		1).
valid_abstract_section_header('A NATIONAL ACCREDITATION PROGRAM',		1).
valid_abstract_section_header('A NATIONAL INCENTIVE FOR IMPROVEMENT',		1).
valid_abstract_section_header('A NEW ACADEMIC SUBSPECIALTY',		1).
valid_abstract_section_header('A NEW CONCEPT OF LIGHT INDUCED POLYMERIZATION OF ESTHETIC MATERIALS IN DENTISTRY',		1).
valid_abstract_section_header('A NEW GENUS AND SPECIES OF PYGMY GRASSHOPPER (ORTHOPTERA',		1).
valid_abstract_section_header('A NEW MARKET MODEL',		1).
valid_abstract_section_header('A NEW MODEL',		1).
valid_abstract_section_header('A NEW PCI MESOSYSTEM',		1).
valid_abstract_section_header('A NEW SPECIES OF COCCIDIAN (PROTOZOA',		1).
valid_abstract_section_header('A NOVEL APPROACH',		1).
valid_abstract_section_header('A NOVEL FOLLOW UP TOOL',		1).
valid_abstract_section_header('A PEDOT',		1).
valid_abstract_section_header('A PIECE OF PAPER (APOP) IS BORN',		1).
valid_abstract_section_header('A PLANNING WORKSHEET',		1).
valid_abstract_section_header('A POLICY FOR DNR ORDERS WITHIN A FRAMEWORK OF GOALS OF CARE',		1).
valid_abstract_section_header('A PRACTICAL FRAMEWORK',		1).
valid_abstract_section_header('A PRE-TREATMENT DOCUMENTS',		1).
valid_abstract_section_header('A PRESCRIPTION FOR CHANGE',		1).
valid_abstract_section_header('A PROPOSED MODEL',		1).
valid_abstract_section_header('A QUALITY ASSESSMENT TOOL',		1).
valid_abstract_section_header('A QUESTION',		1).
valid_abstract_section_header('A REPORT ON THE PRECISION MEDICINE',		1).
valid_abstract_section_header('A RETROSPECTIVELY REGISTERED STUDY',		1).
valid_abstract_section_header('A ROAD MAP FOR MEASURES DEVELOPMENT',		1).
valid_abstract_section_header('A RULE',		1).
valid_abstract_section_header('A SECOND REVOLUTION',		1).
valid_abstract_section_header('A SOLUTION',		1).
valid_abstract_section_header('A STUDY OF ORGAN DONOR PANCREASES',		1).
valid_abstract_section_header('A SUMMARY OF BACKGROUND DATA',		1).
valid_abstract_section_header('A SYSTEMS APPROACH TO ADDRESS INAPPROPRIATE USE OF CONTROLLED SUBSTANCES',		1).
valid_abstract_section_header('A TRADITIONAL REGULATOR',		1).
valid_abstract_section_header('A TRIAL WAS CONDUCTED TO',		1).
valid_abstract_section_header('A WEANING TOOL',		1).
valid_abstract_section_header('A YAG',		1).
valid_abstract_section_header('A-P-BAV',		1).
valid_abstract_section_header('AA) CONCLUSION',		1).
valid_abstract_section_header('AA/GA',		1).
valid_abstract_section_header('AB (SBP',		1).
valid_abstract_section_header('ABBREVIATIONS CES-D',		1).
valid_abstract_section_header('ABLATION METHODS',		1).
valid_abstract_section_header('ABOUT THE ISSUE',		1).
valid_abstract_section_header('ABOUT THE UPDATE',		1).
valid_abstract_section_header('ABSTRACT FRIENDLY YOUNG LADIES',		1).
valid_abstract_section_header('ABSTRACT INTRODUCTION',		1).
valid_abstract_section_header('ABSTRACT RECLAIMING A USABLE PAST PASSIONATE COMMUNITIES',		1).
valid_abstract_section_header('ABSTRACT REFERENCE',		1).
valid_abstract_section_header('ABSTRACT\' BACKGROUND',		1).
valid_abstract_section_header('ABSTRACTCONTEXT AND OBJECTIVE',		1).
valid_abstract_section_header('ABSTRAGT',		1).
valid_abstract_section_header('AC (R',		1).
valid_abstract_section_header('AC/AA',		1).
valid_abstract_section_header('ACADEMIC PROGRAMMES FOR TRAINING AND RESEARCH IN PUBLIC HEALTH IN SOUTH EASTERN EUROPE',		1).
valid_abstract_section_header('ACCELERATING THE PACE OF IMPROVEMENT',		1).
valid_abstract_section_header('ACCEPTABLE STANDARDS',		1).
valid_abstract_section_header('ACCESS AND MEASUREMENT ISSUES',		1).
valid_abstract_section_header('ACCESS TO HEALTHCARE',		1).
valid_abstract_section_header('ACCESSION NUMBER',		1).
valid_abstract_section_header('ACCME ACCREDIATION',		1).
valid_abstract_section_header('ACCOMPLISHMENTS',		1).
valid_abstract_section_header('ACCORDING TO DAMESHEK, TRUE POLYCYTHEMIA (POLYCYTHEMIA VERA',		1).
valid_abstract_section_header('ACCORDING TO THE RESULTS OF RECENT STUDIES',		1).
valid_abstract_section_header('ACCREDITATION AS A STIMULUS FOR CONTINUOUS IMPROVEMENT',		1).
valid_abstract_section_header('ACCURACY OF DIAGNOSIS',		1).
valid_abstract_section_header('ACE INHIBITION FOLLOWING MYOCARDIAL INJURY',		1).
valid_abstract_section_header('ACE STRUCTURE AND FUNCTION',		1).
valid_abstract_section_header('ACETAMINOPHEN',		1).
valid_abstract_section_header('ACHIEVING CYBER SAFE DIABETES DEVICES',		1).
valid_abstract_section_header('ACHTERGROND',		1).
valid_abstract_section_header('ACQUIRED VALVE LESIONS',		1).
valid_abstract_section_header('ACQUISITION AND SYNTHESIS OF EVIDENCE',		1).
valid_abstract_section_header('ACRONYMS',		1).
valid_abstract_section_header('ACRYLICS',		1).
valid_abstract_section_header('ACTION AND OUTCOME',		1).
valid_abstract_section_header('ACTION OF LOSARTAN',		1).
valid_abstract_section_header('ACTION OF NEWER CALCIUM ANTAGONISTS',		1).
valid_abstract_section_header('ACTION PLAN OBJECTIVES',		1).
valid_abstract_section_header('ACTION RESEARCH DESIGN',		1).
valid_abstract_section_header('ACTIONS AND OUTCOMES',		1).
valid_abstract_section_header('ACTIVE ARM',		1).
valid_abstract_section_header('ACTIVE COATING',		1).
valid_abstract_section_header('ACTIVE INGREDIENTS',		1).
valid_abstract_section_header('ACTIVIDADES',		1).
valid_abstract_section_header('ACTIVITES',		1).
valid_abstract_section_header('ACTIVITIES AND OUTCOMES',		1).
valid_abstract_section_header('ACTUAL SITUATION AND METHODS',		1).
valid_abstract_section_header('ACTUAL TOPICS',		1).
valid_abstract_section_header('ACTUALITIES AND MAIN POINTS',		1).
valid_abstract_section_header('ACUPUNCTURE APPROACH',		1).
valid_abstract_section_header('ACUTE BACTERIAL RHINOSINUSITIS',		1).
valid_abstract_section_header('ACUTE COMPLEX CARE MODEL (ACCM)',		1).
valid_abstract_section_header('ACUTE CONGESTION',		1).
valid_abstract_section_header('ACUTE MANAGEMENT OF SUDDEN CARDIAC DEATH',		1).
valid_abstract_section_header('ACUTE PAIN IN CHILDHOOD',		1).
valid_abstract_section_header('ACUTE RHINOSINUSITIS CAUSED BY DENTAL PROBLEMS',		1).
valid_abstract_section_header('ACUTE STUDY',		1).
valid_abstract_section_header('ACUTE TREATMENT',		1).
valid_abstract_section_header('ADAPTATION FOR ADOLESCENTS',		1).
valid_abstract_section_header('ADAPTING BENCHMARKING TO HEALTH CARE',		1).
valid_abstract_section_header('ADDITIONAL COMMENTS',		1).
valid_abstract_section_header('ADDITIONAL EXAMINATIONS',		1).
valid_abstract_section_header('ADDITIONAL INFORMATION',		1).
valid_abstract_section_header('ADDITIONAL INTERVENTIONS',		1).
valid_abstract_section_header('ADDITIONAL PROCEDURES',		1).
valid_abstract_section_header('ADDITIVES TO LOCAL ANESTHETICS',		1).
valid_abstract_section_header('ADDRESSING PATIENT SAFETY AIMS',		1).
valid_abstract_section_header('ADDRESSING THE GOAL',		1).
valid_abstract_section_header('ADDRESSING THE INSTITUTE OF MEDICINE (IOM) QUALITY AIMS',		1).
valid_abstract_section_header('ADDRESSING THE NEED',		1).
valid_abstract_section_header('ADDRESSING THE SIX IOM QUALITY AIMS',		1).
valid_abstract_section_header('ADHD) CONCLUSION',		1).
valid_abstract_section_header('ADJUNCTIVE MEASURES BEFORE ADMINISTRATION OF ANTIMICROBIAL THERAPY',		1).
valid_abstract_section_header('ADJUVANT AND NEOADJUVANT TREATMENT',		1).
valid_abstract_section_header('ADJUVANT HDCT',		1).
valid_abstract_section_header('ADJUVANT THERAPY IN HIGH RISK MELANOMA',		1).
valid_abstract_section_header('ADMINISTRATION OF SEXUAL STEROIDS DURING PREGNANCY AND SEXUAL DIFFERENTIATION',		1).
valid_abstract_section_header('ADMINISTRATIVE ASPECTS',		1).
valid_abstract_section_header('ADMINISTRATIVE COSTS',		1).
valid_abstract_section_header('ADMINISTRATIVE DATABASES TO STUDY OUTCOMES AND QUALITY OF CARE',		1).
valid_abstract_section_header('ADMISSION',		1).
valid_abstract_section_header('ADOPTION',		1).
valid_abstract_section_header('ADR MONITORING PROGRAM',		1).
valid_abstract_section_header('ADT AND LHRH ANALOGS',		1).
valid_abstract_section_header('ADULT ONSET FOCAL DYSTONIA',		1).
valid_abstract_section_header('ADVANCED LIFE SUPPORT',		1).
valid_abstract_section_header('ADVANCEMENTS',		1).
valid_abstract_section_header('ADVERSE BIOLOGICAL EFFECTS OF COBALT AND CHROMIUM',		1).
valid_abstract_section_header('ADVERSE EFFECT PROFILE',		1).
valid_abstract_section_header('ADVERSE EVENT',		1).
valid_abstract_section_header('AERAS COVERED',		1).
valid_abstract_section_header('AEROMEDICAL DISPOSTION',		1).
valid_abstract_section_header('AESTHETIC RESULTS',		1).
valid_abstract_section_header('AESTHETIC SEQUELAE',		1).
valid_abstract_section_header('AETIOLOGY AND PATHOGENESIS',		1).
valid_abstract_section_header('AETIOLOGY AND TRANSMISSION',		1).
valid_abstract_section_header('AETIOLOGY AND VIRUS TRANSMISSION',		1).
valid_abstract_section_header('AETIOPATHOGENESIS',		1).
valid_abstract_section_header('AFFINITY MATURATION OCCURS THROUGH TWO SELECTION PROCESSES',		1).
valid_abstract_section_header('AFFLUENCE AND OBESITY AS RISK FACTORS',		1).
valid_abstract_section_header('AFSSAPS NO',		1).
valid_abstract_section_header('AFTER IRRADIATION',		1).
valid_abstract_section_header('AFTER THE WAR',		1).
valid_abstract_section_header('AGAINST THE INFLAMMATORY CASCADE',		1).
valid_abstract_section_header('AGE AND GENDER',		1).
valid_abstract_section_header('AGEING POPULATION',		1).
valid_abstract_section_header('AGENCIES THAT FUND SCIENTIFIC RESEARCH MUST CHOOSE',		1).
valid_abstract_section_header('AGENDA FOR THE FUTURE',		1).
valid_abstract_section_header('AGREEMENT',		1).
valid_abstract_section_header('AGREEMENT ON COLLABORATION OF THE PUBLIC HEALTH CONSORTIUM FOR SOUTH EASTERN EUROPE',		1).
valid_abstract_section_header('AIM & BACKGROUND',		1).
valid_abstract_section_header('AIM & DESIGN',		1).
valid_abstract_section_header('AIM & OBJECTIVE',		1).
valid_abstract_section_header('AIM & PURPOSE',		1).
valid_abstract_section_header('AIM & SCOPE',		1).
valid_abstract_section_header('AIM &OBJECTIVES',		1).
valid_abstract_section_header('AIM AND ASSUMPTIONS',		1).
valid_abstract_section_header('AIM AND CASE REPORT',		1).
valid_abstract_section_header('AIM AND DESIGN OF THE STUDY',		1).
valid_abstract_section_header('AIM AND DISCUSSION',		1).
valid_abstract_section_header('AIM AND KEY SCIENTIFIC CONCEPTS OF THE REVIEW',		1).
valid_abstract_section_header('AIM AND MAIN METHOD',		1).
valid_abstract_section_header('AIM AND MAIN OUTCOME MEASURE',		1).
valid_abstract_section_header('AIM AND MATERIAL AND METHODS',		1).
valid_abstract_section_header('AIM AND MATERIAL.',		1).
valid_abstract_section_header('AIM AND MATERIALS & METHODS',		1).
valid_abstract_section_header('AIM AND RATIONALE',		1).
valid_abstract_section_header('AIM AND STUDY DESIGN',		1).
valid_abstract_section_header('AIM FOR DATABASE',		1).
valid_abstract_section_header('AIM FOR THE STUDY',		1).
valid_abstract_section_header('AIM OF ARTICLE',		1).
valid_abstract_section_header('AIM OF INNOVATION',		1).
valid_abstract_section_header('AIM OF LEADERSHIP PROGRAMME',		1).
valid_abstract_section_header('AIM OF PRESENTATION',		1).
valid_abstract_section_header('AIM OF REPORT',		1).
valid_abstract_section_header('AIM OF SURGERY',		1).
valid_abstract_section_header('AIM OF THE CONSENSUS CONFERENCE',		1).
valid_abstract_section_header('AIM OF THE GUIDELINE',		1).
valid_abstract_section_header('AIM OF THE OPERATION',		1).
valid_abstract_section_header('AIM OF THE PRESENT STUDY',		1).
valid_abstract_section_header('AIM OF THE STUDY AND CASES',		1).
valid_abstract_section_header('AIM OF THE STUDY AND ETHNOPHARMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('AIM OF THE SUDY',		1).
valid_abstract_section_header('AIM OF THE VIDEO / INTRODUCTION',		1).
valid_abstract_section_header('AIM OF THE VIDEO/INTRODUCTION',		1).
valid_abstract_section_header('AIM OF VIDEO/INTRODUCTION',		1).
valid_abstract_section_header('AIM OR OBJECTIVE',		1).
valid_abstract_section_header('AIM THIS STUDY',		1).
valid_abstract_section_header('AIM\'S',		1).
valid_abstract_section_header('AIM(S)/RESEARCH QUESTION',		1).
valid_abstract_section_header('AIM, METHOD AND RESULTS',		1).
valid_abstract_section_header('AIM, METHODS AND EXAMINEES',		1).
valid_abstract_section_header('AIM, METHODS AND PATIENTS',		1).
valid_abstract_section_header('AIM, METHODS, RESULTS',		1).
valid_abstract_section_header('AIM, PATIENTS, METHOD',		1).
valid_abstract_section_header('AIM-CASE REPORT',		1).
valid_abstract_section_header('AIM-METHODS',		1).
valid_abstract_section_header('AIM/METHODOLOGY',		1).
valid_abstract_section_header('AIM/PURPOSE OF THE STUDY',		1).
valid_abstract_section_header('AIM/RESULTS',		1).
valid_abstract_section_header('AIM/S',		1).
valid_abstract_section_header('AIM/SCOPE',		1).
valid_abstract_section_header('AIM/SETTING/PARTICIPANTS',		1).
valid_abstract_section_header('AIMS & BACKGROUNDS',		1).
valid_abstract_section_header('AIMS & RESULTS',		1).
valid_abstract_section_header('AIMS AND DISCUSSION',		1).
valid_abstract_section_header('AIMS AND HYPOTHESES',		1).
valid_abstract_section_header('AIMS AND OBJCTIVES',		1).
valid_abstract_section_header('AIMS AND OBJECTIVES/PURPOSE/RESEARCH QUESTIONS',		1).
valid_abstract_section_header('AIMS AND OBJECTIVIES',		1).
valid_abstract_section_header('AIMS AND OBJECTS',		1).
valid_abstract_section_header('AIMS AND SAMPLE',		1).
valid_abstract_section_header('AIMS AND SETTINGS',		1).
valid_abstract_section_header('AIMS OF PART 1',		1).
valid_abstract_section_header('AIMS OF PART 2',		1).
valid_abstract_section_header('AIMS OF PART 4',		1).
valid_abstract_section_header('AIMS OF THE QUALITY STANDARDS',		1).
valid_abstract_section_header('AIMS OF THE RESEARCH',		1).
valid_abstract_section_header('AIMS OF THIS REVIEW',		1).
valid_abstract_section_header('AIMS SUBJECTS AND METHODS',		1).
valid_abstract_section_header('AIMS TO',		1).
valid_abstract_section_header('AIMS, MATERIALS AND METHOD',		1).
valid_abstract_section_header('AIMS, METHODS & FINDINGS',		1).
valid_abstract_section_header('AIMS, OBJECTIVES AND METHODS',		1).
valid_abstract_section_header('AIMS/CASE DESCRIPTION',		1).
valid_abstract_section_header('AIMS/METHODS & PROCEDURES',		1).
valid_abstract_section_header('AIMS/STUDY DESIGN',		1).
valid_abstract_section_header('AKINESIA AND SENSORY BLOCKADE',		1).
valid_abstract_section_header('ALC/AMC PS',		1).
valid_abstract_section_header('ALCOHOL AND PREVALENCE OF ORAL LEUKOPLAKIA',		1).
valid_abstract_section_header('ALCOHOL CONSUMPTION',		1).
valid_abstract_section_header('ALCOHOL DRINKING',		1).
valid_abstract_section_header('ALCOHOLIC LIVER DISEASE HAS VARIOUS MANIFESTATIONS',		1).
valid_abstract_section_header('ALDOSTERONISM',		1).
valid_abstract_section_header('ALGORITHM AVAILABILITY',		1).
valid_abstract_section_header('ALGORITHM FOR DIAGNOSTIC STUDIES',		1).
valid_abstract_section_header('ALLERGOLOGIC INVESTIGATIONS',		1).
valid_abstract_section_header('ALLERGOLOGIC STUDY',		1).
valid_abstract_section_header('ALLERGY STUDY',		1).
valid_abstract_section_header('ALLIANCE DEVELOPMENT',		1).
valid_abstract_section_header('ALLIANCE MANAGEMENT',		1).
valid_abstract_section_header('ALLIANCE MODELS',		1).
valid_abstract_section_header('ALLOGENEIC TRANSPLANTATION',		1).
valid_abstract_section_header('ALMOST ALL LUMBRICID EARTHWORMS (OLIGOCHAETA',		1).
valid_abstract_section_header('ALOE WHOLE LEAF OR DECOLORIZED WHOLE LEAF',		1).
valid_abstract_section_header('ALP-ISMC',		1).
valid_abstract_section_header('ALTERATIONS',		1).
valid_abstract_section_header('ALTERATIONS IN ENDOTHELIAL FUNCTION',		1).
valid_abstract_section_header('ALTERATIONS OF ORAL HEALTH DURING PREGNANCY',		1).
valid_abstract_section_header('ALTERNATE APPROACHES',		1).
valid_abstract_section_header('ALTERNATIVE MODEL',		1).
valid_abstract_section_header('ALTERNATIVE TECHNOLOGY',		1).
valid_abstract_section_header('ALTERNATIVE THERAPY',		1).
valid_abstract_section_header('AMBLYOPIA',		1).
valid_abstract_section_header('AMBLYOPIA THROUGH THE DECADES',		1).
valid_abstract_section_header('AMS MATHEMATICS SUBJECT CLASSIFICATION',		1).
valid_abstract_section_header('AMS VERSIONS AVAILABLE',		1).
valid_abstract_section_header('AN IMPORTANT ROLE',		1).
valid_abstract_section_header('AN INDICATOR APPROACH',		1).
valid_abstract_section_header('AN INFORMATION SECURITY PROGRAM',		1).
valid_abstract_section_header('AN INTEGRATIVE FRAMEWORK',		1).
valid_abstract_section_header('AN INTERVIEW WITH JULIE MORATH',		1).
valid_abstract_section_header('AN OBJECTIVE',		1).
valid_abstract_section_header('AN OBSERVATIONAL STUDY',		1).
valid_abstract_section_header('AN OVERVIEW',		1).
valid_abstract_section_header('ANAESTHETIC CONSIDERATIONS',		1).
valid_abstract_section_header('ANALGESICS',		1).
valid_abstract_section_header('ANALYSES AND RESULTS',		1).
valid_abstract_section_header('ANALYSIS AND CONCLUSION',		1).
valid_abstract_section_header('ANALYSIS AND DISCUSSION',		1).
valid_abstract_section_header('ANALYSIS AND INTERPRETATION',		1).
valid_abstract_section_header('ANALYSIS AND MAIN OUTCOME MEASURE',		1).
valid_abstract_section_header('ANALYSIS AND REVIEW OF IMAGING COMPATIBILITY AND SAFETY OF COMMON IMPLANTS AND DEVICES',		1).
valid_abstract_section_header('ANALYSIS APPROACH',		1).
valid_abstract_section_header('ANALYSIS OF A SELECTED SET OF ANTIMICROBIAL PEPTIDES',		1).
valid_abstract_section_header('ANALYSIS OF HEART RATE VARIABILITY',		1).
valid_abstract_section_header('ANALYSIS OF INTERVIEWS',		1).
valid_abstract_section_header('ANALYSIS OF THE DIFFERENT METHODS',		1).
valid_abstract_section_header('ANALYSIS OF THE PROBLEM',		1).
valid_abstract_section_header('ANALYSIS TOOLS',		1).
valid_abstract_section_header('ANALYSIS USED',		1).
valid_abstract_section_header('ANALYSIS WITH RESULTS',		1).
valid_abstract_section_header('ANALYTIC APPROACHES',		1).
valid_abstract_section_header('ANALYTIC METHODS',		1).
valid_abstract_section_header('ANALYTICAL FLEXIBILITY',		1).
valid_abstract_section_header('ANALYTICAL FRAMEWORK',		1).
valid_abstract_section_header('ANALYTICAL PLAN',		1).
valid_abstract_section_header('ANALYTICAL PROCEDURES',		1).
valid_abstract_section_header('ANATOMIC INJURY SEVERITY SCALES',		1).
valid_abstract_section_header('ANATOMICAL AND ELECTROPHYSIOLOGICAL EVIDENCE',		1).
valid_abstract_section_header('ANATOMICAL STUDY',		1).
valid_abstract_section_header('ANATOMOCLINICAL ENTITIES',		1).
valid_abstract_section_header('ANATOMY AND PROCEDURE',		1).
valid_abstract_section_header('ANESTHESIA MACHINES',		1).
valid_abstract_section_header('ANESTHETIC AND INTRAOPERATIVE MONITORING PROTOCOLS',		1).
valid_abstract_section_header('ANGIOGRAPHY AND EMBOLIZATION',		1).
valid_abstract_section_header('ANGIOTENSIN II AS A PARADIGM OF A MULTIFUNCTIONAL HORMONE',		1).
valid_abstract_section_header('ANGIOTENSIN RECEPTOR ANTAGONISTS',		1).
valid_abstract_section_header('ANGIOTENSIN RECEPTORS',		1).
valid_abstract_section_header('ANIMAL AND OWNER',		1).
valid_abstract_section_header('ANIMAL AND PATIENT(S)',		1).
valid_abstract_section_header('ANIMAL DATA',		1).
valid_abstract_section_header('ANIMAL EXPERIMENT',		1).
valid_abstract_section_header('ANIMAL HOSTS',		1).
valid_abstract_section_header('ANIMAL MODEL',		1).
valid_abstract_section_header('ANIMAL STUDIED AND PROCEDURE',		1).
valid_abstract_section_header('ANIMALS AND METHOD',		1).
valid_abstract_section_header('ANIMALS AND PROCEDURES',		1).
valid_abstract_section_header('ANIMALS AND PROTOCOL',		1).
valid_abstract_section_header('ANIMALS INCLUDED',		1).
valid_abstract_section_header('ANIMALS OR ANIMAL POPULATION',		1).
valid_abstract_section_header('ANIMALS STUDIED AND METHODS',		1).
valid_abstract_section_header('ANIMALS STUDIED CLINICALLY',		1).
valid_abstract_section_header('ANIMALS(S)',		1).
valid_abstract_section_header('ANIMALS, METHODS AND RESULTS',		1).
valid_abstract_section_header('ANNUAL REPORT',		1).
valid_abstract_section_header('ANSWER TO QUESTION',		1).
valid_abstract_section_header('ANSWER TO THE QUESTION',		1).
valid_abstract_section_header('ANTEGRADE SELECTIVE CEREBRAL PERFUSION (ASCP)',		1).
valid_abstract_section_header('ANTENATAL DIAGNOSIS',		1).
valid_abstract_section_header('ANTIBIOTIC CHOICE',		1).
valid_abstract_section_header('ANTIBIOTIC RESISTANCE',		1).
valid_abstract_section_header('ANTIBIOTIC TIMING AS A PERFORMANCE MEASURE',		1).
valid_abstract_section_header('ANTIBIOTICS AND HIV/AIDS',		1).
valid_abstract_section_header('ANTIBIOTICS WITHOUT DELAY',		1).
valid_abstract_section_header('ANTICIPATED BENEFITS',		1).
valid_abstract_section_header('ANTICIPATED IMPACT',		1).
valid_abstract_section_header('ANTICIPATING THE FUTURE',		1).
valid_abstract_section_header('ANTICOAGULATION CONTROL',		1).
valid_abstract_section_header('ANTIHELMINTH TREATMENT (TEST)',		1).
valid_abstract_section_header('ANTIHYPERTENSIVES',		1).
valid_abstract_section_header('ANTIPHLOGISTIC TREATMENT',		1).
valid_abstract_section_header('ANTITHESIS',		1).
valid_abstract_section_header('ANY SIGNS OF IMPAIRED CIRCULATION',		1).
valid_abstract_section_header('ANZCTRN',		1).
valid_abstract_section_header('AORTIC VALVE RECONSTRUCTION',		1).
valid_abstract_section_header('APARATURA',		1).
valid_abstract_section_header('APD METHODS',		1).
valid_abstract_section_header('APICAL LESIONS',		1).
valid_abstract_section_header('APICO-MARGINAL LESIONS',		1).
valid_abstract_section_header('APOP IN PRACTICE',		1).
valid_abstract_section_header('APOPHYSIS OF THE GREATER TROCHANTER',		1).
valid_abstract_section_header('APOPTOSIS AND MOLECULAR IMAGING',		1).
valid_abstract_section_header('APPARATUS DESIGN AND TESTING',		1).
valid_abstract_section_header('APPLIANCE DESIGN AND TESTING',		1).
valid_abstract_section_header('APPLICABILITY AND IMPLEMENTATION ISSUES',		1).
valid_abstract_section_header('APPLICABLE PROJECT AREA',		1).
valid_abstract_section_header('APPLICATION AND DISCUSSIONS',		1).
valid_abstract_section_header('APPLICATION AND RELEVANCE',		1).
valid_abstract_section_header('APPLICATION AND UTILITY',		1).
valid_abstract_section_header('APPLICATION IN CLINICAL ROUTINE',		1).
valid_abstract_section_header('APPLICATION OF METHODS',		1).
valid_abstract_section_header('APPLICATION OF PCR PROCEDURE IN THE FIELD OF IMMUNOGENETICS AND HISTOCOMPATIBILITY',		1).
valid_abstract_section_header('APPLICATION OF PRINCIPLES',		1).
valid_abstract_section_header('APPLICATION OF TELEMETRY',		1).
valid_abstract_section_header('APPLICATION TO HEALTH PROFESSIONAL EDUCATION',		1).
valid_abstract_section_header('APPLICATION TO NURSING',		1).
valid_abstract_section_header('APPLICATION TO PEDIATRIC NEUROSURGERY',		1).
valid_abstract_section_header('APPLICATIONS FOR CHILD NUTRITION PROFESSIONALS',		1).
valid_abstract_section_header('APPLICATIONS OF THE DMM IN MEDICAL RECORD TEMPLATES',		1).
valid_abstract_section_header('APPLICATIONS TO CHILD NUTRITION PROFESSIONALS',		1).
valid_abstract_section_header('APPLICATIONS TO PERIPHERAL PERFUSION',		1).
valid_abstract_section_header('APPLYING EBP TO COMBINED MEDICAL AND NURSING PROBLEMS',		1).
valid_abstract_section_header('APPLYING THE MODEL TO THE HEPARIN QI PROJECT',		1).
valid_abstract_section_header('APPRAISAL AND DATA EXTRACTION',		1).
valid_abstract_section_header('APPRAISAL OF LITERATURE',		1).
valid_abstract_section_header('APPRAISAL OF THE INITIATIVE',		1).
valid_abstract_section_header('APPROACH AND CONCLUSIONS',		1).
valid_abstract_section_header('APPROACH AND CONTENT',		1).
valid_abstract_section_header('APPROACH AND EVIDENCE',		1).
valid_abstract_section_header('APPROACH AND FINDINGS',		1).
valid_abstract_section_header('APPROACH AND RESULT',		1).
valid_abstract_section_header('APPROACH AND RESULTS--',		1).
valid_abstract_section_header('APPROACH TO ADDRESSING THE SIX IOM AIMS',		1).
valid_abstract_section_header('APPROACH TO THE PROBLEM',		1).
valid_abstract_section_header('APPROACH/METHODS',		1).
valid_abstract_section_header('APPROACH/RESULTS',		1).
valid_abstract_section_header('APPROPRIATE USE OF CRITICAL CARE RESOURCES',		1).
valid_abstract_section_header('APPROVAL',		1).
valid_abstract_section_header('APPROVAL PROCESS',		1).
valid_abstract_section_header('APTEM',		1).
valid_abstract_section_header('AQUATIC EXERCISE 5 TRIALS',		1).
valid_abstract_section_header('AREA OF AGREEMENT',		1).
valid_abstract_section_header('AREAS COVERED IN REVIEW',		1).
valid_abstract_section_header('AREAS OF AGREEMENT IMMEDIATE FIELD TREATMENT',		1).
valid_abstract_section_header('AREAS OF AGREEMENT, AREAS OF CONTROVERSY',		1).
valid_abstract_section_header('AREAS OF AGREEMENT, AREAS OF CONTROVERSY, GROWING POINTS, AREAS TIMELY FOR DEVELOPING RESEARCH',		1).
valid_abstract_section_header('AREAS OF CONCERN',		1).
valid_abstract_section_header('AREAS OF DISAGREEMENT',		1).
valid_abstract_section_header('AREAS OF RESEARCH',		1).
valid_abstract_section_header('AREAS OF REVIEW',		1).
valid_abstract_section_header('AREAS OF TIMELY FOR DEVELOPMENT RESEARCH',		1).
valid_abstract_section_header('AREAS OF UNCERTAINTY/RESEARCH NEED',		1).
valid_abstract_section_header('AREAS THAT ARE CONTROVERSIAL',		1).
valid_abstract_section_header('AREAS TIMELY FOR DEVELOPING FURTHER RESEARCH',		1).
valid_abstract_section_header('AREAS TIMELY FOR DEVELOPING NEW RESEARCH',		1).
valid_abstract_section_header('AREAS TIMELY FOR POLICY RESEARCH AND ACTION',		1).
valid_abstract_section_header('AREAS TIMELY FOR RESEARCH',		1).
valid_abstract_section_header('ARGUMENT AND CONCLUSION',		1).
valid_abstract_section_header('ARGUMENT/ANALYSIS',		1).
valid_abstract_section_header('ARGUMENTS AND DISCUSSION',		1).
valid_abstract_section_header('ARTERIOLAR HYPERTROPHY',		1).
valid_abstract_section_header('ARTICLE CATEGORY',		1).
valid_abstract_section_header('ARTICLE FOCUS',		1).
valid_abstract_section_header('ARTICLE TITLE AND BIBILOGRAPHIC INFORMATION',		1).
valid_abstract_section_header('ARTICLE TITLE AND BIBLIOGRAPHICAL INFORMATION',		1).
valid_abstract_section_header('ARTIFICIAL BLOOD PUMPS',		1).
valid_abstract_section_header('ASBTRACT',		1).
valid_abstract_section_header('ASCO PANEL CONCLUSION',		1).
valid_abstract_section_header('ASESSMENT',		1).
valid_abstract_section_header('ASPECTS OF ANTIBIOTIC THERAPY',		1).
valid_abstract_section_header('ASPECTS OF OUTPATIENT DIAGNOSTICS',		1).
valid_abstract_section_header('ASSAY METHOD',		1).
valid_abstract_section_header('ASSESSING PHYSICIAN TEAMWORK',		1).
valid_abstract_section_header('ASSESSING PROGNOSIS',		1).
valid_abstract_section_header('ASSESSING RISK',		1).
valid_abstract_section_header('ASSESSMENT AND CONCLUSION',		1).
valid_abstract_section_header('ASSESSMENT AND EXTRACTION OF DATA',		1).
valid_abstract_section_header('ASSESSMENT AND OUTCOME MEASUREMENTS',		1).
valid_abstract_section_header('ASSESSMENT AND RECORD KEEPING',		1).
valid_abstract_section_header('ASSESSMENT AS INSTRUCTIONAL DESIGN',		1).
valid_abstract_section_header('ASSESSMENT DEVELOPMENT',		1).
valid_abstract_section_header('ASSESSMENT OF EFFECTIVENESS',		1).
valid_abstract_section_header('ASSESSMENT OF EMERGENCY PREPAREDNESS',		1).
valid_abstract_section_header('ASSESSMENT OF ENERGY AND NUTRIENT INTAKE',		1).
valid_abstract_section_header('ASSESSMENT OF EXERCISE CAPACITY',		1).
valid_abstract_section_header('ASSESSMENT OF LOCAL PRACTICE',		1).
valid_abstract_section_header('ASSESSMENT OF PARAMETERS FOR THERAPY ALGORITHM',		1).
valid_abstract_section_header('ASSESSMENT OF RISK FACTORS (INDEPENDENT VARIABLES)',		1).
valid_abstract_section_header('ASSESSMENT OF TARGETS FOR GLYCEMIA CONTROL',		1).
valid_abstract_section_header('ASSESSMENT OF THE CURRENT SITUATION',		1).
valid_abstract_section_header('ASSESSMENT OF VARIABLES',		1).
valid_abstract_section_header('ASSESSMENT TOOLS/OUTCOMES',		1).
valid_abstract_section_header('ASSESSMENT VISITS',		1).
valid_abstract_section_header('ASSESSMENTS OF STIFFNESS',		1).
valid_abstract_section_header('ASSIGNMENT',		1).
valid_abstract_section_header('ASSIST DEVICES',		1).
valid_abstract_section_header('ASSOCIATED CANCERS',		1).
valid_abstract_section_header('ASSOCIATION POSITION',		1).
valid_abstract_section_header('ASYMPTOMATIC BACTERIURIA',		1).
valid_abstract_section_header('ASYMPTOMATIC DISEASE',		1).
valid_abstract_section_header('AT BASELINE',		1).
valid_abstract_section_header('AT THE START OF PREGNANCY',		1).
valid_abstract_section_header('AT1 RECEPTOR STRUCTURE AND LIGAND BINDING SITES',		1).
valid_abstract_section_header('ATRIAL REMODELING',		1).
valid_abstract_section_header('AUASS',		1).
valid_abstract_section_header('AUC/ROC',		1).
valid_abstract_section_header('AUCSAPSII',		1).
valid_abstract_section_header('AUDIOLOGIC DIAGNOSTICS',		1).
valid_abstract_section_header('AUDIT METHODS AND RESULTS',		1).
valid_abstract_section_header('AUDITING AND MANDATORY FUNCTIONALITY',		1).
valid_abstract_section_header('AUGMENTATION WITH IMPLANTS',		1).
valid_abstract_section_header('AUSTRALIA NEW ZEALAND CLINICAL TRIALS REGISTRATION',		1).
valid_abstract_section_header('AUSTRALIAN AND NEW ZEALAND EXPERT ADVISORS',		1).
valid_abstract_section_header('AUSTRALIAN AND NEW ZEALAND TRIAL REGISTRATION NUMBER',		1).
valid_abstract_section_header('AUSTRALIAN CLINICAL TRIAL REGISTRATION NUMBER',		1).
valid_abstract_section_header('AUSWIRKUNGEN DER ERYTHROZYTENLAGERUNG',		1).
valid_abstract_section_header('AUTHORS\' OPINION',		1).
valid_abstract_section_header('AUTHORS\' SUMMARY',		1).
valid_abstract_section_header('AUTOIMMUNE BULLOUS DISEASES',		1).
valid_abstract_section_header('AUTOIMMUNITY',		1).
valid_abstract_section_header('AUTOLOGOUS TRANSPLANTATION',		1).
valid_abstract_section_header('AUTOLOGUS TISSUE TRANSFER',		1).
valid_abstract_section_header('AUTOMATIC DEVICES',		1).
valid_abstract_section_header('AUTOPSY FINDINGS',		1).
valid_abstract_section_header('AUTOPTIC DIAGNOSIS',		1).
valid_abstract_section_header('AV NODE ABLATION AND MODULATION',		1).
valid_abstract_section_header('AVAILABILITY AND',		1).
valid_abstract_section_header('AVAILABILITY AND AND IMPLEMENTATION',		1).
valid_abstract_section_header('AVAILABILITY AND REQUIREMENTS',		1).
valid_abstract_section_header('AVAILABLE CHEMOPROTECTORS',		1).
valid_abstract_section_header('AVAILABLE EVIDENCE',		1).
valid_abstract_section_header('AVAILABLE MODELS',		1).
valid_abstract_section_header('AVAILABLE STUDIES',		1).
valid_abstract_section_header('AVAILIABILITY',		1).
valid_abstract_section_header('AVERAGE LENGTH OF STAY IN HOSPITAL',		1).
valid_abstract_section_header('AXILLARY HYPERHIDROSIS',		1).
valid_abstract_section_header('Aims',		1).
valid_abstract_section_header('Analysis',		1).
valid_abstract_section_header('B POSTTREATMENT DOCUMENTS',		1).
valid_abstract_section_header('B) PERSONNEL RESOURCES',		1).
valid_abstract_section_header('B) RESULTS',		1).
valid_abstract_section_header('BAACKGROUND',		1).
valid_abstract_section_header('BACKBROUND',		1).
valid_abstract_section_header('BACKBROUND AND PURPOSE',		1).
valid_abstract_section_header('BACKGKROUND',		1).
valid_abstract_section_header('BACKGORUND AND OBJECTIVES',		1).
valid_abstract_section_header('BACKGOUND/OBJECTIVES',		1).
valid_abstract_section_header('BACKGROOUND',		1).
valid_abstract_section_header('BACKGROUD & AIMS',		1).
valid_abstract_section_header('BACKGROUD AND AIMS',		1).
valid_abstract_section_header('BACKGROUD AND OBJECTIVE',		1).
valid_abstract_section_header('BACKGROUD AND STUDY AIM',		1).
valid_abstract_section_header('BACKGROUD/AIM',		1).
valid_abstract_section_header('BACKGROUD/OBJECTIVES',		1).
valid_abstract_section_header('BACKGROUN',		1).
valid_abstract_section_header('BACKGROUND & AIMS METHODS',		1).
valid_abstract_section_header('BACKGROUND & MATERIALS AND METHODS',		1).
valid_abstract_section_header('BACKGROUND & PATIENT',		1).
valid_abstract_section_header('BACKGROUND & PROPOSE',		1).
valid_abstract_section_header('BACKGROUND &/AIMS',		1).
valid_abstract_section_header('BACKGROUND &OBJECTIVE',		1).
valid_abstract_section_header('BACKGROUND &OBJECTIVES',		1).
valid_abstract_section_header('BACKGROUND (OR PURPOSE)',		1).
valid_abstract_section_header('BACKGROUND -',		1).
valid_abstract_section_header('BACKGROUND / OBJECTIVES',		1).
valid_abstract_section_header('BACKGROUND / PURPOSE',		1).
valid_abstract_section_header('BACKGROUND /OBJECTIVES',		1).
valid_abstract_section_header('BACKGROUND AN AIMS',		1).
valid_abstract_section_header('BACKGROUND AND AIMS OF STUDY',		1).
valid_abstract_section_header('BACKGROUND AND CASE PRESENTATION',		1).
valid_abstract_section_header('BACKGROUND AND DEVELOPMENT',		1).
valid_abstract_section_header('BACKGROUND AND GOAL OF STUDY',		1).
valid_abstract_section_header('BACKGROUND AND IMPORTANCE:',		1).
valid_abstract_section_header('BACKGROUND AND INCIDENCE',		1).
valid_abstract_section_header('BACKGROUND AND KEY ISSUES',		1).
valid_abstract_section_header('BACKGROUND AND MOTIVATIONS',		1).
valid_abstract_section_header('BACKGROUND AND NEED FOR THE STUDY',		1).
valid_abstract_section_header('BACKGROUND AND OBJECTIVE(S)',		1).
valid_abstract_section_header('BACKGROUND AND OBJECTS',		1).
valid_abstract_section_header('BACKGROUND AND OBJESTIVES',		1).
valid_abstract_section_header('BACKGROUND AND OBJETIVES',		1).
valid_abstract_section_header('BACKGROUND AND QUESTION',		1).
valid_abstract_section_header('BACKGROUND AND STUDY HYPOTHESIS',		1).
valid_abstract_section_header('BACKGROUND AND STUDY OBJECT',		1).
valid_abstract_section_header('BACKGROUND AND SUMMARY',		1).
valid_abstract_section_header('BACKGROUND AND THE STUDY AIM',		1).
valid_abstract_section_header('BACKGROUND AND TROPHIC DIVERSITY STUDY',		1).
valid_abstract_section_header('BACKGROUND AND VIEWPOINT',		1).
valid_abstract_section_header('BACKGROUND CANCER SIGNIFICANCE AND QUESTION',		1).
valid_abstract_section_header('BACKGROUND DATA DISCUSSING THE PRESENT STATUS OF THE FIELD',		1).
valid_abstract_section_header('BACKGROUND E SCOPI',		1).
valid_abstract_section_header('BACKGROUND GOALS',		1).
valid_abstract_section_header('BACKGROUND MATERIAL',		1).
valid_abstract_section_header('BACKGROUND METHODS',		1).
valid_abstract_section_header('BACKGROUND NAD(P)H',		1).
valid_abstract_section_header('BACKGROUND OBJECTIVE',		1).
valid_abstract_section_header('BACKGROUND OF CONTEXT',		1).
valid_abstract_section_header('BACKGROUND OF STUDY',		1).
valid_abstract_section_header('BACKGROUND OF THE HYPOTHESIS',		1).
valid_abstract_section_header('BACKGROUND OR STATEMENT OF PROBLEM',		1).
valid_abstract_section_header('BACKGROUND RATIONALE',		1).
valid_abstract_section_header('BACKGROUND RESULTS',		1).
valid_abstract_section_header('BACKGROUND SETTING AND DESIGN',		1).
valid_abstract_section_header('BACKGROUND STUDY DESIGN/PATIENTS',		1).
valid_abstract_section_header('BACKGROUND THEORY AND EVIDENCE',		1).
valid_abstract_section_header('BACKGROUND TO METHOD DEVELOPMENT',		1).
valid_abstract_section_header('BACKGROUND&PROBLEMS',		1).
valid_abstract_section_header('BACKGROUND, AIMS & METHODS',		1).
valid_abstract_section_header('BACKGROUND, AIMS AND DESIGN',		1).
valid_abstract_section_header('BACKGROUND, OBJECTIVE',		1).
valid_abstract_section_header('BACKGROUND, OBJECTIVES AND METHODS',		1).
valid_abstract_section_header('BACKGROUND, OBJECTIVES, DESIGN',		1).
valid_abstract_section_header('BACKGROUND-',		1).
valid_abstract_section_header('BACKGROUND-AIMS',		1).
valid_abstract_section_header('BACKGROUND-OBJECTIVES',		1).
valid_abstract_section_header('BACKGROUND.',		1).
valid_abstract_section_header('BACKGROUND/ OBJECTIVE',		1).
valid_abstract_section_header('BACKGROUND/ PURPOSE',		1).
valid_abstract_section_header('BACKGROUND/AIM OF THE STUDY',		1).
valid_abstract_section_header('BACKGROUND/IMPORTANCE',		1).
valid_abstract_section_header('BACKGROUND/INTRODUCTION TO CASE',		1).
valid_abstract_section_header('BACKGROUND/NEED OF STUDY',		1).
valid_abstract_section_header('BACKGROUND/OBJECFTIVES',		1).
valid_abstract_section_header('BACKGROUND/OBJECTS',		1).
valid_abstract_section_header('BACKGROUND/QUESTION',		1).
valid_abstract_section_header('BACKGROUND/SYNOPSIS',		1).
valid_abstract_section_header('BACKGROUNDCONTEXT',		1).
valid_abstract_section_header('BACKGROUNDG',		1).
valid_abstract_section_header('BACKGROUNDS, MATERIAL AND METHODS',		1).
valid_abstract_section_header('BACKGROUNDS/AIM',		1).
valid_abstract_section_header('BACKGROUNDS/OBJECTOVES',		1).
valid_abstract_section_header('BACKGROUNG & AIMS',		1).
valid_abstract_section_header('BACKGROUNG AND OBJECTIVES',		1).
valid_abstract_section_header('BACKGROUNG/OBJECTIVES',		1).
valid_abstract_section_header('BACKGROUP AND PURPOSE',		1).
valid_abstract_section_header('BACKGRUOND',		1).
valid_abstract_section_header('BACKROUND AND AIM',		1).
valid_abstract_section_header('BACKROUND AND PURPOSE',		1).
valid_abstract_section_header('BACKROUND/OBJECTIVE',		1).
valid_abstract_section_header('BACTERIAL BIOFILMS ARE BECOMING A SIGNIFICANT SOCIETAL PROBLEM',		1).
valid_abstract_section_header('BACTERIAL INFLAMMATION IN ACUTE RHINOSINUSITIS',		1).
valid_abstract_section_header('BACTERIOLOGICAL PROOF',		1).
valid_abstract_section_header('BAKCGROUND',		1).
valid_abstract_section_header('BAKGROUND/OBJECTIVES',		1).
valid_abstract_section_header('BALANCE',		1).
valid_abstract_section_header('BALANCING THE HEALTH CARE SCORECARDS',		1).
valid_abstract_section_header('BALDNESS',		1).
valid_abstract_section_header('BALNEOTHERAPY',		1).
valid_abstract_section_header('BANTAO',		1).
valid_abstract_section_header('BARCKGROUND AND OBJECTIVE',		1).
valid_abstract_section_header('BARRIERS OR ISSUES AND POTENTIAL SOLUTIONS',		1).
valid_abstract_section_header('BARRIERS TO IMPROVEMENT',		1).
valid_abstract_section_header('BARRIERS TO SCALING UP INTERVENTIONS',		1).
valid_abstract_section_header('BASAL INSULIN ANALOGUES',		1).
valid_abstract_section_header('BASCKGROUND/AIM',		1).
valid_abstract_section_header('BASE CASE ANALYSIS',		1).
valid_abstract_section_header('BASELINE POPULATION',		1).
valid_abstract_section_header('BASES',		1).
valid_abstract_section_header('BASIC COMMUNICATION COMPONENTS AND PROCESSES',		1).
valid_abstract_section_header('BASIC CONCEPTS',		1).
valid_abstract_section_header('BASIC DATA AND PATIENTS',		1).
valid_abstract_section_header('BASIC ELEMENTS',		1).
valid_abstract_section_header('BASIC PATHOLOGIES',		1).
valid_abstract_section_header('BASIC PROBLEM AND AIM OF STUDY',		1).
valid_abstract_section_header('BASIC PROBLEM AND OBJECTIVE OF THE STUDY',		1).
valid_abstract_section_header('BASIC PROBLEMS AND OBJECTIVE',		1).
valid_abstract_section_header('BASIC REGIMENS',		1).
valid_abstract_section_header('BASIC STATEMENTS',		1).
valid_abstract_section_header('BASIC STUDIES',		1).
valid_abstract_section_header('BASIC/CLINICAL SCIENCE ADDRESSED',		1).
valid_abstract_section_header('BASIS AND PURPOSE',		1).
valid_abstract_section_header('BASIS FOR ANTIBIOTIC PROPHYLAXIS',		1).
valid_abstract_section_header('BASIS PROCEDURE',		1).
valid_abstract_section_header('BCKGROUND',		1).
valid_abstract_section_header('BDNF METHODS',		1).
valid_abstract_section_header('BEHAVIOR UNDER EXTERNAL LOADS',		1).
valid_abstract_section_header('BEHAVIORAL ANALYSIS',		1).
valid_abstract_section_header('BEHAVIORAL PHYSIOTHERAPY',		1).
valid_abstract_section_header('BENCHMARKING AND REENGINEERING',		1).
valid_abstract_section_header('BENEFITS AND CHALLENGES',		1).
valid_abstract_section_header('BENEFITS AND EXPECTATIONS',		1).
valid_abstract_section_header('BENEFITS AND RISKS',		1).
valid_abstract_section_header('BENEFITS COSTS AND HARMS',		1).
valid_abstract_section_header('BENEFITS OF A COMMUNITY HEALTH IMPROVEMENT COALITION',		1).
valid_abstract_section_header('BENEFITS TO THE COMMUNITY',		1).
valid_abstract_section_header('BENEFITS, COSTS AND LIMITATIONS',		1).
valid_abstract_section_header('BENEFITS, HARMS AND/OR COSTS',		1).
valid_abstract_section_header('BENEFITS, HARMS, AND COST',		1).
valid_abstract_section_header('BENEFITS/ANTICIPATED OUTCOMES',		1).
valid_abstract_section_header('BENIGN LESIONS',		1).
valid_abstract_section_header('BEST PRACTICE ADVICE',		1).
valid_abstract_section_header('BEST PRACTICE ADVICE 7',		1).
valid_abstract_section_header('BEST PRACTICE METHODOLOGY',		1).
valid_abstract_section_header('BEST PRACTICES AND TIPS FOR DEBRIEFING TEAMS',		1).
valid_abstract_section_header('BEST PRACTICES FOR ADVANCED SERVICE RECOVERY',		1).
valid_abstract_section_header('BEST PRACTICES FOR BASIC SERVICE RECOVERY',		1).
valid_abstract_section_header('BEST SUPPORTIVE CARE',		1).
valid_abstract_section_header('BETA BLOCKERS',		1).
valid_abstract_section_header('BETWEEN FINDINGS',		1).
valid_abstract_section_header('BEVACIZUMAB',		1).
valid_abstract_section_header('BIAS, CONFOUNDING FACTORS, DRAWBACKS',		1).
valid_abstract_section_header('BIASES/DISCLOSURES',		1).
valid_abstract_section_header('BIBLIOGRAPHIC RETRIEVAL METHOD',		1).
valid_abstract_section_header('BIBLIOGRAPHY WITH ANNOTATIONS',		1).
valid_abstract_section_header('BIFURCATION',		1).
valid_abstract_section_header('BILATERAL CLEFT LIP',		1).
valid_abstract_section_header('BILE ACIDS',		1).
valid_abstract_section_header('BILE DUCTS INJURIES',		1).
valid_abstract_section_header('BILIARY CARCINOGENESIS',		1).
valid_abstract_section_header('BIOASSAYS',		1).
valid_abstract_section_header('BIOCHAR CHARACTERISTICS',		1).
valid_abstract_section_header('BIOCHEMICAL CHARACTERISTICS',		1).
valid_abstract_section_header('BIOCHEMICAL INVESTIGATION',		1).
valid_abstract_section_header('BIOCHEMICAL MARKERS IN ACUTE CORONARY SYNDROMES',		1).
valid_abstract_section_header('BIOCHEMICAL MARKERS IN STABLE ATHEROSCLEROTIC DISEASE',		1).
valid_abstract_section_header('BIOGRAPHY IN BRIEF',		1).
valid_abstract_section_header('BIOLOGICAL ACTIVITIES AND ETHNOPHARMACOLOGICAL APPRAISAL',		1).
valid_abstract_section_header('BIOLOGICAL AND PSYCHOLOGICAL IMPLICATIONS',		1).
valid_abstract_section_header('BIOLOGICAL APPROACHES',		1).
valid_abstract_section_header('BIOLOGICAL ARGUMENTS',		1).
valid_abstract_section_header('BIOLOGICAL FUNCTIONS',		1).
valid_abstract_section_header('BIOLOGICAL ISSUE',		1).
valid_abstract_section_header('BIOLOGICAL STUDIES',		1).
valid_abstract_section_header('BIOLOGICAL THEORIES',		1).
valid_abstract_section_header('BIOLOGICAL WEAPONS',		1).
valid_abstract_section_header('BIOLOGY',		1).
valid_abstract_section_header('BIOLOGY AND REPRODUCTION',		1).
valid_abstract_section_header('BIOMARKER INSIGHTS',		1).
valid_abstract_section_header('BIOMECHANICAL ADAPTATION OF THE SPINE IN PATHOLOGY',		1).
valid_abstract_section_header('BIOMECHANICAL SOLUTION',		1).
valid_abstract_section_header('BIOVIGILANCE',		1).
valid_abstract_section_header('BLASTOCYSTIS',		1).
valid_abstract_section_header('BLOOD PRESSURE AMBULATORY MONITORING',		1).
valid_abstract_section_header('BLOOD TYPE',		1).
valid_abstract_section_header('BLOW FLIES (DIPTERA',		1).
valid_abstract_section_header('BMI FOR ALS PATIENTS',		1).
valid_abstract_section_header('BMI FOR STROKE MOTOR RECOVERY',		1).
valid_abstract_section_header('BOLSTER THE NURSING EDUCATIONAL INFRASTRUCTURE',		1).
valid_abstract_section_header('BONE FRACTURES IN PATIENTS WITH DIABETIC DISEASE',		1).
valid_abstract_section_header('BONE LESIONS',		1).
valid_abstract_section_header('BONE LOSS',		1).
valid_abstract_section_header('BONE MINERAL DENSITY TESTING AND TREATMENT AFTER A FRAGILITY FRACTURE',		1).
valid_abstract_section_header('BONE MORPHOGENETIC PROTEINS',		1).
valid_abstract_section_header('BOOK DETAILS',		1).
valid_abstract_section_header('BOOK REVIEW AND DISCUSSION',		1).
valid_abstract_section_header('BOOLEAN NETWORKS (OR',		1).
valid_abstract_section_header('BORON DELIVERY AGENTS',		1).
valid_abstract_section_header('BOTULINUM TOXIN TYPE A',		1).
valid_abstract_section_header('BRAIN AND OXIDATIVE STRESS',		1).
valid_abstract_section_header('BRAIN STRUCTURES',		1).
valid_abstract_section_header('BRAINS',		1).
valid_abstract_section_header('BRIEF REPORT',		1).
valid_abstract_section_header('BROADER CO-MANAGEMENT',		1).
valid_abstract_section_header('BRONCHIAL INVOLVEMENT',		1).
valid_abstract_section_header('BUILDER',		1).
valid_abstract_section_header('BUILDING A DATABASE PROTOTYPE',		1).
valid_abstract_section_header('BUILDING A GOVERNANCE FRAMEWORK BASED IN TRUST AVOIDING SURPRISES',		1).
valid_abstract_section_header('BUILDING A SYSTEM',		1).
valid_abstract_section_header('BUILDING ON SUCCESS AND LESSONS LEARNED',		1).
valid_abstract_section_header('BUILDING THE AGENDA FOR CHANGE',		1).
valid_abstract_section_header('BUILDING TOWARDS A SOLUTION',		1).
valid_abstract_section_header('BUSINESS CASE FRAMEWORK',		1).
valid_abstract_section_header('BUSINESS MODELLING',		1).
valid_abstract_section_header('BUT',		1).
valid_abstract_section_header('BVD IN AUSTRALIA',		1).
valid_abstract_section_header('BW, ADG, F',		1).
valid_abstract_section_header('BY TYPE OF DIABETES',		1).
valid_abstract_section_header('BYPASS SURGERY',		1).
valid_abstract_section_header('C ASPI',		1).
valid_abstract_section_header('C DOCUMENTS AT END OF RETENTION',		1).
valid_abstract_section_header('C POSTRETENTION RECORDS',		1).
valid_abstract_section_header('CADE REPORT',		1).
valid_abstract_section_header('CADRE DE LA REVUE',		1).
valid_abstract_section_header('CALCANEAL APOPHYSIS',		1).
valid_abstract_section_header('CALCIUM ANTAGONISTS AND THE ENDOTHELIUM',		1).
valid_abstract_section_header('CALCIUM ANTAGONISTS IN HEART FAILURE',		1).
valid_abstract_section_header('CALCULATION OF LIMB LENGTH DISCREPANCY',		1).
valid_abstract_section_header('CALCULATION STRATEGY',		1).
valid_abstract_section_header('CALCULATIONS AND THEORY',		1).
valid_abstract_section_header('CALIBRATED SYRINGES AFTER PRAVAZ',		1).
valid_abstract_section_header('CAMBIOS PERTINENTES',		1).
valid_abstract_section_header('CAMBIOS RELEVANTES',		1).
valid_abstract_section_header('CANCER CARE AS A SYSTEM',		1).
valid_abstract_section_header('CANCER CONDITION IN WHICH REHABILITATION OF OLDER INDIVIDUALS MAY BE NEEDED',		1).
valid_abstract_section_header('CANNULATION STRATEGY',		1).
valid_abstract_section_header('CAP18 SHOWS A HIGH BROAD SPECTRUM ANTIMICROBIAL ACTIVITY',		1).
valid_abstract_section_header('CAPTURE ABSTRACT',		1).
valid_abstract_section_header('CARBIDE FINISHING BUR',		1).
valid_abstract_section_header('CARDIAC AND VASCULAR ADRENOCEPTOR FUNCTION',		1).
valid_abstract_section_header('CARDIAC ARRHYTHMIAS IN WOMEN',		1).
valid_abstract_section_header('CARDIAC CYTOKINES',		1).
valid_abstract_section_header('CARDIAC DISEASES',		1).
valid_abstract_section_header('CARDIAC FAILURE',		1).
valid_abstract_section_header('CARDIAC MRI',		1).
valid_abstract_section_header('CARDIOVASCULAR DRUGS',		1).
valid_abstract_section_header('CARDIOVASCULAR EFFECT',		1).
valid_abstract_section_header('CARDIOVASCULAR EFFECTS AND RISK OF SEXUAL ACTIVITY',		1).
valid_abstract_section_header('CARDIOVASCULAR IMPACT',		1).
valid_abstract_section_header('CARE PLAN',		1).
valid_abstract_section_header('CARE PRESENTATION',		1).
valid_abstract_section_header('CAREGIVING SUPPORT AND EDUCATION',		1).
valid_abstract_section_header('CARNITINE SUPPLEMENTATION',		1).
valid_abstract_section_header('CASA DESCRIPTION',		1).
valid_abstract_section_header('CASE 3',		1).
valid_abstract_section_header('CASE AND CONCLUSION',		1).
valid_abstract_section_header('CASE AND LABORATORY DATA',		1).
valid_abstract_section_header('CASE BY CASE',		1).
valid_abstract_section_header('CASE CAPSULE',		1).
valid_abstract_section_header('CASE COMMENTARY',		1).
valid_abstract_section_header('CASE DATA',		1).
valid_abstract_section_header('CASE DATA AND DISCUSSION',		1).
valid_abstract_section_header('CASE DECSRIPTION',		1).
valid_abstract_section_header('CASE DEMONSTRATION',		1).
valid_abstract_section_header('CASE DESCIPTION',		1).
valid_abstract_section_header('CASE DESCRIPTION AND CONCLUSION',		1).
valid_abstract_section_header('CASE DESCRIPTION AND CONCLUSIONS',		1).
valid_abstract_section_header('CASE DESCRIPTION AND INTERVENTION',		1).
valid_abstract_section_header('CASE DESCRIPTION AND LITERATURE REVIEW',		1).
valid_abstract_section_header('CASE DESCRIPTION AND THE LITERATURE REVIEW',		1).
valid_abstract_section_header('CASE DESCRIPTION/LITERATURE REVIEW',		1).
valid_abstract_section_header('CASE DESCRIPTION/OPERATIVE TECHNIQUE',		1).
valid_abstract_section_header('CASE DESCRIPTION:',		1).
valid_abstract_section_header('CASE DESCRIPTIONS AND VARIATION AMONG SITES',		1).
valid_abstract_section_header('CASE DIAGNOSIS AND TREATMENTS',		1).
valid_abstract_section_header('CASE EXAMPLE',		1).
valid_abstract_section_header('CASE FINDING',		1).
valid_abstract_section_header('CASE FINDINGS/PATIENT CONCERNS',		1).
valid_abstract_section_header('CASE HISTORY AND DISCUSSION',		1).
valid_abstract_section_header('CASE HISTORY AND RESULTS',		1).
valid_abstract_section_header('CASE HISTORY/RESULTS',		1).
valid_abstract_section_header('CASE IDENTIFICATION AND DETAILS',		1).
valid_abstract_section_header('CASE ILLUSTRATIONS',		1).
valid_abstract_section_header('CASE IMPLICATIONS',		1).
valid_abstract_section_header('CASE MANAGEMENT AND OUTCOME',		1).
valid_abstract_section_header('CASE MANAGEMENT AND OUTCOMES',		1).
valid_abstract_section_header('CASE PREPARATION',		1).
valid_abstract_section_header('CASE PRESENT',		1).
valid_abstract_section_header('CASE PRESENTANTION',		1).
valid_abstract_section_header('CASE PRESENTATION & METHODS',		1).
valid_abstract_section_header('CASE PRESENTATION AND FURTHER CLINICAL COURSE',		1).
valid_abstract_section_header('CASE PRESENTATION AND METHODS',		1).
valid_abstract_section_header('CASE PRESENTATION AND RESULTS',		1).
valid_abstract_section_header('CASE PRESENTATION AND REVIEW',		1).
valid_abstract_section_header('CASE PRESENTATION AND SUMMARY',		1).
valid_abstract_section_header('CASE PRESENTATIONS AND INTERVENTION',		1).
valid_abstract_section_header('CASE PRESENTATIONS AND MANAGEMENT',		1).
valid_abstract_section_header('CASE REPORT & RESULTS',		1).
valid_abstract_section_header('CASE REPORT 2',		1).
valid_abstract_section_header('CASE REPORT AND FINDINGS',		1).
valid_abstract_section_header('CASE REPORT AND GENETIC FINDINGS',		1).
valid_abstract_section_header('CASE REPORT AND INTERVENTION',		1).
valid_abstract_section_header('CASE REPORT AND MANAGEMENT',		1).
valid_abstract_section_header('CASE REPORT AND METHODS FOR LITERATURE REVIEW',		1).
valid_abstract_section_header('CASE REPORT AND PROCEDURE',		1).
valid_abstract_section_header('CASE REPORT AND REVIEW OF THE LITERATURE',		1).
valid_abstract_section_header('CASE REPORT AND REVIEW SUMMARY',		1).
valid_abstract_section_header('CASE REPORT AND STUDY',		1).
valid_abstract_section_header('CASE REPORT AND STUDY DESIGN',		1).
valid_abstract_section_header('CASE REPORT AND SURGICAL TECHNIQUE',		1).
valid_abstract_section_header('CASE REPORT AND THERAPY',		1).
valid_abstract_section_header('CASE REPORT I',		1).
valid_abstract_section_header('CASE REPORT II',		1).
valid_abstract_section_header('CASE REPORT SUMMARY',		1).
valid_abstract_section_header('CASE REPORT/OBJECTIVES',		1).
valid_abstract_section_header('CASE REPORT/TECHNIQUE DESCRIPTION',		1).
valid_abstract_section_header('CASE REPORT/TECHNIQUE PRESENTATION',		1).
valid_abstract_section_header('CASE REPORTS (AND LITERATURE REVIEW)',		1).
valid_abstract_section_header('CASE REPORTS (FIGURE)',		1).
valid_abstract_section_header('CASE REPORTS AND PATHOLOGIC FINDINGS',		1).
valid_abstract_section_header('CASE REPORTS AND RESULTS',		1).
valid_abstract_section_header('CASE REPORTS CONCLUSION',		1).
valid_abstract_section_header('CASE REPORTS FIRST',		1).
valid_abstract_section_header('CASE SCENARIO',		1).
valid_abstract_section_header('CASE SERIES DESCRIPTION',		1).
valid_abstract_section_header('CASE SERIES STUDY',		1).
valid_abstract_section_header('CASE STUDIES AND DISCUSSION',		1).
valid_abstract_section_header('CASE STUDY 2',		1).
valid_abstract_section_header('CASE STUDY AND DISCUSSION',		1).
valid_abstract_section_header('CASE STUDY EXAMPLE',		1).
valid_abstract_section_header('CASE SUBJECT AND METHODS',		1).
valid_abstract_section_header('CASE SUMMARY AND WHAT IS NEW',		1).
valid_abstract_section_header('CASE SUMMARY/RESULTS',		1).
valid_abstract_section_header('CASE SYNOPSIS',		1).
valid_abstract_section_header('CASE(S) PRESENTATION',		1).
valid_abstract_section_header('CASE-DESCRIPTION',		1).
valid_abstract_section_header('CASE/INTERVENTION',		1).
valid_abstract_section_header('CASE/METHODS',		1).
valid_abstract_section_header('CASE/OBSERVATIONS',		1).
valid_abstract_section_header('CASEREPORT',		1).
valid_abstract_section_header('CASES & METHODS',		1).
valid_abstract_section_header('CASES AND DISCUSSION',		1).
valid_abstract_section_header('CASES AND KINDREDS',		1).
valid_abstract_section_header('CASES AND SURGICAL TECHNIQUE',		1).
valid_abstract_section_header('CASES AND THERAPY',		1).
valid_abstract_section_header('CASES DESCRIPTIONS',		1).
valid_abstract_section_header('CASES MANAGEMENT',		1).
valid_abstract_section_header('CASES OUTCOME',		1).
valid_abstract_section_header('CASES OUTLINES',		1).
valid_abstract_section_header('CASES REPORTED',		1).
valid_abstract_section_header('CASES SERIES',		1).
valid_abstract_section_header('CASES(S)',		1).
valid_abstract_section_header('CASI CLINICI',		1).
valid_abstract_section_header('CASR REPORT',		1).
valid_abstract_section_header('CASUALTY EVACUATION',		1).
valid_abstract_section_header('CASUISTIC & METHODS',		1).
valid_abstract_section_header('CASUISTICS AND METHOD',		1).
valid_abstract_section_header('CASUS',		1).
valid_abstract_section_header('CATECHOLAMINES IN THE HUMAN HYPOTHALAMUS',		1).
valid_abstract_section_header('CATEGORY',		1).
valid_abstract_section_header('CATHETERIZATION FACILITIES',		1).
valid_abstract_section_header('CAUSAL CRITERIA',		1).
valid_abstract_section_header('CAUSE AND THERAPY',		1).
valid_abstract_section_header('CAUSE OR CONSEQUENCE',		1).
valid_abstract_section_header('CAUSES AND DIAGNOSIS',		1).
valid_abstract_section_header('CAUSES OF FEMALE VOIDING DYSFUNCTION',		1).
valid_abstract_section_header('CAUSES OF HYPERHOMOCYSTEINEMIA',		1).
valid_abstract_section_header('CAUSES OF IMPAIRED FRACTURE HEALING',		1).
valid_abstract_section_header('CAUSES OF PAIN',		1).
valid_abstract_section_header('CAUSES OF SUDDEN CARDIAC DEATH',		1).
valid_abstract_section_header('CAUTIONARY NOTE',		1).
valid_abstract_section_header('CAUTIONARY REMARK',		1).
valid_abstract_section_header('CBCHP',		1).
valid_abstract_section_header('CBZ-CR',		1).
valid_abstract_section_header('CC BUA',		1).
valid_abstract_section_header('CC/CG',		1).
valid_abstract_section_header('CCO RECOMMENDATIONS',		1).
valid_abstract_section_header('CEL BADANIA',		1).
valid_abstract_section_header('CELE',		1).
valid_abstract_section_header('CELEM',		1).
valid_abstract_section_header('CELEM BADANIA',		1).
valid_abstract_section_header('CELEM NINIEJSZEJ PRACY',		1).
valid_abstract_section_header('CELL DEATH AND AVAILABLE ASSAYS',		1).
valid_abstract_section_header('CELL DEATH MARKERS AND IMAGING MODALITIES',		1).
valid_abstract_section_header('CELL LINE',		1).
valid_abstract_section_header('CELLS',		1).
valid_abstract_section_header('CELLULAR AND MOLECULAR MECHANISMS',		1).
valid_abstract_section_header('CELLULAR VIABILITY',		1).
valid_abstract_section_header('CENTER FOR AIDS PREVENTION STUDIES (CAPS) MODEL OF INTERNATIONAL COLLABORATIVE RESEARCH',		1).
valid_abstract_section_header('CENTRAL MASSAGE',		1).
valid_abstract_section_header('CENTRALISED OPHTHALMIC ELECTRONIC REFERRAL UNIT',		1).
valid_abstract_section_header('CEPHALOMETRICAL RESEARCH',		1).
valid_abstract_section_header('CERAPP',		1).
valid_abstract_section_header('CEREBROSPINAL FLUID',		1).
valid_abstract_section_header('CEREBROVASCULAR DISEASES',		1).
valid_abstract_section_header('CERVICAL CANCER',		1).
valid_abstract_section_header('CERVICAL SYNDROME THERAPY',		1).
valid_abstract_section_header('CFT-VFT',		1).
valid_abstract_section_header('CHAIN OF SURVIVAL',		1).
valid_abstract_section_header('CHALLENGES AND COMPLEXITIES IN TEAM PERFORMANCE MEASUREMENT',		1).
valid_abstract_section_header('CHALLENGES AND CURRENT CHOICES',		1).
valid_abstract_section_header('CHALLENGES AND DEVELOPMENTS',		1).
valid_abstract_section_header('CHALLENGES AND NEXT STEPS',		1).
valid_abstract_section_header('CHALLENGES AND THE FUTURE',		1).
valid_abstract_section_header('CHALLENGES IN SWEDISH HEALTH CARE',		1).
valid_abstract_section_header('CHALLENGES TO CONCLUSIVELY PROVE THE HYPOTHESES',		1).
valid_abstract_section_header('CHANGE',		1).
valid_abstract_section_header('CHANGE PROCESS',		1).
valid_abstract_section_header('CHANGEMENT SIGNIFICATIFS',		1).
valid_abstract_section_header('CHANGEMENTS PERTINENTS',		1).
valid_abstract_section_header('CHANGES IN FOREST STRUCTURE',		1).
valid_abstract_section_header('CHANGES OF LIFESTYLE',		1).
valid_abstract_section_header('CHANGING OF THE RECEPTORS IN THE FAILING HEART',		1).
valid_abstract_section_header('CHAPTERS/SECTIONS',		1).
valid_abstract_section_header('CHARACTERISTICS AND SEARCH PROCEDURES',		1).
valid_abstract_section_header('CHARACTERISTICS OF ACCREDITATION',		1).
valid_abstract_section_header('CHARACTERISTICS OF CONTINGENCY PLANS',		1).
valid_abstract_section_header('CHARACTERISTICS OF DAILY TRIPS TAKEN BY OLDER ADULTS',		1).
valid_abstract_section_header('CHARACTERISTICS OF DIRS-1 MEDIATED KNOCK-DOWNS',		1).
valid_abstract_section_header('CHARACTERISTICS OF PLANT TRANSGLUTAMINASES',		1).
valid_abstract_section_header('CHARACTERISTICS OF THE CHILDREN',		1).
valid_abstract_section_header('CHARACTERISTICS OF THE MECHANISM',		1).
valid_abstract_section_header('CHARACTERISTICS, COMPLICATIONS AND THERAPY',		1).
valid_abstract_section_header('CHARACTERIZATION METHODS',		1).
valid_abstract_section_header('CHARACTERIZATION OF ANALGETICS AND SEDATIVES',		1).
valid_abstract_section_header('CHECKLISTS',		1).
valid_abstract_section_header('CHEMICAL ANALYSIS',		1).
valid_abstract_section_header('CHEMICAL COMPOUNDS ISOLATED IN THIS ARTICLE',		1).
valid_abstract_section_header('CHEMICAL CONSTITUENTS',		1).
valid_abstract_section_header('CHEMICALS',		1).
valid_abstract_section_header('CHEMOSENSITIVITY',		1).
valid_abstract_section_header('CHEMOSENSORY TESTING',		1).
valid_abstract_section_header('CHEMOTHERAPY AT RECURRENCE',		1).
valid_abstract_section_header('CHEMOTHERAPY FOR ADVANCED GASTRIC CANCER',		1).
valid_abstract_section_header('CHILDHOOD AND EDUCATION',		1).
valid_abstract_section_header('CHILDHOOD ONSET GENERALIZED DYSTONIA',		1).
valid_abstract_section_header('CHLAMYDIA AND OCULAR ADNEXAL LYMPHOMAS',		1).
valid_abstract_section_header('CHNAS AND EHRS FOR POPULATION HEALTH',		1).
valid_abstract_section_header('CHOICE AND PROVISION OF DEVICES',		1).
valid_abstract_section_header('CHOICE CRITERIA',		1).
valid_abstract_section_header('CHOICE OF A SOLUTION',		1).
valid_abstract_section_header('CHOICE OF DRUG TO INITIATE TREATMENT',		1).
valid_abstract_section_header('CHOICE OF DRUGS AND PROCEDURE',		1).
valid_abstract_section_header('CHOICE OF EMPIRICAL ANTIMICROBIAL THERAPY',		1).
valid_abstract_section_header('CHOICE OF MEASURING INSTRUMENTS',		1).
valid_abstract_section_header('CHOICE OF SERA FOR CMX',		1).
valid_abstract_section_header('CHOICE OF SURGICAL PROCEDURE',		1).
valid_abstract_section_header('CHOICE OF THE ASSIST SYSTEM',		1).
valid_abstract_section_header('CHOICE OF THE DEVICE AND PARAMETERS',		1).
valid_abstract_section_header('CHOICE OF THE OPERATIVE APPROACH',		1).
valid_abstract_section_header('CHOICE OF THERAPY',		1).
valid_abstract_section_header('CHOICE THEORY',		1).
valid_abstract_section_header('CHOLANGIOCARCINOMAS',		1).
valid_abstract_section_header('CHOLESTEROL AND DEPRESSION',		1).
valid_abstract_section_header('CHOLESTEROL AND SUICIDAL BEHAVIOR',		1).
valid_abstract_section_header('CHOOSING COMPUTER SOFTWARE',		1).
valid_abstract_section_header('CHOOSING THE CORRECT CONTROL CHART',		1).
valid_abstract_section_header('CHRONIC COURSE',		1).
valid_abstract_section_header('CHRONIC DISEASES IN ADOLESENCE',		1).
valid_abstract_section_header('CHRONIC RENAL FAILURE',		1).
valid_abstract_section_header('CHRONIC WOUND CARE',		1).
valid_abstract_section_header('CHRONOLOGY AND INVESTIGATIONS',		1).
valid_abstract_section_header('CILJI',		1).
valid_abstract_section_header('CINICALTRIALSGOV',		1).
valid_abstract_section_header('CIRCULATING TOXINS NAMELY',		1).
valid_abstract_section_header('CLASSICAL OPERATIVE PROCEDURES',		1).
valid_abstract_section_header('CLASSIFICATION AND THERAPY OF CVI',		1).
valid_abstract_section_header('CLASSIFICATION AND THERAPY OF NOSE INJURIES',		1).
valid_abstract_section_header('CLASSIFICATION CODES',		1).
valid_abstract_section_header('CLASSIFICATION OF ABDOMINAL AORTIC ANEURYSMS',		1).
valid_abstract_section_header('CLASSIFICATION OF DISORDERS',		1).
valid_abstract_section_header('CLASSIFICATION OF EVIDENCE REVIEW',		1).
valid_abstract_section_header('CLASSIFICATION OF MUSCULOSKELETAL DISEASES',		1).
valid_abstract_section_header('CLASSIFICATION OF STUDY DESIGN',		1).
valid_abstract_section_header('CLASSIFICATION OF THROMBOPHILIA',		1).
valid_abstract_section_header('CLASSIFICATIONS',		1).
valid_abstract_section_header('CLASSIFICATON OF EVIDENCE',		1).
valid_abstract_section_header('CLIENT COMMUNICATION',		1).
valid_abstract_section_header('CLIMATE VARIATIONS AND SUICIDE',		1).
valid_abstract_section_header('CLIMATIC FACTORS',		1).
valid_abstract_section_header('CLINCAL FINDINGS',		1).
valid_abstract_section_header('CLINCAL IMPLICATIONS',		1).
valid_abstract_section_header('CLINCAL RELEVANCE',		1).
valid_abstract_section_header('CLINCAL TRIAL REGISTRATION',		1).
valid_abstract_section_header('CLINCIAL PRESENTATION',		1).
valid_abstract_section_header('CLINCIAL RELEVANCE',		1).
valid_abstract_section_header('CLINIC PROBLEM AND CASE SERIES',		1).
valid_abstract_section_header('CLINICAL ADVANCE',		1).
valid_abstract_section_header('CLINICAL ADVANTAGES/ RECOMMENDATIONS',		1).
valid_abstract_section_header('CLINICAL AND EXPERIMENTAL DATA',		1).
valid_abstract_section_header('CLINICAL AND HISTOLOGICAL FINDINGS',		1).
valid_abstract_section_header('CLINICAL AND RADIOLOGICAL MANIFESTATIONS',		1).
valid_abstract_section_header('CLINICAL AND RELATED BASIC RESEARCH',		1).
valid_abstract_section_header('CLINICAL AND RESEARCH RECOMMENDATIONS',		1).
valid_abstract_section_header('CLINICAL APPROACH',		1).
valid_abstract_section_header('CLINICAL ARGUMENTS',		1).
valid_abstract_section_header('CLINICAL BACKGROUND',		1).
valid_abstract_section_header('CLINICAL BENCHMARKING',		1).
valid_abstract_section_header('CLINICAL CARE',		1).
valid_abstract_section_header('CLINICAL CARE AND RESEARCH',		1).
valid_abstract_section_header('CLINICAL CASE AND LASER PHOTOTHERAPY PROTOCOL',		1).
valid_abstract_section_header('CLINICAL CASE HISTORY',		1).
valid_abstract_section_header('CLINICAL CASE STUDIES',		1).
valid_abstract_section_header('CLINICAL CASE SUMMARY',		1).
valid_abstract_section_header('CLINICAL CASE/CONCLUSIONS',		1).
valid_abstract_section_header('CLINICAL CASE/DISCUSSION',		1).
valid_abstract_section_header('CLINICAL CASES AND CONCLUSION',		1).
valid_abstract_section_header('CLINICAL CLASSIFICATION',		1).
valid_abstract_section_header('CLINICAL CLASSIFICATION, PREDICTION AND TREATMENT OF PROSTATE CANCER',		1).
valid_abstract_section_header('CLINICAL COMMUNITY HEALTH AND ITS APPLICATIONS AT MOREHOUSE SCHOOL OF MEDICINE',		1).
valid_abstract_section_header('CLINICAL CONDITIONS',		1).
valid_abstract_section_header('CLINICAL CONSEQUENCE',		1).
valid_abstract_section_header('CLINICAL CONSEQUENCES FOLLOWING THE DEVELOPMENT OF TOLERANCE',		1).
valid_abstract_section_header('CLINICAL COURSE AND OUTCOME',		1).
valid_abstract_section_header('CLINICAL COURSE AND RESULT',		1).
valid_abstract_section_header('CLINICAL COURSE, PROGNOSIS AND TREATMENT',		1).
valid_abstract_section_header('CLINICAL DATA INCLUDED',		1).
valid_abstract_section_header('CLINICAL DESCRIPTION AND ETIOLOGY',		1).
valid_abstract_section_header('CLINICAL DIAGNOSTICS',		1).
valid_abstract_section_header('CLINICAL EFFICACY',		1).
valid_abstract_section_header('CLINICAL EFFICACY IN RRMS',		1).
valid_abstract_section_header('CLINICAL EFFICACY IN SPMS',		1).
valid_abstract_section_header('CLINICAL EPIDEMIOLOGY',		1).
valid_abstract_section_header('CLINICAL FACT',		1).
valid_abstract_section_header('CLINICAL FACTORS INCLUDED',		1).
valid_abstract_section_header('CLINICAL FEATURES AND AUDIT METHODS',		1).
valid_abstract_section_header('CLINICAL FEATURES AND PATHOLOGY',		1).
valid_abstract_section_header('CLINICAL FEATURES AND SOURCE',		1).
valid_abstract_section_header('CLINICAL FEATURES, INTERVENTION AND OUTCOME',		1).
valid_abstract_section_header('CLINICAL FINDING',		1).
valid_abstract_section_header('CLINICAL FINDINGS AND DIAGNOSES',		1).
valid_abstract_section_header('CLINICAL FUTURES',		1).
valid_abstract_section_header('CLINICAL GENETIC STUDIES',		1).
valid_abstract_section_header('CLINICAL IMPLICATION OF HFOV IN ADULT ACUTE RESPIRATORY DISTRESS SYNDROME',		1).
valid_abstract_section_header('CLINICAL IMPLICATIONS AND CONCLUSION',		1).
valid_abstract_section_header('CLINICAL IMPLICATIONS AND RECOMMENDATIONS',		1).
valid_abstract_section_header('CLINICAL IMPLICATIONS/CONCLUSION',		1).
valid_abstract_section_header('CLINICAL INDICATIONS',		1).
valid_abstract_section_header('CLINICAL INNOVATION',		1).
valid_abstract_section_header('CLINICAL INNOVATION REPORT',		1).
valid_abstract_section_header('CLINICAL INTERPRETATION OF PK/PD INTERACTIONS',		1).
valid_abstract_section_header('CLINICAL ISSUE AND DIAGNOSTIC STANDARDS',		1).
valid_abstract_section_header('CLINICAL MANAGEMENT AND THERAPY',		1).
valid_abstract_section_header('CLINICAL MANIFESTATIONS AND OPINIONS',		1).
valid_abstract_section_header('CLINICAL MATERIAL AND RESULTS',		1).
valid_abstract_section_header('CLINICAL METHODOLOGICAL ISSUES',		1).
valid_abstract_section_header('CLINICAL MICROSYSTEMS',		1).
valid_abstract_section_header('CLINICAL OBSERVATION AND METHODS',		1).
valid_abstract_section_header('CLINICAL OBSERVATIONS AND REVIEW OF THE LITERATURE',		1).
valid_abstract_section_header('CLINICAL OUTCOME MEASUREMENT',		1).
valid_abstract_section_header('CLINICAL OVERVIEW',		1).
valid_abstract_section_header('CLINICAL PICTURE AND INVESTIGATION',		1).
valid_abstract_section_header('CLINICAL PICTURE AND OUTCOME',		1).
valid_abstract_section_header('CLINICAL PICTURE, TREATMENT AND OUTCOME',		1).
valid_abstract_section_header('CLINICAL PICTURE, TREATMENT, AND OUTCOME',		1).
valid_abstract_section_header('CLINICAL PILOT STUDY',		1).
valid_abstract_section_header('CLINICAL PRACTICE GUIDELINE',		1).
valid_abstract_section_header('CLINICAL PRACTICE RELEVANCE',		1).
valid_abstract_section_header('CLINICAL PRESENTATION AND RADIOLOGICAL STUDIES',		1).
valid_abstract_section_header('CLINICAL PRESENTATION AND TREATMENT',		1).
valid_abstract_section_header('CLINICAL PRESENTATION:',		1).
valid_abstract_section_header('CLINICAL PROBLEM ADDRESSED',		1).
valid_abstract_section_header('CLINICAL PROCEDURES',		1).
valid_abstract_section_header('CLINICAL PROFILE',		1).
valid_abstract_section_header('CLINICAL PROOF',		1).
valid_abstract_section_header('CLINICAL PROTOCOL',		1).
valid_abstract_section_header('CLINICAL QUESTIO/LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('CLINICAL QUESTION /LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('CLINICAL QUESTION/ LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('CLINICAL QUESTION/LEVEL OF EVIDEMCE',		1).
valid_abstract_section_header('CLINICAL QUESTION/LEVEVL OF EVIDENCE',		1).
valid_abstract_section_header('CLINICAL REGISTRATION NO',		1).
valid_abstract_section_header('CLINICAL REHABIL IMPACT',		1).
valid_abstract_section_header('CLINICAL REHABILITAITON IMPACT',		1).
valid_abstract_section_header('CLINICAL REHABILITATION EFFECT',		1).
valid_abstract_section_header('CLINICAL RELEVANCE AND CONCLUSION',		1).
valid_abstract_section_header('CLINICAL RELEVANCE SCIENTIFIC RATIONALE FOR STUDY',		1).
valid_abstract_section_header('CLINICAL RELEVANCE:',		1).
valid_abstract_section_header('CLINICAL RELEVANCY STATEMENT',		1).
valid_abstract_section_header('CLINICAL RELEVENCE',		1).
valid_abstract_section_header('CLINICAL REPORT AND RESULTS',		1).
valid_abstract_section_header('CLINICAL REVELANCE',		1).
valid_abstract_section_header('CLINICAL REVELENCE',		1).
valid_abstract_section_header('CLINICAL REVIEW',		1).
valid_abstract_section_header('CLINICAL SIGNIFICANCE OF PLATELET AND GRANULOCYTE ANTIGENS',		1).
valid_abstract_section_header('CLINICAL SIGNS AND COMPLEMENTARY INVESTIGATIONS',		1).
valid_abstract_section_header('CLINICAL SPECTRUM',		1).
valid_abstract_section_header('CLINICAL STUDY DESIGN',		1).
valid_abstract_section_header('CLINICAL TECHNIQUES',		1).
valid_abstract_section_header('CLINICAL TRAIL REGISTRATION',		1).
valid_abstract_section_header('CLINICAL TRIAL IDENTIFICATION NUMBER',		1).
valid_abstract_section_header('CLINICAL TRIAL ISRCTN',		1).
valid_abstract_section_header('CLINICAL TRIAL LISTING',		1).
valid_abstract_section_header('CLINICAL TRIAL NAME AND IDENTIFIER',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTER',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRACTION',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION -URL',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION CLINICALTRIALSGOV IDENTIFIER',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION EUCTR',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION NAME AND NUMBER',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION NCT',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATION STATEMENT',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATIONCLINICALTRIALSGOVIDENTIFIER',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRATIONS URL',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRY AND TRIAL REGISTRATION NUMBER',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRY IDENTIFIER',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRY INDIA CTRI',		1).
valid_abstract_section_header('CLINICAL TRIAL REGISTRY NUMBER AND WEBSITE',		1).
valid_abstract_section_header('CLINICAL TRIALS GOV IDENTIFIER',		1).
valid_abstract_section_header('CLINICAL TRIALS NO',		1).
valid_abstract_section_header('CLINICAL TRIALS NUMBERS',		1).
valid_abstract_section_header('CLINICAL TRIALS OF IPT AND DEVELOPMENTS',		1).
valid_abstract_section_header('CLINICAL TRIALS OF MBT AND DEVELOPMENTS',		1).
valid_abstract_section_header('CLINICAL TRIALS REGISTER',		1).
valid_abstract_section_header('CLINICAL TRIALS REGISTRATION ID',		1).
valid_abstract_section_header('CLINICAL TRIALS REGISTRATION INFORMATION URL',		1).
valid_abstract_section_header('CLINICAL TRIALS REGISTRATION NO',		1).
valid_abstract_section_header('CLINICAL TRIALS REGISTRATION NR',		1).
valid_abstract_section_header('CLINICAL TRIALS REGISTRATION URL',		1).
valid_abstract_section_header('CLINICAL TRIALS REGISTRY NO',		1).
valid_abstract_section_header('CLINICAL TRIALS REGISTRY NUMBER',		1).
valid_abstract_section_header('CLINICAL TRIALS WITH ACTR NUMBER',		1).
valid_abstract_section_header('CLINICAL TRIALSGOV ID',		1).
valid_abstract_section_header('CLINICAL TRIALSGOV NO',		1).
valid_abstract_section_header('CLINICAL USE OF ABC',		1).
valid_abstract_section_header('CLINICAL UTILITY AND IMPLICATIONS',		1).
valid_abstract_section_header('CLINICAL VIGNETTE',		1).
valid_abstract_section_header('CLINICALSCENARIO',		1).
valid_abstract_section_header('CLINICALTRAILSGOV',		1).
valid_abstract_section_header('CLINICALTRAILSGOV NUMBER',		1).
valid_abstract_section_header('CLINICALTRIALGOV',		1).
valid_abstract_section_header('CLINICALTRIALGOV IDENTIFIER',		1).
valid_abstract_section_header('CLINICALTRIALS GOV IDENTIFIER',		1).
valid_abstract_section_header('CLINICALTRIALS GOV NUMBER',		1).
valid_abstract_section_header('CLINICALTRIALS GOV REGISTRATION',		1).
valid_abstract_section_header('CLINICALTRIALS IDENTIFIER',		1).
valid_abstract_section_header('CLINICALTRIALS.GOV ID',		1).
valid_abstract_section_header('CLINICALTRIALSGOV IDENTIFICATION NUMBER',		1).
valid_abstract_section_header('CLINICALTRIALSGOV NO',		1).
valid_abstract_section_header('CLINICALTRIALSGOV NUMBERS',		1).
valid_abstract_section_header('CLINICALTRIALSGOV REGISTRATION ID',		1).
valid_abstract_section_header('CLINICALTRIALSGOV REGISTRATION NUMBERS',		1).
valid_abstract_section_header('CLINICALTRIALSGOV REGISTRY',		1).
valid_abstract_section_header('CLINICALTRIALSGOV REGISTRY NUMBER',		1).
valid_abstract_section_header('CLINICIAL QUESTION/LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('CLINTRIALSGOV ID',		1).
valid_abstract_section_header('CLOSING ADDRESS',		1).
valid_abstract_section_header('CLUSTER QUALITY METRICS',		1).
valid_abstract_section_header('CME ACCREDITATION',		1).
valid_abstract_section_header('CME INSTRUCTIONS',		1).
valid_abstract_section_header('CN CONCLUSION',		1).
valid_abstract_section_header('CN VI DEFICIT',		1).
valid_abstract_section_header('CNCLUSIONS',		1).
valid_abstract_section_header('COBALT CARDIOTOXICITY',		1).
valid_abstract_section_header('COCHLEAR IMPLANT PROVISION FOR CHILDREN',		1).
valid_abstract_section_header('COCHLEAR IMPLANTS',		1).
valid_abstract_section_header('COCHRANE REGISTRATION NUMBER',		1).
valid_abstract_section_header('COGNITIVE PHENOTYPE',		1).
valid_abstract_section_header('COGNITIVE PSYCHOLOGY RESEARCH',		1).
valid_abstract_section_header('COHORT COMPARISON',		1).
valid_abstract_section_header('COHORT PARTICIPANTS',		1).
valid_abstract_section_header('COLLABORATION TO IMPROVE QUALITY OF CARE',		1).
valid_abstract_section_header('COLLABORATIVE APPROACH',		1).
valid_abstract_section_header('COLLABORATIVE EPIDEMIOLOGICAL STUDIES',		1).
valid_abstract_section_header('COLLABORATIVE MEASUREMENT DEVELOPMENT',		1).
valid_abstract_section_header('COLLABORATIVE TOPICS AND STRUCTURE',		1).
valid_abstract_section_header('COLLEAGUES',		1).
valid_abstract_section_header('COLLECTING DATA AND PROVIDING FEEDBACK',		1).
valid_abstract_section_header('COLLECTION AND ANALYSIS OF DATA',		1).
valid_abstract_section_header('COLLECTIVES AND METHODS',		1).
valid_abstract_section_header('COLOR DUPLEX SONOGRAPHY',		1).
valid_abstract_section_header('COLORECTAL CANCER IS CLASSIFIED IN TO THREE FORMS',		1).
valid_abstract_section_header('COMBINATIONS',		1).
valid_abstract_section_header('COMBINED TREATMENT OF COPD',		1).
valid_abstract_section_header('COMBO II',		1).
valid_abstract_section_header('COMING CHANGES AND POSSIBILITIES',		1).
valid_abstract_section_header('COMMENT AND CONCLUSION',		1).
valid_abstract_section_header('COMMENT AND CONCLUSIONS',		1).
valid_abstract_section_header('COMMENT ON',		1).
valid_abstract_section_header('COMMENTARY OF EXPERT OPINION',		1).
valid_abstract_section_header('COMMENTI E CONCLUSIONI',		1).
valid_abstract_section_header('COMMENTS AND RECOMMENDATIONS',		1).
valid_abstract_section_header('COMMENTS/CONCLUSION',		1).
valid_abstract_section_header('COMMON CAUSES OF COMPLAINTS',		1).
valid_abstract_section_header('COMMON COLD',		1).
valid_abstract_section_header('COMMON GROUND',		1).
valid_abstract_section_header('COMMON INJURIES',		1).
valid_abstract_section_header('COMMON NAME',		1).
valid_abstract_section_header('COMMON TREATMENT',		1).
valid_abstract_section_header('COMMON VIEW',		1).
valid_abstract_section_header('COMMUNITIES ARE THOUGHT TO BE ASSEMBLED BY TWO TYPES OF FILTERS',		1).
valid_abstract_section_header('COMMUNITY LEVEL',		1).
valid_abstract_section_header('COMMUNITY VS AC',		1).
valid_abstract_section_header('COMMUNITY VS SNF',		1).
valid_abstract_section_header('COMPARATIVE ANATOMY',		1).
valid_abstract_section_header('COMPARATIVE STUDY/LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('COMPARED WITH EXISTING METHODS',		1).
valid_abstract_section_header('COMPARED WITH PHAGOCYTIC LEUKOCYTES, THE ERYTHROCYTES',		1).
valid_abstract_section_header('COMPARISON ACROSS BUTTE',		1).
valid_abstract_section_header('COMPARISON AND CONCLUSIONS',		1).
valid_abstract_section_header('COMPARISON GROUP',		1).
valid_abstract_section_header('COMPARISON OF ACUTE DISTRESS RESPONSES',		1).
valid_abstract_section_header('COMPARISON OF BETA INTERFERONS',		1).
valid_abstract_section_header('COMPARISON OF EFFECTIVENESS',		1).
valid_abstract_section_header('COMPARISON OF EXISTING METHODS',		1).
valid_abstract_section_header('COMPARISON OF THE GUIDELINES',		1).
valid_abstract_section_header('COMPARISON WITH A EXISTING METHOD',		1).
valid_abstract_section_header('COMPARISON WITH AN EXISTING METHOD',		1).
valid_abstract_section_header('COMPARISON WITH EXCITING METHODS',		1).
valid_abstract_section_header('COMPARISON WITH EXISTING APPROACHES',		1).
valid_abstract_section_header('COMPARISON WITH EXISTING METHOD AND CONCLUSIONS',		1).
valid_abstract_section_header('COMPARISON WITH EXISTING METHODS/CONCLUSION',		1).
valid_abstract_section_header('COMPARISON WITH OLD METHOD',		1).
valid_abstract_section_header('COMPARISON WITH OTHER ANTIHYPERTENSIVE DRUGS',		1).
valid_abstract_section_header('COMPARISON WITH OTHER EXISTING METHODS',		1).
valid_abstract_section_header('COMPARISON WITH OTHER MODELS',		1).
valid_abstract_section_header('COMPARISON WITH OTHER ORGANS',		1).
valid_abstract_section_header('COMPARISON WITH OTHER STRATEGIES TO REDUCE RESTENOSIS RATE',		1).
valid_abstract_section_header('COMPARISON WITH OTHER TRAUMA SYSTEMS',		1).
valid_abstract_section_header('COMPARISON WITH PREVIOUS METHOD',		1).
valid_abstract_section_header('COMPARISONS ACROSS STRATEGIES',		1).
valid_abstract_section_header('COMPARISONS WITH EXISTING METHOD',		1).
valid_abstract_section_header('COMPARISONS WITH EXISTING METHOD(S)',		1).
valid_abstract_section_header('COMPARISONS WITH EXISTING METHODS',		1).
valid_abstract_section_header('COMPASSION MODULATORS',		1).
valid_abstract_section_header('COMPASSION, ALTRUISM AND REWARD',		1).
valid_abstract_section_header('COMPASSIONATE USE',		1).
valid_abstract_section_header('COMPETENCE AND TIMING OF EXPERTISE',		1).
valid_abstract_section_header('COMPETING INTEREST',		1).
valid_abstract_section_header('COMPETITIVE AND INSURMOUNTABLE ANTAGONISTS',		1).
valid_abstract_section_header('COMPLETE WORK UP INCLUDING',		1).
valid_abstract_section_header('COMPLEX METHODOLOGICAL PROBLEMS',		1).
valid_abstract_section_header('COMPLICATED DIVORCE ISSUES',		1).
valid_abstract_section_header('COMPLICATIONS RELATED TO INSERTION OF VERESS NEEDLE',		1).
valid_abstract_section_header('COMPONENTS OF DEVELOPMENT AND IMPLEMENTATION',		1).
valid_abstract_section_header('COMPONENTS OF THE CONFLICT MANAGEMENT PROCESS',		1).
valid_abstract_section_header('COMPOSITES',		1).
valid_abstract_section_header('COMPOSITION AND CONCENTRATION OF INGREDIENTS',		1).
valid_abstract_section_header('COMPREHENSIVE CLINICAL GUIDELINES',		1).
valid_abstract_section_header('COMPREHENSIVE REFLECTION',		1).
valid_abstract_section_header('COMPREHENSIVE REVIEW',		1).
valid_abstract_section_header('COMPUTER EVALUATION',		1).
valid_abstract_section_header('COMPUTER SYSTEMS FOR AUTOMATIC LANDMARK DETECTION',		1).
valid_abstract_section_header('COMPUTERS AND ELECTRONIC MEDICINE',		1).
valid_abstract_section_header('CON) CONCLUSION',		1).
valid_abstract_section_header('CONCENTRATION AND ROUTE',		1).
valid_abstract_section_header('CONCEPT AND STRUCTURE OF THE GUIDELINES',		1).
valid_abstract_section_header('CONCEPT APPLICATION',		1).
valid_abstract_section_header('CONCEPT OF FACILITATED PCI',		1).
valid_abstract_section_header('CONCEPT OF QUALITY OF LIFE',		1).
valid_abstract_section_header('CONCEPT OF SUPPORT',		1).
valid_abstract_section_header('CONCEPT TESTING',		1).
valid_abstract_section_header('CONCEPTION',		1).
valid_abstract_section_header('CONCEPTION OF HEALTH',		1).
valid_abstract_section_header('CONCEPTS AND MODELS',		1).
valid_abstract_section_header('CONCEPTS AND OBJECTIVES',		1).
valid_abstract_section_header('CONCEPTUALIZATION',		1).
valid_abstract_section_header('CONCERNING',		1).
valid_abstract_section_header('CONCERNS',		1).
valid_abstract_section_header('CONCISE ABSTRACT',		1).
valid_abstract_section_header('CONCKUSIONS',		1).
valid_abstract_section_header('CONCLISION',		1).
valid_abstract_section_header('CONCLISONS',		1).
valid_abstract_section_header('CONCLISOPNS',		1).
valid_abstract_section_header('CONCLLUSIONS',		1).
valid_abstract_section_header('CONCLSION',		1).
valid_abstract_section_header('CONCLUDING COMMENTS',		1).
valid_abstract_section_header('CONCLUDING HYPOTHESIS',		1).
valid_abstract_section_header('CONCLUDING SUGGESTIONS',		1).
valid_abstract_section_header('CONCLUDING SUMMARY',		1).
valid_abstract_section_header('CONCLUDING/SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUDION',		1).
valid_abstract_section_header('CONCLUDIONS',		1).
valid_abstract_section_header('CONCLUISIONS',		1).
valid_abstract_section_header('CONCLUISONS',		1).
valid_abstract_section_header('CONCLUS ION',		1).
valid_abstract_section_header('CONCLUSIE',		1).
valid_abstract_section_header('CONCLUSIN',		1).
valid_abstract_section_header('CONCLUSIOMS',		1).
valid_abstract_section_header('CONCLUSION & FUTURE IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSION & PERSPECTIVE',		1).
valid_abstract_section_header('CONCLUSION & RECOMMENDATION',		1).
valid_abstract_section_header('CONCLUSION & RELEVANCE',		1).
valid_abstract_section_header('CONCLUSION (FOCUS ON NUTRITIONAL RELEVANCE)',		1).
valid_abstract_section_header('CONCLUSION / INTERPRETATION',		1).
valid_abstract_section_header('CONCLUSION /SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSION AND APPLICATIONS',		1).
valid_abstract_section_header('CONCLUSION AND CLINICAL IMPACT',		1).
valid_abstract_section_header('CONCLUSION AND COMPARISON WITH OTHER METHODS',		1).
valid_abstract_section_header('CONCLUSION AND FUTURE PERSPECTIVES',		1).
valid_abstract_section_header('CONCLUSION AND IMPLICATION FOR NURSE MANAGEMENT',		1).
valid_abstract_section_header('CONCLUSION AND IMPLICATION FOR NURSING PRACTICE',		1).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR FUTURE GUIDELINE DEVELOPERS',		1).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR NURSING AND NURSING POLICY',		1).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR NURSING AND/OR HEALTH POLICY',		1).
valid_abstract_section_header('CONCLUSION AND IMPLICATIONS FOR NURSING PRACTICE',		1).
valid_abstract_section_header('CONCLUSION AND OUTCOME',		1).
valid_abstract_section_header('CONCLUSION AND POLICY IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSION AND POTENTIAL APPLICATIONS',		1).
valid_abstract_section_header('CONCLUSION AND PRACTICE IMPLICATION',		1).
valid_abstract_section_header('CONCLUSION AND PUBLIC HEALTH IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSION AND REHABILITATION IMPACT',		1).
valid_abstract_section_header('CONCLUSION AND RELEVANCE OF PRACTICE',		1).
valid_abstract_section_header('CONCLUSION AND RELEVANCE TO CHILD WELFARE',		1).
valid_abstract_section_header('CONCLUSION AND RESULT',		1).
valid_abstract_section_header('CONCLUSION AND SIGNIFICANCE AND IMPACT OF THE STUDY',		1).
valid_abstract_section_header('CONCLUSION AND SIGNIFICANCE OF THE STUDY',		1).
valid_abstract_section_header('CONCLUSION AND SUGGESTION',		1).
valid_abstract_section_header('CONCLUSION AND SUMMARY',		1).
valid_abstract_section_header('CONCLUSION FOR PRACTICE',		1).
valid_abstract_section_header('CONCLUSION FOR THE PRACTICE',		1).
valid_abstract_section_header('CONCLUSION HOW MARKET DATA CAN SUPPORT BETTER PUBLIC HEALTH INTERVENTIONS',		1).
valid_abstract_section_header('CONCLUSION IMPLICATIONS FOR PRACTICE',		1).
valid_abstract_section_header('CONCLUSION PRESTROKE',		1).
valid_abstract_section_header('CONCLUSION ST7612AA1',		1).
valid_abstract_section_header('CONCLUSION, SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSION, SIGNIFICANCE AND IMPACT OF STUDY',		1).
valid_abstract_section_header('CONCLUSION-',		1).
valid_abstract_section_header('CONCLUSION.',		1).
valid_abstract_section_header('CONCLUSION/ RECOMMENDATION',		1).
valid_abstract_section_header('CONCLUSION/ SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSION/APPLICATION OF THE STUDY',		1).
valid_abstract_section_header('CONCLUSION/CLINICAL IMPORTANCE',		1).
valid_abstract_section_header('CONCLUSION/CLINICAL SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSION/IMPACT',		1).
valid_abstract_section_header('CONCLUSION/IMPLICATION FOR PRACTICE',		1).
valid_abstract_section_header('CONCLUSION/IMPLICATION FOR RESEARCH',		1).
valid_abstract_section_header('CONCLUSION/IMPLICATIONS FOR NURSING MANAGEMENT',		1).
valid_abstract_section_header('CONCLUSION/INTERPRETATIONS',		1).
valid_abstract_section_header('CONCLUSION/KEY MESSAGE',		1).
valid_abstract_section_header('CONCLUSION/LIMITATIONS',		1).
valid_abstract_section_header('CONCLUSION/MAJOR FINDING',		1).
valid_abstract_section_header('CONCLUSION/MAJOR FINDINGS',		1).
valid_abstract_section_header('CONCLUSION/OUTLOOK',		1).
valid_abstract_section_header('CONCLUSION/PRACTICE IMPLICATION',		1).
valid_abstract_section_header('CONCLUSION/PRACTICE/IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSION/RELEVANCE TO CLINICAL PRACTICE',		1).
valid_abstract_section_header('CONCLUSION/RELEVANCE TO PRACTICE',		1).
valid_abstract_section_header('CONCLUSION/S SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSION/TRANSLATION TO HEALTH EDUCATION PRACTICE',		1).
valid_abstract_section_header('CONCLUSIONC',		1).
valid_abstract_section_header('CONCLUSIONE E DISCUSSIONE',		1).
valid_abstract_section_header('CONCLUSIONES',		1).
valid_abstract_section_header('CONCLUSIONS & CLINICAL IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSIONS & CLINICAL IMPORTANCE',		1).
valid_abstract_section_header('CONCLUSIONS & INTERFENCES',		1).
valid_abstract_section_header('CONCLUSIONS & INTERPRETATION',		1).
valid_abstract_section_header('CONCLUSIONS & PRACTICE IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSIONS & SCIENTIFIC SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSIONS AND  RELEVANCE',		1).
valid_abstract_section_header('CONCLUSIONS AND APPLICATION TO CLINICAL PRACTICE',		1).
valid_abstract_section_header('CONCLUSIONS AND APPLICATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND CLINIC IMPORTANCE',		1).
valid_abstract_section_header('CONCLUSIONS AND CLINICAL REHABILITATION IMPACT',		1).
valid_abstract_section_header('CONCLUSIONS AND CLINICAL RELEVANCY',		1).
valid_abstract_section_header('CONCLUSIONS AND CLINICAL SIGNIFICANT',		1).
valid_abstract_section_header('CONCLUSIONS AND CONSIDERATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND FUTURE DIRECTION',		1).
valid_abstract_section_header('CONCLUSIONS AND FUTURE RESEARCH NEEDS',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR CANCER SURVIVOR',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR HEALTH POLICIES',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR NURSING AND HEALTH POLICY',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR NURSING AND MIDWIFERY',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR NURSING PRACTICE',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR POLICY',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR PRACTICE AND FUTURE RESEARCH',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR PRACTICE AND RESEARCH',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR PUBLIC HEALTH',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS FOR THE SCHOOL NURSE',		1).
valid_abstract_section_header('CONCLUSIONS AND IMPLICATIONS OF THE MAIN FINDINGS',		1).
valid_abstract_section_header('CONCLUSIONS AND INFERENCE',		1).
valid_abstract_section_header('CONCLUSIONS AND INTERPRETATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND KEY FINDINGS',		1).
valid_abstract_section_header('CONCLUSIONS AND LESSONS LEARNED',		1).
valid_abstract_section_header('CONCLUSIONS AND LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('CONCLUSIONS AND LIMITATION',		1).
valid_abstract_section_header('CONCLUSIONS AND MESSAGE OF THE PAPER',		1).
valid_abstract_section_header('CONCLUSIONS AND NEXT STEPS',		1).
valid_abstract_section_header('CONCLUSIONS AND OPPORTUNITIES',		1).
valid_abstract_section_header('CONCLUSIONS AND POLICY IMPLICATION',		1).
valid_abstract_section_header('CONCLUSIONS AND POLICY RECOMMENDATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND POSSIBLE CLINICAL IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND PRACTICE',		1).
valid_abstract_section_header('CONCLUSIONS AND PRACTICE IMPLICATION',		1).
valid_abstract_section_header('CONCLUSIONS AND PROPOSITIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND RECOMMENDATIONS FOR ORGANIZATIONAL POLICIES',		1).
valid_abstract_section_header('CONCLUSIONS AND RELEVANCE TO CLINICAL NURSING',		1).
valid_abstract_section_header('CONCLUSIONS AND RELEVANCE TO PRACTICE',		1).
valid_abstract_section_header('CONCLUSIONS AND RELEVANCE:',		1).
valid_abstract_section_header('CONCLUSIONS AND RELEVENCE',		1).
valid_abstract_section_header('CONCLUSIONS AND RESEARCH RECOMMENDATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND SERVICE IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND SIGNIFICANCE OF THE FINDINGS',		1).
valid_abstract_section_header('CONCLUSIONS AND SIGNIFICANCE OF THIS STUDY',		1).
valid_abstract_section_header('CONCLUSIONS AND SPECULATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND STUDY LIMITATIONS',		1).
valid_abstract_section_header('CONCLUSIONS AND TRANSLATIONAL RELEVANCE',		1).
valid_abstract_section_header('CONCLUSIONS AND VIEWPOINTS',		1).
valid_abstract_section_header('CONCLUSIONS DRAWN FROM THE STUDY AND CLINICAL IMPLICATIONS',		1).
valid_abstract_section_header('CONCLUSIONS FROM LITERATURE SURVEY',		1).
valid_abstract_section_header('CONCLUSIONS LOE',		1).
valid_abstract_section_header('CONCLUSIONS OF THE ACADEMY',		1).
valid_abstract_section_header('CONCLUSIONS OF THE PANEL',		1).
valid_abstract_section_header('CONCLUSIONS RESULTS',		1).
valid_abstract_section_header('CONCLUSIONS VERSUS SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSIONS, AND IMPACT OF THE STUDY',		1).
valid_abstract_section_header('CONCLUSIONS, AND SIGNIFICANCE AND IMPACT OF THE STUDY',		1).
valid_abstract_section_header('CONCLUSIONS, SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSIONS-',		1).
valid_abstract_section_header('CONCLUSIONS--',		1).
valid_abstract_section_header('CONCLUSIONS/CLASSIFICATION OF EVIDENCE',		1).
valid_abstract_section_header('CONCLUSIONS/FUTURE WORK',		1).
valid_abstract_section_header('CONCLUSIONS/IMPLICATIONS FOR CLINICAL PRACTICE',		1).
valid_abstract_section_header('CONCLUSIONS/IMPLICATIONS FOR PRACTISE',		1).
valid_abstract_section_header('CONCLUSIONS/IMPLICATIONS FOR RESEARCH AND POLICY',		1).
valid_abstract_section_header('CONCLUSIONS/PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('CONCLUSIONS/SCIENTIFIC SIGNIFICANCE',		1).
valid_abstract_section_header('CONCLUSIONSS',		1).
valid_abstract_section_header('CONCLUSISON',		1).
valid_abstract_section_header('CONCLUSIVE COMMENTARIES',		1).
valid_abstract_section_header('CONCLUSONS',		1).
valid_abstract_section_header('CONCULUSION',		1).
valid_abstract_section_header('CONCULUSIONS',		1).
valid_abstract_section_header('CONDENSED VERSION AND BOTTOM LINE',		1).
valid_abstract_section_header('CONDUCT OF THE CONSENSUS CONFERENCE',		1).
valid_abstract_section_header('CONFERENCE REPORT',		1).
valid_abstract_section_header('CONFIDENCE AND COORDINATED EFFORTS',		1).
valid_abstract_section_header('CONFIDENCE INTERVAL',		1).
valid_abstract_section_header('CONFLICT OF INTEREST AND FINANCIAL SUPPORT',		1).
valid_abstract_section_header('CONFLICT OF INTEREST/STUDY FUNDING',		1).
valid_abstract_section_header('CONFLICTS BETWEEN QM AND RA',		1).
valid_abstract_section_header('CONFOUNDING FACTORS',		1).
valid_abstract_section_header('CONFOUNDING FACTORS AND OTHER REASONS FOR CAUTION',		1).
valid_abstract_section_header('CONGENITAL HEART DISEASE WITH PRIOR PALLIATION',		1).
valid_abstract_section_header('CONGESTIVE HEART FAILURE',		1).
valid_abstract_section_header('CONGRESS REPORT',		1).
valid_abstract_section_header('CONJUGATE VACCINES',		1).
valid_abstract_section_header('CONLCLUSIONS',		1).
valid_abstract_section_header('CONLCUSIONS',		1).
valid_abstract_section_header('CONLUSIONS/SIGNIFICANCE',		1).
valid_abstract_section_header('CONNECTION BETWEEN ION CHANNELS AND GENE MUTATION',		1).
valid_abstract_section_header('CONNECTIVE TISSUE DISEASES',		1).
valid_abstract_section_header('CONSCIOUS SEDATION TECHNIQUES',		1).
valid_abstract_section_header('CONSCLUSION',		1).
valid_abstract_section_header('CONSENSUS FINDS',		1).
valid_abstract_section_header('CONSENSUS PANEL',		1).
valid_abstract_section_header('CONSENSUS REPORT',		1).
valid_abstract_section_header('CONSENT',		1).
valid_abstract_section_header('CONSENT AND INFORMATION',		1).
valid_abstract_section_header('CONSENTS',		1).
valid_abstract_section_header('CONSEQUENCES FOR DEVELOPMENT AND/OR ALLOCATION OF ADJUVANT THERAPY',		1).
valid_abstract_section_header('CONSEQUENCES OF GRF',		1).
valid_abstract_section_header('CONSERVATIVE ENDOLARYNGEAL TREATMENT',		1).
valid_abstract_section_header('CONSIDERATION',		1).
valid_abstract_section_header('CONSIDERATIONS FOR FUTURE IMPROVEMENTS',		1).
valid_abstract_section_header('CONSIDERATIONS FOR POLICY DEVELOPMENT',		1).
valid_abstract_section_header('CONSLUSIONS/SIGNIFICANCE',		1).
valid_abstract_section_header('CONSOLIDATION',		1).
valid_abstract_section_header('CONSTAT',		1).
valid_abstract_section_header('CONSTITUTION ASPECT',		1).
valid_abstract_section_header('CONSTRUCTING COMPARISON CHARTS',		1).
valid_abstract_section_header('CONSULSIONS',		1).
valid_abstract_section_header('CONSULTATION, PLANNING AND IMPLEMENTATION PHASES',		1).
valid_abstract_section_header('CONTEMPORARY LESBIAN WRITING',		1).
valid_abstract_section_header('CONTEMPORARY TRENDS IN ORGANIZATION OF LABORATORY SERVICES',		1).
valid_abstract_section_header('CONTEMPORARY UNDERSTANDING AND EXISTING THERAPEUTIC GAPS',		1).
valid_abstract_section_header('CONTENIDO',		1).
valid_abstract_section_header('CONTENT AND CONCLUSIONS',		1).
valid_abstract_section_header('CONTENT AND IMPLEMENTATION',		1).
valid_abstract_section_header('CONTENT AND OBJECTIVE',		1).
valid_abstract_section_header('CONTENT OF THE FORA',		1).
valid_abstract_section_header('CONTENT OF THE REVIEW',		1).
valid_abstract_section_header('CONTENTS AND CONCLUSIONS',		1).
valid_abstract_section_header('CONTENTS OF THE STUDY',		1).
valid_abstract_section_header('CONTEX',		1).
valid_abstract_section_header('CONTEXT & BACKGROUND',		1).
valid_abstract_section_header('CONTEXT AND BACKGROUND',		1).
valid_abstract_section_header('CONTEXT AND OVERVIEW',		1).
valid_abstract_section_header('CONTEXT AND PURPOSE OF STUDY',		1).
valid_abstract_section_header('CONTEXT AND PURPOSE OF THE STUDY',		1).
valid_abstract_section_header('CONTEXT AND QUESTIONS ASKED',		1).
valid_abstract_section_header('CONTEXT AND RATIONALE',		1).
valid_abstract_section_header('CONTEXT AND SETTING',		1).
valid_abstract_section_header('CONTEXT OBJECTIVE',		1).
valid_abstract_section_header('CONTEXT OF THE PROBLEM',		1).
valid_abstract_section_header('CONTEXT SCIENTIFIC AND THERAPEUTIC ADVANCES',		1).
valid_abstract_section_header('CONTEXT/PROBLEM',		1).
valid_abstract_section_header('CONTEXTE ET OBJECTIF',		1).
valid_abstract_section_header('CONTEXTE ET OBJECTIFS DE LA REVUE',		1).
valid_abstract_section_header('CONTEXTE ET PARTICIPANTS',		1).
valid_abstract_section_header('CONTEXTE MOTIVANT LA REVUE',		1).
valid_abstract_section_header('CONTEXTES',		1).
valid_abstract_section_header('CONTEXTO LOCAL',		1).
valid_abstract_section_header('CONTEXTUAL ANALYSIS',		1).
valid_abstract_section_header('CONTINUED LEARNING OBJECTIVES',		1).
valid_abstract_section_header('CONTINUING EFFORTS',		1).
valid_abstract_section_header('CONTINUOUS OPERATIONAL ASSESSMENT AND RESPONSE SYSTEM',		1).
valid_abstract_section_header('CONTRACEPTION MEANS',		1).
valid_abstract_section_header('CONTRAST AGENTS',		1).
valid_abstract_section_header('CONTRIBUTION OF AUTHORS',		1).
valid_abstract_section_header('CONTRIBUTION OF IADR',		1).
valid_abstract_section_header('CONTRIBUTION OF MRI',		1).
valid_abstract_section_header('CONTRIBUTIONS TO SURGERY',		1).
valid_abstract_section_header('CONTRIBUTORY FACTORS',		1).
valid_abstract_section_header('CONTROL ARM',		1).
valid_abstract_section_header('CONTROL CONDITION',		1).
valid_abstract_section_header('CONTROL INTERVENTION',		1).
valid_abstract_section_header('CONTROL INTERVENTIONS',		1).
valid_abstract_section_header('CONTROL OF POSITION',		1).
valid_abstract_section_header('CONTROL OF SELECTION EFFECTS',		1).
valid_abstract_section_header('CONTROL/CONTROL',		1).
valid_abstract_section_header('CONTROL/FF',		1).
valid_abstract_section_header('CONTROL/FR',		1).
valid_abstract_section_header('CONTROLLING TOBACCO EXPORTS',		1).
valid_abstract_section_header('CONTROVERSIAL SOCIAL SKILL',		1).
valid_abstract_section_header('CONTROVERSY OVER CALCIUM ANTAGONISTS',		1).
valid_abstract_section_header('CONTROVERSY OVER USE OF CALCIUM ANTAGONISTS IN HYPERTENSION',		1).
valid_abstract_section_header('CONVENTIONAL CHEMOTHERAPY',		1).
valid_abstract_section_header('CONVENTIONAL MANAGEMENT (CM)',		1).
valid_abstract_section_header('CONVERGENCES BETWEEN TELEMEDICINE AT SEA AND ONSHORE',		1).
valid_abstract_section_header('CONVERSATION ANALYSIS AND THE MEDICAL INTERVIEW',		1).
valid_abstract_section_header('CONVERSION INTO CLINICAL PRACTICE',		1).
valid_abstract_section_header('CONVERSION TO NONLATEX PRODUCTS AND EXAMINATION GLOVES',		1).
valid_abstract_section_header('CONVULSIONS',		1).
valid_abstract_section_header('COOLEY DICKINSON HOSPITAL (CDH)',		1).
valid_abstract_section_header('COOPERATIVE STUDIES',		1).
valid_abstract_section_header('COORDINATION PROCESS DIAGRAMMING',		1).
valid_abstract_section_header('COPAXONE',		1).
valid_abstract_section_header('COPPER',		1).
valid_abstract_section_header('COPRIMARY OUTCOMES',		1).
valid_abstract_section_header('COPYRIGHT',		1).
valid_abstract_section_header('CORE ARGUMENT',		1).
valid_abstract_section_header('CORE ISSUE DISCUSSION',		1).
valid_abstract_section_header('CORE PERFORMANCE MEASURES',		1).
valid_abstract_section_header('CORE TIP',		1).
valid_abstract_section_header('CORNEAL DYSTROPHIES',		1).
valid_abstract_section_header('CORONARY ANGIOPLASTY',		1).
valid_abstract_section_header('CORONARY ARTERIES',		1).
valid_abstract_section_header('CORONARY ARTERY BYPASS GRAFT (CABG) PATIENTS',		1).
valid_abstract_section_header('CORONARY ARTERY BYPASS GRAFTING (CABG)',		1).
valid_abstract_section_header('CORONARY HEART DISEASE AND HYPERTENSION',		1).
valid_abstract_section_header('CORONARY HEART DISEASE IN CT',		1).
valid_abstract_section_header('CORONARY STENTING',		1).
valid_abstract_section_header('CORPORATE GLOBALISATION AND ITS IMPACT ON MEDICAL PROFESSIONALISM',		1).
valid_abstract_section_header('CORRECTION OF POSTOPERATIVE APHAKIA',		1).
valid_abstract_section_header('CORRECTIVE ACTIONS',		1).
valid_abstract_section_header('CORRELATION WITH DOMINANT TUMOR LOCATION',		1).
valid_abstract_section_header('CORRELATION WITH OTHER TESTS',		1).
valid_abstract_section_header('CORRIGENDUM',		1).
valid_abstract_section_header('COST AND UTILIZATION FINDINGS',		1).
valid_abstract_section_header('COST CONSIDERATIONS',		1).
valid_abstract_section_header('COST EFFECTIVENESS',		1).
valid_abstract_section_header('COST ISSUES',		1).
valid_abstract_section_header('COST SAVINGS THROUGH QUALITY IMPROVEMENT',		1).
valid_abstract_section_header('COSTIMULATION',		1).
valid_abstract_section_header('COSTING METHODOLOGY',		1).
valid_abstract_section_header('COUCLUSION',		1).
valid_abstract_section_header('COUCLUSIONS',		1).
valid_abstract_section_header('COUGH ASSESSMENT AND MANAGEMENT',		1).
valid_abstract_section_header('COUNTRY COMPARISONS',		1).
valid_abstract_section_header('COURSE DESCRIPTION',		1).
valid_abstract_section_header('COURSE EVALUATION',		1).
valid_abstract_section_header('COURSE OF DISEASE',		1).
valid_abstract_section_header('COURSE OF DISEASE AND TREATMENT',		1).
valid_abstract_section_header('COURSE OF NEC',		1).
valid_abstract_section_header('COURSE OF TREATMENT',		1).
valid_abstract_section_header('COURSE STRUCTURE',		1).
valid_abstract_section_header('COVER PHOTOGRAPH',		1).
valid_abstract_section_header('CPOE AND SAFE MEDICATION USE',		1).
valid_abstract_section_header('CPOE AT BWH',		1).
valid_abstract_section_header('CPS) EXPERIMENTAL DESIGN',		1).
valid_abstract_section_header('CPX CONCLUSIONS',		1).
valid_abstract_section_header('CQI PROGRAM',		1).
valid_abstract_section_header('CQI PROJECTS',		1).
valid_abstract_section_header('CRAFFT',		1).
valid_abstract_section_header('CRANIAL BASE',		1).
valid_abstract_section_header('CRANIOVERTEBRAL ABNORMALITIES IN CHILDREN',		1).
valid_abstract_section_header('CRASSULACEAN ACID METABOLISM',		1).
valid_abstract_section_header('CREATE A CULTURE OF RETENTION',		1).
valid_abstract_section_header('CREATING A BICAMPUS PEDIATRIC QUALITY AND SAFETY TEAM',		1).
valid_abstract_section_header('CREATING A CULTURE OF LEARNING, JUSTICE, AND ACCOUNTABILITY',		1).
valid_abstract_section_header('CREATING A CULTURE OF SAFETY',		1).
valid_abstract_section_header('CREATING QUALITY FROM THE FAMILY PERSPECTIVE',		1).
valid_abstract_section_header('CREATING THE PRINCIPLES OF A FAIR AND JUST CULTURE',		1).
valid_abstract_section_header('CREATING THE PROGRAM',		1).
valid_abstract_section_header('CRITERIA FOR ANIMAL MODELS',		1).
valid_abstract_section_header('CRITERIA FOR ASSESSING SCIENTIFIC SOUNDNESS',		1).
valid_abstract_section_header('CRITERIA FOR CONSIDERING STUDIES IN THIS REVIEW',		1).
valid_abstract_section_header('CRITERIA FOR EXCLUSION',		1).
valid_abstract_section_header('CRITERIA FOR INCLUSION',		1).
valid_abstract_section_header('CRITERIA FOR INCLUSION AND EXCLUSION OF ARTICLES',		1).
valid_abstract_section_header('CRITERIA FOR INCLUSION AND EXCLUSION OF PUBLISHED STUDIES',		1).
valid_abstract_section_header('CRITERIA FOR SELECTING MEANINGFUL ASSESSMENT AREAS',		1).
valid_abstract_section_header('CRITERIA OF EVALUATION',		1).
valid_abstract_section_header('CRITICAL APPRAISAL, DATA COLLECTION & DATA SYNTHESIS',		1).
valid_abstract_section_header('CRITICAL APPRAISAL, DATA EXTRACTION AND DATA SYNTHESIS',		1).
valid_abstract_section_header('CRITICAL AREAS IDENTIFIED',		1).
valid_abstract_section_header('CRITICAL ELEMENTS IN FLAP DESIGN AND SURGICAL EXECUTION',		1).
valid_abstract_section_header('CRITICAL ELEMENTS IN THE USE OF SOFT TISSUE REPLACEMENT GRAFTS',		1).
valid_abstract_section_header('CRITICAL RENAL LESIONS IN DIABETIC NEPHROPATHY',		1).
valid_abstract_section_header('CRITICAL SUCCESS FACTORS',		1).
valid_abstract_section_header('CRITICAL SUCCESS FACTORS AND CHALLENGES',		1).
valid_abstract_section_header('CRITICALLY APPRAISED PAPER',		1).
valid_abstract_section_header('CRM TRAINING',		1).
valid_abstract_section_header('CROSS REACTIVITY BETWEEN HCV PROTEINS AND HUMAN PROTEINS',		1).
valid_abstract_section_header('CROSS/SECTIONAL RESULTS',		1).
valid_abstract_section_header('CROWDSOURCING',		1).
valid_abstract_section_header('CRUDE INCIDENCE',		1).
valid_abstract_section_header('CSF LEAK',		1).
valid_abstract_section_header('CSIG PROGRAM',		1).
valid_abstract_section_header('CTRI ACKNOWLEDGEMENT NO',		1).
valid_abstract_section_header('CTRI REGISTRATION NUMBER',		1).
valid_abstract_section_header('CULTURAL DIMENSION',		1).
valid_abstract_section_header('CULTURAL IMPLICATIONS OF INTRODUCING NEW TECHNOLOGY',		1).
valid_abstract_section_header('CULTURAL OR RELIGIOUS OBJECTIONS',		1).
valid_abstract_section_header('CULTURE OF HEALTH ACTION AREAS',		1).
valid_abstract_section_header('CUNCLUSIONS',		1).
valid_abstract_section_header('CURRENT & FUTURE DEVELOPMENT',		1).
valid_abstract_section_header('CURRENT ACTION',		1).
valid_abstract_section_header('CURRENT APPLICATIONS',		1).
valid_abstract_section_header('CURRENT APPROACH',		1).
valid_abstract_section_header('CURRENT APPROACHES',		1).
valid_abstract_section_header('CURRENT CONCEPT',		1).
valid_abstract_section_header('CURRENT CONDITION',		1).
valid_abstract_section_header('CURRENT DATA AND RESULTS',		1).
valid_abstract_section_header('CURRENT DEBATES',		1).
valid_abstract_section_header('CURRENT DIET AND VULNERABLE GROUPS',		1).
valid_abstract_section_header('CURRENT DISCUSSION AND INDICATIONS',		1).
valid_abstract_section_header('CURRENT FOCUS',		1).
valid_abstract_section_header('CURRENT FUNDING FOR BONE MINERAL DENSITY TESTING',		1).
valid_abstract_section_header('CURRENT INITIATIVES AND POLICIES',		1).
valid_abstract_section_header('CURRENT ISSUES',		1).
valid_abstract_section_header('CURRENT KNOWLEDGES AND KEY POINTS',		1).
valid_abstract_section_header('CURRENT LIMITATIONS',		1).
valid_abstract_section_header('CURRENT MODELS FOR COMMUNITY SCHOLARSHIP',		1).
valid_abstract_section_header('CURRENT OBSERVATIONS',		1).
valid_abstract_section_header('CURRENT POSITION AND MAIN POINTS',		1).
valid_abstract_section_header('CURRENT PROGRAM STATUS',		1).
valid_abstract_section_header('CURRENT PROGRESS',		1).
valid_abstract_section_header('CURRENT PROTOCOLS',		1).
valid_abstract_section_header('CURRENT PSYCHIATRIC TRAINING',		1).
valid_abstract_section_header('CURRENT REALITY AND BARRIERS',		1).
valid_abstract_section_header('CURRENT RELEVANCE',		1).
valid_abstract_section_header('CURRENT RESULTS',		1).
valid_abstract_section_header('CURRENT ROLE OF ANTIFIBRINOLYTICS IN ORTHOPAEDIC SURGERY',		1).
valid_abstract_section_header('CURRENT SCIENTIFIC KNOWLEDGE',		1).
valid_abstract_section_header('CURRENT STATE OF OPEN SURGICAL SIMULATION',		1).
valid_abstract_section_header('CURRENT STATE OF PROBLEM SOLUTION',		1).
valid_abstract_section_header('CURRENT STRATEGIES FOR HCV TREATMENT',		1).
valid_abstract_section_header('CURRENT STUDIES',		1).
valid_abstract_section_header('CURRENT STUDY RESULTS',		1).
valid_abstract_section_header('CURRENT SYSTEM',		1).
valid_abstract_section_header('CURRENT TAX TREATMENT',		1).
valid_abstract_section_header('CURRENT TECHNICAL AND GOVERNANCE CHALLENGES',		1).
valid_abstract_section_header('CURRENT TOPICS AND IMPORTANT RESULT',		1).
valid_abstract_section_header('CURRENT TOPICS AND IMPORTANT RESULTS',		1).
valid_abstract_section_header('CURRENT TRENDS',		1).
valid_abstract_section_header('CURRENT TRENDS IN CARIES TREATMENT',		1).
valid_abstract_section_header('CURRENT UK PRACTICE',		1).
valid_abstract_section_header('CURRENT USE OF NEUROIMAGING IN STROKE PATIENTS',		1).
valid_abstract_section_header('CURRENT WORK',		1).
valid_abstract_section_header('CURRENTLY EXISTING ACTIVITIES',		1).
valid_abstract_section_header('CURRICULAR DESIGN',		1).
valid_abstract_section_header('CURRICULUM DEVELOPMENT AND EVALUATION PLAN',		1).
valid_abstract_section_header('CURRICULUM EVALUATION',		1).
valid_abstract_section_header('CURRICULUM REFORM',		1).
valid_abstract_section_header('CURRICULUM VITAE AND MERITS',		1).
valid_abstract_section_header('CUSTOMIZED PERL SCRIPTS',		1).
valid_abstract_section_header('CUTANEOUS MANIFESTATIONS',		1).
valid_abstract_section_header('CYP19A1',		1).
valid_abstract_section_header('CYSTOGRAPHY AND URETHROCYSTOGRAPHY',		1).
valid_abstract_section_header('CYTOGENETICS',		1).
valid_abstract_section_header('Clinical Implications',		1).
valid_abstract_section_header('Clinical Trial Registration',		1).
valid_abstract_section_header('Clinical trial registration number:',		1).
valid_abstract_section_header('Conclusions and Recommendation',		1).
valid_abstract_section_header('Conclusions and Recommendations',		1).
valid_abstract_section_header('Conclusions and Revelance',		1).
valid_abstract_section_header('Conlusions',		1).
valid_abstract_section_header('D CLINICAL OBSERVATIONS',		1).
valid_abstract_section_header('DACRS',		1).
valid_abstract_section_header('DAILY CONTROL/PROCESS MANAGEMENT',		1).
valid_abstract_section_header('DAILY PRACTICE RECOMMENDATIONS',		1).
valid_abstract_section_header('DAMAGE TO ERYTHROCYTES REDUCES ADENOSINE LEVELS',		1).
valid_abstract_section_header('DATA ACCESSIBILITY',		1).
valid_abstract_section_header('DATA ANALYSES AND DELIVERABLE',		1).
valid_abstract_section_header('DATA ANALYSIS AND STUDY APPRAISAL METHODS',		1).
valid_abstract_section_header('DATA ANALYSIS METHODS',		1).
valid_abstract_section_header('DATA ANALYSIS/RESULTS',		1).
valid_abstract_section_header('DATA AND INTERPRETATIONS',		1).
valid_abstract_section_header('DATA AND LITERATURE SOURCES',		1).
valid_abstract_section_header('DATA AND MEASURES',		1).
valid_abstract_section_header('DATA AND SAMPLE',		1).
valid_abstract_section_header('DATA AND SOURCE',		1).
valid_abstract_section_header('DATA AND STUDY POPULATION',		1).
valid_abstract_section_header('DATA AND SUMMARY',		1).
valid_abstract_section_header('DATA AVAILABILITY',		1).
valid_abstract_section_header('DATA BASES',		1).
valid_abstract_section_header('DATA BASES AND DATA TREATMENT',		1).
valid_abstract_section_header('DATA COLLECTION &AMP ANALYSIS',		1).
valid_abstract_section_header('DATA COLLECTION ANALYSIS',		1).
valid_abstract_section_header('DATA COLLECTION AND ANALYSIS-',		1).
valid_abstract_section_header('DATA COLLECTION AND CODING',		1).
valid_abstract_section_header('DATA COLLECTION AND EXTRACTION',		1).
valid_abstract_section_header('DATA COLLECTION AND USE',		1).
valid_abstract_section_header('DATA COLLECTION AND/OR ANALYSIS',		1).
valid_abstract_section_header('DATA COLLECTION EXTRACTION',		1).
valid_abstract_section_header('DATA COLLECTION INCLUDES',		1).
valid_abstract_section_header('DATA COLLECTION INSTRUMENT',		1).
valid_abstract_section_header('DATA COLLECTION METHODS AND ANALYSIS',		1).
valid_abstract_section_header('DATA COLLECTION PHASES',		1).
valid_abstract_section_header('DATA COLLECTION PROCEDURE',		1).
valid_abstract_section_header('DATA COLLECTION, ANALYSIS AND SYNTHESIS',		1).
valid_abstract_section_header('DATA COLLECTION/ EXTRACTION',		1).
valid_abstract_section_header('DATA COLLECTION/EXTRACTING METHODS',		1).
valid_abstract_section_header('DATA COLLECTIONS',		1).
valid_abstract_section_header('DATA DESCRIPTION',		1).
valid_abstract_section_header('DATA EXTRACTED',		1).
valid_abstract_section_header('DATA EXTRACTION AND ASSESSMENT',		1).
valid_abstract_section_header('DATA EXTRACTION/DATA SYNTHESIS',		1).
valid_abstract_section_header('DATA EXTRACTIONS AND SYNTHESIS',		1).
valid_abstract_section_header('DATA FROM',		1).
valid_abstract_section_header('DATA GROUPING',		1).
valid_abstract_section_header('DATA LEADS TO DECISION SUPPORT TOOLS FOR CARDIAC CARE',		1).
valid_abstract_section_header('DATA MANAGEMENT',		1).
valid_abstract_section_header('DATA MANAGEMENT AND ANALYSIS',		1).
valid_abstract_section_header('DATA MEASUREMENTS',		1).
valid_abstract_section_header('DATA ON PRESSURE ULCER DEVELOPMENT',		1).
valid_abstract_section_header('DATA OVERVIEW',		1).
valid_abstract_section_header('DATA PATIENTS AND METHODS',		1).
valid_abstract_section_header('DATA POSTING',		1).
valid_abstract_section_header('DATA REDUCTION',		1).
valid_abstract_section_header('DATA REPOSITORY',		1).
valid_abstract_section_header('DATA RESULTS',		1).
valid_abstract_section_header('DATA RETRIVAL',		1).
valid_abstract_section_header('DATA REVIEW',		1).
valid_abstract_section_header('DATA SEARCH',		1).
valid_abstract_section_header('DATA SEARCHES',		1).
valid_abstract_section_header('DATA SECTION',		1).
valid_abstract_section_header('DATA SELECTION AND ANALYSIS',		1).
valid_abstract_section_header('DATA SETS',		1).
valid_abstract_section_header('DATA SHARING',		1).
valid_abstract_section_header('DATA SOURCE AND RESEARCH DESIGN',		1).
valid_abstract_section_header('DATA SOURCE AND STUDY DESIGN',		1).
valid_abstract_section_header('DATA SOURCE/EXTRACTION',		1).
valid_abstract_section_header('DATA SOURCES & SELECTION',		1).
valid_abstract_section_header('DATA SOURCES & STUDY SETTING',		1).
valid_abstract_section_header('DATA SOURCES AND APPRAISAL',		1).
valid_abstract_section_header('DATA SOURCES AND DATA SELECTION',		1).
valid_abstract_section_header('DATA SOURCES AND ELIGIBILITY CRITERIA',		1).
valid_abstract_section_header('DATA SOURCES AND MAIN RESULTS',		1).
valid_abstract_section_header('DATA SOURCES AND REVIEW METHOD',		1).
valid_abstract_section_header('DATA SOURCES AND SEARCH TERMS',		1).
valid_abstract_section_header('DATA SOURCES AND STUDY ELIGIBILITY',		1).
valid_abstract_section_header('DATA SOURCES STUDY SELECTION',		1).
valid_abstract_section_header('DATA SOURCES, DATA EXTRACTION, AND DATA SYNTHESIS',		1).
valid_abstract_section_header('DATA SOURCES, DATA EXTRACTION, DATA SYNTHESIS',		1).
valid_abstract_section_header('DATA SOURCES, DATA SELECTION, AND DATA EXTRACTION',		1).
valid_abstract_section_header('DATA SOURCES, EXTRACTION AND ANALYSIS',		1).
valid_abstract_section_header('DATA SOURCES, EXTRACTION AND SYNTHESIS',		1).
valid_abstract_section_header('DATA SOURCES, EXTRACTIONS, AND SYNTHESIS',		1).
valid_abstract_section_header('DATA SOURCES, SEARCH TERMS AND STUDY SELECTION',		1).
valid_abstract_section_header('DATA SOURCES, STUDY DESIGN, METHODS',		1).
valid_abstract_section_header('DATA SOURCES, STUDY SELECTION',		1).
valid_abstract_section_header('DATA SOURCES, STUDY SELECTION, AND DATA ABSTRACTION',		1).
valid_abstract_section_header('DATA SOURCES, STUDY SELECTION, AND REVIEW METHODS',		1).
valid_abstract_section_header('DATA SOURCES, STUDY SELECTION, DATA EXTRACTION, DATA SYNTHESIS',		1).
valid_abstract_section_header('DATA SOURCES/CASE SUMMARY',		1).
valid_abstract_section_header('DATA SOURCES/DESIGN',		1).
valid_abstract_section_header('DATA SOURCES/STUDY SESSION',		1).
valid_abstract_section_header('DATA SOURCES/STUDY SETTING/STUDY DESIGN',		1).
valid_abstract_section_header('DATA SUMMARY AND CONCLUSION',		1).
valid_abstract_section_header('DATA SYNOPSIS AND CONCLUSIONS',		1).
valid_abstract_section_header('DATA SYNTHESIS AND CASE PRESENTATION',		1).
valid_abstract_section_header('DATA SYNTHESIS AND MAIN FINDING',		1).
valid_abstract_section_header('DATA SYNTHESIS AND RESULTS',		1).
valid_abstract_section_header('DATA, METHODS, AND RESULTS',		1).
valid_abstract_section_header('DATA, SETTING, AND PARTICIPANTS',		1).
valid_abstract_section_header('DATA, SOURCES, AND SELECTION',		1).
valid_abstract_section_header('DATA-EXTRACTION AND ANALYSIS',		1).
valid_abstract_section_header('DATA/SETTING/DESIGN',		1).
valid_abstract_section_header('DATA/SOURCES/STUDY SELECTION',		1).
valid_abstract_section_header('DATABASE ACCESSION CODES',		1).
valid_abstract_section_header('DATABASE DEVELOPMENT',		1).
valid_abstract_section_header('DATABASE ENVIRONMENT',		1).
valid_abstract_section_header('DATABASE REGISTRATION',		1).
valid_abstract_section_header('DATASET',		1).
valid_abstract_section_header('DATASET AND RESULTS',		1).
valid_abstract_section_header('DATASOURCES',		1).
valid_abstract_section_header('DATE OF ENROLMENT OF FIRST PATIENT',		1).
valid_abstract_section_header('DATE OF TRIAL REGISTRATION',		1).
valid_abstract_section_header('DATE SOURCE',		1).
valid_abstract_section_header('DATES OF SEARCH',		1).
valid_abstract_section_header('DE-ESCALATION OF RADIOTHERAPY OF INFILTRATING BREAST CANCER',		1).
valid_abstract_section_header('DEAR COLLEAGUES',		1).
valid_abstract_section_header('DEBRIEFING',		1).
valid_abstract_section_header('DECISION SUPPORT',		1).
valid_abstract_section_header('DECISIONS',		1).
valid_abstract_section_header('DECLARATION INTEREST',		1).
valid_abstract_section_header('DECLARATION OF CONFLICTING INTERESTS',		1).
valid_abstract_section_header('DECREASED LEVEL OF HDL CHOLESTEROL',		1).
valid_abstract_section_header('DEFINE PROFESSIONAL PRACTICE GAP AND EDUCATIONAL NEED',		1).
valid_abstract_section_header('DEFINING AND DEVELOPING COMPETENCY IN QI',		1).
valid_abstract_section_header('DEFINITION & INCLUSION CRITERIA',		1).
valid_abstract_section_header('DEFINITION AND ASSESSMENT OF AGING',		1).
valid_abstract_section_header('DEFINITION AND CLINICAL ATTRIBUTES OF PHN',		1).
valid_abstract_section_header('DEFINITION AND EPIDEMIOLOGY',		1).
valid_abstract_section_header('DEFINITION AND INCIDENCE',		1).
valid_abstract_section_header('DEFINITION OF MEDICAL HARM AND SETTING THE GOAL',		1).
valid_abstract_section_header('DEFINITION OF THE ANATOMIC DISLOCATION ANGLES OF THE FEMORAL EPIPHYSIS (ED AND ET ANGLES) AT THE PROXIMAL FEMUR',		1).
valid_abstract_section_header('DEFINITION OF THE DISEASE',		1).
valid_abstract_section_header('DEFINITION, SYMPTOMS, THERAPY',		1).
valid_abstract_section_header('DEFINITIONS AND CHARACTERISTICS OF DECISIONAL CONTROL',		1).
valid_abstract_section_header('DEFINITIONS AND MANAGEMENT',		1).
valid_abstract_section_header('DEFINITIONS AND RECOMMENDATIONS',		1).
valid_abstract_section_header('DEGRADATION PATHWAYS',		1).
valid_abstract_section_header('DELIBERATE PRACTICE',		1).
valid_abstract_section_header('DELIBERATE RELEASE',		1).
valid_abstract_section_header('DEMOGRAPHIC DEVELOPMENT',		1).
valid_abstract_section_header('DEMONSTRATING THE PROPOSED SOLUTION',		1).
valid_abstract_section_header('DEMONSTRATION',		1).
valid_abstract_section_header('DEMONSTRATION PROJECTS',		1).
valid_abstract_section_header('DENSITY',		1).
valid_abstract_section_header('DENTAL NEGLECT',		1).
valid_abstract_section_header('DEPARTMENT OF INFECTIOUS DISEASES',		1).
valid_abstract_section_header('DEPARTMENTS',		1).
valid_abstract_section_header('DEPENDENCIES',		1).
valid_abstract_section_header('DEPENDING ON THE CONTAMINATION',		1).
valid_abstract_section_header('DEPENDING ON THE DISEASE',		1).
valid_abstract_section_header('DEPENDING ON THE PRODUCTION OF SLIME',		1).
valid_abstract_section_header('DEPIGMENTATION LASERS',		1).
valid_abstract_section_header('DEPILATORY LASERS',		1).
valid_abstract_section_header('DEPRESSION AS A STROKE RISK FACTOR',		1).
valid_abstract_section_header('DEPRESSIVE STATES',		1).
valid_abstract_section_header('DERMAL ABSORPTION AND TOXICITY OF NANOPARTICLES',		1).
valid_abstract_section_header('DERMAL FIBROPROLIFERATIVE DISORDERS',		1).
valid_abstract_section_header('DESCRIPTION AND DISCUSSION',		1).
valid_abstract_section_header('DESCRIPTION AND RESULTS',		1).
valid_abstract_section_header('DESCRIPTION OF A CASE',		1).
valid_abstract_section_header('DESCRIPTION OF A NEW INSTRUMENT',		1).
valid_abstract_section_header('DESCRIPTION OF A NEW TECHNIQUE',		1).
valid_abstract_section_header('DESCRIPTION OF APPROACHES',		1).
valid_abstract_section_header('DESCRIPTION OF ARCHITECTURE',		1).
valid_abstract_section_header('DESCRIPTION OF CASES AND RESULTS',		1).
valid_abstract_section_header('DESCRIPTION OF CONCEPT',		1).
valid_abstract_section_header('DESCRIPTION OF CONDITION',		1).
valid_abstract_section_header('DESCRIPTION OF COURSE',		1).
valid_abstract_section_header('DESCRIPTION OF EPISODE GROUPER SOFTWARE PRODUCTS',		1).
valid_abstract_section_header('DESCRIPTION OF INJURY AND CURRENT MANAGEMENT',		1).
valid_abstract_section_header('DESCRIPTION OF INTEGRATED PRACTICE',		1).
valid_abstract_section_header('DESCRIPTION OF MEASURE',		1).
valid_abstract_section_header('DESCRIPTION OF METHODS',		1).
valid_abstract_section_header('DESCRIPTION OF POLICY',		1).
valid_abstract_section_header('DESCRIPTION OF POLICY AND CASE',		1).
valid_abstract_section_header('DESCRIPTION OF POLICY DEVELOPMENT',		1).
valid_abstract_section_header('DESCRIPTION OF PURPOSE',		1).
valid_abstract_section_header('DESCRIPTION OF SERVICES',		1).
valid_abstract_section_header('DESCRIPTION OF THE DISEASE',		1).
valid_abstract_section_header('DESCRIPTION OF THE EXAMINATION',		1).
valid_abstract_section_header('DESCRIPTION OF THE HIE AND BASE INFRASTRUCTURE',		1).
valid_abstract_section_header('DESCRIPTION OF THE INDEX',		1).
valid_abstract_section_header('DESCRIPTION OF THE INSTITUTIONAL TRAINING PROGRAM',		1).
valid_abstract_section_header('DESCRIPTION OF THE MODEL',		1).
valid_abstract_section_header('DESCRIPTION OF THE PROCEDURE',		1).
valid_abstract_section_header('DESCRIPTION OF THE PROGRAMME',		1).
valid_abstract_section_header('DESCRIPTION OF THE SITUATION',		1).
valid_abstract_section_header('DESCRIPTIVE EPIDEMIOLOGY',		1).
valid_abstract_section_header('DESCRIZIONE DEL CASO',		1).
valid_abstract_section_header('DESENHO',		1).
valid_abstract_section_header('DESIG',		1).
valid_abstract_section_header('DESIGN & DEFINITION',		1).
valid_abstract_section_header('DESIGN & INTERVENTION',		1).
valid_abstract_section_header('DESIGN & METHODOLOGY',		1).
valid_abstract_section_header('DESIGN (MATERIALS AND METHODS)',		1).
valid_abstract_section_header('DESIGN / OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN AND AIM',		1).
valid_abstract_section_header('DESIGN AND CONTENT',		1).
valid_abstract_section_header('DESIGN AND DATA',		1).
valid_abstract_section_header('DESIGN AND DATA ANALYSIS',		1).
valid_abstract_section_header('DESIGN AND DEVELOPMENT',		1).
valid_abstract_section_header('DESIGN AND FINDINGS',		1).
valid_abstract_section_header('DESIGN AND FUNCTIONS',		1).
valid_abstract_section_header('DESIGN AND HYPOTHESIS',		1).
valid_abstract_section_header('DESIGN AND IMPLEMENTATION',		1).
valid_abstract_section_header('DESIGN AND INTERVIEW',		1).
valid_abstract_section_header('DESIGN AND MAIN RESULTS',		1).
valid_abstract_section_header('DESIGN AND MEASUREMENT PROCEDURES',		1).
valid_abstract_section_header('DESIGN AND OUTCOME MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN AND OUTCOMES',		1).
valid_abstract_section_header('DESIGN AND PURPOSE',		1).
valid_abstract_section_header('DESIGN AND RATIONALE',		1).
valid_abstract_section_header('DESIGN AND REVIEW METHOD',		1).
valid_abstract_section_header('DESIGN AND SETTING AND MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN AND SETTING AND PATIENTS',		1).
valid_abstract_section_header('DESIGN AND SETTING/PARTICIPANTS',		1).
valid_abstract_section_header('DESIGN AND STATISTICAL ANALYSES',		1).
valid_abstract_section_header('DESIGN AND STRATEGY',		1).
valid_abstract_section_header('DESIGN AND SUBJECT',		1).
valid_abstract_section_header('DESIGN AND SUBJECTS, AND METHODS',		1).
valid_abstract_section_header('DESIGN AND TREATMENT',		1).
valid_abstract_section_header('DESIGN APPROACH',		1).
valid_abstract_section_header('DESIGN CONCEPTS',		1).
valid_abstract_section_header('DESIGN CROSS-SECTIONAL STUDY SETTING',		1).
valid_abstract_section_header('DESIGN HYPERTENSIVE',		1).
valid_abstract_section_header('DESIGN MATERIALS AND METHODS',		1).
valid_abstract_section_header('DESIGN METHODS AND STATISTICAL ANALYSIS',		1).
valid_abstract_section_header('DESIGN NMA DATA SOURCES',		1).
valid_abstract_section_header('DESIGN OF CLINICAL TRIAL',		1).
valid_abstract_section_header('DESIGN OF METHODS',		1).
valid_abstract_section_header('DESIGN OF STUDY AND RESULTS',		1).
valid_abstract_section_header('DESIGN OF THE CONFERENCE',		1).
valid_abstract_section_header('DESIGN OF THE PROGRAM',		1).
valid_abstract_section_header('DESIGN OF THE RESEARCH',		1).
valid_abstract_section_header('DESIGN OR SETTING',		1).
valid_abstract_section_header('DESIGN PARTICIPANTS & MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN PARTICIPANTS AND INTERVENTIONS',		1).
valid_abstract_section_header('DESIGN PARTICIPANTS AND METHODS',		1).
valid_abstract_section_header('DESIGN PATIENTS',		1).
valid_abstract_section_header('DESIGN PATIENTS, INTERVENTION',		1).
valid_abstract_section_header('DESIGN POSTINTERVENTION',		1).
valid_abstract_section_header('DESIGN PRINCIPLES OF AN EDUCATIONAL LIGHT SHEET MICROSCOPE',		1).
valid_abstract_section_header('DESIGN PROCESS',		1).
valid_abstract_section_header('DESIGN REPRODUCTIVE',		1).
valid_abstract_section_header('DESIGN RETROSPECTIVE STUDY PATIENTS',		1).
valid_abstract_section_header('DESIGN SETTING AND PARTICIPATES',		1).
valid_abstract_section_header('DESIGN SETTING AND SAMPLE',		1).
valid_abstract_section_header('DESIGN SETTING ANDPARTICIPANTS',		1).
valid_abstract_section_header('DESIGN SETTING PATIENTS AND INTERVENTION',		1).
valid_abstract_section_header('DESIGN SETTING PATIENTS AND INTERVENTIONS',		1).
valid_abstract_section_header('DESIGN SETTING, AND PARTICIPANTS',		1).
valid_abstract_section_header('DESIGN SETTING, PARTICIPANTS AND MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN SETTING, PARTICIPANTS, AND MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN SETTING, PARTICIPANTS, INTERVENTION, MEASUREMENTS AND RESULTS',		1).
valid_abstract_section_header('DESIGN SETTINGS',		1).
valid_abstract_section_header('DESIGN SETTINGS PARTICIPANTS & MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN, COHORT STUDY, AND RESULTS',		1).
valid_abstract_section_header('DESIGN, DATA COLLECTION AND ANALYSIS',		1).
valid_abstract_section_header('DESIGN, DATA COLLECTION, METHODS',		1).
valid_abstract_section_header('DESIGN, INTERVENTION AND MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN, MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN, MAIN OUTCOME MEASURES, AND RESULTS',		1).
valid_abstract_section_header('DESIGN, MEASUREMENT, AND RESULTS',		1).
valid_abstract_section_header('DESIGN, MEASUREMENTS AND MAIN RESULTS',		1).
valid_abstract_section_header('DESIGN, MEASUREMENTS, AND RESULTS',		1).
valid_abstract_section_header('DESIGN, METHOD, PARTICIPANTS AND SETTING',		1).
valid_abstract_section_header('DESIGN, METHODS',		1).
valid_abstract_section_header('DESIGN, METHODS AND MEASURES',		1).
valid_abstract_section_header('DESIGN, PARTICIPANTS AND INTERVENTION',		1).
valid_abstract_section_header('DESIGN, PARTICIPANTS, & MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN, PARTICIPANTS, AND APPROACH',		1).
valid_abstract_section_header('DESIGN, PARTICIPANTS, INTERVENTION, SETTING',		1).
valid_abstract_section_header('DESIGN, PARTICIPANTS, INTERVENTIONS AND MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN, PARTICIPANTS, SETTINGS, & METHODS',		1).
valid_abstract_section_header('DESIGN, PATIENTS AND INTERVENTIONS',		1).
valid_abstract_section_header('DESIGN, PATIENTS, & SETTING',		1).
valid_abstract_section_header('DESIGN, PATIENTS, AND MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN, PATIENTS, AND RESULTS',		1).
valid_abstract_section_header('DESIGN, PATIENTS, METHODS',		1).
valid_abstract_section_header('DESIGN, POPULATION AND SAMPLE SIZE',		1).
valid_abstract_section_header('DESIGN, SAMPLE, AND MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN, SETTING AND DATA SOURCE',		1).
valid_abstract_section_header('DESIGN, SETTING AND INTERVENTION',		1).
valid_abstract_section_header('DESIGN, SETTING AND OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN, SETTING AND PARTICIPANT',		1).
valid_abstract_section_header('DESIGN, SETTING AND POPULATIONS',		1).
valid_abstract_section_header('DESIGN, SETTING, AND ANIMAL SUBJECTS',		1).
valid_abstract_section_header('DESIGN, SETTING, AND EXPOSURES',		1).
valid_abstract_section_header('DESIGN, SETTING, AND PARTICIPATIONS',		1).
valid_abstract_section_header('DESIGN, SETTING, AND SAMPLES',		1).
valid_abstract_section_header('DESIGN, SETTING, AND SPECIMENS',		1).
valid_abstract_section_header('DESIGN, SETTING, AND, PARTICIPANTS',		1).
valid_abstract_section_header('DESIGN, SETTING, MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN, SETTING, METHODS',		1).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS AND OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, & OBJECTIVES',		1).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, AND MEASUREMENT',		1).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, INTERVENTION, AND OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN, SETTING, PARTICIPANTS, METHODS AND FINDINGS',		1).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS AND INTERVENTION',		1).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS, INTERVENTIONS AND RESULTS',		1).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS, INTERVENTIONS, AND MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN, SETTING, PATIENTS, OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN, SETTING, POPULATION',		1).
valid_abstract_section_header('DESIGN, SETTING, SUBJECTS AND INTERVENTIONS',		1).
valid_abstract_section_header('DESIGN, SETTING, SUBJECTS AND METHODS',		1).
valid_abstract_section_header('DESIGN, SETTING, SUBJECTS, AND INTERVENTIONS',		1).
valid_abstract_section_header('DESIGN, SETTINGS AND PATIENTS',		1).
valid_abstract_section_header('DESIGN, SETTINGS, AND POPULATION',		1).
valid_abstract_section_header('DESIGN, SETTINGS, PARTICIPANTS AND INTERVENTIONS',		1).
valid_abstract_section_header('DESIGN, SETTINGS, PARTICIPANTS, & METHODS',		1).
valid_abstract_section_header('DESIGN, SIZE AND DURATION',		1).
valid_abstract_section_header('DESIGN, STUDY, PARTICIPANTS, & MEASUREMENTS',		1).
valid_abstract_section_header('DESIGN, SUBJECTS AND MAIN MEASURES',		1).
valid_abstract_section_header('DESIGN, SUBJECTS, MEASUREMENT',		1).
valid_abstract_section_header('DESIGN, TIME AND SETTING',		1).
valid_abstract_section_header('DESIGN-RESULTS',		1).
valid_abstract_section_header('DESIGN/ANALYSIS',		1).
valid_abstract_section_header('DESIGN/APPROACH',		1).
valid_abstract_section_header('DESIGN/DATA',		1).
valid_abstract_section_header('DESIGN/DATA SOURCES',		1).
valid_abstract_section_header('DESIGN/DESIGN',		1).
valid_abstract_section_header('DESIGN/INTERVENTION/PARTICIPANTS',		1).
valid_abstract_section_header('DESIGN/MAIN OUTCOME MEASURE',		1).
valid_abstract_section_header('DESIGN/MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN/METHODS/RESULTS',		1).
valid_abstract_section_header('DESIGN/METHODS/SETTING',		1).
valid_abstract_section_header('DESIGN/PATIENT/SETTING',		1).
valid_abstract_section_header('DESIGN/PATIENTS/SETTING',		1).
valid_abstract_section_header('DESIGN/SETTING/MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN/SETTING/METHODOLOGY/APPROACH',		1).
valid_abstract_section_header('DESIGN/SETTING/METHODS',		1).
valid_abstract_section_header('DESIGN/SETTING/PARTICIPANTS/INTERVENTION',		1).
valid_abstract_section_header('DESIGN/SETTING/PARTICIPANTS/INTERVENTIONS/MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN/SETTING/PARTICIPANTS/MEASUREMENT',		1).
valid_abstract_section_header('DESIGN/SETTING/PARTICIPANTS/METHODS',		1).
valid_abstract_section_header('DESIGN/SETTING/PATIENTS/INTERVENTIONS/OUTCOME MEASURES',		1).
valid_abstract_section_header('DESIGN/SETTINGS/PARTICIPANTS/METHODS',		1).
valid_abstract_section_header('DESIGN/SUBJECT',		1).
valid_abstract_section_header('DESIGN/SUBJECTS/MAIN MEASURES',		1).
valid_abstract_section_header('DESIGN/SUBJECTS/RESULTS',		1).
valid_abstract_section_header('DESIGNED',		1).
valid_abstract_section_header('DESIGNING A SOLUTION',		1).
valid_abstract_section_header('DESIGNING THE PROJECT',		1).
valid_abstract_section_header('DESIGNS AND SETTINGS',		1).
valid_abstract_section_header('DESIGNS, SETTING, PARTICIPANTS, & MEASUREMENTS',		1).
valid_abstract_section_header('DESIGNS/METHODS',		1).
valid_abstract_section_header('DESIGNS/STUDY POPULATIONS',		1).
valid_abstract_section_header('DETAILS OF THE CASES',		1).
valid_abstract_section_header('DETECTION AID',		1).
valid_abstract_section_header('DETECTION AND TREATMENT',		1).
valid_abstract_section_header('DETERMINANTS OF DIASTOLIC FILLING',		1).
valid_abstract_section_header('DETERMINANTS OF PROXIMAL AORTA REMODELING',		1).
valid_abstract_section_header('DETERMINANTS/CONSEQUENCES OF POP',		1).
valid_abstract_section_header('DETERMINATIONS',		1).
valid_abstract_section_header('DETERMINING THE SCOPE OF THE GUIDE',		1).
valid_abstract_section_header('DEVELOPER AND FUNDING',		1).
valid_abstract_section_header('DEVELOPING A STRATEGIC APPROACH TO CONFLICT MANAGEMENT',		1).
valid_abstract_section_header('DEVELOPING ALTERNATIVES',		1).
valid_abstract_section_header('DEVELOPING INDICATORS OF QUALITY OF CARE',		1).
valid_abstract_section_header('DEVELOPING OBJECTIVE AND UNIVERSAL MEASURES',		1).
valid_abstract_section_header('DEVELOPING PRISM',		1).
valid_abstract_section_header('DEVELOPING THE CAMPAIGN',		1).
valid_abstract_section_header('DEVELOPING THE CLINICAL PATHWAY CONSTRUCTOR (CPC)',		1).
valid_abstract_section_header('DEVELOPING THE COLLABORATIVE STRATEGY',		1).
valid_abstract_section_header('DEVELOPING THE NATIONAL QUALITY IMPROVEMENT SYSTEM',		1).
valid_abstract_section_header('DEVELOPING THE PI PLAN',		1).
valid_abstract_section_header('DEVELOPMENT AND METHODOLOGY',		1).
valid_abstract_section_header('DEVELOPMENT AND ROLE OF THE GROUP',		1).
valid_abstract_section_header('DEVELOPMENT AND TESTING OF THE ONTOLOGY',		1).
valid_abstract_section_header('DEVELOPMENT DURING THE LAST 100 YEARS',		1).
valid_abstract_section_header('DEVELOPMENT OF A DECISION-MAKING MODEL',		1).
valid_abstract_section_header('DEVELOPMENT OF ACTION PLANS',		1).
valid_abstract_section_header('DEVELOPMENT OF ASSAYS FOR HEPATITIS VIRUS',		1).
valid_abstract_section_header('DEVELOPMENT OF AUTOIMMUNITY',		1).
valid_abstract_section_header('DEVELOPMENT OF CARDIAC CELLS',		1).
valid_abstract_section_header('DEVELOPMENT OF FMECA PROCEDURE AND TOOL',		1).
valid_abstract_section_header('DEVELOPMENT OF INDICATORS',		1).
valid_abstract_section_header('DEVELOPMENT OF KT PROGRAMME',		1).
valid_abstract_section_header('DEVELOPMENT OF MEDICINE AND PUBLIC HEALTH AT THE AGA KHAN UNIVERSITY',		1).
valid_abstract_section_header('DEVELOPMENT OF PATHWAYS AND ALGORITHMS',		1).
valid_abstract_section_header('DEVELOPMENT OF PROGRAM',		1).
valid_abstract_section_header('DEVELOPMENT OF QUALITY INDICATORS',		1).
valid_abstract_section_header('DEVELOPMENT OF ROBOTICS AND ITS APPLICATION IN MEDICINE',		1).
valid_abstract_section_header('DEVELOPMENT OF THE ACTIVITY',		1).
valid_abstract_section_header('DEVELOPMENT OF THE GUIDELINES',		1).
valid_abstract_section_header('DEVELOPMENT OF THE METHOD',		1).
valid_abstract_section_header('DEVELOPMENT OF THE PROGRAM',		1).
valid_abstract_section_header('DEVELOPMENT OF THE PROTOCOL',		1).
valid_abstract_section_header('DEVELOPMENT OF THE QUESTIONNAIRE',		1).
valid_abstract_section_header('DEVELOPMENT OF THE RESOURCE',		1).
valid_abstract_section_header('DEVELOPMENT OF THE SOFTWARE TOOL',		1).
valid_abstract_section_header('DEVELOPMENT OF THE TREATMENT PROGRAMME',		1).
valid_abstract_section_header('DEVELOPMENT PRINCIPLES',		1).
valid_abstract_section_header('DEVELOPMENT PROCESS',		1).
valid_abstract_section_header('DEVELOPMENT, THEORY AND APPLICATION OF LEAN THINKING TO HEALTH CARE',		1).
valid_abstract_section_header('DEVELOPMENTAL DEFICITS',		1).
valid_abstract_section_header('DEVELOPMENTS AND THEIR IMPLICATIONS',		1).
valid_abstract_section_header('DEVELOPMENTS IN EDUCATION AND ASSESSMENT',		1).
valid_abstract_section_header('DEVELOPMENTS IN MEDICAL EDUCATION',		1).
valid_abstract_section_header('DEVICES REVIEWED',		1).
valid_abstract_section_header('DIABETES MELLITUS',		1).
valid_abstract_section_header('DIABETIC NEPHROPATHY AND HYPERTENSION',		1).
valid_abstract_section_header('DIABETIC NEUROPATHY',		1).
valid_abstract_section_header('DIAGNOSES, INTERVENTIONS, AND OUTCOMES',		1).
valid_abstract_section_header('DIAGNOSIS AND CLASSIFICATION OF UTI',		1).
valid_abstract_section_header('DIAGNOSIS AND COMPLICATIONS',		1).
valid_abstract_section_header('DIAGNOSIS AND CONCLUSION',		1).
valid_abstract_section_header('DIAGNOSIS AND HISTORY OF DISEASE',		1).
valid_abstract_section_header('DIAGNOSIS AND INTERVENTION',		1).
valid_abstract_section_header('DIAGNOSIS AND OUTCOME',		1).
valid_abstract_section_header('DIAGNOSIS AND RISK STRATIFICATION',		1).
valid_abstract_section_header('DIAGNOSIS AND THERAPY OF DISEASES OF THE LARYNX',		1).
valid_abstract_section_header('DIAGNOSIS OF ABDOMINAL AORTIC ANEURYSMS',		1).
valid_abstract_section_header('DIAGNOSIS OF BORRELIA',		1).
valid_abstract_section_header('DIAGNOSIS OF CELIAC DISEASE',		1).
valid_abstract_section_header('DIAGNOSIS OF CMV',		1).
valid_abstract_section_header('DIAGNOSIS OF CONCUSSION',		1).
valid_abstract_section_header('DIAGNOSIS OF CUP',		1).
valid_abstract_section_header('DIAGNOSIS OF GAS INFECTIONS',		1).
valid_abstract_section_header('DIAGNOSIS OF GYNECOLOGICAL DISEASES',		1).
valid_abstract_section_header('DIAGNOSIS OF MALE INFERTILITY',		1).
valid_abstract_section_header('DIAGNOSIS OF PRIMARY ALDOSTERONISM',		1).
valid_abstract_section_header('DIAGNOSIS OF RADIATION PNEUMONITIS',		1).
valid_abstract_section_header('DIAGNOSIS OF RHEUMATOLOGIC DISEASE IN FINGERS',		1).
valid_abstract_section_header('DIAGNOSIS OF RHEUMATOLOGIC DISEASE IN WRISTS',		1).
valid_abstract_section_header('DIAGNOSIS OF TB',		1).
valid_abstract_section_header('DIAGNOSIS, THERAPY, AND CLINICAL COURSE',		1).
valid_abstract_section_header('DIAGNOSIS, TREATMENT AND FOLLOW UP',		1).
valid_abstract_section_header('DIAGNOSIS, TREATMENT AND RESPONSE TO THERAPY',		1).
valid_abstract_section_header('DIAGNOSIS/EPIDEMIOLOGY/THERAPY OF TMD',		1).
valid_abstract_section_header('DIAGNOSTIC APPROACH',		1).
valid_abstract_section_header('DIAGNOSTIC CRITERIA AND FIBROMYALGIA ASSESSMENT',		1).
valid_abstract_section_header('DIAGNOSTIC DILEMMAS',		1).
valid_abstract_section_header('DIAGNOSTIC FEATURES, TREATMENT AND COURSE',		1).
valid_abstract_section_header('DIAGNOSTIC IMPLICATIONS',		1).
valid_abstract_section_header('DIAGNOSTIC INNOVATIONS',		1).
valid_abstract_section_header('DIAGNOSTIC JOURNEY',		1).
valid_abstract_section_header('DIAGNOSTIC PROCEDURES AND TREATMENT',		1).
valid_abstract_section_header('DIAGNOSTIC REFERRAL SHEET',		1).
valid_abstract_section_header('DIAGNOSTIC STANDARD',		1).
valid_abstract_section_header('DIAGNOSTICS AND CLASSIFICATION',		1).
valid_abstract_section_header('DIAGNOSTICS AND CLINICAL COURSE',		1).
valid_abstract_section_header('DIAGNOSTICS AND COURSE',		1).
valid_abstract_section_header('DIAGNOSTICS, THERAPY, AND CLINICAL COURSE',		1).
valid_abstract_section_header('DIALECTIC OF SURVEY DEVELOPMENT',		1).
valid_abstract_section_header('DIAPHANOSCOPY OF THE LARYNX',		1).
valid_abstract_section_header('DIAPHANOSCOPY OF THE PARANASAL SINUSES',		1).
valid_abstract_section_header('DIARET SK',		1).
valid_abstract_section_header('DIASTOLIC MYOCARDIAL VELOCITY PROFILE',		1).
valid_abstract_section_header('DICUSSION AND CONCLUSION',		1).
valid_abstract_section_header('DIETARY FACTORS',		1).
valid_abstract_section_header('DIETARY FAT AND FAT BALANCE',		1).
valid_abstract_section_header('DIETARY PLANT FIBER',		1).
valid_abstract_section_header('DIETARY SUPPLEMENTATION',		1).
valid_abstract_section_header('DIFFERENCES BETWEEN ANG II ANTAGONISTS AND ACE INHIBITORS',		1).
valid_abstract_section_header('DIFFERENCES BETWEEN LOSARTAN AND ACE INHIBITORS',		1).
valid_abstract_section_header('DIFFERENCES IN DAILY DOSES BETWEEN DIFFERENT GEOGRAPHIC REGIONS',		1).
valid_abstract_section_header('DIFFERENCES IN DIAGNOSES',		1).
valid_abstract_section_header('DIFFERENCES, AGREEMENTS',		1).
valid_abstract_section_header('DIFFERENT ADVERSE EFFECTS',		1).
valid_abstract_section_header('DIFFERENT DESCRIPTIVE PARAMETERS MAY BE PROPOSED',		1).
valid_abstract_section_header('DIFFERENT TYPES OF STANDARDIZED QUESTIONNAIRES FOR QUALITY OF LIFE EVALUATION',		1).
valid_abstract_section_header('DIFFERENTIAL DIAGNOSIS OF THE OPERATIVE SPECIMEN',		1).
valid_abstract_section_header('DIFFERENTIAL UBIQUITINATION OF THE DOPAMINE D4 RECEPTOR POLYMORPHIC VARIANTS',		1).
valid_abstract_section_header('DIFFERENTIATION',		1).
valid_abstract_section_header('DIGIT SUCKING',		1).
valid_abstract_section_header('DIGITAL EXTRACTION OF THE TONSILS',		1).
valid_abstract_section_header('DIGITAL SLIDES IN PATHOLOGY',		1).
valid_abstract_section_header('DIPYRIDAMOLE',		1).
valid_abstract_section_header('DIRECT AND INDIRECT OUTCOME MEASURES',		1).
valid_abstract_section_header('DIRECT ORAL ANTICOAGULANTS DOAC',		1).
valid_abstract_section_header('DIRECTIONS FOR SURGICAL TREATMENT',		1).
valid_abstract_section_header('DIRECTIONS FOR THE NEXT DECADE',		1).
valid_abstract_section_header('DIRECTIVE CONTROL',		1).
valid_abstract_section_header('DISCLOSURE AND MANAGEMENT OF CONFLICTS OF INTEREST',		1).
valid_abstract_section_header('DISCLOSURE POLICY AND PROCEDURE',		1).
valid_abstract_section_header('DISCLOSURE STATEMENT',		1).
valid_abstract_section_header('DISCLOSURES AND FUNDING',		1).
valid_abstract_section_header('DISCRIMINANT VALIDITY',		1).
valid_abstract_section_header('DISCURSIVE ANALYSIS',		1).
valid_abstract_section_header('DISCUSSANTS',		1).
valid_abstract_section_header('DISCUSSION & EVALUATION',		1).
valid_abstract_section_header('DISCUSSION & SUMMARY',		1).
valid_abstract_section_header('DISCUSSION - CONCLUSION DISCUSSION',		1).
valid_abstract_section_header('DISCUSSION / CONCLUSIONS',		1).
valid_abstract_section_header('DISCUSSION AND (PRELIMINARY) CONCLUSION',		1).
valid_abstract_section_header('DISCUSSION AND ARGUMENTS',		1).
valid_abstract_section_header('DISCUSSION AND BASELINE DESCRIPTION',		1).
valid_abstract_section_header('DISCUSSION AND CASE REPORT',		1).
valid_abstract_section_header('DISCUSSION AND CLINICAL SIGNIFICANCE',		1).
valid_abstract_section_header('DISCUSSION AND CONCLUSIONS:',		1).
valid_abstract_section_header('DISCUSSION AND EVALUATION CONCLUSIONS',		1).
valid_abstract_section_header('DISCUSSION AND IMPLICATION',		1).
valid_abstract_section_header('DISCUSSION AND PRACTICE IMPLICATIONS',		1).
valid_abstract_section_header('DISCUSSION AND PROJECTIONS',		1).
valid_abstract_section_header('DISCUSSION AND RECOMMENDATION',		1).
valid_abstract_section_header('DISCUSSION AND REVIEW',		1).
valid_abstract_section_header('DISCUSSION AND SIGNIFICANCE',		1).
valid_abstract_section_header('DISCUSSION AND SUMMARY/CONCLUSIONS',		1).
valid_abstract_section_header('DISCUSSION AND THERAPEUTIC CONCLUSIONS',		1).
valid_abstract_section_header('DISCUSSION EVALUATION AND CONCLUSION',		1).
valid_abstract_section_header('DISCUSSION FOR READERS',		1).
valid_abstract_section_header('DISCUSSION FOR RESEARCHERS',		1).
valid_abstract_section_header('DISCUSSION OF FORUM THEMES',		1).
valid_abstract_section_header('DISCUSSION OF THE POSSIBLE ROLE OF THE ICF IN EBM',		1).
valid_abstract_section_header('DISCUSSION POINTS',		1).
valid_abstract_section_header('DISCUSSION(AND CONCLUSION)',		1).
valid_abstract_section_header('DISCUSSION, CONCLUSIONS, AND SCIENTIFIC SIGNIFICANCE',		1).
valid_abstract_section_header('DISCUSSION, IMPLICATIONS, AND FOLLOW-UP',		1).
valid_abstract_section_header('DISCUSSION/ CONCLUSION',		1).
valid_abstract_section_header('DISCUSSION/ CONCLUSIONS',		1).
valid_abstract_section_header('DISCUSSION/CONCLUSION/RECOMMENDATIONS FOR PRACTICE',		1).
valid_abstract_section_header('DISCUSSION/CONCLUSIONS/SCIENTIFIC SIGNIFICANCE',		1).
valid_abstract_section_header('DISCUSSION/CONLUSION',		1).
valid_abstract_section_header('DISCUSSION/IMPLICATIONS FOR NURSING AND HEALTH POLICY',		1).
valid_abstract_section_header('DISCUSSION/INTERPRETATION',		1).
valid_abstract_section_header('DISCUSSION/MAIN EXPECTED RESULTS',		1).
valid_abstract_section_header('DISCUSSION/RELEVANCE TO CLINICAL PRACTICE',		1).
valid_abstract_section_header('DISCUSSION/RESULTS/CONCLUSION',		1).
valid_abstract_section_header('DISCUSSION/TECHNIQUE',		1).
valid_abstract_section_header('DISCUSSIONE E COMMENTI',		1).
valid_abstract_section_header('DISCUSSIONE E CONLUSIONI',		1).
valid_abstract_section_header('DISCUSSIONE/CONCLUSIONI',		1).
valid_abstract_section_header('DISEASE AGENT',		1).
valid_abstract_section_header('DISEASE COURSE AND TREATMENT',		1).
valid_abstract_section_header('DISEASE OUTCOME AND TREATMENT',		1).
valid_abstract_section_header('DISEASE PATTERN',		1).
valid_abstract_section_header('DISEASE SEQUELAE',		1).
valid_abstract_section_header('DISEASE SYMPTOMS AND HOST RANGE',		1).
valid_abstract_section_header('DISEASES AND APOPTOSIS',		1).
valid_abstract_section_header('DISEASES SCREENED',		1).
valid_abstract_section_header('DISINFECTANTS TO AVOID',		1).
valid_abstract_section_header('DISQUALIFICATION FROM SPORT',		1).
valid_abstract_section_header('DISSCUSION',		1).
valid_abstract_section_header('DISSCUSSION',		1).
valid_abstract_section_header('DISSCUSSION AND CONCLUSION',		1).
valid_abstract_section_header('DISSCUTION',		1).
valid_abstract_section_header('DISSEMINATION AND IMPLEMENTATION IMPLICATIONS',		1).
valid_abstract_section_header('DISSEMINATION AND RESULTS',		1).
valid_abstract_section_header('DISTINCT ENTITIES',		1).
valid_abstract_section_header('DISTINCTIONS AMONG DIHYDROPYRIDINE CALCIUM ANTAGONISTS',		1).
valid_abstract_section_header('DISTRIBUTED RESEARCH NETWORK MODEL',		1).
valid_abstract_section_header('DISTURBANCE OF WOUND HEALING AFTER RADIATION',		1).
valid_abstract_section_header('DISTURBANCES OF FIBRINOLYTIC SYSTEM',		1).
valid_abstract_section_header('DIURETIC THERAPY',		1).
valid_abstract_section_header('DIURNAL CURVES FOR INTRAOCULAR PRESSURE MEASUREMENT',		1).
valid_abstract_section_header('DIVERGENCES BETWEEN TELEMEDICINE AT SEA AND ONSHORE',		1).
valid_abstract_section_header('DOCETAXEL IN COMBINATION CHEMOTHERAPY',		1).
valid_abstract_section_header('DOCUMENTARY SOURCE',		1).
valid_abstract_section_header('DOCUMENTARY SOURCES',		1).
valid_abstract_section_header('DOCUMENTARY SOURCES (KEY WORDS AND LANGUAGE)',		1).
valid_abstract_section_header('DOCUMENTARY SOURCES AND STUDY SELECTION',		1).
valid_abstract_section_header('DOCUMENTATION AND ANALYSIS OF DATA',		1).
valid_abstract_section_header('DOMAINS OF IMPROVEMENT',		1).
valid_abstract_section_header('DONOR DISSECTION',		1).
valid_abstract_section_header('DONOR ORGANS',		1).
valid_abstract_section_header('DONORS PATIENTS AND METHODS',		1).
valid_abstract_section_header('DOPAMINE D4 RECEPTOR POLYMORPHISM',		1).
valid_abstract_section_header('DOPPLER ANOMALIES',		1).
valid_abstract_section_header('DOSE AND ROUTE OF ADMINISTRATION',		1).
valid_abstract_section_header('DOSE ASSESSMENT IN PERSONNEL AND PATIENT',		1).
valid_abstract_section_header('DOSING, SAFETY AND EFFICACY',		1).
valid_abstract_section_header('DOX-MI',		1).
valid_abstract_section_header('DRAFTING AND IMPLEMENTATION',		1).
valid_abstract_section_header('DRAINAGE',		1).
valid_abstract_section_header('DRI TRIAL REGISTRATION',		1).
valid_abstract_section_header('DRUG APPROVAL',		1).
valid_abstract_section_header('DRUG COMBINATIONS',		1).
valid_abstract_section_header('DRUG EFFECTS',		1).
valid_abstract_section_header('DRUG ELUTING STENTS',		1).
valid_abstract_section_header('DRUG INTERACTION',		1).
valid_abstract_section_header('DRUG STUDIES',		1).
valid_abstract_section_header('DRUG SUSCEPTIBILITY TEST',		1).
valid_abstract_section_header('DRUG TARGETS',		1).
valid_abstract_section_header('DRUG TREATMENT',		1).
valid_abstract_section_header('DRUGS AND FALLS',		1).
valid_abstract_section_header('DRUGS IMPLICATED',		1).
valid_abstract_section_header('DRUGS THAT MODULATE THE SYMPATHETIC NERVOUS SYSTEM',		1).
valid_abstract_section_header('DRUGS USED FOR MEDICAL ABORTION',		1).
valid_abstract_section_header('DSM-IV) METHODS',		1).
valid_abstract_section_header('DUAL MEG SETUP',		1).
valid_abstract_section_header('DURATION OF CIRCULATORY ASSISTANCE',		1).
valid_abstract_section_header('DURATION OF LIGHT THERAPY',		1).
valid_abstract_section_header('DURATION OF RETENTION',		1).
valid_abstract_section_header('DURATION OF STUDY',		1).
valid_abstract_section_header('DURATION OF THE PROGRAMME',		1).
valid_abstract_section_header('DURING IRRADIATION',		1).
valid_abstract_section_header('DUTCH TRIAL REGISTRATION NUMBER',		1).
valid_abstract_section_header('DUTCH TRIAL REGISTRY',		1).
valid_abstract_section_header('DYSFUNCTION',		1).
valid_abstract_section_header('DYSFUNCTION OF THE ENDOTHELIUM',		1).
valid_abstract_section_header('DYSPHONIA AND DYSPNEA',		1).
valid_abstract_section_header('Data Sources and Study Selection',		1).
valid_abstract_section_header('Data Sources, Study Selection, and Data Synthesis',		1).
valid_abstract_section_header('Data, Setting, and Participants',		1).
valid_abstract_section_header('Design, Methods, and Participants',		1).
valid_abstract_section_header('Design, Setting, and Animal Models',		1).
valid_abstract_section_header('Design, Setting, and Exposures',		1).
valid_abstract_section_header('Design, Setting, and Materials',		1).
valid_abstract_section_header('Design, Setting, and Participant',		1).
valid_abstract_section_header('Discussion:',		1).
valid_abstract_section_header('EARLY MULTIDISCIPLINARY CARE',		1).
valid_abstract_section_header('EARLY OBSERVATIONS',		1).
valid_abstract_section_header('EARLY STAGES OF SURGERY',		1).
valid_abstract_section_header('EARLY SYNDROMES',		1).
valid_abstract_section_header('EARTHWORMS HAVE SEVERAL NAMES IN DIFFERENT COUNTRIES (IN CHINESE',		1).
valid_abstract_section_header('EATING HABITS',		1).
valid_abstract_section_header('EBCT AND CAD',		1).
valid_abstract_section_header('EBM LEVEL V',		1).
valid_abstract_section_header('ECCENTRIC EXERCISES',		1).
valid_abstract_section_header('ECHOCARDIOGRAPHIC DEGREES OF SEVERITY',		1).
valid_abstract_section_header('ECHOCARDIOGRAPHIC FEATURES',		1).
valid_abstract_section_header('ECHOCARDIOGRAPHIC METHODS FOR ESTIMATION OF SEVERITY',		1).
valid_abstract_section_header('ECOG PS',		1).
valid_abstract_section_header('ECOHUMANIST DESIGN STRATEGIES',		1).
valid_abstract_section_header('ECOLOGICAL IMPLICATIONS',		1).
valid_abstract_section_header('ECONOMIC EVALUATION',		1).
valid_abstract_section_header('ECONOMIC OUTCOME MEASURES',		1).
valid_abstract_section_header('ECTOPIC URETEROCELE',		1).
valid_abstract_section_header('ED TREATMENT',		1).
valid_abstract_section_header('EDEMA EVALUATION',		1).
valid_abstract_section_header('EDITOR',		1).
valid_abstract_section_header('EDUCATION OF PATIENTS',		1).
valid_abstract_section_header('EDUCATIONAL FRAMEWORK',		1).
valid_abstract_section_header('EDUCATIONAL LEVEL',		1).
valid_abstract_section_header('EDUCATIONAL STRATEGIES',		1).
valid_abstract_section_header('EDUCATIONAL THEORY',		1).
valid_abstract_section_header('EFFECT OF ANGIOTENSIN CONVERTING ENZYME (ACE) INHIBITORS',		1).
valid_abstract_section_header('EFFECT OF ANTIHYPERTENSIVE TREATMENT',		1).
valid_abstract_section_header('EFFECT OF CHANGES ON MEDULLARY BLOOD FLOW',		1).
valid_abstract_section_header('EFFECT OF LOSARTAN',		1).
valid_abstract_section_header('EFFECT OF MODERN ANTIHYPERTENSIVE AGENTS',		1).
valid_abstract_section_header('EFFECT OF NIACIN ON LIPID METABOLISM',		1).
valid_abstract_section_header('EFFECT OF RECEPTOR SUBTYPE',		1).
valid_abstract_section_header('EFFECT OF SMOKING IN MOVIES ON NEW ZEALAND YOUTH',		1).
valid_abstract_section_header('EFFECT OF THE PROGRAM',		1).
valid_abstract_section_header('EFFECT OF THERAPY ON ENDOTHELIAL DYSFUNCTION',		1).
valid_abstract_section_header('EFFECT-SIZE INDEXES',		1).
valid_abstract_section_header('EFFECTIVE MEASURES',		1).
valid_abstract_section_header('EFFECTIVENESS OF BIOLOGICAL THERAPEUTICS',		1).
valid_abstract_section_header('EFFECTIVENESS OF INTERVENTION TYPE',		1).
valid_abstract_section_header('EFFECTIVENESS OF INTERVENTIONS USING SPECIFIC OUTCOME CATEGORIES',		1).
valid_abstract_section_header('EFFECTIVENESS OF SMBG',		1).
valid_abstract_section_header('EFFECTS AT MOLECULAR LEVEL',		1).
valid_abstract_section_header('EFFECTS OF ANGIOTENSIN CONVERTING ENZYME (ACE) INHIBITION',		1).
valid_abstract_section_header('EFFECTS OF ANGIOTENSIN II AND ALDOSTERONE',		1).
valid_abstract_section_header('EFFECTS OF ANTIHYPERTENSIVE AGENTS',		1).
valid_abstract_section_header('EFFECTS OF BILE ACIDS AND THEIR SALTS ON ABSORPTION OF OTHER SUBSTANCES AND THEIR POTENTIAL ACTION',		1).
valid_abstract_section_header('EFFECTS OF EPO THERAPY',		1).
valid_abstract_section_header('EFFECTS OF EXERCISE TRAINING',		1).
valid_abstract_section_header('EFFECTS OF HIGH THORACIC EPIDURAL ANESTHESIA',		1).
valid_abstract_section_header('EFFECTS OF LOW LEVEL LASER THERAPY ON BIOLOGICAL SYSTEMS',		1).
valid_abstract_section_header('EFFECTS OF LOW LEVEL LASER THERAPY ON WOUND HEALING',		1).
valid_abstract_section_header('EFFECTS OF METFORMIN',		1).
valid_abstract_section_header('EFFECTS OF NEWER ANTIHYPERTENSIVE AGENTS',		1).
valid_abstract_section_header('EFFECTS OF NICARDIPINE IN THE BRAIN',		1).
valid_abstract_section_header('EFFECTS OF NICARDIPINE ON BLOOD PRESSURE',		1).
valid_abstract_section_header('EFFECTS OF PHARMACOLOGICAL INTERFERENCE',		1).
valid_abstract_section_header('EFFECTS OF PHYSICAL EXERCISE',		1).
valid_abstract_section_header('EFFECTS OF STRUCTURAL ALTERATIONS TO LARGE ARTERIES ON BLOOD PRESSURE',		1).
valid_abstract_section_header('EFFECTS OF STRUCTURAL CHANGES',		1).
valid_abstract_section_header('EFFECTS OF THIAZIDE DIURETICS',		1).
valid_abstract_section_header('EFFECTS OF VIGOROUS PHYSICAL ACTIVITY',		1).
valid_abstract_section_header('EFFECTS ON BODY SYSTEMS',		1).
valid_abstract_section_header('EFFECTS ON INSULIN',		1).
valid_abstract_section_header('EFFICACY CRVO',		1).
valid_abstract_section_header('EFFICACY OF ANGIOTENSIN CONVERTING ENZYME (ACE) INHIBITORS',		1).
valid_abstract_section_header('EFFICACY OF CALCIUM CHANNEL BLOCKERS',		1).
valid_abstract_section_header('EFFICACY OF LINEZOLIDE',		1).
valid_abstract_section_header('EFFICACY RESULTS',		1).
valid_abstract_section_header('EFFICACY TEST',		1).
valid_abstract_section_header('EFFORTS TO ENSURE ACCESS TO EXPENSIVE, PATENTED DRUGS',		1).
valid_abstract_section_header('EGD CONCLUSIONS',		1).
valid_abstract_section_header('EGEMS TO DATE',		1).
valid_abstract_section_header('ELABORATE TECHNIQUE',		1).
valid_abstract_section_header('ELAINE',		1).
valid_abstract_section_header('ELECTROMAGNETIC FIELDS',		1).
valid_abstract_section_header('ELECTRONIC DATA EXCHANGE FORMAT',		1).
valid_abstract_section_header('ELECTRONIC DATABASES USED',		1).
valid_abstract_section_header('ELECTROPHORETIC DISORDERS',		1).
valid_abstract_section_header('ELECTROPHYSIOLOGY',		1).
valid_abstract_section_header('ELEMENTS OF A SUCCESSFUL SYSTEM',		1).
valid_abstract_section_header('ELEMENTS OF SUCCESS',		1).
valid_abstract_section_header('ELIGIBILITY AND CRITERIA FOR SELECTING STUDIES',		1).
valid_abstract_section_header('ELIGIBILITY CRITERIA (PARTICIPANTS AND INTERVENTIONS)',		1).
valid_abstract_section_header('ELIGIBILITY CRITERIA AND SYNTHESIS METHODS',		1).
valid_abstract_section_header('ELIGIBILITY CRITERIA FOR SELECTION STUDIES',		1).
valid_abstract_section_header('ELIGIBILITY CRITERIA FOR STUDIES',		1).
valid_abstract_section_header('ELIGIBILITY CRITERIA PARTICIPANTS',		1).
valid_abstract_section_header('ELIGIBLE SOURCES',		1).
valid_abstract_section_header('ELISA TEST',		1).
valid_abstract_section_header('EMABS TRIAL REGISTRATION',		1).
valid_abstract_section_header('EMBOLIZATION',		1).
valid_abstract_section_header('EMERGENCY DEPARTMENT',		1).
valid_abstract_section_header('EMERGENCY PLANNING AND PREPAREDNESS',		1).
valid_abstract_section_header('EMERGING ACQUISITIONS',		1).
valid_abstract_section_header('EMERGING POINTS',		1).
valid_abstract_section_header('EMOTIONAL STRESS',		1).
valid_abstract_section_header('EMPHASIS',		1).
valid_abstract_section_header('EMPIRIC TREATMENT',		1).
valid_abstract_section_header('EMPIRICAL AND THEORETICAL DEVELOPMENTS',		1).
valid_abstract_section_header('EMPIRICAL EVIDENCE',		1).
valid_abstract_section_header('EMPIRICAL REGIMENS',		1).
valid_abstract_section_header('ENCOURAGING COMPLIANCE THROUGH ENFORCEMENT',		1).
valid_abstract_section_header('ENCOURAGING COMPLIANCE THROUGH SYSTEM STRUCTURE',		1).
valid_abstract_section_header('END OF TREATMENT',		1).
valid_abstract_section_header('ENDOMETRIAL CANCER',		1).
valid_abstract_section_header('ENDOMETRIOSIS NATURAL HISTORY',		1).
valid_abstract_section_header('ENDOSCOPES',		1).
valid_abstract_section_header('ENDOSCOPIC INTRAUTERINE SURGERY',		1).
valid_abstract_section_header('ENDOSCOPY AND DIAGNOSIS',		1).
valid_abstract_section_header('ENDOTHELIAL DYSFUNCTION',		1).
valid_abstract_section_header('ENDOTHELIAL FUNCTION',		1).
valid_abstract_section_header('ENDOTHELIAL FUNCTIONS',		1).
valid_abstract_section_header('ENDOVENOUS LASER ABLATION (EVLA) HAS TWO PITFALLS',		1).
valid_abstract_section_header('ENDPOINT DEFINITIONS',		1).
valid_abstract_section_header('ENDPOINT MEASURES',		1).
valid_abstract_section_header('ENHANCING GDL THROUGH SYSTEM STRUCTURE',		1).
valid_abstract_section_header('ENHANCING PUBLIC EDUCATION',		1).
valid_abstract_section_header('ENREGISTREMENT DE L\'ESSAI',		1).
valid_abstract_section_header('ENROLLMENT AND DIAGNOSES',		1).
valid_abstract_section_header('ENVIRONMENT AND FACILITIES',		1).
valid_abstract_section_header('ENVIRONMENTAL MODIFICATIONS',		1).
valid_abstract_section_header('ENZYME REPLACEMENT THERAPY',		1).
valid_abstract_section_header('EOSINOPHILS',		1).
valid_abstract_section_header('EPIDEMIOLOGIC METHODS',		1).
valid_abstract_section_header('EPIDEMIOLOGICAL ASPECTS AND SCREENING',		1).
valid_abstract_section_header('EPIDEMIOLOGICAL CHARACTERISTICS',		1).
valid_abstract_section_header('EPIDEMIOLOGICAL FINDINGS',		1).
valid_abstract_section_header('EPIDEMIOLOGICAL INVESTIGATION',		1).
valid_abstract_section_header('EPIDEMIOLOGY AND BACKGROUND',		1).
valid_abstract_section_header('EPIDEMIOLOGY AND CLINICAL COURSE',		1).
valid_abstract_section_header('EPIDEMIOLOGY AND CLINICAL STUDIES',		1).
valid_abstract_section_header('EPIDEMIOLOGY AND ETIOLOGY',		1).
valid_abstract_section_header('EPIDEMIOLOGY AND GENETICS',		1).
valid_abstract_section_header('EPIDEMIOLOGY AND PATHOPHYSIOLOGY',		1).
valid_abstract_section_header('EPIDEMIOLOGY OF CUP',		1).
valid_abstract_section_header('EPILEPSY',		1).
valid_abstract_section_header('EPITHELIAL CELL',		1).
valid_abstract_section_header('EQUIPMENT AND TECHNIQUES',		1).
valid_abstract_section_header('ERGOMETRY I',		1).
valid_abstract_section_header('ERGOMETRY II',		1).
valid_abstract_section_header('ERGOMETRY III',		1).
valid_abstract_section_header('ERP/GFP',		1).
valid_abstract_section_header('ERRORS OF PAIN MANAGEMENT',		1).
valid_abstract_section_header('ESD PROCEDURES',		1).
valid_abstract_section_header('ESRD PEDIATRIC PATIENTS',		1).
valid_abstract_section_header('ESSENTIAL CONSIDERATIONS',		1).
valid_abstract_section_header('ESSENTIAL METHODS',		1).
valid_abstract_section_header('ESSENTIAL POINTS',		1).
valid_abstract_section_header('ESSENTIAL THERAPEUTIC STEP',		1).
valid_abstract_section_header('ESTABLISH FINANCIAL INCENTIVES FOR INVESTING IN NURSING',		1).
valid_abstract_section_header('ESTABLISHING THE TEAMS',		1).
valid_abstract_section_header('ESTHETIC ASPECT',		1).
valid_abstract_section_header('ETHANOPHARMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHICAL APPROVAL NUMBER',		1).
valid_abstract_section_header('ETHICAL ASPECTS OF PLACEBO USE IN CLINICAL PHARMACOTHERAPEUTIC RESEARCH',		1).
valid_abstract_section_header('ETHICAL ASSESSMENT OF BIOMARKERS IN PALLIATIVE RADIOTHERAPY TRIALS',		1).
valid_abstract_section_header('ETHICAL CONSIDERATIONS AND DISSEMINATION',		1).
valid_abstract_section_header('ETHICAL ISSUES/APPROVAL',		1).
valid_abstract_section_header('ETHICAL NOTE',		1).
valid_abstract_section_header('ETHICAL STATEMENT',		1).
valid_abstract_section_header('ETHICS AND APPROVAL',		1).
valid_abstract_section_header('ETHICS AND DISCUSSION',		1).
valid_abstract_section_header('ETHICS AND LEGAL QUESTIONS OF MEDICAL LAW',		1).
valid_abstract_section_header('ETHICS AND REGISTRATION',		1).
valid_abstract_section_header('ETHICS APPROVAL NUMBER',		1).
valid_abstract_section_header('ETHICS DISSEMINATION',		1).
valid_abstract_section_header('ETHICS STATEMENT',		1).
valid_abstract_section_header('ETHNAPHARMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHNOBOTANICAL AND ETHNOMEDICINAL RELEVANCE',		1).
valid_abstract_section_header('ETHNOMEDICINAL RELEVANCE',		1).
valid_abstract_section_header('ETHNOPARMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHNOPHARAMCOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHNOPHARMACOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHNOPHARMACOLGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL INVESTIGATION',		1).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL RELEVANCE OF THE STUDY',		1).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL RELEVANCE/INTRODUCTION',		1).
valid_abstract_section_header('ETHNOPHARMACOLOGICAL RELEVANCES',		1).
valid_abstract_section_header('ETHNOPHARMACOLOGICALS RELEVANCE',		1).
valid_abstract_section_header('ETHNOPHARMALOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHNOPHARMMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHONOPHARMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHONOPHARMACOLOGICAL RELEVANCES',		1).
valid_abstract_section_header('ETHOPHARMAACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHOPHARMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETHOPHARMACOLOGY RELEVANCE',		1).
valid_abstract_section_header('ETIOLOGIC OR RISK FACTORS',		1).
valid_abstract_section_header('ETIOLOGIES ARE VARIOUS AND INCLUDE',		1).
valid_abstract_section_header('ETIOLOGY AND PREVALENCE',		1).
valid_abstract_section_header('ETIOLOGY AND PREVENTION',		1).
valid_abstract_section_header('ETIOLOGY AND TREATMENT',		1).
valid_abstract_section_header('ETIOLOGY OF CONVULSIVE STATUS EPILEPTICUS',		1).
valid_abstract_section_header('ETIOLOGY OF MALNUTRITION IN DIALYSIS PATIENTS',		1).
valid_abstract_section_header('ETIOLOGY OF PNEUMONIA',		1).
valid_abstract_section_header('ETIOPATHOGENESIS',		1).
valid_abstract_section_header('ETIOPATHOGENESIS OF CARDIOVASCULAR DISEASES ACCOMPANIED WITH HIGHER LEVEL OF HOMOCYSTEINE',		1).
valid_abstract_section_header('ETNOPHARMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('ETOMOXIR',		1).
valid_abstract_section_header('EU-CERT-ICD, DANISH-ICD AND DO-IT',		1).
valid_abstract_section_header('EUDRA CT NO',		1).
valid_abstract_section_header('EUDRACT N',		1).
valid_abstract_section_header('EUDRACT NUMBER AND CLINICALTRIALSGOV IDENTIFIER',		1).
valid_abstract_section_header('EUROPE',		1).
valid_abstract_section_header('EUROPEAN COMMUNITY CLINICAL TRIAL SYSTEM (EUDRACT) NUMBER',		1).
valid_abstract_section_header('EUROPEAN LACIDIPINE STUDY ON ATHEROSCLEROSIS (ELSA) TRIAL',		1).
valid_abstract_section_header('EUROPEAN PRODUCT DATABASE',		1).
valid_abstract_section_header('EUROPEAN UNION DRUG REGULATING AUTHORITIES CLINICAL TRIAL NO',		1).
valid_abstract_section_header('EUTHYROID GROUP',		1).
valid_abstract_section_header('EVACUATION',		1).
valid_abstract_section_header('EVALUATE THE RISK',		1).
valid_abstract_section_header('EVALUATED VARIABLES',		1).
valid_abstract_section_header('EVALUATING ASSESSMENT',		1).
valid_abstract_section_header('EVALUATING CANCER CARE',		1).
valid_abstract_section_header('EVALUATING LEARNING THROUGH PDSA CYCLES',		1).
valid_abstract_section_header('EVALUATION AIMS',		1).
valid_abstract_section_header('EVALUATION AND CONCLUSION',		1).
valid_abstract_section_header('EVALUATION AND CONCLUSIONS',		1).
valid_abstract_section_header('EVALUATION AND DISCUSSION',		1).
valid_abstract_section_header('EVALUATION AND DOCUMENTATION',		1).
valid_abstract_section_header('EVALUATION AND OUTCOMES',		1).
valid_abstract_section_header('EVALUATION METHODOLOGY',		1).
valid_abstract_section_header('EVALUATION OF NUTRITIONAL STATUS',		1).
valid_abstract_section_header('EVALUATION OF RADIOGRAPHIC FINDINGS',		1).
valid_abstract_section_header('EVALUATION OF THE CHEMICAL COMPOSITION, PARTICLE SIZE, AND PULMONARY TOXICITY OF CELLULOSE INSULATION',		1).
valid_abstract_section_header('EVALUATION OF THE METHOD',		1).
valid_abstract_section_header('EVALUATION OF THE PSP',		1).
valid_abstract_section_header('EVALUATION OF THE VRQC PROGRAM',		1).
valid_abstract_section_header('EVALUATION OF THERAPEUTIC RESULTS',		1).
valid_abstract_section_header('EVALUATION RESULTS AND DISCUSSIONS',		1).
valid_abstract_section_header('EVALUATION STANDARDIZATION',		1).
valid_abstract_section_header('EVALUATION/EXAMINATION',		1).
valid_abstract_section_header('EVERY HUMAN CULTURE HAS SOME FORM OF MUSIC WITH A BEAT',		1).
valid_abstract_section_header('EVERYWHERE AND NOWHERE LOCATING LESBIAN EROTICS',		1).
valid_abstract_section_header('EVIDENC SYNTHESIS',		1).
valid_abstract_section_header('EVIDENCE ACQUISITION/RESULTS',		1).
valid_abstract_section_header('EVIDENCE ANALYSIS',		1).
valid_abstract_section_header('EVIDENCE AND ACQUISITION',		1).
valid_abstract_section_header('EVIDENCE AND DATA ACQUISITION',		1).
valid_abstract_section_header('EVIDENCE AND DISCUSSION',		1).
valid_abstract_section_header('EVIDENCE BASED MEDICINE',		1).
valid_abstract_section_header('EVIDENCE FOR ARGUMENT',		1).
valid_abstract_section_header('EVIDENCE FOR EFFECTIVENESS',		1).
valid_abstract_section_header('EVIDENCE FOR IMPROVED OUTCOMES WITH EARLIER IDENTIFICATION',		1).
valid_abstract_section_header('EVIDENCE FOR THE EFFECTIVENESS OF THIS COMBINATION',		1).
valid_abstract_section_header('EVIDENCE FOR THE HYPOTHESIS',		1).
valid_abstract_section_header('EVIDENCE FOR VACCINE EFFECTIVENESS WAS SOUGHT FROM',		1).
valid_abstract_section_header('EVIDENCE FROM LITERATURE',		1).
valid_abstract_section_header('EVIDENCE FROM THE SHANGHAL TRIAL OF NIFEDIPINE IN THE ELDERLY (STONE)',		1).
valid_abstract_section_header('EVIDENCE OF ACQUISITION AND SYNTHESIS',		1).
valid_abstract_section_header('EVIDENCE REPORT CONCLUSIONS',		1).
valid_abstract_section_header('EVIDENCE REPORT DATA SOURCES',		1).
valid_abstract_section_header('EVIDENCE REPORT RESULTS',		1).
valid_abstract_section_header('EVIDENCE REPORT REVIEW METHODS',		1).
valid_abstract_section_header('EVIDENCE REVIEW CONCLUSION',		1).
valid_abstract_section_header('EVIDENCE SINTHESIS',		1).
valid_abstract_section_header('EVIDENCE SYNTESIS',		1).
valid_abstract_section_header('EVIDENCE SYNTHESIS AND DISCUSSION',		1).
valid_abstract_section_header('EVIDENCE THAT LINKS THE DRUG TO THE EVENT',		1).
valid_abstract_section_header('EVIDENCE TO SUPPORT PARTNER INVOLVEMENT IN ED THERAPY',		1).
valid_abstract_section_header('EVIDENCE-BASED MEDICINE LEVEL II',		1).
valid_abstract_section_header('EVIDENCE-BASED STRATEGIES',		1).
valid_abstract_section_header('EVIDENTIARY VALUE',		1).
valid_abstract_section_header('EVLA VERSUS SURGERY',		1).
valid_abstract_section_header('EVOLUTION AND MORPHOLOGICAL VARIATION IN FOSSIL HOMINIDS',		1).
valid_abstract_section_header('EVOLUTION OF CORONARY STENTS',		1).
valid_abstract_section_header('EVOLUTION OF EFFORTS TO INTEGRATE IMPROVEMENT KNOWLEDGE INTO HEALTH PROFESSIONS EDUCATION',		1).
valid_abstract_section_header('EVOLUTION OF THE JOINT ACTION',		1).
valid_abstract_section_header('EXAMINATION AND OPERATION',		1).
valid_abstract_section_header('EXAMINATION CONSISTED OF',		1).
valid_abstract_section_header('EXAMINATION MATERIAL',		1).
valid_abstract_section_header('EXAMINATION OF THE REPORT ITSELF',		1).
valid_abstract_section_header('EXAMINATION RESULTS',		1).
valid_abstract_section_header('EXAMINATIONS AND COURSE OF EVENTS',		1).
valid_abstract_section_header('EXAMPLE AND OBSERVATIONS',		1).
valid_abstract_section_header('EXAMPLES OF BENEFITS',		1).
valid_abstract_section_header('EXAMPLES OF CLINICAL QI WORK IN SIBERIA',		1).
valid_abstract_section_header('EXAMPLES OF LIMITATIONS',		1).
valid_abstract_section_header('EXAMPLES OF THE CONTINUOUS QUALITY IMPROVEMENT CYCLE',		1).
valid_abstract_section_header('EXCESS CATECHOLAMINE SYNDROME',		1).
valid_abstract_section_header('EXCESSIVE ATTENTION TO DETAILS',		1).
valid_abstract_section_header('EXCLUSION',		1).
valid_abstract_section_header('EXCRETORY UROGRAPHY',		1).
valid_abstract_section_header('EXERCISE INTOLERANCE',		1).
valid_abstract_section_header('EXERCISE PRESCRIPTION',		1).
valid_abstract_section_header('EXHIBITS',		1).
valid_abstract_section_header('EXISTING METHODS',		1).
valid_abstract_section_header('EXISTING/HEALED PUS',		1).
valid_abstract_section_header('EXPANDING SURVEILLANCE CAPACITY AND PROGRESS TOWARD A LEARNING HEALTH SYSTEM',		1).
valid_abstract_section_header('EXPECTATIONS',		1).
valid_abstract_section_header('EXPECTED IMPACT FOR PUBLIC HEALTH',		1).
valid_abstract_section_header('EXPECTED IMPACT OF THE STUDY ON PUBLIC HEALTH',		1).
valid_abstract_section_header('EXPECTED OUTCOME',		1).
valid_abstract_section_header('EXPECTED PUBLIC HEALTH IMPACT',		1).
valid_abstract_section_header('EXPECTED RESULTS AND CONTRIBUTIONS',		1).
valid_abstract_section_header('EXPECTED RESULTS AND PERSPECTIVES',		1).
valid_abstract_section_header('EXPECTED RESULTS AND VIEWPOINTS',		1).
valid_abstract_section_header('EXPECTED VALUE',		1).
valid_abstract_section_header('EXPERIENCE AND OUTCOME',		1).
valid_abstract_section_header('EXPERIENCE IN EUROPE',		1).
valid_abstract_section_header('EXPERIENCE IN THE UNITED STATES',		1).
valid_abstract_section_header('EXPERIENCE OF SYSTEMATIC REVIEWS IN APHASIA',		1).
valid_abstract_section_header('EXPERIENCE WITH SIX SIGMA IN THE NETHERLANDS',		1).
valid_abstract_section_header('EXPERIENCE WITH THE UNIT ASSESSMENT TOOL',		1).
valid_abstract_section_header('EXPERIMENT AND CLINICAL CASES',		1).
valid_abstract_section_header('EXPERIMENT AND FINDINGS',		1).
valid_abstract_section_header('EXPERIMENTAL AND CLINICAL FINDINGS',		1).
valid_abstract_section_header('EXPERIMENTAL APPROACH & KEY RESULTS',		1).
valid_abstract_section_header('EXPERIMENTAL APPROACH/PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('EXPERIMENTAL CONDITIONS',		1).
valid_abstract_section_header('EXPERIMENTAL DESIG',		1).
valid_abstract_section_header('EXPERIMENTAL METHODS AND RESULTS',		1).
valid_abstract_section_header('EXPERIMENTAL OBJECTIVE',		1).
valid_abstract_section_header('EXPERIMENTAL OBSERVATIONS',		1).
valid_abstract_section_header('EXPERIMENTAL PROCEDURES AND RESULTS',		1).
valid_abstract_section_header('EXPERIMENTAL PROTOCOLS AND RESULTS',		1).
valid_abstract_section_header('EXPERIMENTAL RESULTS',		1).
valid_abstract_section_header('EXPERIMENTAL SECTION',		1).
valid_abstract_section_header('EXPERIMENTAL TECHNIQUES',		1).
valid_abstract_section_header('EXPERIMENTAL TESTING',		1).
valid_abstract_section_header('EXPERIMENTAL TREATMENT',		1).
valid_abstract_section_header('EXPERIMENTALDESIGN',		1).
valid_abstract_section_header('EXPERIMENTATION',		1).
valid_abstract_section_header('EXPERIMENTS AND FINDINGS',		1).
valid_abstract_section_header('EXPERIMENTS AND MODELS',		1).
valid_abstract_section_header('EXPERT PERFORMANCE APPROACH',		1).
valid_abstract_section_header('EXPERT REVIEWERS',		1).
valid_abstract_section_header('EXPLORATORY OUTCOMES',		1).
valid_abstract_section_header('EXPLORING SEAMLESS REGISTRATION',		1).
valid_abstract_section_header('EXPOSITION',		1).
valid_abstract_section_header('EXPOSURE ASSESSMENT OF CELLULOSE INSULATION APPLICATORS',		1).
valid_abstract_section_header('EXPOSURE MEASURE',		1).
valid_abstract_section_header('EXPOSURE OF INTEREST',		1).
valid_abstract_section_header('EXPOSURE TO VIOLENCE AND POST-TRAUMATIC STRESS DISORDERS',		1).
valid_abstract_section_header('EXPOSURE VARIABLE',		1).
valid_abstract_section_header('EXPOSURE VARIABLES',		1).
valid_abstract_section_header('EXPOSURE/INTERVENTION',		1).
valid_abstract_section_header('EXPRESSION ANALYSIS OF IRF10 IN VIVO AND IN VITRO',		1).
valid_abstract_section_header('EXTERNAL PACEMAKERS',		1).
valid_abstract_section_header('EXTRACELLULAR PROTEIN ANALYSIS',		1).
valid_abstract_section_header('EXTRIP GUIDELINES',		1).
valid_abstract_section_header('EYE CARE INTEGRATION PROJECT (SCOTLAND)',		1).
valid_abstract_section_header('Evidence acquisition',		1).
valid_abstract_section_header('FACING THE PROBLEM',		1).
valid_abstract_section_header('FACTOR AND OUTCOME',		1).
valid_abstract_section_header('FACTORS AFFECTING PSYCHIATRIC TRAINING',		1).
valid_abstract_section_header('FACTORS CONDITIONING USE',		1).
valid_abstract_section_header('FACTORS FOR SUCCESS OF EBM RECOMMENDATIONS AND GUIDELINES',		1).
valid_abstract_section_header('FACTORS IN INTENSIVE CARE',		1).
valid_abstract_section_header('FACTORS INFLUENCING SOURCE MONITORING',		1).
valid_abstract_section_header('FACTORS RELATED TO GUIDELINES',		1).
valid_abstract_section_header('FACTORS RELATED TO PHYSICIANS',		1).
valid_abstract_section_header('FACTORS RELATING TO THE CIRCUMSTANCES OF THE INJURY',		1).
valid_abstract_section_header('FACTS AND CIRCUMSTANCES',		1).
valid_abstract_section_header('FACTS AND THEORETICAL CONSIDERATIONS',		1).
valid_abstract_section_header('FAIR INFORMATION PRACTICES PRINCIPLES',		1).
valid_abstract_section_header('FAMILY DESCRIPTION',		1).
valid_abstract_section_header('FAMILY NARRATIVES IN CONFRONTING ILLNESS',		1).
valid_abstract_section_header('FEASIBILITY AND PERSPECTIVES',		1).
valid_abstract_section_header('FEASIBILITY RESULTS',		1).
valid_abstract_section_header('FEATURES OF VIRTUAL PATHOLOGY',		1).
valid_abstract_section_header('FED VS FASTING',		1).
valid_abstract_section_header('FEEDBACK CONTROL',		1).
valid_abstract_section_header('FEMALE WISTAR RATS (WT',		1).
valid_abstract_section_header('FETAL ASPECTS',		1).
valid_abstract_section_header('FETAL RISK',		1).
valid_abstract_section_header('FETAL RISKS',		1).
valid_abstract_section_header('FF/CONTROL',		1).
valid_abstract_section_header('FF/FF',		1).
valid_abstract_section_header('FF/FR',		1).
valid_abstract_section_header('FIBRINOLYSIS AND THROMBOSIS',		1).
valid_abstract_section_header('FIBRINOLYSIS IN ISCHEMIC BRAIN DISEASE',		1).
valid_abstract_section_header('FIBRINOLYTIC MECHANISMS',		1).
valid_abstract_section_header('FIELD',		1).
valid_abstract_section_header('FIELD OF RESEARCH',		1).
valid_abstract_section_header('FIELD OF STUDY',		1).
valid_abstract_section_header('FIELD SITE',		1).
valid_abstract_section_header('FILLING DATA GAPS',		1).
valid_abstract_section_header('FINAL COMMENTS',		1).
valid_abstract_section_header('FINAL CONCLUSIONS',		1).
valid_abstract_section_header('FINANCES',		1).
valid_abstract_section_header('FINANCIAL DISCLOSURES(S)',		1).
valid_abstract_section_header('FINANCIAL INCENTIVES',		1).
valid_abstract_section_header('FINDING AND CONCLUSIONS',		1).
valid_abstract_section_header('FINDING AND INTERPRETATION',		1).
valid_abstract_section_header('FINDING THE IDEAL MARKER FOR MEASUREMENT OF RENAL FUNCTION IN SPINA BIFIDA',		1).
valid_abstract_section_header('FINDING THE SOLUTIONS',		1).
valid_abstract_section_header('FINDINGS & CONCLUSIONS',		1).
valid_abstract_section_header('FINDINGS & DISCUSSION',		1).
valid_abstract_section_header('FINDINGS AND DISCUSSIONS',		1).
valid_abstract_section_header('FINDINGS AND INTERPRETATIONS',		1).
valid_abstract_section_header('FINDINGS AND INTERVENTIONS',		1).
valid_abstract_section_header('FINDINGS AND KEY CONCLUSIONS',		1).
valid_abstract_section_header('FINDINGS AND PRACTICAL IMPLICATIONS',		1).
valid_abstract_section_header('FINDINGS AND PRACTICAL RELEVANCE',		1).
valid_abstract_section_header('FINDINGS DISPLACEMENT',		1).
valid_abstract_section_header('FINDINGS FROM THE ROOT CAUSE ANALYSIS (RCA)',		1).
valid_abstract_section_header('FINDINGS IN THE LITERATURE',		1).
valid_abstract_section_header('FINDINGS MAIN RESULTS',		1).
valid_abstract_section_header('FINDINGS OF LITERATURE REVIEW AND ANALYSIS',		1).
valid_abstract_section_header('FINDINGS OF SYSTEMATIC REVIEW AND ANALYSIS',		1).
valid_abstract_section_header('FINDINGS OF UTILIZATION ANALYSIS',		1).
valid_abstract_section_header('FINDINGS/APPROACH',		1).
valid_abstract_section_header('FINDINGS:',		1).
valid_abstract_section_header('FINDINGSS',		1).
valid_abstract_section_header('FIRE FIGHTER FATIGUE MANAGEMENT PROGRAM',		1).
valid_abstract_section_header('FIRMWARE DESIGN AND METHODS',		1).
valid_abstract_section_header('FIRST AID',		1).
valid_abstract_section_header('FIRST CASE OF MENINGIOMA',		1).
valid_abstract_section_header('FIRST CRITERIA',		1).
valid_abstract_section_header('FIRST EDUCATED GYNECOLOGIST IN VRBAS',		1).
valid_abstract_section_header('FIRST INTERVENTIONS AND SOLUTION OF TECHNICAL PROBLEMS',		1).
valid_abstract_section_header('FIRST PATIENT INCLUDED',		1).
valid_abstract_section_header('FIRST YEARS OF TREATMENT',		1).
valid_abstract_section_header('FISHERY CATCHES FROM EACH CLOSED SITE',		1).
valid_abstract_section_header('FISHERY INCOME IN IMPLEMENTING VILLAGES',		1).
valid_abstract_section_header('FIXATION',		1).
valid_abstract_section_header('FLAME CURTAIN BIOCHAR KILNS',		1).
valid_abstract_section_header('FLESH FLIES (DIPTERA',		1).
valid_abstract_section_header('FLESHD',		1).
valid_abstract_section_header('FLESHF',		1).
valid_abstract_section_header('FLUORIDE PROPHYLAXIS',		1).
valid_abstract_section_header('FLUORO-GOLD',		1).
valid_abstract_section_header('FNA/CNB',		1).
valid_abstract_section_header('FOAM MATTRESSES',		1).
valid_abstract_section_header('FOAM SCLEROTHERAPY',		1).
valid_abstract_section_header('FOCAL HYPERHIDROSIS OF THE PALMS AND AXILLAE',		1).
valid_abstract_section_header('FOCAL SPLENICE LESIONS',		1).
valid_abstract_section_header('FOCUS OF ATTENTION',		1).
valid_abstract_section_header('FOCUS OF RESEARCH',		1).
valid_abstract_section_header('FOCUS OF THIS ARTICLE',		1).
valid_abstract_section_header('FOLATES IN NUTRITION',		1).
valid_abstract_section_header('FOLGERUNG',		1).
valid_abstract_section_header('FOLIC ACID SUPPLEMENTATION',		1).
valid_abstract_section_header('FOLLOW-UP CARE',		1).
valid_abstract_section_header('FOLLOW-UP TREATMENT',		1).
valid_abstract_section_header('FOODS',		1).
valid_abstract_section_header('FOR ALL PATIENTS',		1).
valid_abstract_section_header('FOR COOKING',		1).
valid_abstract_section_header('FOR HUNTING',		1).
valid_abstract_section_header('FOR PATIENTS WITH TYPE I DIABETES',		1).
valid_abstract_section_header('FOR PATIENTS WITH TYPE II DIABETES',		1).
valid_abstract_section_header('FOR THE MOTHER',		1).
valid_abstract_section_header('FOR THE OFFSPRING',		1).
valid_abstract_section_header('FORMAL MODELS AND HISTORY',		1).
valid_abstract_section_header('FORMAT',		1).
valid_abstract_section_header('FORMS OF THERAPY',		1).
valid_abstract_section_header('FORMULARY OF WOUND MANAGEMENT PRODUCTS',		1).
valid_abstract_section_header('FORWARD LOOK',		1).
valid_abstract_section_header('FOUNDATION FOR THE FUTURE',		1).
valid_abstract_section_header('FOUNDATIONS AND AIM',		1).
valid_abstract_section_header('FOUNDING',		1).
valid_abstract_section_header('FOUR CLINICAL SUBTYPES',		1).
valid_abstract_section_header('FOUR KNOWN FLAVONOIDS',		1).
valid_abstract_section_header('FOUR PRELIMINARY ELEMENTS',		1).
valid_abstract_section_header('FOUR QUESTIONS',		1).
valid_abstract_section_header('FPC SCREENING',		1).
valid_abstract_section_header('FRACTURE HEALING ENHANCEMENT',		1).
valid_abstract_section_header('FRACTURE HEALING IN RELATION TO OSTEOPOROSIS',		1).
valid_abstract_section_header('FRAMEWORK AND METHODS',		1).
valid_abstract_section_header('FRAMEWORK AND NEXT STEPS',		1).
valid_abstract_section_header('FRAMEWORK AND PRACTICE PARADOXES',		1).
valid_abstract_section_header('FRAMEWORK FOR POPULATION HEALTH MEASUREMENT',		1).
valid_abstract_section_header('FRAMEWORK FOR THE EVIDENCE',		1).
valid_abstract_section_header('FRAMEWORK OF STRUCTURAL FACTORS',		1).
valid_abstract_section_header('FREE NERVE ENDINGS',		1).
valid_abstract_section_header('FREQUENT MISREADING',		1).
valid_abstract_section_header('FROM A MICROBIOLOGICAL POINT OF VIEW',		1).
valid_abstract_section_header('FROM AN AETIOLOGICAL POINT OF VIEW',		1).
valid_abstract_section_header('FROM AN ANATOMOPATHOLOGICAL POINT OF VIEW',		1).
valid_abstract_section_header('FROM EXPERIMENTAL BLEEDING TO ERYTHROPOIETIN',		1).
valid_abstract_section_header('FROM OZENA TO SINUSITIS',		1).
valid_abstract_section_header('FROM RADIOACTIVITY TO NUCLEAR MEDICINE',		1).
valid_abstract_section_header('FROM VULNERABLE MAN TO CAPABLE MAN',		1).
valid_abstract_section_header('FSL) RESULTS',		1).
valid_abstract_section_header('FT (OR',		1).
valid_abstract_section_header('FULMINANT HEPATITIS',		1).
valid_abstract_section_header('FUNCTION OF VENOM HYALURONIDASES',		1).
valid_abstract_section_header('FUNCTIONAL ELECTRICAL THERAPY',		1).
valid_abstract_section_header('FUNCTIONAL MEASURES',		1).
valid_abstract_section_header('FUNCTIONAL SEQUELAE',		1).
valid_abstract_section_header('FUNCTIONS OF NK RECEPTORS',		1).
valid_abstract_section_header('FUNDAMENTAL ASPECTS',		1).
valid_abstract_section_header('FUNDAMENTAL BASIS OF METASTATIC PROCESS',		1).
valid_abstract_section_header('FUNDAMENTAL PRINCIPLES IN PLASTIC SURGERY',		1).
valid_abstract_section_header('FUNDAMENTAL PRINCIPLES OF ORAL SOFT TISSUE WOUND HEALING',		1).
valid_abstract_section_header('FUNDAMENTAL RULE',		1).
valid_abstract_section_header('FUNDAMENTO',		1).
valid_abstract_section_header('FUNDED',		1).
valid_abstract_section_header('FUNDERS',		1).
valid_abstract_section_header('FUNDIN',		1).
valid_abstract_section_header('FUNDING STATEMENT',		1).
valid_abstract_section_header('FUNDING/COMPETING INTERESTS',		1).
valid_abstract_section_header('FUNDING/CONFLICTS OF INTEREST',		1).
valid_abstract_section_header('FUNDING/SPONSOR',		1).
valid_abstract_section_header('FUNDING/SUPPORT',		1).
valid_abstract_section_header('FURTHER CARE',		1).
valid_abstract_section_header('FURTHER COURSE, DIAGNOSIS AND TREATMENT',		1).
valid_abstract_section_header('FURTHER DISCUSSION',		1).
valid_abstract_section_header('FURTHER IMPLICATIONS OF THE FORMAL MODEL',		1).
valid_abstract_section_header('FURTHER INFORMATION NEEDED',		1).
valid_abstract_section_header('FURTHER INVESTIGATIONS',		1).
valid_abstract_section_header('FURTHER PROBLEMS WITH AMBULATORY MONITORING',		1).
valid_abstract_section_header('FUTURE ACTIVITIES',		1).
valid_abstract_section_header('FUTURE ADVANCES',		1).
valid_abstract_section_header('FUTURE AND PROSPECTS',		1).
valid_abstract_section_header('FUTURE APPLICATIONS',		1).
valid_abstract_section_header('FUTURE BENEFITS',		1).
valid_abstract_section_header('FUTURE CHALLENGES IN PLANT PATHOLOGY',		1).
valid_abstract_section_header('FUTURE CLINICAL APPLICATION',		1).
valid_abstract_section_header('FUTURE CONSIDERATIONS',		1).
valid_abstract_section_header('FUTURE DATA NEEDS',		1).
valid_abstract_section_header('FUTURE DIRECTIONS AND CRITICAL ISSUES',		1).
valid_abstract_section_header('FUTURE IMPACT',		1).
valid_abstract_section_header('FUTURE OF OPEN SURGICAL SIMULATION',		1).
valid_abstract_section_header('FUTURE OPPORTUNITY',		1).
valid_abstract_section_header('FUTURE OUTLOOK',		1).
valid_abstract_section_header('FUTURE REQUIREMENTS',		1).
valid_abstract_section_header('FUTURE RESEARCH ACTIVITIES',		1).
valid_abstract_section_header('FUTURE RESEARCH RECOMMENDATIONS',		1).
valid_abstract_section_header('FUTURE STATE OF GOVERNANCE',		1).
valid_abstract_section_header('FUTURE STRATEGIES',		1).
valid_abstract_section_header('FUTURE SURVEILLANCE PLANNING',		1).
valid_abstract_section_header('FUTURE TECHNOLOGICAL POSSIBILITIES',		1).
valid_abstract_section_header('FUTURE TREATMENT',		1).
valid_abstract_section_header('FUTURE VIEW',		1).
valid_abstract_section_header('FUTURE, DISCUSSION AND CONCLUSION',		1).
valid_abstract_section_header('FUTURES AND PROJECTS',		1).
valid_abstract_section_header('Future prospects of cohort:',		1).
valid_abstract_section_header('GACT SOFTWARE',		1).
valid_abstract_section_header('GAIA HYPOTHESIS',		1).
valid_abstract_section_header('GAIT ANALYSIS',		1).
valid_abstract_section_header('GAIT ANALYSIS FOLLOWING TAR',		1).
valid_abstract_section_header('GAIT MAINLY DEPENDS ON THE RELATIONSHIP BETWEEN POSTURE BALANCE AND MOVEMENT',		1).
valid_abstract_section_header('GAMMA CAMERA',		1).
valid_abstract_section_header('GANGLION METASTASIS',		1).
valid_abstract_section_header('GANODERMA LUCIDU',		1).
valid_abstract_section_header('GAS EMISSION FACTORS',		1).
valid_abstract_section_header('GASTRIC SECRETION',		1).
valid_abstract_section_header('GENBANK ACCESSION NUMBER',		1).
valid_abstract_section_header('GENBANK ACCESSION NUMBERS',		1).
valid_abstract_section_header('GENDER, ALCOHOL, AND CULTURE',		1).
valid_abstract_section_header('GENE POLYMORPHISMS THAT MAY PREDISPOSE INFANTS TO SUDDEN INFANT DEATH UNDER CERTAIN CIRCUMSTANCES',		1).
valid_abstract_section_header('GENE THERAPY',		1).
valid_abstract_section_header('GENERAL BACKGROUND',		1).
valid_abstract_section_header('GENERAL BIODIVERSITY INDICATORS',		1).
valid_abstract_section_header('GENERAL FACTS ABOUT ZOONOSES',		1).
valid_abstract_section_header('GENERAL FEATURES',		1).
valid_abstract_section_header('GENERAL INTERESTS',		1).
valid_abstract_section_header('GENERAL OBJETIVE',		1).
valid_abstract_section_header('GENERAL OBSERVATION',		1).
valid_abstract_section_header('GENERAL OVERVIEW',		1).
valid_abstract_section_header('GENERAL PRINCIPLES AND METHODS',		1).
valid_abstract_section_header('GENERAL PRINCIPLES FOR EDUCATIONAL EXPERIENCES IN HEALTH CARE IMPROVEMENT',		1).
valid_abstract_section_header('GENERAL RECOMMENDATIONS',		1).
valid_abstract_section_header('GENERAL SIGNIFICANCE AND INTEREST',		1).
valid_abstract_section_header('GENERAL SIGNIFICANT',		1).
valid_abstract_section_header('GENERALLY INDUCED FATIGUE',		1).
valid_abstract_section_header('GENERATION FROM EMBRYONIC STEM CELLS',		1).
valid_abstract_section_header('GENERATION OF HYPOTHESES',		1).
valid_abstract_section_header('GENES IMPLICATED',		1).
valid_abstract_section_header('GENETIC ALTERATIONS THAT MAY CAUSE SUDDEN INFANT DEATH',		1).
valid_abstract_section_header('GENETIC ANALYSIS',		1).
valid_abstract_section_header('GENETIC AND MOLECULAR ANALYSES OF DOMESTICATION',		1).
valid_abstract_section_header('GENETIC ASPECTS OF LQTS',		1).
valid_abstract_section_header('GENETIC CONTROL OF CELL VOLUME IS MULTIGENIC',		1).
valid_abstract_section_header('GENETIC COUNSELING',		1).
valid_abstract_section_header('GENETIC COUNSELING AND PRENATAL DIAGNOSIS',		1).
valid_abstract_section_header('GENETIC DATA',		1).
valid_abstract_section_header('GENETIC DEFECTS AND HORMONES',		1).
valid_abstract_section_header('GENETIC RESOURCES',		1).
valid_abstract_section_header('GENETIC STRUCTURE',		1).
valid_abstract_section_header('GENETIC/GENOMIC METHODS',		1).
valid_abstract_section_header('GENOME DIVERSITY',		1).
valid_abstract_section_header('GENOME MINIATURIZATION',		1).
valid_abstract_section_header('GENOME ORGANIZATION',		1).
valid_abstract_section_header('GENOME ORGANIZATION, GENE FUNCTION AND TAXONOMY',		1).
valid_abstract_section_header('GENOMIC CHARACTERISTICS OF HEPATITIS C VIRUS',		1).
valid_abstract_section_header('GENOTOXICITY TEST SYSTEMS',		1).
valid_abstract_section_header('GENOTYPING AND PHENOTYPING',		1).
valid_abstract_section_header('GEO SERIES ACCESSION NUMBER',		1).
valid_abstract_section_header('GEOGRAPHICAL LOCATION UK PARTICIPANTS',		1).
valid_abstract_section_header('GEOMETRIC ASPECT',		1).
valid_abstract_section_header('GEOMETRY',		1).
valid_abstract_section_header('GERMAN CLINICAL TRIAL REGISTER (DRKS)',		1).
valid_abstract_section_header('GERMAN CLINICAL TRIALS REGISTER',		1).
valid_abstract_section_header('GERMAN CLINICAL TRIALS REGISTER NUMBER',		1).
valid_abstract_section_header('GERMAN SOCIETY OF SURGERY',		1).
valid_abstract_section_header('GESTATIONAL DIABETES MELLITUS (GDM)',		1).
valid_abstract_section_header('GETTING EVIDENCE INTO PRACTICE',		1).
valid_abstract_section_header('GETTING STARTED',		1).
valid_abstract_section_header('GG) CONCLUSION',		1).
valid_abstract_section_header('GG) CONCLUSIONS',		1).
valid_abstract_section_header('GG) METHODS',		1).
valid_abstract_section_header('GG/GC',		1).
valid_abstract_section_header('GIVEN KEYWORDS',		1).
valid_abstract_section_header('GLAXOSMITHKLINE PROTOCOL',		1).
valid_abstract_section_header('GLOBAL ACTION AGENDA',		1).
valid_abstract_section_header('GLOSS',		1).
valid_abstract_section_header('GLOVE TRIALS',		1).
valid_abstract_section_header('GLUCOCORTICOID HYPERTENSION',		1).
valid_abstract_section_header('GOAL AND QUESTIONS',		1).
valid_abstract_section_header('GOAL OF THE ARTICLE',		1).
valid_abstract_section_header('GOALS AND BACKGROUNDS',		1).
valid_abstract_section_header('GOALS AND RESULTS',		1).
valid_abstract_section_header('GOALS OF DRUG TREATMENT IN CONGESTIVE HEART FAILURE',		1).
valid_abstract_section_header('GOALS OF STUDY',		1).
valid_abstract_section_header('GOLD STANDARD',		1).
valid_abstract_section_header('GOLD STANDARD TREATMENT',		1).
valid_abstract_section_header('GOLDEN HOUR',		1).
valid_abstract_section_header('GOOD MEDICAL PRACTICE COURSE',		1).
valid_abstract_section_header('GOOD RESEARCH PRACTICES',		1).
valid_abstract_section_header('GOV ID',		1).
valid_abstract_section_header('GOV IDENTIFIER NUMBER',		1).
valid_abstract_section_header('GOV NO',		1).
valid_abstract_section_header('GOV REGISTRATION NO',		1).
valid_abstract_section_header('GOV STUDY NUMBER',		1).
valid_abstract_section_header('GOVERNANCE',		1).
valid_abstract_section_header('GOVIDENTIFIERS',		1).
valid_abstract_section_header('GRADE OF RECOMMENDATION AND LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('GRADE QUALITY OF EVIDENCE FOR CARDIAC MRI IN THE DIAGNOSIS OF CAD',		1).
valid_abstract_section_header('GRADE QUALITY OF THE EVIDENCE',		1).
valid_abstract_section_header('GRAPHIC PRESENTATION OF QUANTITATIVE RESULTS OF HEARING TESTING',		1).
valid_abstract_section_header('GRAVITY SCORES',		1).
valid_abstract_section_header('GREAT',		1).
valid_abstract_section_header('GROUP CONSENSUS',		1).
valid_abstract_section_header('GROUP CONSENSUS/GUIDELINE',		1).
valid_abstract_section_header('GROUP I DISEASES',		1).
valid_abstract_section_header('GROUP II',		1).
valid_abstract_section_header('GROUP II DISEASES',		1).
valid_abstract_section_header('GROUP OF THE PATIENTS',		1).
valid_abstract_section_header('GROUPS/ORGANISATIONS',		1).
valid_abstract_section_header('GROWING POINTS AND AREAS FOR RESEARCH',		1).
valid_abstract_section_header('GROWING POINTS OR AREAS TIMELY FOR DEVELOPING RESEARCH',		1).
valid_abstract_section_header('GROWING POINTS, AREAS TIMELY FOR DEVELOPING RESEARCH',		1).
valid_abstract_section_header('GROWTH OF SIMULATION',		1).
valid_abstract_section_header('GT/TT',		1).
valid_abstract_section_header('GUIDANCE FOR CLINICAL PRACTICE',		1).
valid_abstract_section_header('GUIDANCE STATEMENT 1',		1).
valid_abstract_section_header('GUIDELINE DEVELOPMENT CASE STUDIES',		1).
valid_abstract_section_header('GUIDELINE HIGHLIGHTS',		1).
valid_abstract_section_header('GUIDELINE OBJECTIVES',		1).
valid_abstract_section_header('GUIDELINE UPDATE',		1).
valid_abstract_section_header('GUIDELINES FOR BONE MINERAL DENSITY TESTING',		1).
valid_abstract_section_header('GUIDELINES FOR CULTURALLY COMPETENT QI',		1).
valid_abstract_section_header('GUSTATORY SWEATING',		1).
valid_abstract_section_header('GYNECOLOGICAL CAUSES',		1).
valid_abstract_section_header('HABLAMOS JUNTOS',		1).
valid_abstract_section_header('HAEMODYNAMICS',		1).
valid_abstract_section_header('HAIR CELL REGENERATION',		1).
valid_abstract_section_header('HANDLING',		1).
valid_abstract_section_header('HANDOFF QUALITY MEASURES CLASSIFIED BY PRIMARY HANDOFF PURPOSE',		1).
valid_abstract_section_header('HARNESSING CROWDWORKERS FOR ENGINEERING',		1).
valid_abstract_section_header('HASIL',		1).
valid_abstract_section_header('HAZARD CLASSIFICATION',		1).
valid_abstract_section_header('HC RESULTS',		1).
valid_abstract_section_header('HDCT FOR STAGE IV DISEASE',		1).
valid_abstract_section_header('HEADING',		1).
valid_abstract_section_header('HEADING AIMS',		1).
valid_abstract_section_header('HEADINGS AIMS',		1).
valid_abstract_section_header('HEALING OF ARTICULAR CARTILAGE',		1).
valid_abstract_section_header('HEALTH AND ENVIRONMENT',		1).
valid_abstract_section_header('HEALTH CARE AS A CONTEXT FOR HEALTH PROFESSIONS LEARNING',		1).
valid_abstract_section_header('HEALTH CARE SITUATION',		1).
valid_abstract_section_header('HEALTH CLAIMS',		1).
valid_abstract_section_header('HEALTH ECONOMICS AND CLINICAL GUIDELINES',		1).
valid_abstract_section_header('HEALTH EFFECTS',		1).
valid_abstract_section_header('HEALTH MEASUREMENT',		1).
valid_abstract_section_header('HEALTH OF SENIORS/MEDICARE HEALTH OUTCOMES SURVEY (HOS)',		1).
valid_abstract_section_header('HEALTH POLICY',		1).
valid_abstract_section_header('HEALTH STATUS AND QUALITY OF LIFE',		1).
valid_abstract_section_header('HEALTH STATUS OF THE POPULATION OF THE NOVI SAD',		1).
valid_abstract_section_header('HEALTH TECHNOLOGY ASSESSMENT',		1).
valid_abstract_section_header('HEALTHCARE MATRIX',		1).
valid_abstract_section_header('HEARING TESTS',		1).
valid_abstract_section_header('HEART DISEASES',		1).
valid_abstract_section_header('HEART FAILURE AND SEXUALITY',		1).
valid_abstract_section_header('HEART TRANSPLANTATION',		1).
valid_abstract_section_header('HEMATOLOGIC AND OTHER NEOPLASTIC DISEASES',		1).
valid_abstract_section_header('HEMOCHROMATOSIS MAY BE CLASSIFIED INTO TWO GROUPS',		1).
valid_abstract_section_header('HEMODYNAMIC FACTORS',		1).
valid_abstract_section_header('HEMODYNAMIC MONITORING',		1).
valid_abstract_section_header('HEMOHIM (A NEW HERBAL PREPARATION OF THREE EDIBLE HERBS',		1).
valid_abstract_section_header('HEPATIC ENZYME ANALYSIS',		1).
valid_abstract_section_header('HEPATITIS INFECTION',		1).
valid_abstract_section_header('HEPATOMEGALY AND SPLENOMEGALY',		1).
valid_abstract_section_header('HERA TRIAL NO',		1).
valid_abstract_section_header('HEREDITY',		1).
valid_abstract_section_header('HESC-MSC',		1).
valid_abstract_section_header('HF) CONCLUSIONS',		1).
valid_abstract_section_header('HFCWO FVC',		1).
valid_abstract_section_header('HIGH THORACIC EPIDURAL ANESTHESIA TECHNIQUE',		1).
valid_abstract_section_header('HIGH-CONDITIONING STEPWISE CONDITIONING PROGRAMME',		1).
valid_abstract_section_header('HIGH-VALUE CARE ADVICE',		1).
valid_abstract_section_header('HIGH-VALUE CARE ADVICE 5',		1).
valid_abstract_section_header('HIGH-VALUE CARE ADVICE 6',		1).
valid_abstract_section_header('HIGH-VALUE CARE ADVICE 7',		1).
valid_abstract_section_header('HIGHLIGHT OF A REPORT',		1).
valid_abstract_section_header('HIGHLIGHT OF PAPER',		1).
valid_abstract_section_header('HIGHLIGHTS OF PAPER',		1).
valid_abstract_section_header('HINTERGRUND UND ZIEL',		1).
valid_abstract_section_header('HINTERGRUND UND ZIELVORGABEN',		1).
valid_abstract_section_header('HISTOANATOMICAL CHARACTERISTICS OF THE ABDOMINAL ADIPOSE TISSUE',		1).
valid_abstract_section_header('HISTOLOGIC ASPECTS',		1).
valid_abstract_section_header('HISTOLOGIC CHARACTERISTICS',		1).
valid_abstract_section_header('HISTOLOGICAL WORK UP',		1).
valid_abstract_section_header('HISTOPATHOLOGIC VERIFICATION',		1).
valid_abstract_section_header('HISTOPATHOLOGICAL FINDINGS',		1).
valid_abstract_section_header('HISTORICAL DATA',		1).
valid_abstract_section_header('HISTORICAL PERSPECTIVES AND TRENDS IN THE MANAGEMENT OF PAIN',		1).
valid_abstract_section_header('HISTORICAL SUMMARY OF WBD',		1).
valid_abstract_section_header('HISTORICAL, THEORETICAL AND INSTITUTIONAL BACKGROUND',		1).
valid_abstract_section_header('HISTORIQUE ET OBJECTIFS',		1).
valid_abstract_section_header('HISTORY AND CLINICALLY FINDINGS',		1).
valid_abstract_section_header('HISTORY AND ENTITLEMENT',		1).
valid_abstract_section_header('HISTORY AND GENERAL INVESTIGATIONS',		1).
valid_abstract_section_header('HISTORY AND OBJECTIVES',		1).
valid_abstract_section_header('HISTORY AND PRESENTING COMPLAINT',		1).
valid_abstract_section_header('HISTORY AND TREATMENT',		1).
valid_abstract_section_header('HISTORY OF ANTIBIOTICS',		1).
valid_abstract_section_header('HISTORY OF CANCER REGISTRIES',		1).
valid_abstract_section_header('HISTORY OF THE ARTIFICIAL DISC',		1).
valid_abstract_section_header('HISTORY OF THE TREATMENT IN THE WORLD',		1).
valid_abstract_section_header('HISTORY, DIAGNOSIS, TREATMENT AND COURSE',		1).
valid_abstract_section_header('HIT AND DISPARITIES',		1).
valid_abstract_section_header('HIV PATHOGENESIS',		1).
valid_abstract_section_header('HIV PREVENTION',		1).
valid_abstract_section_header('HIV(-)',		1).
valid_abstract_section_header('HIV/AIDS CONSTRUCTIONIST QUESTIONS ENGENDERING AIDS',		1).
valid_abstract_section_header('HIV/AIDS PREVENTION',		1).
valid_abstract_section_header('HNS-CHD',		1).
valid_abstract_section_header('HOLDING THE GAIN',		1).
valid_abstract_section_header('HOME MESSAGE',		1).
valid_abstract_section_header('HOMEOPATHIC TRIAL SUBSTANCE',		1).
valid_abstract_section_header('HONOURING THE OATH',		1).
valid_abstract_section_header('HOPEFUL PREMISE',		1).
valid_abstract_section_header('HORMONAL CONTRACEPTION',		1).
valid_abstract_section_header('HORMONAL CONTRACEPTION AND SEXUALITY',		1).
valid_abstract_section_header('HORMONAL EFFECTS',		1).
valid_abstract_section_header('HOSPITAL AND ACCREDITATION AGENCY ACTIVITIES ON PATIENT SAFETY ISSUES',		1).
valid_abstract_section_header('HOSPITAL AND CLINICAL PHARMACY IN ISRAEL',		1).
valid_abstract_section_header('HOSPITALS AND TEAMS',		1).
valid_abstract_section_header('HOST RANGE AND EPIDEMIOLOGY',		1).
valid_abstract_section_header('HOST RESPONSE',		1).
valid_abstract_section_header('HOST RESPONSE TO INFECTION',		1).
valid_abstract_section_header('HOW QI CAN REDUCE DISPARITIES',		1).
valid_abstract_section_header('HOW THE VQRC PROGRAM WORKS',		1).
valid_abstract_section_header('HOW TO IMPLEMENT THE FRAMEWORK',		1).
valid_abstract_section_header('HOW TO MAKE COMPUTERS TEAM PLAYERS',		1).
valid_abstract_section_header('HRCT PA',		1).
valid_abstract_section_header('HS CONCLUSION',		1).
valid_abstract_section_header('HUMAN AND ANIMAL FINDINGS',		1).
valid_abstract_section_header('HUMAN BABESIOSIS',		1).
valid_abstract_section_header('HUMAN CASES OF AVIAN INFLUENZA A',		1).
valid_abstract_section_header('HUMAN EXPOSURE',		1).
valid_abstract_section_header('HUMAN FACTORS',		1).
valid_abstract_section_header('HUMAN FACTORS ENGINEERING ANALYSIS',		1).
valid_abstract_section_header('HUMAN INFECTION',		1).
valid_abstract_section_header('HUMAN THERAPEUTICS',		1).
valid_abstract_section_header('HURDLES AND PROGRESSES',		1).
valid_abstract_section_header('HYALURONIC ACID',		1).
valid_abstract_section_header('HYALURONIC ACID DERMAL FILLERS',		1).
valid_abstract_section_header('HYDRODYNAMIC CONCEPT OF HYDROCEPHALUS',		1).
valid_abstract_section_header('HYGIENE AND EDUCATION',		1).
valid_abstract_section_header('HYPERAPOBETALIPOPROTEINEMIA',		1).
valid_abstract_section_header('HYPEREOSINOPHILIA WITH ORGAN DYSFUNCTION',		1).
valid_abstract_section_header('HYPERSCANNING',		1).
valid_abstract_section_header('HYPERTENSION AND IMPAIRED VASCULAR GROWTH',		1).
valid_abstract_section_header('HYPHOTESIS',		1).
valid_abstract_section_header('HYPOSTHESIS/OBJECTIVES',		1).
valid_abstract_section_header('HYPOTESIS',		1).
valid_abstract_section_header('HYPOTHERMIC CIRCULATORY ARREST (HCA)',		1).
valid_abstract_section_header('HYPOTHESES CONCERNING DEPRESSION',		1).
valid_abstract_section_header('HYPOTHESES/OBJECTIVES',		1).
valid_abstract_section_header('HYPOTHESIS / INTRODUCTION',		1).
valid_abstract_section_header('HYPOTHESIS AND CONCLUSIONS',		1).
valid_abstract_section_header('HYPOTHESIS AND EVIDENCE',		1).
valid_abstract_section_header('HYPOTHESIS AND RATIONALE',		1).
valid_abstract_section_header('HYPOTHESIS METHODS',		1).
valid_abstract_section_header('HYPOTHESIS TESTED',		1).
valid_abstract_section_header('HYPOTHESIZED PATHWAYS',		1).
valid_abstract_section_header('HYPOTHETICAL MECHANISMS',		1).
valid_abstract_section_header('HYSTEROSCOPY',		1).
valid_abstract_section_header('I BACKGROUND',		1).
valid_abstract_section_header('I CONCLUSIONS',		1).
valid_abstract_section_header('I DYSPHONIAS CAUSED BY PRIMARY FUNCTIONAL DISORDERS',		1).
valid_abstract_section_header('I GETTING INFORMATION ON THE PROBLEM AND PREVIOUS EXAMINATIONS',		1).
valid_abstract_section_header('I-CAA',		1).
valid_abstract_section_header('IA/B) HR',		1).
valid_abstract_section_header('ICD INDICATION',		1).
valid_abstract_section_header('ICSI IN THE EVOLVING MINNEAPOLIS MARKETPLACE',		1).
valid_abstract_section_header('ID/II',		1).
valid_abstract_section_header('IDENTIFICATION OF BEST PRACTICES AND RECOMMENDATIONS FOR FUTURE APPLICANTS',		1).
valid_abstract_section_header('IDENTIFICATION OF EFFECTS',		1).
valid_abstract_section_header('IDENTIFICATION OF STUDIES',		1).
valid_abstract_section_header('IDENTIFICATION OF SYSTEM ISSUES',		1).
valid_abstract_section_header('IDENTIFICATION OF THE PROBLEM',		1).
valid_abstract_section_header('IDENTIFYING THE ACCESS ISSUES',		1).
valid_abstract_section_header('IDENTIFYING THE PROBLEM',		1).
valid_abstract_section_header('IF VENOUS MATERIAL IS NOT AVAILABLE',		1).
valid_abstract_section_header('IGNORANCE',		1).
valid_abstract_section_header('II IRR',		1).
valid_abstract_section_header('II PREOPERATIVE PREPARATION',		1).
valid_abstract_section_header('III). RESULTS:',		1).
valid_abstract_section_header('IINTERVENTION',		1).
valid_abstract_section_header('ILLUSTRATION IMAGES',		1).
valid_abstract_section_header('IMAGE ANALYSIS',		1).
valid_abstract_section_header('IMAGE-DIRECTED BIOPSY',		1).
valid_abstract_section_header('IMAGING AND HORMONAL ASSESSMENT',		1).
valid_abstract_section_header('IMAGING FINDINGS OR PROCEDURE DETAILS',		1).
valid_abstract_section_header('IMAGING FOR THE PRETERM NEONATE',		1).
valid_abstract_section_header('IMAGING FOR THE TERM INFANT',		1).
valid_abstract_section_header('IMAGING INVESTIGATIONS',		1).
valid_abstract_section_header('IMAGING STUDIES',		1).
valid_abstract_section_header('IMAGING TECHNIQUES AND TREATMENT FOR CRANIOVERTEBRAL ABNORMALITIES',		1).
valid_abstract_section_header('IMATINIB IN CML',		1).
valid_abstract_section_header('IMATINIB IN GIST',		1).
valid_abstract_section_header('IMMEDIATE OUTCOMES',		1).
valid_abstract_section_header('IMMUNE EVASION MECHANISMS OF PROTOZOA',		1).
valid_abstract_section_header('IMMUNOADSORPTION',		1).
valid_abstract_section_header('IMMUNOGENICITY',		1).
valid_abstract_section_header('IMMUNOGENICITY OF PNEUMOCOCCAL CONJUGATE VACCINES',		1).
valid_abstract_section_header('IMMUNOGLOBULIN TREATMENT',		1).
valid_abstract_section_header('IMMUNOGLOBULINS',		1).
valid_abstract_section_header('IMMUNOGLOBULINS (IVIG)',		1).
valid_abstract_section_header('IMMUNOHISTOCHEMICAL INVESTIGATION',		1).
valid_abstract_section_header('IMMUNOHISTOCHEMISTRY AND STRUCTURAL FINDINGS',		1).
valid_abstract_section_header('IMMUNOLOGICAL ISSUE',		1).
valid_abstract_section_header('IMMUNOLOGICAL STATUS',		1).
valid_abstract_section_header('IMMUNOPATHOGENESIS OF GRANULOMA AND THE ROLE OF VITAMIN D',		1).
valid_abstract_section_header('IMMUNOSENESCENCE',		1).
valid_abstract_section_header('IMMUNOTHERAPY',		1).
valid_abstract_section_header('IMPACT FOR NURSING MANAGEMENT',		1).
valid_abstract_section_header('IMPACT OF CHANGES',		1).
valid_abstract_section_header('IMPACT OF EBM',		1).
valid_abstract_section_header('IMPACT OF EXTENT OF RESECTION ON OS',		1).
valid_abstract_section_header('IMPACT OF NUTRITION DURING PREGNANCY ON ORAL HEALTH',		1).
valid_abstract_section_header('IMPACT OF PROVIDING A FRAMEWORK FOR UNDERSTANDING QUALITY',		1).
valid_abstract_section_header('IMPACT OF STRUCTURAL INTERVENTIONS',		1).
valid_abstract_section_header('IMPACT OF THE IMR PROCESS',		1).
valid_abstract_section_header('IMPACT OF THE QUALITY IMPROVEMENT INITIATIVE',		1).
valid_abstract_section_header('IMPACT ON CLINICAL MANAGEMENT',		1).
valid_abstract_section_header('IMPACT ON PATIENT SAFETY',		1).
valid_abstract_section_header('IMPACT ON THE HEART',		1).
valid_abstract_section_header('IMPACT STATEMENT',		1).
valid_abstract_section_header('IMPACT/EFFECTIVENESS',		1).
valid_abstract_section_header('IMPAIRED COGNITIVE FLEXIBILITY',		1).
valid_abstract_section_header('IMPLEMENTATION AND EVALUATION OF QUALITY MANAGEMENT',		1).
valid_abstract_section_header('IMPLEMENTATION AND PERFORMANCES',		1).
valid_abstract_section_header('IMPLEMENTATION AND PROJECTS',		1).
valid_abstract_section_header('IMPLEMENTATION CHALLENGES AND SOLUTIONS',		1).
valid_abstract_section_header('IMPLEMENTATION CHALLENGES/ANALYTIC ISSUES',		1).
valid_abstract_section_header('IMPLEMENTATION OF CHANGE',		1).
valid_abstract_section_header('IMPLEMENTATION OF DOTS STRATEGY',		1).
valid_abstract_section_header('IMPLEMENTATION OF FEATURES',		1).
valid_abstract_section_header('IMPLEMENTATION OF THE DHPSP',		1).
valid_abstract_section_header('IMPLEMENTING A SYSTEMWIDE EHR',		1).
valid_abstract_section_header('IMPLEMENTING A TAX CAP',		1).
valid_abstract_section_header('IMPLEMENTING CHANGES',		1).
valid_abstract_section_header('IMPLEMENTING GUIDELINES',		1).
valid_abstract_section_header('IMPLEMENTING SIX SIGMA',		1).
valid_abstract_section_header('IMPLEMENTING THE CAMPAIGN',		1).
valid_abstract_section_header('IMPLEMENTING THE CRITICAL CARE PROJECT SYSTEMWIDE',		1).
valid_abstract_section_header('IMPLEMENTING THE MODEL',		1).
valid_abstract_section_header('IMPLEMENTING THE PILOT STUDY',		1).
valid_abstract_section_header('IMPLEMENTING THE QI PROCESS',		1).
valid_abstract_section_header('IMPLEMENTING THE UB MODEL',		1).
valid_abstract_section_header('IMPLEMENTING THE VIDEO VERIFICATION PROCESS',		1).
valid_abstract_section_header('IMPLEMENTING TQM',		1).
valid_abstract_section_header('IMPLICATION FOR CLINICAL PRACTICE',		1).
valid_abstract_section_header('IMPLICATION FOR FURTHER PRACTICE',		1).
valid_abstract_section_header('IMPLICATION FOR NURSING KNOWLEDGE',		1).
valid_abstract_section_header('IMPLICATION FOR NURSING LEADERSHIP',		1).
valid_abstract_section_header('IMPLICATION FOR PHYSIOTHERAPY PRACTICE',		1).
valid_abstract_section_header('IMPLICATION FOR POLICY DEVELOPMENT',		1).
valid_abstract_section_header('IMPLICATION FOR PRACTISE',		1).
valid_abstract_section_header('IMPLICATION FOR RESEARCH PRACTICE',		1).
valid_abstract_section_header('IMPLICATION OF CANCER SURVIVORS',		1).
valid_abstract_section_header('IMPLICATION PARAGRAPH',		1).
valid_abstract_section_header('IMPLICATION TO PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS AND PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS AND REMAINING QUESTIONS',		1).
valid_abstract_section_header('IMPLICATIONS AND SIGNIFICANCE',		1).
valid_abstract_section_header('IMPLICATIONS FOR CANCER SURVIVOR',		1).
valid_abstract_section_header('IMPLICATIONS FOR CARE MANAGEMENT PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS FOR CLINICAL NURSING PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS FOR CLINICIANS',		1).
valid_abstract_section_header('IMPLICATIONS FOR CNS PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS FOR CYTOMEGALOVIRUS INFECTION AND DISEASE',		1).
valid_abstract_section_header('IMPLICATIONS FOR DEVELOPMENT AND RESEARCH',		1).
valid_abstract_section_header('IMPLICATIONS FOR EDUCATION',		1).
valid_abstract_section_header('IMPLICATIONS FOR FUTURE RESEARCH AND PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH AND NURSING POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH AND SOCIAL CARE POLICIES',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH AND SOCIAL DEVELOPMENT POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH CARE PROVISIONS AND USE',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH POLICIES AND FURTHER RESEARCH',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH POLICIES AND FUTURE RESEARCH',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH POLICY AND NURSING',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTH POLICY AND RESEARCH',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTHCARE PROVISION',		1).
valid_abstract_section_header('IMPLICATIONS FOR HEALTHCARE PROVISION AND USE',		1).
valid_abstract_section_header('IMPLICATIONS FOR INTEGRATIVE CANCER CARE',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSE MANAGER',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSE PRACTITIONERS',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSES',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSES AND HEALTH POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSES AND MIDWIVES',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING & HEALTH POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING AND CONCLUSION',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING AND POLICY MAKERS',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING AND/HEALTH POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING EDUCATION AND POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING EDUCATION, TRAINING AND PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING POLICY AND EDUCATION',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING PRACTICE AND EDUCATION',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING PRACTICE AND RESEARCH',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING PRACTICE OR HEALTH POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING/HEALTH POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR NURSING/MIDWIFERY',		1).
valid_abstract_section_header('IMPLICATIONS FOR ORGANIZATIONAL AND PUBLIC POLICIES',		1).
valid_abstract_section_header('IMPLICATIONS FOR OTHER QUALITY IMPROVEMENT EFFORTS',		1).
valid_abstract_section_header('IMPLICATIONS FOR PERIANESTHESIA NURSES',		1).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE & RESEARCH',		1).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE -',		1).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE PERFORMANCE ASSESSMENT',		1).
valid_abstract_section_header('IMPLICATIONS FOR PRACTICE/ RESEARCH',		1).
valid_abstract_section_header('IMPLICATIONS FOR PRACTITIONERS',		1).
valid_abstract_section_header('IMPLICATIONS FOR RESEARCH -',		1).
valid_abstract_section_header('IMPLICATIONS FOR RESEARCH, PRACTICE, AND POLICY',		1).
valid_abstract_section_header('IMPLICATIONS FOR THERAPY',		1).
valid_abstract_section_header('IMPLICATIONS FOR TREATMENT',		1).
valid_abstract_section_header('IMPLICATIONS OF THE FINDINGS',		1).
valid_abstract_section_header('IMPLICATIONS OF THE WORK',		1).
valid_abstract_section_header('IMPLICATIONS OF THESE TENSIONS',		1).
valid_abstract_section_header('IMPLICATIONS ON NURSING AND HEALTH PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS ON PATIENT CARE',		1).
valid_abstract_section_header('IMPLICATIONS OR PRACTICE',		1).
valid_abstract_section_header('IMPLICATIONS TO NURSING CARE AND NURSING MANAGEMENT',		1).
valid_abstract_section_header('IMPLICATIONS, REASONS FOR CAUTION',		1).
valid_abstract_section_header('IMPLICATIONS/CONCLUSION',		1).
valid_abstract_section_header('IMPLICATIONS/DISCUSSION',		1).
valid_abstract_section_header('IMPORTANCE AND AIMS',		1).
valid_abstract_section_header('IMPORTANCE AND BACKGROUND:',		1).
valid_abstract_section_header('IMPORTANCE AND UTILITY',		1).
valid_abstract_section_header('IMPORTANCE OF ACE CATALYTIC SITES',		1).
valid_abstract_section_header('IMPORTANCE OF AN AMSSM STATEMENT',		1).
valid_abstract_section_header('IMPORTANCE OF EARLY DIAGNOSIS',		1).
valid_abstract_section_header('IMPORTANCE OF LEADERSHIP IN PATIENT SAFETY',		1).
valid_abstract_section_header('IMPORTANCE OF POSTPRANDIAL HYPERGLYCEMIA',		1).
valid_abstract_section_header('IMPORTANCE OF SURVEILLANCE',		1).
valid_abstract_section_header('IMPORTANCE OF THE AMANHI MORBIDITY STUDY',		1).
valid_abstract_section_header('IMPORTANCE OF THE AMANHI MORTALITY STUDY',		1).
valid_abstract_section_header('IMPORTANCE OF THIS STUDY',		1).
valid_abstract_section_header('IMPORTANCE OF TNF',		1).
valid_abstract_section_header('IMPORTANCE OF VENTILATION',		1).
valid_abstract_section_header('IMPORTANCE SECTION',		1).
valid_abstract_section_header('IMPORTANCE TO MEMBERS',		1).
valid_abstract_section_header('IMPORTANCE TO PEDIATRIC PHYSICAL THERAPY',		1).
valid_abstract_section_header('IMPORTANT CONSIDERATIONS',		1).
valid_abstract_section_header('IMPORTANT DRAWBACKS',		1).
valid_abstract_section_header('IMPORTANT GENES IDENTIFIED',		1).
valid_abstract_section_header('IMPORTANT HOSTS',		1).
valid_abstract_section_header('IMPORTANT POINTS',		1).
valid_abstract_section_header('IMPROVED PREDICTIVE FACTORS',		1).
valid_abstract_section_header('IMPROVEMENT CYCLE 1',		1).
valid_abstract_section_header('IMPROVEMENT EFFORTS',		1).
valid_abstract_section_header('IMPROVEMENT IN FOCUS AND OUTCOMES',		1).
valid_abstract_section_header('IMPROVEMENTS IN TREATMENT',		1).
valid_abstract_section_header('IMPROVING MEDICATION USE',		1).
valid_abstract_section_header('IMPROVING PI PROCESSES',		1).
valid_abstract_section_header('IMPROVING THE ENVIRONMENT FOR QUALITY',		1).
valid_abstract_section_header('IMR PROCESS',		1).
valid_abstract_section_header('IN BRIEF',		1).
valid_abstract_section_header('IN CONCLUSIONE',		1).
valid_abstract_section_header('IN DIABETIC PATIENTS',		1).
valid_abstract_section_header('IN NO DIABETIC SUBJECTS',		1).
valid_abstract_section_header('IN PROSPERO',		1).
valid_abstract_section_header('IN THE GENERAL POPULATION',		1).
valid_abstract_section_header('IN THE PRESENT STUDY WE ADDRESSED TWO NOVEL QUESTIONS',		1).
valid_abstract_section_header('IN THE PRESENT WORK, SIX CURIMATID SPECIES WERE ANALYZED',		1).
valid_abstract_section_header('IN THE SECOND HALF OF PREGNANCY',		1).
valid_abstract_section_header('IN THE SETTING OF ADJUVANT CHEMOTHERAPY',		1).
valid_abstract_section_header('IN THIS PAPER WE INTRODUCE A NEW GENERATION OF LANGUAGE TRAINERS',		1).
valid_abstract_section_header('IN THIS PAPER WE REPORT ON GROUND BEETLES (COLEOPTERA',		1).
valid_abstract_section_header('IN THIS PAPER, WE INVESTIGATED TWO QUESTIONS',		1).
valid_abstract_section_header('IN VITRO ACTIVITY ON MYCOBACTERIA',		1).
valid_abstract_section_header('IN VITRO MODELS',		1).
valid_abstract_section_header('INCIDENCE AND DIAGNOSIS',		1).
valid_abstract_section_header('INCIDENCE AND PREVALENCE OF CELIAC DISEASE',		1).
valid_abstract_section_header('INCIDENCE AND RISK FACTORS',		1).
valid_abstract_section_header('INCIDENCE OF CELIAC DISEASE IN THE GENERAL POPULATION',		1).
valid_abstract_section_header('INCIDENCE OF SUDDEN CARDIAC DEATH',		1).
valid_abstract_section_header('INCIDENCE, RISK FACTORS AND OUTCOME',		1).
valid_abstract_section_header('INCIDENT',		1).
valid_abstract_section_header('INCIDENT PATIENTS',		1).
valid_abstract_section_header('INCISION',		1).
valid_abstract_section_header('INCLUSION & EXCLUSION CRITERIA',		1).
valid_abstract_section_header('INCLUSION & EXCLUSIONS',		1).
valid_abstract_section_header('INCLUSION CRITERIA PARTICIPANTS',		1).
valid_abstract_section_header('INCLUSION CRITERIA, TYPES OF PARTICIPANTS',		1).
valid_abstract_section_header('INCLUSION- AND EXCLUSION CRITERIA',		1).
valid_abstract_section_header('INCORPORATING THE PRINCIPLES INTO PRACTICE',		1).
valid_abstract_section_header('INCREASE IN MINERAL ABSORPTION',		1).
valid_abstract_section_header('INCREASED BMI AND HYPERTENSION AS RISK FACTORS FOR CKD',		1).
valid_abstract_section_header('INCREASED OXIDATIVE MODIFICATION OF THE LDL AND HDL PARTICLES',		1).
valid_abstract_section_header('INCREASES IN STIFFNESS',		1).
valid_abstract_section_header('INCREASING THE QUALITY AND QUANTITY OF MS REHABILITATION RESEARCH',		1).
valid_abstract_section_header('INDEX',		1).
valid_abstract_section_header('INDEX & REFERENCE TESTS',		1).
valid_abstract_section_header('INDEXING',		1).
valid_abstract_section_header('INDICATION FOR EXAMINATION',		1).
valid_abstract_section_header('INDICATION FOR THERAPY',		1).
valid_abstract_section_header('INDICATIONS AND RESULTS',		1).
valid_abstract_section_header('INDICATIONS AND RISKS',		1).
valid_abstract_section_header('INDICATIONS FOR COLORECTAL ESD',		1).
valid_abstract_section_header('INDICATIONS FOR EMPIRICAL ANTIMICROBIAL THERAPY',		1).
valid_abstract_section_header('INDICATIONS FOR OPERATION',		1).
valid_abstract_section_header('INDICATIONS FOR RADIOGRAPHIC IMAGING OF THE HIP JOINT IN TWO PLANES',		1).
valid_abstract_section_header('INDICATIONS TO SURGICAL TREATMENT',		1).
valid_abstract_section_header('INDICATOR SELECTION AND DEVELOPMENT',		1).
valid_abstract_section_header('INDICES USED',		1).
valid_abstract_section_header('INDIKATIONEN',		1).
valid_abstract_section_header('INDIVIDUAL AWARD WINNERS',		1).
valid_abstract_section_header('INDIVIDUAL, GROUP, AND ORGANIZATIONAL FACTORS',		1).
valid_abstract_section_header('INDUCTION',		1).
valid_abstract_section_header('INFECTION AND IMMUNE ANOMALIES',		1).
valid_abstract_section_header('INFECTION AND INFLAMMATION',		1).
valid_abstract_section_header('INFECTION RATES AND CAUSATIVE FACTORS',		1).
valid_abstract_section_header('INFECTIOUS COMPLICATIONS',		1).
valid_abstract_section_header('INFLAMMATION AND BONE METABOLISM',		1).
valid_abstract_section_header('INFLAMMATION AND INFECTION',		1).
valid_abstract_section_header('INFLAMMATORY CYTOKINES',		1).
valid_abstract_section_header('INFLUENCE OF AGE ON THE PHARMACOLOGY OF ANALGESICS',		1).
valid_abstract_section_header('INFLUENCE OF MAGNETIC/ELECTROMAGNETIC FIELDS ON BIOLOGICAL SYSTEMS',		1).
valid_abstract_section_header('INFLUENZA VACCINATION',		1).
valid_abstract_section_header('INFLUENZA VIRUS',		1).
valid_abstract_section_header('INFORMATICS STANDARDS IN PATHOLOGY',		1).
valid_abstract_section_header('INFORMATION DISSEMINATION',		1).
valid_abstract_section_header('INFORMATION SOURCE',		1).
valid_abstract_section_header('INFORMATION SOURCES AND ANALYSIS',		1).
valid_abstract_section_header('INFORMATION SOURCES AND DATA EXTRACTION',		1).
valid_abstract_section_header('INFORMATIONS CONNUES',		1).
valid_abstract_section_header('INFORMING THE PATIENT',		1).
valid_abstract_section_header('INGREDIENTS',		1).
valid_abstract_section_header('INHIBITION OF THE RAS AND PROGRESSION OF RENAL DISEASES',		1).
valid_abstract_section_header('INHIBITORS OF PANCREAS SECRETION',		1).
valid_abstract_section_header('INITIAL ASSESSMENTS',		1).
valid_abstract_section_header('INITIAL TREATMENT AND CAUSES',		1).
valid_abstract_section_header('INITIATING TREATMENT',		1).
valid_abstract_section_header('INJECTION TECHNIQUE',		1).
valid_abstract_section_header('INJURIES AND METHODS',		1).
valid_abstract_section_header('INNOVATING SURGERY',		1).
valid_abstract_section_header('INNOVATION AND SIGNIFICANCE',		1).
valid_abstract_section_header('INNOVATIONS AND EVALUATION',		1).
valid_abstract_section_header('INNOVATIONS IN INTERACTION',		1).
valid_abstract_section_header('INNOVATIONS/PICTURES OF SITUATIONAL EXAMPLES',		1).
valid_abstract_section_header('INNOVATIVE APPROACHES',		1).
valid_abstract_section_header('INNOVATIVE SOLUTION',		1).
valid_abstract_section_header('INPATIENT RESULTS',		1).
valid_abstract_section_header('INQUILINE',		1).
valid_abstract_section_header('INRTODUCTION',		1).
valid_abstract_section_header('INSIGHTS FROM THE UDP',		1).
valid_abstract_section_header('INSIGHTS GAINED',		1).
valid_abstract_section_header('INSTITUTE OF LABORATORY MEDICINE OF THE CLINICAL CENTER OF VOJVODINA',		1).
valid_abstract_section_header('INSTITUTION ETHICS BOARD APPROVAL ID',		1).
valid_abstract_section_header('INSTITUTIONAL REVIEW BOARD THAT APPROVED THE STUDY',		1).
valid_abstract_section_header('INSTITUTIONAL REVIEWS',		1).
valid_abstract_section_header('INSTRUCTION METHODS',		1).
valid_abstract_section_header('INSTRUCTIONAL AIM AND CONTENTS',		1).
valid_abstract_section_header('INSTRUMENTS FOR DATA COLLECTION',		1).
valid_abstract_section_header('INSULIN RESISTANCE IN CHRONIC RENAL FAILURE',		1).
valid_abstract_section_header('INSULIN RESISTANCE IN HUMANS',		1).
valid_abstract_section_header('INTE RPRETATION',		1).
valid_abstract_section_header('INTEGRATED CASE MANAGEMENT',		1).
valid_abstract_section_header('INTEGRATED TESTING AND INTELLIGENT ASSESSMENT',		1).
valid_abstract_section_header('INTEGRATED TREATMENT CONCEPT',		1).
valid_abstract_section_header('INTEGRATING THE MODELS',		1).
valid_abstract_section_header('INTEGRATING TQM/QI INTO THE HWO PARADIGM',		1).
valid_abstract_section_header('INTELLIGENCE DATA',		1).
valid_abstract_section_header('INTENTION OF THE STUDY',		1).
valid_abstract_section_header('INTERACTION OF GRAPEFRUIT WITH DRUGS',		1).
valid_abstract_section_header('INTERACTIONS EVALUATED',		1).
valid_abstract_section_header('INTERACTIONS WITH NUCLEOSIDE ANALOGUES',		1).
valid_abstract_section_header('INTERACTIONS WITH PROTEASE INHIBITORS',		1).
valid_abstract_section_header('INTERACTIVE PLANNING',		1).
valid_abstract_section_header('INTEREST OF THE WORK',		1).
valid_abstract_section_header('INTERESTS AND OBJECTIVES',		1).
valid_abstract_section_header('INTERFERON THERAPY',		1).
valid_abstract_section_header('INTERFERONS',		1).
valid_abstract_section_header('INTERIM RESULTS',		1).
valid_abstract_section_header('INTERLEUKINS AND PROCALCITONIN IN DIAGNOSIS OF SEPSIS',		1).
valid_abstract_section_header('INTERMAXILLARY FIXATION TECHNIQUES',		1).
valid_abstract_section_header('INTERNAL AUDIT',		1).
valid_abstract_section_header('INTERNAL STRUCTURE',		1).
valid_abstract_section_header('INTERNATIONAL ASSOCIATION OF CANCER REGISTRIES',		1).
valid_abstract_section_header('INTERNATIONAL EXPERT ADVISORS',		1).
valid_abstract_section_header('INTERNATIONAL PERSPECTIVE',		1).
valid_abstract_section_header('INTERNATIONAL STANDARD RANDOMISED TRIAL NUMBER REGISTER',		1).
valid_abstract_section_header('INTEROPERABILITY IN PATHOLOGY INFORMATION SYSTEMS',		1).
valid_abstract_section_header('INTERPREATION',		1).
valid_abstract_section_header('INTERPRETATION AND DISCUSSION',		1).
valid_abstract_section_header('INTERPRETATION AND SIGNIFICANCE',		1).
valid_abstract_section_header('INTERPRETATION OF SEROLOGICAL RESULTS',		1).
valid_abstract_section_header('INTERPRETATION OF TRIALS',		1).
valid_abstract_section_header('INTERPRETATION/DISCUSSION',		1).
valid_abstract_section_header('INTERVAL BETWEEN RADIATION THERAPY AND POSTRADIATION SURGERY',		1).
valid_abstract_section_header('INTERVENCIONES',		1).
valid_abstract_section_header('INTERVENSIONS',		1).
valid_abstract_section_header('INTERVENTION & MEASUREMENTS',		1).
valid_abstract_section_header('INTERVENTION & OUTCOME',		1).
valid_abstract_section_header('INTERVENTION & OUTCOMES',		1).
valid_abstract_section_header('INTERVENTION A',		1).
valid_abstract_section_header('INTERVENTION AND COMPARATORS',		1).
valid_abstract_section_header('INTERVENTION AND CONTROL GROUPS',		1).
valid_abstract_section_header('INTERVENTION AND MAIN VARIABLE STUDIED',		1).
valid_abstract_section_header('INTERVENTION AND MANAGEMENT',		1).
valid_abstract_section_header('INTERVENTION AND PHENOMENON OF INTEREST',		1).
valid_abstract_section_header('INTERVENTION AND RESEARCH METHODS',		1).
valid_abstract_section_header('INTERVENTION AND VARIABLES',		1).
valid_abstract_section_header('INTERVENTION AS OUTCOME MEASUREMENTS AND STATISTICAL ANALYSIS',		1).
valid_abstract_section_header('INTERVENTION B',		1).
valid_abstract_section_header('INTERVENTION BA',		1).
valid_abstract_section_header('INTERVENTION CI MAIN OUTCOME MEASURE',		1).
valid_abstract_section_header('INTERVENTION DESCRIPTION',		1).
valid_abstract_section_header('INTERVENTION DETAILS',		1).
valid_abstract_section_header('INTERVENTION EMR MAIN OUTCOME MEASUREMENTS',		1).
valid_abstract_section_header('INTERVENTION ERCP MAIN OUTCOME MEASUREMENTS',		1).
valid_abstract_section_header('INTERVENTION GI',		1).
valid_abstract_section_header('INTERVENTION GROUPS',		1).
valid_abstract_section_header('INTERVENTION HC',		1).
valid_abstract_section_header('INTERVENTION PATIENT',		1).
valid_abstract_section_header('INTERVENTION RFA MAIN OUTCOME MEASUREMENTS',		1).
valid_abstract_section_header('INTERVENTION S',		1).
valid_abstract_section_header('INTERVENTION SC MAIN OUTCOME MEASUREMENTS',		1).
valid_abstract_section_header('INTERVENTION STUDIES',		1).
valid_abstract_section_header('INTERVENTION YG',		1).
valid_abstract_section_header('INTERVENTION(S) AND MEASUREMENT',		1).
valid_abstract_section_header('INTERVENTION(S) AND RESULTS',		1).
valid_abstract_section_header('INTERVENTION(S)/COMPARATOR(S)',		1).
valid_abstract_section_header('INTERVENTION/OBSERVATION',		1).
valid_abstract_section_header('INTERVENTION/PROGRAM/PRACTICE',		1).
valid_abstract_section_header('INTERVENTION/VARIABLES',		1).
valid_abstract_section_header('INTERVENTION:',		1).
valid_abstract_section_header('INTERVENTIONS AND COMPARATORS',		1).
valid_abstract_section_header('INTERVENTIONS AND HANDLING',		1).
valid_abstract_section_header('INTERVENTIONS AND MAIN OUTCOME MEASURE',		1).
valid_abstract_section_header('INTERVENTIONS AND MAIN OUTCOME MEASURE(S)',		1).
valid_abstract_section_header('INTERVENTIONS AND MEASURES',		1).
valid_abstract_section_header('INTERVENTIONS AND PREVENTION',		1).
valid_abstract_section_header('INTERVENTIONS AND RESEARCH',		1).
valid_abstract_section_header('INTERVENTIONS AND SUBJECTS',		1).
valid_abstract_section_header('INTERVENTIONS AVR MEASUREMENTS AND MAIN RESULTS',		1).
valid_abstract_section_header('INTERVENTIONS BPA MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('INTERVENTIONS EUS MAIN OUTCOME MEASUREMENTS',		1).
valid_abstract_section_header('INTERVENTIONS IN BARS',		1).
valid_abstract_section_header('INTERVENTIONS N/A MEASUREMENTS AND MAIN RESULTS',		1).
valid_abstract_section_header('INTERVENTIONS N/A MEASUREMENTS AND RESULTS',		1).
valid_abstract_section_header('INTERVENTIONS N/A PRIMARY AND SECONDARY OUTCOME MEASURES',		1).
valid_abstract_section_header('INTERVENTIONS OR EXPOSURES',		1).
valid_abstract_section_header('INTERVENTIONS OR MAIN EXPOSURES',		1).
valid_abstract_section_header('INTERVENTIONS POEM MAIN OUTCOME MEASUREMENTS',		1).
valid_abstract_section_header('INTERVENTIONS/EVENTS',		1).
valid_abstract_section_header('INTERVENTIONS/MAIN OUTCOMES MEASURED',		1).
valid_abstract_section_header('INTERVENTIONS/OUTCOME',		1).
valid_abstract_section_header('INTERVENTIONS/RESULTS',		1).
valid_abstract_section_header('INTERVENTIONS:',		1).
valid_abstract_section_header('INTESTINAL METAPLASIA',		1).
valid_abstract_section_header('INTRA-OPERATIVE',		1).
valid_abstract_section_header('INTRACELLULAR PERSISTENCE OF CHLAMYDIA',		1).
valid_abstract_section_header('INTRACELLULAR PROTEIN ANALYSIS',		1).
valid_abstract_section_header('INTRACEREBRAL ELECTRICAL STIMULATIONS',		1).
valid_abstract_section_header('INTRACRANIAL ANGIOPLASTY AND STENT REVASCULARIZATION',		1).
valid_abstract_section_header('INTRACRANIAL HEMORRHAGE IN PREMATURE NEWBORN INFANTS',		1).
valid_abstract_section_header('INTRAOPERATIVE',		1).
valid_abstract_section_header('INTRAOPERATIVE AND POSTOPERATIVE MONITORING',		1).
valid_abstract_section_header('INTRAOPERATIVE BLEEDING',		1).
valid_abstract_section_header('INTRAOPERATIVE MAPPING OF TUMORS IN ELOQUENT AREAS',		1).
valid_abstract_section_header('INTRAOPERATIVE NEUROPHYSIOLOGICAL TECHNIQUES',		1).
valid_abstract_section_header('INTRAOPERATIVE TREATMENT',		1).
valid_abstract_section_header('INTRAVENOUS AND INHALED ANAESTHETIC AGENTS',		1).
valid_abstract_section_header('INTRAVENOUS INJECTIONS',		1).
valid_abstract_section_header('INTRAVENOUS UROGRAPHY',		1).
valid_abstract_section_header('INTRIDUCTION',		1).
valid_abstract_section_header('INTRO/OBJECTIVE',		1).
valid_abstract_section_header('INTRODUCTION',		1).
valid_abstract_section_header('INTRODUCTION AND CASE REPORT',		1).
valid_abstract_section_header('INTRODUCTION AND CONTEXT',		1).
valid_abstract_section_header('INTRODUCTION AND HYPOTHESES',		1).
valid_abstract_section_header('INTRODUCTION AND METHODOLOGY',		1).
valid_abstract_section_header('INTRODUCTION AND OBJECCTIVES',		1).
valid_abstract_section_header('INTRODUCTION AND OBJECT',		1).
valid_abstract_section_header('INTRODUCTION AND OBJECTIVE OF THE STUDY',		1).
valid_abstract_section_header('INTRODUCTION AND OBJECTIVES AND AIMS',		1).
valid_abstract_section_header('INTRODUCTION AND POTENTIALS OF CLASSICAL RADIOTHERAPY',		1).
valid_abstract_section_header('INTRODUCTION AND PRESENTATION OF CASE',		1).
valid_abstract_section_header('INTRODUCTION AND PROBLEM',		1).
valid_abstract_section_header('INTRODUCTION AND PROGNOSIS',		1).
valid_abstract_section_header('INTRODUCTION AND PROPOSED STUDY',		1).
valid_abstract_section_header('INTRODUCTION AND RESULTS',		1).
valid_abstract_section_header('INTRODUCTION AND STUDY AIM',		1).
valid_abstract_section_header('INTRODUCTION OBJECTIVE',		1).
valid_abstract_section_header('INTRODUCTION OF SURFACE ANAESTHESIA',		1).
valid_abstract_section_header('INTRODUCTION, DEFECTS',		1).
valid_abstract_section_header('INTRODUCTION, MATERIAL AND METHODS',		1).
valid_abstract_section_header('INTRODUCTION, MATERIAL AND METHODS, AIM OF THE STUDY',		1).
valid_abstract_section_header('INTRODUCTION, OBJECTIVE',		1).
valid_abstract_section_header('INTRODUCTION-OBJECTIVE',		1).
valid_abstract_section_header('INTRODUCTION-OBJECTIVES',		1).
valid_abstract_section_header('INTRODUCTION.',		1).
valid_abstract_section_header('INTRODUCTION/CONTEXT',		1).
valid_abstract_section_header('INTRODUCTION/SIGNIFICANCE/POPULATION',		1).
valid_abstract_section_header('INTRODUCTIONS/OBJECTIVES',		1).
valid_abstract_section_header('INTRODUCTUION',		1).
valid_abstract_section_header('INTRODUZIONE ED OBIETTIVI',		1).
valid_abstract_section_header('INTRTODUCTION',		1).
valid_abstract_section_header('INTRUDUCTION',		1).
valid_abstract_section_header('INTRUODUCTION',		1).
valid_abstract_section_header('INVESTIGATED GROUPS AND METHODS',		1).
valid_abstract_section_header('INVESTIGATION AND RESEARCH',		1).
valid_abstract_section_header('INVESTIGATION COLLECTIVE',		1).
valid_abstract_section_header('INVESTIGATION OF PATHOGENESIS',		1).
valid_abstract_section_header('INVESTIGATION(S)',		1).
valid_abstract_section_header('INVESTIGATIONAL PLAN',		1).
valid_abstract_section_header('INVESTIGATIONS AND CLINICAL COURSE',		1).
valid_abstract_section_header('INVESTIGATIONS AND RESULTS',		1).
valid_abstract_section_header('INVESTIGATIONS AND THERAPY',		1).
valid_abstract_section_header('INVESTIGATIONS/DIAGNOSIS',		1).
valid_abstract_section_header('INVESTING GROUP AND METHOD',		1).
valid_abstract_section_header('INVITATION FOR PARTICIPATION',		1).
valid_abstract_section_header('IODINE IS A TRACE ELEMENT THAT IS FUNDAMENTAL FOR HUMAN HEALTH',		1).
valid_abstract_section_header('IPOTESI',		1).
valid_abstract_section_header('IRB APPROVAL',		1).
valid_abstract_section_header('IRB NUMBER',		1).
valid_abstract_section_header('IRB OR ETHICAL COMMITTEE APPROVAL',		1).
valid_abstract_section_header('IRBESARTAN',		1).
valid_abstract_section_header('IRCT NUMBER',		1).
valid_abstract_section_header('IRRITANT GASES INTOXICATION',		1).
valid_abstract_section_header('ISC) CONCLUSIONS',		1).
valid_abstract_section_header('ISOFORMS OF NOS',		1).
valid_abstract_section_header('ISOKINETIC RANGE OF MOTION (ROM) HAS THREE DISTINCT PHASES',		1).
valid_abstract_section_header('ISOLATED RADIOTHERAPY',		1).
valid_abstract_section_header('ISRCTN REGISTRATION NUMBER',		1).
valid_abstract_section_header('ISRCTN REGISTRY',		1).
valid_abstract_section_header('ISS GEMS',		1).
valid_abstract_section_header('ISSUES FACING THE DEANS',		1).
valid_abstract_section_header('ISSUES IN OBTAINING PHYSICIAN INVOLVEMENT',		1).
valid_abstract_section_header('IV IMMUNOGLOBULINS',		1).
valid_abstract_section_header('IVD PRODUCT EVALUATIONS AT ABBOTT DIAGNOSTICS',		1).
valid_abstract_section_header('Implications',		1).
valid_abstract_section_header('JAN VAN DIJK',		1).
valid_abstract_section_header('JAPAN PHARMACEUTICAL INFORMATION CENTER REGISTRATION',		1).
valid_abstract_section_header('JEL CLASSIFICATION CODES',		1).
valid_abstract_section_header('JEL CODE',		1).
valid_abstract_section_header('JOB PROFILES',		1).
valid_abstract_section_header('JUSTIFICATION FOR THE STUDY',		1).
valid_abstract_section_header('JUSTIFICATION OF AUTHORSHIP',		1).
valid_abstract_section_header('JUSTIFICATIVE',		1).
valid_abstract_section_header('KAPE SURVEYS IN ASIA',		1).
valid_abstract_section_header('KATE HILL',		1).
valid_abstract_section_header('KESIMPULAN',		1).
valid_abstract_section_header('KETAMINE',		1).
valid_abstract_section_header('KEY ACTION ITEMS AND RESULTS RELATED TO RCA',		1).
valid_abstract_section_header('KEY AREAS',		1).
valid_abstract_section_header('KEY AREAS SELECTED FOR ACTION',		1).
valid_abstract_section_header('KEY AREAS TARGETED FOR IMPROVEMENT',		1).
valid_abstract_section_header('KEY ASSUMPTIONS',		1).
valid_abstract_section_header('KEY CLINICAL MASSAGE',		1).
valid_abstract_section_header('KEY CLINICAL MEASSAGE',		1).
valid_abstract_section_header('KEY CONCLUSION AND IMPLICATION FOR PRACTICE',		1).
valid_abstract_section_header('KEY DATA AND STATISTICS',		1).
valid_abstract_section_header('KEY ELEMENTS OF CHANGE',		1).
valid_abstract_section_header('KEY FACETS OF EFFECTIVE POLICIES',		1).
valid_abstract_section_header('KEY FACTORS',		1).
valid_abstract_section_header('KEY FACTS',		1).
valid_abstract_section_header('KEY FINDINGS AND CONCLUSIONS',		1).
valid_abstract_section_header('KEY FINDINGS AND ISSUES IDENTIFIED',		1).
valid_abstract_section_header('KEY FINDINGS AND SIGNIFICANCE',		1).
valid_abstract_section_header('KEY FINDINGS FROM EVIDENCE SYNTHESIS',		1).
valid_abstract_section_header('KEY FINDINGS, IMPLICATIONS AND CONCLUSIONS',		1).
valid_abstract_section_header('KEY FINDINGS/RESULT',		1).
valid_abstract_section_header('KEY FNDINGS',		1).
valid_abstract_section_header('KEY IDEA AND MODEL',		1).
valid_abstract_section_header('KEY INSIGHTS',		1).
valid_abstract_section_header('KEY ISSUES AND CONCLUSIONS',		1).
valid_abstract_section_header('KEY ISSUES FOR GOOD RELATIONSHIP',		1).
valid_abstract_section_header('KEY LEARNING POINTS OF THIS ARTICLE',		1).
valid_abstract_section_header('KEY LESSONS/CONCLUSION',		1).
valid_abstract_section_header('KEY MANAGEMENT POINTS',		1).
valid_abstract_section_header('KEY MEASUREMENTS',		1).
valid_abstract_section_header('KEY MEASUREMENTS FOR IMPROVEMENT',		1).
valid_abstract_section_header('KEY MEASURES OF IMPROVEMENT',		1).
valid_abstract_section_header('KEY MESSAGE AND IMPLICATIONS',		1).
valid_abstract_section_header('KEY MESSAGE AND PRACTICAL IMPLICATION',		1).
valid_abstract_section_header('KEY MESSEAGE',		1).
valid_abstract_section_header('KEY METHODS AND PROCEDURAL ISSUES',		1).
valid_abstract_section_header('KEY OBJECTIVE',		1).
valid_abstract_section_header('KEY POINTS OF STABILIZATION',		1).
valid_abstract_section_header('KEY RESULT',		1).
valid_abstract_section_header('KEY RESULTS AND FINDINGS',		1).
valid_abstract_section_header('KEY STEPS/HURDLES ADDRESSED',		1).
valid_abstract_section_header('KEY TERMS',		1).
valid_abstract_section_header('KEY-MESSAGE',		1).
valid_abstract_section_header('KEY-WORDS',		1).
valid_abstract_section_header('KEYPHRASES',		1).
valid_abstract_section_header('KEYS FINDINGS',		1).
valid_abstract_section_header('KEYS RESULTS',		1).
valid_abstract_section_header('KLHL12 PROMOTES UBIQUITINATION OF THE DOPAMINE D4 RECEPTOR ON NON-LYSINE RESIDUES',		1).
valid_abstract_section_header('KNEE ARTHROGRAPHY MENISCAL AND EXTRAMENISCAL LESIONSABSTRACT',		1).
valid_abstract_section_header('KONTRAINDIKATIONEN',		1).
valid_abstract_section_header('KS CONCLUSIONS',		1).
valid_abstract_section_header('LABOR PERSPECTIVE',		1).
valid_abstract_section_header('LABORATORY',		1).
valid_abstract_section_header('LABORATORY DIAGNOSIS OF THROMBOPHILIA',		1).
valid_abstract_section_header('LABORATORY TECHNIQUES',		1).
valid_abstract_section_header('LABORATORY TEST RESULTS',		1).
valid_abstract_section_header('LACK OF DATA',		1).
valid_abstract_section_header('LACUNA IN KNOWLEDGE',		1).
valid_abstract_section_header('LARGE OUTBREAKS',		1).
valid_abstract_section_header('LAST FOLLOW-UP',		1).
valid_abstract_section_header('LATE SYNDROMES',		1).
valid_abstract_section_header('LATER COURSE',		1).
valid_abstract_section_header('LATERAL DISPLACEMENT',		1).
valid_abstract_section_header('LATEST STRATEGIES IN BYPASS STENOSES',		1).
valid_abstract_section_header('LAUNCHING THE PROGRAM',		1).
valid_abstract_section_header('LAUNCHING THE PROJECT',		1).
valid_abstract_section_header('LAUNCHING THE TRANSPARENCY INITIATIVE',		1).
valid_abstract_section_header('LAW CONCERNING PHYSISIANS',		1).
valid_abstract_section_header('LAY PERSON INTERPRETATION',		1).
valid_abstract_section_header('LC/MS/MS ASSAY',		1).
valid_abstract_section_header('LDL CHOLESTEROL AND PROGRESSION OF CORONARY CALCIUM',		1).
valid_abstract_section_header('LEADERSHIP',		1).
valid_abstract_section_header('LEADERSHIP AT THE FRONT LINE',		1).
valid_abstract_section_header('LEADERSHIP FOR QUALITY',		1).
valid_abstract_section_header('LEARNING FROM HISTORY',		1).
valid_abstract_section_header('LEARNING FROM THE PROCESS',		1).
valid_abstract_section_header('LEARNING SESSIONS',		1).
valid_abstract_section_header('LEARNING TO PERCEIVE IS FACED WITH A CLASSICAL PARADOX',		1).
valid_abstract_section_header('LEFT VENTRICULAR HYPERTROPHY',		1).
valid_abstract_section_header('LEG REGROWTH RESULTS',		1).
valid_abstract_section_header('LEGAL CONSIDERATIONS',		1).
valid_abstract_section_header('LEGAL/ETHICAL CONSIDERATIONS',		1).
valid_abstract_section_header('LEGISLATIVE ACTION',		1).
valid_abstract_section_header('LEGISLATIVE CHANGES',		1).
valid_abstract_section_header('LEISHMANIASIS HAS SEVERAL CLINICAL FORMS',		1).
valid_abstract_section_header('LENGTH OF SURVIVAL AND CANCER MORTALITY',		1).
valid_abstract_section_header('LENGTH OF TREATMENT',		1).
valid_abstract_section_header('LESBIAN AUTHORS, LESBIAN BOOKS',		1).
valid_abstract_section_header('LESBIAN EMPIRE',		1).
valid_abstract_section_header('LESSON',		1).
valid_abstract_section_header('LESSONS AND CHALLENGES',		1).
valid_abstract_section_header('LESSONS AND REFLECTIONS',		1).
valid_abstract_section_header('LESSONS FOR HEALTH DEVELOPMENT',		1).
valid_abstract_section_header('LESSONS FOR RESEARCH',		1).
valid_abstract_section_header('LESSONS FROM COMPETENCE ASSESSMENT',		1).
valid_abstract_section_header('LESSONS FROM LEAN THINKING',		1).
valid_abstract_section_header('LESSONS FROM MICRO PRACTICES',		1).
valid_abstract_section_header('LESSONS FROM THE LITERATURE',		1).
valid_abstract_section_header('LESSONS FROM THE PROGRAM',		1).
valid_abstract_section_header('LESSONS FROM THE RUSSIAN EXPERIENCE',		1).
valid_abstract_section_header('LESSONS LEARNED AND FUTURE PLANS',		1).
valid_abstract_section_header('LESSONS TO BE LEARNT',		1).
valid_abstract_section_header('LEVEL 4',		1).
valid_abstract_section_header('LEVEL DE EVIDENCE',		1).
valid_abstract_section_header('LEVEL IV',		1).
valid_abstract_section_header('LEVEL OF EVIDENCE & STUDY DESIGN',		1).
valid_abstract_section_header('LEVEL OF EVIDENCE (WITH STUDY DESIGN)',		1).
valid_abstract_section_header('LEVEL OF EVIDENCE AND ETHICAL STATEMENTS',		1).
valid_abstract_section_header('LEVEL OF EVIDENCE III TREATMENT STUDY',		1).
valid_abstract_section_header('LEVEL OF EVIDENCE IV CASE SERIES',		1).
valid_abstract_section_header('LEVEL OF EVIDENCE OF THE STUDY',		1).
valid_abstract_section_header('LEVEL OF EVIDENCE STATEMENT',		1).
valid_abstract_section_header('LEVEL OF RELEVANCE',		1).
valid_abstract_section_header('LEVEL OF SIGNIFICANCE',		1).
valid_abstract_section_header('LEVEL OF-EVIDENCE',		1).
valid_abstract_section_header('LEVEL-OF-EVIDENCE RATING',		1).
valid_abstract_section_header('LEVERAGING EXISTING SYSTEMS',		1).
valid_abstract_section_header('LICHEN PLANUS',		1).
valid_abstract_section_header('LICHEN PLANUS, ERYTHEMA MULTIFORME',		1).
valid_abstract_section_header('LIEU',		1).
valid_abstract_section_header('LIFE HISTORY',		1).
valid_abstract_section_header('LIFE STAGE CLASSIFICATION',		1).
valid_abstract_section_header('LIGHT OR ANTIDEPRESSANT DRUGS',		1).
valid_abstract_section_header('LIGHT SHEET MICROSCOPY IN THE MUSEUM',		1).
valid_abstract_section_header('LIGHT THERAPY MAY BE AN ADJUVANT TREATMENT FOR OTHER PSYCHIATRIC ILLNESSES WITH A SEASONAL TIME COURSE',		1).
valid_abstract_section_header('LIMITATION OF THE WORK',		1).
valid_abstract_section_header('LIMITATION, REASON FOR CAUTION',		1).
valid_abstract_section_header('LIMITATIONS AND FUTURE RESEARCH',		1).
valid_abstract_section_header('LIMITATIONS AND IMPLICATIONS',		1).
valid_abstract_section_header('LIMITATIONS AND LIMITS OF CAUTION',		1).
valid_abstract_section_header('LIMITATIONS AND PERSPECTIVES',		1).
valid_abstract_section_header('LIMITATIONS AND POSSIBLE UNINTENDED CONSEQUENCES OF THRESHOLD MEASURES',		1).
valid_abstract_section_header('LIMITATIONS AND WIDER IMPLICATIONS OF THE FINDINGS',		1).
valid_abstract_section_header('LIMITATIONS INCLUDE THE FOLLOWING',		1).
valid_abstract_section_header('LIMITATIONS OF CURRENT TECHNIQUES',		1).
valid_abstract_section_header('LIMITATIONS OF THE DESIGN',		1).
valid_abstract_section_header('LIMITATIONS, BARRIERS, AND NEXT DIRECTIONS',		1).
valid_abstract_section_header('LIMITATIONS, CONCERNS, AND CHALLENGES OF QUALITY MEASUREMENT IN ORAL ANTICOAGULATION',		1).
valid_abstract_section_header('LIMITATIONS, CONCLUSIONS AND IMPLICATION OF KEY FINDINGS',		1).
valid_abstract_section_header('LIMITATIONS, REASONS OF CAUTION',		1).
valid_abstract_section_header('LIMITED INDICATIONS',		1).
valid_abstract_section_header('LIMITES DE LA REVUE',		1).
valid_abstract_section_header('LIMTATIONS',		1).
valid_abstract_section_header('LINEAR VERSUS SIGMOID RELATIONSHIP BETWEEN BLOOD PRESSURE FALL AND DRUG CONCENTRATION',		1).
valid_abstract_section_header('LINKED EDITORIALS',		1).
valid_abstract_section_header('LINKED TRACS CLUSTER RANDOMISED CONTROLLED TRIAL NUMBER',		1).
valid_abstract_section_header('LINKING EVIDENCE TO ACTIONS',		1).
valid_abstract_section_header('LINKING GROUP AND ORGANIZATIONAL KNOWLEDGE TO IMPROVEMENT STRATEGIES',		1).
valid_abstract_section_header('LIPID AND LIPOPROTEIN DISORDERS',		1).
valid_abstract_section_header('LIST OF ABBREVIATIONS LIF',		1).
valid_abstract_section_header('LIST OF COMMERCIAL PRODUCTS',		1).
valid_abstract_section_header('LIST OF PROPRIETARY DEVICES CITED IN TEXT',		1).
valid_abstract_section_header('LITERARY METHOD',		1).
valid_abstract_section_header('LITERATURE FINDING',		1).
valid_abstract_section_header('LITERATURE ON DOUBLE GLOVING',		1).
valid_abstract_section_header('LITERATURE RESEARCH AND RESULTS',		1).
valid_abstract_section_header('LITERATURE SEARCH AND RESULTS',		1).
valid_abstract_section_header('LITERATURE SEARCH METHOD',		1).
valid_abstract_section_header('LITERATURE SEARCH METHODS',		1).
valid_abstract_section_header('LITERATURE SOURCE AND SELECTION CRITERIA',		1).
valid_abstract_section_header('LITERATURE SUMMARY',		1).
valid_abstract_section_header('LITERATURE SYNTHESIS',		1).
valid_abstract_section_header('LITERATURE-SEARCH-METHODOLOGY',		1).
valid_abstract_section_header('LIVING DONOR KIDNEY TRANSPLANTS',		1).
valid_abstract_section_header('LMD-ACS',		1).
valid_abstract_section_header('LN QUANTITY AND DISTRIBUTION',		1).
valid_abstract_section_header('LOCAL ANESTHETICS',		1).
valid_abstract_section_header('LOCAL CONTROL',		1).
valid_abstract_section_header('LOCAL TREATMENT',		1).
valid_abstract_section_header('LOCALISATION',		1).
valid_abstract_section_header('LOCALLY INDUCED FATIGUE',		1).
valid_abstract_section_header('LOCUSTS ARE GRASSHOPPER SPECIES THAT EXPRESS PHASE POLYPHENISM',		1).
valid_abstract_section_header('LOGIC OF VASODILATOR THERAPY',		1).
valid_abstract_section_header('LONG TERM FOLLOW UP RESULTS',		1).
valid_abstract_section_header('LONG-TERM COST-EFFECTIVENESS',		1).
valid_abstract_section_header('LONG-TERM STABILITY',		1).
valid_abstract_section_header('LONGHORN BEETLES (COLEOPTERA',		1).
valid_abstract_section_header('LOVE, MYSTERY, AND INTRIGUE FROM NAIAD PRESS',		1).
valid_abstract_section_header('LP(A) AND FIBRINOLYSIS',		1).
valid_abstract_section_header('LPFS CONCLUSION',		1).
valid_abstract_section_header('LUCAS\' POLITICAL CAREER',		1).
valid_abstract_section_header('LUNG DAMAGE BY IONIZING RADIATION',		1).
valid_abstract_section_header('LUNG LESIONS',		1).
valid_abstract_section_header('LUNG TRANSPLANTATION',		1).
valid_abstract_section_header('LYMPHATIC TISSUE',		1).
valid_abstract_section_header('LYMPHOCYTE DIFFERENTIATION',		1).
valid_abstract_section_header('M ETHODS',		1).
valid_abstract_section_header('M METHODS',		1).
valid_abstract_section_header('M-VAC',		1).
valid_abstract_section_header('MACROANGIOPATHY',		1).
valid_abstract_section_header('MACROFAUNA, MICROBES AND THE BENTHIC N-CYCLE',		1).
valid_abstract_section_header('MACROPHAGES CONSIST OF TWO MAIN SUBSETS',		1).
valid_abstract_section_header('MACROSCOPY',		1).
valid_abstract_section_header('MACROSCOPY AND HISTOPATHOLOGY',		1).
valid_abstract_section_header('MAGNITUDE OF THE RISK',		1).
valid_abstract_section_header('MAIN',		1).
valid_abstract_section_header('MAIN AGENTS',		1).
valid_abstract_section_header('MAIN AIM',		1).
valid_abstract_section_header('MAIN ASSESSMENT MEASURES',		1).
valid_abstract_section_header('MAIN BODY OF THE ABSTRACT',		1).
valid_abstract_section_header('MAIN CHALLENGES IDENTIFIED',		1).
valid_abstract_section_header('MAIN CLINICAL OUTCOME MEASURES',		1).
valid_abstract_section_header('MAIN COMPARISONS',		1).
valid_abstract_section_header('MAIN COMPONENT OF PROGRAM',		1).
valid_abstract_section_header('MAIN CONCERNS, IMPORTANT FINDINGS',		1).
valid_abstract_section_header('MAIN COST AND OUTCOME MEASURES',		1).
valid_abstract_section_header('MAIN DATA REPORTED',		1).
valid_abstract_section_header('MAIN DISCUSSION AND CONCLUSIONS',		1).
valid_abstract_section_header('MAIN DISCUSSION POINTS',		1).
valid_abstract_section_header('MAIN EVIDENCE',		1).
valid_abstract_section_header('MAIN EXPOSURE AND OUTCOME MEASURES',		1).
valid_abstract_section_header('MAIN EXPOSURE VARIABLES',		1).
valid_abstract_section_header('MAIN FEATURES AND CHALLENGES',		1).
valid_abstract_section_header('MAIN FEATURES, MATERIALS AND METHODS',		1).
valid_abstract_section_header('MAIN FINDING AND CONCLUSION',		1).
valid_abstract_section_header('MAIN FINDINGS, KEY DATA, AND STATISTICS',		1).
valid_abstract_section_header('MAIN FINDINGS/SIGNIFICANCE',		1).
valid_abstract_section_header('MAIN LESSONS',		1).
valid_abstract_section_header('MAIN LESSONS LEARNED FROM THIS CASE',		1).
valid_abstract_section_header('MAIN LIMITATIONS OF THE STUDY',		1).
valid_abstract_section_header('MAIN MEASUREMENT OUTCOMES',		1).
valid_abstract_section_header('MAIN MEASURES AND ANALYSES',		1).
valid_abstract_section_header('MAIN MEASURES/APPROACH',		1).
valid_abstract_section_header('MAIN MEAURES',		1).
valid_abstract_section_header('MAIN MESSAGES/TEACHING POINTS',		1).
valid_abstract_section_header('MAIN METHEODS',		1).
valid_abstract_section_header('MAIN OBJECTIVE MEASURE',		1).
valid_abstract_section_header('MAIN OUT COME MEASURES',		1).
valid_abstract_section_header('MAIN OUTCOME AND MEASURE(S)',		1).
valid_abstract_section_header('MAIN OUTCOME AND THE ROLE OF CHANCE',		1).
valid_abstract_section_header('MAIN OUTCOME ASSESSMENT',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE (S)',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE ADL CAT RESULTS',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE DOCS RESULTS',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE DRS RESULTS',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE LEMOCOT RESULTS',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE S',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE(S) FSH RESULT(S)',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE(S) VMS RESULT(S)',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE(S):',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURE:',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURES (DEPENDENT VARIABLE)',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURES AND ANALYSIS',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURES IMT RESULTS',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURES STUDIED',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURES TMT, PASAT RESULTS',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURES:',		1).
valid_abstract_section_header('MAIN OUTCOME MEASURS',		1).
valid_abstract_section_header('MAIN OUTCOME(S) AND MEASURES:',		1).
valid_abstract_section_header('MAIN OUTCOME-MEASURE',		1).
valid_abstract_section_header('MAIN OUTCOME/RESULTS',		1).
valid_abstract_section_header('MAIN OUTCOMES AND MEASUREMENTS',		1).
valid_abstract_section_header('MAIN OUTCOMES AND MEASURES METHODS',		1).
valid_abstract_section_header('MAIN OUTCOMES, RESULT AND CONCLUSIONS',		1).
valid_abstract_section_header('MAIN OUTCOMES/INTERVENTIONS',		1).
valid_abstract_section_header('MAIN OUTCOMES/MEASUREMENTS',		1).
valid_abstract_section_header('MAIN OUTCOMES/MEASURES',		1).
valid_abstract_section_header('MAIN OUTCOMESS AND MEASURES',		1).
valid_abstract_section_header('MAIN PARTICIPANTS AND RESEARCH CONTEXT',		1).
valid_abstract_section_header('MAIN POINT',		1).
valid_abstract_section_header('MAIN POINTS AND CONCLUSIONS',		1).
valid_abstract_section_header('MAIN POINTS CONSIDERED',		1).
valid_abstract_section_header('MAIN PREDICTOR MEASURE',		1).
valid_abstract_section_header('MAIN PREDICTOR MEASURES',		1).
valid_abstract_section_header('MAIN PREDICTORS MEASURES',		1).
valid_abstract_section_header('MAIN PURPOSE AND RESEARCH QUESTION',		1).
valid_abstract_section_header('MAIN REMARKS',		1).
valid_abstract_section_header('MAIN RESULTS AND DISCUSSION',		1).
valid_abstract_section_header('MAIN RESULTS AND MEASUREMENTS',		1).
valid_abstract_section_header('MAIN RESULTS AND ROLE OF THE CHANCE',		1).
valid_abstract_section_header('MAIN RESULTS AND THE ROLE OF CHANCE THE GUIDELINE PROVIDES',		1).
valid_abstract_section_header('MAIN RESULTS AND THE ROLE OF CHOICE',		1).
valid_abstract_section_header('MAIN RESULTS AND THE ROLE OF THE CHANCE',		1).
valid_abstract_section_header('MAIN RESULTS AND THEIR SIGNIFICANCE',		1).
valid_abstract_section_header('MAIN RESULTS OF THE REVIEW',		1).
valid_abstract_section_header('MAIN RESULTS, AND THE ROLE OF CHANCE',		1).
valid_abstract_section_header('MAIN RESULTS/DISCUSSION',		1).
valid_abstract_section_header('MAIN RESULTS/MEASUREMENTS',		1).
valid_abstract_section_header('MAIN STATEMENTS',		1).
valid_abstract_section_header('MAIN STUDY PARAMETER',		1).
valid_abstract_section_header('MAIN STUDY PARAMETERS/ENDPOINTS',		1).
valid_abstract_section_header('MAIN THESES',		1).
valid_abstract_section_header('MAIN TYPES OF DATA COLLECTED',		1).
valid_abstract_section_header('MAIN VARIABLES/DESCRIPTIVE DATA',		1).
valid_abstract_section_header('MAINTAINING THE GAIN',		1).
valid_abstract_section_header('MAINTENANCE',		1).
valid_abstract_section_header('MAINTENANCE THERAPY',		1).
valid_abstract_section_header('MAIZE EAR FASCIATION',		1).
valid_abstract_section_header('MAJOR ASPECTS',		1).
valid_abstract_section_header('MAJOR CHALLENGES',		1).
valid_abstract_section_header('MAJOR COMPLICATIONS AND DEATHS',		1).
valid_abstract_section_header('MAJOR CONCEPTS IN THE DEFINITION',		1).
valid_abstract_section_header('MAJOR CONCLUSIONS AND SIGNIFICANCE',		1).
valid_abstract_section_header('MAJOR DISCUSSION POINTS',		1).
valid_abstract_section_header('MAJOR EFFORTS',		1).
valid_abstract_section_header('MAJOR ELEMENTS OF A CLINICAL INFORMATION SYSTEM',		1).
valid_abstract_section_header('MAJOR FINDINGS AND CONCLUSIONS',		1).
valid_abstract_section_header('MAJOR FINDINGS AND PRINCIPAL CONCLUSION',		1).
valid_abstract_section_header('MAJOR INFORMATION',		1).
valid_abstract_section_header('MAJOR MANAGEMENT',		1).
valid_abstract_section_header('MAJOR OUTCOMES AND MEASURES',		1).
valid_abstract_section_header('MAJOR POINTS CONSIDERED',		1).
valid_abstract_section_header('MAJOR POINTS STUDIED',		1).
valid_abstract_section_header('MAJOR PRINCIPLES OF THE THEORY',		1).
valid_abstract_section_header('MAJOR RECOMMENDATIONS',		1).
valid_abstract_section_header('MAJOR RISK FACTORS',		1).
valid_abstract_section_header('MAJOR THEMES AND CONCLUSIONS',		1).
valid_abstract_section_header('MAJOR TOPICS DISCUSSED',		1).
valid_abstract_section_header('MAKING THE TRANSITION FROM QUALITY ASSURANCE TO QUALITY IMPROVEMENT',		1).
valid_abstract_section_header('MALE HORMONAL CONTRACEPTION',		1).
valid_abstract_section_header('MALERIALS AND METHODS',		1).
valid_abstract_section_header('MALIGNANT HYPERCALCEMIA',		1).
valid_abstract_section_header('MALIGNANT HYPONATREMIA',		1).
valid_abstract_section_header('MALT',		1).
valid_abstract_section_header('MANAGEMENT AND PREVENTION',		1).
valid_abstract_section_header('MANAGEMENT AND PROGNOSIS',		1).
valid_abstract_section_header('MANAGEMENT APPROACH',		1).
valid_abstract_section_header('MANAGEMENT CONSENSUS',		1).
valid_abstract_section_header('MANAGEMENT GUIDELINES',		1).
valid_abstract_section_header('MANAGEMENT IMPLICATIONS',		1).
valid_abstract_section_header('MANAGEMENT ISSUES',		1).
valid_abstract_section_header('MANAGEMENT OF ACUTE EPISTAXIS',		1).
valid_abstract_section_header('MANAGEMENT OF COINFECTIONS',		1).
valid_abstract_section_header('MANAGEMENT OF CONGENITAL CLUBFOOT IN CHILDREN',		1).
valid_abstract_section_header('MANAGEMENT OF FRAILTY',		1).
valid_abstract_section_header('MANAGEMENT OF FUNCTIONAL PROBLEMS',		1).
valid_abstract_section_header('MANAGEMENT OF POP',		1).
valid_abstract_section_header('MANAGEMENT OF RELAPSED/REFRACTORY DISEASE',		1).
valid_abstract_section_header('MANAGEMENT OF SEVERE ADVERSE EVENTS',		1).
valid_abstract_section_header('MANAGEMENT OF TB',		1).
valid_abstract_section_header('MANAGEMENT OF TB IN SPECIAL SITUATIONS',		1).
valid_abstract_section_header('MANAGEMENT OF THROMBOPHILIA',		1).
valid_abstract_section_header('MANAGEMENT PROPOSAL',		1).
valid_abstract_section_header('MANAGING EXTERNAL ENVIRONMENT',		1).
valid_abstract_section_header('MANDATORY REQUIREMENTS',		1).
valid_abstract_section_header('MANI RESULTS',		1).
valid_abstract_section_header('MANPOWER',		1).
valid_abstract_section_header('MANUAL (PHYSICAL) DIAGNOSIS',		1).
valid_abstract_section_header('MANUAL (PHYSICAL) THERAPY',		1).
valid_abstract_section_header('MARCO DE REFERENCIA Y OBJETIVO',		1).
valid_abstract_section_header('MARCO DE REFERNCIA',		1).
valid_abstract_section_header('MARETIAL AND METHODS',		1).
valid_abstract_section_header('MARKERS AND RISK FACTORS',		1).
valid_abstract_section_header('MARKET ACCOUNTABILITY',		1).
valid_abstract_section_header('MARTERIALS AND METHODS',		1).
valid_abstract_section_header('MARTIALS & METHODS',		1).
valid_abstract_section_header('MARTIALS AND METHODS',		1).
valid_abstract_section_header('MASS SPECTROMETRY ANALYSIS',		1).
valid_abstract_section_header('MATERAL AND METHODS',		1).
valid_abstract_section_header('MATERIAL & METHODS AND RESULTS',		1).
valid_abstract_section_header('MATERIAL (CASES)',		1).
valid_abstract_section_header('MATERIAL (PATIENTS)',		1).
valid_abstract_section_header('MATERIAL AL METHOD',		1).
valid_abstract_section_header('MATERIAL AND ANALYSIS',		1).
valid_abstract_section_header('MATERIAL AND CONTEXT',		1).
valid_abstract_section_header('MATERIAL AND FINDINGS',		1).
valid_abstract_section_header('MATERIAL AND METHOD DESIGN',		1).
valid_abstract_section_header('MATERIAL AND METHOD(S)',		1).
valid_abstract_section_header('MATERIAL AND METHODES',		1).
valid_abstract_section_header('MATERIAL AND METHODS AND RESULTS',		1).
valid_abstract_section_header('MATERIAL AND METHODS RESULTS',		1).
valid_abstract_section_header('MATERIAL AND METHODS, RESULTS AND CONCLUSION',		1).
valid_abstract_section_header('MATERIAL AND METHODS/AIM',		1).
valid_abstract_section_header('MATERIAL AND OBSERVATION',		1).
valid_abstract_section_header('MATERIAL AND PROCEDURE',		1).
valid_abstract_section_header('MATERIAL ANDF METHODS',		1).
valid_abstract_section_header('MATERIAL ANDMETHODS',		1).
valid_abstract_section_header('MATERIAL ANS METHODS',		1).
valid_abstract_section_header('MATERIAL E METHODS',		1).
valid_abstract_section_header('MATERIAL END METHODS',		1).
valid_abstract_section_header('MATERIAL ET METHOD',		1).
valid_abstract_section_header('MATERIAL METHODS AND SURGICAL TECHNIQUE',		1).
valid_abstract_section_header('MATERIAL OF THE STUDY',		1).
valid_abstract_section_header('MATERIAL OR SUBJECT',		1).
valid_abstract_section_header('MATERIAL REVIEWED',		1).
valid_abstract_section_header('MATERIAL S',		1).
valid_abstract_section_header('MATERIAL UND METHODIK',		1).
valid_abstract_section_header('MATERIAL Y METHODS',		1).
valid_abstract_section_header('MATERIAL, METHOD AND RESULT',		1).
valid_abstract_section_header('MATERIAL, METHODOLOGY AND RESULTS',		1).
valid_abstract_section_header('MATERIAL, METHODS AND THERAPY',		1).
valid_abstract_section_header('MATERIAL, RESULTS, AND INTERPRETATION',		1).
valid_abstract_section_header('MATERIAL, SETTING AND METHODS',		1).
valid_abstract_section_header('MATERIAL, SETTING, METHODS',		1).
valid_abstract_section_header('MATERIAL, SUBJECTS, METHODS',		1).
valid_abstract_section_header('MATERIAL/ METHODS',		1).
valid_abstract_section_header('MATERIAL/ PATIENTS AND METHODS',		1).
valid_abstract_section_header('MATERIAL/MEHODS',		1).
valid_abstract_section_header('MATERIAL/PARTICIPANTS',		1).
valid_abstract_section_header('MATERIAL/PATIENTS & METHODS',		1).
valid_abstract_section_header('MATERIAL/RESULTS',		1).
valid_abstract_section_header('MATERIAL/SUBJECT AND METHODS',		1).
valid_abstract_section_header('MATERIALA AND METHODS',		1).
valid_abstract_section_header('MATERIALE DELLO STUDIO',		1).
valid_abstract_section_header('MATERIALES Y METODOS',		1).
valid_abstract_section_header('MATERIALI METODI',		1).
valid_abstract_section_header('MATERIALIEN UND METHODE',		1).
valid_abstract_section_header('MATERIALIEN UND METHODEN',		1).
valid_abstract_section_header('MATERIALS & RESULTS',		1).
valid_abstract_section_header('MATERIALS AN METHODS',		1).
valid_abstract_section_header('MATERIALS ANATOMY',		1).
valid_abstract_section_header('MATERIALS AND AND METHODS',		1).
valid_abstract_section_header('MATERIALS AND DISCUSSION',		1).
valid_abstract_section_header('MATERIALS AND EQUIPMENT',		1).
valid_abstract_section_header('MATERIALS AND MAIN RESULTS',		1).
valid_abstract_section_header('MATERIALS AND METHODES',		1).
valid_abstract_section_header('MATERIALS AND METHODS (DESIGN, SETTING AND PATIENTS)',		1).
valid_abstract_section_header('MATERIALS AND METHODS (STUDY DESIGN)',		1).
valid_abstract_section_header('MATERIALS AND METHODS - STUDY SETTING',		1).
valid_abstract_section_header('MATERIALS AND METHODS AND TREATMENT',		1).
valid_abstract_section_header('MATERIALS AND METHODS OF THE STUDY',		1).
valid_abstract_section_header('MATERIALS AND METHOLOGY',		1).
valid_abstract_section_header('MATERIALS AND THE METHODS',		1).
valid_abstract_section_header('MATERIALS ANDD METHODS',		1).
valid_abstract_section_header('MATERIALS OF THE STUDY',		1).
valid_abstract_section_header('MATERIALS PATIENTS AND METHODS',		1).
valid_abstract_section_header('MATERIALS SETTING',		1).
valid_abstract_section_header('MATERIALS, MEASURES AND METHODS',		1).
valid_abstract_section_header('MATERIALS, METHOD AND RESULTS',		1).
valid_abstract_section_header('MATERIALS, METHODS, AND PROCEDURES',		1).
valid_abstract_section_header('MATERIALS, RESULTS, CONCLUSIONS',		1).
valid_abstract_section_header('MATERIALS/METHODS/RESULTS',		1).
valid_abstract_section_header('MATERIALS/METHODS:',		1).
valid_abstract_section_header('MATERIALS/PARTICIPANTS',		1).
valid_abstract_section_header('MATERIALS/PATIENTS',		1).
valid_abstract_section_header('MATERIALS/PATIENTS & METHODS',		1).
valid_abstract_section_header('MATERIALS/SUBJECTS',		1).
valid_abstract_section_header('MATERIA? I METODA WYNIKI',		1).
valid_abstract_section_header('MATERIEL AND METHOD',		1).
valid_abstract_section_header('MATERIEL ET METHODES',		1).
valid_abstract_section_header('MATERIEL/PATIENTS AND METHODS',		1).
valid_abstract_section_header('MATERIELS AND METHODS',		1).
valid_abstract_section_header('MATERILI E METODI',		1).
valid_abstract_section_header('MATERNAL AGE',		1).
valid_abstract_section_header('MATERNAL RISKS',		1).
valid_abstract_section_header('MATERYAL VE METOD',		1).
valid_abstract_section_header('MATER?AL AND METHODS',		1).
valid_abstract_section_header('MATHEMATICAL ANALYSIS',		1).
valid_abstract_section_header('MATHEMATICAL DESCRIPTION',		1).
valid_abstract_section_header('MATHEMATICAL METHODS',		1).
valid_abstract_section_header('MATHEMATICAL MODELING',		1).
valid_abstract_section_header('MATHEMATICAL MODELS',		1).
valid_abstract_section_header('MATHEMATICS SUBJECT CLASSIFICATION 2000',		1).
valid_abstract_section_header('MATHEMATICS SUBJECT CLASSIFICATIONS',		1).
valid_abstract_section_header('MATHERIAL OF STUDY',		1).
valid_abstract_section_header('MATHERIALS & METHODS',		1).
valid_abstract_section_header('MATHERIALS OF STUDY',		1).
valid_abstract_section_header('MATHODS AND RESULTS',		1).
valid_abstract_section_header('MATREIALS AND METHODS',		1).
valid_abstract_section_header('MATRIAL AND METHODS',		1).
valid_abstract_section_header('MATURE PROJECTS',		1).
valid_abstract_section_header('MAZ-DM',		1).
valid_abstract_section_header('MBP\'S AMOROUS INFERNO',		1).
valid_abstract_section_header('MEAN OUTCOME RESULTS',		1).
valid_abstract_section_header('MEANING AND IMPLICATIONS OF THE ADVANCE',		1).
valid_abstract_section_header('MEASURAMENTS AND MAIN RESULTS',		1).
valid_abstract_section_header('MEASURE INSTRUMENTS',		1).
valid_abstract_section_header('MEASURED VARIABLES',		1).
valid_abstract_section_header('MEASUREMENT AND ANALYSIS',		1).
valid_abstract_section_header('MEASUREMENT AND PARTICIPANTS',		1).
valid_abstract_section_header('MEASUREMENT AND VALIDITY',		1).
valid_abstract_section_header('MEASUREMENT CRITERIA',		1).
valid_abstract_section_header('MEASUREMENT INSTRUMENT',		1).
valid_abstract_section_header('MEASUREMENT INSTRUMENTS',		1).
valid_abstract_section_header('MEASUREMENT METHODS IN CLINICAL GAIT ANALYSIS',		1).
valid_abstract_section_header('MEASUREMENT OF EXPOSURE',		1).
valid_abstract_section_header('MEASUREMENT OF QUALITY OF LIFE',		1).
valid_abstract_section_header('MEASUREMENT TECHNIQUES',		1).
valid_abstract_section_header('MEASUREMENT VARIABLES',		1).
valid_abstract_section_header('MEASUREMENTS & MAIN RESULTS',		1).
valid_abstract_section_header('MEASUREMENTS AND MAJOR RESULTS',		1).
valid_abstract_section_header('MEASUREMENTS AND MEAN RESULTS',		1).
valid_abstract_section_header('MEASUREMENTS AND OBSERVATIONS',		1).
valid_abstract_section_header('MEASUREMENTS AND PRIMARY OUTCOMES',		1).
valid_abstract_section_header('MEASUREMENTS AND PRIMARY RESULTS',		1).
valid_abstract_section_header('MEASUREMENTS MAIN RESULTS',		1).
valid_abstract_section_header('MEASUREMENTS OF ENDOTHELIAL FUNCTION',		1).
valid_abstract_section_header('MEASUREMENTS/APPROACH',		1).
valid_abstract_section_header('MEASUREMENTS/STATISTICAL ANALYSIS',		1).
valid_abstract_section_header('MEASURES AND FINDING',		1).
valid_abstract_section_header('MEASURES AND METHOD',		1).
valid_abstract_section_header('MEASURES AND OUTCOME',		1).
valid_abstract_section_header('MEASURES/ANALYSES',		1).
valid_abstract_section_header('MEASURES/APPROACH',		1).
valid_abstract_section_header('MEASURES/METHODS',		1).
valid_abstract_section_header('MEASURING DECISIONAL CONTROL',		1).
valid_abstract_section_header('MEASURING QUALITY OF LIFE',		1).
valid_abstract_section_header('MEASURING TOOL',		1).
valid_abstract_section_header('MEASURING TOOLS',		1).
valid_abstract_section_header('MEASURING WOUND VOLUME',		1).
valid_abstract_section_header('MEAUREMENTS',		1).
valid_abstract_section_header('MECHANICAL COMPLICATIONS',		1).
valid_abstract_section_header('MECHANICAL PRINCIPLE',		1).
valid_abstract_section_header('MECHANICAL STRENGTH STUDIES',		1).
valid_abstract_section_header('MECHANICAL STUDY',		1).
valid_abstract_section_header('MECHANISM OF ACE ANCHORAGE AND SOLUBILIZATION',		1).
valid_abstract_section_header('MECHANISM OF ACE INHIBITOR COUGH',		1).
valid_abstract_section_header('MECHANISM OF ACTION AND PHARMACODYNAMICS',		1).
valid_abstract_section_header('MECHANISM OF APOPTOSIS IN HASHIMOTO THYROIDITIS',		1).
valid_abstract_section_header('MECHANISM OF EXCEPTIONAL STRESS RESISTANCE',		1).
valid_abstract_section_header('MECHANISM OF IMMUNOSTIMULATION',		1).
valid_abstract_section_header('MECHANISM PROGRAMMED DEATH CELL',		1).
valid_abstract_section_header('MECHANISMS AND CAUSES',		1).
valid_abstract_section_header('MECHANISMS IN DEVELOPMENT OF DRY EYE DISEASE',		1).
valid_abstract_section_header('MECHANISMS OF BONE PAIN',		1).
valid_abstract_section_header('MECHANISMS OF HOMOCYSTEINE ACTION',		1).
valid_abstract_section_header('MECHODS',		1).
valid_abstract_section_header('MEDIAN AGE',		1).
valid_abstract_section_header('MEDIAN FOLLOW UP',		1).
valid_abstract_section_header('MEDIASTINAL LESIONS',		1).
valid_abstract_section_header('MEDIATOR',		1).
valid_abstract_section_header('MEDICAL ABORTIONS AT OUR DEPARTMENT OF OBSTETRICS AND GYNECOLOGY, NOVI SAD, CLINICAL CENTER VOJVODINE, SERBIA',		1).
valid_abstract_section_header('MEDICAL ADVISORY SECRETARIAT REVIEW',		1).
valid_abstract_section_header('MEDICAL ANTIREFLUX THERAPY',		1).
valid_abstract_section_header('MEDICAL AUDIT',		1).
valid_abstract_section_header('MEDICAL CHEMOTHERAPY',		1).
valid_abstract_section_header('MEDICAL COURSE',		1).
valid_abstract_section_header('MEDICAL EFFECTIVENESS FINDINGS',		1).
valid_abstract_section_header('MEDICAL ETHICS AND DISSEMINATION',		1).
valid_abstract_section_header('MEDICAL JOURNALS IN VOJVODINA',		1).
valid_abstract_section_header('MEDICAL REGISTRIES',		1).
valid_abstract_section_header('MEDICAL SOCIETIES IN VOJVODINA',		1).
valid_abstract_section_header('MEDICAL TEACHING DURING THE WAR',		1).
valid_abstract_section_header('MEDICAL THERAPY',		1).
valid_abstract_section_header('MEDICAL WRITING',		1).
valid_abstract_section_header('MEDICATION ERRORS AFFECT THE PEDIATRIC AGE GROUP IN ALL SETTINGS',		1).
valid_abstract_section_header('MEDICATION USE PROBLEM',		1).
valid_abstract_section_header('MEDICINAL PLANTS',		1).
valid_abstract_section_header('MEDICINE IN THE NOTAPHILY IN FORMER YUGOSLAVIA, SERBIA AND MONTENEGRO AND SERBIA',		1).
valid_abstract_section_header('MEDICINE IN THE NOTAPHILY IN THE WORLD',		1).
valid_abstract_section_header('MEDICIONES',		1).
valid_abstract_section_header('MEDICIONES DE RESULTADOS PRINCIPALES',		1).
valid_abstract_section_header('MEDICIONES DE RESULTADOS SECUNDARIOS',		1).
valid_abstract_section_header('MEDICOLEGAL',		1).
valid_abstract_section_header('MEDICOLEGAL ASPECTS',		1).
valid_abstract_section_header('MEDIUM TO LONG TERM',		1).
valid_abstract_section_header('MEETING OF EXPERTS',		1).
valid_abstract_section_header('MEETING OUTCOMES',		1).
valid_abstract_section_header('MEETING PRESENTATION',		1).
valid_abstract_section_header('MESH',		1).
valid_abstract_section_header('MESSAGES/TEACHING POINTS',		1).
valid_abstract_section_header('MESULTS',		1).
valid_abstract_section_header('MESUREMENTS',		1).
valid_abstract_section_header('METABOLIC ACIDOSIS',		1).
valid_abstract_section_header('METABOLIC AND BIOCHEMICAL ASPECTS',		1).
valid_abstract_section_header('METABOLIC DISEASES',		1).
valid_abstract_section_header('METABOLIC DISORDERS AS RISK FACTORS',		1).
valid_abstract_section_header('METABOLIC FLEXIBILITY WAS ASSESSED IN MALE ZUCKER RATS',		1).
valid_abstract_section_header('METABOLOMIC ANALYSIS',		1).
valid_abstract_section_header('METARIAL AND METHODS',		1).
valid_abstract_section_header('METHOD',		1).
valid_abstract_section_header('METHOD & EXAMINATION',		1).
valid_abstract_section_header('METHOD & SUBJECTS',		1).
valid_abstract_section_header('METHOD (INTERVENTION STRATEGIES)',		1).
valid_abstract_section_header('METHOD AND ANALYSES',		1).
valid_abstract_section_header('METHOD AND APPLICATION',		1).
valid_abstract_section_header('METHOD AND DATA SOURCES',		1).
valid_abstract_section_header('METHOD AND EVALUATION OF RESULTS',		1).
valid_abstract_section_header('METHOD AND EXPERIMENTAL DESIGN',		1).
valid_abstract_section_header('METHOD AND FINDING',		1).
valid_abstract_section_header('METHOD AND IMPLICATIONS',		1).
valid_abstract_section_header('METHOD AND LIMITATIONS',		1).
valid_abstract_section_header('METHOD AND MAIN FINDINGS',		1).
valid_abstract_section_header('METHOD AND METHODOLOGY',		1).
valid_abstract_section_header('METHOD AND OBSERVATION',		1).
valid_abstract_section_header('METHOD AND OUTCOME MEASURES',		1).
valid_abstract_section_header('METHOD AND OUTCOMES',		1).
valid_abstract_section_header('METHOD AND STUDY',		1).
valid_abstract_section_header('METHOD AND STUDY DESIGN',		1).
valid_abstract_section_header('METHOD AND TECHNIQUE',		1).
valid_abstract_section_header('METHOD OF APPRAISAL',		1).
valid_abstract_section_header('METHOD OF DIAGNOSIS AND DECISION MAKING',		1).
valid_abstract_section_header('METHOD OF EVIDENCE COLLECTION',		1).
valid_abstract_section_header('METHOD OF PATIENT INVOLVEMENT',		1).
valid_abstract_section_header('METHOD OF PROTOCOL DEVELOPMENT',		1).
valid_abstract_section_header('METHOD OF STUDY/RESULTS',		1).
valid_abstract_section_header('METHOD STUDY SELECTION CRITERIA',		1).
valid_abstract_section_header('METHOD USED',		1).
valid_abstract_section_header('METHOD, DISCUSSION, AND RESULTS',		1).
valid_abstract_section_header('METHOD, PATIENTS',		1).
valid_abstract_section_header('METHOD, RESULTS AND CONCLUSION',		1).
valid_abstract_section_header('METHOD, RESULTS, CONCLUSIONS',		1).
valid_abstract_section_header('METHOD/ANALYSIS',		1).
valid_abstract_section_header('METHOD/APPROACH',		1).
valid_abstract_section_header('METHOD/DATA SOURCES',		1).
valid_abstract_section_header('METHOD/DESIGNS',		1).
valid_abstract_section_header('METHOD/OBJECTIVE',		1).
valid_abstract_section_header('METHOD/OBJECTIVES',		1).
valid_abstract_section_header('METHOD/OUTCOME',		1).
valid_abstract_section_header('METHOD/PROCEDURE',		1).
valid_abstract_section_header('METHOD/RESULTS/CONCLUSION',		1).
valid_abstract_section_header('METHOD/SEARCH STRATEGY',		1).
valid_abstract_section_header('METHOD/STUDY DESIGN',		1).
valid_abstract_section_header('METHOD/SUBJECTS',		1).
valid_abstract_section_header('METHOD/TECHNIQUE',		1).
valid_abstract_section_header('METHODA',		1).
valid_abstract_section_header('METHODEN UND ERGEBNISSE',		1).
valid_abstract_section_header('METHODICAL INNOVATIONS AND PERFORMANCE',		1).
valid_abstract_section_header('METHODODOLOGY',		1).
valid_abstract_section_header('METHODOLGY AND FINDINGS',		1).
valid_abstract_section_header('METHODOLOGIC PROBLEMS',		1).
valid_abstract_section_header('METHODOLOGICAL CONTEXT',		1).
valid_abstract_section_header('METHODOLOGICAL DESIGN AND INSTRUMENT',		1).
valid_abstract_section_header('METHODOLOGICAL DESIGN AND RESEARCH METHODS',		1).
valid_abstract_section_header('METHODOLOGICAL DIVERSITY TO BETTER CAPTURE CAUSAL MECHANISMS AND PROCESSES',		1).
valid_abstract_section_header('METHODOLOGICAL INCONSISTENCIES',		1).
valid_abstract_section_header('METHODOLOGICAL PURITY',		1).
valid_abstract_section_header('METHODOLOGICAL VALIDITY',		1).
valid_abstract_section_header('METHODOLOGIES AND PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('METHODOLOGIES/PRINCIPLE FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY & PRINCIPLE FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY / PRINCIPAL FINDING',		1).
valid_abstract_section_header('METHODOLOGY / PRINCIPLE FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY / PRINCIPLES FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY AND ANALYSIS',		1).
valid_abstract_section_header('METHODOLOGY AND CRITICAL FINDING',		1).
valid_abstract_section_header('METHODOLOGY AND CRITICAL FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY AND DATA ANALYSIS',		1).
valid_abstract_section_header('METHODOLOGY AND DISCUSSION',		1).
valid_abstract_section_header('METHODOLOGY AND IMPLEMENTATION',		1).
valid_abstract_section_header('METHODOLOGY AND MAIN FINDING',		1).
valid_abstract_section_header('METHODOLOGY AND MAJOR FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY AND PRINCIPAL',		1).
valid_abstract_section_header('METHODOLOGY AND/OR PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY, RESULTS',		1).
valid_abstract_section_header('METHODOLOGY/ FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY/ METHODS',		1).
valid_abstract_section_header('METHODOLOGY/ PRINCIPAL FINDING',		1).
valid_abstract_section_header('METHODOLOGY/ RESULTS',		1).
valid_abstract_section_header('METHODOLOGY//PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY/DESIGN/SETTING',		1).
valid_abstract_section_header('METHODOLOGY/HYPOTHESES',		1).
valid_abstract_section_header('METHODOLOGY/MAIN FINDING',		1).
valid_abstract_section_header('METHODOLOGY/MAJOR FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY/MAJOR RESULTS',		1).
valid_abstract_section_header('METHODOLOGY/PARTICIPANTS',		1).
valid_abstract_section_header('METHODOLOGY/PHYSICAL FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY/PRELIMINARY FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY/PRIMARY FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY/PRINCIPAL FIDNINGS',		1).
valid_abstract_section_header('METHODOLOGY/PRINCIPAL FINDINGS AND CONCLUSIONS',		1).
valid_abstract_section_header('METHODOLOGY/PRINCIPAL FINDINGS, AND CONCLUSIONS/SIGNIFICANCE',		1).
valid_abstract_section_header('METHODOLOGY/PRINCIPAL FINDS',		1).
valid_abstract_section_header('METHODOLOGY/PRINCIPAL RESULTS',		1).
valid_abstract_section_header('METHODOLOGY/PRINCIPALS',		1).
valid_abstract_section_header('METHODOLOGY/PRINCIPALS FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY/PRINICPAL FINDINGS',		1).
valid_abstract_section_header('METHODOLOGY/SIGNIFICANT FINDING',		1).
valid_abstract_section_header('METHODOLOGY:',		1).
valid_abstract_section_header('METHODOS',		1).
valid_abstract_section_header('METHODS & DESIGN/ RESULTS',		1).
valid_abstract_section_header('METHODS & MAIN RESULTS',		1).
valid_abstract_section_header('METHODS & OBJECTIVES',		1).
valid_abstract_section_header('METHODS & OUTCOME MEASURES',		1).
valid_abstract_section_header('METHODS & PROCEDURES/OUTCOMES & RESULTS',		1).
valid_abstract_section_header('METHODS & SAMPLE',		1).
valid_abstract_section_header('METHODS (CASE DESCRIPTION)',		1).
valid_abstract_section_header('METHODS (CLINICAL)',		1).
valid_abstract_section_header('METHODS (DEVELOPMENT STUDY)',		1).
valid_abstract_section_header('METHODS (I)',		1).
valid_abstract_section_header('METHODS (MODEL DESCRIPTION)',		1).
valid_abstract_section_header('METHODS (VALIDATION STUDY)',		1).
valid_abstract_section_header('METHODS -',		1).
valid_abstract_section_header('METHODS - STUDY DESIGN',		1).
valid_abstract_section_header('METHODS / OUTCOME MEASURES',		1).
valid_abstract_section_header('METHODS / PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('METHODS /DESIGN',		1).
valid_abstract_section_header('METHODS A',		1).
valid_abstract_section_header('METHODS ACQUISITION',		1).
valid_abstract_section_header('METHODS AND ANTICIPATED RESULTS',		1).
valid_abstract_section_header('METHODS AND APPARATUS',		1).
valid_abstract_section_header('METHODS AND CASE REPORT',		1).
valid_abstract_section_header('METHODS AND CASES',		1).
valid_abstract_section_header('METHODS AND CLINICAL DATA',		1).
valid_abstract_section_header('METHODS AND COHORT',		1).
valid_abstract_section_header('METHODS AND CONSENSUS',		1).
valid_abstract_section_header('METHODS AND CONTENTS',		1).
valid_abstract_section_header('METHODS AND DATA ANALYSIS',		1).
valid_abstract_section_header('METHODS AND DATA EXTRACTION',		1).
valid_abstract_section_header('METHODS AND DESCRIPTION',		1).
valid_abstract_section_header('METHODS AND DEVICE',		1).
valid_abstract_section_header('METHODS AND DISCUSSIONS',		1).
valid_abstract_section_header('METHODS AND EMPIRICAL STRATEGY',		1).
valid_abstract_section_header('METHODS AND EVIDENCE',		1).
valid_abstract_section_header('METHODS AND EXPECTED RESULTS',		1).
valid_abstract_section_header('METHODS AND FINDS',		1).
valid_abstract_section_header('METHODS AND FRAMEWORK',		1).
valid_abstract_section_header('METHODS AND INSTRUMENTS',		1).
valid_abstract_section_header('METHODS AND KEY FINDINGS',		1).
valid_abstract_section_header('METHODS AND LIMITATIONS',		1).
valid_abstract_section_header('METHODS AND MAIN OUTCOME MEASURE',		1).
valid_abstract_section_header('METHODS AND MAIN OUTCOMES MEASURES',		1).
valid_abstract_section_header('METHODS AND MATERIALS/RESULTS',		1).
valid_abstract_section_header('METHODS AND MATERIALS:',		1).
valid_abstract_section_header('METHODS AND MEASUREMENT',		1).
valid_abstract_section_header('METHODS AND OBJECTIVE',		1).
valid_abstract_section_header('METHODS AND OPERATIVE TECHNIQUE',		1).
valid_abstract_section_header('METHODS AND PATIENT COHORT',		1).
valid_abstract_section_header('METHODS AND PRINCIPAL FINDING',		1).
valid_abstract_section_header('METHODS AND PROGRESS',		1).
valid_abstract_section_header('METHODS AND QUESTIONS',		1).
valid_abstract_section_header('METHODS AND RECOMMENDATIONS',		1).
valid_abstract_section_header('METHODS AND RESOURCES',		1).
valid_abstract_section_header('METHODS AND RESULES',		1).
valid_abstract_section_header('METHODS AND RESULTS (CASE)',		1).
valid_abstract_section_header('METHODS AND RESULTS FOR CHRONIC BRONCHITIS',		1).
valid_abstract_section_header('METHODS AND RESULTS FOR EMPHYSEMA',		1).
valid_abstract_section_header('METHODS AND RESULTS I',		1).
valid_abstract_section_header('METHODS AND RESULTS II',		1).
valid_abstract_section_header('METHODS AND RESULTS PERTAINING TO SR-HA/CPC',		1).
valid_abstract_section_header('METHODS AND SAMPLES',		1).
valid_abstract_section_header('METHODS AND SELECTION CRITERIA',		1).
valid_abstract_section_header('METHODS AND SERVICE DESCRIPTION',		1).
valid_abstract_section_header('METHODS AND SOLUTION',		1).
valid_abstract_section_header('METHODS AND SOURCES',		1).
valid_abstract_section_header('METHODS AND STOCK DEVELOPMENT',		1).
valid_abstract_section_header('METHODS AND STRAIN SELECTION',		1).
valid_abstract_section_header('METHODS AND STUDY OUTCOMES',		1).
valid_abstract_section_header('METHODS AND SUBJECT',		1).
valid_abstract_section_header('METHODS AND SUMMARY OF BACKGROUND DATA',		1).
valid_abstract_section_header('METHODS AND SURGICAL TECHNIQUE',		1).
valid_abstract_section_header('METHODS AND THEORY',		1).
valid_abstract_section_header('METHODS AND TREATMENT',		1).
valid_abstract_section_header('METHODS AND TREATMENTS',		1).
valid_abstract_section_header('METHODS AND WORK',		1).
valid_abstract_section_header('METHODS CURRENT KNOWLEDGE',		1).
valid_abstract_section_header('METHODS DATA SOURCES',		1).
valid_abstract_section_header('METHODS DESCRIBED',		1).
valid_abstract_section_header('METHODS DESIGN, SETTING, AND PARTICIPANTS',		1).
valid_abstract_section_header('METHODS FIELD BASED',		1).
valid_abstract_section_header('METHODS FINDINGS',		1).
valid_abstract_section_header('METHODS FOR ESTIMATION OF THE RELIABILITY',		1).
valid_abstract_section_header('METHODS FOR INTERPRETING GAIT ANALYSIS DATA',		1).
valid_abstract_section_header('METHODS FOR QUANTIFICATION OF MEASURING HEARING ACUITY',		1).
valid_abstract_section_header('METHODS FOR THE REVIEW',		1).
valid_abstract_section_header('METHODS FOR UNDERSTANDING THE EFFECTS OF INTERVENTION',		1).
valid_abstract_section_header('METHODS IN TOTAL,',		1).
valid_abstract_section_header('METHODS ND ANALYSIS',		1).
valid_abstract_section_header('METHODS OF ASSESSING ARTERIAL ABNORMALITIES',		1).
valid_abstract_section_header('METHODS OF DETECTION',		1).
valid_abstract_section_header('METHODS OF DEVELOPING CLINICAL GUIDELINES',		1).
valid_abstract_section_header('METHODS OF EVALUATION',		1).
valid_abstract_section_header('METHODS OF IMPLEMENTATION',		1).
valid_abstract_section_header('METHODS OF REVIEW',		1).
valid_abstract_section_header('METHODS OF SOFT TISSUE RECONSTRUCTION',		1).
valid_abstract_section_header('METHODS OF STATEMENT DEVELOPMENT',		1).
valid_abstract_section_header('METHODS OF THE STUDY SELECTION',		1).
valid_abstract_section_header('METHODS OF TREATMENT',		1).
valid_abstract_section_header('METHODS OF ULTRASONOGRAPHY IN DERMATOLOGY',		1).
valid_abstract_section_header('METHODS ONE HUNDRED TWELVE',		1).
valid_abstract_section_header('METHODS PART I',		1).
valid_abstract_section_header('METHODS PART II',		1).
valid_abstract_section_header('METHODS PROCEDURES',		1).
valid_abstract_section_header('METHODS RESULTS CONCLUSIONS',		1).
valid_abstract_section_header('METHODS REVIEW',		1).
valid_abstract_section_header('METHODS SETTING',		1).
valid_abstract_section_header('METHODS SETTING AND PARTICIPANTS',		1).
valid_abstract_section_header('METHODS THE AUTHORS CONDUCTED',		1).
valid_abstract_section_header('METHODS THE AUTHORS DISTRIBUTED',		1).
valid_abstract_section_header('METHODS TO CALCULATE NORMATIVE ADHERENCE',		1).
valid_abstract_section_header('METHODS USED TO CONDUCT THE STUDY',		1).
valid_abstract_section_header('METHODS& MATERIALS',		1).
valid_abstract_section_header('METHODS(AND PATIENTS)',		1).
valid_abstract_section_header('METHODS(S)',		1).
valid_abstract_section_header('METHODS, DATA AND MATERIALS',		1).
valid_abstract_section_header('METHODS, MATERIAL AND RESULTS',		1).
valid_abstract_section_header('METHODS, MATERIALS AND RESULTS',		1).
valid_abstract_section_header('METHODS, MEASUREMENTS',		1).
valid_abstract_section_header('METHODS, OUTCOMES DATA AND STATISTICAL ANALYSIS',		1).
valid_abstract_section_header('METHODS, PARTICIPANTS',		1).
valid_abstract_section_header('METHODS, PATIENTS, MATERIAL',		1).
valid_abstract_section_header('METHODS, PATIENTS, RESULTS',		1).
valid_abstract_section_header('METHODS, RESEARCH DESIGN AND PROCEDURES',		1).
valid_abstract_section_header('METHODS, RESULTS AND DISCUSSION',		1).
valid_abstract_section_header('METHODS, RESULTS, AND CONCLUSION',		1).
valid_abstract_section_header('METHODS, RESULTS, DISCUSSION, CONCLUSION',		1).
valid_abstract_section_header('METHODS, SETTING, AND PARTICIPANTS',		1).
valid_abstract_section_header('METHODS, SETTING, AND SUBJECTS',		1).
valid_abstract_section_header('METHODS-',		1).
valid_abstract_section_header('METHODS-DESIGN',		1).
valid_abstract_section_header('METHODS-PARTICIPANTS AND MEASUREMENTS',		1).
valid_abstract_section_header('METHODS-PATIENTS',		1).
valid_abstract_section_header('METHODS.',		1).
valid_abstract_section_header('METHODS/ DESIGN',		1).
valid_abstract_section_header('METHODS/ FINDINGS',		1).
valid_abstract_section_header('METHODS/ PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('METHODS/ RESULTS',		1).
valid_abstract_section_header('METHODS/ACTIVITIES',		1).
valid_abstract_section_header('METHODS/AIMS',		1).
valid_abstract_section_header('METHODS/ANIMALS',		1).
valid_abstract_section_header('METHODS/CASE',		1).
valid_abstract_section_header('METHODS/CASE DESCRIPTION',		1).
valid_abstract_section_header('METHODS/CASE REPORT',		1).
valid_abstract_section_header('METHODS/CASE REPORTS',		1).
valid_abstract_section_header('METHODS/CASE SUMMARY',		1).
valid_abstract_section_header('METHODS/CASES',		1).
valid_abstract_section_header('METHODS/CONTENT',		1).
valid_abstract_section_header('METHODS/DATA BASE',		1).
valid_abstract_section_header('METHODS/DESIGN INDEX GROUP',		1).
valid_abstract_section_header('METHODS/DESIGN/PARTICIPANTS',		1).
valid_abstract_section_header('METHODS/EVIDENCE',		1).
valid_abstract_section_header('METHODS/EVIDENCE ACQUISITION',		1).
valid_abstract_section_header('METHODS/FINDINGS/INTERPRETATION',		1).
valid_abstract_section_header('METHODS/IMPORTANCE',		1).
valid_abstract_section_header('METHODS/INITIATIVES',		1).
valid_abstract_section_header('METHODS/INTERVENTIONS',		1).
valid_abstract_section_header('METHODS/MEASUREMENTS',		1).
valid_abstract_section_header('METHODS/OUTCOME MEASURES',		1).
valid_abstract_section_header('METHODS/PATIENTS AND MATERIALS',		1).
valid_abstract_section_header('METHODS/PRELIMINARY FINDINGS',		1).
valid_abstract_section_header('METHODS/PRINCIPAL RESULTS',		1).
valid_abstract_section_header('METHODS/PROCEDURE',		1).
valid_abstract_section_header('METHODS/RESEARCH DESIGN',		1).
valid_abstract_section_header('METHODS/RESULTS AND CONCLUSIONS',		1).
valid_abstract_section_header('METHODS/RESULTS AND FINDINGS',		1).
valid_abstract_section_header('METHODS/RESULTS/CONCLUSIONS',		1).
valid_abstract_section_header('METHODS/SETTINGS',		1).
valid_abstract_section_header('METHODS/STRATEGIES',		1).
valid_abstract_section_header('METHODS: MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('METHODS: PARTICIPANTS',		1).
valid_abstract_section_header('METHODS: SETTING',		1).
valid_abstract_section_header('METHODSAND RESULTS',		1).
valid_abstract_section_header('METHODSDESIGN',		1).
valid_abstract_section_header('METHOLODOGY',		1).
valid_abstract_section_header('METHOLODOLOGY/PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('METOD OF STUDY SELECTION',		1).
valid_abstract_section_header('METODOLOGY',		1).
valid_abstract_section_header('METOLOGIA',		1).
valid_abstract_section_header('MICROABSTRACT',		1).
valid_abstract_section_header('MICROANGIOPATHY STUDY',		1).
valid_abstract_section_header('MICROBIOLOGICAL DIAGNOSIS',		1).
valid_abstract_section_header('MICROBIOLOGICAL EVALUATION',		1).
valid_abstract_section_header('MICROBIOLOGICAL EXAMINATION',		1).
valid_abstract_section_header('MICROBIOLOGICAL FINDINGS',		1).
valid_abstract_section_header('MICROSCOPIC CHARACTERISTICS',		1).
valid_abstract_section_header('MICROSCOPIC DIFFERENTIAL DIAGNOSIS',		1).
valid_abstract_section_header('MID-CON',		1).
valid_abstract_section_header('MIGRAINE AND VASCULAR DISEASES',		1).
valid_abstract_section_header('MILD TO MODERATE DISEASE',		1).
valid_abstract_section_header('MILITARY AREAS',		1).
valid_abstract_section_header('MILITARY SITES',		1).
valid_abstract_section_header('MINERALS',		1).
valid_abstract_section_header('MINI ABSTRACT',		1).
valid_abstract_section_header('MINIATURIZATION',		1).
valid_abstract_section_header('MITOCARE',		1).
valid_abstract_section_header('MITOGENIC INFLUENCE',		1).
valid_abstract_section_header('MIXED MODELS',		1).
valid_abstract_section_header('MMETHODS',		1).
valid_abstract_section_header('MOBBING IN NURSING PROFESSION',		1).
valid_abstract_section_header('MOBILE OBSTETRIC SIMULATION AND TEAM TRAINING PROGRAM',		1).
valid_abstract_section_header('MOBILITY',		1).
valid_abstract_section_header('MOBILIZING FOR HFE',		1).
valid_abstract_section_header('MODE OF ACTION OF PAF IN ACUTE PANCREATITIS',		1).
valid_abstract_section_header('MODEL & OUTCOMES',		1).
valid_abstract_section_header('MODEL AND CONCLUSION',		1).
valid_abstract_section_header('MODEL AND METHODS',		1).
valid_abstract_section_header('MODEL COMPONENTS',		1).
valid_abstract_section_header('MODEL FRAMEWORK',		1).
valid_abstract_section_header('MODEL METHODS',		1).
valid_abstract_section_header('MODEL OF CARE',		1).
valid_abstract_section_header('MODEL RESULTS',		1).
valid_abstract_section_header('MODEL SCOPE',		1).
valid_abstract_section_header('MODEL STRUCTURE AND ASSUMPTIONS',		1).
valid_abstract_section_header('MODEL, PERSPECTIVE, AND TIMELINE',		1).
valid_abstract_section_header('MODEL, PERSPECTIVE, TIMEFRAME',		1).
valid_abstract_section_header('MODELING AND CONCLUSION',		1).
valid_abstract_section_header('MODELING APPROACH',		1).
valid_abstract_section_header('MODELING METHODS',		1).
valid_abstract_section_header('MODELLING AND EXPERIMENTS',		1).
valid_abstract_section_header('MODELLING METHODS',		1).
valid_abstract_section_header('MODERATE RATE OF SUCCESS',		1).
valid_abstract_section_header('MODERN IMAGING TECHNOLOGIES',		1).
valid_abstract_section_header('MODERN INFECTIOUS DISEASES',		1).
valid_abstract_section_header('MODIFICATIONS OF THE DUKE CRITERIA',		1).
valid_abstract_section_header('MODULATION OF WOUND HEALING',		1).
valid_abstract_section_header('MODULE DESCRIPTION',		1).
valid_abstract_section_header('MODULE ORGANIZATION',		1).
valid_abstract_section_header('MOLECULAR BASE',		1).
valid_abstract_section_header('MOLECULAR CHARACTERIZATION OF IRF10 IN THREE FISH SPECIES',		1).
valid_abstract_section_header('MOLECULAR EVALUATION OF METASTATIC DISEASE',		1).
valid_abstract_section_header('MOLECULAR MECHANISM OF CARDIAC SUBSTRATE UPTAKE',		1).
valid_abstract_section_header('MOLECULAR MECHANISMS',		1).
valid_abstract_section_header('MOLECULAR MIMICRY',		1).
valid_abstract_section_header('MOLECULAR ONCOLOGY STUDIES',		1).
valid_abstract_section_header('MOLECULAR PATHOLOGY',		1).
valid_abstract_section_header('MOLECULAR STUDIES',		1).
valid_abstract_section_header('MOLECULAR TESTING',		1).
valid_abstract_section_header('MOLECULAR-BASED TREATMENT',		1).
valid_abstract_section_header('MONITOR',		1).
valid_abstract_section_header('MONITORING AND EDUCATIONAL PROGRAM',		1).
valid_abstract_section_header('MONITORING FOR CMV',		1).
valid_abstract_section_header('MONITORING OF BIAS',		1).
valid_abstract_section_header('MONITORING THE ADEQUACY OF TISSUE OXYGENATION',		1).
valid_abstract_section_header('MONITORING THE PROCESS',		1).
valid_abstract_section_header('MONOTHERAPY STUDIES',		1).
valid_abstract_section_header('MONOTHERAPY VERSUS COMBINED THERAPY',		1).
valid_abstract_section_header('MOOD DISORDERS COMMITTEE',		1).
valid_abstract_section_header('MORBID OBESITY EPIDEMIOLOGY',		1).
valid_abstract_section_header('MORBIDITY OF TREATMENT',		1).
valid_abstract_section_header('MORE',		1).
valid_abstract_section_header('MORE LESSONS FROM DEVELOPED COUNTRIES FOR IMCI',		1).
valid_abstract_section_header('MORPHOGENETIC PATHWAY',		1).
valid_abstract_section_header('MORPHOLOGIC FINDINGS',		1).
valid_abstract_section_header('MORPHOTYPICAL AND PHYSIOTYPICAL PLASTICITY OF CAM',		1).
valid_abstract_section_header('MORRIS WATER MAZE',		1).
valid_abstract_section_header('MORTALITY RATES',		1).
valid_abstract_section_header('MORTALITY RESULTS',		1).
valid_abstract_section_header('MOST IMPORTANT DISCOVERIES AND SIGNIFICANCE',		1).
valid_abstract_section_header('MOST IMPORTANT METHODS',		1).
valid_abstract_section_header('MOTHERHOOD HOME',		1).
valid_abstract_section_header('MOTIF DE LA REVUE',		1).
valid_abstract_section_header('MOTIVATION AND BACKGROUND',		1).
valid_abstract_section_header('MOTIVATION/BACKGROUND',		1).
valid_abstract_section_header('MOTOR CONTROL',		1).
valid_abstract_section_header('MOVEMENT DISORDERS',		1).
valid_abstract_section_header('MOVEMENTS STUDIED',		1).
valid_abstract_section_header('MOVING AHEAD',		1).
valid_abstract_section_header('MOVING FROM HYPOTHESIS TO CONCLUSION',		1).
valid_abstract_section_header('MOVING OUTSIDE THE WALLS TO IMPROVE QUALITY',		1).
valid_abstract_section_header('MP, OS',		1).
valid_abstract_section_header('MPR-C-DIAS',		1).
valid_abstract_section_header('MR AND CT DOCUMENTATION OF OPLL',		1).
valid_abstract_section_header('MR-SUV VOI',		1).
valid_abstract_section_header('MREC REG NO',		1).
valid_abstract_section_header('MRS VERSIONS AVAILABLE',		1).
valid_abstract_section_header('MSC CLASSIFICATION',		1).
valid_abstract_section_header('MSF EXPERIENCE',		1).
valid_abstract_section_header('MTA CONCLUSIONS',		1).
valid_abstract_section_header('MTD/RPTD',		1).
valid_abstract_section_header('MTEHODS',		1).
valid_abstract_section_header('MTJ-CON',		1).
valid_abstract_section_header('MULTICENTER STUDY',		1).
valid_abstract_section_header('MULTIMODALITY APPROACHES',		1).
valid_abstract_section_header('MULTIPLE CLINICAL FORMS',		1).
valid_abstract_section_header('MULTIPLE DRUG THERAPY',		1).
valid_abstract_section_header('MULTIPLE INJURIES',		1).
valid_abstract_section_header('MULTIPLE PATHWAYS FOR ADENOSINE FORMATION',		1).
valid_abstract_section_header('MULTIPLE SCLEROSIS',		1).
valid_abstract_section_header('MULTIPLE SCLEROSIS (MS)',		1).
valid_abstract_section_header('MULTIPLICATION AND DIVISION ARE CONCEPTUALLY INVERSELY RELATED',		1).
valid_abstract_section_header('MULTIPOTENT SOMATIC STEM CELLS',		1).
valid_abstract_section_header('MUSCLE AND WEIGHT LOSS',		1).
valid_abstract_section_header('MUSCLE RELAXANTS',		1).
valid_abstract_section_header('MUSICAL VOICE RANGE',		1).
valid_abstract_section_header('MYCOTOXICOLOGY',		1).
valid_abstract_section_header('MYOCARDIAL HIBERNATION',		1).
valid_abstract_section_header('Main Exposures and Outcomes',		1).
valid_abstract_section_header('Main Outcomes and Measure',		1).
valid_abstract_section_header('Main Outcomes and Measurements',		1).
valid_abstract_section_header('Main findings:',		1).
valid_abstract_section_header('Material and Method',		1).
valid_abstract_section_header('Material and Methods:',		1).
valid_abstract_section_header('Material and method',		1).
valid_abstract_section_header('Materials and Method',		1).
valid_abstract_section_header('Materials and Methods:',		1).
valid_abstract_section_header('Measures',		1).
valid_abstract_section_header('Methodological aspects:',		1).
valid_abstract_section_header('Methodology:',		1).
valid_abstract_section_header('Methods.',		1).
valid_abstract_section_header('NAME OF THE SYNDROME',		1).
valid_abstract_section_header('NAMEN RAZISKAVE',		1).
valid_abstract_section_header('NANOTECHNOLOGY (NANO',		1).
valid_abstract_section_header('NASAL BONE',		1).
valid_abstract_section_header('NASOGENITAL REFLEX THEORY',		1).
valid_abstract_section_header('NATIONAL HEART, LUNG, AND BLOOD INSTITUTE ACTION',		1).
valid_abstract_section_header('NATIONAL IMPLEMENTATION OF THE PRIMARY CARE RESEARCH TEAM ASSESSMENT',		1).
valid_abstract_section_header('NATIONAL LEVEL',		1).
valid_abstract_section_header('NATIONAL ORGANIZATIONS ENGAGED IN IMPLEMENTING QI ACTIVITIES',		1).
valid_abstract_section_header('NATIONAL POLICY AND HEALTH SERVICE MODEL',		1).
valid_abstract_section_header('NATIONAL TRENDS',		1).
valid_abstract_section_header('NATRIURETIC PEPTIDE IN ACCESSEMENT GRADIENT OF HEART FAILURE',		1).
valid_abstract_section_header('NATURAL AND SPECIFIC IMMUNE RESPONSE TO PROTOZOA',		1).
valid_abstract_section_header('NATURAL COURSE',		1).
valid_abstract_section_header('NATURAL COURSE AND PROGNOSIS',		1).
valid_abstract_section_header('NATURE OF EBM',		1).
valid_abstract_section_header('NATURE OF PROBLEM',		1).
valid_abstract_section_header('NCT NO',		1).
valid_abstract_section_header('NE CONCLUSION',		1).
valid_abstract_section_header('NEAR TOTAL LARYNGECTOMY',		1).
valid_abstract_section_header('NEAT CLINICALTRIALSGOV',		1).
valid_abstract_section_header('NECESSARY PREVENTION',		1).
valid_abstract_section_header('NECROPSY FINDINGS',		1).
valid_abstract_section_header('NEDERLANDS TRIAL REGISTER (NTR)',		1).
valid_abstract_section_header('NEED FOR BROAD APPROACH',		1).
valid_abstract_section_header('NEED FOR FURTHER TRIALS',		1).
valid_abstract_section_header('NEED FOR GENETIC MARKERS',		1).
valid_abstract_section_header('NEED FOR REVIEW',		1).
valid_abstract_section_header('NEED FOR THE STUDY',		1).
valid_abstract_section_header('NEED TO ASSESS THE SKILL OF ECOSYSTEM MODELS',		1).
valid_abstract_section_header('NEEDS ASSESSMENT',		1).
valid_abstract_section_header('NEGATIVE CONTROL GROUP',		1).
valid_abstract_section_header('NEGATIVE RECOMMENDATIONS',		1).
valid_abstract_section_header('NEONATAL SCREENING',		1).
valid_abstract_section_header('NEONATAL TORSION',		1).
valid_abstract_section_header('NEPHROPATHY',		1).
valid_abstract_section_header('NET ECONOMIC BENEFITS FROM EACH CLOSED SITE',		1).
valid_abstract_section_header('NETWORK CLUSTERING ALGORITHMS',		1).
valid_abstract_section_header('NEUROLOGICAL LESIONS',		1).
valid_abstract_section_header('NEUROMONITORING',		1).
valid_abstract_section_header('NEURONAL MORPHOLOGIES ARE PIVOTAL FOR BRAIN FUNCTIONING',		1).
valid_abstract_section_header('NEURONAL REGENERATION',		1).
valid_abstract_section_header('NEUROPATHY',		1).
valid_abstract_section_header('NEUROPHYSIOLOGICAL CLASSIFICATION SYSTEMS',		1).
valid_abstract_section_header('NEUROPHYSIOLOGICAL DEFICITS',		1).
valid_abstract_section_header('NEUROPHYSIOLOGY RESULTS',		1).
valid_abstract_section_header('NEUROPROTECTIVE STRATEGIES',		1).
valid_abstract_section_header('NEUROPSYCHOLOGICAL DATA',		1).
valid_abstract_section_header('NEUROPSYCHOLOGICAL TESTING',		1).
valid_abstract_section_header('NEUROPSYCHOLOGY',		1).
valid_abstract_section_header('NEUROSURGICAL PROCEDURES',		1).
valid_abstract_section_header('NEUROSURGICAL TECHNIQUES',		1).
valid_abstract_section_header('NEW ADVANCES',		1).
valid_abstract_section_header('NEW ANALYSIS',		1).
valid_abstract_section_header('NEW AND SOPHISTICATED INVESTIGATIONS METHODS ARE AVAILABLE',		1).
valid_abstract_section_header('NEW AND UNIQUE INFORMATION',		1).
valid_abstract_section_header('NEW AND UNIQUE INFORMATION PROVIDED',		1).
valid_abstract_section_header('NEW APPROACHES TO CONVERTING TO ALL NONLATEX STERILE GLOVES',		1).
valid_abstract_section_header('NEW ASPECTS',		1).
valid_abstract_section_header('NEW AUC DOSING CHART',		1).
valid_abstract_section_header('NEW CLASSIFICATION',		1).
valid_abstract_section_header('NEW CLINICAL STUDIES',		1).
valid_abstract_section_header('NEW COMBINATIONS ARE',		1).
valid_abstract_section_header('NEW CORONARY ARTERY REVASCULARIZATION STRATEGIES ARE DEVELOPING',		1).
valid_abstract_section_header('NEW DATA APPARENTLY CHALLENGE THIS NOTION',		1).
valid_abstract_section_header('NEW DISCIPLINARY EMPHASES IN TRAINING DOCTORS',		1).
valid_abstract_section_header('NEW DRUGS AND TARGETS',		1).
valid_abstract_section_header('NEW EPIDEMIOLOGICAL STUDIES',		1).
valid_abstract_section_header('NEW HYPOTHESIS',		1).
valid_abstract_section_header('NEW INSIGHTS',		1).
valid_abstract_section_header('NEW METHOD/COMPARISON WITH EXISTING METHODS',		1).
valid_abstract_section_header('NEW OPPORTUNITIES',		1).
valid_abstract_section_header('NEW OR UNIQUE INFORMATION',		1).
valid_abstract_section_header('NEW ORAL HYPOGLYCEMIC AGENTS',		1).
valid_abstract_section_header('NEW PLAYERS IN OSTEOIMMUNOLOGY',		1).
valid_abstract_section_header('NEW POTENTIAL ANTIHYPERTENSIVE DRUGS',		1).
valid_abstract_section_header('NEW PROFESSIONALISM',		1).
valid_abstract_section_header('NEW RECORDS OF PONTARACHNID MITES (ACARI',		1).
valid_abstract_section_header('NEW SCHOOLS',		1).
valid_abstract_section_header('NEW SCORING SYSTEMS',		1).
valid_abstract_section_header('NEW SERVICES',		1).
valid_abstract_section_header('NEW SYSTEM',		1).
valid_abstract_section_header('NEW TEAMS',		1).
valid_abstract_section_header('NEW TECHNOLOGIES',		1).
valid_abstract_section_header('NEW THERAPEUTIC MODALITIES',		1).
valid_abstract_section_header('NEW TOOL',		1).
valid_abstract_section_header('NEW TOOLS FOR ASSESSMENT OF IMMUNE RECONSTITUTION AFTER HSCT',		1).
valid_abstract_section_header('NEWBORN SCREENING',		1).
valid_abstract_section_header('NEWS AND NOTEWORTHY',		1).
valid_abstract_section_header('NIGERIA',		1).
valid_abstract_section_header('NIH, ID',		1).
valid_abstract_section_header('NIHR PORTFOLIO NUMBER',		1).
valid_abstract_section_header('NIHSS AND T-PA THERAPY',		1).
valid_abstract_section_header('NL NTR NUMBER',		1).
valid_abstract_section_header('NLM IDENTIFIER',		1).
valid_abstract_section_header('NO LEVEL EVIDENCE',		1).
valid_abstract_section_header('NO SYSTEMIC INVOLVEMENT',		1).
valid_abstract_section_header('NON-DIRECTIVE CONTROL',		1).
valid_abstract_section_header('NON-OPIOIDS',		1).
valid_abstract_section_header('NON-RANDOMISED INTERVENTION',		1).
valid_abstract_section_header('NON-SURGICAL METHODS OF FERTILITY CONTROL',		1).
valid_abstract_section_header('NONCLINICAL RESULTS',		1).
valid_abstract_section_header('NONCONFORMITIES',		1).
valid_abstract_section_header('NONCONSERVATIVENESS OF PI IN APPLICATION',		1).
valid_abstract_section_header('NONINVASIVE DIAGNOSIS OF CAD',		1).
valid_abstract_section_header('NONPHARMACOLOGICAL TECHNIQUES',		1).
valid_abstract_section_header('NORMAL FUNCTION',		1).
valid_abstract_section_header('NORMAL IMMUNE RECONSTITUTION FOLLOWING ALLOGENEIC HEMATOPOIETIC STEM CELL TRANSPLANTATION (HSCT)',		1).
valid_abstract_section_header('NORMAL INSULIN SECRETION',		1).
valid_abstract_section_header('NORMAL PULMONARY VENOUS FLOW PATTERN',		1).
valid_abstract_section_header('NORMAL SUBJECTS',		1).
valid_abstract_section_header('NORMAL TISSUE OUTCOMES',		1).
valid_abstract_section_header('NORMAL TRANSMITRAL FLOW PATTERN',		1).
valid_abstract_section_header('NORMAL TYPE AND BORDERLINE',		1).
valid_abstract_section_header('NORMATIVE APPRAISAL',		1).
valid_abstract_section_header('NORTHEAST US ATLANTIS MARINE ECOSYSTEM MODEL',		1).
valid_abstract_section_header('NOTE ABOUT TERMINOLOGY',		1).
valid_abstract_section_header('NOTES FROM A MICROSYSTEM JOURNEY',		1).
valid_abstract_section_header('NOTICE OF CLARIFICATION',		1).
valid_abstract_section_header('NOVEL CONCEPT',		1).
valid_abstract_section_header('NOVEL DIAGNOSTIC APPROACH OF THE DISSECTED AORTA',		1).
valid_abstract_section_header('NOVEL DRUGS FOR HCV TREATMENT',		1).
valid_abstract_section_header('NOVEL FACTS',		1).
valid_abstract_section_header('NOVEL FINDINGS',		1).
valid_abstract_section_header('NOVEL INSIGHTS',		1).
valid_abstract_section_header('NOVEL KNOWLEDGE',		1).
valid_abstract_section_header('NOVELTY & IMPACT STATEMENTS',		1).
valid_abstract_section_header('NOVELTY ASPECTS',		1).
valid_abstract_section_header('NOVELTY OF THE FINDINGS',		1).
valid_abstract_section_header('NRT/B',		1).
valid_abstract_section_header('NSAID IN BACTERIAL RHINOSINUSITIS',		1).
valid_abstract_section_header('NSAIDH (OR',		1).
valid_abstract_section_header('NSIP CONCLUSIONS',		1).
valid_abstract_section_header('NUMBER AND QUALITY OF STUDIES AND DIRECTION OF EVIDENCE',		1).
valid_abstract_section_header('NUMBER OF INCLUDED STUDIES',		1).
valid_abstract_section_header('NUMBER OF STUDIES INCLUDED',		1).
valid_abstract_section_header('NUMBER OF STUDY CENTERS',		1).
valid_abstract_section_header('NURSING STUDENTS\' EVALUATION',		1).
valid_abstract_section_header('NUTRIENT REFERENCE VALUES',		1).
valid_abstract_section_header('NUTRITIONAL ASSISTANCE',		1).
valid_abstract_section_header('NUTRITIONAL FACTORS',		1).
valid_abstract_section_header('NUTRITIONAL SUPPORT AND EXERCISE',		1).
valid_abstract_section_header('OBBJECTIF',		1).
valid_abstract_section_header('OBECTIVES',		1).
valid_abstract_section_header('OBEJCTIVE',		1).
valid_abstract_section_header('OBJCTIVE',		1).
valid_abstract_section_header('OBJECITVES',		1).
valid_abstract_section_header('OBJECIVE',		1).
valid_abstract_section_header('OBJECT AND BACKGROUND',		1).
valid_abstract_section_header('OBJECT AND RESULTS',		1).
valid_abstract_section_header('OBJECT OF THE PAPER',		1).
valid_abstract_section_header('OBJECT OF WORK',		1).
valid_abstract_section_header('OBJECTICS',		1).
valid_abstract_section_header('OBJECTIFS DU SONDAGE',		1).
valid_abstract_section_header('OBJECTISVES',		1).
valid_abstract_section_header('OBJECTIVE',		1).
valid_abstract_section_header('OBJECTIVE & AIMS',		1).
valid_abstract_section_header('OBJECTIVE & HYPOTHESIS',		1).
valid_abstract_section_header('OBJECTIVE ACOUSTIC ANALYSIS',		1).
valid_abstract_section_header('OBJECTIVE AIM',		1).
valid_abstract_section_header('OBJECTIVE AND CASE REPORT',		1).
valid_abstract_section_header('OBJECTIVE AND DATA',		1).
valid_abstract_section_header('OBJECTIVE AND EVIDENCE ACQUISITION',		1).
valid_abstract_section_header('OBJECTIVE AND HYPOTHESES',		1).
valid_abstract_section_header('OBJECTIVE AND INTRODUCTION',		1).
valid_abstract_section_header('OBJECTIVE AND MATERIAL AND METHODS',		1).
valid_abstract_section_header('OBJECTIVE AND PRINCIPLES',		1).
valid_abstract_section_header('OBJECTIVE AND PROCEDURE',		1).
valid_abstract_section_header('OBJECTIVE AND PROGRAM OVERVIEW',		1).
valid_abstract_section_header('OBJECTIVE AND RATIONAL',		1).
valid_abstract_section_header('OBJECTIVE AND RESULT',		1).
valid_abstract_section_header('OBJECTIVE AND SUMMARY OF BACKGROUND DATA',		1).
valid_abstract_section_header('OBJECTIVE MEASURES',		1).
valid_abstract_section_header('OBJECTIVE METHODS',		1).
valid_abstract_section_header('OBJECTIVE OF CONFERENCE PARTICIPANTS',		1).
valid_abstract_section_header('OBJECTIVE OF DRUG SCREENING',		1).
valid_abstract_section_header('OBJECTIVE OF NARRATIVE REVIEW',		1).
valid_abstract_section_header('OBJECTIVE OF SURGERY',		1).
valid_abstract_section_header('OBJECTIVE OF THE WORK',		1).
valid_abstract_section_header('OBJECTIVE OF THIS REVIEW',		1).
valid_abstract_section_header('OBJECTIVE SCORES',		1).
valid_abstract_section_header('OBJECTIVE, DESIGN',		1).
valid_abstract_section_header('OBJECTIVE, DESIGN AND PATIENTS',		1).
valid_abstract_section_header('OBJECTIVE, DESIGN, AND PARTICIPANTS',		1).
valid_abstract_section_header('OBJECTIVE, DESIGN, SETTING, AND PATIENT',		1).
valid_abstract_section_header('OBJECTIVE, DESIGN, SETTING, PATIENTS, INTERVENTIONS, AND MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('OBJECTIVE, MATERIALS AND METHODS',		1).
valid_abstract_section_header('OBJECTIVE, METHODS, RESULTS AND CONCLUSIONS',		1).
valid_abstract_section_header('OBJECTIVE-',		1).
valid_abstract_section_header('OBJECTIVE--',		1).
valid_abstract_section_header('OBJECTIVE/BACKGROUND/AIM',		1).
valid_abstract_section_header('OBJECTIVE/CONTENT',		1).
valid_abstract_section_header('OBJECTIVE/DESIGN/SETTING',		1).
valid_abstract_section_header('OBJECTIVE/MATERIALS AND METHODS',		1).
valid_abstract_section_header('OBJECTIVE/PROPOSAL',		1).
valid_abstract_section_header('OBJECTIVE/RATIONALE',		1).
valid_abstract_section_header('OBJECTIVE/REVIEW QUESTIONS',		1).
valid_abstract_section_header('OBJECTIVE/S',		1).
valid_abstract_section_header('OBJECTIVE/SETTING/DESIGN',		1).
valid_abstract_section_header('OBJECTIVE/STUDY DESIGN',		1).
valid_abstract_section_header('OBJECTIVES &AIMS',		1).
valid_abstract_section_header('OBJECTIVES (I)',		1).
valid_abstract_section_header('OBJECTIVES AND CONCLUSION',		1).
valid_abstract_section_header('OBJECTIVES AND DESIGN STUDY',		1).
valid_abstract_section_header('OBJECTIVES AND DIFFICULTIES',		1).
valid_abstract_section_header('OBJECTIVES AND ETHICAL CONSIDERATIONS',		1).
valid_abstract_section_header('OBJECTIVES AND FINDINGS',		1).
valid_abstract_section_header('OBJECTIVES AND HIGHLIGHTS',		1).
valid_abstract_section_header('OBJECTIVES AND METHODOLOGY',		1).
valid_abstract_section_header('OBJECTIVES AND MOTIVATION',		1).
valid_abstract_section_header('OBJECTIVES AND OBSERVATIONS',		1).
valid_abstract_section_header('OBJECTIVES AND PARTICIPANTS',		1).
valid_abstract_section_header('OBJECTIVES AND PROGRESS',		1).
valid_abstract_section_header('OBJECTIVES AND PROJECT DESIGN',		1).
valid_abstract_section_header('OBJECTIVES AND SEARCH STRATEGY',		1).
valid_abstract_section_header('OBJECTIVES AND WORKING HYPOTHESIS',		1).
valid_abstract_section_header('OBJECTIVES DESIGN',		1).
valid_abstract_section_header('OBJECTIVES OF RESEARCH',		1).
valid_abstract_section_header('OBJECTIVES OF THE PROJECT',		1).
valid_abstract_section_header('OBJECTIVES OF THE STUDY AND BACKGROUND',		1).
valid_abstract_section_header('OBJECTIVES STUDY DESIGN',		1).
valid_abstract_section_header('OBJECTIVES, ACTIVITIES AND CHALLENGES',		1).
valid_abstract_section_header('OBJECTIVES, DESIGN AND SETTING',		1).
valid_abstract_section_header('OBJECTIVES, MATERIAL AND METHODS',		1).
valid_abstract_section_header('OBJECTIVES, MATERIALS AND METHODS',		1).
valid_abstract_section_header('OBJECTIVES, METHODS',		1).
valid_abstract_section_header('OBJECTIVES, METHODS AND RESULTS',		1).
valid_abstract_section_header('OBJECTIVES, METHODS, AND RESULTS',		1).
valid_abstract_section_header('OBJECTIVES, METHODS, RESULTS',		1).
valid_abstract_section_header('OBJECTIVES, PATIENTS, AND METHODS',		1).
valid_abstract_section_header('OBJECTIVES, SETTING AND DESIGN',		1).
valid_abstract_section_header('OBJECTIVES/GOALS',		1).
valid_abstract_section_header('OBJECTIVES/HYPOTHOSIS',		1).
valid_abstract_section_header('OBJECTIVES/METHOD/DESIGN',		1).
valid_abstract_section_header('OBJECTIVES/METHODOLOGY',		1).
valid_abstract_section_header('OBJECTIVES/PATIENTS',		1).
valid_abstract_section_header('OBJECTIVESS',		1).
valid_abstract_section_header('OBJECTIVEW',		1).
valid_abstract_section_header('OBJECTIVOS',		1).
valid_abstract_section_header('OBJECTIVS',		1).
valid_abstract_section_header('OBJECTIVSE',		1).
valid_abstract_section_header('OBJECTRIVE',		1).
valid_abstract_section_header('OBJECTS AND DESIGN',		1).
valid_abstract_section_header('OBJECTS ARE REMINISCENT OF ACTIONS OFTEN PERFORMED WITH THEM',		1).
valid_abstract_section_header('OBJET',		1).
valid_abstract_section_header('OBJET ET PUBLICS CIBLES',		1).
valid_abstract_section_header('OBJETICVE',		1).
valid_abstract_section_header('OBJETIVES AND METHODS',		1).
valid_abstract_section_header('OBJETIVO',		1).
valid_abstract_section_header('OBJETIVO DEL ESTUDIO',		1).
valid_abstract_section_header('OBJETIVOS Y METODOS',		1).
valid_abstract_section_header('OBSERVATION & DISCUSSION',		1).
valid_abstract_section_header('OBSERVATION & RESULT',		1).
valid_abstract_section_header('OBSERVATION AND COMMENTARY',		1).
valid_abstract_section_header('OBSERVATION AND RESULT',		1).
valid_abstract_section_header('OBSERVATION(S)',		1).
valid_abstract_section_header('OBSERVATIONAL PRACTICE',		1).
valid_abstract_section_header('OBSERVATIONAL STUDIES',		1).
valid_abstract_section_header('OBSERVATIONS AND ADVANCES',		1).
valid_abstract_section_header('OBSERVATIONS AND CONCLUSION',		1).
valid_abstract_section_header('OBSERVATIONS AND HYPOTHESES',		1).
valid_abstract_section_header('OBSERVATIONS/INVESTIGATIONS',		1).
valid_abstract_section_header('OBSERVATIONS/METHOD',		1).
valid_abstract_section_header('OBSERVATIONS/RESULTS',		1).
valid_abstract_section_header('OBSERVED BENEFITS OF PHYSICIANS IN MEDEVAC',		1).
valid_abstract_section_header('OBSTACLES TO HUMILITY AND RESPECT',		1).
valid_abstract_section_header('OBSTETRIC HISTORY',		1).
valid_abstract_section_header('OBSTETRICAL CONSEQUENCES',		1).
valid_abstract_section_header('OBTAINING PHYSICIAN PARTICIPATION IN RISK MANAGEMENT',		1).
valid_abstract_section_header('OCCLUSION',		1).
valid_abstract_section_header('OCCULT HEPATITIS C',		1).
valid_abstract_section_header('OCR, AER, CER, O',		1).
valid_abstract_section_header('OF CASES',		1).
valid_abstract_section_header('OFFICIAL INDICATIONS',		1).
valid_abstract_section_header('OJBECTIVE',		1).
valid_abstract_section_header('OJECTIVES',		1).
valid_abstract_section_header('OLIGOMETASTASIS AND OLIGOPROGRESSION',		1).
valid_abstract_section_header('OLZ-RIS) RESULTS',		1).
valid_abstract_section_header('ON EST ENSEMBLE',		1).
valid_abstract_section_header('ONE SENTENCE SUMMARY',		1).
valid_abstract_section_header('ONE-SENTENCE SUMMARY',		1).
valid_abstract_section_header('ONGOING EVALUATION',		1).
valid_abstract_section_header('ONGOING IMPROVEMENT PROJECTS',		1).
valid_abstract_section_header('ONGOING STUDIES',		1).
valid_abstract_section_header('ONGOING TRIALS',		1).
valid_abstract_section_header('ONGOING WORK AND RESEARCH AGENDA',		1).
valid_abstract_section_header('ONLINE REGISTRATION',		1).
valid_abstract_section_header('ONSCREEN SMOKING IS A FORM OF TOBACCO MARKETING',		1).
valid_abstract_section_header('ONSET OF LIGHT THERAPY EFFECT',		1).
valid_abstract_section_header('ONTARIO HEALTH SYSTEM IMPACT ANALYSIS',		1).
valid_abstract_section_header('OPEN FIELD TEST',		1).
valid_abstract_section_header('OPERATING CHARACTERISTICS',		1).
valid_abstract_section_header('OPERATION',		1).
valid_abstract_section_header('OPERATION WITH SNARES AND CUTTING INSTRUMENTS',		1).
valid_abstract_section_header('OPERATIONAL DEFINITION OF MEASURES',		1).
valid_abstract_section_header('OPERATIONAL DEFINITIONS',		1).
valid_abstract_section_header('OPERATIONSTECHNIK',		1).
valid_abstract_section_header('OPERATIVE ANTIREFLUX THERAPY',		1).
valid_abstract_section_header('OPERATIVE APPROACH',		1).
valid_abstract_section_header('OPERATIVE APPROACHES',		1).
valid_abstract_section_header('OPERATIVE PRINCIPLE',		1).
valid_abstract_section_header('OPERATIVE THERAPY',		1).
valid_abstract_section_header('OPERATIVE THERAPY CONCEPTS',		1).
valid_abstract_section_header('OPHTHALMOLOGICAL RISKS',		1).
valid_abstract_section_header('OPHTHALMOSCOPY',		1).
valid_abstract_section_header('OPINION AND ATTITUDE',		1).
valid_abstract_section_header('OPINION/FEEDBACK/INTRODUCTION',		1).
valid_abstract_section_header('OPINION/FEEDBACK/METHOD',		1).
valid_abstract_section_header('OPIS BADANIA',		1).
valid_abstract_section_header('OPPORTUNITIES FOR INVOLVEMENT AND IDENTIFICATION OF CONSUMERS',		1).
valid_abstract_section_header('OPPORTUNITY',		1).
valid_abstract_section_header('OPPORTUNITY FOR CHANGE',		1).
valid_abstract_section_header('OPPORTUNITY FOR IMPROVEMENT',		1).
valid_abstract_section_header('OPTICAL RADIATION',		1).
valid_abstract_section_header('OPTIMIZING SECURITY',		1).
valid_abstract_section_header('OPTIMUM COMPLEXITY AND DYNAMIC MODELING',		1).
valid_abstract_section_header('OPTIONS AND OUTCOMES CONSIDERED',		1).
valid_abstract_section_header('OPTIONS FOR PARTNER INVOLVEMENT IN ED THERAPY',		1).
valid_abstract_section_header('ORAL CONTRACEPTIVES',		1).
valid_abstract_section_header('ORAL HYGIENE',		1).
valid_abstract_section_header('ORAL PRESENTATION',		1).
valid_abstract_section_header('ORDINAL-TO-INTERVAL SCALE CONVERSION EXAMPLE',		1).
valid_abstract_section_header('ORG IDENTIFIER',		1).
valid_abstract_section_header('ORGAN WEIGHTS AND PATHOLOGY FINDINGS',		1).
valid_abstract_section_header('ORGANISMS',		1).
valid_abstract_section_header('ORGANIZATION AND METHODS OF THE WORKING GROUPS',		1).
valid_abstract_section_header('ORGANIZATION AWARD WINNERS',		1).
valid_abstract_section_header('ORGANIZATION LEVEL',		1).
valid_abstract_section_header('ORGANIZATION OF MEDICAL CARE',		1).
valid_abstract_section_header('ORGANIZATION OF THE APS',		1).
valid_abstract_section_header('ORGANIZATION OF THE GUIDE',		1).
valid_abstract_section_header('ORGANIZATION OF THE RESCUE OF THE PATIENT',		1).
valid_abstract_section_header('ORGANIZATIONAL CONSTRUCT',		1).
valid_abstract_section_header('ORGANIZATIONAL CONTEXT',		1).
valid_abstract_section_header('ORIGINAL EXPERIENCE',		1).
valid_abstract_section_header('ORIGINAL POSITION',		1).
valid_abstract_section_header('ORIGINAL STATEMENT',		1).
valid_abstract_section_header('ORIGINALITY AND VALUE',		1).
valid_abstract_section_header('ORIGINALITY/ VALUE',		1).
valid_abstract_section_header('ORIGINALITY/INNOVATIONS',		1).
valid_abstract_section_header('ORIGINALITY/VALUE OF PAPER',		1).
valid_abstract_section_header('ORNITHODOROS BRASILIENSIS',		1).
valid_abstract_section_header('OROPHARYNGEAL DYSPHAGIA IN NEURODEGENERATIVE DISEASES',		1).
valid_abstract_section_header('OROPHARYNGEAL SWALLOWING DISORDERS IN PARKINSON\'S DISEASE',		1).
valid_abstract_section_header('ORTHOGNATHIC SURGERY IS A UNIQUE ENDEAVOR IN FACIAL SURGERY',		1).
valid_abstract_section_header('ORTHOPAEDIC RELEVANCE',		1).
valid_abstract_section_header('ORTHOTOPIC URETEROCELE',		1).
valid_abstract_section_header('OSA AND DELIRIUM',		1).
valid_abstract_section_header('OSTEOPROTEGERIN',		1).
valid_abstract_section_header('OSTEOPROTEGERIN AS A THERAPEUTIC AGENT',		1).
valid_abstract_section_header('OTALGIA',		1).
valid_abstract_section_header('OTHER ACTIONS OF ACE INHIBITORS',		1).
valid_abstract_section_header('OTHER ADVANTAGES OF LINEZOLID',		1).
valid_abstract_section_header('OTHER ASPECTS',		1).
valid_abstract_section_header('OTHER COMMENTS',		1).
valid_abstract_section_header('OTHER COMPLICATIONS',		1).
valid_abstract_section_header('OTHER CONTRIBUTIONS',		1).
valid_abstract_section_header('OTHER DERMATOLOGICAL DISORDERS',		1).
valid_abstract_section_header('OTHER INDICATIONS',		1).
valid_abstract_section_header('OTHER INITIATIVES',		1).
valid_abstract_section_header('OTHER MEASURES',		1).
valid_abstract_section_header('OTHER NEUROPATHIES',		1).
valid_abstract_section_header('OTHER NEW DEVICES',		1).
valid_abstract_section_header('OTHER OUTCOME MEASURES',		1).
valid_abstract_section_header('OTHER POSSIBLE EFFECTS',		1).
valid_abstract_section_header('OTHER RISK FACTORS AGGRAVATE THIS CONDITION',		1).
valid_abstract_section_header('OTHER STRATEGIES',		1).
valid_abstract_section_header('OTHER THERAPIES',		1).
valid_abstract_section_header('OTHER VESSELS',		1).
valid_abstract_section_header('OTOACOUSTIC EMISSION',		1).
valid_abstract_section_header('OUR AIMS ARE TO IDENTIFY',		1).
valid_abstract_section_header('OUR EXPERIENCES',		1).
valid_abstract_section_header('OUR FINDINGS',		1).
valid_abstract_section_header('OUR INVESTIGATIONS',		1).
valid_abstract_section_header('OUR INVESTIGATIONS AND DATA',		1).
valid_abstract_section_header('OUR MODIFICATION',		1).
valid_abstract_section_header('OUR OBSERVATIONS WERE',		1).
valid_abstract_section_header('OUR RESULTS SUPPORT THE HYPOTHESES',		1).
valid_abstract_section_header('OUTBREAK CONTROL',		1).
valid_abstract_section_header('OUTBREAK PERIOD',		1).
valid_abstract_section_header('OUTCOME ANALYSIS',		1).
valid_abstract_section_header('OUTCOME AND CONCLUSION',		1).
valid_abstract_section_header('OUTCOME AND CONCLUSIONS',		1).
valid_abstract_section_header('OUTCOME AND FINDINGS',		1).
valid_abstract_section_header('OUTCOME AND SURVIVAL',		1).
valid_abstract_section_header('OUTCOME IN ADULTHOOD',		1).
valid_abstract_section_header('OUTCOME MAPPING',		1).
valid_abstract_section_header('OUTCOME MEASUREMENT AND STATISTICAL ANALYSES',		1).
valid_abstract_section_header('OUTCOME MEASUREMENTS AND ANALYSIS',		1).
valid_abstract_section_header('OUTCOME MEASUREMENTS AND STATISTIC ANALYSES',		1).
valid_abstract_section_header('OUTCOME MEASUREMENTS AND STATISTICS',		1).
valid_abstract_section_header('OUTCOME MEASURES & RESULTS',		1).
valid_abstract_section_header('OUTCOME MEASURES AND STATISTICAL ANALYSES',		1).
valid_abstract_section_header('OUTCOME MEASURES INCLUDED',		1).
valid_abstract_section_header('OUTCOME MEASURES, DATA EXTRACTION AND ANALYSIS',		1).
valid_abstract_section_header('OUTCOME OBJECTIVE',		1).
valid_abstract_section_header('OUTCOME, MEASUREMENTS AND STATISTICAL ANALYSIS',		1).
valid_abstract_section_header('OUTCOMES & CONCLUSIONS',		1).
valid_abstract_section_header('OUTCOMES & IMPORTANCE',		1).
valid_abstract_section_header('OUTCOMES & RESULTS AND CONCLUSIONS & IMPLICATIONS',		1).
valid_abstract_section_header('OUTCOMES AND IMPLICATIONS',		1).
valid_abstract_section_header('OUTCOMES AND MEASURES',		1).
valid_abstract_section_header('OUTCOMES ASSESSMENT IN SPECIFIC DISEASES',		1).
valid_abstract_section_header('OUTCOMES EXAMINED',		1).
valid_abstract_section_header('OUTCOMES IN ACCREDITATION',		1).
valid_abstract_section_header('OUTCOMES IN CLINICAL TRIALS',		1).
valid_abstract_section_header('OUTCOMES MEASUREMENT',		1).
valid_abstract_section_header('OUTCOMES MEASUREMENTS AND STATISTICAL ANALYSES',		1).
valid_abstract_section_header('OUTCOMES OF THE NATIONAL INVITATIONAL CONFERENCE',		1).
valid_abstract_section_header('OUTCOMES RESULTS',		1).
valid_abstract_section_header('OUTCOMES/CONCLUSION',		1).
valid_abstract_section_header('OUTCOMES/IMPLICATIONS',		1).
valid_abstract_section_header('OUTCOMES:',		1).
valid_abstract_section_header('OUTLINE OF THE DEVELOPMENTAL PROCESS',		1).
valid_abstract_section_header('OUTLINE OF THERAPY',		1).
valid_abstract_section_header('OUTLINES',		1).
valid_abstract_section_header('OUTPUT AND CONCLUSION',		1).
valid_abstract_section_header('OUTREACH TRAINING',		1).
valid_abstract_section_header('OVARIAN CANCER',		1).
valid_abstract_section_header('OVARIAN GCTS',		1).
valid_abstract_section_header('OVERALL',		1).
valid_abstract_section_header('OVERALL OBJECTIVE',		1).
valid_abstract_section_header('OVERALL STUDY RESULTS',		1).
valid_abstract_section_header('OVERCOMING PRACTITIONER OBJECTIONS',		1).
valid_abstract_section_header('OVERVIEW AND CONCLUSIONS',		1).
valid_abstract_section_header('OVERVIEW OF PHARMACODYNAMIC PROPERTIES',		1).
valid_abstract_section_header('OVERVIEW OF PHARMACOKINETIC PROPERTIES',		1).
valid_abstract_section_header('OVERVIEW OF POC INR DEVICES',		1).
valid_abstract_section_header('OVERVIEW OF THE BLADDER BUNDLE INITIATIVE IN MICHIGAN',		1).
valid_abstract_section_header('OVERVIEW OF THE EUROPEAN HEALTH SYSTEMS',		1).
valid_abstract_section_header('OWN EXPERIMENTS',		1).
valid_abstract_section_header('OXFORD LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('OXIDATION',		1).
valid_abstract_section_header('OXIDATION STRESS',		1).
valid_abstract_section_header('OXYGEN DEPRIVATION AND INTOXICATION',		1).
valid_abstract_section_header('Objectives:',		1).
valid_abstract_section_header('Objetive:',		1).
valid_abstract_section_header('Objetivo -',		1).
valid_abstract_section_header('Objetivos:',		1).
valid_abstract_section_header('Observation',		1).
valid_abstract_section_header('P T TENGBERG, N B FOSS, H PALM, T KALLEMOSE, A TROELSEN',		1).
valid_abstract_section_header('PACJENCI I METODY',		1).
valid_abstract_section_header('PACS NUMBER',		1).
valid_abstract_section_header('PACTR',		1).
valid_abstract_section_header('PAF ANTAGONISTS',		1).
valid_abstract_section_header('PAIN AND SCHIZOPHRENIA',		1).
valid_abstract_section_header('PAIN TREATMENT',		1).
valid_abstract_section_header('PAITENTS AND METHODS',		1).
valid_abstract_section_header('PALLIATIVE TREATMENT',		1).
valid_abstract_section_header('PALMAR HYPERHIDROSIS',		1).
valid_abstract_section_header('PANEL',		1).
valid_abstract_section_header('PANEL 1 RESEARCH IN CONTEXT SYSTEMATIC REVIEW',		1).
valid_abstract_section_header('PANEL I',		1).
valid_abstract_section_header('PANEL II',		1).
valid_abstract_section_header('PANEL III',		1).
valid_abstract_section_header('PAPER TYPE',		1).
valid_abstract_section_header('PAPERCLIP',		1).
valid_abstract_section_header('PAPERS IN THE SPECIAL ISSUE',		1).
valid_abstract_section_header('PARACETAMOL',		1).
valid_abstract_section_header('PARADIGM',		1).
valid_abstract_section_header('PARADIGM SHIFT TO PERSONALISED MEDICINE',		1).
valid_abstract_section_header('PARAMETERS MEASURES',		1).
valid_abstract_section_header('PARAMETERS OF INFLAMMATION',		1).
valid_abstract_section_header('PARASITE CYCLE',		1).
valid_abstract_section_header('PARASITES',		1).
valid_abstract_section_header('PARKINSONISM',		1).
valid_abstract_section_header('PAROXYSMAL ATRIAL FIBRILLATION',		1).
valid_abstract_section_header('PARTCIPANTS/MATERIALS, SETTING, METHOD',		1).
valid_abstract_section_header('PARTICIANTS',		1).
valid_abstract_section_header('PARTICIPANT AND RESEARCH CONTEXT',		1).
valid_abstract_section_header('PARTICIPANT AND SETTING',		1).
valid_abstract_section_header('PARTICIPANT SETTINGS',		1).
valid_abstract_section_header('PARTICIPANT\'S',		1).
valid_abstract_section_header('PARTICIPANT/SETTINGS',		1).
valid_abstract_section_header('PARTICIPANTS & INTERVENTIONS',		1).
valid_abstract_section_header('PARTICIPANTS /PATIENTS',		1).
valid_abstract_section_header('PARTICIPANTS AND ASSESSMENTS',		1).
valid_abstract_section_header('PARTICIPANTS AND CONTEXTS',		1).
valid_abstract_section_header('PARTICIPANTS AND DATA',		1).
valid_abstract_section_header('PARTICIPANTS AND DATASET USED',		1).
valid_abstract_section_header('PARTICIPANTS AND EXPOSURE',		1).
valid_abstract_section_header('PARTICIPANTS AND MAIN BIOLOGICAL MEASUREMENTS',		1).
valid_abstract_section_header('PARTICIPANTS AND OUTCOMES',		1).
valid_abstract_section_header('PARTICIPANTS AND PRESENTATION',		1).
valid_abstract_section_header('PARTICIPANTS AND PROCEDURE',		1).
valid_abstract_section_header('PARTICIPANTS AND RESULTS',		1).
valid_abstract_section_header('PARTICIPANTS AND STUDY CONTEXT',		1).
valid_abstract_section_header('PARTICIPANTS AND/OR CONTEXT',		1).
valid_abstract_section_header('PARTICIPANTS FINDINGS',		1).
valid_abstract_section_header('PARTICIPANTS MATERIALS, SETTING, METHOD',		1).
valid_abstract_section_header('PARTICIPANTS MATERIALS, SETTINGS, METHODS',		1).
valid_abstract_section_header('PARTICIPANTS MEASUREMENTS AND RESULTS',		1).
valid_abstract_section_header('PARTICIPANTS METHODS',		1).
valid_abstract_section_header('PARTICIPANTS, INTERVENTION',		1).
valid_abstract_section_header('PARTICIPANTS, INTERVENTION, AND MEASURES',		1).
valid_abstract_section_header('PARTICIPANTS, INTERVENTIONS AND MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('PARTICIPANTS, MATERIALS, SETTING AND METHODS',		1).
valid_abstract_section_header('PARTICIPANTS, MATERIALS, SETTINGS, METHODS',		1).
valid_abstract_section_header('PARTICIPANTS, MEASUREMENTS',		1).
valid_abstract_section_header('PARTICIPANTS, SETTING AND METHOD',		1).
valid_abstract_section_header('PARTICIPANTS/ DURATION/METHODS',		1).
valid_abstract_section_header('PARTICIPANTS/CONTEXT',		1).
valid_abstract_section_header('PARTICIPANTS/DESIGN',		1).
valid_abstract_section_header('PARTICIPANTS/ELIGIBILITY CRITERIA',		1).
valid_abstract_section_header('PARTICIPANTS/INFORMATION',		1).
valid_abstract_section_header('PARTICIPANTS/MATERIALS AND SETTING, METHODS',		1).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTING',		1).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTING AND METHOD',		1).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTINGS AND METHOD',		1).
valid_abstract_section_header('PARTICIPANTS/MATERIALS, SETTINGS, METHOD',		1).
valid_abstract_section_header('PARTICIPANTS/MATERIALS/SETTING, METHOD',		1).
valid_abstract_section_header('PARTICIPANTS/MATERIALS/SETTINGS/METHODS',		1).
valid_abstract_section_header('PARTICIPANTS/METHOD',		1).
valid_abstract_section_header('PARTICIPANTS/RESULTS',		1).
valid_abstract_section_header('PARTICIPANTS/SAMPLES',		1).
valid_abstract_section_header('PARTICIPANTS/SUBJECTS',		1).
valid_abstract_section_header('PARTICIPANTSS',		1).
valid_abstract_section_header('PARTICIPASNTS',		1).
valid_abstract_section_header('PARTICIPATION IN NRCPR',		1).
valid_abstract_section_header('PARTICIPITANTS',		1).
valid_abstract_section_header('PARTICLE ENTRY',		1).
valid_abstract_section_header('PARTICPANTS',		1).
valid_abstract_section_header('PARTICULAR CASES',		1).
valid_abstract_section_header('PARTNERING WITH PURCHASERS AND MEDICAID',		1).
valid_abstract_section_header('PARTNERSHIP DEVELOPMENT',		1).
valid_abstract_section_header('PASIREOTIDE',		1).
valid_abstract_section_header('PASSIVE COATING',		1).
valid_abstract_section_header('PAST AND CURRENT PERSPECTIVES',		1).
valid_abstract_section_header('PAST APPROACHES',		1).
valid_abstract_section_header('PAT -',		1).
valid_abstract_section_header('PATEINTS AND METHODS',		1).
valid_abstract_section_header('PATENT',		1).
valid_abstract_section_header('PATERNAL AGE',		1).
valid_abstract_section_header('PATHOANATOMICAL PRINCIPLES',		1).
valid_abstract_section_header('PATHOBIOCHEMICAL MECHANISMS',		1).
valid_abstract_section_header('PATHOGENESIS AND CLINICAL ASPECTS',		1).
valid_abstract_section_header('PATHOGENESIS AND INTERACTIONS',		1).
valid_abstract_section_header('PATHOGENESIS AND THERAPY OF INFERTILITY',		1).
valid_abstract_section_header('PATHOGENESIS OF DIABETIC OSTEOPATHY',		1).
valid_abstract_section_header('PATHOGENESIS OF ENDOMETRIOSIS',		1).
valid_abstract_section_header('PATHOGENESIS OF GLOMERULAR AND INTERSTITIAL RENAL FIBROSIS IN HUMANS',		1).
valid_abstract_section_header('PATHOGENESIS OF GROUP A STREPTOCOCCAL INFECTIONS',		1).
valid_abstract_section_header('PATHOGENESIS OF IMPAIRED VASCULAR GROWTH',		1).
valid_abstract_section_header('PATHOGENESIS OF TSE',		1).
valid_abstract_section_header('PATHOGENETIC ASSOCIATIONS',		1).
valid_abstract_section_header('PATHOGENIC FACTORS OF MALNUTRITION IN DIALYSIS PATIENTS',		1).
valid_abstract_section_header('PATHOGENIC POTENTIAL',		1).
valid_abstract_section_header('PATHOGENIC SPECIES',		1).
valid_abstract_section_header('PATHOGENICITY FEATURES',		1).
valid_abstract_section_header('PATHOLOGIC FEATURES',		1).
valid_abstract_section_header('PATHOLOGIC FILLING PATTERNS',		1).
valid_abstract_section_header('PATHOLOGICAL AND MOLECULAR FINDINGS',		1).
valid_abstract_section_header('PATHOLOGICAL DIAGNOSIS',		1).
valid_abstract_section_header('PATHOLOGICAL EXAMINATION',		1).
valid_abstract_section_header('PATHOLOGICAL SPECTRUM',		1).
valid_abstract_section_header('PATHOLOGY AND CLINICAL PRESENTATION',		1).
valid_abstract_section_header('PATHOLOGY AND MOLECULAR ABNORMALITIES',		1).
valid_abstract_section_header('PATHOLOGY AT RECURRENCE',		1).
valid_abstract_section_header('PATHOMORPHOLOGICAL AND CLINICAL STUDIES',		1).
valid_abstract_section_header('PATHOPHYSIOLOGIC MECHANISMS',		1).
valid_abstract_section_header('PATHOPHYSIOLOGICAL CONSIDERATIONS',		1).
valid_abstract_section_header('PATHOPHYSIOLOGICAL EVALUATION METHODS OF OCULAR SURFACE USING TEAR FLUID',		1).
valid_abstract_section_header('PATHOPHYSIOLOGICAL MECHANISM',		1).
valid_abstract_section_header('PATHOPHYSIOLOGICAL MECHANISMS',		1).
valid_abstract_section_header('PATHOPHYSIOLOGY AND DEFINITIONS',		1).
valid_abstract_section_header('PATHOPHYSIOLOGY OF LDL AND HDL METABOLISM',		1).
valid_abstract_section_header('PATHOPHYSIOLOGY OF NEUROPATHIC PAIN',		1).
valid_abstract_section_header('PATHWAYS COMMUNITY HUB MODEL AND FORMALIZATION',		1).
valid_abstract_section_header('PATICIPANTS',		1).
valid_abstract_section_header('PATIENS AND METHOD',		1).
valid_abstract_section_header('PATIENST AND METHODS',		1).
valid_abstract_section_header('PATIENT & METHOD',		1).
valid_abstract_section_header('PATIENT & RESULTS',		1).
valid_abstract_section_header('PATIENT AND COURSE OF THE DISEASE',		1).
valid_abstract_section_header('PATIENT AND FAMILY INVOLVEMENT',		1).
valid_abstract_section_header('PATIENT AND METHOD(S)',		1).
valid_abstract_section_header('PATIENT AND METHOD:',		1).
valid_abstract_section_header('PATIENT AND PROCEDURE',		1).
valid_abstract_section_header('PATIENT AND PUBLIC INVOLVEMENT IN THE RESEARCH',		1).
valid_abstract_section_header('PATIENT AND RESULT',		1).
valid_abstract_section_header('PATIENT CARE LEVEL',		1).
valid_abstract_section_header('PATIENT CASE HISTORY',		1).
valid_abstract_section_header('PATIENT COHORT',		1).
valid_abstract_section_header('PATIENT COMPLIANCE',		1).
valid_abstract_section_header('PATIENT CONCERNS AND DIAGNOSE',		1).
valid_abstract_section_header('PATIENT CONCERNS AND DIAGNOSES',		1).
valid_abstract_section_header('PATIENT CONCERNS AND DIAGNOSIS',		1).
valid_abstract_section_header('PATIENT CONCERNS/DIAGNOSES/INTERVENTIONS/OUTCOMES',		1).
valid_abstract_section_header('PATIENT DEMOGRAPHICS',		1).
valid_abstract_section_header('PATIENT FOCUS, PROCESS IMPROVEMENT, AND PERFORMANCE PATTERNS',		1).
valid_abstract_section_header('PATIENT FOCUSED SIMULATION',		1).
valid_abstract_section_header('PATIENT HISTORIES',		1).
valid_abstract_section_header('PATIENT HISTORY AND INFORMED CONSENT',		1).
valid_abstract_section_header('PATIENT OR STUDY POPULATION',		1).
valid_abstract_section_header('PATIENT PARTICIPANTS',		1).
valid_abstract_section_header('PATIENT PRESENTATIONS',		1).
valid_abstract_section_header('PATIENT PROFILE',		1).
valid_abstract_section_header('PATIENT RECORD',		1).
valid_abstract_section_header('PATIENT SAFETY RECOMMENDATIONS',		1).
valid_abstract_section_header('PATIENT SAFETY TOPICS',		1).
valid_abstract_section_header('PATIENT SATISFACTION SURVEYS',		1).
valid_abstract_section_header('PATIENT SELECTION FOR OUTPATIENT THERAPY',		1).
valid_abstract_section_header('PATIENT SERIES',		1).
valid_abstract_section_header('PATIENT STUDY GROUP AND METHODS',		1).
valid_abstract_section_header('PATIENT SUBJECTS AND RESULTS',		1).
valid_abstract_section_header('PATIENT\'S MEASUREMENTS',		1).
valid_abstract_section_header('PATIENT(S) AND INTERVENTIONS',		1).
valid_abstract_section_header('PATIENT(S) N/A INTERVENTION(S)',		1).
valid_abstract_section_header('PATIENT, METHOD, AND RESULT',		1).
valid_abstract_section_header('PATIENT-SUBJECT SELECTION AND METHODOLOGY',		1).
valid_abstract_section_header('PATIENT/RESULTS',		1).
valid_abstract_section_header('PATIENTIN UND METHODEN',		1).
valid_abstract_section_header('PATIENTS & SETTING',		1).
valid_abstract_section_header('PATIENTS &METHODS',		1).
valid_abstract_section_header('PATIENTS (OR MATERIALS) AND METHODS',		1).
valid_abstract_section_header('PATIENTS (STUDIES)',		1).
valid_abstract_section_header('PATIENTS AND DATA',		1).
valid_abstract_section_header('PATIENTS AND METHODS/STUDY DESIGN',		1).
valid_abstract_section_header('PATIENTS AND OBJECTIVES',		1).
valid_abstract_section_header('PATIENTS AND SURGICAL PROCEDURE',		1).
valid_abstract_section_header('PATIENTS AND SURGICAL TECHNIQUE',		1).
valid_abstract_section_header('PATIENTS AND VOLUNTEERS',		1).
valid_abstract_section_header('PATIENTS ANDV METHODS',		1).
valid_abstract_section_header('PATIENTS COURSE',		1).
valid_abstract_section_header('PATIENTS E METHODS',		1).
valid_abstract_section_header('PATIENTS FOR PARTICIPATION',		1).
valid_abstract_section_header('PATIENTS MATERIAL AND METHODS',		1).
valid_abstract_section_header('PATIENTS MATERIALS & METHODS',		1).
valid_abstract_section_header('PATIENTS MATERIALS AND METHOD',		1).
valid_abstract_section_header('PATIENTS MATERIALS AND METHODS',		1).
valid_abstract_section_header('PATIENTS METHOD',		1).
valid_abstract_section_header('PATIENTS METHODS AND RESULTS',		1).
valid_abstract_section_header('PATIENTS RANDOM SAMPLE',		1).
valid_abstract_section_header('PATIENTS SAND METHODS',		1).
valid_abstract_section_header('PATIENTS SELECTED',		1).
valid_abstract_section_header('PATIENTS SIX HUNDRED THIRTY-SIX',		1).
valid_abstract_section_header('PATIENTS Y METHODS',		1).
valid_abstract_section_header('PATIENTS, DESIGN AND METHODS',		1).
valid_abstract_section_header('PATIENTS, DESIGN AND RESULTS',		1).
valid_abstract_section_header('PATIENTS, MATERIAL, AND METHOD',		1).
valid_abstract_section_header('PATIENTS, MEASUREMENT AND MAIN RESULTS',		1).
valid_abstract_section_header('PATIENTS, MEASUREMENTS AND ANALYSIS',		1).
valid_abstract_section_header('PATIENTS, METHODOLOGY AND RESULTS',		1).
valid_abstract_section_header('PATIENTS, METHODS AND STATUS',		1).
valid_abstract_section_header('PATIENTS, METHODS, AND MATERIALS',		1).
valid_abstract_section_header('PATIENTS, METHODS, RESULTS',		1).
valid_abstract_section_header('PATIENTS, RESULTS, CONCLUSIONS',		1).
valid_abstract_section_header('PATIENTS, SETTING, AND DESIGN',		1).
valid_abstract_section_header('PATIENTS/INTERVENTION/MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('PATIENTS/INTERVENTION/MEASUREMENTS',		1).
valid_abstract_section_header('PATIENTS/METHODS/MATERIALS',		1).
valid_abstract_section_header('PATIENTS/RESULT',		1).
valid_abstract_section_header('PATIENTS/SETTINGS',		1).
valid_abstract_section_header('PATIENTSMETHODS',		1).
valid_abstract_section_header('PATRICIPANTS',		1).
valid_abstract_section_header('PATTERN OF RETINOPATHY',		1).
valid_abstract_section_header('PATTERN OF SPREAD',		1).
valid_abstract_section_header('PATTERNS',		1).
valid_abstract_section_header('PATTERNS OF USE',		1).
valid_abstract_section_header('PAZIENTI E METODI',		1).
valid_abstract_section_header('PCR PROCEDURE',		1).
valid_abstract_section_header('PDB ACCESSION CODE',		1).
valid_abstract_section_header('PDB ID CODES',		1).
valid_abstract_section_header('PEDAGOGICAL DESIGN AND CONCLUSION',		1).
valid_abstract_section_header('PEER REVIEW',		1).
valid_abstract_section_header('PELVIC STRUCTURES',		1).
valid_abstract_section_header('PEPTIDE ANALOGUE ANTAGONISTS OF ANGIOTENSIN II RECEPTORS',		1).
valid_abstract_section_header('PERCEPTION AND ACTION ARE TIGHTLY LINKED',		1).
valid_abstract_section_header('PERCEPTION AND ATTENTION',		1).
valid_abstract_section_header('PERCEPTIONS OF RISK',		1).
valid_abstract_section_header('PERCUTANEOUS VERTEBROPLASTY',		1).
valid_abstract_section_header('PERFORATED DIVERTICULITIS',		1).
valid_abstract_section_header('PERFORATION OF THE NASAL SEPTUM MAY HAVE MULTIPLE CAUSES',		1).
valid_abstract_section_header('PERFORMANCE AND ACHIEVEMENTS',		1).
valid_abstract_section_header('PERFORMANCE EXPECTATIONS',		1).
valid_abstract_section_header('PERFORMANCE IMPROVEMENT',		1).
valid_abstract_section_header('PERFORMANCE IMPROVEMENT TEAM',		1).
valid_abstract_section_header('PERFORMANCE MEASURES IN THE ASSESSMENT OF FRACTURE RISK',		1).
valid_abstract_section_header('PERFORMANCE MEASURES PROPOSAL',		1).
valid_abstract_section_header('PERIODONTITIS',		1).
valid_abstract_section_header('PERIOPERATIVE MORBIDITY',		1).
valid_abstract_section_header('PERIPHERAL ARTERIAL DISEASE',		1).
valid_abstract_section_header('PERIPHERAL NEUROPATHY',		1).
valid_abstract_section_header('PERITONSILLAR ABSCESS',		1).
valid_abstract_section_header('PEROXISOMAL DIVISION COMPRISES THREE STEPS',		1).
valid_abstract_section_header('PEROXISOMES ARE FORMED BY TWO DISTINCT PATHWAYS',		1).
valid_abstract_section_header('PERPSPECTIVE',		1).
valid_abstract_section_header('PERSONAL IMPROVEMENT PROJECTS',		1).
valid_abstract_section_header('PERSONAL NON-PUBLISHED STUDIES',		1).
valid_abstract_section_header('PERSONALITY AND WORK OF FLORENCE NIGHTINGALE',		1).
valid_abstract_section_header('PERSONNEL RESOURCES',		1).
valid_abstract_section_header('PERSPECTIVE & TIME FRAME',		1).
valid_abstract_section_header('PERSPECTIVE STATEMENT',		1).
valid_abstract_section_header('PERSPECTIVES, CONCLUSION',		1).
valid_abstract_section_header('PERTUSSIS VACCINE',		1).
valid_abstract_section_header('PET AS AN IMAGING TOOL',		1).
valid_abstract_section_header('PFF-CM',		1).
valid_abstract_section_header('PHARMACEUTICAL THERAPY',		1).
valid_abstract_section_header('PHARMACODYNAMIC INTERACTIONS',		1).
valid_abstract_section_header('PHARMACOECONOMIC EVALUATION',		1).
valid_abstract_section_header('PHARMACOKINETIC INTERACTIONS',		1).
valid_abstract_section_header('PHARMACOKINETIC MODEL',		1).
valid_abstract_section_header('PHARMACOKINETICS AND METABOLISM',		1).
valid_abstract_section_header('PHARMACOKINETICS AS A DETERMINANT OF RESPONSE',		1).
valid_abstract_section_header('PHARMACOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('PHARMACOTHERAPY',		1).
valid_abstract_section_header('PHARMACY CONTRIBUTION',		1).
valid_abstract_section_header('PHARMOCOLOGICAL RELEVANCE',		1).
valid_abstract_section_header('PHASE IV',		1).
valid_abstract_section_header('PHASE OF STUDY',		1).
valid_abstract_section_header('PHASES OF THE ASSESSMENT',		1).
valid_abstract_section_header('PHASES OF WOUND HEALING',		1).
valid_abstract_section_header('PHENOMENOLOGY',		1).
valid_abstract_section_header('PHENOMENON OF INTEREST/VARIABLES MEASURED',		1).
valid_abstract_section_header('PHENOTYPE',		1).
valid_abstract_section_header('PHENOTYPIC PROFILE',		1).
valid_abstract_section_header('PHILIPPE BOURGOIS THEORIZING VIOLENCE IN THE AMERICAS A THIRTY-YEAR ETHNOGRAPHIC RETROSPECTIVE',		1).
valid_abstract_section_header('PHONETIC ASPECT',		1).
valid_abstract_section_header('PHONOSURGICAL OPERATIONS',		1).
valid_abstract_section_header('PHOTOGRAPHIC SCHEMES',		1).
valid_abstract_section_header('PHSSR DATA NEEDS MEETING',		1).
valid_abstract_section_header('PHYLOGENY AND LIFESTYLE',		1).
valid_abstract_section_header('PHYSICAL ABUSE',		1).
valid_abstract_section_header('PHYSICAL ACTIVITY',		1).
valid_abstract_section_header('PHYSICAL CHARACTERISTICS OF LASER RADIATION',		1).
valid_abstract_section_header('PHYSICAL EXAMINATION AND MANAGEMENT',		1).
valid_abstract_section_header('PHYSICAL EXERCISE 8 TRIALS',		1).
valid_abstract_section_header('PHYSICAL THERAPY',		1).
valid_abstract_section_header('PHYSICAL/PSYCHOSOCIAL INTERVENTIONS',		1).
valid_abstract_section_header('PHYSICIAN',		1).
valid_abstract_section_header('PHYSICIANS AS LEADERS',		1).
valid_abstract_section_header('PHYSIOGNOMIC THEORY',		1).
valid_abstract_section_header('PHYSIOLOGIC INJURY SEVERITY SCALES',		1).
valid_abstract_section_header('PHYSIOLOGICAL AND MEDICAL FACTORS',		1).
valid_abstract_section_header('PHYSIOLOGICAL AND MEDICAL RELEVANCE',		1).
valid_abstract_section_header('PHYSIOLOGICAL PROBLEMS',		1).
valid_abstract_section_header('PHYSIOLOGICAL THEORY',		1).
valid_abstract_section_header('PHYSIOLOGY OF DIASTOLE',		1).
valid_abstract_section_header('PHYSIOPATHOLOGIC IMPLICATIONS',		1).
valid_abstract_section_header('PHYTOTOXICITY',		1).
valid_abstract_section_header('PICO (M)',		1).
valid_abstract_section_header('PILOT APPLICATION STUDY',		1).
valid_abstract_section_header('PILOT PHASE',		1).
valid_abstract_section_header('PITCHER',		1).
valid_abstract_section_header('PITFALLS IN CULTURAL COMPETENCE TRAINING',		1).
valid_abstract_section_header('PITUITARY FUNCTION',		1).
valid_abstract_section_header('PIVOTAL RESULTS',		1).
valid_abstract_section_header('PLACEBO ANALGESIA',		1).
valid_abstract_section_header('PLACEBO REACTION',		1).
valid_abstract_section_header('PLAIN FILM OF THE URINARY TRACT',		1).
valid_abstract_section_header('PLANNED AND PRIMARY OUTCOME MEASURES',		1).
valid_abstract_section_header('PLANNED OUTPUTS',		1).
valid_abstract_section_header('PLANNED SAMPLE SIZE',		1).
valid_abstract_section_header('PLANNING ACTUAL IMPLEMENTATION',		1).
valid_abstract_section_header('PLANNING AND ACTIONS',		1).
valid_abstract_section_header('PLANNING AND IMPLEMENTATION',		1).
valid_abstract_section_header('PLANNING AND IMPLEMENTING THE TOOL',		1).
valid_abstract_section_header('PLANNING CARE',		1).
valid_abstract_section_header('PLANNING PROCESS',		1).
valid_abstract_section_header('PLANNING TECHNIQUE',		1).
valid_abstract_section_header('PLANNING THE PROGRAM',		1).
valid_abstract_section_header('PLANTS ARE UNIQUE AMONG EUKARYOTES IN HAVING EVOLVED ORGANELLES',		1).
valid_abstract_section_header('PLASMA AND RBC LITHIUM CONCENTRATIONS',		1).
valid_abstract_section_header('PLATELET FUNCTION',		1).
valid_abstract_section_header('PLEASE SEE RELATED RESEARCH',		1).
valid_abstract_section_header('PLEIOTROPIC EFFECTS',		1).
valid_abstract_section_header('PLEURAL LESIONS',		1).
valid_abstract_section_header('POINT 1',		1).
valid_abstract_section_header('POINT 2',		1).
valid_abstract_section_header('POINT 3',		1).
valid_abstract_section_header('POINT 4',		1).
valid_abstract_section_header('POINTS',		1).
valid_abstract_section_header('POINTS SAILLANTS',		1).
valid_abstract_section_header('POLICIES',		1).
valid_abstract_section_header('POLICY',		1).
valid_abstract_section_header('POLICY DESCRIPTION',		1).
valid_abstract_section_header('POLICY DEVELOPMENT',		1).
valid_abstract_section_header('POLICY DEVELOPMENTS',		1).
valid_abstract_section_header('POLICY IMPLICATION',		1).
valid_abstract_section_header('POLICY OPTIONS',		1).
valid_abstract_section_header('POLICY THEMES AND RECOMMENDATIONS',		1).
valid_abstract_section_header('POLITICAL SCIENCE RESEARCH',		1).
valid_abstract_section_header('POLYMERIZATION SHRINKAGE STRESS AND STRESS REDUCTION POSSIBILITIES',		1).
valid_abstract_section_header('POLYMORPHISMS',		1).
valid_abstract_section_header('POLYPOSIS',		1).
valid_abstract_section_header('POLYPOSIS AND ALLERGY',		1).
valid_abstract_section_header('POLYUNSATURATED FATTY ACID AND DEPRESSION',		1).
valid_abstract_section_header('POOR PROGNOSIS FACTORS',		1).
valid_abstract_section_header('POPULATION AND MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('POPULATION AND MEASUREMENTS',		1).
valid_abstract_section_header('POPULATION AND SAMPLE SIZE CONSIDERATIONS',		1).
valid_abstract_section_header('POPULATION AND SAMPLES',		1).
valid_abstract_section_header('POPULATION AND/OR METHODS',		1).
valid_abstract_section_header('POPULATION ECOLOGY',		1).
valid_abstract_section_header('POPULATION MAINTENANCE',		1).
valid_abstract_section_header('POPULATION SAMPLE',		1).
valid_abstract_section_header('POPULATION/DATA SOURCE',		1).
valid_abstract_section_header('POPULATION/METHODS',		1).
valid_abstract_section_header('PORPHYRIA CUTANEA TARDA',		1).
valid_abstract_section_header('POSIT',		1).
valid_abstract_section_header('POSITION OF THE PROBLEM',		1).
valid_abstract_section_header('POSITIVE CONTROL GROUP',		1).
valid_abstract_section_header('POSITIVE RECOMMENDATIONS',		1).
valid_abstract_section_header('POSSIBLE CLINICAL APPLICATIONS',		1).
valid_abstract_section_header('POSSIBLE ERRORS',		1).
valid_abstract_section_header('POSSIBLE MECHANISM OF THE ACTION BY MEANS OF IMMUNE SYSTEM TO NERVOUS SYSTEM',		1).
valid_abstract_section_header('POSSIBLE MECHANISMS',		1).
valid_abstract_section_header('POSSIBLE NEW SURVEILLANCE RECOMMENDATIONS',		1).
valid_abstract_section_header('POSSIBLE REASONS FOR THE LACK OF SUCCESS WITH FACILITATED PCI',		1).
valid_abstract_section_header('POSSIBLE RENAL REPLACEMENT MODALITIES',		1).
valid_abstract_section_header('POSSIBLE SITES OF INSULIN RESISTANCE IN TERMINAL RENAL FAILURE',		1).
valid_abstract_section_header('POST-NOBEL PERSPECTIVE',		1).
valid_abstract_section_header('POST-OPERATIVE',		1).
valid_abstract_section_header('POST-PREGNANCY OUTCOMES',		1).
valid_abstract_section_header('POST-TREATMENT RECORD',		1).
valid_abstract_section_header('POSTDISCHARGE EDUCATION OF COMPLEX PATIENTS BY PHARMACISTS',		1).
valid_abstract_section_header('POSTERIORLY',		1).
valid_abstract_section_header('POSTGRADUATE AND CONTINUING MEDICAL EDUCATION',		1).
valid_abstract_section_header('POSTMANIPULATION MANAGEMENT',		1).
valid_abstract_section_header('POSTOPERATIVE COURSE AND TIMING OF THE OPERATION',		1).
valid_abstract_section_header('POSTOPERATIVE FINDINGS',		1).
valid_abstract_section_header('POSTSCRIPT',		1).
valid_abstract_section_header('POSTTRANSPLANT FACTOR EFFECTS',		1).
valid_abstract_section_header('POTASSIUM AND BLOOD PRESSURE',		1).
valid_abstract_section_header('POTENTIAL AND LIMITATIONS',		1).
valid_abstract_section_header('POTENTIAL AND RESEARCH QUESTIONS',		1).
valid_abstract_section_header('POTENTIAL APOPTOTIC FACTORS AND THERAPEUTIC TARGET',		1).
valid_abstract_section_header('POTENTIAL BENEFITS',		1).
valid_abstract_section_header('POTENTIAL BENEFITS AND RISKS OF DATA SHARING',		1).
valid_abstract_section_header('POTENTIAL BENEFITS OF MCFA METABOLISM IN CARDIAC DISEASES',		1).
valid_abstract_section_header('POTENTIAL CLINICAL APPLICATIONS',		1).
valid_abstract_section_header('POTENTIAL CONFLICT OF INTERESTS',		1).
valid_abstract_section_header('POTENTIAL IMPACT OF PAY FOR PERFORMANCE',		1).
valid_abstract_section_header('POTENTIAL IMPACT OF STUDY',		1).
valid_abstract_section_header('POTENTIAL INDICATIONS',		1).
valid_abstract_section_header('POTENTIAL LIMITATIONS',		1).
valid_abstract_section_header('POTENTIAL LIMITATIONS AND METHODOLOGICAL CHALLENGES',		1).
valid_abstract_section_header('POTENTIAL MEDICAL APPLICATIONS',		1).
valid_abstract_section_header('POTENTIAL OF ANGIOTENSIN (ANG) II ANTAGONISTS',		1).
valid_abstract_section_header('POTENTIAL PROMOTION EFFECT',		1).
valid_abstract_section_header('POTENTIAL RESULTS',		1).
valid_abstract_section_header('POTENTIAL TREATMENT FOR SDB IN PREGNANCY',		1).
valid_abstract_section_header('POWER OF THE TECHNIQUE',		1).
valid_abstract_section_header('PRACTICAL AND SOCIAL IMPLICATIONS',		1).
valid_abstract_section_header('PRACTICAL APPROACH',		1).
valid_abstract_section_header('PRACTICAL APPROACHES TO DISCLOSING ADVERSE EVENTS',		1).
valid_abstract_section_header('PRACTICAL BASICS AND INTERPRETATION OF THE FINDINGS',		1).
valid_abstract_section_header('PRACTICAL IMPLEMENTATIONS',		1).
valid_abstract_section_header('PRACTICAL PRELIMINARY CONCLUSIONS',		1).
valid_abstract_section_header('PRACTICAL RECOMMENDATION',		1).
valid_abstract_section_header('PRACTICAL RESULTS',		1).
valid_abstract_section_header('PRACTICAL SUGGESTIONS',		1).
valid_abstract_section_header('PRACTICAL UTILITY',		1).
valid_abstract_section_header('PRACTICALITIES AND PROBLEMS',		1).
valid_abstract_section_header('PRACTICE ADVANCEMENT',		1).
valid_abstract_section_header('PRACTICE APPLICATION',		1).
valid_abstract_section_header('PRACTICE CONSIDERATIONS',		1).
valid_abstract_section_header('PRACTICE GUIDELINES',		1).
valid_abstract_section_header('PRACTICE GUIDELINES FOR MEDICAL CARE',		1).
valid_abstract_section_header('PRACTICE IMPLICATIONS:',		1).
valid_abstract_section_header('PRACTICE INNOVATION AND RESULTS',		1).
valid_abstract_section_header('PRACTICE INTERVENTION',		1).
valid_abstract_section_header('PRACTICE SETTINGS',		1).
valid_abstract_section_header('PRACTICE/IMPLICATIONS',		1).
valid_abstract_section_header('PRACTICE/POLICY',		1).
valid_abstract_section_header('PRACTISE IMPLICATIONS',		1).
valid_abstract_section_header('PRATICE IMPLICATIONS',		1).
valid_abstract_section_header('PRE-TREATMENT RECORDS',		1).
valid_abstract_section_header('PREACHING TO THE CHOIR, AND BEYONDHETEROSEXISM',		1).
valid_abstract_section_header('PRECAUTIONS FOR USE',		1).
valid_abstract_section_header('PRECEDENTS AND AIM',		1).
valid_abstract_section_header('PRECISE INDICATIONS',		1).
valid_abstract_section_header('PRECLINICAL',		1).
valid_abstract_section_header('PRECLINICAL DATA',		1).
valid_abstract_section_header('PRECLINICAL STUDIES WITH LOSARTAN',		1).
valid_abstract_section_header('PRECONDITIONING IN HUMANS',		1).
valid_abstract_section_header('PRECONDITIONS FOR INDICATION',		1).
valid_abstract_section_header('PREDEMENTIA STATES',		1).
valid_abstract_section_header('PREDICTABLE VARIABLES',		1).
valid_abstract_section_header('PREDICTIONS',		1).
valid_abstract_section_header('PREDICTOR & OUTCOME',		1).
valid_abstract_section_header('PREDICTORS & OUTCOME',		1).
valid_abstract_section_header('PREDICTORS OF MORTALITY',		1).
valid_abstract_section_header('PREFORMED CROWNS',		1).
valid_abstract_section_header('PREGNANCY AND TUBERCULOSIS',		1).
valid_abstract_section_header('PREGNANCY AS A STRESSFUL EVENT',		1).
valid_abstract_section_header('PRELIMINARY ASSESSMENT',		1).
valid_abstract_section_header('PRELIMINARY BINDING MODEL OF SERUM ALBUMIN',		1).
valid_abstract_section_header('PRELIMINARY OUTCOME',		1).
valid_abstract_section_header('PRELIMINARY RESULTS AND OBSERVATIONS',		1).
valid_abstract_section_header('PREMISE OF RESEARCH',		1).
valid_abstract_section_header('PREMISE OF THIS STUDY',		1).
valid_abstract_section_header('PREOPERATIVE',		1).
valid_abstract_section_header('PREOPERATIVE DIAGNOSTICS AND DOCUMENTATION',		1).
valid_abstract_section_header('PREOPERATIVE INFORMED CONSENT',		1).
valid_abstract_section_header('PREOPERATIVE MEDICATION',		1).
valid_abstract_section_header('PREOPERATIVE MORTALITY RISK FACTORS',		1).
valid_abstract_section_header('PREOPERATIVE RECOGNITION',		1).
valid_abstract_section_header('PREPARATION FOR ADULT LIFE',		1).
valid_abstract_section_header('PREPARATION OF THE BONDING SURFACE',		1).
valid_abstract_section_header('PREPAREDNESS AND MITIGATION',		1).
valid_abstract_section_header('PREREQUISITES AND ACCELERATORS',		1).
valid_abstract_section_header('PREREQUISITES OF SURGICAL TREATMENT',		1).
valid_abstract_section_header('PRERSENTATION OF THE CASE',		1).
valid_abstract_section_header('PRESCRIPTION',		1).
valid_abstract_section_header('PRESCRIPTION RULES',		1).
valid_abstract_section_header('PRESENCE IN MOSS OF GENES INVOLVED IN POLLEN WALL DEVELOPMENT',		1).
valid_abstract_section_header('PRESENT AND FUTURE DIRECTIONS',		1).
valid_abstract_section_header('PRESENT MATERIAL',		1).
valid_abstract_section_header('PRESENT REPORT',		1).
valid_abstract_section_header('PRESENT STATE',		1).
valid_abstract_section_header('PRESENTAION OF CASE',		1).
valid_abstract_section_header('PRESENTATIOIN OF THE CASE',		1).
valid_abstract_section_header('PRESENTATION AND CLINICAL EVALUATION',		1).
valid_abstract_section_header('PRESENTATION OF CASE SERIES',		1).
valid_abstract_section_header('PRESENTATION OF HYPOTHESES',		1).
valid_abstract_section_header('PRESENTATION OF IPT',		1).
valid_abstract_section_header('PRESENTATION OF MAIN THERAPEUTIC INTERVENTIONS',		1).
valid_abstract_section_header('PRESENTATION OF THE INNOVATION',		1).
valid_abstract_section_header('PRESENTATION OF TWO CASES',		1).
valid_abstract_section_header('PRESENTATIONS',		1).
valid_abstract_section_header('PRESENTED IN PART',		1).
valid_abstract_section_header('PRESENTIATION OF CASE',		1).
valid_abstract_section_header('PRESENTING AND DEVELOPING FOCUS GROUP RESEARCH',		1).
valid_abstract_section_header('PRESENTING CONCERNS',		1).
valid_abstract_section_header('PRESENTING CONCERNS OF THE PATIENT',		1).
valid_abstract_section_header('PRESSURE ANALYSIS',		1).
valid_abstract_section_header('PRESSURE DAMAGE',		1).
valid_abstract_section_header('PRESSURE SORE PREVENTION RESEARCH MINDEDNESS FOR PRACTICE',		1).
valid_abstract_section_header('PRESSURE SORES PREVENTION MANUAL LEG AND FOOT ULCERS',		1).
valid_abstract_section_header('PRETERM BIRTH',		1).
valid_abstract_section_header('PRETRANSPLANT FACTOR EFFECTS',		1).
valid_abstract_section_header('PREVALENCE AND CLINICAL SIGNIFICANCE OF IMT',		1).
valid_abstract_section_header('PREVALENCE AND PROGNOSTIC IMPORTANCE',		1).
valid_abstract_section_header('PREVALENCE OF ABDOMINAL AORTIC ANEURYSMS',		1).
valid_abstract_section_header('PREVALENCE OF CELIAC DISEASE IN HIGH RISK SUBJECTS',		1).
valid_abstract_section_header('PREVALENCE OF CELIAC DISEASE IN THE GENERAL POPULATION',		1).
valid_abstract_section_header('PREVALENCE OF EXISTING PUS',		1).
valid_abstract_section_header('PREVALENCES',		1).
valid_abstract_section_header('PREVALENT PATIENTS',		1).
valid_abstract_section_header('PREVENTION AND CONTROL',		1).
valid_abstract_section_header('PREVENTION AND MANAGEMENT',		1).
valid_abstract_section_header('PREVENTION AND MANAGEMENT OF FISTULAS',		1).
valid_abstract_section_header('PREVENTION AND STATISTICS',		1).
valid_abstract_section_header('PREVENTION OF AND DEALING WITH ADVERSE EVENTS',		1).
valid_abstract_section_header('PREVENTION OF BLOOD CONTAMINATION',		1).
valid_abstract_section_header('PREVENTION OF DEVELOPMENT OF BACTERIAL RESISTANCE',		1).
valid_abstract_section_header('PREVENTION OF DISEASE TRANSMISSION',		1).
valid_abstract_section_header('PREVENTION OF GYNECOLOGICAL DISEASE',		1).
valid_abstract_section_header('PREVENTION OF INFECTION AND SURGICAL APPROACH',		1).
valid_abstract_section_header('PREVENTION OF INFECTIVE ENDOCARDITIS',		1).
valid_abstract_section_header('PREVENTION OF NEPHROPATHY',		1).
valid_abstract_section_header('PREVENTION OF REINJURY',		1).
valid_abstract_section_header('PREVENTION OF RELAPSE',		1).
valid_abstract_section_header('PREVENTION OF SIDE EFFECTS',		1).
valid_abstract_section_header('PREVENTION STRATEGIES OF NEUROCOGNITIVE DECLINE DUE TO WHOLE BRAIN RADIOTHERAPY (WBRT)',		1).
valid_abstract_section_header('PREVENTION STRATEGY',		1).
valid_abstract_section_header('PREVENTION/EARLY DETECTION',		1).
valid_abstract_section_header('PREVENTIVE MEASURES DURING PREGNANCY',		1).
valid_abstract_section_header('PREVIOUS CLASSIFICATIONS AND TERMINOLOGY',		1).
valid_abstract_section_header('PREVIOUS SURGICAL HISTORY',		1).
valid_abstract_section_header('PRICING CONTROL',		1).
valid_abstract_section_header('PRIMARY',		1).
valid_abstract_section_header('PRIMARY ANALYSIS',		1).
valid_abstract_section_header('PRIMARY AND SECOND OUTCOME MEASURES',		1).
valid_abstract_section_header('PRIMARY AND SECONDARY ENDPOINTS',		1).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME MEASURE METHODS',		1).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME MEASURES AND DATA SYNTHESIS',		1).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME MEASURES METHODS',		1).
valid_abstract_section_header('PRIMARY AND SECONDARY OUTCOME MEASURES PLANNED',		1).
valid_abstract_section_header('PRIMARY CARE RESEARCH TEAM ASSESSMENT',		1).
valid_abstract_section_header('PRIMARY CONCLUSIONS',		1).
valid_abstract_section_header('PRIMARY FUNDING SOURCES',		1).
valid_abstract_section_header('PRIMARY HEALTH CARE IN POLAND',		1).
valid_abstract_section_header('PRIMARY INDEPENDENT VARIABLES',		1).
valid_abstract_section_header('PRIMARY OUTCOME CVD RESULTS',		1).
valid_abstract_section_header('PRIMARY OUTCOME MEASUREMENT',		1).
valid_abstract_section_header('PRIMARY OUTCOME PARAMETERS',		1).
valid_abstract_section_header('PRIMARY OUTCOMES MEASURE',		1).
valid_abstract_section_header('PRIMARY PRACTICE',		1).
valid_abstract_section_header('PRIMARY PRACTICE SITE',		1).
valid_abstract_section_header('PRIMARY PROPHYLAXIS',		1).
valid_abstract_section_header('PRIMARY QUESTIONS',		1).
valid_abstract_section_header('PRIMARY SCREENING EXAMPLE',		1).
valid_abstract_section_header('PRIMARY SECONDARY OUTCOME MEASURES',		1).
valid_abstract_section_header('PRIMARY SETTING',		1).
valid_abstract_section_header('PRIMARY SOURCE OF FUNDING',		1).
valid_abstract_section_header('PRIMARY STUDY OBJECTIVES',		1).
valid_abstract_section_header('PRIMARY STUDY VARIABLES',		1).
valid_abstract_section_header('PRIMARY SUBJECT HEADING',		1).
valid_abstract_section_header('PRIMARY THERAPY IN HIV INFECTION',		1).
valid_abstract_section_header('PRIMARY TREATMENT',		1).
valid_abstract_section_header('PRIMARY/SECONDARY OUTCOMES',		1).
valid_abstract_section_header('PRINATAL TUBERCULOSIS',		1).
valid_abstract_section_header('PRINCIPAL',		1).
valid_abstract_section_header('PRINCIPAL EVIDENCE',		1).
valid_abstract_section_header('PRINCIPAL FEATURES',		1).
valid_abstract_section_header('PRINCIPAL FINDING AND CONCLUSIONS',		1).
valid_abstract_section_header('PRINCIPAL FINDINGS (STUDY1)',		1).
valid_abstract_section_header('PRINCIPAL FINDINGS (STUDY2)',		1).
valid_abstract_section_header('PRINCIPAL FINDINGS AND CONCLUSION',		1).
valid_abstract_section_header('PRINCIPAL JUDGEMENT CRITERIA',		1).
valid_abstract_section_header('PRINCIPAL MEASUREMENT',		1).
valid_abstract_section_header('PRINCIPAL RECOMMENDATIONS',		1).
valid_abstract_section_header('PRINCIPLE FEATURES',		1).
valid_abstract_section_header('PRINCIPLE FINDING/CONCLUSION',		1).
valid_abstract_section_header('PRINCIPLE FINDINGS/CONCLUSIONS',		1).
valid_abstract_section_header('PRINCIPLE OBSERVATION',		1).
valid_abstract_section_header('PRINCIPLE OF AIET',		1).
valid_abstract_section_header('PRINCIPLE OF THE METHOD',		1).
valid_abstract_section_header('PRINCIPLE RESEARCH QUESTION',		1).
valid_abstract_section_header('PRINCIPLES AND FRAMEWORK',		1).
valid_abstract_section_header('PRINCIPLES AND STRATEGIES FOR QUALITY MANAGEMENT AND DEVELOPMENT',		1).
valid_abstract_section_header('PRINCIPLES ILLUSTRATED',		1).
valid_abstract_section_header('PRINCIPLES OF MAGNETOENCEPHALOGRAPHY',		1).
valid_abstract_section_header('PRINCIPLES OF SURGICAL PROCEDURES',		1).
valid_abstract_section_header('PRIOR PRESENTATIONS',		1).
valid_abstract_section_header('PRIORITIES AND GOALS OF TREATMENT',		1).
valid_abstract_section_header('PRIORITIES FOR INTERNATIONAL RELIEF',		1).
valid_abstract_section_header('PRISTINA IN THE MEASUREMENTS POINT IN',		1).
valid_abstract_section_header('PRISTINAMYCIN VS AMOXICILLIN/CLAVULANIC ACID',		1).
valid_abstract_section_header('PRISTINAMYCIN VS CEFUROXIME AXETIL',		1).
valid_abstract_section_header('PRIVATE SECTOR ROLE IN HEALTH CARE QUALITY MONITORING',		1).
valid_abstract_section_header('PROBANDEN UND METHODIK',		1).
valid_abstract_section_header('PROBLEM AND AIMS',		1).
valid_abstract_section_header('PROBLEM AND PURPOSE',		1).
valid_abstract_section_header('PROBLEM AREAS',		1).
valid_abstract_section_header('PROBLEM ASSESSED',		1).
valid_abstract_section_header('PROBLEM IDENTIFICATION PROCESS',		1).
valid_abstract_section_header('PROBLEM OVERVIEW',		1).
valid_abstract_section_header('PROBLEM PRESENTED',		1).
valid_abstract_section_header('PROBLEM SETTING',		1).
valid_abstract_section_header('PROBLEM STATEMENT AND PURPOSE',		1).
valid_abstract_section_header('PROBLEM/CONTEXT',		1).
valid_abstract_section_header('PROBLEMATIC',		1).
valid_abstract_section_header('PROBLEMS ADDRESSED',		1).
valid_abstract_section_header('PROBLEMS AND AIMS',		1).
valid_abstract_section_header('PROBLEMS OF RADIOACTIVE STENTS',		1).
valid_abstract_section_header('PROBLEMS WITH QUALITY OF DATA EXTRACTED FROM THE CHAP DATABASE',		1).
valid_abstract_section_header('PROCEDURE AND OUTCOMES',		1).
valid_abstract_section_header('PROCEDURE AND PROTOCOL FOR MEASUREMENTS',		1).
valid_abstract_section_header('PROCEDURE(S)',		1).
valid_abstract_section_header('PROCEDURE, RESULTS, AND CONCLUSIONS',		1).
valid_abstract_section_header('PROCEDURE/FINDINGS',		1).
valid_abstract_section_header('PROCEDURES AND OUTCOME MEASURES',		1).
valid_abstract_section_header('PROCEDURES AND OUTCOMES',		1).
valid_abstract_section_header('PROCEDURES TO PROMOTE GOOD MEASUREMENT',		1).
valid_abstract_section_header('PROCEDURES/MEASUREMENTS',		1).
valid_abstract_section_header('PROCEDURES/RESULTS',		1).
valid_abstract_section_header('PROCESS AND OUTCOMES',		1).
valid_abstract_section_header('PROCESS AND RESULTS',		1).
valid_abstract_section_header('PROCESS CHANGES',		1).
valid_abstract_section_header('PROCESS EVALUATION',		1).
valid_abstract_section_header('PROCESS IMPROVEMENT TEAMS',		1).
valid_abstract_section_header('PROCESSES',		1).
valid_abstract_section_header('PROCESSES FOR MEDICATION MANAGEMENT IN HOME HEALTH CARE',		1).
valid_abstract_section_header('PRODUCT CATEGORY SYSTEM',		1).
valid_abstract_section_header('PRODUCT NOTIFICATION PROCEDURES',		1).
valid_abstract_section_header('PRODUCT TECHNOLOGY',		1).
valid_abstract_section_header('PRODUCTION OF RADIONUCLIDES USING NEWER TECHNOLOGIES',		1).
valid_abstract_section_header('PRODUCTS AND APPLICATIONS',		1).
valid_abstract_section_header('PROFESSIONAL ACCOUNTABILITY',		1).
valid_abstract_section_header('PROFESSIONAL ETHICS, ATTITUDES, AND PRACTICE UNDER MANAGED CARE',		1).
valid_abstract_section_header('PROFESSIONAL SURVEY',		1).
valid_abstract_section_header('PROGNOSIS AND THERAPY PREDICTORS',		1).
valid_abstract_section_header('PROGNOSIS OF CUP',		1).
valid_abstract_section_header('PROGNOSIS, RESULTS',		1).
valid_abstract_section_header('PROGNOSTIC AND PREDICTIVE CHARACTERIZATION',		1).
valid_abstract_section_header('PROGNOSTIC CRITERIA',		1).
valid_abstract_section_header('PROGNOSTIC FACTORS',		1).
valid_abstract_section_header('PROGNOSTICATION',		1).
valid_abstract_section_header('PROGRAM AND RESEARCH NEEDS',		1).
valid_abstract_section_header('PROGRAM BENEFITS',		1).
valid_abstract_section_header('PROGRAM DEVELOPMENT AND IMPLEMENTATION',		1).
valid_abstract_section_header('PROGRAM FEASIBILITY EVALUATION',		1).
valid_abstract_section_header('PROGRAM IMPACT',		1).
valid_abstract_section_header('PROGRAM INTERVENTIONS',		1).
valid_abstract_section_header('PROGRAM OUTLINE',		1).
valid_abstract_section_header('PROGRAM PRIORITIES AND INITIATIVES',		1).
valid_abstract_section_header('PROGRAM STRATEGIES',		1).
valid_abstract_section_header('PROGRAMME',		1).
valid_abstract_section_header('PROGRAMME ACTIVITIES AND ANALYSIS',		1).
valid_abstract_section_header('PROGRAMME DESCRIPTION AND OUTCOMES',		1).
valid_abstract_section_header('PROGRAMME DESIGN',		1).
valid_abstract_section_header('PROGRAMME IMPLEMENTATION',		1).
valid_abstract_section_header('PROGRAMME INDICATORS',		1).
valid_abstract_section_header('PROGRAMME SETTING',		1).
valid_abstract_section_header('PROGRAMME\'S NATIONAL COMMITTEE',		1).
valid_abstract_section_header('PROGRAMMES',		1).
valid_abstract_section_header('PROGRESS IN THE STUDY OF CABG SURGERY',		1).
valid_abstract_section_header('PROGRESS IN THE STUDY OF PCIS',		1).
valid_abstract_section_header('PROGRESSION AND PERSPECTIVES',		1).
valid_abstract_section_header('PROGRESSION OF CORONARY CALCIUM',		1).
valid_abstract_section_header('PROJECT DESCRIPTION/METHODOLOGY',		1).
valid_abstract_section_header('PROJECT DESIGN',		1).
valid_abstract_section_header('PROJECT DESIGN AND APPROACH',		1).
valid_abstract_section_header('PROJECT EXECUTION',		1).
valid_abstract_section_header('PROJECT METHODOLOGY',		1).
valid_abstract_section_header('PROJECT OVERVIEW',		1).
valid_abstract_section_header('PROJECT REGISTRATION NUMBER',		1).
valid_abstract_section_header('PROJECT SETTING',		1).
valid_abstract_section_header('PROJECT URL',		1).
valid_abstract_section_header('PROJECTING INTO THE FUTURE AND CONCLUSIONS',		1).
valid_abstract_section_header('PROJECTIONS',		1).
valid_abstract_section_header('PROJECTIVE IDENTIFICATION',		1).
valid_abstract_section_header('PROLIFERATIVE PATHWAY',		1).
valid_abstract_section_header('PROLONGED POSTPRANDIAL LIPEMIA',		1).
valid_abstract_section_header('PROMISING NEW TRENDS IN DIABETIC NEPHROPATHY TREATMENT',		1).
valid_abstract_section_header('PROOF LEVEL',		1).
valid_abstract_section_header('PROPHYLACTIC REGIMEN',		1).
valid_abstract_section_header('PROPHYLACTIC SURGICAL INTERVENTIONS',		1).
valid_abstract_section_header('PROPHYLAXIS AND REHABILITATION OF PERSONS EXPOSED TO EMOTIONAL STRESS',		1).
valid_abstract_section_header('PROPORTION OF CATS THAT ARE NEUTERED',		1).
valid_abstract_section_header('PROPOSAL AND CONCLUSIONS',		1).
valid_abstract_section_header('PROPOSAL OF A METHODOLOGY',		1).
valid_abstract_section_header('PROPOSALS AND RECOMMENDATIONS',		1).
valid_abstract_section_header('PROPOSALS FOR IMPROVEMENT',		1).
valid_abstract_section_header('PROPOSED DESIGN',		1).
valid_abstract_section_header('PROPOSED DISCUSSION POINTS',		1).
valid_abstract_section_header('PROPOSED EXPERIMENTAL SETTING TO TEST THE HYPOTHESIS',		1).
valid_abstract_section_header('PROPOSED FOLLOW-UP PATHWAY',		1).
valid_abstract_section_header('PROPOSED FRAMEWORK',		1).
valid_abstract_section_header('PROPOSED MECHANISM',		1).
valid_abstract_section_header('PROPOSED MODEL FOR COMMUNITY SCHOLARSHIP',		1).
valid_abstract_section_header('PROPOSED OUTCOME MEASURES',		1).
valid_abstract_section_header('PROPOSED POLICIES',		1).
valid_abstract_section_header('PROPOSED PROTOCOLS',		1).
valid_abstract_section_header('PROPOSED STANDARDIZATION AND RESEARCH',		1).
valid_abstract_section_header('PROSPECT AND CONCLUSION',		1).
valid_abstract_section_header('PROSPECT AND PROJECT',		1).
valid_abstract_section_header('PROSPECTIVE COHORT STUDY',		1).
valid_abstract_section_header('PROSPECTIVE INTERVENTION STUDY IN PROGRESS',		1).
valid_abstract_section_header('PROSPECTIVE RESULTS',		1).
valid_abstract_section_header('PROSPECTIVE STUDIES',		1).
valid_abstract_section_header('PROSPECTS AND CONCLUSION',		1).
valid_abstract_section_header('PROSPECTS/PROJECTIONS',		1).
valid_abstract_section_header('PROSPERO 2014',		1).
valid_abstract_section_header('PROSPERO CRD REGISTRATION NUMBER',		1).
valid_abstract_section_header('PROSPERO NUMBER',		1).
valid_abstract_section_header('PROSPERO REFERENCE',		1).
valid_abstract_section_header('PROSPERO REGISTER',		1).
valid_abstract_section_header('PROSPERO REGISTER NUMBER',		1).
valid_abstract_section_header('PROSPERO REGISTRATION INFORMATION',		1).
valid_abstract_section_header('PROSTATIC CARCINOMA',		1).
valid_abstract_section_header('PROSTHETIC RECONSTRUCTION OF DEPULPED TEETH',		1).
valid_abstract_section_header('PROSTHETIC REHABILITATION',		1).
valid_abstract_section_header('PROTECT-PACE',		1).
valid_abstract_section_header('PROTECTION THROUGH HELICOBACTER PYLORI',		1).
valid_abstract_section_header('PROTECTIVE MECHANISMS IN THE PLACENTA',		1).
valid_abstract_section_header('PROTECTIVE MECHANISMS SHARED BY THE PLACENTA AND UTERUS',		1).
valid_abstract_section_header('PROTEIN DATA BANK ACCESSION NUMBERS',		1).
valid_abstract_section_header('PROTEIN MARKERS IN URINE',		1).
valid_abstract_section_header('PROTEINURIA',		1).
valid_abstract_section_header('PROTOCOL AND INTERIM RESULTS OF INTERNATIONAL TRANSLATION OF PROJECT EX',		1).
valid_abstract_section_header('PROTOCOL B',		1).
valid_abstract_section_header('PROTOCOL DESIGN',		1).
valid_abstract_section_header('PROTOCOL NUMBER',		1).
valid_abstract_section_header('PROTOCOL NUMBERS',		1).
valid_abstract_section_header('PROTOCOL OF THE TRIAL',		1).
valid_abstract_section_header('PROTOCOL PROSPERO REGISTRATION NUMBER',		1).
valid_abstract_section_header('PROTOCOL PUBLICATION',		1).
valid_abstract_section_header('PROTOCOL REGISTRY',		1).
valid_abstract_section_header('PROTOCOL REGISTRY NUMBER',		1).
valid_abstract_section_header('PROTOTYPE CONOPS TO ELIMINATE HARM',		1).
valid_abstract_section_header('PROTOTYPE TESTING',		1).
valid_abstract_section_header('PROTRUSION',		1).
valid_abstract_section_header('PROVOCATION STUDY',		1).
valid_abstract_section_header('PRZYPADEK 1',		1).
valid_abstract_section_header('PRZYPADEK 2',		1).
valid_abstract_section_header('PSYCHIATRIC PHENOTYPE',		1).
valid_abstract_section_header('PSYCHIATRY AS AN INTEGRATIVE DISCIPLINE',		1).
valid_abstract_section_header('PSYCHOGENESIS',		1).
valid_abstract_section_header('PSYCHOLOGICAL CHANGES DURING PREGNANCY',		1).
valid_abstract_section_header('PSYCHOLOGICAL INTERVENTIONS',		1).
valid_abstract_section_header('PSYCHOLOGICAL THEORIES',		1).
valid_abstract_section_header('PSYCHOLOGICAL THEORY',		1).
valid_abstract_section_header('PSYCHOLOGICAL VARIABLES AS A FACTOR OF CONTRACEPTIVE (NON)USE',		1).
valid_abstract_section_header('PSYCHOPATHOLOGICAL ASPECTS',		1).
valid_abstract_section_header('PSYCHOSOCIAL CONSTRUCTS',		1).
valid_abstract_section_header('PSYCHOSOCIAL FACTORS',		1).
valid_abstract_section_header('PSYCHOSOCIAL KNOWLEDGE',		1).
valid_abstract_section_header('PSYCHOSOCIAL PROBLEMS',		1).
valid_abstract_section_header('PSYCHOSOCIAL PROBLEMS AND TREATMENT CONCEPTS',		1).
valid_abstract_section_header('PSYCHOSOMATICS IN UROLOGY',		1).
valid_abstract_section_header('PSYCHOTROPIC DRUGS',		1).
valid_abstract_section_header('PSYCINFO CLASSIFICATION',		1).
valid_abstract_section_header('PU PREVENTION IMPLEMENTATION PROJECT',		1).
valid_abstract_section_header('PUBLIC HEALTH AGENCIES AND IMPACT FOR LPHS',		1).
valid_abstract_section_header('PUBLIC HEALTH AND DIETARY IMPLICATIONS',		1).
valid_abstract_section_header('PUBLIC HEALTH CASE',		1).
valid_abstract_section_header('PUBLIC HEALTH FINDINGS',		1).
valid_abstract_section_header('PUBLIC OPINION',		1).
valid_abstract_section_header('PUBLICATION ABSTRACT',		1).
valid_abstract_section_header('PUBLICATION INDEX',		1).
valid_abstract_section_header('PUBLICATIONS',		1).
valid_abstract_section_header('PUBLISHED STUDIES',		1).
valid_abstract_section_header('PULMONARY DISEASES',		1).
valid_abstract_section_header('PULSED DOPPLER TISSUE IMAGING',		1).
valid_abstract_section_header('PUNCTURE ASPIRATION',		1).
valid_abstract_section_header('PUPOSE OF REVIEW',		1).
valid_abstract_section_header('PUROPSE',		1).
valid_abstract_section_header('PURPOSE & ABSTRACT',		1).
valid_abstract_section_header('PURPOSE & FIELD',		1).
valid_abstract_section_header('PURPOSE AND AIM',		1).
valid_abstract_section_header('PURPOSE AND CONCEPTUAL FRAMEWORK',		1).
valid_abstract_section_header('PURPOSE AND DESCRIPTION',		1).
valid_abstract_section_header('PURPOSE AND GOALS',		1).
valid_abstract_section_header('PURPOSE AND IMPORTANCE',		1).
valid_abstract_section_header('PURPOSE AND INTENDED AUDIENCE',		1).
valid_abstract_section_header('PURPOSE AND INTRODUCTION',		1).
valid_abstract_section_header('PURPOSE AND KEY ISSUES',		1).
valid_abstract_section_header('PURPOSE AND PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('PURPOSE AND QUESTIONS',		1).
valid_abstract_section_header('PURPOSE AND RESULTS',		1).
valid_abstract_section_header('PURPOSE AND SIGNIFICANCE',		1).
valid_abstract_section_header('PURPOSE AND STUDY DESIGN',		1).
valid_abstract_section_header('PURPOSE MATERIALS AND METHODS',		1).
valid_abstract_section_header('PURPOSE OF A NUTRITIONAL SCREENING TOOL',		1).
valid_abstract_section_header('PURPOSE OF LECTURE',		1).
valid_abstract_section_header('PURPOSE OF THE LECTURE',		1).
valid_abstract_section_header('PURPOSE OF THE STUDY AND RESULTS',		1).
valid_abstract_section_header('PURPOSE OF THIS PAPER',		1).
valid_abstract_section_header('PURPOSE OFREVIEW',		1).
valid_abstract_section_header('PURPOSE OR OBJECTIVE',		1).
valid_abstract_section_header('PURPOSE OR REVIEW',		1).
valid_abstract_section_header('PURPOSE of the STUDY',		1).
valid_abstract_section_header('PURPOSE(S)',		1).
valid_abstract_section_header('PURPOSE, OBJECTIVE',		1).
valid_abstract_section_header('PURPOSE-',		1).
valid_abstract_section_header('PURPOSE-METHODS',		1).
valid_abstract_section_header('PURPOSE/DESIGN',		1).
valid_abstract_section_header('PURPOSE/GOAL',		1).
valid_abstract_section_header('PURPOSE/OBJECTIVE(S):',		1).
valid_abstract_section_header('PURPOSE/OBJECTIVE/FOCUS',		1).
valid_abstract_section_header('PURPOSE/OBJECTIVE/RESEARCH QUESTION/FOCUS OF STUDY',		1).
valid_abstract_section_header('PURPOSE/OBJECTIVES(S)',		1).
valid_abstract_section_header('PURPOSE/OBJECTIVES:',		1).
valid_abstract_section_header('PURPOSEOBJECTIVES',		1).
valid_abstract_section_header('PURPOSES OF STUDY',		1).
valid_abstract_section_header('PURPOSES/METHODS',		1).
valid_abstract_section_header('PURPOSES:',		1).
valid_abstract_section_header('PURPOSING',		1).
valid_abstract_section_header('PURSPOSE',		1).
valid_abstract_section_header('PUSPOSE',		1).
valid_abstract_section_header('PUTTING IT ALL TOGETHER',		1).
valid_abstract_section_header('PUTTING THE MODEL TO WORK',		1).
valid_abstract_section_header('Patients and Methods',		1).
valid_abstract_section_header('Patients and methods',		1).
valid_abstract_section_header('Perspective',		1).
valid_abstract_section_header('Population and methods',		1).
valid_abstract_section_header('Purpose:',		1).
valid_abstract_section_header('Purpose?',		1).
valid_abstract_section_header('QCSPV',		1).
valid_abstract_section_header('QI INITIATIVES',		1).
valid_abstract_section_header('QI PROGRAMS',		1).
valid_abstract_section_header('QI STRATEGIES AND GUIDELINES IN DEVELOPING COUNTRIES',		1).
valid_abstract_section_header('QUALIFICATION OF NOSE INJURIES',		1).
valid_abstract_section_header('QUALIFYING STATEMENT',		1).
valid_abstract_section_header('QUALITATIVE FINDINGS',		1).
valid_abstract_section_header('QUALITY AND OUTCOMES',		1).
valid_abstract_section_header('QUALITY ASSESSMENT AND DATA EXTRACTION',		1).
valid_abstract_section_header('QUALITY CARE',		1).
valid_abstract_section_header('QUALITY IMPROVEMENT (QI)',		1).
valid_abstract_section_header('QUALITY MANAGEMENT INITIATIVES',		1).
valid_abstract_section_header('QUALITY MANAGEMENT MODELS AND TOOLS',		1).
valid_abstract_section_header('QUALITY OF LIFE CONCEPT',		1).
valid_abstract_section_header('QUALITY OF LIFE EVALUATION',		1).
valid_abstract_section_header('QUALITY OF LIFE MEASUREMENT IN CHILDREN',		1).
valid_abstract_section_header('QUALITY OF LIFE RESULTS',		1).
valid_abstract_section_header('QUALITY OF RESULTS',		1).
valid_abstract_section_header('QUALITY OF STUDIES',		1).
valid_abstract_section_header('QUALITY STANDARDS',		1).
valid_abstract_section_header('QUESTION ADDRESSED',		1).
valid_abstract_section_header('QUESTION AND AIM',		1).
valid_abstract_section_header('QUESTION AND RESULTS',		1).
valid_abstract_section_header('QUESTION SYNTHESIS',		1).
valid_abstract_section_header('QUESTION UNDER STUDY/PRINCIPLES',		1).
valid_abstract_section_header('QUESTION, HYPOTHESIS OR AIM',		1).
valid_abstract_section_header('QUESTIONNAIRE RESULTS',		1).
valid_abstract_section_header('QUESTIONNAIRE/PATIENTS',		1).
valid_abstract_section_header('QUESTIONS AND AIMS',		1).
valid_abstract_section_header('QUESTIONS AND METHODS',		1).
valid_abstract_section_header('QUININE AND SIMILAR MOLECULES',		1).
valid_abstract_section_header('R ESULTS',		1).
valid_abstract_section_header('R NA A',		1).
valid_abstract_section_header('RACE STRUCTURE, PATHOGENESIS AND EPIDEMIOLOGY',		1).
valid_abstract_section_header('RADIATION AT RECURRENCE',		1).
valid_abstract_section_header('RADIATION THERAPY',		1).
valid_abstract_section_header('RADIOLOGIC MORPHOLOGY',		1).
valid_abstract_section_header('RADIOLOGIC TECHNIQUE',		1).
valid_abstract_section_header('RADIOLOGICAL ARGUMENTS',		1).
valid_abstract_section_header('RADIOLOGICAL CHANGES',		1).
valid_abstract_section_header('RADIOLOGICAL DIAGNOSIS',		1).
valid_abstract_section_header('RADIOLOGICAL INVESTIGATION',		1).
valid_abstract_section_header('RADIOLOGY',		1).
valid_abstract_section_header('RADIOPHARMACEUTICALS LABELED WITH RHENIUM RADIOISOTOPES AND THEIR CLINICAL APPLICATIONS',		1).
valid_abstract_section_header('RADIOSURGERY',		1).
valid_abstract_section_header('RAISING TOBACCO EXCISE TAXES',		1).
valid_abstract_section_header('RAMIFICATIONS OF THIS REPORT',		1).
valid_abstract_section_header('RANDOMIZED CONTROLLED TRIALS',		1).
valid_abstract_section_header('RANDOMIZED TRIALS',		1).
valid_abstract_section_header('RANGE OF TOPICS',		1).
valid_abstract_section_header('RATERS',		1).
valid_abstract_section_header('RATING SCALES',		1).
valid_abstract_section_header('RATIONAL AND BACKGROUND',		1).
valid_abstract_section_header('RATIONALE & OBJECTIVE',		1).
valid_abstract_section_header('RATIONALE AND AIMS AND OBJECTIVES',		1).
valid_abstract_section_header('RATIONALE AND AIMS OF THE STUDY',		1).
valid_abstract_section_header('RATIONALE AND DEVELOPMENT',		1).
valid_abstract_section_header('RATIONALE AND EXPERIMENTAL APPROACH',		1).
valid_abstract_section_header('RATIONALE AND GOALS',		1).
valid_abstract_section_header('RATIONALE AND METHOD',		1).
valid_abstract_section_header('RATIONALE AND RESULTS',		1).
valid_abstract_section_header('RATIONALE FOR CASE REPORT',		1).
valid_abstract_section_header('RATIONALE FOR DEVELOPMENT OF A NEW METHODOLOGY',		1).
valid_abstract_section_header('RATIONALE FOR PRODUCT DEVELOPMENT',		1).
valid_abstract_section_header('RATIONALE FOR THE COMBINATION OF ACE INHIBITORS AND CALCIUM CHANNEL BLOCKERS',		1).
valid_abstract_section_header('RATIONALE FOR THIS CASE REPORT',		1).
valid_abstract_section_header('RATIONALE REVISITED',		1).
valid_abstract_section_header('RATIONALE, AIM & OBJECTIVES',		1).
valid_abstract_section_header('RATIONALE, AIMS & OBJECTIVES',		1).
valid_abstract_section_header('RATIONALE, AIMS, OBJECTIVES AND METHODS',		1).
valid_abstract_section_header('RATIONALE, METHOD',		1).
valid_abstract_section_header('RATIONALE/AIM',		1).
valid_abstract_section_header('RATIONALES AND CASE',		1).
valid_abstract_section_header('RATIONALES, AIMS AND OBJECTIVES',		1).
valid_abstract_section_header('RAVEL TRIAL',		1).
valid_abstract_section_header('RCT B',		1).
valid_abstract_section_header('RCT MAIN OUTCOME',		1).
valid_abstract_section_header('REACH',		1).
valid_abstract_section_header('REACTIVE EOSINOPHILIA DUE TO INFECTIOUS AND PARASITIC DISEASES',		1).
valid_abstract_section_header('REAL-LIFE IMPLICATIONS',		1).
valid_abstract_section_header('REALIZING RESULTS',		1).
valid_abstract_section_header('REASON',		1).
valid_abstract_section_header('REASON FOR THE STUDY',		1).
valid_abstract_section_header('REASON TO REPORT',		1).
valid_abstract_section_header('REASONS FOR PARTICIPATION OR DECLINING STUDY',		1).
valid_abstract_section_header('REASONS FOR PERFOMING STUDY',		1).
valid_abstract_section_header('REASONS FOR PERFORMING STUDY AND OBJECTIVE',		1).
valid_abstract_section_header('REASSESSMENT OF LOCAL PRACTICE',		1).
valid_abstract_section_header('REB APPROVAL',		1).
valid_abstract_section_header('RECEIVERS',		1).
valid_abstract_section_header('RECENT ADVANCES AND FUTURE DIRECTIONS',		1).
valid_abstract_section_header('RECENT ADVANCES AND FUTURE PROSPECTS',		1).
valid_abstract_section_header('RECENT CHANGES IN THE EUROPEAN THEATER',		1).
valid_abstract_section_header('RECENT CLINICAL AND EXPERIMENTAL FINDINGS',		1).
valid_abstract_section_header('RECENT DEVELOPMENT',		1).
valid_abstract_section_header('RECENT DRUGS',		1).
valid_abstract_section_header('RECENT FACTS',		1).
valid_abstract_section_header('RECENT FINDINGS/CONCLUSION',		1).
valid_abstract_section_header('RECENT FINDINGS/SUMMARY',		1).
valid_abstract_section_header('RECENT LITERATURE',		1).
valid_abstract_section_header('RECENT PROJECT',		1).
valid_abstract_section_header('RECENT RESEARCH ON CAUSES OF DIABETES',		1).
valid_abstract_section_header('RECENT RESEARCH PARADIGM',		1).
valid_abstract_section_header('RECENT RESULTS',		1).
valid_abstract_section_header('RECENT STUDY RESULTS',		1).
valid_abstract_section_header('RECENT THEMES',		1).
valid_abstract_section_header('RECEPTOR ANTAGONISTS',		1).
valid_abstract_section_header('RECEPTOR RELATED CHANGES INVOLVED IN TOLERANCE',		1).
valid_abstract_section_header('RECEPTOR SUBTYPE DIVERSITY',		1).
valid_abstract_section_header('RECIPIENT INSERTION',		1).
valid_abstract_section_header('RECOGNITION OF FRAILTY',		1).
valid_abstract_section_header('RECOGNITION, DIAGNOSTICS, MANAGEMENT AND TREATMENT',		1).
valid_abstract_section_header('RECOMMENDATION AND LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('RECOMMENDATION FIVE',		1).
valid_abstract_section_header('RECOMMENDATION FOUR',		1).
valid_abstract_section_header('RECOMMENDATION SIX',		1).
valid_abstract_section_header('RECOMMENDATION STATEMENTS',		1).
valid_abstract_section_header('RECOMMENDATION THREE',		1).
valid_abstract_section_header('RECOMMENDATION TWO',		1).
valid_abstract_section_header('RECOMMENDATIONS  LEVEL III',		1).
valid_abstract_section_header('RECOMMENDATIONS AND LESSONS LEARNED',		1).
valid_abstract_section_header('RECOMMENDATIONS FOLLOW-UP STRATEGIES',		1).
valid_abstract_section_header('RECOMMENDATIONS FOR ACTION',		1).
valid_abstract_section_header('RECOMMENDATIONS FOR FURTHER RESEARCH',		1).
valid_abstract_section_header('RECOMMENDATIONS FOR FURTHER STUDIES',		1).
valid_abstract_section_header('RECOMMENDATIONS FOR ORGANIZATIONS IN IMPLEMENTING, CPOE',		1).
valid_abstract_section_header('RECOMMENDATIONS FOR PRACTICE',		1).
valid_abstract_section_header('RECOMMENDATIONS FOR PSG AND MSLT USE',		1).
valid_abstract_section_header('RECOMMENDATIONS LEVEL II',		1).
valid_abstract_section_header('RECOMMENDATIONS SUMMARY',		1).
valid_abstract_section_header('RECOMMENDATIONS TO IMPROVE DATA',		1).
valid_abstract_section_header('RECOMMENDATIONS TREATMENTS',		1).
valid_abstract_section_header('RECOMMENDATIONS/OUTLOOK',		1).
valid_abstract_section_header('RECOMMENDED APPROACH',		1).
valid_abstract_section_header('RECORDING OF EVOKED RESPONSES AND PATTERNS OF NERVE STIMULATION',		1).
valid_abstract_section_header('RECOVERY AFTER STROKE',		1).
valid_abstract_section_header('RECRUITMENT AND BASELINE DATA',		1).
valid_abstract_section_header('RECRUITMENT AND RETENTION',		1).
valid_abstract_section_header('RECRUITMENT OF MOS',		1).
valid_abstract_section_header('RECRUTEMENT DES PARTICIPANTS',		1).
valid_abstract_section_header('RECTILINEAR SCANNER',		1).
valid_abstract_section_header('RECURRENT DISEASE AFTER LIVER TRANSPLANTATION',		1).
valid_abstract_section_header('RECURRENT EROSIONS',		1).
valid_abstract_section_header('RECURRENT THEMES',		1).
valid_abstract_section_header('REDESIGN OF THE PI SYSTEM',		1).
valid_abstract_section_header('REDUCED ACTIVITY OF THE POTENT ANABOLIC EFFECTORS',		1).
valid_abstract_section_header('REDUCING RISK THROUGH CONSUMER ENGAGEMENT',		1).
valid_abstract_section_header('REDUCTION OF HOMOCYSTEINE LEVELS',		1).
valid_abstract_section_header('REEFS AND PEOPLE AT RISK',		1).
valid_abstract_section_header('REENTRANT CIRCUITS',		1).
valid_abstract_section_header('REFERENCE POPULATION',		1).
valid_abstract_section_header('REFERENCE POPULATION COMPARISON',		1).
valid_abstract_section_header('REFERENCE RANGES',		1).
valid_abstract_section_header('REFERENCE STANDARD FOR HYDRATION STATUS',		1).
valid_abstract_section_header('REFERENCE TEST & MEASUREMENTS',		1).
valid_abstract_section_header('REFERRAL AND ASSESSMENT',		1).
valid_abstract_section_header('REFINING IMPLEMENTATION',		1).
valid_abstract_section_header('REFLECTANCE CONFOCAL MICROSCOPY',		1).
valid_abstract_section_header('REFLECTING ON LESBIAN LIVES',		1).
valid_abstract_section_header('REFLECTION AND DISCUSSION',		1).
valid_abstract_section_header('REFLEX EFFECTS ON THE CARDIOVASCULAR SYSTEM',		1).
valid_abstract_section_header('REFORMS',		1).
valid_abstract_section_header('REFVENTION(S)',		1).
valid_abstract_section_header('REGARDING TREATMENTS',		1).
valid_abstract_section_header('REGIONAL ANTICOAGULATION',		1).
valid_abstract_section_header('REGIONAL LEVEL',		1).
valid_abstract_section_header('REGISTER',		1).
valid_abstract_section_header('REGISTERED CLINICAL TRIAL NUMBER',		1).
valid_abstract_section_header('REGISTERED CLINICAL TRIAL NUMBERS',		1).
valid_abstract_section_header('REGISTERED TRIAL',		1).
valid_abstract_section_header('REGISTRATION DETAILS CLINICALTRIALSGOV IDENTIFIER',		1).
valid_abstract_section_header('REGISTRATION NO',		1).
valid_abstract_section_header('REGISTRATION NUMBER AND NAME OF TRIAL REGISTRY',		1).
valid_abstract_section_header('REGISTRATION NUMBER OF THE STUDY',		1).
valid_abstract_section_header('REGISTRATION NUMBER OF THE SYSTEMATIC REVIEW',		1).
valid_abstract_section_header('REGISTRATION OF PROTOCOL NUMBER',		1).
valid_abstract_section_header('REGISTRATION PROSPERO',		1).
valid_abstract_section_header('REGISTRATION TRIAL DATABASE',		1).
valid_abstract_section_header('REGISTRY DESIGN',		1).
valid_abstract_section_header('REGISTRY INFORMATION',		1).
valid_abstract_section_header('REGISTRY OF PROTOCOL',		1).
valid_abstract_section_header('REGRESSION OF ATHEROSCLEROSIS',		1).
valid_abstract_section_header('REGULATORY ACCOUNTABILITY',		1).
valid_abstract_section_header('REGULATORY OPTIONS',		1).
valid_abstract_section_header('REGULATORY PERSPECTIVES OF THE LEVELS OF TOXICITY',		1).
valid_abstract_section_header('REGULATORY REQUIREMENTS THERAPEUTIC EQUIVALENCE',		1).
valid_abstract_section_header('REHABILITATION EXERCISE PROGRAM',		1).
valid_abstract_section_header('REHABILITATION NEEDS IN OLDER CANCER SURVIVORS',		1).
valid_abstract_section_header('REHABILITATION PROGRAM',		1).
valid_abstract_section_header('REHABILITATION TECHNIQUES AFTER CHEST TRAUMA',		1).
valid_abstract_section_header('REIMBURSEMENT',		1).
valid_abstract_section_header('RELAPSE',		1).
valid_abstract_section_header('RELAPSE (INDUCTION)',		1).
valid_abstract_section_header('RELAPSE (POST REMISSION)',		1).
valid_abstract_section_header('RELATED FACTORS',		1).
valid_abstract_section_header('RELATED LJPC PAPERS',		1).
valid_abstract_section_header('RELATING ASSESSMENT TO THE CURRICULUM',		1).
valid_abstract_section_header('RELATIONAL DYNAMICS',		1).
valid_abstract_section_header('RELATIONS TO OTHER VARIABLES',		1).
valid_abstract_section_header('RELATIONSHIP',		1).
valid_abstract_section_header('RELATIVE PROGNOSTIC VALUES',		1).
valid_abstract_section_header('RELEVANCE AND CONCLUSION',		1).
valid_abstract_section_header('RELEVANCE FOR IMAGING STUDIES',		1).
valid_abstract_section_header('RELEVANCE OF DIAGNOSTIC CRITERIA FOR INDICATION',		1).
valid_abstract_section_header('RELEVANCE TO CLINIC PRACTICE',		1).
valid_abstract_section_header('RELEVANCE TO CLINICAL PRACTISE',		1).
valid_abstract_section_header('RELEVANCE TO CURRENT KNOWLEDGE',		1).
valid_abstract_section_header('RELEVANT DATA',		1).
valid_abstract_section_header('RELEVANT DISORDERS',		1).
valid_abstract_section_header('RELEVANT INFORMATION AND CONCLUSIONS',		1).
valid_abstract_section_header('RELEVANT INTERNATIONAL GUIDELINE',		1).
valid_abstract_section_header('RELEVANT LOCAL INJURY EPIDEMIOLOGY',		1).
valid_abstract_section_header('RELEVANT RESULTS',		1).
valid_abstract_section_header('RELEVANT STUDIES',		1).
valid_abstract_section_header('RELEVANT THEORIES',		1).
valid_abstract_section_header('RELEVANT TO PRACTICE',		1).
valid_abstract_section_header('RELIABILITY OF THE WATERLOW SCORE CLASSIC RESEARCH',		1).
valid_abstract_section_header('REMAINING QUESTIONS',		1).
valid_abstract_section_header('REMEMBER, REMEMBER, FIREWORK SAFETY',		1).
valid_abstract_section_header('REMITS',		1).
valid_abstract_section_header('REMOVING THE CAUSE',		1).
valid_abstract_section_header('RENAL ADRENOCEPTOR FUNCTION',		1).
valid_abstract_section_header('RENAL AND BLOOD PRESSURE EFFECTS',		1).
valid_abstract_section_header('RENAL CALCULOSIS AS A SIGN OF SARCOIDOSIS ACTIVITY',		1).
valid_abstract_section_header('RENAL DISEASES',		1).
valid_abstract_section_header('RENAL EFFECT',		1).
valid_abstract_section_header('RENIN INHIBITORS',		1).
valid_abstract_section_header('RENORENAL REFLEXES',		1).
valid_abstract_section_header('REPETITIVE STIMULATION',		1).
valid_abstract_section_header('REPLY',		1).
valid_abstract_section_header('REPORT OF PATIENT',		1).
valid_abstract_section_header('REPORT OF THE CASES',		1).
valid_abstract_section_header('REPORT OF THE OUTBREAK',		1).
valid_abstract_section_header('REPORTED EFFECTS AND TREATMENTS',		1).
valid_abstract_section_header('REPORTED RISK FACTORS',		1).
valid_abstract_section_header('REPORTING MDS QIS FOR QUALITY IMPROVEMENT',		1).
valid_abstract_section_header('REPORTING OF DATA',		1).
valid_abstract_section_header('REPORTING THE MEASURES',		1).
valid_abstract_section_header('REPRESENTATIVES OF THE SUBFAMILY BRUCHINAE (COLEOPTERA',		1).
valid_abstract_section_header('REPRODUCIBILITY',		1).
valid_abstract_section_header('REQUIRED EQUIPMENT',		1).
valid_abstract_section_header('REQUIREMENTS AND METHODS',		1).
valid_abstract_section_header('REQUIREMENTS FOR A POPULATION-LEVEL RESPONSE',		1).
valid_abstract_section_header('REQUIREMENTS FOR DATA ARCHITECTURE TO SUPPORT THE PATHWAYS COMMUNITY HUB MODEL',		1).
valid_abstract_section_header('RERULTS',		1).
valid_abstract_section_header('RESEARCH AND METHOD',		1).
valid_abstract_section_header('RESEARCH AND PROGRESS',		1).
valid_abstract_section_header('RESEARCH BACKGROUND',		1).
valid_abstract_section_header('RESEARCH CONTEXT AND DATA SOURCES',		1).
valid_abstract_section_header('RESEARCH DESIGN AND INDICATORS',		1).
valid_abstract_section_header('RESEARCH DESIGN AND STUDY SAMPLE',		1).
valid_abstract_section_header('RESEARCH DESIGN, METHODS, AND RESULTS',		1).
valid_abstract_section_header('RESEARCH DESIGN, METHODS, AND SUBJECTS',		1).
valid_abstract_section_header('RESEARCH DESIGN, PARTICIPANTS, AND CONTEXT',		1).
valid_abstract_section_header('RESEARCH DESIGN, SAMPLE, AND METHODS',		1).
valid_abstract_section_header('RESEARCH DESIGN, SUBJECTS, AND OUTCOME MEASURES',		1).
valid_abstract_section_header('RESEARCH DESIGN/SETTING',		1).
valid_abstract_section_header('RESEARCH DESIGN/SUBJECTS/MEASURES',		1).
valid_abstract_section_header('RESEARCH DEVELOPMENT',		1).
valid_abstract_section_header('RESEARCH GOAL',		1).
valid_abstract_section_header('RESEARCH GOALS WERE',		1).
valid_abstract_section_header('RESEARCH HIGHLIGHTS AND CONCLUSIONS',		1).
valid_abstract_section_header('RESEARCH HYPOTHESIS',		1).
valid_abstract_section_header('RESEARCH METHOD AND MATERIAL',		1).
valid_abstract_section_header('RESEARCH METHOD/ANALYSIS',		1).
valid_abstract_section_header('RESEARCH METHODOLOGY AND DESIGN',		1).
valid_abstract_section_header('RESEARCH METHODOLOGY/FINDINGS',		1).
valid_abstract_section_header('RESEARCH METHODS AND DESIGN',		1).
valid_abstract_section_header('RESEARCH METHODS/PATIENTS',		1).
valid_abstract_section_header('RESEARCH PARTICIPANTS',		1).
valid_abstract_section_header('RESEARCH PARTICIPANTS AND RESEARCH CONTEXT',		1).
valid_abstract_section_header('RESEARCH PARTNERSHIPS',		1).
valid_abstract_section_header('RESEARCH PLAN AND METHODS',		1).
valid_abstract_section_header('RESEARCH PROCEDURE',		1).
valid_abstract_section_header('RESEARCH PROCEDURES',		1).
valid_abstract_section_header('RESEARCH PROCEDURES AND RESULTS',		1).
valid_abstract_section_header('RESEARCH PROJECTS',		1).
valid_abstract_section_header('RESEARCH PURPOSES',		1).
valid_abstract_section_header('RESEARCH QUESTION AND OBJECTIVES',		1).
valid_abstract_section_header('RESEARCH QUESTION/AIM',		1).
valid_abstract_section_header('RESEARCH QUESTION/OBJECTIVE',		1).
valid_abstract_section_header('RESEARCH QUESTIONS AND METHODS',		1).
valid_abstract_section_header('RESEARCH SAMPLE AND METHODOLOGY',		1).
valid_abstract_section_header('RESEARCH STRATEGIES',		1).
valid_abstract_section_header('RESEARCH SUBJECTS AND DESIGN',		1).
valid_abstract_section_header('RESEARCH SUPPORT',		1).
valid_abstract_section_header('RESEARCH TYPE',		1).
valid_abstract_section_header('RESEARCH USE',		1).
valid_abstract_section_header('RESEARCH-ORIENTED RADIONUCLIDES',		1).
valid_abstract_section_header('RESIDENCY HISTORY',		1).
valid_abstract_section_header('RESISTANCE TO ACTIVATED PROTEIN C',		1).
valid_abstract_section_header('RESISTANCE TO ANTIBIOTIC DRUGS',		1).
valid_abstract_section_header('RESISTANCE TO SAQUINAVIR',		1).
valid_abstract_section_header('RESOLUSIONS',		1).
valid_abstract_section_header('RESOURCE',		1).
valid_abstract_section_header('RESPIRATORY REHABILITATION',		1).
valid_abstract_section_header('RESPONDENTS AND METHODS',		1).
valid_abstract_section_header('RESPONDING TO NEWBORN FALLS',		1).
valid_abstract_section_header('RESPONSE AND CONCLUSIONS',		1).
valid_abstract_section_header('RESPONSE ASSESSMENT',		1).
valid_abstract_section_header('RESPONSE AT A CONCEPTUAL LEVEL',		1).
valid_abstract_section_header('RESPONSE AT A CURRICULAR LEVEL',		1).
valid_abstract_section_header('RESPONSE FORMATS',		1).
valid_abstract_section_header('RESPONSE PROCESS',		1).
valid_abstract_section_header('RESPONSE TO GLUTAMATE',		1).
valid_abstract_section_header('RESPONSE TO REVIEWERS',		1).
valid_abstract_section_header('RESPONSE TO STRYCHNINE',		1).
valid_abstract_section_header('RESPONSE TO SUBSTANCE P AND PHYSALAEMIN',		1).
valid_abstract_section_header('RESPONSES TO SUPPLEMENTATION',		1).
valid_abstract_section_header('RESPONSIBILITIES',		1).
valid_abstract_section_header('RESPONSIBILITY FOR HEALTH',		1).
valid_abstract_section_header('RESPONSIBILITY OF THE PHYSICIAN',		1).
valid_abstract_section_header('RESPONSIBILITY OF THE SHIP CAPTAIN',		1).
valid_abstract_section_header('RESTRICTING ACCESS BY CHILDREN AND TEENAGERS',		1).
valid_abstract_section_header('RESTRICTING ADVERTISING AND PROMOTION',		1).
valid_abstract_section_header('RESTULTS',		1).
valid_abstract_section_header('RESULS',		1).
valid_abstract_section_header('RESULT & CONCLUSIONS',		1).
valid_abstract_section_header('RESULT 1',		1).
valid_abstract_section_header('RESULT AND FINDING',		1).
valid_abstract_section_header('RESULT AND IMPACT ON INDUSTRY AND GOVERNMENT',		1).
valid_abstract_section_header('RESULT AND PERSPECTIVE',		1).
valid_abstract_section_header('RESULT OF THE INTERPRETATION',		1).
valid_abstract_section_header('RESULTADOS',		1).
valid_abstract_section_header('RESULTADOS Y CONCLUSIONES',		1).
valid_abstract_section_header('RESULTANTS',		1).
valid_abstract_section_header('RESULTING HYPOTHESES',		1).
valid_abstract_section_header('RESULTS',		1).
valid_abstract_section_header('RESULTS & IMPLICATIONS',		1).
valid_abstract_section_header('RESULTS (CLINICAL OBSERVATIONS)',		1).
valid_abstract_section_header('RESULTS (DEVELOPMENT STUDY)',		1).
valid_abstract_section_header('RESULTS (FEMALE STERILIZATION)',		1).
valid_abstract_section_header('RESULTS (MALE STERILIZATION)',		1).
valid_abstract_section_header('RESULTS (MEANS AND STANDARD DEVIATIONS)',		1).
valid_abstract_section_header('RESULTS (MEDIAN VALUES)',		1).
valid_abstract_section_header('RESULTS (PROGRESS)',		1).
valid_abstract_section_header('RESULTS (TABLE I)',		1).
valid_abstract_section_header('RESULTS (VALIDATION STUDY)',		1).
valid_abstract_section_header('RESULTS - CONCLUSIONS',		1).
valid_abstract_section_header('RESULTS / CONCLUSION',		1).
valid_abstract_section_header('RESULTS AFFECTED HEMISPHERE',		1).
valid_abstract_section_header('RESULTS AND ANALYSES',		1).
valid_abstract_section_header('RESULTS AND CASE STUDIES',		1).
valid_abstract_section_header('RESULTS AND COMPARISON OF METHODS',		1).
valid_abstract_section_header('RESULTS AND COMPARISON WITH OTHER METHODS',		1).
valid_abstract_section_header('RESULTS AND COMPARISONS WITH EXISTING METHODS',		1).
valid_abstract_section_header('RESULTS AND CONCLUTIONS',		1).
valid_abstract_section_header('RESULTS AND CONLCLUSIONS',		1).
valid_abstract_section_header('RESULTS AND CONTRIBUTION',		1).
valid_abstract_section_header('RESULTS AND DISCUSSING',		1).
valid_abstract_section_header('RESULTS AND FINAL CONSIDERATIONS',		1).
valid_abstract_section_header('RESULTS AND GENERAL SIGNIFICANCE',		1).
valid_abstract_section_header('RESULTS AND HYPOTHESIS',		1).
valid_abstract_section_header('RESULTS AND IMPACT',		1).
valid_abstract_section_header('RESULTS AND IMPLEMENTATION',		1).
valid_abstract_section_header('RESULTS AND INCLUDED ARTICLES\' CHARACTERISTICS',		1).
valid_abstract_section_header('RESULTS AND INFERENCES',		1).
valid_abstract_section_header('RESULTS AND KEY ACTIVITIES',		1).
valid_abstract_section_header('RESULTS AND MAIN FINDINGS',		1).
valid_abstract_section_header('RESULTS AND METHODOLOGY',		1).
valid_abstract_section_header('RESULTS AND OUTCOME',		1).
valid_abstract_section_header('RESULTS AND PRACTICE IMPLICATIONS',		1).
valid_abstract_section_header('RESULTS AND PRESENTATION OF CASE',		1).
valid_abstract_section_header('RESULTS AND PRINCIPAL FINDINGS',		1).
valid_abstract_section_header('RESULTS AND REFLECTION',		1).
valid_abstract_section_header('RESULTS AND RELEVANCE',		1).
valid_abstract_section_header('RESULTS AND SIGNIFICANCE OF THE STUDY',		1).
valid_abstract_section_header('RESULTS AND STATE OF AFFAIRS',		1).
valid_abstract_section_header('RESULTS AND STATISTICAL ANALYSIS',		1).
valid_abstract_section_header('RESULTS AND STUDIES',		1).
valid_abstract_section_header('RESULTS AND STUDY DESIGN',		1).
valid_abstract_section_header('RESULTS AND STUDY LIMITATION',		1).
valid_abstract_section_header('RESULTS AND STUDY LIMITATIONS',		1).
valid_abstract_section_header('RESULTS AND SURGICAL TECHNIQUES',		1).
valid_abstract_section_header('RESULTS AND/OR CONCLUSION',		1).
valid_abstract_section_header('RESULTS ARE AS FOLLOWS',		1).
valid_abstract_section_header('RESULTS AT BWH',		1).
valid_abstract_section_header('RESULTS B',		1).
valid_abstract_section_header('RESULTS BM SOUP',		1).
valid_abstract_section_header('RESULTS CD-C',		1).
valid_abstract_section_header('RESULTS COMPARED WITH EXISTING METHODS',		1).
valid_abstract_section_header('RESULTS CONCLUSION AND SIGNIFICANCE',		1).
valid_abstract_section_header('RESULTS CONCLUSIONS',		1).
valid_abstract_section_header('RESULTS DISCUSSION',		1).
valid_abstract_section_header('RESULTS ESEM',		1).
valid_abstract_section_header('RESULTS ET',		1).
valid_abstract_section_header('RESULTS FAS',		1).
valid_abstract_section_header('RESULTS FIELD BASED',		1).
valid_abstract_section_header('RESULTS IN COMPARISON WITH EXISTING METHODS',		1).
valid_abstract_section_header('RESULTS IN TERMS OF MORBIDITY',		1).
valid_abstract_section_header('RESULTS IN TERMS OF PSA MONITORING',		1).
valid_abstract_section_header('RESULTS IN THE INVESTIGATED AREAS',		1).
valid_abstract_section_header('RESULTS INCLUDING KM CHALLENGES AND RESPONSE',		1).
valid_abstract_section_header('RESULTS INSOMNIA',		1).
valid_abstract_section_header('RESULTS INTERPRETATION',		1).
valid_abstract_section_header('RESULTS OBTAINED',		1).
valid_abstract_section_header('RESULTS OF ANALYSIS AND DISCUSSION',		1).
valid_abstract_section_header('RESULTS OF APPRAISAL',		1).
valid_abstract_section_header('RESULTS OF BEST-CASE SCENARIO',		1).
valid_abstract_section_header('RESULTS OF CLINICAL STUDY',		1).
valid_abstract_section_header('RESULTS OF CLINICAL TRIALS',		1).
valid_abstract_section_header('RESULTS OF DATA SURVEY',		1).
valid_abstract_section_header('RESULTS OF FUNCTIONAL ELECTRICAL THERAPY STUDIES',		1).
valid_abstract_section_header('RESULTS OF INITIAL EVALUATION OF GOAL QUALITY',		1).
valid_abstract_section_header('RESULTS OF INVESTIGATIONS',		1).
valid_abstract_section_header('RESULTS OF RECENT STUDIES',		1).
valid_abstract_section_header('RESULTS OF THE FEASIBILITY STUDY',		1).
valid_abstract_section_header('RESULTS OF THE GUIDING QUESTIONS',		1).
valid_abstract_section_header('RESULTS OF THE LITERATURE SEARCH',		1).
valid_abstract_section_header('RESULTS OF THE PRECLINICAL TRIALS OF RECOMBINANT THROMBOPOIETIN',		1).
valid_abstract_section_header('RESULTS OF THE REVIEW',		1).
valid_abstract_section_header('RESULTS OF TREATMENT',		1).
valid_abstract_section_header('RESULTS ON DIETARY FAT',		1).
valid_abstract_section_header('RESULTS ONCOLOGIC',		1).
valid_abstract_section_header('RESULTS OVER ONE YEAR',		1).
valid_abstract_section_header('RESULTS PART I',		1).
valid_abstract_section_header('RESULTS PART II',		1).
valid_abstract_section_header('RESULTS PRIMARY OUTCOME',		1).
valid_abstract_section_header('RESULTS SINGLE TWITCHES',		1).
valid_abstract_section_header('RESULTS STUDY 1',		1).
valid_abstract_section_header('RESULTS SYSTEMATIC REVIEW',		1).
valid_abstract_section_header('RESULTS THE HCV GENOTYPE DISTRIBUTION WAS AS FOLLOWS',		1).
valid_abstract_section_header('RESULTS VEGF-A',		1).
valid_abstract_section_header('RESULTS\' CONCLUSIONS',		1).
valid_abstract_section_header('RESULTS, COMMENTS AND PROPOSAL',		1).
valid_abstract_section_header('RESULTS, COMPARISON WITH EXISTING METHOD(S) AND CONCLUSIONS',		1).
valid_abstract_section_header('RESULTS, CONCLUSION AND SIGNIFICANCE',		1).
valid_abstract_section_header('RESULTS, CONCLUSIONS, AND RECOMMENDATIONS',		1).
valid_abstract_section_header('RESULTS, DISCUSSION AND IMPACT ON INDUSTRY',		1).
valid_abstract_section_header('RESULTS, DISCUSSION, CONCLUSION',		1).
valid_abstract_section_header('RESULTS, DISCUSSION, CONCLUSIONS',		1).
valid_abstract_section_header('RESULTS, SYSTEMATIC ANALYSIS',		1).
valid_abstract_section_header('RESULTS, THE MAIN FINDINGS',		1).
valid_abstract_section_header('RESULTS-',		1).
valid_abstract_section_header('RESULTS-DESIGN AND ANALYSIS CONSIDERATIONS',		1).
valid_abstract_section_header('RESULTS-REVIEW OF EVIDENCE',		1).
valid_abstract_section_header('RESULTS.',		1).
valid_abstract_section_header('RESULTS/ CASE REPORT',		1).
valid_abstract_section_header('RESULTS/ DISCUSSION',		1).
valid_abstract_section_header('RESULTS/CASE REPORTS',		1).
valid_abstract_section_header('RESULTS/COMMENTS',		1).
valid_abstract_section_header('RESULTS/COMPARISON',		1).
valid_abstract_section_header('RESULTS/COMPARISON WITH EXISTING METHODS',		1).
valid_abstract_section_header('RESULTS/DISCUSSION/CONCLUSION',		1).
valid_abstract_section_header('RESULTS/DISSCUSSION',		1).
valid_abstract_section_header('RESULTS/SYNTHESIS',		1).
valid_abstract_section_header('RESULTSS',		1).
valid_abstract_section_header('RESUMEN DE LOS ANTECEDENTES',		1).
valid_abstract_section_header('RESUMEN OBJETIVO',		1).
valid_abstract_section_header('RESYLTS',		1).
valid_abstract_section_header('RETINOPATHY',		1).
valid_abstract_section_header('RETRANSPLANTATION',		1).
valid_abstract_section_header('RETROGRADE UROGRAPHY',		1).
valid_abstract_section_header('RETROSPECTIVE COHORT STUDY',		1).
valid_abstract_section_header('RETROSPECTIVE READING STUDY',		1).
valid_abstract_section_header('RETROSPECTIVE REVIEW',		1).
valid_abstract_section_header('RETURN TO CLASS',		1).
valid_abstract_section_header('RETURN TO PLAY',		1).
valid_abstract_section_header('REVAMPING, REPOSITIONING, AND RENAMING HEALTH SERVICES RESEARCH',		1).
valid_abstract_section_header('REVASCULARIZATION RESULTS',		1).
valid_abstract_section_header('REVIEW AIM',		1).
valid_abstract_section_header('REVIEW AND ANALYSIS',		1).
valid_abstract_section_header('REVIEW AND CONCLUSIONS',		1).
valid_abstract_section_header('REVIEW DESIGN METHODS',		1).
valid_abstract_section_header('REVIEW METHOD USED/DATA SOURCES',		1).
valid_abstract_section_header('REVIEW METHODS/INTERVENTION',		1).
valid_abstract_section_header('REVIEW OBJECTIVE/QUESTION',		1).
valid_abstract_section_header('REVIEW OF CLINICAL EVIDENCE',		1).
valid_abstract_section_header('REVIEW OF CURRENT LITERATURE RESULTS',		1).
valid_abstract_section_header('REVIEW OF DATA',		1).
valid_abstract_section_header('REVIEW OF EVIDENCE',		1).
valid_abstract_section_header('REVIEW OF LITERATURE AND CONCLUSION',		1).
valid_abstract_section_header('REVIEW OF LITERATURE AND HYPOTHESIS GENERATING FINDINGS',		1).
valid_abstract_section_header('REVIEW OF MAIN RESULTS OF THE BASEL SSI COHORT STUDY',		1).
valid_abstract_section_header('REVIEW OF OPEN FRACTURE MANAGEMENT',		1).
valid_abstract_section_header('REVIEW OF PSYCHOPHYSICAL LITERATURE',		1).
valid_abstract_section_header('REVIEW OF RESEARCH',		1).
valid_abstract_section_header('REVIEW OF THERAPEUTIC STRATEGIES IN PRECLINICAL AND CLINICAL TRIALS',		1).
valid_abstract_section_header('REVIEW OF VANCOMYCIN DOSING',		1).
valid_abstract_section_header('REVIEW OUTLINE',		1).
valid_abstract_section_header('REVIEW PROTOCOL',		1).
valid_abstract_section_header('REVIEW PROTOCOL NUMBER',		1).
valid_abstract_section_header('REVIEW SCOPE',		1).
valid_abstract_section_header('REVIEW STUDY',		1).
valid_abstract_section_header('REVIEW/FINDINGS',		1).
valid_abstract_section_header('REVIEW/RESEARCH METHODS',		1).
valid_abstract_section_header('REVIEWERS\' REPORT',		1).
valid_abstract_section_header('REVIEWERS\' REPORTS',		1).
valid_abstract_section_header('REVIEWS',		1).
valid_abstract_section_header('REVISION',		1).
valid_abstract_section_header('REVISION FOR FAILURE',		1).
valid_abstract_section_header('REVUE',		1).
valid_abstract_section_header('REWARDING PARTICIPATING PHYSICIANS',		1).
valid_abstract_section_header('REZULTATY',		1).
valid_abstract_section_header('RFA OF IPVS',		1).
valid_abstract_section_header('RFA VERSUS SURGERY',		1).
valid_abstract_section_header('RHINOSINUSOPHARYNGITIS IN CHILDREN',		1).
valid_abstract_section_header('RIASSUNTO',		1).
valid_abstract_section_header('RIPC I/R',		1).
valid_abstract_section_header('RISK ANALYSIS',		1).
valid_abstract_section_header('RISK ASSESSMENT',		1).
valid_abstract_section_header('RISK FACTORS AND OUTCOME',		1).
valid_abstract_section_header('RISK FACTORS FOR DEVELOPING TUBERCULOSIS',		1).
valid_abstract_section_header('RISK FACTORS TESTED',		1).
valid_abstract_section_header('RISK IDENTIFICATION AND PROACTIVE RISK ASSESSMENT',		1).
valid_abstract_section_header('RISK MITIGATION',		1).
valid_abstract_section_header('RISK OF CANCER',		1).
valid_abstract_section_header('RISK OF COMPLICATIONS',		1).
valid_abstract_section_header('RISK OF INSTITUTIONALIZATION',		1).
valid_abstract_section_header('RISK PRIORITIZATION',		1).
valid_abstract_section_header('RISK STRATIFICATION FOR SEXUAL ACTIVITY DEPENDING ON THE CLINICAL STATUS OF HEART DISEASE',		1).
valid_abstract_section_header('RISK-STRATIFICATION',		1).
valid_abstract_section_header('RISKS FACED BY STAFF',		1).
valid_abstract_section_header('RISULTATI E DISCUSSIONE',		1).
valid_abstract_section_header('RNA MARKERS IN URINE',		1).
valid_abstract_section_header('ROBOTICS IN MEDICAL REHABILITATION',		1).
valid_abstract_section_header('ROLE IN UROLOGICAL PRACTICE',		1).
valid_abstract_section_header('ROLE OF ASBESTOS BODIES',		1).
valid_abstract_section_header('ROLE OF ECHOCARDIOGRAPHY',		1).
valid_abstract_section_header('ROLE OF FIBER ANALYSIS',		1).
valid_abstract_section_header('ROLE OF GP',		1).
valid_abstract_section_header('ROLE OF NATRIURETIC PEPTIDE IN PATIENTS WITH DISPNEA',		1).
valid_abstract_section_header('ROLE OF PRIMARY DATA COLLECTION',		1).
valid_abstract_section_header('ROLE OF PROSTANOIDS IN THE CLINICAL EXPRESSION OF ALLERGIC RHINITIS',		1).
valid_abstract_section_header('ROLE OF THE CAPSULE',		1).
valid_abstract_section_header('ROLES',		1).
valid_abstract_section_header('ROOT CANAL PREPARATION',		1).
valid_abstract_section_header('ROOT CAUSE ANALYSIS (RCA)',		1).
valid_abstract_section_header('ROTAROD TEST',		1).
valid_abstract_section_header('ROUNDTABLE FINDINGS',		1).
valid_abstract_section_header('ROUTINE CARE',		1).
valid_abstract_section_header('RRESULTS',		1).
valid_abstract_section_header('RUNNING TIME',		1).
valid_abstract_section_header('RUNNING TITLE',		1).
valid_abstract_section_header('RUSULTS',		1).
valid_abstract_section_header('Recommendation 1',		1).
valid_abstract_section_header('Recommendation 2',		1).
valid_abstract_section_header('Recommendation 3',		1).
valid_abstract_section_header('Recommendation 4',		1).
valid_abstract_section_header('Research strategies',		1).
valid_abstract_section_header('Research strategy',		1).
valid_abstract_section_header('Result:',		1).
valid_abstract_section_header('Resultado',		1).
valid_abstract_section_header('Resultado:',		1).
valid_abstract_section_header('Resultados y debate',		1).
valid_abstract_section_header('Results and Conclusion',		1).
valid_abstract_section_header('Results and Conclusions:',		1).
valid_abstract_section_header('S CANIS INFECTION',		1).
valid_abstract_section_header('S EQUI SUBSP ZOOEPIDEMICUS INFECTION',		1).
valid_abstract_section_header('SAFE HANDLING OF CYTOTOXICS IN ISRAEL',		1).
valid_abstract_section_header('SAFETY ANALOGS',		1).
valid_abstract_section_header('SAFETY AND EFFECTIVENESS OF SURGICAL OPTIONS',		1).
valid_abstract_section_header('SAFETY AND HYPERGLYCEMIA',		1).
valid_abstract_section_header('SAFETY AND TOXICITY',		1).
valid_abstract_section_header('SAFETY OPTIMIZER',		1).
valid_abstract_section_header('SAFETY PROFILE OF SILDENAFIL',		1).
valid_abstract_section_header('SAFETY RESULTS',		1).
valid_abstract_section_header('SALB RESULTS',		1).
valid_abstract_section_header('SALIVARY HORMONE TESTS',		1).
valid_abstract_section_header('SALUS',		1).
valid_abstract_section_header('SAMENVATTING',		1).
valid_abstract_section_header('SAMPLE & SETTING',		1).
valid_abstract_section_header('SAMPLE AND ELIGIBILITY',		1).
valid_abstract_section_header('SAMPLE CHARACTERISTICS',		1).
valid_abstract_section_header('SAMPLE EVIDENCE',		1).
valid_abstract_section_header('SAMPLE SELECTION',		1).
valid_abstract_section_header('SAMPLE SIZE AND DESIGN',		1).
valid_abstract_section_header('SAMPLE SIZE AND STATISTICAL METHODS',		1).
valid_abstract_section_header('SAMPLE SIZE CALCULATION',		1).
valid_abstract_section_header('SAMPLE SIZE FORMULAS',		1).
valid_abstract_section_header('SAMPLES & METHODS',		1).
valid_abstract_section_header('SAMPLING METHOD',		1).
valid_abstract_section_header('SAMPLING RESULTS',		1).
valid_abstract_section_header('SAMPLING/METHODS',		1).
valid_abstract_section_header('SC SUV',		1).
valid_abstract_section_header('SCANNING TECHNIQUE',		1).
valid_abstract_section_header('SCAR TISSUE',		1).
valid_abstract_section_header('SCHEDULING',		1).
valid_abstract_section_header('SCHISANDRAE FRUCTUS (SF), WHICH POSSESSES FIVE TASTES',		1).
valid_abstract_section_header('SCHOLASTIC ACHIEVEMENT',		1).
valid_abstract_section_header('SCHT GROUP',		1).
valid_abstract_section_header('SCIENTFIC SIGNIFICANCE',		1).
valid_abstract_section_header('SCIENTIFIC APPROACH',		1).
valid_abstract_section_header('SCIENTIFIC HYPOTHESIS OF THE STUDY',		1).
valid_abstract_section_header('SCIENTIFIC QUESTION AND AIMS OF THE STUDY',		1).
valid_abstract_section_header('SCIENTIFIC RESULTS',		1).
valid_abstract_section_header('SCLEROTHERAPY',		1).
valid_abstract_section_header('SCOOP OF REVIEW',		1).
valid_abstract_section_header('SCOPE AND CONCLUSION',		1).
valid_abstract_section_header('SCOPE AND CONTENTS OF REVIEW',		1).
valid_abstract_section_header('SCOPE AND METHOD OF REVIEW',		1).
valid_abstract_section_header('SCOPE AND OBJECTIVES',		1).
valid_abstract_section_header('SCOPE AND RESULT',		1).
valid_abstract_section_header('SCOPE AND RESULTS',		1).
valid_abstract_section_header('SCOPE ICU PATIENTS',		1).
valid_abstract_section_header('SCOPE OF SIMULATION',		1).
valid_abstract_section_header('SCOPE OF THE REPORT',		1).
valid_abstract_section_header('SCOPE, SOURCES USED',		1).
valid_abstract_section_header('SCOPES',		1).
valid_abstract_section_header('SCOPING REVIEW QUESTION/OBJECTIVE',		1).
valid_abstract_section_header('SCOPO E RAZIONALE',		1).
valid_abstract_section_header('SCREENING FOR FRAILTY',		1).
valid_abstract_section_header('SCREENING FOR THE STATE OF MAHARASHTRA',		1).
valid_abstract_section_header('SCS ON',		1).
valid_abstract_section_header('SD/PD',		1).
valid_abstract_section_header('SEARCH AND RESULTS',		1).
valid_abstract_section_header('SEARCH AND REVIEW METHODOLOGY',		1).
valid_abstract_section_header('SEARCH AND SELECTION OF LITERATURE',		1).
valid_abstract_section_header('SEARCH FOR THE PRIMARY CANCER',		1).
valid_abstract_section_header('SEARCH FOR WEB SITES ON QUALITY CARE',		1).
valid_abstract_section_header('SEARCH METHODS USED',		1).
valid_abstract_section_header('SEARCH PERIOD',		1).
valid_abstract_section_header('SEARCH RESULT AND QUALITY ASSESSMENT',		1).
valid_abstract_section_header('SEARCH RESULTS',		1).
valid_abstract_section_header('SEARCH STRATEGIES AND DESIGN',		1).
valid_abstract_section_header('SEARCH STRATEGY & SELECTION CRITERIA',		1).
valid_abstract_section_header('SEARCH STRATEGY AND DATA COLLECTION',		1).
valid_abstract_section_header('SEARCH STRATEGY AND EVALUATION',		1).
valid_abstract_section_header('SEARCH STRATEGY AND EVALUATION METHOD',		1).
valid_abstract_section_header('SEARCH STRATEGY AND INCLUSION CRITERIA',		1).
valid_abstract_section_header('SEARCH STRATEGY AND METHODOLOGY',		1).
valid_abstract_section_header('SEARCH STRATEGY AND SOURCES',		1).
valid_abstract_section_header('SEARCH STRATEGY AND SYNTHESIS',		1).
valid_abstract_section_header('SEARCH STRATEGY AND TYPE OF REVIEW',		1).
valid_abstract_section_header('SEARCH STRATEGY, INCLUSION AND EXCLUSION CRITERIA',		1).
valid_abstract_section_header('SEARCH STRATEGY-',		1).
valid_abstract_section_header('SEARCH STRATEGY/SELECTION CRITERIA/DATA COLLECTION AND ANALYSIS',		1).
valid_abstract_section_header('SEARCHING AND SUMMARIZING THE LITERATURE',		1).
valid_abstract_section_header('SEARCHING FOR A NEW PROCESS',		1).
valid_abstract_section_header('SEARCHING METHOD',		1).
valid_abstract_section_header('SECOND HARMONIC IMAGING',		1).
valid_abstract_section_header('SECONDARY AIMS',		1).
valid_abstract_section_header('SECONDARY CATHETER ABLATION',		1).
valid_abstract_section_header('SECONDARY END-POINTS',		1).
valid_abstract_section_header('SECONDARY HYPOTHESIS',		1).
valid_abstract_section_header('SECONDARY INDEPENDENT VARIABLES',		1).
valid_abstract_section_header('SECONDARY LEUKEMIAS',		1).
valid_abstract_section_header('SECONDARY MEASURE',		1).
valid_abstract_section_header('SECONDARY OUTCOME',		1).
valid_abstract_section_header('SECONDARY OUTCOME PARAMETERS',		1).
valid_abstract_section_header('SECONDARY OUTCOMES MEASURES',		1).
valid_abstract_section_header('SECONDARY QUESTIONS',		1).
valid_abstract_section_header('SECONDARY RESULTS',		1).
valid_abstract_section_header('SECONDARY SUBJECT HEADING',		1).
valid_abstract_section_header('SECONDARY TREATMENT',		1).
valid_abstract_section_header('SECRETORY ACTIVITY OF HYPOTHALAMIC NEURONS',		1).
valid_abstract_section_header('SECTION',		1).
valid_abstract_section_header('SECTION TITLE',		1).
valid_abstract_section_header('SEDATION',		1).
valid_abstract_section_header('SEDATIVES AND HYPNOTICS',		1).
valid_abstract_section_header('SEE RELATED RESEARCH',		1).
valid_abstract_section_header('SEE RELATED RESEARCH ARTICLE',		1).
valid_abstract_section_header('SEE RELATED RESEARCH ARTICLES',		1).
valid_abstract_section_header('SEEKING INPUT FOR THE BUILDING DESIGN',		1).
valid_abstract_section_header('SELECION OF STUDIES',		1).
valid_abstract_section_header('SELECTED CASE STUDIES',		1).
valid_abstract_section_header('SELECTION CRITERIA-',		1).
valid_abstract_section_header('SELECTION OF PUBLICATIONS',		1).
valid_abstract_section_header('SELECTION OF RECOMMENDATIONS',		1).
valid_abstract_section_header('SELECTION OF STUDIES AND DATA EXTRACTION',		1).
valid_abstract_section_header('SELECTION OF STUDY',		1).
valid_abstract_section_header('SELECTION OF THE MATERIAL',		1).
valid_abstract_section_header('SELECTION/ELIGIBILITY CRITERIA',		1).
valid_abstract_section_header('SELECTIVITY TEST',		1).
valid_abstract_section_header('SELENIUM',		1).
valid_abstract_section_header('SELF-THEORIES',		1).
valid_abstract_section_header('SENSITIVITY ANALYSIS',		1).
valid_abstract_section_header('SENTENCE',		1).
valid_abstract_section_header('SENTENCE SUMMARY',		1).
valid_abstract_section_header('SENTENCE SUMMARY FOR THE TABLE OF CONTENTS',		1).
valid_abstract_section_header('SENTINEL LYMPH NODE MAPPING',		1).
valid_abstract_section_header('SEPARATION OF BLOOD SUPPLY',		1).
valid_abstract_section_header('SEQUELAE OF CENTRAL NERVOUS SYSTEM METASTASES AND THEIR TREATMENTS',		1).
valid_abstract_section_header('SEQUENCE ACCESSION NUMBERS',		1).
valid_abstract_section_header('SEQUENCE ACCESSIONS',		1).
valid_abstract_section_header('SEQUENCE OF EVENTS',		1).
valid_abstract_section_header('SERBIA',		1).
valid_abstract_section_header('SERIAL BONE MINERAL DENSITY TESTING IN PEOPLE RECEIVING OSTEOPOROSIS THERAPY',		1).
valid_abstract_section_header('SERINE PROTEASE ACTIVITY',		1).
valid_abstract_section_header('SEROLOGIC CELIAC DISEASE TESTS EVALUATED',		1).
valid_abstract_section_header('SEROLOGIC TESTING IN THE DIAGNOSIS CELIAC DISEASE',		1).
valid_abstract_section_header('SERONEGATIVE FORMS',		1).
valid_abstract_section_header('SERUM CONCENTRATIONS OF SAA (P',		1).
valid_abstract_section_header('SERUM MARKERS',		1).
valid_abstract_section_header('SERVER HOMEPAGE',		1).
valid_abstract_section_header('SERVICE',		1).
valid_abstract_section_header('SERVICE RECOVERY',		1).
valid_abstract_section_header('SESSION A',		1).
valid_abstract_section_header('SESSION B',		1).
valid_abstract_section_header('SESSION C',		1).
valid_abstract_section_header('SET OF PATIENTS AND METHODOLOGY',		1).
valid_abstract_section_header('SETTIGS',		1).
valid_abstract_section_header('SETTING & METHODS',		1).
valid_abstract_section_header('SETTING & SAMPLE',		1).
valid_abstract_section_header('SETTING ANALYSIS',		1).
valid_abstract_section_header('SETTING AND',		1).
valid_abstract_section_header('SETTING AND DATA SOURCE',		1).
valid_abstract_section_header('SETTING AND MEASUREMENTS',		1).
valid_abstract_section_header('SETTING AND PATIENT',		1).
valid_abstract_section_header('SETTING AND PROCEDURE',		1).
valid_abstract_section_header('SETTING AND SAMPLES',		1).
valid_abstract_section_header('SETTING AND SCOPE',		1).
valid_abstract_section_header('SETTING E PARTICIPANTS',		1).
valid_abstract_section_header('SETTING ICU SUBJECTS',		1).
valid_abstract_section_header('SETTING N/A PATIENTS',		1).
valid_abstract_section_header('SETTING PACE PARTICIPANTS',		1).
valid_abstract_section_header('SETTING PARTICIPANTS',		1).
valid_abstract_section_header('SETTING PARTICIPANTS AND MAIN MEASURES',		1).
valid_abstract_section_header('SETTING PATIENTS/PARTICIPANTS',		1).
valid_abstract_section_header('SETTING UK INTERVENTION',		1).
valid_abstract_section_header('SETTING UK SAMPLE',		1).
valid_abstract_section_header('SETTING USA',		1).
valid_abstract_section_header('SETTING, AND PARTICIPANTS',		1).
valid_abstract_section_header('SETTING, DESIGN AND PARTICIPANTS',		1).
valid_abstract_section_header('SETTING, DESIGN, AND PARTICIPANTS',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS AND DESIGN',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS AND INTERVENTION',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS AND MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS AND METHODS',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS AND OUTCOMES',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS, AND DESCRIPTION',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS, AND INTERVENTIONS',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS, AND MEASURES',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS, AND METHODS',		1).
valid_abstract_section_header('SETTING, PARTICIPANTS, INTERVENTIONS AND OUTCOMES',		1).
valid_abstract_section_header('SETTING, POPULATION, & INTERVENTION',		1).
valid_abstract_section_header('SETTING, SUBJECTS & INTERVENTIONS',		1).
valid_abstract_section_header('SETTING-INTERVENTIONS',		1).
valid_abstract_section_header('SETTING/CASES',		1).
valid_abstract_section_header('SETTING/CONTEXT',		1).
valid_abstract_section_header('SETTING/DATA SOURCE',		1).
valid_abstract_section_header('SETTING/DATA SOURCES',		1).
valid_abstract_section_header('SETTING/MEASUREMENTS',		1).
valid_abstract_section_header('SETTING/PARTICIPANTS/INTERVENTIONS',		1).
valid_abstract_section_header('SETTING/PERIOD',		1).
valid_abstract_section_header('SETTING/PRACTICE DESCRIPTION',		1).
valid_abstract_section_header('SETTING/STUDY PARTICIPANTS',		1).
valid_abstract_section_header('SETTING/SUBJECTS/METHODS',		1).
valid_abstract_section_header('SETTING/VENUE',		1).
valid_abstract_section_header('SETTINGAND PARTICIPANTS',		1).
valid_abstract_section_header('SETTINGS AND DATA',		1).
valid_abstract_section_header('SETTINGS AND DEIGN',		1).
valid_abstract_section_header('SETTINGS AND DESIGN & MATERIALS AND METHODS',		1).
valid_abstract_section_header('SETTINGS AND DESIGN MATERIAL AND METHODS',		1).
valid_abstract_section_header('SETTINGS AND DESIGN/MATERIALS AND METHODS',		1).
valid_abstract_section_header('SETTINGS AND OBJECTIVES',		1).
valid_abstract_section_header('SETTINGS AND SAMPLE POPULATION',		1).
valid_abstract_section_header('SETTINGS AND STUDY DESIGN',		1).
valid_abstract_section_header('SETTINGS AND TRIAL DESIGN',		1).
valid_abstract_section_header('SETTINGS DESIGN',		1).
valid_abstract_section_header('SETTINGS, DESIGN, METHODS, AND MATERIAL',		1).
valid_abstract_section_header('SETTINGS, PARTICIPANTS, AND METHODS',		1).
valid_abstract_section_header('SEVEN FLOW STRATEGIES, WITH A FOCUS ON THE INITIAL APPOINTMENT',		1).
valid_abstract_section_header('SEVERAL BASIC CONDITIONS',		1).
valid_abstract_section_header('SEVERE PROGNOSIS',		1).
valid_abstract_section_header('SEVERELY IMPAIRED CIRCULATION',		1).
valid_abstract_section_header('SEVERITY OF THE INFECTIONS',		1).
valid_abstract_section_header('SEXUAL ABUSE',		1).
valid_abstract_section_header('SEXUALITY AND HORMONE REPLACEMENT THERAPY',		1).
valid_abstract_section_header('SFONDO',		1).
valid_abstract_section_header('SHARING INFORMATION IN A COMPETITIVE ENVIRONMENT',		1).
valid_abstract_section_header('SHORT STATEMENT',		1).
valid_abstract_section_header('SHORT SUMMARY (FOR ANNOTATED TABLE OF CONTENTS)',		1).
valid_abstract_section_header('SHRINKING PLANET',		1).
valid_abstract_section_header('SICK BEHAVIOR',		1).
valid_abstract_section_header('SIDELINE EVALUATION AND MANAGEMENT',		1).
valid_abstract_section_header('SIGNED INFORMED CONSENT FORMS',		1).
valid_abstract_section_header('SIGNIFICANCE AND IMPACT OF THIS STUDY',		1).
valid_abstract_section_header('SIGNIFICANCE AND INNOVATION',		1).
valid_abstract_section_header('SIGNIFICANCE AND RECOMMENDATIONS',		1).
valid_abstract_section_header('SIGNIFICANCE AND SCOPE',		1).
valid_abstract_section_header('SIGNIFICANCE AND SOME POTENTIAL APPLICATIONS',		1).
valid_abstract_section_header('SIGNIFICANCE FOR PRACTICE AND RESEARCH DEVELOPMENT',		1).
valid_abstract_section_header('SIGNIFICANCE FOR THE FIELD',		1).
valid_abstract_section_header('SIGNIFICANCE OF HYPERURICAEMIA',		1).
valid_abstract_section_header('SIGNIFICANCE OF RESEARCH PARAGRAPH',		1).
valid_abstract_section_header('SIGNIFICANCE OF STUDY',		1).
valid_abstract_section_header('SIGNIFICANCE OF THE FINDINGS',		1).
valid_abstract_section_header('SIGNIFICANCE OF THE RESULTS',		1).
valid_abstract_section_header('SIGNIFICANCE OF THIS STUDY',		1).
valid_abstract_section_header('SIGNIFICANCE OF WORK',		1).
valid_abstract_section_header('SIGNIFICANCE STATEMENTS',		1).
valid_abstract_section_header('SIGNIFICANCE/IMPLICATIONS',		1).
valid_abstract_section_header('SIGNIFICANT AND IMPACT OF STUDY',		1).
valid_abstract_section_header('SIGNIFICANT FINDINGS',		1).
valid_abstract_section_header('SIGNIFICANT OUTCOME',		1).
valid_abstract_section_header('SIGNS OF IMPAIRED CIRCULATION, BUT NOT SEVERELY IMPAIRED',		1).
valid_abstract_section_header('SILVER NANOPARTICLES (SIZE',		1).
valid_abstract_section_header('SIMPLE MODELS',		1).
valid_abstract_section_header('SIMULATION AND POWER',		1).
valid_abstract_section_header('SIMULATION METHODS',		1).
valid_abstract_section_header('SIMULATION MODEL',		1).
valid_abstract_section_header('SIMULATION MODELING',		1).
valid_abstract_section_header('SIMULATION MODELLING APPROACH',		1).
valid_abstract_section_header('SIMULATION SCRIPT',		1).
valid_abstract_section_header('SIMULATOR DEVELOPMENT',		1).
valid_abstract_section_header('SIMULATOR USE IN MEDICAL EDUCATION',		1).
valid_abstract_section_header('SIMULATORS',		1).
valid_abstract_section_header('SINGLE IOP MEASUREMENTS',		1).
valid_abstract_section_header('SINOPSIS',		1).
valid_abstract_section_header('SITE AND METHODS',		1).
valid_abstract_section_header('SITES',		1).
valid_abstract_section_header('SITES OF NERVE STIMULATION AND DIFFERING MUSCLE RESPONSE',		1).
valid_abstract_section_header('SIX CONSUMER SURVEYS',		1).
valid_abstract_section_header('SIX MEDICAL SCHOOLS',		1).
valid_abstract_section_header('SIX PRAGMATIC MODELS',		1).
valid_abstract_section_header('SKILL ASSESSMENT IS BOTH POSSIBLE AND ADVISABLE',		1).
valid_abstract_section_header('SKIN AND MUCOSAL MANIFESTATIONS',		1).
valid_abstract_section_header('SKIN NOCICEPTORS',		1).
valid_abstract_section_header('SLOWLY PROGRESSIVE DISEASE',		1).
valid_abstract_section_header('SMALL BOWEL TRANSPLANTATION',		1).
valid_abstract_section_header('SMD CONTROL',		1).
valid_abstract_section_header('SMOKING AND PREVALENCE OF ORAL LEUKOPLAKIA',		1).
valid_abstract_section_header('SMOKING HABITS',		1).
valid_abstract_section_header('SOCIAL CAPITAL',		1).
valid_abstract_section_header('SOCIAL DEFENCES',		1).
valid_abstract_section_header('SOCIAL MEDIA GUIDE',		1).
valid_abstract_section_header('SOCIAL MEDIA QUESTION',		1).
valid_abstract_section_header('SOCIAL NETWORK ANALYSIS',		1).
valid_abstract_section_header('SOFTWARE AND IMPLEMENTATION',		1).
valid_abstract_section_header('SOFTWARE DEVELOPMENT PROCESS',		1).
valid_abstract_section_header('SOFTWARE ENABLED GOVERNANCE POPMEDNET',		1).
valid_abstract_section_header('SOFTWARE FEATURES',		1).
valid_abstract_section_header('SOLUTION METHOD',		1).
valid_abstract_section_header('SOLUTION VS TABLET',		1).
valid_abstract_section_header('SOLUTIONS AND RESULTS',		1).
valid_abstract_section_header('SOLUTIONS IDENTIFIED THROUGH TRAINING AND DEVELOPMENT',		1).
valid_abstract_section_header('SOME CHARACTERISTICS OF LP(A) LIPOPROTEIN',		1).
valid_abstract_section_header('SOME CLUES TO GUIDE TREATMENT',		1).
valid_abstract_section_header('SOME EXAMPLES',		1).
valid_abstract_section_header('SOME OTHER HYPOGLYCEMIC AGENTS',		1).
valid_abstract_section_header('SOME SUGGESTIONS',		1).
valid_abstract_section_header('SOMMARIO',		1).
valid_abstract_section_header('SORTED B',		1).
valid_abstract_section_header('SORTED C',		1).
valid_abstract_section_header('SOURCE AND SUPPLEMENTARY INFORMATION',		1).
valid_abstract_section_header('SOURCE MONITORING AND OTHER PSYCHIATRIC DISORDERS',		1).
valid_abstract_section_header('SOURCE MONITORING AND SCHIZOPHRENIA',		1).
valid_abstract_section_header('SOURCE MONITORING DEFICITS IN NEUROLOGICAL DISORDERS',		1).
valid_abstract_section_header('SOURCE MONITORING IN PSYCHIATRIC DISORDERS WITHIN THE SCHIZOPHRENIC SPECTRUM',		1).
valid_abstract_section_header('SOURCE OF THE INFORMATION',		1).
valid_abstract_section_header('SOURCE(S)',		1).
valid_abstract_section_header('SOURCES AND METHODS',		1).
valid_abstract_section_header('SOURCES AND STUDY SELECTION',		1).
valid_abstract_section_header('SOURCES DE L\'INFORMATION',		1).
valid_abstract_section_header('SOURCES FOR THIS STUDY INCLUDE',		1).
valid_abstract_section_header('SOURCES OF DATA AND AREAS OF DEBATE',		1).
valid_abstract_section_header('SOURCES OF DATA SELECTION',		1).
valid_abstract_section_header('SOURCES OF DATA STUDY SELECTION',		1).
valid_abstract_section_header('SOURCES OF DATA/ STUDY SELECTION',		1).
valid_abstract_section_header('SOURCES OF DATA/STUDY SELECTION',		1).
valid_abstract_section_header('SOURCES OF MATERIAL',		1).
valid_abstract_section_header('SOURCES OF RESISTANCE',		1).
valid_abstract_section_header('SOURCES/METHODS',		1).
valid_abstract_section_header('SOUTIEN',		1).
valid_abstract_section_header('SPATIO-TEMPORAL PATTERNS OF THE MICROBIAL COMMUNITIES',		1).
valid_abstract_section_header('SPECIAL ASPECTS OF ELECTRICAL CARDIOVERSION',		1).
valid_abstract_section_header('SPECIAL FORMS',		1).
valid_abstract_section_header('SPECIAL REHABILITATION MEASURES',		1).
valid_abstract_section_header('SPECIALIZED CARE',		1).
valid_abstract_section_header('SPECIALIZED UNITS',		1).
valid_abstract_section_header('SPECIFIC DERMATOSES OF PREGNANCY',		1).
valid_abstract_section_header('SPECIFIC POPULATIONS',		1).
valid_abstract_section_header('SPECIFIC QUESTION',		1).
valid_abstract_section_header('SPECIFIC RECOMMENDATIONS',		1).
valid_abstract_section_header('SPECIFICITY',		1).
valid_abstract_section_header('SPEECH AUDIOMETRY',		1).
valid_abstract_section_header('SPEECH EXAMINATION',		1).
valid_abstract_section_header('SPENDER PATIENTEN UND METHODEN',		1).
valid_abstract_section_header('SPLENECTOMY',		1).
valid_abstract_section_header('SPLENIC ULTRASONOGRAPHY',		1).
valid_abstract_section_header('SPONTANEOUS OR SECONDARY',		1).
valid_abstract_section_header('SSCP ANALYSIS',		1).
valid_abstract_section_header('STABILITY AT THE CRANIOCERVICAL JUNCTION',		1).
valid_abstract_section_header('STABILITY OF PANTOPRAZOLE',		1).
valid_abstract_section_header('STAGE OF DEVELOPMENT',		1).
valid_abstract_section_header('STAGES OF EVOLUTION OF THE SOMATOMEDIN HYPOTHESIS',		1).
valid_abstract_section_header('STAGES OF EXAMINATION',		1).
valid_abstract_section_header('STAISTICAL ANALYSIS',		1).
valid_abstract_section_header('STAKEHOLDERS',		1).
valid_abstract_section_header('STANDARD METHODS',		1).
valid_abstract_section_header('STANDARD OPERATIVE TECHNIQUE',		1).
valid_abstract_section_header('STANDARD PROCEDURE',		1).
valid_abstract_section_header('STANDARD RADIOLOGICAL PROCEDURES',		1).
valid_abstract_section_header('STANDARD RADIONUCLIDES',		1).
valid_abstract_section_header('STANDARD TREATMENT PROCEDURES',		1).
valid_abstract_section_header('STANDARD TREATMENT/TREATMENT INNOVATIONS',		1).
valid_abstract_section_header('STANDARD VERSUS CULTURALLY COMPETENT QUALITY IMPROVEMENT (QI)',		1).
valid_abstract_section_header('STANDARDIZED EMISSION INVENTORY METHODOLOGY',		1).
valid_abstract_section_header('STANDARDIZED EVALUATION',		1).
valid_abstract_section_header('STANDARDIZED PROCEDURE IN EXAMINATIONS',		1).
valid_abstract_section_header('STANDARDIZING INSULIN ADMINISTRATION',		1).
valid_abstract_section_header('STANDARDS AND CONTROVERSIES',		1).
valid_abstract_section_header('STANDARDS DEVELOPMENT',		1).
valid_abstract_section_header('STANDARDS KPIS',		1).
valid_abstract_section_header('STANDING BALANCE 11 PATIENTS AND 16 CONTROLS',		1).
valid_abstract_section_header('STARTING THE INFANT STRESS REDUCTION QUALITY IMPROVEMENT (QI) TEAM',		1).
valid_abstract_section_header('STASTICAL ANALYSIS USED',		1).
valid_abstract_section_header('STATE OF ART AND PERSPECTIVE',		1).
valid_abstract_section_header('STATE OF ART AND PERSPECTIVE(S)',		1).
valid_abstract_section_header('STATE OF ARTS/PERSPECTIVES',		1).
valid_abstract_section_header('STATE OF DEVELOPMENT',		1).
valid_abstract_section_header('STATE OF SIGNIFICANCE',		1).
valid_abstract_section_header('STATE OF THE ART AND PERPECTIVES',		1).
valid_abstract_section_header('STATE OF THE ART IN RESEARCH AND DEVELOPMENT',		1).
valid_abstract_section_header('STATE OF THE ARTS',		1).
valid_abstract_section_header('STATE OF THE SCIENCE CONFERENCE',		1).
valid_abstract_section_header('STATEMENT OF CONCLUSIONS AND RECOMMENDATIONS FOR PHYSICAL THERAPY PRACTICE',		1).
valid_abstract_section_header('STATEMENT OF PROBLEM AND RATIONALE',		1).
valid_abstract_section_header('STATEMENT OF RECOMMENDATIONS',		1).
valid_abstract_section_header('STATEMENT OF SIGNIFICANT',		1).
valid_abstract_section_header('STATEMENT OF SIGNIGFICANCE',		1).
valid_abstract_section_header('STATEMENT OF THE PROBLEMS',		1).
valid_abstract_section_header('STATEMENTS',		1).
valid_abstract_section_header('STATEMENTS AND DISCUSSION',		1).
valid_abstract_section_header('STATEMENTS OF SIGNIFICANCE',		1).
valid_abstract_section_header('STATEWIDE DATA',		1).
valid_abstract_section_header('STATICAL ANALYSIS',		1).
valid_abstract_section_header('STATINS AND ATHEROSCLEROSIS',		1).
valid_abstract_section_header('STATISCAL ANALYSIS',		1).
valid_abstract_section_header('STATISTIC ANALYSIS',		1).
valid_abstract_section_header('STATISTICAL ANALYSES USED AND RESULTS',		1).
valid_abstract_section_header('STATISTICAL ANALYSIS USED AND RESULT',		1).
valid_abstract_section_header('STATISTICAL AND ANALYSIS',		1).
valid_abstract_section_header('STATISTICAL BASIS FOR THE ASSUMED CONSERVATIVENESS OF PI',		1).
valid_abstract_section_header('STATISTICAL COMPARISON',		1).
valid_abstract_section_header('STATISTICAL DATA',		1).
valid_abstract_section_header('STATISTICAL METHODS USED',		1).
valid_abstract_section_header('STATISTICAL POWER',		1).
valid_abstract_section_header('STATISTICAL TEST USED',		1).
valid_abstract_section_header('STATISTICS AND ANALYSIS',		1).
valid_abstract_section_header('STATISTICS AND DISCUSSION',		1).
valid_abstract_section_header('STATISTICS AND MAIN RESULTS INCLUDE',		1).
valid_abstract_section_header('STATISTICS USED',		1).
valid_abstract_section_header('STATUS AND PERSPECTIVES',		1).
valid_abstract_section_header('STATUS OF KOSMOCERATOPS IN THE DINOSAUR PARK FORMATION',		1).
valid_abstract_section_header('STEERING STRATEGIES',		1).
valid_abstract_section_header('STEPS OF INTEGRATION',		1).
valid_abstract_section_header('STEPS TOWARDS CHANGE',		1).
valid_abstract_section_header('STILLBIRTH',		1).
valid_abstract_section_header('STIMULUS FORMAT',		1).
valid_abstract_section_header('STRATEGIC APPROACH TO ADDRESS ISSUES IDENTIFIED',		1).
valid_abstract_section_header('STRATEGIC APPROACH TO ORGANIZATIONAL CHANGE',		1).
valid_abstract_section_header('STRATEGIC DIMENSION',		1).
valid_abstract_section_header('STRATEGIC PLAN SUMMARY',		1).
valid_abstract_section_header('STRATEGIC PLANNING AND MICROSYSTEM THINKING',		1).
valid_abstract_section_header('STRATEGIES AND EXAMPLES',		1).
valid_abstract_section_header('STRATEGIES AND OPPORTUNITIES FOR IMPROVEMENTS',		1).
valid_abstract_section_header('STRATEGIES FOR IMPLEMENTING QUALITY MANAGEMENT',		1).
valid_abstract_section_header('STRATEGIES FOR RESOLVING COMPLAINTS',		1).
valid_abstract_section_header('STRATEGIES FOR RISK REDUCTION',		1).
valid_abstract_section_header('STRATEGIES OF GENETIC STUDIES',		1).
valid_abstract_section_header('STRATEGIES TO IMPROVE CARE FOR PATIENTS WITH CONGESTIVE HEART FAILURE',		1).
valid_abstract_section_header('STRATEGY AND GOALS',		1).
valid_abstract_section_header('STRATEGY AND MAIN RESULTS',		1).
valid_abstract_section_header('STRATEGY AND NOVEL DIAGNOSTICAL PROCEDURES',		1).
valid_abstract_section_header('STRATEGY AND OBJECTIVES',		1).
valid_abstract_section_header('STRATEGY AND RESULTS',		1).
valid_abstract_section_header('STRATEGY FRAMEWORK FOR HEALTH PROMOTION',		1).
valid_abstract_section_header('STRENGTH AND LIMITATIONS OF THIS STUDY',		1).
valid_abstract_section_header('STRENGTH OF RECOMMENDATIONS TAXONOMY SORT',		1).
valid_abstract_section_header('STRENGTH-OF-RECOMMENDATION TAXONOMY',		1).
valid_abstract_section_header('STRENGTHS AND LIMITATIONS OF THIS STUDY',		1).
valid_abstract_section_header('STRESS AS A TRIGGER',		1).
valid_abstract_section_header('STRETCHING EXERCISES',		1).
valid_abstract_section_header('STROKE',		1).
valid_abstract_section_header('STRONG POINTS',		1).
valid_abstract_section_header('STRUCTURAL BONE ALLOGRAFT HEALING',		1).
valid_abstract_section_header('STRUCTURAL DIMENSION',		1).
valid_abstract_section_header('STRUCTURAL RESOURCES',		1).
valid_abstract_section_header('STRUCTURE AND CONFORMATIONAL CHANGES OF SERUM ALBUMIN',		1).
valid_abstract_section_header('STRUCTURE AND DESIGN',		1).
valid_abstract_section_header('STRUCTURE AND PROCESS OF THE COLLABORATIVE',		1).
valid_abstract_section_header('STRUCTURE OF CUPIENNIUS SALEI VENOM HYALURONIDASE',		1).
valid_abstract_section_header('STRUCTURE OF PROGRAMME',		1).
valid_abstract_section_header('STRUCTURE OF THE PROGRAMME',		1).
valid_abstract_section_header('STRUCTURED ABSTRACT:',		1).
valid_abstract_section_header('STRUCTURED ASSESSMENT INSTRUMENTS',		1).
valid_abstract_section_header('STRUCTURES AND FUNCTIONS',		1).
valid_abstract_section_header('STUDENT EXPERIENCES',		1).
valid_abstract_section_header('STUDIED ANIMALS AND PROCEDURES',		1).
valid_abstract_section_header('STUDIES AND RESULTS',		1).
valid_abstract_section_header('STUDIES CHOSEN',		1).
valid_abstract_section_header('STUDIES CONSIDERED',		1).
valid_abstract_section_header('STUDIES ELIGIBILITY CRITERIA',		1).
valid_abstract_section_header('STUDIES OF HFOV AND LUNG INJURY',		1).
valid_abstract_section_header('STUDIES ON COMBINATION THERAPY',		1).
valid_abstract_section_header('STUDIO',		1).
valid_abstract_section_header('STUDY AIMS AND OBJECTIVES',		1).
valid_abstract_section_header('STUDY AND AIMS',		1).
valid_abstract_section_header('STUDY AND DISCUSSION',		1).
valid_abstract_section_header('STUDY AND RESULT',		1).
valid_abstract_section_header('STUDY AND RESULTS',		1).
valid_abstract_section_header('STUDY AND SOURCE SELECTION',		1).
valid_abstract_section_header('STUDY APPRAISAL AND DATA SYNTHESIS',		1).
valid_abstract_section_header('STUDY APPRAISALS AND SYNTHESIS METHODS',		1).
valid_abstract_section_header('STUDY AREA AND METHODS',		1).
valid_abstract_section_header('STUDY CASES',		1).
valid_abstract_section_header('STUDY CENTRE',		1).
valid_abstract_section_header('STUDY CENTRES',		1).
valid_abstract_section_header('STUDY CONCEPT',		1).
valid_abstract_section_header('STUDY CONDUCT',		1).
valid_abstract_section_header('STUDY DATA',		1).
valid_abstract_section_header('STUDY DATA SOURCE AND ELIGIBILITY CRITERIA',		1).
valid_abstract_section_header('STUDY DESGIN',		1).
valid_abstract_section_header('STUDY DESIGN & PATIENTS',		1).
valid_abstract_section_header('STUDY DESIGN (INCLUDE PARTICIPANTS AND SETTING)',		1).
valid_abstract_section_header('STUDY DESIGN (METHOD)',		1).
valid_abstract_section_header('STUDY DESIGN (SUBJECTS/PATIENTS/MATERIALS/METHODS)',		1).
valid_abstract_section_header('STUDY DESIGN /METHODS',		1).
valid_abstract_section_header('STUDY DESIGN AND M-ETHODS',		1).
valid_abstract_section_header('STUDY DESIGN AND OUTCOME VARIABLES',		1).
valid_abstract_section_header('STUDY DESIGN AND SETTING/METHODS',		1).
valid_abstract_section_header('STUDY DESIGN AND SIZE DURATION',		1).
valid_abstract_section_header('STUDY DESIGN AND STUDY METHODS',		1).
valid_abstract_section_header('STUDY DESIGN AND SUBJECT SELECTION',		1).
valid_abstract_section_header('STUDY DESIGN CASE SERIES METHODS',		1).
valid_abstract_section_header('STUDY DESIGN SAMPLES/MATERIALS, METHODS',		1).
valid_abstract_section_header('STUDY DESIGN SIZE AND DURATION',		1).
valid_abstract_section_header('STUDY DESIGN, MATERIALS, METHODS',		1).
valid_abstract_section_header('STUDY DESIGN, MEASURES, AND OUTCOMES',		1).
valid_abstract_section_header('STUDY DESIGN, METHODS AND RELEVANT RESULTS',		1).
valid_abstract_section_header('STUDY DESIGN, METHODS, AND DATA',		1).
valid_abstract_section_header('STUDY DESIGN, OBSERVATIONAL, METHODS',		1).
valid_abstract_section_header('STUDY DESIGN, PATIENT-SELECTION, AND METHODOLOGY',		1).
valid_abstract_section_header('STUDY DESIGN, PATIENTS AND MEASUREMENTS',		1).
valid_abstract_section_header('STUDY DESIGN, PATIENTS AND METHODS',		1).
valid_abstract_section_header('STUDY DESIGN, SETTING & PARTICIPANTS',		1).
valid_abstract_section_header('STUDY DESIGN, SETTING AND PATIENTS',		1).
valid_abstract_section_header('STUDY DESIGN, SETTING, DURATION',		1).
valid_abstract_section_header('STUDY DESIGN, SETTING, PATIENTS',		1).
valid_abstract_section_header('STUDY DESIGN, SETTINGS, SIZE AND DURATION',		1).
valid_abstract_section_header('STUDY DESIGN, SIZE, PARTICIPANTS',		1).
valid_abstract_section_header('STUDY DESIGN, STUDY SIZE AND DURATION',		1).
valid_abstract_section_header('STUDY DESIGN, SUBJECTS, AND OUTCOME MEASURES',		1).
valid_abstract_section_header('STUDY DESIGN, SUBJECTS, OUTCOME MEASURES',		1).
valid_abstract_section_header('STUDY DESIGN/ SETTING',		1).
valid_abstract_section_header('STUDY DESIGN/ANALYSIS',		1).
valid_abstract_section_header('STUDY DESIGN/MAIN OUTCOME MEASURES',		1).
valid_abstract_section_header('STUDY DESIGN/METHODOLOGY',		1).
valid_abstract_section_header('STUDY DESIGN/REVIEW METHODS',		1).
valid_abstract_section_header('STUDY DESIGN/SETTINGS',		1).
valid_abstract_section_header('STUDY DESIGN/SUBJECT',		1).
valid_abstract_section_header('STUDY DESIGN/SUBJECTS',		1).
valid_abstract_section_header('STUDY DESIGNS AND SUBJECTS',		1).
valid_abstract_section_header('STUDY DESIGNS, SIZE, DURATION',		1).
valid_abstract_section_header('STUDY DEVELOPMENT & IMPLEMENTATION',		1).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA (PARTICIPANTS AND INTERVENTIONS)',		1).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA AND INTERVENTIONS',		1).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA FOR SELECTING STUDIES',		1).
valid_abstract_section_header('STUDY ELIGIBILITY CRITERIA, PARTICIPANTS, INTERVENTIONS',		1).
valid_abstract_section_header('STUDY ELIGIBILITY PARTICIPANTS AND INTERVENTIONS',		1).
valid_abstract_section_header('STUDY ELIGIBILITY, PARTICIPANTS, AND INTERVENTIONS',		1).
valid_abstract_section_header('STUDY ELIGIBLE CRITERIA PARTICIPANTS AND INTERVENTIONS',		1).
valid_abstract_section_header('STUDY ENDPOINTS',		1).
valid_abstract_section_header('STUDY FUNDING / COMPETING INTEREST(S)',		1).
valid_abstract_section_header('STUDY FUNDING AND COMPETING INTEREST(S)',		1).
valid_abstract_section_header('STUDY FUNDING AND CONFLICT OF INTEREST',		1).
valid_abstract_section_header('STUDY FUNDING/ AND COMPETING INTERESTS',		1).
valid_abstract_section_header('STUDY FUNDING/COMPETENT INTERESTS',		1).
valid_abstract_section_header('STUDY FUNDING/COMPETING INTEREST S',		1).
valid_abstract_section_header('STUDY FUNDING/COMPETING INTERETS(S)',		1).
valid_abstract_section_header('STUDY FUNDINGS AND COMPETING INTERESTS',		1).
valid_abstract_section_header('STUDY FUNDINGS/COMPETING INTEREST(S)',		1).
valid_abstract_section_header('STUDY GROUP AND RESULTS',		1).
valid_abstract_section_header('STUDY GROUP SUBJECTS AND METHODS',		1).
valid_abstract_section_header('STUDY GROUPS AND METHODS',		1).
valid_abstract_section_header('STUDY IN MOTHER TO INFANT TRANSMISSION',		1).
valid_abstract_section_header('STUDY INCLUSION CRITERIA',		1).
valid_abstract_section_header('STUDY INTERVENTION AND DATA COLLECTION',		1).
valid_abstract_section_header('STUDY LOCATION AND DURATION',		1).
valid_abstract_section_header('STUDY METHODOLOGY AND RESULTS',		1).
valid_abstract_section_header('STUDY OBJECTIVE AND RATIONALE',		1).
valid_abstract_section_header('STUDY OBJECTIVES, MATERIALS AND METHODS',		1).
valid_abstract_section_header('STUDY OUTCOME MEASURES',		1).
valid_abstract_section_header('STUDY OUTLINE',		1).
valid_abstract_section_header('STUDY OVERVIEW',		1).
valid_abstract_section_header('STUDY PARTICIPANTS AND INTERVENTION',		1).
valid_abstract_section_header('STUDY POPULATION AND MEASUREMENTS',		1).
valid_abstract_section_header('STUDY POPULATION AND MEASURES',		1).
valid_abstract_section_header('STUDY PRINCIPLE',		1).
valid_abstract_section_header('STUDY PRINCIPLES',		1).
valid_abstract_section_header('STUDY PURPOSE AND METHODS',		1).
valid_abstract_section_header('STUDY PURPOSES',		1).
valid_abstract_section_header('STUDY QUALITY DESIGN',		1).
valid_abstract_section_header('STUDY RATIONALE AND OBJECTIVES',		1).
valid_abstract_section_header('STUDY REGISTRY NUMBER',		1).
valid_abstract_section_header('STUDY RESULTS AND CONCLUSION',		1).
valid_abstract_section_header('STUDY SAMPLE, DATA COLLECTION, AND ANALYSIS',		1).
valid_abstract_section_header('STUDY SELECTION & DATA EXTRACTION',		1).
valid_abstract_section_header('STUDY SELECTION & RESULTS',		1).
valid_abstract_section_header('STUDY SELECTION AND CONCLUSIONS',		1).
valid_abstract_section_header('STUDY SELECTION AND INTERVENTION',		1).
valid_abstract_section_header('STUDY SELECTION INCLUSION CRITERIA',		1).
valid_abstract_section_header('STUDY SELECTION PARTICIPANTS AND INTERVENTIONS',		1).
valid_abstract_section_header('STUDY SELECTION, APPRAISAL, AND SYNTHESIS',		1).
valid_abstract_section_header('STUDY SELECTION, STUDY APPRAISAL AND SYNTHESIS METHODS',		1).
valid_abstract_section_header('STUDY SELECTION/SATA EXTRACTION',		1).
valid_abstract_section_header('STUDY SERIE',		1).
valid_abstract_section_header('STUDY SETTING AND REGISTRY DESIGN',		1).
valid_abstract_section_header('STUDY SETTING/DATA COLLECTION',		1).
valid_abstract_section_header('STUDY SETTING/PATIENTS',		1).
valid_abstract_section_header('STUDY SITES',		1).
valid_abstract_section_header('STUDY STRENGTHS',		1).
valid_abstract_section_header('STUDY SUBJECTS AND TREATMENTS',		1).
valid_abstract_section_header('STUDY TITLE',		1).
valid_abstract_section_header('STUDY TYPE AND PATIENT GROUP',		1).
valid_abstract_section_header('STUDY\'S AIM',		1).
valid_abstract_section_header('STUDY, DESIGN AND METHODS',		1).
valid_abstract_section_header('STUDY-SETTING',		1).
valid_abstract_section_header('STUDY/TRIAL REGISTRATION',		1).
valid_abstract_section_header('STUDYDESIGN',		1).
valid_abstract_section_header('STUDYING TIME TO PREGNANCY',		1).
valid_abstract_section_header('STYLE OF LIFE FACTORS',		1).
valid_abstract_section_header('SUB-QUESTIONS',		1).
valid_abstract_section_header('SUBBBJECTS',		1).
valid_abstract_section_header('SUBCUTANEOUS INJECTION AND LOCAL ANAESTHESIA',		1).
valid_abstract_section_header('SUBJECT AND DESIGN',		1).
valid_abstract_section_header('SUBJECT AND RESULT',		1).
valid_abstract_section_header('SUBJECT AND RESULTS',		1).
valid_abstract_section_header('SUBJECT AND SETTING',		1).
valid_abstract_section_header('SUBJECT SELECTION AND ENROLLMENT',		1).
valid_abstract_section_header('SUBJECT, PARTICIPANTS',		1).
valid_abstract_section_header('SUBJECT/OBJECTIVE',		1).
valid_abstract_section_header('SUBJECTAND METHODS',		1).
valid_abstract_section_header('SUBJECTIVE ACOUSTIC ANALYSIS',		1).
valid_abstract_section_header('SUBJECTIVE ELICITATION',		1).
valid_abstract_section_header('SUBJECTIVE MEASURES',		1).
valid_abstract_section_header('SUBJECTIVES',		1).
valid_abstract_section_header('SUBJECTS & METHOD',		1).
valid_abstract_section_header('SUBJECTS & OUTCOME MEASURES',		1).
valid_abstract_section_header('SUBJECTS AND BASIC RESULTS',		1).
valid_abstract_section_header('SUBJECTS AND DATA',		1).
valid_abstract_section_header('SUBJECTS AND RESEARCH DESIGN',		1).
valid_abstract_section_header('SUBJECTS DESIGN',		1).
valid_abstract_section_header('SUBJECTS DESIGN AND METHODS',		1).
valid_abstract_section_header('SUBJECTS METHODS',		1).
valid_abstract_section_header('SUBJECTS OF THE ASSESSMENT',		1).
valid_abstract_section_header('SUBJECTS, DESIGN, AND METHODS',		1).
valid_abstract_section_header('SUBJECTS, INTERVENTION AND MEASUREMENTS',		1).
valid_abstract_section_header('SUBJECTS, INTERVENTIONS, AND MEASUREMENTS',		1).
valid_abstract_section_header('SUBJECTS, MATERIAL & METHODS',		1).
valid_abstract_section_header('SUBJECTS, METHODS',		1).
valid_abstract_section_header('SUBJECTS, METHODS, RESULTS',		1).
valid_abstract_section_header('SUBJECTS-METHODS',		1).
valid_abstract_section_header('SUBJECTS/AUDIENCE',		1).
valid_abstract_section_header('SUBJECTS/MATERIALS AND METHOD',		1).
valid_abstract_section_header('SUBJECTS/PATIENTS (OR MATERIALS) AND METHODS',		1).
valid_abstract_section_header('SUBJECTS:',		1).
valid_abstract_section_header('SUBJECTVIES/METHODS',		1).
valid_abstract_section_header('SUBTITLE',		1).
valid_abstract_section_header('SUCCESS',		1).
valid_abstract_section_header('SUCCESSES',		1).
valid_abstract_section_header('SUCCESSES AND CHALLENGES',		1).
valid_abstract_section_header('SUDDEN CARDIAC DEATH AND PHYSICAL ACTIVITY',		1).
valid_abstract_section_header('SUGAMMADEX IS BELONGING TO A NEW CLASS OF DRUGS',		1).
valid_abstract_section_header('SUGGESTED ACTION',		1).
valid_abstract_section_header('SUGGESTED NEW FRAMEWORK',		1).
valid_abstract_section_header('SUGGESTED TEACHING STRATEGIES',		1).
valid_abstract_section_header('SUGGESTED WAITING PERIODS',		1).
valid_abstract_section_header('SUGGESTION FOR THE FUTURE',		1).
valid_abstract_section_header('SUGGESTIONS FOR FUTURE USE',		1).
valid_abstract_section_header('SUJECTS',		1).
valid_abstract_section_header('SUJETS',		1).
valid_abstract_section_header('SUMAMRY',		1).
valid_abstract_section_header('SUMMARY AND AUTHORS REFLECTIONS',		1).
valid_abstract_section_header('SUMMARY AND BACKGROUND DATE',		1).
valid_abstract_section_header('SUMMARY AND CLINICAL RELEVANCE',		1).
valid_abstract_section_header('SUMMARY AND INTRODUCTION',		1).
valid_abstract_section_header('SUMMARY AND KEY MESSAGES',		1).
valid_abstract_section_header('SUMMARY AND NEXT STEPS',		1).
valid_abstract_section_header('SUMMARY AND OUTLOOK',		1).
valid_abstract_section_header('SUMMARY AND RECENT FINDINGS',		1).
valid_abstract_section_header('SUMMARY AND RECOMMENDATIONS FOR CLINICAL PRACTICE',		1).
valid_abstract_section_header('SUMMARY AND RELEVANCE',		1).
valid_abstract_section_header('SUMMARY ANSWERS',		1).
valid_abstract_section_header('SUMMARY FOR ANNOTATED TABLE OF CONTENTS',		1).
valid_abstract_section_header('SUMMARY IMPLICATIONS',		1).
valid_abstract_section_header('SUMMARY OBSERVATIONS',		1).
valid_abstract_section_header('SUMMARY OF BACKGROUND CONTEXT',		1).
valid_abstract_section_header('SUMMARY OF BACKGROUND DATA AND METHODS',		1).
valid_abstract_section_header('SUMMARY OF BACKGROUND DATA:',		1).
valid_abstract_section_header('SUMMARY OF BACKGROUND DATAE',		1).
valid_abstract_section_header('SUMMARY OF BACKROUND DATA',		1).
valid_abstract_section_header('SUMMARY OF BENEFITS',		1).
valid_abstract_section_header('SUMMARY OF CLINICAL DETAILS',		1).
valid_abstract_section_header('SUMMARY OF COMMENTARY',		1).
valid_abstract_section_header('SUMMARY OF CONTENTS',		1).
valid_abstract_section_header('SUMMARY OF ELIGIBILITY CRITERIA',		1).
valid_abstract_section_header('SUMMARY OF EVIDENCE FOR PROPOSED METHOD',		1).
valid_abstract_section_header('SUMMARY OF EXPERIENCE',		1).
valid_abstract_section_header('SUMMARY OF FINDING',		1).
valid_abstract_section_header('SUMMARY OF GUIDELINES',		1).
valid_abstract_section_header('SUMMARY OF IMPACTS',		1).
valid_abstract_section_header('SUMMARY OF METHODS OF THE BASEL SSI COHORT STUDY',		1).
valid_abstract_section_header('SUMMARY OF METHODS/RESULTS',		1).
valid_abstract_section_header('SUMMARY OF OUTCOMES',		1).
valid_abstract_section_header('SUMMARY OF RECENT FINDINGS',		1).
valid_abstract_section_header('SUMMARY OF RECOMMENDATION',		1).
valid_abstract_section_header('SUMMARY OF SELECTED RECOMMENDATIONS',		1).
valid_abstract_section_header('SUMMARY OF THE ARTICLE',		1).
valid_abstract_section_header('SUMMARY OF THE CASE',		1).
valid_abstract_section_header('SUMMARY OF THE CASE REPORT',		1).
valid_abstract_section_header('SUMMARY OF THE CLINICAL CASE',		1).
valid_abstract_section_header('SUMMARY OF THE METHODS',		1).
valid_abstract_section_header('SUMMARY OF THE RESULTS',		1).
valid_abstract_section_header('SUMMARY OR REVIEW',		1).
valid_abstract_section_header('SUMMARY THOUGHTS',		1).
valid_abstract_section_header('SUMMARY/CLINICAL IMPLICATIONS',		1).
valid_abstract_section_header('SUMMARY/DISCUSSION',		1).
valid_abstract_section_header('SUMMERY OF BACKGROUND DATA',		1).
valid_abstract_section_header('SUMMING UP',		1).
valid_abstract_section_header('SUPERFICIAL TYPE',		1).
valid_abstract_section_header('SUPPORTIVE THERAPY WITH HAEMATOPOIETIC GROWTH FACTORS',		1).
valid_abstract_section_header('SUPPRESSION OF RESISTANCE',		1).
valid_abstract_section_header('SUPRAAORTIC ARTERIES',		1).
valid_abstract_section_header('SURGERY AND ADJUVANT TREATMENT',		1).
valid_abstract_section_header('SURGERY AT RECURRENCE',		1).
valid_abstract_section_header('SURGICAL APPROACHES',		1).
valid_abstract_section_header('SURGICAL CONSIDERATIONS',		1).
valid_abstract_section_header('SURGICAL DE-ESCALATION FOR INVASIVE BREAST CANCER TREATMENT',		1).
valid_abstract_section_header('SURGICAL ENDOLARYNGEAL INTERVENTIONS',		1).
valid_abstract_section_header('SURGICAL OBJECTIVE',		1).
valid_abstract_section_header('SURGICAL PRINCIPAL AND OBJECTIVE',		1).
valid_abstract_section_header('SURGICAL PROTOCOL',		1).
valid_abstract_section_header('SURGICAL TECHNIQUE (SEE VIDEO)',		1).
valid_abstract_section_header('SURGICAL TECHNIQUE AND METHODS',		1).
valid_abstract_section_header('SURGICAL TECHNIQUE/PROCEDURE',		1).
valid_abstract_section_header('SURGICAL TREATMENT AND OUTCOME',		1).
valid_abstract_section_header('SURGICAL TREATMENT OF PAIN',		1).
valid_abstract_section_header('SURROUNDING ELEMENTS',		1).
valid_abstract_section_header('SURVEY APPLICATION',		1).
valid_abstract_section_header('SURVEY DESIGN & SETTING',		1).
valid_abstract_section_header('SURVEY FEEDBACK',		1).
valid_abstract_section_header('SURVEY METHOD',		1).
valid_abstract_section_header('SURVEY RESPONDENTS AND METHODS',		1).
valid_abstract_section_header('SURVEYED DESCRIPTIONS RELATED TO PHYSIOLOGY AND BRAIN FUNCTION',		1).
valid_abstract_section_header('SURVIVAL RESULTS',		1).
valid_abstract_section_header('SURVIVAL, BODY WEIGHTS, AND CLINICAL FINDINGS',		1).
valid_abstract_section_header('SURVIVAL, FEED CONSUMPTION, AND BODY WEIGHTS',		1).
valid_abstract_section_header('SUSCEPTIBILITY TESTING',		1).
valid_abstract_section_header('SUSTAINABILITY FACTORS',		1).
valid_abstract_section_header('SWITZERIAND',		1).
valid_abstract_section_header('SWOT ANALYSIS',		1).
valid_abstract_section_header('SYMBOLS',		1).
valid_abstract_section_header('SYMPATHETIC ACTIVATION IN HEART FAILURE',		1).
valid_abstract_section_header('SYMPTOM TRIAD',		1).
valid_abstract_section_header('SYMPTOMATIC CARE',		1).
valid_abstract_section_header('SYMPTOMS AND CLINICAL FINDINGS',		1).
valid_abstract_section_header('SYMPTOMS AND DIAGNOSIS',		1).
valid_abstract_section_header('SYMPTOMS AND RESULTS',		1).
valid_abstract_section_header('SYNCHRONIC LESIONS',		1).
valid_abstract_section_header('SYNERGISM',		1).
valid_abstract_section_header('SYNTHESIS AND BIOLOGICAL ROLE OF CRP',		1).
valid_abstract_section_header('SYNTHESIS METHOD',		1).
valid_abstract_section_header('SYSTEM AND METHODS',		1).
valid_abstract_section_header('SYSTEM APPROACH',		1).
valid_abstract_section_header('SYSTEM ARCHITECTURE',		1).
valid_abstract_section_header('SYSTEM DESIGN',		1).
valid_abstract_section_header('SYSTEMATIC REVIEW',		1).
valid_abstract_section_header('SYSTEMATIC REVIEW PROTOCOL NUMBER',		1).
valid_abstract_section_header('SYSTEMATIC REVIEWS REGISTRATION NUMBER',		1).
valid_abstract_section_header('SYSTEMIC CORTICOSTEROIDS',		1).
valid_abstract_section_header('SYSTEMIC DISEASE',		1).
valid_abstract_section_header('SYSTEMIC INVOLVEMENT',		1).
valid_abstract_section_header('SYSTEMIC MODEL',		1).
valid_abstract_section_header('SYSTEMIC REVIEW',		1).
valid_abstract_section_header('SYSTEMIC THERAPIES',		1).
valid_abstract_section_header('SYSTEMIC TREATMENT FOR TRIPLE NEGATIVE BREAST CANCER (TNBC',		1).
valid_abstract_section_header('SYSTEMIC TREATMENTS',		1).
valid_abstract_section_header('SYSTEMS',		1).
valid_abstract_section_header('SYSTEMWIDE IMPLEMENTATION',		1).
valid_abstract_section_header('SYSTOLIC HYPERTENSION',		1).
valid_abstract_section_header('Statistical Analysis',		1).
valid_abstract_section_header('Study Selection/Data Extraction',		1).
valid_abstract_section_header('Study design',		1).
valid_abstract_section_header('Subjects',		1).
valid_abstract_section_header('TABLEAU CLINIQUE',		1).
valid_abstract_section_header('TABULATION INTEGRATION AND RESULTS',		1).
valid_abstract_section_header('TAKE-HOME MESSAGES',		1).
valid_abstract_section_header('TAKE-HOME SUMMARY',		1).
valid_abstract_section_header('TAKING ACTION',		1).
valid_abstract_section_header('TAKING RESPONSIBILITY AS HEALTHCARE PROFESSIONALS',		1).
valid_abstract_section_header('TANDEMS OF THERAPEUTIC AND DIAGNOSTIC AGENTS',		1).
valid_abstract_section_header('TARDIVE DYSKINESIA',		1).
valid_abstract_section_header('TARGET GROUPS',		1).
valid_abstract_section_header('TARGET SETTING',		1).
valid_abstract_section_header('TARGETED POPULATION',		1).
valid_abstract_section_header('TASK DESIGN AND MAIN RESULTS',		1).
valid_abstract_section_header('TAX CAP RECOMMENDATIONS',		1).
valid_abstract_section_header('TAXOL',		1).
valid_abstract_section_header('TAXONOMIC STATUS',		1).
valid_abstract_section_header('TAXONOMICAL CLASSIFICATION METHODS',		1).
valid_abstract_section_header('TAXONOMY-BASED ECOLOGICAL INDICATORS',		1).
valid_abstract_section_header('TB PREVALENCE AND TB MORTALITY',		1).
valid_abstract_section_header('TB, OR',		1).
valid_abstract_section_header('TC-AR',		1).
valid_abstract_section_header('TC-CC',		1).
valid_abstract_section_header('TEACHING AND LEARNING',		1).
valid_abstract_section_header('TEACHING IMPLICATIONS',		1).
valid_abstract_section_header('TEACHING OBJECTIVES',		1).
valid_abstract_section_header('TEACHING/LEARNING PACKETS',		1).
valid_abstract_section_header('TEAM APPROACH',		1).
valid_abstract_section_header('TECHNICAL ABSTRACT',		1).
valid_abstract_section_header('TECHNICAL APPROACH',		1).
valid_abstract_section_header('TECHNICAL CONCEPTS',		1).
valid_abstract_section_header('TECHNICAL CONSIDERATION',		1).
valid_abstract_section_header('TECHNICAL DESCRIPTION',		1).
valid_abstract_section_header('TECHNICAL DETAILS OF THE METHOD',		1).
valid_abstract_section_header('TECHNICAL DIMENSION',		1).
valid_abstract_section_header('TECHNICAL FINDINGS',		1).
valid_abstract_section_header('TECHNICAL FINE POINTS',		1).
valid_abstract_section_header('TECHNICAL IMPROVEMENTS IN TUNING FORKS',		1).
valid_abstract_section_header('TECHNICAL INNOVATIONS',		1).
valid_abstract_section_header('TECHNIKA BADANIA',		1).
valid_abstract_section_header('TECHNIQUE AND CASE EXAMPLES',		1).
valid_abstract_section_header('TECHNIQUE CONSIDERATIONS',		1).
valid_abstract_section_header('TECHNIQUE FOR MANAGING DEMAND',		1).
valid_abstract_section_header('TECHNIQUE POINTS',		1).
valid_abstract_section_header('TECHNIQUES AND BIOLOGICAL RESULTS',		1).
valid_abstract_section_header('TECHNIQUES AND RESULTS',		1).
valid_abstract_section_header('TECHNIQUES OF LOCAL ANESTHESIA',		1).
valid_abstract_section_header('TECHNOLOGICAL ASPECTS AND CONCLUSION',		1).
valid_abstract_section_header('TECHNOLOGICAL LEVELS OF RADIOTHERAPY IN DEVELOPED COUNTRIES',		1).
valid_abstract_section_header('TECHNOLOGY AND MEDICATION SYSTEMS',		1).
valid_abstract_section_header('TECHNOLOGY AND PERFORMANCE',		1).
valid_abstract_section_header('TECHNOLOGY AND SAFETY',		1).
valid_abstract_section_header('TECHNOLOGY AND SYSTEMS',		1).
valid_abstract_section_header('TECHNOLOGY DESCRIPTION',		1).
valid_abstract_section_header('TECNIQUE',		1).
valid_abstract_section_header('TELEMEDICINE',		1).
valid_abstract_section_header('TEMOZOLOMIDE IN CANCER PATIENTS',		1).
valid_abstract_section_header('TEMOZOLOMIDE RECHALLENGE',		1).
valid_abstract_section_header('TEN TOPICS RELEVANT FOR THE DIAGNOSIS OF FUNCTIONAL PSYCHOSES',		1).
valid_abstract_section_header('TENSIONS BETWEEN CPGS AND PIS',		1).
valid_abstract_section_header('TENTATIVE CONCLUSIONS',		1).
valid_abstract_section_header('TERMS AND CONCEPTS',		1).
valid_abstract_section_header('TERMS AND DEFINITIONS',		1).
valid_abstract_section_header('TERTIARY OUTCOME',		1).
valid_abstract_section_header('TEST INTERVENTION',		1).
valid_abstract_section_header('TESTICULAR GCT',		1).
valid_abstract_section_header('TESTING A CHANGE FOR IMPROVEMENT',		1).
valid_abstract_section_header('TESTING HYPOTHESIS',		1).
valid_abstract_section_header('TESTING OF INTERNET DOCUMENTS BY CONSUMERS',		1).
valid_abstract_section_header('TESTING THE HYPOTHESES AND THEIR UTILITY',		1).
valid_abstract_section_header('TESTS AND METHODS',		1).
valid_abstract_section_header('TESTS OF THE HYPOTHESIS',		1).
valid_abstract_section_header('TEXT',		1).
valid_abstract_section_header('THE ACTION EFFECT METHOD',		1).
valid_abstract_section_header('THE ADCAR TRIAL',		1).
valid_abstract_section_header('THE AIM OF OUR WORK',		1).
valid_abstract_section_header('THE AIM OF THE INVESTIGATION WAS TO',		1).
valid_abstract_section_header('THE AIM OF THIS ARTICLE IS TO',		1).
valid_abstract_section_header('THE AIM OF THIS REVIEW',		1).
valid_abstract_section_header('THE AIM OF THIS STUDY IS TO EXAMINE',		1).
valid_abstract_section_header('THE AIM OF THIS STUDY WAS',		1).
valid_abstract_section_header('THE AIM OF THIS STUDY WAS TO ASSESS THE',		1).
valid_abstract_section_header('THE AIM OF THIS WORK WAS TO VALIDATE',		1).
valid_abstract_section_header('THE AIM, MATERIAL AND METHODS',		1).
valid_abstract_section_header('THE AIMS AND OBJECTIVES OF THIS STUDY WERE',		1).
valid_abstract_section_header('THE AIMS OF THE PRESENT STUDY WERE',		1).
valid_abstract_section_header('THE ALLIANCE',		1).
valid_abstract_section_header('THE AMBIGUOUS TASK',		1).
valid_abstract_section_header('THE ANALYTICAL METHOD ENTAILED THE FOLLOWING STEPS',		1).
valid_abstract_section_header('THE ANTICOAGULATION PROTOCOL WAS AS FOLLOWS IN ALL CASES',		1).
valid_abstract_section_header('THE ASSESSMENT CYCLE',		1).
valid_abstract_section_header('THE ASSOCIATION BETWEEN PERIODONTAL DISEASE AND GENERAL HEALTH',		1).
valid_abstract_section_header('THE ASSOCIATION WITH BIOPSYCHOSOCIAL FACTORS',		1).
valid_abstract_section_header('THE ASSUMED HYPOTHESIS',		1).
valid_abstract_section_header('THE ASYMPTOMATIC PREVENTION PATIENT',		1).
valid_abstract_section_header('THE AUSSIE OPTIMISM',		1).
valid_abstract_section_header('THE AUTHORS ATTEMPTED TO DETERMINE WHETHER',		1).
valid_abstract_section_header('THE BACKGROUND AND PURPOSE',		1).
valid_abstract_section_header('THE BANDWIDTH OF TOTAL RELATIONSHIP',		1).
valid_abstract_section_header('THE BASIC PRINCIPLES OF ECHOCONTRAST AGENTS',		1).
valid_abstract_section_header('THE BERNESE HYPOTHERMIA ALGORITHM',		1).
valid_abstract_section_header('THE BIG NATIONAL DATABASES AND STUDIES ON REAL WORLD DATA',		1).
valid_abstract_section_header('THE BINDING PROBLEM IS A LONGSTANDING ISSUE IN VISION SCIENCE',		1).
valid_abstract_section_header('THE BUSINESS CASE',		1).
valid_abstract_section_header('THE CALL FOR STRUCTURE',		1).
valid_abstract_section_header('THE CAPSULE ABSTRACT',		1).
valid_abstract_section_header('THE CASE FOR PREVENTION',		1).
valid_abstract_section_header('THE CASE REPORT',		1).
valid_abstract_section_header('THE CASES',		1).
valid_abstract_section_header('THE CATS ASSESSMENT',		1).
valid_abstract_section_header('THE CAUSE OF CKD',		1).
valid_abstract_section_header('THE CHALLENGES OF INTERDISCIPLINARY EDUCATION FOR ACCREDITATION',		1).
valid_abstract_section_header('THE CHIP WORK TEAMS',		1).
valid_abstract_section_header('THE CLINICAL CASE',		1).
valid_abstract_section_header('THE COLLABORATIVE APPROACH',		1).
valid_abstract_section_header('THE COLLABORATIVE MODEL',		1).
valid_abstract_section_header('THE COMPLAINT MANAGEMENT PROCESS',		1).
valid_abstract_section_header('THE COMPLICATIONS',		1).
valid_abstract_section_header('THE CONCEPT',		1).
valid_abstract_section_header('THE CONCEPT OF EVIDENCE',		1).
valid_abstract_section_header('THE CONSENSUAL FRENCH VERSION',		1).
valid_abstract_section_header('THE CONTEXT FOR IVD PRODUCT EVALUATIONS',		1).
valid_abstract_section_header('THE CONTEXT FOR QUALITY IMPROVEMENT (QI)',		1).
valid_abstract_section_header('THE CONTEXT FOR THE HIPPOCRATIC OATH',		1).
valid_abstract_section_header('THE CONVERSATION ANALYSIS APPROACH',		1).
valid_abstract_section_header('THE CONVERSION PROCESS',		1).
valid_abstract_section_header('THE CURRENT CONFLICT',		1).
valid_abstract_section_header('THE DATA COLLECTION STRATEGY',		1).
valid_abstract_section_header('THE DECISION TO TREAT',		1).
valid_abstract_section_header('THE DEFENCE SYSTEMS',		1).
valid_abstract_section_header('THE DEVELOPMENT OF A CONTROL TOOL',		1).
valid_abstract_section_header('THE DEVELOPMENT OF THE ORTHODONTIC TEAM',		1).
valid_abstract_section_header('THE DEVICE',		1).
valid_abstract_section_header('THE DIABETIC FOOT',		1).
valid_abstract_section_header('THE DISCUSSION',		1).
valid_abstract_section_header('THE DISCUSSIONS AND CONCLUSIONS',		1).
valid_abstract_section_header('THE DUKE CRITERIA',		1).
valid_abstract_section_header('THE EDUSPIM DESIGN IS TAILORED EASILY TO FIT NUMEROUS APPLICATIONS',		1).
valid_abstract_section_header('THE EFFECT OF GALANTAMINE ON CARDIAC CONDUCTION TIME BACKGROUND',		1).
valid_abstract_section_header('THE EFFECTS OF DANGER ON THE GROUP',		1).
valid_abstract_section_header('THE EFFECTS OF SPIRONOLACTONE',		1).
valid_abstract_section_header('THE ELDERLY AND ANTIBIOTICS',		1).
valid_abstract_section_header('THE EMOTIONS OF LARGER GROUPS',		1).
valid_abstract_section_header('THE EVIDENCE',		1).
valid_abstract_section_header('THE EVIDENCE FOR DIFFERENT STRATEGIES OF IMPLEMENTING CHANGE',		1).
valid_abstract_section_header('THE EVOLUTION OF THE TREATMENT',		1).
valid_abstract_section_header('THE EXPERIENCE OF THE AVEDIS DONABEDIAN FOUNDATION',		1).
valid_abstract_section_header('THE FAMILY TROGOSSITIDAE (COLEOPTERA',		1).
valid_abstract_section_header('THE FEMALE SEXUAL DYSFUNCTION (FSD)',		1).
valid_abstract_section_header('THE FINANCIAL AND SCIENTIFIC EVIDENCE BEHIND PREVENTION',		1).
valid_abstract_section_header('THE FINDINGS',		1).
valid_abstract_section_header('THE FIRST SIGNS OF THIS DISEASE WERE',		1).
valid_abstract_section_header('THE FIRST TWO POSTGRADUATE YEARS',		1).
valid_abstract_section_header('THE FIVE CS OF CULTURE CHANGE AND CULTURE SURVEYS',		1).
valid_abstract_section_header('THE FIVE PHASES',		1).
valid_abstract_section_header('THE FOLLOWING DEVELOPMENT',		1).
valid_abstract_section_header('THE FOLLOWING JUNIOR SUBJECTIVE SYNONYMS ARE ESTABLISHED',		1).
valid_abstract_section_header('THE FOLLOWING NEW SYNONYMIES ARE PROPOSED',		1).
valid_abstract_section_header('THE FOLLOWING OUTCOMES WERE EVALUATED',		1).
valid_abstract_section_header('THE FOLLOWING SYNONYMIES WERE ESTABLISHED',		1).
valid_abstract_section_header('THE FOLLOWING TAXONOMIC OR NOMENCLATURAL CHANGES ARE PROPOSED',		1).
valid_abstract_section_header('THE GOAL OF THE WORK',		1).
valid_abstract_section_header('THE HIP REPLACEMENT CASE',		1).
valid_abstract_section_header('THE HIPAA PRIVACY RULE PUTS SOME GOVERNANCE IN THE HANDS OF INDIVIDUALS',		1).
valid_abstract_section_header('THE HISTORY OF RESEARCH AND PRODUCTION OF THROMBOPOIETIN',		1).
valid_abstract_section_header('THE HISTORY OF THE TREATMENT OF INFECTIOUS DISEASES',		1).
valid_abstract_section_header('THE HISTORY OF TREATMENT IN OUR COUNTRY',		1).
valid_abstract_section_header('THE IDEA',		1).
valid_abstract_section_header('THE IGF SYSTEM',		1).
valid_abstract_section_header('THE IMPACT TO THE INDUSTRY',		1).
valid_abstract_section_header('THE IMPORTANCE OF CONTEXT',		1).
valid_abstract_section_header('THE IMPORTANCE OF HUMAN INFECTION WITH AVIAN INFLUENZA VIRUSES',		1).
valid_abstract_section_header('THE IMPORTANCE OF IMPLEMENTATION',		1).
valid_abstract_section_header('THE IMPROVEMENT METHOD',		1).
valid_abstract_section_header('THE IMPROVEMENT PROGRAMME',		1).
valid_abstract_section_header('THE INCIDENCE OF BREAST CANCER CONTINUES TO RISE',		1).
valid_abstract_section_header('THE INDEPENDENT APPRAISAL SERVICE',		1).
valid_abstract_section_header('THE INFLUENCE OF DIETARY FAT ON FOOD INTAKE',		1).
valid_abstract_section_header('THE INFLUENCE OF HEMOGLOBIN CONCENTRATION ON TUMOR OXYGENATION AND OUTCOME',		1).
valid_abstract_section_header('THE INNER WORLD',		1).
valid_abstract_section_header('THE INSTITUTIONAL REVIEW BOARD',		1).
valid_abstract_section_header('THE INTERVENTIONS AND OUTCOMES',		1).
valid_abstract_section_header('THE INVENTION OF THE TUNING FORK',		1).
valid_abstract_section_header('THE JOURNEY BEGINS',		1).
valid_abstract_section_header('THE JOURNEY CONTINUES',		1).
valid_abstract_section_header('THE KEY IS ADAPTATION ON ALL LEVELS',		1).
valid_abstract_section_header('THE KTA PROCESS',		1).
valid_abstract_section_header('THE LEVEL OF FOLATES IN SERUM AND NTD',		1).
valid_abstract_section_header('THE LIFE AND CAREER',		1).
valid_abstract_section_header('THE MAIN AIM OF THE STUDY',		1).
valid_abstract_section_header('THE MAIN DIAGNOSES, THERAPEUTICS INTERVENTIONS, AND OUTCOMES',		1).
valid_abstract_section_header('THE MAIN DIAGNOSTIC PROCEDURE',		1).
valid_abstract_section_header('THE MAIN FEATURES OF THE SELECTIVE SCREENING PROGRAM WERE',		1).
valid_abstract_section_header('THE MAIN FINDING',		1).
valid_abstract_section_header('THE MAIN FINDINGS OF THE STUDY',		1).
valid_abstract_section_header('THE MAIN METHOD OF TREATMENT',		1).
valid_abstract_section_header('THE MAIN OBJECTIVES OF THE STUDY',		1).
valid_abstract_section_header('THE MAIN SYMPTOMS AND THE IMPORTANT CLINICAL FINDINGS',		1).
valid_abstract_section_header('THE MEANING OF RISK',		1).
valid_abstract_section_header('THE MEASURE DEVELOPMENT PROCESS',		1).
valid_abstract_section_header('THE MEDICAL TREATMENT',		1).
valid_abstract_section_header('THE MIDDLE MENINGEAL VESSELS IN NONHUMAN PRIMATES',		1).
valid_abstract_section_header('THE MODIFICATION OF RISK',		1).
valid_abstract_section_header('THE MULTIDISCIPLINARY TEAM',		1).
valid_abstract_section_header('THE MYCOFLORA OF THE ENVIRONMENT',		1).
valid_abstract_section_header('THE NATIONAL AGENDA',		1).
valid_abstract_section_header('THE NATURAL HISTORY OF CMV INFECTION',		1).
valid_abstract_section_header('THE NATURE OF PUBLIC PARTICIPATION GROUPS',		1).
valid_abstract_section_header('THE NATURE OF THE CURRENT EVIDENCE OF IMPACT',		1).
valid_abstract_section_header('THE NAVARRE OCCUPATIONAL HEALTH INSTITUTE',		1).
valid_abstract_section_header('THE NEED FOR A MORE COMPREHENSIVE SOFTWARE PROGRAM',		1).
valid_abstract_section_header('THE NEW CONCEPT',		1).
valid_abstract_section_header('THE NEW MODEL',		1).
valid_abstract_section_header('THE NEW TYPE OF THE CHEMICAL CASCADE REACTION WAS FOUND',		1).
valid_abstract_section_header('THE NOTION OF RISK',		1).
valid_abstract_section_header('THE OBJECTIVES',		1).
valid_abstract_section_header('THE OBJECTIVES OF THE STUDY',		1).
valid_abstract_section_header('THE OBJECTIVES OF THIS STUDY WERE',		1).
valid_abstract_section_header('THE OBJECTIVES OF THIS STUDY WERE TO',		1).
valid_abstract_section_header('THE OBSERVATIONAL UNIT',		1).
valid_abstract_section_header('THE PARTNERSHIP STRENGTH SURVEY',		1).
valid_abstract_section_header('THE PATIENT GROUP AND METHODOLOGY',		1).
valid_abstract_section_header('THE PATIENT MANAGEMENT TOOL (PMT)',		1).
valid_abstract_section_header('THE PATIENT SAFETY ADVISORY',		1).
valid_abstract_section_header('THE PATIENT SAFETY EVALUATION',		1).
valid_abstract_section_header('THE PATIENT WITH STABLE CHEST PAIN',		1).
valid_abstract_section_header('THE PDSA CYCLE AS LEARNING THEORY',		1).
valid_abstract_section_header('THE PELVIC RING IS STRESSED BY EXTERNAL FORCES',		1).
valid_abstract_section_header('THE PHYSICS OF THE TUNING FORK',		1).
valid_abstract_section_header('THE PI PLAN IN ACTION',		1).
valid_abstract_section_header('THE PLAUSIBILITY OF THE COMPARABILITY CONDITIONS',		1).
valid_abstract_section_header('THE PRESENT PAPER',		1).
valid_abstract_section_header('THE PRESENT WORK TESTED THE HYPOTHESES THAT',		1).
valid_abstract_section_header('THE PREVALENCE OF ERECTILE DYSFUNCTION (ED) IN POPULATION BASED STUDIES',		1).
valid_abstract_section_header('THE PRINCIPAL RESULTS AND MAJOR CONCLUSIONS',		1).
valid_abstract_section_header('THE PRINCIPLE AGENTS UNDER ASSESSMENT',		1).
valid_abstract_section_header('THE PROBLEM AND THE SOLUTION',		1).
valid_abstract_section_header('THE PROJECT DATA SPHERE INITIATIVE',		1).
valid_abstract_section_header('THE PROPERTIES OF NESP',		1).
valid_abstract_section_header('THE PROPOSED FRAMEWORK',		1).
valid_abstract_section_header('THE PURPOSE OF THE PHD THESIS WAS',		1).
valid_abstract_section_header('THE PURPOSE OF THE STUDY WAS',		1).
valid_abstract_section_header('THE PURPOSE OF THIS PAPER WAS',		1).
valid_abstract_section_header('THE PURPOSE OF THIS STUDY WAS TO',		1).
valid_abstract_section_header('THE PURPOSE OF THIS STUDY WAS TO DETERMINE',		1).
valid_abstract_section_header('THE PURPOSES OF THIS METHODOLOGICAL PAPER ARE',		1).
valid_abstract_section_header('THE QUALITY MEASURES',		1).
valid_abstract_section_header('THE QUESTION OF SHORT COURSES OF CORTICOSTEROIDS',		1).
valid_abstract_section_header('THE QUESTION TO SOLVE IN THE PRESENT WORK IS',		1).
valid_abstract_section_header('THE RAS SUPERFAMILY OF PROTEINS CONSISTS OF FIVE BRANCHES',		1).
valid_abstract_section_header('THE REALITIES',		1).
valid_abstract_section_header('THE REPORT CARD',		1).
valid_abstract_section_header('THE REPORTS',		1).
valid_abstract_section_header('THE RESEARCH AGENDA',		1).
valid_abstract_section_header('THE RESEARCH OBJECTIVE',		1).
valid_abstract_section_header('THE RESULT AND CONCLUSION',		1).
valid_abstract_section_header('THE RESULTS AND CONCLUSION',		1).
valid_abstract_section_header('THE RESULTS OF HPLC ANALYSIS WERE AS FOLLOWS',		1).
valid_abstract_section_header('THE REVIEW OF FOUR EXPERIMENTAL METHODS',		1).
valid_abstract_section_header('THE RINNE TEST',		1).
valid_abstract_section_header('THE RISKS OF SEDATION AND ANALGESICS',		1).
valid_abstract_section_header('THE RISKS OF TRANSFUSION',		1).
valid_abstract_section_header('THE ROARING NINETIES',		1).
valid_abstract_section_header('THE ROLE OF HERBS AND SPICES IN HEALTH',		1).
valid_abstract_section_header('THE ROLE OF IMMUNOGLOBULIN THERAPY',		1).
valid_abstract_section_header('THE ROLE OF PHYSIOLOGICAL FACTORS AND ANTIMICROBIAL AGENTS IN WOUND HEALING',		1).
valid_abstract_section_header('THE ROLE OF PSYCHIATRY IN MODERN MEDICINE AND SOCIETY',		1).
valid_abstract_section_header('THE ROLE OF THE BREZOVIK HOSPITAL IN THE NATIONAL TB PROGRAM',		1).
valid_abstract_section_header('THE ROLE OF THE CIO',		1).
valid_abstract_section_header('THE ROLE OF THE LEADER',		1).
valid_abstract_section_header('THE ROLE OF THE PSP',		1).
valid_abstract_section_header('THE ROLE OF VITAMIN D IN CALCIUM METABOLISM',		1).
valid_abstract_section_header('THE RURAL FELLOWSHIP PROGRAM',		1).
valid_abstract_section_header('THE SAMPLES WERE DIVIDED INTO THE FOLLOWING GROUPS',		1).
valid_abstract_section_header('THE SCHOLAR GROUP',		1).
valid_abstract_section_header('THE SET OF THE PATIENTS',		1).
valid_abstract_section_header('THE SET OF THE PATIENTS AND THE METHOD',		1).
valid_abstract_section_header('THE SIGNIFICANT FINDING OF THE STUDY',		1).
valid_abstract_section_header('THE SIGNIFICANT FINDINGS OF THE STUDY',		1).
valid_abstract_section_header('THE SIGNIFICANT FINDINGS OF THIS STUDY',		1).
valid_abstract_section_header('THE SOLUTION HYPOTHESIS',		1).
valid_abstract_section_header('THE SOURCE AND NATURE OF VARIABILITY IN DEMAND',		1).
valid_abstract_section_header('THE SOUTHEAST MICHIGAN EXPANSION PROJECT',		1).
valid_abstract_section_header('THE SPECIAL CASE HISTORY',		1).
valid_abstract_section_header('THE STORY',		1).
valid_abstract_section_header('THE STRATEGIES USED IN ONE CANCER NETWORK',		1).
valid_abstract_section_header('THE STUDIES WERE BASED ON',		1).
valid_abstract_section_header('THE SUBJECTS WERE ALLOCATED TO ONE OF TWO TREATMENT ORDERS',		1).
valid_abstract_section_header('THE SURVEY HAD THE FOLLOWING AIMS',		1).
valid_abstract_section_header('THE SURVEYS',		1).
valid_abstract_section_header('THE SYNTHESIS OF SIX THIADIAZOLE NUCLEOSIDE ANALOGS IS REPORTED',		1).
valid_abstract_section_header('THE TAXONOMY OF STAG BEETLES (COLEOPTERA',		1).
valid_abstract_section_header('THE TEAM',		1).
valid_abstract_section_header('THE TIME FOR SURGERY',		1).
valid_abstract_section_header('THE TITLE XANTHONE (SYSTEMATIC NAME',		1).
valid_abstract_section_header('THE TOOL AIMED TO HELP PHYSICIANS ACHIEVE THREE MAIN GOALS',		1).
valid_abstract_section_header('THE TRIAL',		1).
valid_abstract_section_header('THE TRIAL IS REGISTERED',		1).
valid_abstract_section_header('THE TYPE OF STUDY',		1).
valid_abstract_section_header('THE UK RENAL REGISTRY INTERACTIVE DATA PORTAL',		1).
valid_abstract_section_header('THE UNIVERSITY OF WARWICK ORTHODONTIC OUTREACH CENTRE',		1).
valid_abstract_section_header('THE USE OF BLOOD COMPONENTS',		1).
valid_abstract_section_header('THE USE OF COMPARISON CHARTS',		1).
valid_abstract_section_header('THE USE OF OTHER MATERIALS',		1).
valid_abstract_section_header('THE VARIOUS LEADS EXPLORED',		1).
valid_abstract_section_header('THE VIRTUAL SLIDE',		1).
valid_abstract_section_header('THE VIRTUAL SLIDES',		1).
valid_abstract_section_header('THE VISUAL SYSTEM IS SPLIT INTO TWO PROCESSING STREAMS',		1).
valid_abstract_section_header('THEMATA',		1).
valid_abstract_section_header('THEMATIC ANALYSIS',		1).
valid_abstract_section_header('THEME DEVELOPMENT',		1).
valid_abstract_section_header('THEMES OF THE QUALITY STANDARD',		1).
valid_abstract_section_header('THEORETICAL AND EXPERIMENTAL RESULTS',		1).
valid_abstract_section_header('THEORETICAL ASSUMPTIONS',		1).
valid_abstract_section_header('THEORETICAL BACKGROUND AND CURRENT ISSUES',		1).
valid_abstract_section_header('THEORETICAL DESIGN',		1).
valid_abstract_section_header('THEORETICAL FOUNDATION',		1).
valid_abstract_section_header('THEORETICAL FRAMEWORK AND METHOD',		1).
valid_abstract_section_header('THEORETICAL RESULTS',		1).
valid_abstract_section_header('THEORETICAL SIGNAL PROCESSING MODEL',		1).
valid_abstract_section_header('THEORETICAL SYNTHESIS',		1).
valid_abstract_section_header('THEORETICAL UNDERPINNINGS',		1).
valid_abstract_section_header('THEORIES',		1).
valid_abstract_section_header('THEORY & METHODS',		1).
valid_abstract_section_header('THEORY AND LITERATURE REVIEW',		1).
valid_abstract_section_header('THEORY AND MATERIALS AND METHODS',		1).
valid_abstract_section_header('THEORY AND METHOD',		1).
valid_abstract_section_header('THEORY AND METHODOLOGY',		1).
valid_abstract_section_header('THEORY CONSTRUCTION',		1).
valid_abstract_section_header('THEORY IN PRACTICE',		1).
valid_abstract_section_header('THEORY OF ABDUCTIVE INFERENCE',		1).
valid_abstract_section_header('THEORY OF OPERATION',		1).
valid_abstract_section_header('THEORY/METHODS',		1).
valid_abstract_section_header('THERAPEUTIC CHALLENGES',		1).
valid_abstract_section_header('THERAPEUTIC CONCEPTS',		1).
valid_abstract_section_header('THERAPEUTIC DE-ESCALATION IN BREAST CANCER SURGERY',		1).
valid_abstract_section_header('THERAPEUTIC DECISION-MAKING',		1).
valid_abstract_section_header('THERAPEUTIC DESIGNS',		1).
valid_abstract_section_header('THERAPEUTIC EFFICACY',		1).
valid_abstract_section_header('THERAPEUTIC GOAL',		1).
valid_abstract_section_header('THERAPEUTIC IMPLICATIONS AND FUTURE DIRECTIONS',		1).
valid_abstract_section_header('THERAPEUTIC INTERVENTION',		1).
valid_abstract_section_header('THERAPEUTIC INTERVENTION AND OUTCOMES',		1).
valid_abstract_section_header('THERAPEUTIC OPTIONS FOR MODIFYING CARDIOMETABOLIC RISK',		1).
valid_abstract_section_header('THERAPEUTIC PRINCIPLES',		1).
valid_abstract_section_header('THERAPEUTIC TARGETS',		1).
valid_abstract_section_header('THERAPEUTIC USE OF FATTY ACIDS',		1).
valid_abstract_section_header('THERAPEUTIC USE OF RADIONUCLIDES',		1).
valid_abstract_section_header('THERAPEUTICAL CONSEQUENCES',		1).
valid_abstract_section_header('THERAPEUTICAL IMPLICATIONS',		1).
valid_abstract_section_header('THERAPEUTICAL STUDIES',		1).
valid_abstract_section_header('THERAPY MODALITIES',		1).
valid_abstract_section_header('THERAPY OF COMPLICATIONS',		1).
valid_abstract_section_header('THERAPY OF CUP',		1).
valid_abstract_section_header('THERAPY OF GYNECOLOGICAL DISEASE',		1).
valid_abstract_section_header('THERAPY OPTIONS',		1).
valid_abstract_section_header('THERAPY STRATEGIES',		1).
valid_abstract_section_header('THERE ARE TWO DISTINCT ISSUES REGARDING NETWORK VALIDATION',		1).
valid_abstract_section_header('THERE IS A BASIC RULE TO MAMMALIAN NEOCORTICAL EXPANSION',		1).
valid_abstract_section_header('THEREFORE, THE FOLLOWING ACTIONS ARE RECOMMENDED',		1).
valid_abstract_section_header('THESE PROGRAMS PROVIDE',		1).
valid_abstract_section_header('THICKNESS OF COMPACT BONE',		1).
valid_abstract_section_header('THINKING OUTSIDE THE BOX',		1).
valid_abstract_section_header('THIRD',		1).
valid_abstract_section_header('THIS ARTICLE ADDRESSES FOUR INTERRELATED RESEARCH QUESTIONS',		1).
valid_abstract_section_header('THIS ARTICLE HAS BEEN RETRACTED',		1).
valid_abstract_section_header('THIS CASE REPORT',		1).
valid_abstract_section_header('THIS IS SCIENCE',		1).
valid_abstract_section_header('THIS PAPER ADDRESSES THE QUESTION',		1).
valid_abstract_section_header('THIS PAPER AIMS',		1).
valid_abstract_section_header('THIS PAPER DESCRIBES A NOVEL FORMULATION OF ANTINEOPLASTIC DRUG',		1).
valid_abstract_section_header('THIS PAPER PRESENTS TWO CASE STUDIES',		1).
valid_abstract_section_header('THIS PAPER TAKES A NEW LOOK AT AN OLD QUESTION',		1).
valid_abstract_section_header('THIS RETROSPECTIVE STUDY AIMS',		1).
valid_abstract_section_header('THIS REVIEW HOPES TO CLEARLY EXPLAIN THE FOLLOWING VIEWPOINTS',		1).
valid_abstract_section_header('THIS STUDY AIMED TO INVESTIGATE DIETARY CONCENTRATE',		1).
valid_abstract_section_header('THIS STUDY HAD A TWOFOLD PURPOSE',		1).
valid_abstract_section_header('THIS STUDY HAD THE FOLLOWING TWO AIMS',		1).
valid_abstract_section_header('THIS STUDY TESTED TWO HYPOTHESES',		1).
valid_abstract_section_header('THIS STUDY WAS CONDUCTED TO DETERMINE',		1).
valid_abstract_section_header('THIS TRIAL IS REGISTERED IN THE DUTCH TRIAL REGISTER',		1).
valid_abstract_section_header('THREATS AND CHALLENGES TO SUSTAINABILITY',		1).
valid_abstract_section_header('THREE BOUNDARIES WELL CROSSED WOMEN CROSSING BOUNDARIES',		1).
valid_abstract_section_header('THREE CAREER PHASES',		1).
valid_abstract_section_header('THREE CORE CONCEPTS',		1).
valid_abstract_section_header('THREE EXAMPLES OF SUCCESSFUL PROJECTS',		1).
valid_abstract_section_header('THREE GROUPS WERE EVALUATED',		1).
valid_abstract_section_header('THREE ILLUSTRATIVE CASES',		1).
valid_abstract_section_header('THREE QUESTIONNAIRES WERE GENERATED',		1).
valid_abstract_section_header('THREE TERMS DEFINE BRAIN BEHAVIORAL LATERALITY',		1).
valid_abstract_section_header('THREE TYPES OF HEREDITARY ANGIOEDEMA (HAE) HAVE BEEN DESCRIBED',		1).
valid_abstract_section_header('THROMBOPHILIA, PRETHROMBOTIC STATE AND THROMBOSIS',		1).
valid_abstract_section_header('THROUGH-AND-THROUGH LESIONS',		1).
valid_abstract_section_header('TIBIAL APOPHYSIS',		1).
valid_abstract_section_header('TIDES SOCIAL MARKETING APPROACH',		1).
valid_abstract_section_header('TIME AND PLACE OF STUDY',		1).
valid_abstract_section_header('TIME COURSE AND ENROLLMENT',		1).
valid_abstract_section_header('TIME OF DAY',		1).
valid_abstract_section_header('TIME OF DIAGNOSIS',		1).
valid_abstract_section_header('TIMEBOMB',		1).
valid_abstract_section_header('TIMELINESS OF CARE DELIVERY',		1).
valid_abstract_section_header('TIMELY AREAS FOR DEVELOPMENT',		1).
valid_abstract_section_header('TIMESCALE',		1).
valid_abstract_section_header('TIMING AND METHODS',		1).
valid_abstract_section_header('TIMING OF EPIPHYSIODESIS',		1).
valid_abstract_section_header('TIMING OF MEASUREMENT',		1).
valid_abstract_section_header('TIPS AND TRICKS',		1).
valid_abstract_section_header('TITLE OF THE STUDY',		1).
valid_abstract_section_header('TKI) PRACTICAL RECOMMENDATIONS',		1).
valid_abstract_section_header('TNM STAGING SYSTEM',		1).
valid_abstract_section_header('TO INDUCE VASODILATION',		1).
valid_abstract_section_header('TO PERMIT DIAGNOSIS',		1).
valid_abstract_section_header('TO TREAT HEMOSTATIC ABNORMALITIES',		1).
valid_abstract_section_header('TOC IMAGE',		1).
valid_abstract_section_header('TOMADO',		1).
valid_abstract_section_header('TOMOSCINTIGRAPHY (SPET) AND POSITRON TOMOGRAPHY (PET)',		1).
valid_abstract_section_header('TONGUE THRUST',		1).
valid_abstract_section_header('TOOL DESCRIPTION',		1).
valid_abstract_section_header('TOOL DEVELOPMENT',		1).
valid_abstract_section_header('TOOLS AND METHOD',		1).
valid_abstract_section_header('TOOLS AND TECHNIQUES',		1).
valid_abstract_section_header('TOOLS FOR EARLY DIAGNOSIS',		1).
valid_abstract_section_header('TOPIC DEFINITION AND INCLUSION CRITERIA',		1).
valid_abstract_section_header('TOPICALITY',		1).
valid_abstract_section_header('TOPICS AND RESULTS',		1).
valid_abstract_section_header('TOPICS OF DISCUSSION',		1).
valid_abstract_section_header('TOPICS UNDER REVIEW',		1).
valid_abstract_section_header('TORI (SINGULAR',		1).
valid_abstract_section_header('TOTAL VARIANCE',		1).
valid_abstract_section_header('TOWARD A UNIFORM STUDY PROTOCOL',		1).
valid_abstract_section_header('TOWARDS A TYPOLOGY',		1).
valid_abstract_section_header('TOWARDS INTEGRATED CARE IN SWITZERLAND',		1).
valid_abstract_section_header('TOXIC EFFECTS',		1).
valid_abstract_section_header('TOXIC INDUSTRIAL CHEMICALS',		1).
valid_abstract_section_header('TOXIC PRODUCTS',		1).
valid_abstract_section_header('TRACKS OF WORK',		1).
valid_abstract_section_header('TRAIL REGISTRATION ANZCTR',		1).
valid_abstract_section_header('TRAILBLAZING',		1).
valid_abstract_section_header('TRAIN-TO-HOME INTERVENTION',		1).
valid_abstract_section_header('TRAINING AND CALIBRATION',		1).
valid_abstract_section_header('TRAINING IN IMPLICIT BIAS ENHANCES CULTURAL COMPETENCE',		1).
valid_abstract_section_header('TRAINING OF PROFESSIONALS',		1).
valid_abstract_section_header('TRAINING PROGRAMME AND RESEARCH DESIGN',		1).
valid_abstract_section_header('TRAINING SYSTEM',		1).
valid_abstract_section_header('TRAITEMENT',		1).
valid_abstract_section_header('TRANSCEND II IMPLANTABLE GASTRIC STIMULATION SYSTEM',		1).
valid_abstract_section_header('TRANSCRIPT PROFILING ACCESSION NUMBER',		1).
valid_abstract_section_header('TRANSGLUTAMINASE ACTIVITY IS UBIQUITOUS',		1).
valid_abstract_section_header('TRANSITIONAL RELEVANCE',		1).
valid_abstract_section_header('TRANSKRIPTIONSKONTROLLE DER MESENCHYMALEN STAMMZELLDIFFERENZIERUNG',		1).
valid_abstract_section_header('TRANSLATIONAL PERSPECTIVE',		1).
valid_abstract_section_header('TRANSLATIONAL STUDY PROTOCOLS',		1).
valid_abstract_section_header('TRANSLATIONS IN PROCESS',		1).
valid_abstract_section_header('TRANSMISSION OF ZOONOSES TO HUMANS',		1).
valid_abstract_section_header('TRANSPLANTATION OF AUTOLOGOUS CELLS',		1).
valid_abstract_section_header('TRAUMA DATA STRUCTURE DEVELOPMENT',		1).
valid_abstract_section_header('TRAUMATIC PSYCHOSIS CONCEPT',		1).
valid_abstract_section_header('TREATING THE PATIENT AS AN INDIVIDUAL',		1).
valid_abstract_section_header('TREATMENT AIM',		1).
valid_abstract_section_header('TREATMENT AIMS',		1).
valid_abstract_section_header('TREATMENT ALGORITHM',		1).
valid_abstract_section_header('TREATMENT ALGORITHM AND CLASSIFICATION',		1).
valid_abstract_section_header('TREATMENT ALGORITHMS',		1).
valid_abstract_section_header('TREATMENT AND RESPONSE TO THERAPY',		1).
valid_abstract_section_header('TREATMENT APPROACHES',		1).
valid_abstract_section_header('TREATMENT ASPECTS',		1).
valid_abstract_section_header('TREATMENT COMMENCED',		1).
valid_abstract_section_header('TREATMENT CONCEPT',		1).
valid_abstract_section_header('TREATMENT EFFICACY',		1).
valid_abstract_section_header('TREATMENT FOR GASTROPARESIS',		1).
valid_abstract_section_header('TREATMENT FOR MORBID OBESITY',		1).
valid_abstract_section_header('TREATMENT GROUPS',		1).
valid_abstract_section_header('TREATMENT IMPLICATIONS',		1).
valid_abstract_section_header('TREATMENT METHODS',		1).
valid_abstract_section_header('TREATMENT MODEL',		1).
valid_abstract_section_header('TREATMENT OF ABDOMINAL AORTIC ANEURYSMS',		1).
valid_abstract_section_header('TREATMENT OF ADVANCED DISEASE',		1).
valid_abstract_section_header('TREATMENT OF BLADDER CANCER',		1).
valid_abstract_section_header('TREATMENT OF BONE TUMORS',		1).
valid_abstract_section_header('TREATMENT OF CARDIOVASCULAR REMODELING',		1).
valid_abstract_section_header('TREATMENT OF CENTRAL DIABETES INSIPIDUS',		1).
valid_abstract_section_header('TREATMENT OF CHRONIC HCV INFECTION',		1).
valid_abstract_section_header('TREATMENT OF CMV RETINITIS',		1).
valid_abstract_section_header('TREATMENT OF CRANIOVERTEBRAL JUNCTION ABNORMALITIES',		1).
valid_abstract_section_header('TREATMENT OF GAS INFECTIONS',		1).
valid_abstract_section_header('TREATMENT OF INFLUENZA',		1).
valid_abstract_section_header('TREATMENT OF LOW HDL CHOLESTEROL',		1).
valid_abstract_section_header('TREATMENT OF NEPHROPATHY',		1).
valid_abstract_section_header('TREATMENT OF NSCLC WITH NIVOLUMAB',		1).
valid_abstract_section_header('TREATMENT OF PATIENTS WITH NEUROCOGNITIVE DECLINE',		1).
valid_abstract_section_header('TREATMENT OF PHN',		1).
valid_abstract_section_header('TREATMENT OF RECURRENT EPISTAXIS',		1).
valid_abstract_section_header('TREATMENT OPTIONS FOR BONE PAIN',		1).
valid_abstract_section_header('TREATMENT OUTCOMES',		1).
valid_abstract_section_header('TREATMENT PLANNING',		1).
valid_abstract_section_header('TREATMENT PROGRAM',		1).
valid_abstract_section_header('TREATMENT PROGRAMME',		1).
valid_abstract_section_header('TREATMENT PROGRESS AND RESULTS',		1).
valid_abstract_section_header('TREATMENT RESULTS',		1).
valid_abstract_section_header('TREATMENT RESULTS AND CONCLUSIONS',		1).
valid_abstract_section_header('TREATMENT SELECTION AND PLANNING',		1).
valid_abstract_section_header('TREATMENT UTILIZING ATTACHMENT THEORY',		1).
valid_abstract_section_header('TREATMENT, COURSE AND OUTCOME',		1).
valid_abstract_section_header('TREATMENT/COURSE',		1).
valid_abstract_section_header('TREATMENT/THERAPY',		1).
valid_abstract_section_header('TREATMENTS/DISCUSSION',		1).
valid_abstract_section_header('TREND',		1).
valid_abstract_section_header('TRENDS',		1).
valid_abstract_section_header('TRENDS IN CHILDHOOD OBESITY IN GENERAL, AND SPECIFICALLY AMONG CHILDREN WITH RENAL DISEASES',		1).
valid_abstract_section_header('TRIAL DESIGN AND PARTICIPANTS',		1).
valid_abstract_section_header('TRIAL DESIGN, METHODS AND FINDINGS',		1).
valid_abstract_section_header('TRIAL DETAILS',		1).
valid_abstract_section_header('TRIAL EXPERIENCE',		1).
valid_abstract_section_header('TRIAL GRANT NUMBER',		1).
valid_abstract_section_header('TRIAL ID',		1).
valid_abstract_section_header('TRIAL IDENTIFIER',		1).
valid_abstract_section_header('TRIAL INFORMATION',		1).
valid_abstract_section_header('TRIAL INTERNATIONAL REGISTRATION',		1).
valid_abstract_section_header('TRIAL NATIONAL REGISTRATION',		1).
valid_abstract_section_header('TRIAL PROCESSES AND DATA COLLECTION',		1).
valid_abstract_section_header('TRIAL REGISTATION',		1).
valid_abstract_section_header('TRIAL REGISTATION NUMBER',		1).
valid_abstract_section_header('TRIAL REGISTER DATE',		1).
valid_abstract_section_header('TRIAL REGISTERED AT',		1).
valid_abstract_section_header('TRIAL REGISTERY',		1).
valid_abstract_section_header('TRIAL REGISTRAION',		1).
valid_abstract_section_header('TRIAL REGISTRATION AND FUNDING',		1).
valid_abstract_section_header('TRIAL REGISTRATION AND PROTOCOL',		1).
valid_abstract_section_header('TRIAL REGISTRATION CLINICALTRIALSGOV',		1).
valid_abstract_section_header('TRIAL REGISTRATION CLINICALTRIALSGOV IDENTIFIER',		1).
valid_abstract_section_header('TRIAL REGISTRATION CLINICALTRIALSGOV NUMBER',		1).
valid_abstract_section_header('TRIAL REGISTRATION CRIS',		1).
valid_abstract_section_header('TRIAL REGISTRATION DRKS',		1).
valid_abstract_section_header('TRIAL REGISTRATION EBIS',		1).
valid_abstract_section_header('TRIAL REGISTRATION EINSTEIN-PE',		1).
valid_abstract_section_header('TRIAL REGISTRATION EUDRACT',		1).
valid_abstract_section_header('TRIAL REGISTRATION ISCTN',		1).
valid_abstract_section_header('TRIAL REGISTRATION ISRCTN NO',		1).
valid_abstract_section_header('TRIAL REGISTRATION ISRNCT',		1).
valid_abstract_section_header('TRIAL REGISTRATION NAME',		1).
valid_abstract_section_header('TRIAL REGISTRATION NUMBER (ANZCTR)',		1).
valid_abstract_section_header('TRIAL REGISTRATION NUMBER NMRR ID',		1).
valid_abstract_section_header('TRIAL REGISTRATION NUMBER PROSPERO',		1).
valid_abstract_section_header('TRIAL REGISTRATION PRIMARY STUDY',		1).
valid_abstract_section_header('TRIAL REGISTRATION PRIMARY TRIAL',		1).
valid_abstract_section_header('TRIAL REGISTRATION REFERENCE',		1).
valid_abstract_section_header('TRIAL REGISTRATION TRIALREGISTERNL IDENTIFIER',		1).
valid_abstract_section_header('TRIAL REGISTRATION TRIGGER',		1).
valid_abstract_section_header('TRIAL REGISTRATIONS COMBO I',		1).
valid_abstract_section_header('TRIAL REGISTRATON',		1).
valid_abstract_section_header('TRIAL REGISTRY INFORMATION',		1).
valid_abstract_section_header('TRIAL REGISTRY NUMBERS',		1).
valid_abstract_section_header('TRIAL RIGISTRATION',		1).
valid_abstract_section_header('TRIAL RUN AND EVALUATION',		1).
valid_abstract_section_header('TRIAL-REGISTRATION',		1).
valid_abstract_section_header('TRIALS AND REGISTRATION',		1).
valid_abstract_section_header('TRIALS IN EXPERIMENTAL HUTS',		1).
valid_abstract_section_header('TRIALS REGISTRATION NUMBERS',		1).
valid_abstract_section_header('TRIALS REGISTRATIONS',		1).
valid_abstract_section_header('TRIALS REGISTRATIONS NUMBER',		1).
valid_abstract_section_header('TRIALS REGISTRY NUMBER',		1).
valid_abstract_section_header('TRIALS\' NUMBERS',		1).
valid_abstract_section_header('TRIGGERING MECHANISMS',		1).
valid_abstract_section_header('TROPICAL CAM HABITATS',		1).
valid_abstract_section_header('TROPONIN MEASUREMENT',		1).
valid_abstract_section_header('TT CONCLUSION',		1).
valid_abstract_section_header('TUBERCULOSIS (TB) IS A MAJOR PUBLIC HEALTH CONCERN WORLDWIDE',		1).
valid_abstract_section_header('TUJUAN',		1).
valid_abstract_section_header('TUMOR BIOLOGY',		1).
valid_abstract_section_header('TUMOR SPREAD',		1).
valid_abstract_section_header('TUMORAL MRNA EXPRESSION OF IGF-RELATED GENES (IGFS',		1).
valid_abstract_section_header('TUMOUR CHARACTERISTICS',		1).
valid_abstract_section_header('TUTORIAL',		1).
valid_abstract_section_header('TWEET',		1).
valid_abstract_section_header('TWENTY FOUR PREGNANT NMRI FEMALE MICE (W',		1).
valid_abstract_section_header('TWO ELECTRONIC APPENDICES ARE PROVIDED',		1).
valid_abstract_section_header('TWO MAIN TYPES OF MACROPHAGE FUNCTIONS ARE KNOWN',		1).
valid_abstract_section_header('TWO NEW SPECIES',		1).
valid_abstract_section_header('TWO NEW SPECIES OF HARVESTMAN (OPILIONES',		1).
valid_abstract_section_header('TWO NEW SPECIES OF LEAFHOPPERS',		1).
valid_abstract_section_header('TWO PROCESSES PERTAIN TO ALL DRUGS',		1).
valid_abstract_section_header('TWO TARGETS',		1).
valid_abstract_section_header('TWO THERAPEUTIC PHASES',		1).
valid_abstract_section_header('TWO TYPES OF OA',		1).
valid_abstract_section_header('TWO TYPES OF REACTIONS',		1).
valid_abstract_section_header('TWO VARIANTS OF THE INOSINE TRIPHOSPHATASE (ITPA',		1).
valid_abstract_section_header('TYPE A INFLUENZA',		1).
valid_abstract_section_header('TYPE IB',		1).
valid_abstract_section_header('TYPE OF ANESTHETIC EMPLOYED',		1).
valid_abstract_section_header('TYPE OF HOSPITAL',		1).
valid_abstract_section_header('TYPE OF INTERVENTION',		1).
valid_abstract_section_header('TYPE OF REVIEW AND EVALUATION METHOD',		1).
valid_abstract_section_header('TYPE OF SPORTS',		1).
valid_abstract_section_header('TYPE OF STUDY AND LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('TYPE OF STUDY AND LEVEL OF PROOF',		1).
valid_abstract_section_header('TYPE OF STUDY/LEVEL OF DISEASE',		1).
valid_abstract_section_header('TYPE OF THE STUDY AND SETTING',		1).
valid_abstract_section_header('TYPE OF VASCULAR ACCESS IN PREVALENT DIALYSIS PATIENTS',		1).
valid_abstract_section_header('TYPE STUDY/LEVEL OF EVIDENCE',		1).
valid_abstract_section_header('TYPES',		1).
valid_abstract_section_header('TYPES OF BEDSIDE TEACHING',		1).
valid_abstract_section_header('TYPES OF BENCHMARKING',		1).
valid_abstract_section_header('TYPES OF COMPARATOR',		1).
valid_abstract_section_header('TYPES OF DEVICES',		1).
valid_abstract_section_header('TYPES OF EXPOSURE',		1).
valid_abstract_section_header('TYPES OF INCONTINENCE',		1).
valid_abstract_section_header('TYPES OF INTERVENTION',		1).
valid_abstract_section_header('TYPES OF INTERVENTIONS AND COMPARATORS',		1).
valid_abstract_section_header('TYPES OF LASER DEVICES',		1).
valid_abstract_section_header('TYPES OF PARTICIPANT',		1).
valid_abstract_section_header('TYPES OF POLYUNSATURATED FATTY ACIDS (PUFA)',		1).
valid_abstract_section_header('TYPES OF STUDY',		1).
valid_abstract_section_header('Target Audience and Patient Population',		1).
valid_abstract_section_header('Trial registration',		1).
valid_abstract_section_header('UK GENETIC PROSTATE CANCER STUDY',		1).
valid_abstract_section_header('ULCER COMPLICATIONS',		1).
valid_abstract_section_header('ULCERATIVE COLITIS',		1).
valid_abstract_section_header('ULTRASOUND ARTIFACTS',		1).
valid_abstract_section_header('ULTRASOUND FINDINGS',		1).
valid_abstract_section_header('ULTRASOUND IN DIAGNOSTICS',		1).
valid_abstract_section_header('ULTRASOUND IN MONITORING INTRACRANIAL HEMORRHAGE',		1).
valid_abstract_section_header('UMIN CLINICAL TRIAL REGISTRY',		1).
valid_abstract_section_header('UMIN CLINICAL TRIALS REGISTRY IDENTIFIER',		1).
valid_abstract_section_header('UMIN REGISTRATION NUMBER',		1).
valid_abstract_section_header('UMIN-CTR STUDY DESIGN',		1).
valid_abstract_section_header('UNAFFECTED HEMISPHERE',		1).
valid_abstract_section_header('UNANTICIPATED OUTCOMES',		1).
valid_abstract_section_header('UNCERTAINTIES NOT ADDRESSED IN THE LITERATURE',		1).
valid_abstract_section_header('UNCERTAINTY',		1).
valid_abstract_section_header('UNCOMPLICATED MILD HYPERTENSION',		1).
valid_abstract_section_header('UNCORRECTED CONGENITAL LESIONS',		1).
valid_abstract_section_header('UNDENIABLE PROGRESS',		1).
valid_abstract_section_header('UNDERPINNING ISSUES',		1).
valid_abstract_section_header('UNDERSTANDABILITY',		1).
valid_abstract_section_header('UNDERSTANDING CONTEXT, PROCESS AND OUTCOMES',		1).
valid_abstract_section_header('UNDERTAKING A CONFLICT ASSESSMENT',		1).
valid_abstract_section_header('UNENHANCED HELICAL COMPUTED TOMOGRAPHY',		1).
valid_abstract_section_header('UNILATERAL CLEFT LIP REPAIR',		1).
valid_abstract_section_header('UNION',		1).
valid_abstract_section_header('UNIQUE METABOLIC FEATURES',		1).
valid_abstract_section_header('UNIQUE PRODUCT IDENTIFIER',		1).
valid_abstract_section_header('UNIT ASSESSMENT TOOL',		1).
valid_abstract_section_header('UNIT OF ANALYSIS',		1).
valid_abstract_section_header('UNIT OR TEAM LEVEL',		1).
valid_abstract_section_header('UNKNOWNS',		1).
valid_abstract_section_header('UNMET NEEDS',		1).
valid_abstract_section_header('UNPREDICTABLE NATURAL HISTORY',		1).
valid_abstract_section_header('UNSOLVED PROBLEM',		1).
valid_abstract_section_header('UNSTRUCTURED ABSTRACT',		1).
valid_abstract_section_header('UNUSUAL FEATURES',		1).
valid_abstract_section_header('UPDATED KNOWLEDGE',		1).
valid_abstract_section_header('UREMIC TOXINS',		1).
valid_abstract_section_header('URINARY MARKERS AND FETAL CELLS IN MATERNAL BLOOD',		1).
valid_abstract_section_header('UROLOGIC SURGERY',		1).
valid_abstract_section_header('URPOSE',		1).
valid_abstract_section_header('US FOOD AND DRUG ADMINISTRATION ADVERSE EVENT REPORTING SYSTEM',		1).
valid_abstract_section_header('US PARTICIPANTS',		1).
valid_abstract_section_header('US) CONCLUSIONS',		1).
valid_abstract_section_header('USE OF BIOLOGICAL WEAPONS',		1).
valid_abstract_section_header('USE OF ESWT',		1).
valid_abstract_section_header('USE OF PATIENT FEEDBACK',		1).
valid_abstract_section_header('USE OF PATIENT FEEDBACK AT HARBORVIEW',		1).
valid_abstract_section_header('USE OF VIRTUAL REALITY',		1).
valid_abstract_section_header('USEFUL DRUGS',		1).
valid_abstract_section_header('USEFUL LINK',		1).
valid_abstract_section_header('USEFUL TREATMENT',		1).
valid_abstract_section_header('USEFUL WEBPAGES',		1).
valid_abstract_section_header('USES IN CLINICAL PRACTICE',		1).
valid_abstract_section_header('USES OF NARRATIVE INQUIRY',		1).
valid_abstract_section_header('USG CHARACTERISTICS',		1).
valid_abstract_section_header('USING BAR CODES TO ELIMINATE MEDICATION ERRORS',		1).
valid_abstract_section_header('USING DATA TO PLAN PROCESS IMPROVEMENTS',		1).
valid_abstract_section_header('USING EXPERIMENTAL METHODS TO MAKE QI DECISIONS',		1).
valid_abstract_section_header('USING LIST',		1).
valid_abstract_section_header('USING THE DATABASE',		1).
valid_abstract_section_header('USING THE IOM FRAMEWORK FOR ALL PUBLIC REPORTING ON QUALITY',		1).
valid_abstract_section_header('USING THE SCORECARD FOR QUALITY IMPROVEMENT (QI)',		1).
valid_abstract_section_header('USING THE SOFTWARE TOOL',		1).
valid_abstract_section_header('USUALLY, TREATMENT OF IMPACTION INCLUDES',		1).
valid_abstract_section_header('UTILITY OF MEASUREMENT OF VIRAL LOAD',		1).
valid_abstract_section_header('UTILIZATION',		1).
valid_abstract_section_header('V DYSPHONIAS CAUSED BY PRIMARY ENDOCRINE DISORDERS',		1).
valid_abstract_section_header('VACCINAL EFFICACY',		1).
valid_abstract_section_header('VACCINE',		1).
valid_abstract_section_header('VACCINES FOR HPV INFECTIONS',		1).
valid_abstract_section_header('VAKA',		1).
valid_abstract_section_header('VALIDATION AND RESULTS',		1).
valid_abstract_section_header('VALIDATION AND VALIDITY',		1).
valid_abstract_section_header('VALIDATION BY EXAMPLE',		1).
valid_abstract_section_header('VALIDITY OF THE POSTSTROKE DEPRESSION DIAGNOSIS',		1).
valid_abstract_section_header('VALPROIC ACID AND CARNITINE',		1).
valid_abstract_section_header('VALUATION',		1).
valid_abstract_section_header('VALUE AND A COMPLEX HEALTHCARE MARKET',		1).
valid_abstract_section_header('VALUE AND USES OF FORMALIN FIXED TISSUE',		1).
valid_abstract_section_header('VALUE AND USES OF FROZEN TISSUE',		1).
valid_abstract_section_header('VALUE OF PROJECT',		1).
valid_abstract_section_header('VALUE OF STUDY',		1).
valid_abstract_section_header('VARIABLE OF INTEREST',		1).
valid_abstract_section_header('VARIABLES ANALYZED',		1).
valid_abstract_section_header('VARIABLES IN EVALUATING SAFETY',		1).
valid_abstract_section_header('VARIATIONS BY TIME, GEOGRAPHY, AND RACE',		1).
valid_abstract_section_header('VARIOLA MAJOR',		1).
valid_abstract_section_header('VARIOLA MINOR',		1).
valid_abstract_section_header('VARIOUS FORMS',		1).
valid_abstract_section_header('VASCULAR ANOMALIES ARE DIVIDED INTO TWO MAIN GROUPS',		1).
valid_abstract_section_header('VASCULAR DEPRESSION',		1).
valid_abstract_section_header('VASCULAR EFFECTS OF NEWER ANTIHYPERTENSIVE AGENTS',		1).
valid_abstract_section_header('VASCULAR LASERS',		1).
valid_abstract_section_header('VASCULITIS',		1).
valid_abstract_section_header('VASCULITIS ASSOCIATED WITH MIXED ESSENTIAL CRYOGLOBULINEMIA',		1).
valid_abstract_section_header('VASOACTIVE DRUG TREATMENT AFTER STROKE',		1).
valid_abstract_section_header('VECTORS',		1).
valid_abstract_section_header('VEGETATIONS',		1).
valid_abstract_section_header('VEGF) CONCLUSION',		1).
valid_abstract_section_header('VEIN CUFFS',		1).
valid_abstract_section_header('VENOUS THROMBOEMBOLISM',		1).
valid_abstract_section_header('VENOUS THROMBOEMBOLISM PREVENTION IN STROKE',		1).
valid_abstract_section_header('VERAPAMIL AND TRANDOLAPRIL',		1).
valid_abstract_section_header('VERGENCE',		1).
valid_abstract_section_header('VERGENCE TESTING FROM 8 PATIENTS AND 15 CONTROLS',		1).
valid_abstract_section_header('VERIFICATION OF THE METHOD',		1).
valid_abstract_section_header('VI DYSPHONIAS CAUSED BY COMPLEX PROFESSIONAL REASONS',		1).
valid_abstract_section_header('VII DYSPHONIAS CAUSED BY PRIMARY DISPLASTIC DISORDERS',		1).
valid_abstract_section_header('VIII DYSPHONIAS CAUSED BY LARYNGEAL TUMORS',		1).
valid_abstract_section_header('VIRAL INFECTION - FELV',		1).
valid_abstract_section_header('VIRAL INFECTION - FIV',		1).
valid_abstract_section_header('VIROLOGY',		1).
valid_abstract_section_header('VIRTUAL REALITY',		1).
valid_abstract_section_header('VIRTUAL WIND TUNNEL',		1).
valid_abstract_section_header('VIRULENCE',		1).
valid_abstract_section_header('VIRUS ACCESSION NUMBER',		1).
valid_abstract_section_header('VIRUS CODE',		1).
valid_abstract_section_header('VIRUS NEUTRALIZING',		1).
valid_abstract_section_header('VIRUS PERSISTENCE DESPITE SUCCESSFUL THERAPY',		1).
valid_abstract_section_header('VISCERAL FAT',		1).
valid_abstract_section_header('VISION',		1).
valid_abstract_section_header('VISION FOR HEALTHY COMMUNITIES',		1).
valid_abstract_section_header('VISITATION PROGRAM',		1).
valid_abstract_section_header('VITAMIN D DEFICIENCY',		1).
valid_abstract_section_header('VOJVODINA',		1).
valid_abstract_section_header('VPA-ER',		1).
valid_abstract_section_header('VULNERABILITY CONCEPT',		1).
valid_abstract_section_header('VULNERABILITY OF CHILDREN WITHOUT PARENTAL CARE',		1).
valid_abstract_section_header('VULNERABILITY OF NERVE CELLS TO OXIDATIVE STRESS',		1).
valid_abstract_section_header('WAITING LIST INCLUSION CRITERIA',		1).
valid_abstract_section_header('WANTED',		1).
valid_abstract_section_header('WARFARIN',		1).
valid_abstract_section_header('WDXRF',		1).
valid_abstract_section_header('WE AIMED',		1).
valid_abstract_section_header('WE ARE RARELY PERFECTLY STILL',		1).
valid_abstract_section_header('WE BRIDGE THE GAP BETWEEN TWO ISSUES IN INFANT DEVELOPMENT',		1).
valid_abstract_section_header('WE DESCRIBE A NEW SPECIES OF AMBLYOPSID CAVEFISH (PERCOPSIFORMES',		1).
valid_abstract_section_header('WE DESCRIBE THREE NEW MYXOSPOREAN SPECIES',		1).
valid_abstract_section_header('WE DESCRIBE TWO CASES WITH PSEUDOMEMBRANOUS COLITIS',		1).
valid_abstract_section_header('WE MEASURED TWO CATEGORIES',		1).
valid_abstract_section_header('WE PRESENT A CASE WITH A SEVERE INJECTION ERROR',		1).
valid_abstract_section_header('WE PRESENT THREE CASES OF PELVIC PNET',		1).
valid_abstract_section_header('WEANING',		1).
valid_abstract_section_header('WEB CALCULATOR',		1).
valid_abstract_section_header('WEBSITE',		1).
valid_abstract_section_header('WEIGHT',		1).
valid_abstract_section_header('WEIGHT LOSS 2 TRIALS',		1).
valid_abstract_section_header('WHAT DOES THIS REVIEW ADD',		1).
valid_abstract_section_header('WHAT DOES THIS STUDY/REVIEW ADD',		1).
valid_abstract_section_header('WHAT IS ALREADY KNOWN ON THE SUBJECT',		1).
valid_abstract_section_header('WHAT IS ALREADY KNOWN ON THIS SUBJECT',		1).
valid_abstract_section_header('WHAT IS CURRENT KNOWLEDGE',		1).
valid_abstract_section_header('WHAT IS KNOW ALREADY',		1).
valid_abstract_section_header('WHAT IS KNOWN ABOUT THIS SUBJECT',		1).
valid_abstract_section_header('WHAT IS KNOWN ALREADY AND WHAT THIS PAPER ADDS',		1).
valid_abstract_section_header('WHAT IS KNOWN AND CONCLUSION',		1).
valid_abstract_section_header('WHAT IS KNOWN AND THE OBJECTIVE',		1).
valid_abstract_section_header('WHAT IS KNOWN ON THIS SUBJECT',		1).
valid_abstract_section_header('WHAT IS NEW AND OBJECTIVE',		1).
valid_abstract_section_header('WHAT IS NEW AND OBJECTIVES',		1).
valid_abstract_section_header('WHAT IS NEW HERE',		1).
valid_abstract_section_header('WHAT IS NOW AND CONCLUSION',		1).
valid_abstract_section_header('WHAT OUTCOMES SHOULD TRIGGER DISCLOSURE',		1).
valid_abstract_section_header('WHAT THIS RESEARCH ADDS',		1).
valid_abstract_section_header('WHAT WAS CONCLUDED',		1).
valid_abstract_section_header('WHAT WAS FOUND',		1).
valid_abstract_section_header('WHAT WAS UNIQUE',		1).
valid_abstract_section_header('WHAT WE STILL NEED TO KNOW',		1).
valid_abstract_section_header('WHAT\'S NEW IN THIS MANUSCRIPT',		1).
valid_abstract_section_header('WHATS IS NEW AND CONCLUSION',		1).
valid_abstract_section_header('WHEN AND FOR WHOM',		1).
valid_abstract_section_header('WHERE THIS PIECE FITS',		1).
valid_abstract_section_header('WHO PS',		1).
valid_abstract_section_header('WHY IT MATTERS',		1).
valid_abstract_section_header('WHY THE GUIDELINE WAS CHANGED',		1).
valid_abstract_section_header('WIDE OPEN MOUTH',		1).
valid_abstract_section_header('WIDE RANGE OF QUINAZOLINONE BIOLOGICAL PROPERTIES INCLUDING',		1).
valid_abstract_section_header('WIDER IMPLICATION OF FINDINGS',		1).
valid_abstract_section_header('WIDER IMPLICATIONS OF THE STUDY',		1).
valid_abstract_section_header('WINDER IMPLICATIONS OF THE FINDINGS',		1).
valid_abstract_section_header('WITHOUT GOALS YOU CANNOT IMPROVE',		1).
valid_abstract_section_header('WNIOSEK',		1).
valid_abstract_section_header('WOMEN AND METHODS',		1).
valid_abstract_section_header('WOMEN IN THE NEUROSURGERY WORKFORCE',		1).
valid_abstract_section_header('WORD COUNT',		1).
valid_abstract_section_header('WORK DONE',		1).
valid_abstract_section_header('WORK OF RMOS',		1).
valid_abstract_section_header('WORKFORCE',		1).
valid_abstract_section_header('WORKING EXAMPLE',		1).
valid_abstract_section_header('WORKING MEMORY (WM) INVOLVES THREE COGNITIVE EVENTS',		1).
valid_abstract_section_header('WORKING ON AIDS',		1).
valid_abstract_section_header('WORKING PLAN AND METHODS',		1).
valid_abstract_section_header('WORKING PROCEDURE',		1).
valid_abstract_section_header('WORKING THEORY',		1).
valid_abstract_section_header('WORKING WITH THE BIOME',		1).
valid_abstract_section_header('WORKSHOP AGENDA',		1).
valid_abstract_section_header('WORKSHOP CONTEXT',		1).
valid_abstract_section_header('WORKSHOP DESIGN',		1).
valid_abstract_section_header('WORKSHOP FORMAT',		1).
valid_abstract_section_header('WORKSHOP STRUCTURE',		1).
valid_abstract_section_header('WORKSHOP STRUCTURE AND PROCESS',		1).
valid_abstract_section_header('WORLD TRADE ORGANIZATION AGREEMENTS',		1).
valid_abstract_section_header('WORLDWIDE',		1).
valid_abstract_section_header('WOUND CARE NURSING',		1).
valid_abstract_section_header('WOUNDS',		1).
valid_abstract_section_header('WOVEN BONE ORIGIN STUDIES',		1).
