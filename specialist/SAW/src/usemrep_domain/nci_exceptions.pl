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
:- module( exceptions_DOM, [
		concept_to_ignore_DOM/2,
		conditional_empty_head_base_1_DOM/2,
		conditional_empty_head_base_2N_DOM/3,
		empty_head_base_1_DOM/1,
		empty_head_base_2N_DOM/2,
		ignore_semnet_access_term_DOM/1,
		ignore_type_relation_type_DOM/3,
		non_prepositionally_cued_object_DOM/1,
		non_prepositionally_cued_subject_DOM/1,
	 transform_semnet_access_term_DOM/2,
	 transform_type_relation_type_DOM/6
]).

:- use_module( skr_lib( sicstus_utils ), [
	 lower/2
]).

conditional_empty_head_base_1_DOM(_, _) :- !, fail.

conditional_empty_head_base_2N_DOM(_, _, _) :- !, fail.

non_prepositionally_cued_object_DOM(_) :- !, fail.

non_prepositionally_cued_subject_DOM(_) :- !, fail.

%      Do Not Modify This File    %
%     It is machine generated.    %
%	ignore_type_relation_type_GEN/3,
%	transform_type_relation_type_GEN/6
% File:	    exceptions.pl
% Module:   empty_heads, concepts_to_ignore
% Author:   FML/CA
% Purpose:  database of empty-head terms and MT concepts to ignore compiled by CA
% ----- Module declaration and exported predicates
%		ignore_type_relation_type_GEN/3,
% 		transform_type_relation_type_GEN/6
empty_head_base_1_DOM(acetylation).
empty_head_base_1_DOM(activity).
%empty_head_base_1_DOM(activation).
empty_head_base_1_DOM(addition).
empty_head_base_1_DOM(advance). % GR 05/07/09
empty_head_base_1_DOM(age).
empty_head_base_1_DOM(alkylation).
empty_head_base_1_DOM(allele).
empty_head_base_1_DOM(allelic).
empty_head_base_1_DOM(amount).
empty_head_base_1_DOM(analog).
empty_head_base_1_DOM(assisting). % GR 05/06/09
empty_head_base_1_DOM(author).
empty_head_base_1_DOM(available). % GR 08/07/08
empty_head_base_1_DOM(base). % GR
empty_head_base_1_DOM(based). % GR
empty_head_base_1_DOM(bilateral). % GR 07/24/08
empty_head_base_1_DOM(bioavailability).
empty_head_base_1_DOM(biosynthesis).
empty_head_base_1_DOM(both). % GR 07/16/08
empty_head_base_1_DOM(carboxylation).
empty_head_base_1_DOM(care). % GR
empty_head_base_1_DOM(catabolism).
empty_head_base_1_DOM(catalytic).
empty_head_base_1_DOM(catalytics).
empty_head_base_1_DOM(change).
empty_head_base_1_DOM(changes).
empty_head_base_1_DOM(channel).
%empty_head_base_1_DOM('chemotherapy GR This should not be an empty head').
empty_head_base_1_DOM(chromosome).
empty_head_base_1_DOM(chromosomal).
empty_head_base_1_DOM(clearance).
empty_head_base_1_DOM(combination).
empty_head_base_1_DOM(compound).
empty_head_base_1_DOM(companion).
empty_head_base_1_DOM(concentration).
empty_head_base_1_DOM(conclusion).
empty_head_base_1_DOM(conjugate).
empty_head_base_1_DOM(content).
empty_head_base_1_DOM(contents).
empty_head_base_1_DOM(context). % GR
empty_head_base_1_DOM(corrective). % GR 08/06/08  Corrective [phsu] in UMLS
empty_head_base_1_DOM(current).
%empty_head_base_1_DOM(cytokine).
empty_head_base_1_DOM(dealkylation).
empty_head_base_1_DOM(decarboxylation).
empty_head_base_1_DOM(deficiency).
empty_head_base_1_DOM(degradation).
empty_head_base_1_DOM(demethylation).
empty_head_base_1_DOM(deletion).
empty_head_base_1_DOM(deletional).
empty_head_base_1_DOM(density).
empty_head_base_1_DOM(derivative).
empty_head_base_1_DOM(desulfonation).
empty_head_base_1_DOM(development).
empty_head_base_1_DOM(dimer).
empty_head_base_1_DOM(discipline). % GR 05/06/09
empty_head_base_1_DOM(discussion). % GR 05/06/09
empty_head_base_1_DOM(disposition).
empty_head_base_1_DOM(dosage).
empty_head_base_1_DOM(dos). % this is the base of "doses"...don't ask!
empty_head_base_1_DOM(dose).
empty_head_base_1_DOM(dosing).
empty_head_base_1_DOM(efficacy).
empty_head_base_1_DOM(element).
empty_head_base_1_DOM(enantiomer).
empty_head_base_1_DOM(enrollment). % GR
empty_head_base_1_DOM(enzyme).
empty_head_base_1_DOM(episode).
empty_head_base_1_DOM(ethnic). % GR 05/06/09
empty_head_base_1_DOM(evolution). % GR
empty_head_base_1_DOM(exonic).
empty_head_base_1_DOM(exon).
empty_head_base_1_DOM(exposure).
empty_head_base_1_DOM(expression).
empty_head_base_1_DOM(factor).
empty_head_base_1_DOM(finding).
empty_head_base_1_DOM(formation).
empty_head_base_1_DOM(frequency).
empty_head_base_1_DOM(frequent).
empty_head_base_1_DOM(function).
empty_head_base_1_DOM(gains).
empty_head_base_1_DOM(gene).
empty_head_base_1_DOM(genesis). %GR 06/04/08
empty_head_base_1_DOM(generation).
empty_head_base_1_DOM(genome).
empty_head_base_1_DOM(genotype).
empty_head_base_1_DOM(haplotype).
empty_head_base_1_DOM(healthy).
empty_head_base_1_DOM(hormone).
empty_head_base_1_DOM(hormones).
empty_head_base_1_DOM(hydroxylation).
empty_head_base_1_DOM(identification).
empty_head_base_1_DOM(incidence).
empty_head_base_1_DOM(infusion).
%empty_head_base_1_DOM(inhibitory).
empty_head_base_1_DOM(intronic).
empty_head_base_1_DOM(inflammatory).
empty_head_base_1_DOM(intron).
empty_head_base_1_DOM(introns).
empty_head_base_1_DOM(investigation). % GR 05/06/09
empty_head_base_1_DOM(isoenzyme).
empty_head_base_1_DOM(isoform).
empty_head_base_1_DOM(isoforms).
empty_head_base_1_DOM(isozyme).
empty_head_base_1_DOM(level).
empty_head_base_1_DOM(ligand).
empty_head_base_1_DOM(locus).
empty_head_base_1_DOM(loss).
empty_head_base_1_DOM(majority).
empty_head_base_1_DOM(malfunction).
empty_head_base_1_DOM(manner).
empty_head_base_1_DOM(mechanism).
empty_head_base_1_DOM(meeting). % GR
empty_head_base_1_DOM(metabolic).
empty_head_base_1_DOM(metabolism).
empty_head_base_1_DOM(metabolite).
%empty_head_base_1_DOM(metabolizer). % GR Conflicts with indicator rule trigger
%empty_head_base_1_DOM(metabolizing). % GR Conflicts with indicator rule trigger
%empty_head_base_1_DOM(metabolizings). % GR Conflicts with indicator rule trigger
empty_head_base_1_DOM(methylation).
empty_head_base_1_DOM(modulation).
empty_head_base_1_DOM(molecule).
empty_head_base_1_DOM(mrna).
empty_head_base_1_DOM(mRNA).
empty_head_base_1_DOM(mutant).
empty_head_base_1_DOM(mutation).
empty_head_base_1_DOM(occurrence). % GR 08/07/08
empty_head_base_1_DOM(oncogene).
empty_head_base_1_DOM(option). % GR 7/28/08
empty_head_base_1_DOM(overexpression).
empty_head_base_1_DOM(panel).
empty_head_base_1_DOM(pathway).
empty_head_base_1_DOM(pathogenesis). % GR
empty_head_base_1_DOM(pathophysiologic). % GR 09/2015
empty_head_base_1_DOM(pathophysiology). % GR 06/2015
empty_head_base_1_DOM(peptide).
empty_head_base_1_DOM(pharmacodynamic).
empty_head_base_1_DOM(pharmacodynamics).
empty_head_base_1_DOM(pharmacokinetic).
empty_head_base_1_DOM(pharmacokinetics).
empty_head_base_1_DOM(phenotype).
empty_head_base_1_DOM(polymorphic).
empty_head_base_1_DOM(polymorphism).
empty_head_base_1_DOM(preparation).
empty_head_base_1_DOM(prescription).
%empty_head_base_1_DOM(presence). % IR
empty_head_base_1_DOM(prevention). % Added by Halil %GR thinks it interferes with indicator rule
empty_head_base_1_DOM(product).
empty_head_base_1_DOM(production).
empty_head_base_1_DOM(progression).
empty_head_base_1_DOM(promoter).
empty_head_base_1_DOM(property).
empty_head_base_1_DOM(protein).
empty_head_base_1_DOM(rate). % GR 'Diabetes rates among older adults" 
empty_head_base_1_DOM(reaction).
empty_head_base_1_DOM(reactivation). % GR 05/28/09
empty_head_base_1_DOM(receptor).
%empty_head_base_1_DOM(receptor-).
empty_head_base_1_DOM(recovery).
%empty_head_base_1_DOM(reduction). % GR Conflicts with indicator rule trigger
empty_head_base_1_DOM(regulation).
empty_head_base_1_DOM(relative).
empty_head_base_1_DOM(requirement).
empty_head_base_1_DOM(resistance).
empty_head_base_1_DOM(respect).
%empty_head_base_1_DOM(response).
empty_head_base_1_DOM(rna).
empty_head_base_1_DOM(sample).
empty_head_base_1_DOM(secretion).
empty_head_base_1_DOM(select).
empty_head_base_1_DOM(set).
empty_head_base_1_DOM(severity).
empty_head_base_1_DOM(signaling).
empty_head_base_1_DOM(single).
empty_head_base_1_DOM(sole).
empty_head_base_1_DOM(status).
empty_head_base_1_DOM(statuses).
empty_head_base_1_DOM(strain).
empty_head_base_1_DOM(strategy).
empty_head_base_1_DOM(strategies). %MF added NHLBI
empty_head_base_1_DOM(subject). % needs a metaconc - gets in way otherwise
empty_head_base_1_DOM(subset).
empty_head_base_1_DOM(sulfonation).
empty_head_base_1_DOM(susceptibility).
empty_head_base_1_DOM(synthesis).
empty_head_base_1_DOM(system).
%empty_head_base_1_DOM(test). % GR Conflicts with indicator rule trigger
empty_head_base_1_DOM(therapeutic).
empty_head_base_1_DOM(therapeutics).
empty_head_base_1_DOM(transcription).
empty_head_base_1_DOM(transfer).
empty_head_base_1_DOM(transport).
%empty_head_base_1_DOM(use). % GR Conflicts with indicator rule trigger
%empty_head_base_1_DOM(uses). % GR Conflicts with indicator rule trigger
empty_head_base_1_DOM(variant).
empty_head_base_1_DOM(variation).
empty_head_base_1_DOM(vivo).
empty_head_base_1_DOM(willing). % GR
empty_head_base_2N_DOM(age,[-]).
empty_head_base_2N_DOM(dna,[adduct]).
empty_head_base_2N_DOM(complementary,[dna]).
empty_head_base_2N_DOM(gene,[cluster]).
empty_head_base_2N_DOM(clinical,[study]).
empty_head_base_2N_DOM(clinical,[interview]). % GR
empty_head_base_2N_DOM(continuous,[infusion]).
empty_head_base_2N_DOM(follow,'[-, up]'). % GR
empty_head_base_2N_DOM(food,[refusal]). % GR 
empty_head_base_2N_DOM(gene,[expression]).
empty_head_base_2N_DOM(genetic,[variation]).
empty_head_base_2N_DOM(germline,[mutations]).
empty_head_base_2N_DOM(imaging,[problem]). % GR 05/07/09 
empty_head_base_2N_DOM(immigrant,[inflammatory,cell]). % 08/2015
empty_head_base_2N_DOM(in,[care]). % GR % does not work
empty_head_base_2N_DOM(in,[-,care]). % this does not  work either, I think
empty_head_base_2N_DOM(liver,[enzyme]).
empty_head_base_2N_DOM(long,[arm]).
empty_head_base_2N_DOM(messenger,[rna]).
empty_head_base_2N_DOM(messenger,[rnas]).
empty_head_base_2N_DOM(metabolic,[activation]).
empty_head_base_2N_DOM(mrna,[expression]).
empty_head_base_2N_DOM(multiple,[episode]). % GR
empty_head_base_2N_DOM(mutated,[gene]). % does not work, since mutated is often tagged as verb
empty_head_base_2N_DOM(oxidative,[metabolism]).
empty_head_base_2N_DOM(plasma,[concentration]).
empty_head_base_2N_DOM(polymorphic,[allele]).
empty_head_base_2N_DOM(pro,[-]). % does not work %what is an example where this is an empty head?
empty_head_base_2N_DOM(protein,[expression]).
empty_head_base_2N_DOM(promoter,[region]).
empty_head_base_2N_DOM(receptor,[-]). % does not work % what is an example?
empty_head_base_2N_DOM(reporter,[gene]).
empty_head_base_2N_DOM(s,[')','-']). % Does this work? 06/2015
empty_head_base_2N_DOM(s,[')','+']). % Does this work? 06/2015
empty_head_base_2N_DOM(s,[')','(','+']). % Does this work? 06/2015
empty_head_base_2N_DOM(s,['.',d]). % Does this work? 06/2015
empty_head_base_2N_DOM(serum,[concentration]).
empty_head_base_2N_DOM(short,[arm]).
empty_head_base_2N_DOM(steady,'[-, state]').
empty_head_base_2N_DOM(steady,'[-, states]').
empty_head_base_2N_DOM(subcellular,[fraction]).
empty_head_base_2N_DOM(subcellular,[fractions]).
empty_head_base_2N_DOM(therapy,[response]).
empty_head_base_2N_DOM(candidate,[gene]).
empty_head_base_2N_DOM(candidate,[disease,gene]). % GR
empty_head_base_2N_DOM(cancer,[predisposing,gene]).
empty_head_base_2N_DOM(cancer,[promoting,gene]).
empty_head_base_2N_DOM(family,[protein]).
empty_head_base_2N_DOM(protein,[family]).
empty_head_base_2N_DOM(homologous,[gene]).
empty_head_base_2N_DOM(human,[gene]).
empty_head_base_2N_DOM(microtubule,[associated,protein]).
empty_head_base_2N_DOM(microtubule,[associated,proteins]).
empty_head_base_2N_DOM(tumor,[supressor,gene]).
empty_head_base_2N_DOM('VAS',[-]). % 07/2015
concept_to_ignore_DOM([Base|RestBases], MetaConc) :-
	lower(Base, LCBase),
	concept_to_ignore_aux(RestBases, LCBase, MetaConc).

concept_to_ignore_aux([], Base, MetaConc) :-
	concept_to_ignore_1(Base, MetaConc).
concept_to_ignore_aux([Base2|RestBases], Base1, MetaConc) :-
	concept_to_ignore_2N(Base1, [Base2|RestBases], MetaConc).

concept_to_ignore_1('18S','Supernumerary mandibular left second molar'). % GR 06/28/2016
concept_to_ignore_1(able,'Able'). % GR 06/2015
concept_to_ignore_1(abnormality,'Congenital Abnormality').
concept_to_ignore_1(ace,'cyclophosphamide/doxorubicin protocol'). % GR 09/2015
concept_to_ignore_1('ACE','cyclophosphamide/doxorubicin protocol'). % GR 08/2015
concept_to_ignore_1(algorithmic,algorithm). % GR 06/2015
concept_to_ignore_1(alone,'Living Alone'). % GR 09/2015
concept_to_ignore_1(amusement,entertainment). % GR 06/08/08
concept_to_ignore_1(anomaly,'Congenital Abnormality').
concept_to_ignore_1(anomalous,'Congenital Abnormality').
concept_to_ignore_1(anomalously,'Congenital Abnormality').
concept_to_ignore_1(activity,'*Activity').
concept_to_ignore_1(activity,'% activity'). % GR changed to lower case 07/2015
concept_to_ignore_1(adapt,'Adapt').
concept_to_ignore_1(adapted,'Adapt').
concept_to_ignore_1(adapting,'Adapt').
concept_to_ignore_1(adaptor,'Adapt').
concept_to_ignore_1(adjustment,'Adjustment Action'). % GR 06/2015
concept_to_ignore_1(adjustment,'Individual Adjustment'). %MF
concept_to_ignore_1(adjustment,'Psychological adjustment'). %MF
concept_to_ignore_1(administrate,'Administration procedure').
concept_to_ignore_1(administrated,'Administration procedure').
concept_to_ignore_1(administrates,'Administration procedure').
concept_to_ignore_1(administrating,'Administration procedure').
concept_to_ignore_1(administration,'Administration procedure').
concept_to_ignore_1(administrational,'Administration procedure').
concept_to_ignore_1(administrate,'Administration occupational activities').
concept_to_ignore_1(administrated,'Administration occupational activities').
concept_to_ignore_1(administrates,'Administration occupational activities').
concept_to_ignore_1(administrating,'Administration occupational activities').
concept_to_ignore_1(administration,'Administration occupational activities').
concept_to_ignore_1(administrational,'Administration occupational activities').
concept_to_ignore_1(administrative,'Administrative').
concept_to_ignore_1(advances,'Advance'). % GR 05/07/09
concept_to_ignore_1(affecting,'Affecting'). % GR 06/28/2016
concept_to_ignore_1(affinity,'Affinity').
concept_to_ignore_1(airway,'Airway device'). % GR 06/2015
concept_to_ignore_1(age,'Elderly').
concept_to_ignore_1(aged,'Elderly').
concept_to_ignore_1(ages,'Elderly').
concept_to_ignore_1(aggregation,'% aggregation').
concept_to_ignore_1(aggressive,'Aggressive behavior'). %GR
concept_to_ignore_1(alcohol,'Ethanol'). % GR 06/28/2016
concept_to_ignore_1(alert,'Consciousness clear'). % GR 06/2015
concept_to_ignore_1(analysis,'Analysis of substances').
concept_to_ignore_1(analyticity,'Analysis of substances').
concept_to_ignore_1(antiinflammatory,'Anti-Inflammatory Agents').
concept_to_ignore_1('anti inflammatory','Anti-Inflammatory Agents').
concept_to_ignore_1('anti-TNF agent','Anti-Anxiety Agents'). % GR 06/2015
concept_to_ignore_1('anti-VEGF agent','Anti-Anxiety Agents'). % GR 07/2015
concept_to_ignore_1('April','ANP32B gene'). % GR 06/28/2016
concept_to_ignore_1(area,'Areae'). % GR 06/2015 This does not get triggered with the entry in the singular
concept_to_ignore_1(areas,'Areae'). % GR 06/2015 This does not get triggered with the entry in the singular
concept_to_ignore_1(association,'Mental association'). % GR 06/28/2016
concept_to_ignore_1(attachment,'Fixation - action'). % GR
concept_to_ignore_1(attachment,'Emotional bonding'). % GR 06/2015
concept_to_ignore_1(attendant,'Attendants'). % GR 10/2015
concept_to_ignore_1(available,'Availability of'). % GR 06/2015
concept_to_ignore_1(axe,'Axilla'). % GR, 1/11/13
concept_to_ignore_1(emergency,'Bale out'). % GR 06/2015
concept_to_ignore_1(base,'Base'). % GR 07/2015
concept_to_ignore_1(based,'Base').
concept_to_ignore_1(baseness,'Base').
concept_to_ignore_1(baser,'Base').
concept_to_ignore_1(basest,'Base').
concept_to_ignore_1(basic,'Base').
concept_to_ignore_1(basically,'Base').
concept_to_ignore_1(basicness,'Base').
concept_to_ignore_1(basing,'Base').
concept_to_ignore_1(basis,'Base').
concept_to_ignore_1(base,'Basis'). %GR 07/28/08
concept_to_ignore_1(based,'Basis'). %GR 07/28/08
concept_to_ignore_1(behavior,'Behaviors and observations relating to behavior'). % GR 09/2015
concept_to_ignore_1(best,'Vitelliform dystrophy'). %GR This is not working
concept_to_ignore_1(fundament,'Base').
concept_to_ignore_1(fundamentally,'Base').
concept_to_ignore_1(baseline,'BaseLine dental material').
concept_to_ignore_1(base-line,'BaseLine dental material').
concept_to_ignore_1(blind,'Blind Vision').
concept_to_ignore_1(blinding,'Blind Vision').
concept_to_ignore_1(blindly,'Blind Vision').
%concept_to_ignore_1(blindness,'Blind Vision'). % GR
concept_to_ignore_1(blocking,'Mental blocking').
%concept_to_ignore_1('BMD','DMD,BEST1'). % GR 06/2015 - This entry from Entrez Gene
concept_to_ignore_1(borne,'Ursidae Family'). % GR 04/3/09
concept_to_ignore_1(bulk,'BULK'). % GR 07/25/08
concept_to_ignore_1(c,'Cesium').
concept_to_ignore_1(c,'Supernumerary maxillary right primary canine').
concept_to_ignore_1(cs,'Cesium').
concept_to_ignore_1(cs,'Supernumerary maxillary right primary canine').
concept_to_ignore_1(cad,'CAD gene').
concept_to_ignore_1(canal,'Pulp Canals'). % GR 06/08/09
concept_to_ignore_1(cancer,'Cancer Genus').
concept_to_ignore_1(cancerous,'Cancer Genus').
concept_to_ignore_1(carcinomatous,'Cancer Genus').
% Commented out by FML on 02/22/2016 after extensive e-mail discussion about 16412044.ti.1:
% Phenotype of early cardiomyopathic changes induced by active immunization of rats
% with a synthetic peptide corresponding to the second extracellular loop
% of the human beta-adrenergic receptor.
% The exception was causing the FN
% relation|C0205991|Immunization, Active|topp|topp|||CAUSES|C0878544|Cardiomyopathies|dsyn|dsyn||
% concept_to_ignore_1(cardiomyopathic,'Cardiomyopathies'). % GR 09/2015
concept_to_ignore_1(causing,'Causing'). % GR 06/2015
concept_to_ignore_1(cavity,'Dental caries'). % GR 06/2015
concept_to_ignore_1(century,'Century'). % Halil
concept_to_ignore_1(chemotherapy,'Encounter due to Chemotherapy session for neoplasm'). % GR
concept_to_ignore_1(chemotherapy,pharmacotherapeutic). %GR, 1/11/13
concept_to_ignore_1(child,'Chronic Disease Hospital - Children'). % GR
concept_to_ignore_1(children,'Chronic Disease Hospital - Children'). % GR
concept_to_ignore_1(children,'General Acute Care Hospital - Children'). % GR
concept_to_ignore_1(children,'Rehabilitation Hospital - Children'). % GR
concept_to_ignore_1('cigarette smoking','Cigarette Smoking'). % GR 06/2015
concept_to_ignore_1('clinical interview','History taking'). % GR
concept_to_ignore_1('clinical trials','Clinical Trial [Publication Type]'). % GR 06/2015 This mapping is only triggered with the plural entry
concept_to_ignore_1(collective,collective). % GR 06/2015
concept_to_ignore_1(closure,'Closure'). % GR 06/2015
concept_to_ignore_1(complementary,'Complement System Proteins'). % GR 06/2016
concept_to_ignore_1(complication,'Etiology, operative procedure, as cause of'). % GR 09/11/2015
concept_to_ignore_1(concentration,'Mental concentration'). % GR 06/28/2016
concept_to_ignore_1(concept,'CONCEPT Drug'). % GR 09/2015
concept_to_ignore_1(consideration,'CONSIDERATION'). % GR
concept_to_ignore_1(consumption,'Consumption-archaic term for TB'). % GR 09/2015
concept_to_ignore_1(context,'Context'). % GR
concept_to_ignore_1(contractile,'Contraction'). % GR 09/2015
concept_to_ignore_1(coupled,'Couples'). % GR 04/29/09
concept_to_ignore_1(creation,'Surgical construction'). % GR
concept_to_ignore_1(death,'Adverse Event Associated with Death'). % GR 09/2015
concept_to_ignore_1(dependent,'Dependent'). % GR 1/17/13
concept_to_ignore_1(derived,'DERIVED'). % GR 10/2015
concept_to_ignore_1(detected,'Detected'). % GR 08/2015
concept_to_ignore_1(detected,'Detection'). % GR 06/2015
concept_to_ignore_1(detecting,'Detected'). % GR 08/2015
concept_to_ignore_1(difficulty,'Difficulty'). % GR 06/2015
concept_to_ignore_1(discontinuation,'Discontinuation'). % GR 06/2015
concept_to_ignore_1(discussion,'Discussion'). % GR 06/09/09
concept_to_ignore_1(divorce,'Encounter due to family disruption'). % GR
concept_to_ignore_1(documented,'Documented'). % GR 10/2015
concept_to_ignore_1(dog,'Dog antigen'). % GR 09/2015
concept_to_ignore_1(donor,'Encounter due to donor examination'). % 09/2015
concept_to_ignore_1(down,'Downy hair'). % GR 06/2015
concept_to_ignore_1(dust,'Does dusting'). % GR 06/28/2016
concept_to_ignore_1(dysfunction,physiopathological). %GR 1/11/13
concept_to_ignore_1(effective,'Effect'). % GR 06/29/2016
concept_to_ignore_1(elimination,'Excretory function'). % GR 05/06/09
concept_to_ignore_1(endocrine,'Adverse Event Associated with Endocrine'). % GR 09/2015
concept_to_ignore_1(enrollment,'Enrollment'). % GR 05/06/09
concept_to_ignore_1(ensuring,'Ensure'). % GR 06/28/16
concept_to_ignore_1(entertainment,entertainment). % GR 08/08/08
concept_to_ignore_1(evaluable,'Evaluable Disease'). % GR 06/2015
concept_to_ignore_1(evaluation,'Assessment procedure'). % GR 05/01/09
concept_to_ignore_1(examined,'Examined'). % GR 06/2015
concept_to_ignore_1(examining,'Examined'). % GR 06/2015
concept_to_ignore_1(executive,'Executives'). % GR 08/2015
concept_to_ignore_1(effort,'Exertion'). % GR 07/2015
concept_to_ignore_1(expression,'Expression procedure'). % GR 06/2015
concept_to_ignore_1(family,'Last Name'). % 2012AA
concept_to_ignore_1('line imaging test','Linear Test'). % GR 09/2015
concept_to_ignore_1('first-line imaging test','Linear Test'). % GR 09/2015
concept_to_ignore_1(hemodialysis,'Encounter for extracorporeal dialysis'). % GR
concept_to_ignore_1('family history of','Family'). % GR 06/04/09
concept_to_ignore_1('family history of','Family history of'). % GR 06/04/09
concept_to_ignore_1('family protein','Family'). % GR
concept_to_ignore_1(favor,'FAVOR'). % GR 09/2015
concept_to_ignore_1(favour,'FAVOR'). % GR 09/2015
concept_to_ignore_1(favorable,'FAVOR'). % GR 06/2016
concept_to_ignore_1(favourable,'FAVOR'). % GR 06/2016
concept_to_ignore_1(file,'file device'). % GR
concept_to_ignore_1(filtering,'Filters'). % GR 06/2015
concept_to_ignore_1(fluid,'Fluid Specimen Code'). % GR 09/2015
%concept_to_ignore_1(follow-up,'Follow-up status'). % GR
concept_to_ignore_1(form,'Manufactured form'). % GR 06/2015
concept_to_ignore_1(frank,'Purpura, Thrombocytopenic, Idiopathic'). % GR 06/2015
concept_to_ignore_1(gauge,'GAGE'). % GR 05/06/09
concept_to_ignore_1('GEMS','Coiled Bodies'). % GR
concept_to_ignore_1(grade,'Grade'). % GR 06/28/2016
concept_to_ignore_1(grading,'Grade'). % GR 06/28/2016
concept_to_ignore_1(gravity,'Force of Gravity'). % GR
concept_to_ignore_1(gynaecological,'Gynecology'). % GR 10/2015
concept_to_ignore_1('hair transplant','Encounter due to hair transplant'). % GR 06/2015
concept_to_ignore_1(healthy,'Clinic / Center - Health'). % GR 09/2015
concept_to_ignore_1('heart valves','Heart Valve Prosthesis'). % GR 06/2015
concept_to_ignore_1(hereditary,'HISTORY OF SYMPTOMS & DISEASES:FINDING:POINT IN TIME:HEREDITARY:NOMINAL'). % GR 09/2015
concept_to_ignore_1(hiv,'HIV Vaccine').
concept_to_ignore_1(high,'Euphoric mood'). % GR
concept_to_ignore_1(higher,'Euphoric mood'). % GR
concept_to_ignore_1('hydroxyacyl-CoA dehydrogenase','HYDROXYACYL-CoA DEHYDROGENASE'). % GR 10/2015
concept_to_ignore_1(identify,'Container status - Identified'). %MF no need to add inflection
concept_to_ignore_1(imaging,'Imaging problem'). % GR 05/06/09
concept_to_ignore_1(immigrant,'Immigrants'). % GR 09/2015
concept_to_ignore_1(impact,'Impact'). % GR Impact is semtype 'food'
concept_to_ignore_1(impairment,'Impaired health'). % GR 06/08/09
concept_to_ignore_1(immunization,'Need for immunization against unspecified infectious disease'). % GR 06/2015
concept_to_ignore_1(independent,'Independently able'). %MF
concept_to_ignore_1(indicate,'Indicated'). %MF no need to add inflection
concept_to_ignore_1(independently,'Independently able'). %MF
concept_to_ignore_1(infection,'Adverse Event Associated with Infection').
concept_to_ignore_1(infection,'Communicable Diseases'). % GR 07/29/08
concept_to_ignore_1(infective,'Adverse Event Associated with Infection').
concept_to_ignore_1(inflate,'Economic Inflation').
concept_to_ignore_1(inflated,'Economic Inflation').
concept_to_ignore_1(inflating,'Economic Inflation').
concept_to_ignore_1(inflatable,'Economic Inflation').
concept_to_ignore_1(inflater,'Economic Inflation').
concept_to_ignore_1(inflator,'Economic Inflation').
concept_to_ignore_1(inflation,'Economic Inflation').
concept_to_ignore_1(inflationary,'Economic Inflation').
concept_to_ignore_1(inhibition,'inhibition, physical').
concept_to_ignore_1(inhibition,'Psychological inhibition').
concept_to_ignore_1(inoculation,vaccination). % GR 06/2015
concept_to_ignore_1(insect,'Insect Extract').
concept_to_ignore_1(insulin,'INS gene').
concept_to_ignore_1(investigation,investigation). % 09/2015
concept_to_ignore_1(iva,'Iva'). % 09/2015
concept_to_ignore_1(kidney,'HISTORY OF SYMPTOMS & DISEASES:FINDING:POINT IN TIME:KIDNEY:NOMINAL'). % GR 06/2015
concept_to_ignore_1('kidney donor','Encounter due to kidney donor status'). % GR 06/2015
concept_to_ignore_1(kuru-like,'Kuru'). % GR 05/28/09
concept_to_ignore_1(lead,'Lead'). %MF
concept_to_ignore_1(leads,'Leads'). %MF
concept_to_ignore_1(life,'Laser-Induced Fluorescence Endoscopy').
concept_to_ignore_1(light,'Light'). % GR
concept_to_ignore_1(likelihood,'Probable diagnosis'). % GR 06/29/2016
concept_to_ignore_1(link,'Link brand of magnesium carbonate and aluminum hydroxide').
concept_to_ignore_1(linkable,'Link brand of magnesium carbonate and aluminum hydroxide').
concept_to_ignore_1(linkage,'Link brand of magnesium carbonate and aluminum hydroxide').
concept_to_ignore_1(linked,'Link brand of magnesium carbonate and aluminum hydroxide').
concept_to_ignore_1(linker,'Link brand of magnesium carbonate and aluminum hydroxide').
concept_to_ignore_1(linking,'Link brand of magnesium carbonate and aluminum hydroxide').
%concept_to_ignore_1(little,'Little'). % GR, 1/11/13; 07/15 commenting it out as it appears it's not in 2006AA
concept_to_ignore_1(liver,'Liver Extract').
concept_to_ignore_1(localization,'establishment and maintenance of substrate location'). % GR 06/2015
concept_to_ignore_1(lower,'Body Site Modifier - Lower'). % GR 09/2015
concept_to_ignore_1(lumen,'Lumen'). % GR 09/2015 - This is for a domain file, to change semtype
concept_to_ignore_1(maintenance,'Maintenance'). % GR 06/2015
concept_to_ignore_1(malaria,'Malaria Vaccines').
concept_to_ignore_1(mass,'Mass of body structure').
concept_to_ignore_1(max,'MAX'). % GR 06/2015
concept_to_ignore_1('medical photography','Photography of patient'). % GR 06/2015
concept_to_ignore_1(mediator,'Mediator brand of benfluorex hydrochloride'). %2012AA
concept_to_ignore_1(melanogaster,'Melanogaster'). % GR 08/2015 this maps to a fngs when it follows D. for drosophila
concept_to_ignore_1(melanogaster,'Drosophila melanogaster'). % GR 2015
concept_to_ignore_1(melanoma,'Melanoma vaccine').
concept_to_ignore_1(melanoma,'Mouse Melanoma'). %GR
concept_to_ignore_1(melanomatous,'Melanoma vaccine').
concept_to_ignore_1(meet,'Methionine').
concept_to_ignore_1(mental,'Psyche structure'). 
concept_to_ignore_1(met,'Methionine').
concept_to_ignore_1(min,'Carcinoma in Situ of the Mouse Mammary Gland').
concept_to_ignore_1(min,'Intraepithelial Neoplasia of the Mouse Mammary Gland').
concept_to_ignore_1(min,'Mouse MIN NOS').
concept_to_ignore_1(minor,'Minor (person)').
concept_to_ignore_1(minor,minor).
concept_to_ignore_1(mouse,'Mouse antigen').
concept_to_ignore_1(constitutive,'NOS1 protein, human').
concept_to_ignore_1(constitutively,'NOS1 protein, human').
concept_to_ignore_1(national,'Federal Government'). % GR 06/29/2016
concept_to_ignore_1(net,'Ephrin Receptor EphB1'). % GR 10/2015
concept_to_ignore_1(neuronal,'NOS1 protein, human').
concept_to_ignore_1(neuronally,'NOS1 protein, human').
concept_to_ignore_1(constitutive,'NOS3 protein, human').
concept_to_ignore_1(constitutively,'NOS3 protein, human').
concept_to_ignore_1(neighbouring,neighbor). %GR 'neighbouring neurons' = neuron PART_OF neighbor
concept_to_ignore_1(neuronal,'Neuronal').
concept_to_ignore_1(neuronally,'Neuronal').
concept_to_ignore_1(non,'NON Mouse').
concept_to_ignore_1(normal,'% normal').
concept_to_ignore_1(normally,'% normal').
concept_to_ignore_1(nutrition,'Nutritional Study'). % GR 06/2016
%concept_to_ignore_1('Observed',observed). % GR 06/28/2016
concept_to_ignore_1(one-step,'One-Step dentin bonding system'). % GR 06/2015
concept_to_ignore_1('Opiates','Opiate Assay'). % GR 06/28/2016
concept_to_ignore_1('ORs','ORALIT'). % GR 07/05/2016
concept_to_ignore_1(overlapping,'Imbrication'). % GR 04/29/09
concept_to_ignore_1(parameter,'Observation parameter'). % GR 09/2015
concept_to_ignore_1(pathophysiologic,'Functional disorder'). % GR 09/2015
concept_to_ignore_1(pathophysiological,'Functional disorder'). %MF
concept_to_ignore_1(pathophysiology,'Functional disorder'). % GR 09/2015
concept_to_ignore_1(p450,'CYP2B6 protein, human').
concept_to_ignore_1(p450s,'CYP2B6 protein, human').
concept_to_ignore_1(pain,'Adverse Event Associated with Pain').
concept_to_ignore_1(painful,'Adverse Event Associated with Pain').
concept_to_ignore_1(painfully,'Adverse Event Associated with Pain').
concept_to_ignore_1(pediatric,'Pediatric brand name').
concept_to_ignore_1(pen,penis). % GR
concept_to_ignore_1(physiological,'Physiological Diffuse Hyperplasia of the Mouse Mammary Gland').
concept_to_ignore_1(physiologically,'Physiological Diffuse Hyperplasia of the Mouse Mammary Gland').
concept_to_ignore_1(plaque,'Ductal Mouse MIN'). % GR
concept_to_ignore_1(plasticity,'Plasticity'). % GR 09/2015 - This is for a domain file, to change semtype
concept_to_ignore_1('ponderal index','Weight for height'). % GR 06/2016
concept_to_ignore_1(populations,'geographic population'). % GR 06/2015
concept_to_ignore_1(position,'establishment and maintenance of substrate location'). % GR 05/06/09
concept_to_ignore_1(positioned,'establishment and maintenance of substrate location'). % GR 05/06/09
concept_to_ignore_1(positioning,'establishment and maintenance of substrate location'). % GR 05/06/09
concept_to_ignore_1(possess,'Possessed'). % GR 06/2015
concept_to_ignore_1(possessed,'Possessed'). % GR 06/2015
concept_to_ignore_1(possessing,'Possessed'). % GR 06/2015
concept_to_ignore_1(pretreatment,'Pretreatment'). % GR 10/2015
concept_to_ignore_1(presence,'Presence').
concept_to_ignore_1(pressures,'Pressure- physical agent'). % GR 09/2015
concept_to_ignore_1('PM','Premenstrual syndrome').
concept_to_ignore_1(prevent,'PREVENT').
concept_to_ignore_1(prevented,'PREVENT').
concept_to_ignore_1(preventer,'PREVENT').
concept_to_ignore_1(preventing,'PREVENT').
concept_to_ignore_1(primary,'Primary operation'). % GR 06/08/09
concept_to_ignore_1(primary,'Primary'). % GR 06/08/09
concept_to_ignore_1(problem,'Problem'). % GR
concept_to_ignore_1(pro,'Proline').
concept_to_ignore_1(process,'bony process').
concept_to_ignore_1(processable,'bony process').
concept_to_ignore_1(processible,'bony process').
concept_to_ignore_1(processed,'bony process').
concept_to_ignore_1(processing,'bony process').
concept_to_ignore_1(procession,'bony process').
concept_to_ignore_1(processor,'bony process').
concept_to_ignore_1(processability,'bony process').
concept_to_ignore_1(processibility,'bony process').
concept_to_ignore_1(processionary,'bony process').
concept_to_ignore_1(prognosis,'Forecast of outcome'). % GR
concept_to_ignore_1(prompt,'Prompt').
concept_to_ignore_1(prompted,'Prompt').
concept_to_ignore_1(prompter,'Prompt').
concept_to_ignore_1(prompting,'Prompt').
concept_to_ignore_1(promptly,'Prompt').
concept_to_ignore_1(promptness,'Prompt').
concept_to_ignore_1(promptitude,'Prompt').
concept_to_ignore_1(propagation,'Reproduction').
concept_to_ignore_1(prophylactic,'Condoms, Unspecified'). % GR
concept_to_ignore_1(prophylactic,'Drugs used in migraine prophylaxis'). % GR 05/04/09
concept_to_ignore_1('prophylactic antibiotics','Other prophylactic chemotherapy'). % GR 06/2015
concept_to_ignore_1(prophylaxis,'Condoms, Unspecified'). % GR
concept_to_ignore_1(prostatitis,'Mouse Prostatitis'). % GR 07/29/08
concept_to_ignore_1(prospective,'Longitudinal Studies'). % GR 10/2015
concept_to_ignore_1(protect,protect).
concept_to_ignore_1(protected,protect).
concept_to_ignore_1(protecting,protect).
concept_to_ignore_1(protective,'Protective Agents'). % GR 06/2016
concept_to_ignore_1(protection,'PROTECTION'). % GR 06/2015
concept_to_ignore_1(protector,protect).
concept_to_ignore_1(protectress,protect).
concept_to_ignore_1(protectable,protect).
concept_to_ignore_1(protectible,protect).
concept_to_ignore_1(protectability,protect).
concept_to_ignore_1(protectibility,protect).
concept_to_ignore_1(prove,'proven venom').
concept_to_ignore_1(provable,'proven venom').
concept_to_ignore_1(proved,'proven venom').
concept_to_ignore_1(proves,'proven venom').
concept_to_ignore_1(proven,'proven venom').
concept_to_ignore_1(proving,'proven venom').
concept_to_ignore_1(provide,'Provide'). %MF food no need to add inflection
concept_to_ignore_1(providing,'Provide'). % GR it turns out we need to add inflected forms
concept_to_ignore_1(provided,'Provide'). % GR 07/25/08
concept_to_ignore_1('psychiatric hospitalization','PSYCHIATRIC HOSPITALIZATION'). % GR 09/11/16 to prevent UMLS semtype mobd - this belongs in a domain file
concept_to_ignore_1(psychiatry,'Psychiatrist').
concept_to_ignore_1(psychological,'Psychiatric problem'). % GR 06/28/16
concept_to_ignore_1(push,'Does push'). % GR 06/28/2016
concept_to_ignore_1(pushed,'Does push'). % GR 06/28/2016
concept_to_ignore_1('public health','Public Health Podiatrist'). % GR
concept_to_ignore_1(rabbit,'Rabbit antigen').
concept_to_ignore_1(radiation,'Radiotherapy Research'). % GR 06/28/16
concept_to_ignore_1('radiation therapy',radiotherapeutic). % GR, 1/11/13
concept_to_ignore_1('radiation therapy','Diagnostic Service Section ID - Radiation Therapy'). % GR 09/2015
concept_to_ignore_1(radiograph,'Diagnostic Service Section ID - Radiograph'). % GR 09/2015
concept_to_ignore_1(radiographic,'Radiographic'). % GR 09/2015
concept_to_ignore_1(reach,'Does reach'). % GR, 1/11/13
concept_to_ignore_1(reaching,'Does reach'). % GR, 1/11/13
concept_to_ignore_1(reached,'Does reach'). % GR, 1/11/13
concept_to_ignore_1(recruit,'Recruitment').
concept_to_ignore_1(recruitable,'Recruitment').
concept_to_ignore_1(recruited,'Recruitment').
concept_to_ignore_1(recruiter,'Recruitment').
concept_to_ignore_1(recruiting,'Recruitment').
concept_to_ignore_1(recruitment,'Recruitment').
concept_to_ignore_1(reduction,'Reduction - action').
concept_to_ignore_1(reductional,'Reduction - action').
concept_to_ignore_1(reductionist,'Reduction - action').
concept_to_ignore_1(refusal,'Refusal'). % GR
concept_to_ignore_1(regulator,'Regulators'). % GR
concept_to_ignore_1(related,'Related personal status'). % GR
concept_to_ignore_1(release,'% release').
concept_to_ignore_1(releasable,'% release').
concept_to_ignore_1(released,'% release').
concept_to_ignore_1(releaser,'% release').
concept_to_ignore_1(releasing,'% release').
concept_to_ignore_1(releasability,'% release').
concept_to_ignore_1(release,'Release procedure').
concept_to_ignore_1(release,'Discharge (release)'). % GR 04/03/09
concept_to_ignore_1(releasable,'Release procedure').
concept_to_ignore_1(released,'Release procedure').
concept_to_ignore_1(releaser,'Release procedure').
concept_to_ignore_1(releasing,'Release procedure').
concept_to_ignore_1(releasability,'Release procedure').
concept_to_ignore_1(replication,'DNA biosynthesis'). % GR
concept_to_ignore_1(report,'Reporting'). % GR
concept_to_ignore_1(responsible,'Responsible to').
concept_to_ignore_1(retention,'maintenance of localization'). % Feb 2013
concept_to_ignore_1(rheumatologic,'Rheumatologist'). % GR 06/28/16
concept_to_ignore_1(rheumatology,'Rheumatologist'). % GR 09/2015
concept_to_ignore_1(rho,rho). % GR 06/2015
concept_to_ignore_1(rise,'Risedronate'). % GR 06/2015
concept_to_ignore_1(rubber,'Condoms, Male'). % GR 06/2015
concept_to_ignore_1(rural,'Rural'). % GR
concept_to_ignore_1(saw,'Saws'). %GR
concept_to_ignore_1(scale,'Integumentary scale'). % 07/2015
concept_to_ignore_1(screening,'Special screening finding'). %MF
concept_to_ignore_1(second,'per second'). % GR 06/2016
concept_to_ignore_1(section,'Division (procedure)'). % GR 06/2015
concept_to_ignore_1(see,'Saws'). %GR
concept_to_ignore_1(selected,'Selenium and Vitamin E Efficacy Trial'). % GR 06/2015
concept_to_ignore_1(sensitivity,'Antimicrobial susceptibility'). % GR 06/2015
concept_to_ignore_1(seriousness,'Seriousness'). % GR 06/2015
concept_to_ignore_1(sex,'Sexual intercourse - finding'). %MF
concept_to_ignore_1(sham,'salicylhydroxamic acid'). % GR 05/06/09
concept_to_ignore_1(shield,'Shield'). % GR 06/2015
concept_to_ignore_1(short,'Shortened'). % GR 10/2015
concept_to_ignore_1(similar,'Simile'). % GR 06/28/16
%concept_to_ignore_1(smoker,'Smoker'). % GR 06/2015 but in 2016 I added it to domain file
%concept_to_ignore_1(somatic,'cell body (neuron)'). %GR
concept_to_ignore_1(sound,'Sounds device'). % 07/05/2016
concept_to_ignore_1(source,'Source').
concept_to_ignore_1(specific,'Specific gravity (device)').
concept_to_ignore_1(specifically,'Specific gravity (device)').
concept_to_ignore_1(specificness,'Specific gravity (device)').
concept_to_ignore_1(speed,'Amphetamine'). % GR 06/2015
concept_to_ignore_1(spermatozoa,'Specimen Source Codes - Spermatozoa'). % GR 09/2015
concept_to_ignore_1(s,'Sicca Syndrome').
concept_to_ignore_1(ss,'Sicca Syndrome').
concept_to_ignore_1(s,'Supernumerary mandibular right first primary molar').
concept_to_ignore_1(ss,'Supernumerary mandibular right first primary molar').
concept_to_ignore_1(spiny,'Vertebral column'). % GR 09/2015
%concept_to_ignore_1(stereotype,'Stereotypic Movement'). % GR 05/06/09; commented out 07/28/15 as it's not in 2006AA
concept_to_ignore_1(stomach,'Stomach Diseases'). % GR 09/2015
concept_to_ignore_1(storage,'maintenance of localization').
concept_to_ignore_1(strain,'Muscle strain'). % temporary, should be removed when WSD is operational. --Halil
concept_to_ignore_1(strained,'Muscle strain'). % temporary, should be removed when WSD is operational. --Halil
concept_to_ignore_1(straining,'Muscle strain'). % temporary, should be removed when WSD is operational. --Halil 
concept_to_ignore_1(strangulation,'Death by strangulation'). % GR 09/2015
concept_to_ignore_1(strategy,strategy). % GR 06/2015
concept_to_ignore_1(stress,'Stress bismuth subsalicylate').
concept_to_ignore_1(stressed,'Stress bismuth subsalicylate').
concept_to_ignore_1(stressing,'Stress bismuth subsalicylate').
concept_to_ignore_1(study,'Room of building - Study').
concept_to_ignore_1(study,'Scientific Study').
concept_to_ignore_1(studied,'Room of building - Study').
concept_to_ignore_1(studied,'Scientific Study').
concept_to_ignore_1(studying,'Room of building - Study').
concept_to_ignore_1(studying,'Scientific Study').
concept_to_ignore_1(studious,'Room of building - Study').
concept_to_ignore_1(studious,'Scientific Study').
concept_to_ignore_1(studiousness,'Room of building - Study').
concept_to_ignore_1(studiousness,'Scientific Study').
concept_to_ignore_1(style,'STYLE'). % GR
concept_to_ignore_1(suffer,'Mental Suffering').
concept_to_ignore_1(suffers,'Mental Suffering').
concept_to_ignore_1(suffered,'Mental Suffering').
concept_to_ignore_1(sufferer,'Mental Suffering').
concept_to_ignore_1(suffering,'Mental Suffering').
concept_to_ignore_1(sufferance,'Mental Suffering').
concept_to_ignore_1(suggest,'Suggestible'). %MF
concept_to_ignore_1(suggestible,'Suggestible'). %MF
concept_to_ignore_1(suggestion,'Suggestion'). %MF
concept_to_ignore_1(suicide,'Cancer patients and suicide and depression').
concept_to_ignore_1(suiciding,'Cancer patients and suicide and depression').
concept_to_ignore_1(support,'Supportive care'). % GR 05/07/09
concept_to_ignore_1(support,'Supportive assistance'). % GR 06/2015
concept_to_ignore_1(support,'Support, device'). % GR 06/28/2016
concept_to_ignore_1(supported,'Support, device'). % GR 06/28/2016
concept_to_ignore_1(supporting,'Supportive care'). % GR 10/2015
concept_to_ignore_1(supporting,'Support, device'). % GR 06/28/2016
concept_to_ignore_1(surgery,'Surgical aspects'). % GR 1/11/13
concept_to_ignore_1(survive,'SURVIVE').
concept_to_ignore_1(surviving,'SURVIVE'). % GR 06/29/2016
concept_to_ignore_1(tailor,'Tailor'). % GR 06/28/2016
concept_to_ignore_1(tailored,'Tailor'). % GR 06/28/2016
concept_to_ignore_1(tailoring,'Tailor'). % GR 06/28/2016
concept_to_ignore_1('TAM','Tamoxifen'). % GR 06/2015
concept_to_ignore_1('TAMs','Tamoxifen'). % GR 06/2015
concept_to_ignore_1(tandem,'TANDEM'). % GR
concept_to_ignore_1(target,'Candidate Disease Gene'). % GR
concept_to_ignore_1(targeted,'Candidate Disease Gene'). % GR 06/2015
concept_to_ignore_1(task,'TASK').
concept_to_ignore_1(tend,'Tendor').
concept_to_ignore_1(testis,'Testis as an ingredient').
concept_to_ignore_1(therapeutics,'The science and art of healing'). % GR 08/2015
concept_to_ignore_1(therapy,'Encounter due to therapy'). %MF
concept_to_ignore_1(therapy,'therapeutic aspects'). % GR 1/11/13
concept_to_ignore_1(think,'Thinking and speaking disturbances').
concept_to_ignore_1(thinking,'Thinking and speaking disturbances').
concept_to_ignore_1(thought,'Thinking and speaking disturbances').
concept_to_ignore_1(threat,'THREAT'). % GR 06/2015
concept_to_ignore_1(top,'Termination of pregnancy'). % GR 06/2015
concept_to_ignore_1('TOP','Termination of pregnancy'). % GR 06/2015
concept_to_ignore_1(trail,'TNF-related apoptosis-inducing ligand'). % GR
concept_to_ignore_1(transplantation,'Clinical Nurse Specialist - Transplantation'). % GR 09/2015
concept_to_ignore_1(treatment,'therapeutic aspects'). % ftcn %GR 1/11/13
concept_to_ignore_1(trust,'Trust'). % GR
concept_to_ignore_1('type 2','Type 2'). % GR 2015
concept_to_ignore_1(suppression,'Visual Suppression').
concept_to_ignore_1(suppression,'% suppression'). % GR 06/2015
concept_to_ignore_1(uncouple,'Uncouplers').
concept_to_ignore_1(uncoupled,'Uncouplers').
concept_to_ignore_1(uncoupler,'Uncouplers').
concept_to_ignore_1(uncouples,'Uncouplers').
concept_to_ignore_1(uncoupling,'Uncouplers').
concept_to_ignore_1(understand,'Comprehension').
concept_to_ignore_1(understand,'UNDERSTOOD').
concept_to_ignore_1(understanding,'Comprehension').
concept_to_ignore_1(understanding,'UNDERSTOOD').
concept_to_ignore_1(understood,'Comprehension').
concept_to_ignore_1(understood,'UNDERSTOOD').
concept_to_ignore_1(unrelated,'Unrelated').
concept_to_ignore_1(urine,'In Urine'). % GR 09/2015
concept_to_ignore_1(utility,'Utilities'). % GR 06/29/2016
concept_to_ignore_1('UTMD','Candidate Disease Gene'). % GR 08/2015
concept_to_ignore_1(various,'Various patch test substance').
concept_to_ignore_1(variously,'Various patch test substance').
concept_to_ignore_1(variousness,'Various patch test substance').
concept_to_ignore_1(vascular,'Adverse Event Associated with Vascular').
concept_to_ignore_1(vascularity,'Adverse Event Associated with Vascular').
%concept_to_ignore_1('very low','Very low'). % GR 06/2015
concept_to_ignore_1(vascularly,'Adverse Event Associated with Vascular').
concept_to_ignore_1(vulnerability,'Injury wounds').
concept_to_ignore_1(vulnerability,'Specimen Type - Wound').
concept_to_ignore_1(vulnerability,'Route of Administration - Wound').
concept_to_ignore_1(vulnerability,'Specimen Source Codes - Wound').
concept_to_ignore_1(vulnerability,'Wounded').
concept_to_ignore_1(vulnerability,'Wounds - qualifier').
concept_to_ignore_1(vulnerable,'Injury wounds').
concept_to_ignore_1(vulnerable,'Specimen Type - Wound').
concept_to_ignore_1(vulnerable,'Route of Administration - Wound').
concept_to_ignore_1(vulnerable,'Specimen Source Codes - Wound').
concept_to_ignore_1(vulnerable,'Wounded').
concept_to_ignore_1(vulnerable,'Wounds - qualifier').
concept_to_ignore_1(vulneration,'Injury wounds').
concept_to_ignore_1(vulneration,'Specimen Type - Wound').
concept_to_ignore_1(vulneration,'Route of Administration - Wound').
concept_to_ignore_1(vulneration,'Specimen Source Codes - Wound').
concept_to_ignore_1(vulneration,'Wounded').
concept_to_ignore_1(vulneration,'Wounds - qualifier').
concept_to_ignore_1(vulnerant,'Injury wounds').
concept_to_ignore_1(vulnerant,'Specimen Type - Wound').
concept_to_ignore_1(vulnerant,'Route of Administration - Wound').
concept_to_ignore_1(vulnerant,'Specimen Source Codes - Wound').
concept_to_ignore_1(vulnerant,'Wounded').
concept_to_ignore_1(vulnerant,'Wounds - qualifier').
concept_to_ignore_1(weight,'Weighing patient'). % GR 1/11/13
concept_to_ignore_1(well,'Eosinophilic cellulitis'). % GR 08/2015
concept_to_ignore_1(wells,'Eosinophilic cellulitis'). % GR 06/2015
concept_to_ignore_1(west,'Infantile spasms'). % GR
concept_to_ignore_1(white,'White color finding'). % GR
concept_to_ignore_1(willingness,'Willing'). % GR 06/2015
concept_to_ignore_1(woman,'General Acute Care Hospital - Women').
concept_to_ignore_1(women,'General Acute Care Hospital - Women').
concept_to_ignore_1(woman,'Psychologist - Women'). % GR changed pl to sg.
concept_to_ignore_1(zone,'Zonal'). % GR 06/2015
concept_to_ignore_2N(a,[-,track],'Track'). % GR 09/2015
concept_to_ignore_2N(albicans,[agar,section],'Cesarean section'). % GR 06/2015
concept_to_ignore_2N(agar,[section],'Cesarean section'). % GR 09/2015
concept_to_ignore_2N(alpha,[-,particle],'Alpha Particles'). % GR 09/2015
concept_to_ignore_2N(anti,[-,'TNF'],'TNF gene'). % GR 06/2016
concept_to_ignore_2N(base,[line],'BaseLine dental material').
concept_to_ignore_2N('Bcl-2',[family],'Family').
concept_to_ignore_2N(body,[weight],'Weighing patient'). % GR 09/2015
concept_to_ignore_2N(cancer,[-,free,patient],'Cancer Patient'). % GR 09/2015
concept_to_ignore_2N(cardiovascular,[disease],'Allopathic & Osteopathic Physicians - Cardiovascular Disease'). % GR
concept_to_ignore_2N(daughter,[cell],'Daughter'). % GR 06/2015
concept_to_ignore_2N(driver,[parameter],'Drivers of Vehicles'). % GR 10/2015
concept_to_ignore_2N(equilibration,[buffer],'Buffers'). % GR 06/2015
concept_to_ignore_2N(executive,[function],'Executives'). % GR 06/2015
concept_to_ignore_2N(family,[history,of],'Family'). % GR 06/2015
concept_to_ignore_2N(fasting,[concentration],'Fasting'). % GR 10/2015
concept_to_ignore_2N(fasting,[glucose],'Glucose'). % GR 06/2015
concept_to_ignore_2N(fasting,[level],'Fasting'). % GR 06/2015
concept_to_ignore_2N(fasting,[state],'Fasting'). % GR 06/2015
concept_to_ignore_2N(follow,[-,up],'Follow-up status'). % GR 08/2015
concept_to_ignore_2N(health,[disparity],'health disparity'). % GR 06/2015
concept_to_ignore_2N(human,[airways],'Airway device'). % GR 10/2015
concept_to_ignore_2N('IVA',[disease],'Iva'). % GR 09/2015
concept_to_ignore_2N(image,[quality],'IMAGE QUALITY'). % GR 06/2015
concept_to_ignore_2N(immigrant,[cell],'Immigrants'). % GR 08/2015
concept_to_ignore_2N(immigrant,[inflammatory,cell],'Immigrants'). % GR 06/2015  Not working (08/2015)
concept_to_ignore_2N(inflammatory,[bowel, disease],'Acute Ulcerative Enteritis of the Mouse Intestinal Tract').
concept_to_ignore_2N(internal,[radiation,therapy],'Code System Type - Internal'). % GR 09/2015
concept_to_ignore_2N(lower,[fasting,glucose],'Glucose'). % GR 10/2015
concept_to_ignore_2N(kidney,[donor],'Encounter due to kidney donor status'). % GR 08/2015
concept_to_ignore_2N(mammalian,[egg],'Egg Food Product'). % GR 09/2015
concept_to_ignore_2N(mouse,[pad],'House mice'). % GR 06/2015
concept_to_ignore_2N(multiple,[sclerosis],'MS gene').
concept_to_ignore_2N(nicotine,[dose],'NICOTINE:MASS:POINT IN TIME:DOSE MED OR SUBSTANCE:QUANTITATIVE'). % GR 09/2015
concept_to_ignore_2N(non,[-],'NON Mouse').
concept_to_ignore_2N(non,[coding],'Functional RNA').
concept_to_ignore_2N(patient,[management],'Management procedure'). % GR 08/2015
concept_to_ignore_2N(peer,[support],'peer prevention'). % GR 06/28/2016
concept_to_ignore_2N(plaque,[renal,artery],'Senile Plaques'). % GR 09/2015
concept_to_ignore_2N(prophylactic,[antibiotic],'Other prophylactic chemotherapy'). % GR 09/2015
concept_to_ignore_2N(progenitor,[cell],'Ancestor'). % GR 06/2015
concept_to_ignore_2N(protein,[level],'Protein level - finding'). % GR 09/2015
concept_to_ignore_2N(public,[health],'Public Health Podiatrist'). % GR
concept_to_ignore_2N(recent,[past],recent). % GR 09/2015
concept_to_ignore_2N(red,[blood, cell],'% Ercs').
concept_to_ignore_2N(red,[blood,cells],'% Ercs').
concept_to_ignore_2N(rhesus,[-],'Macaca mulatta'). % GR 09/2015
concept_to_ignore_2N(slow,[down],'Lanugo'). % GR 09/2015
concept_to_ignore_2N(short,[chain],'Shortened'). % GR 10/2015
concept_to_ignore_2N(short,[period],'Short menstrual periods'). % GR 06/28/2016
concept_to_ignore_2N(small,[heart,size],'SMALL SIZE'). % GR 06/2015
concept_to_ignore_2N(small,[size],'SMALL SIZE'). % GR 06/2015
concept_to_ignore_2N(stage,[iva],'Iva'). % GR 08/2015
concept_to_ignore_2N(surgical,[menopause],'Postsurgical menopause'). % GR 06/2015
concept_to_ignore_2N(transplant,[tolerance],'Physiologic tolerance'). % GR 08/2015
concept_to_ignore_2N(very,[low],'Very low'). % GR 08/2015
concept_to_ignore_2N(weak,[effect],'Asthenia'). % GR 09/2015
concept_to_ignore_2N(weight,[loss],'Weight Loss substance').
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% These next predicates are for transforming
%%% terms of the form
%%%    * preferred_relation/1,
%%%    * relation_inverse/2, and
%%%    * type_relation_type/3,
%%% all of which are defined in semnet_accessXX.pl.
%%% These predicates are called from pre_compilation.pl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% associated_with becomes ROOT_RELATION, which we ignore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% These next predicates are for transforming
%%%% terms of the form
%%%%    * preferred_relation/1,
%%%%    * relation_inverse/2, and
%%%%    * type_relation_type/3,
%%%% all of which are defined in semnet_accessXX.pl.
%%%% These predicates are called from pre_compilation.pl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% associated_with becomes ROOT_RELATION, which we ignore
% associated_with becomes ROOT_RELATION, which we ignore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% These next predicates are for transforming
%%%% terms of the form
%%%%    * preferred_relation/1,
%%%%    * relation_inverse/2, and
%%%%    * type_relation_type/3,
%%%% all of which are defined in semnet_accessXX.pl.
%%%% These predicates are called from pre_compilation.pl.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


transform_semnet_access_term_DOM(preferred_relation(Rel),
		preferred_relation(NewRel)) :-
	( transform_preferred_relation_DOM(Rel, NewRel) ->
	  true
	; NewRel = Rel
	).

transform_semnet_access_term_DOM(relation_inverse(Rel,    InverseRel),
		relation_inverse(NewRel, NewInverseRel)) :-
	( transform_relation_inverse_DOM(Rel, InverseRel, NewRel, NewInverseRel) ->
	  true
	; NewRel = Rel,
	  NewInverseRel = InverseRel
	).

transform_semnet_access_term_DOM(type_relation_type(Type1,    Rel,    Type2),
		type_relation_type(NewType1, NewRel, NewType2)) :-
	(transform_type_relation_type_DOM(Type1, Rel, Type2, NewType1, NewRel, NewType2) ->
	  true
	; NewType1 = Type1,
	  NewRel = Rel,
	  NewType2 = Type2
	).

ignore_semnet_access_term_DOM(preferred_relation(Rel)) :-
	ignore_preferred_relation_DOM(Rel).

ignore_semnet_access_term_DOM(relation_inverse(Rel, InverseRel)) :-
	ignore_relation_inverse_DOM(Rel, InverseRel).

ignore_semnet_access_term_DOM(type_relation_type(Type1, Rel, Type2)) :-
	ignore_type_relation_type_DOM(Type1, Rel, Type2).

transform_preferred_relation_DOM('co-occurs_with', coexists_with).

ignore_preferred_relation_DOM(associated_with).

transform_relation_inverse_DOM('co-occurs_with', 'co-occurs_with', coexists_with, coexists_with).

% associated_with becomes ROOT_RELATION, which we ignore

ignore_relation_inverse_DOM(associated_with,  associated_with).

transform_type_relation_type_DOM(Type1,    Rel,    Type2,
		NewType1, NewRel, NewType2) :-
	transform_preferred_relation_if_possible(Type1, NewType1),
	transform_preferred_relation_if_possible(Rel,   NewRel),
	transform_preferred_relation_if_possible(Type2, NewType2).

transform_preferred_relation_if_possible(Rel, TransformedRel) :-
	( transform_preferred_relation_DOM(Rel, TransformedRel) ->
	  true
	; TransformedRel = Rel
).

ignore_type_relation_type_DOM(Type1, Relation, Type2) :-
	( ignore_preferred_relation_DOM(Type1)    ->
	  true
	; ignore_preferred_relation_DOM(Relation) ->
	  true
	; ignore_preferred_relation_DOM(Type2)    ->
	  true
	; ignore_type_relation_type_1_DOM(Type1, Relation, Type2)
).

% associated_with becomes ROOT_RELATION, which we ignore
ignore_type_relation_type_1_DOM(biof,process_of,patf).
ignore_type_relation_type_1_DOM(blor,location_of,tisu).
ignore_type_relation_type_1_DOM(celf,process_of,patf).
ignore_type_relation_type_1_DOM(comd,process_of,patf).
ignore_type_relation_type_1_DOM(genf,process_of,orgf).
ignore_type_relation_type_1_DOM(genf,process_of,patf).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,antb).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,bacs).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,enzy).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,hops).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,horm).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,imft).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,inpo).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,nsba).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,phsu).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,rcpt).
ignore_type_relation_type_1_DOM(gngm,disrupted_by,vita).
ignore_type_relation_type_1_DOM(gngm,location_of,bact).
ignore_type_relation_type_1_DOM(gngm,location_of,emod).
ignore_type_relation_type_1_DOM(gngm,location_of,fngs).
ignore_type_relation_type_1_DOM(gngm,location_of,neop).
ignore_type_relation_type_1_DOM(gngm,location_of,rich).
ignore_type_relation_type_1_DOM(gngm,location_of,virs).
ignore_type_relation_type_1_DOM(nsba,disrupts,gngm).
ignore_type_relation_type_1_DOM(patf,process_of,orgf).
ignore_type_relation_type_1_DOM(patf,process_of,patf).
ignore_type_relation_type_1_DOM(tisu,has_location,blor).
ignore_type_relation_type_1_DOM(antb,disrupts,gngm).
ignore_type_relation_type_1_DOM(bacs,disrupts,gngm).
ignore_type_relation_type_1_DOM(phsu,disrupts,gngm).
ignore_type_relation_type_1_DOM(rcpt,disrupts,gngm).
ignore_type_relation_type_1_DOM(vita,disrupts,gngm).
ignore_type_relation_type_1_DOM(dsyn,process_of,genf).
ignore_type_relation_type_1_DOM(genf,process_of,genf).
ignore_type_relation_type_1_DOM(genf,process_of,phsf).
ignore_type_relation_type_1_DOM(genf,has_process,genf). % GR 08/2015
ignore_type_relation_type_1_DOM(gngm,location_of,anab).
ignore_type_relation_type_1_DOM(gngm,location_of,biof).
ignore_type_relation_type_1_DOM(gngm,location_of,celf).
ignore_type_relation_type_1_DOM(gngm,location_of,cgab).
ignore_type_relation_type_1_DOM(gngm,location_of,comd).
ignore_type_relation_type_1_DOM(gngm,location_of,dsyn).
ignore_type_relation_type_1_DOM(gngm,location_of,genf).
ignore_type_relation_type_1_DOM(gngm,location_of,inpo).
ignore_type_relation_type_1_DOM(gngm,location_of,menp).
ignore_type_relation_type_1_DOM(gngm,location_of,mobd).
ignore_type_relation_type_1_DOM(gngm,location_of,moft).
ignore_type_relation_type_1_DOM(gngm,location_of,orgf).
ignore_type_relation_type_1_DOM(gngm,location_of,ortf).
ignore_type_relation_type_1_DOM(gngm,location_of,patf).
ignore_type_relation_type_1_DOM(gngm,location_of,phsf).
ignore_type_relation_type_1_DOM(neop,process_of,biof).
ignore_type_relation_type_1_DOM(neop,process_of,genf).
ignore_type_relation_type_1_DOM(patf,has_process,dsyn).
ignore_type_relation_type_1_DOM(patf,process_of,phsf).
ignore_type_relation_type_1_DOM(gngm,location_of,acab).
ignore_type_relation_type_1_DOM(hlca,affects,menp).
ignore_type_relation_type_1_DOM(hlca,affects,moft).
ignore_type_relation_type_1_DOM(menp,affected_by,hlca).
ignore_type_relation_type_1_DOM(moft,affected_by,hlca).
ignore_type_relation_type_1_DOM(acab,result_of,hlca).
ignore_type_relation_type_1_DOM(acty,inverse_isa,hlca).
ignore_type_relation_type_1_DOM(aggp,performs,hlca).
ignore_type_relation_type_1_DOM(anab,result_of,hlca).
ignore_type_relation_type_1_DOM(biof,affected_by,hlca).
ignore_type_relation_type_1_DOM(biof,has_location,gngm).
ignore_type_relation_type_1_DOM(biof,has_process,menp).
ignore_type_relation_type_1_DOM(biof,has_process,moft).
ignore_type_relation_type_1_DOM(bird,affected_by,menp).
ignore_type_relation_type_1_DOM(bird,affected_by,moft).
ignore_type_relation_type_1_DOM(bird,exhibits,socb).
%ignore_type_relation_type_1_DOM(bird,has_process,menp). % Unblocked 06/2015 by GR
%ignore_type_relation_type_1_DOM(bird,has_process,moft). % Unblocked 06/2015 by GR
ignore_type_relation_type_1_DOM(bird,has_property,clna).
ignore_type_relation_type_1_DOM(bird,interacts_with,humn).
ignore_type_relation_type_1_DOM(bird,interacts_with,mamm).
ignore_type_relation_type_1_DOM(bird,interacts_with,plnt).
ignore_type_relation_type_1_DOM(blor,location_of,acab).
ignore_type_relation_type_1_DOM(bmod,has_issue,hlca).
ignore_type_relation_type_1_DOM(bmod,has_method,hlca).
ignore_type_relation_type_1_DOM(bpoc,location_of,acab).
ignore_type_relation_type_1_DOM(bsoj,location_of,acab).
ignore_type_relation_type_1_DOM(celc,location_of,acab).
ignore_type_relation_type_1_DOM(celf,affected_by,hlca).
ignore_type_relation_type_1_DOM(celf,has_location,gngm).
ignore_type_relation_type_1_DOM(celf,has_process,menp).
ignore_type_relation_type_1_DOM(celf,has_process,moft).
ignore_type_relation_type_1_DOM(cell,location_of,acab).
ignore_type_relation_type_1_DOM(cgab,result_of,hlca).
ignore_type_relation_type_1_DOM(chvf,affects,menp).
ignore_type_relation_type_1_DOM(chvf,affects,moft).
ignore_type_relation_type_1_DOM(chvf,affects,orga). % 07/2015
ignore_type_relation_type_1_DOM(chvf,affects,plnt). % 07/2015
ignore_type_relation_type_1_DOM(clna,affected_by,menp).
ignore_type_relation_type_1_DOM(clna,affected_by,moft).
ignore_type_relation_type_1_DOM(clna,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(clna,manifestation_of,menp).
ignore_type_relation_type_1_DOM(clna,manifestation_of,moft).
ignore_type_relation_type_1_DOM(clna,measurement_of,menp).
ignore_type_relation_type_1_DOM(clna,measurement_of,moft).
ignore_type_relation_type_1_DOM(clna,property_of,bird).
ignore_type_relation_type_1_DOM(clna,property_of,mamm).
ignore_type_relation_type_1_DOM(clna,property_of,plnt).
ignore_type_relation_type_1_DOM(clna,result_of,menp).
ignore_type_relation_type_1_DOM(clna,result_of,moft).
ignore_type_relation_type_1_DOM(comd,affected_by,hlca).
ignore_type_relation_type_1_DOM(comd,has_location,gngm).
ignore_type_relation_type_1_DOM(comd,has_process,menp).
ignore_type_relation_type_1_DOM(comd,has_process,moft).
ignore_type_relation_type_1_DOM(comd,result_of,hlca).
ignore_type_relation_type_1_DOM(diap,isa,hlca).
ignore_type_relation_type_1_DOM(dsyn,affected_by,hlca).
ignore_type_relation_type_1_DOM(dsyn,has_process,menp).
ignore_type_relation_type_1_DOM(dsyn,has_process,moft).
ignore_type_relation_type_1_DOM(dsyn,process_of,ortf).
ignore_type_relation_type_1_DOM(dsyn,process_of,phsf).
ignore_type_relation_type_1_DOM(dsyn,result_of,hlca).
ignore_type_relation_type_1_DOM(emod,affected_by,hlca).
ignore_type_relation_type_1_DOM(emod,result_of,hlca).
ignore_type_relation_type_1_DOM(evnt,inverse_isa,hlca).
ignore_type_relation_type_1_DOM(famg,performs,hlca).
ignore_type_relation_type_1_DOM(ffas,location_of,acab).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,clna).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,menp).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,moft).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,menp).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,moft).
ignore_type_relation_type_1_DOM(fndg,has_manifestation,sosy). % GR 08/2015
ignore_type_relation_type_1_DOM(genf,affected_by,hlca).
ignore_type_relation_type_1_DOM(genf,has_location,gngm).
ignore_type_relation_type_1_DOM(grup,performs,hlca).
ignore_type_relation_type_1_DOM(hcro,carries_out,hlca).
ignore_type_relation_type_1_DOM(hcro,location_of,hlca).
ignore_type_relation_type_1_DOM(hlca,affects,biof).
ignore_type_relation_type_1_DOM(hlca,affects,celf).
ignore_type_relation_type_1_DOM(hlca,affects,comd).
ignore_type_relation_type_1_DOM(hlca,affects,dsyn).
ignore_type_relation_type_1_DOM(hlca,affects,emod).
ignore_type_relation_type_1_DOM(hlca,affects,genf).
ignore_type_relation_type_1_DOM(hlca,affects,mobd).
ignore_type_relation_type_1_DOM(hlca,affects,neop).
ignore_type_relation_type_1_DOM(hlca,affects,orgf).
ignore_type_relation_type_1_DOM(hlca,affects,ortf).
ignore_type_relation_type_1_DOM(hlca,affects,patf).
ignore_type_relation_type_1_DOM(hlca,affects,phsf).
ignore_type_relation_type_1_DOM(hlca,carried_out_by,hcro).
ignore_type_relation_type_1_DOM(hlca,carried_out_by,orgt).
ignore_type_relation_type_1_DOM(hlca,carried_out_by,pros).
ignore_type_relation_type_1_DOM(hlca,carried_out_by,shro).
ignore_type_relation_type_1_DOM(hlca,has_evaluation,qlco).
ignore_type_relation_type_1_DOM(hlca,has_location,hcro).
ignore_type_relation_type_1_DOM(hlca,has_location,orgt).
ignore_type_relation_type_1_DOM(hlca,has_location,pros).
ignore_type_relation_type_1_DOM(hlca,has_location,shro).
ignore_type_relation_type_1_DOM(hlca,has_result,acab).
ignore_type_relation_type_1_DOM(hlca,has_result,anab).
ignore_type_relation_type_1_DOM(hlca,has_result,cgab).
ignore_type_relation_type_1_DOM(hlca,has_result,comd).
ignore_type_relation_type_1_DOM(hlca,has_result,dsyn).
ignore_type_relation_type_1_DOM(hlca,has_result,emod).
ignore_type_relation_type_1_DOM(hlca,has_result,inpo).
ignore_type_relation_type_1_DOM(hlca,has_result,mobd).
ignore_type_relation_type_1_DOM(hlca,has_result,neop).
ignore_type_relation_type_1_DOM(hlca,has_result,patf).
ignore_type_relation_type_1_DOM(hlca,inverse_isa,diap).
ignore_type_relation_type_1_DOM(hlca,inverse_isa,lbpr).
ignore_type_relation_type_1_DOM(hlca,inverse_isa,topp).
ignore_type_relation_type_1_DOM(hlca,isa,acty).
ignore_type_relation_type_1_DOM(hlca,isa,evnt).
ignore_type_relation_type_1_DOM(hlca,isa,ocac).
ignore_type_relation_type_1_DOM(hlca,issue_in,bmod).
ignore_type_relation_type_1_DOM(hlca,issue_in,ocdi).
ignore_type_relation_type_1_DOM(hlca,method_of,bmod).
ignore_type_relation_type_1_DOM(hlca,method_of,ocdi).
ignore_type_relation_type_1_DOM(hlca,performed_by,aggp).
ignore_type_relation_type_1_DOM(hlca,performed_by,famg).
ignore_type_relation_type_1_DOM(hlca,performed_by,grup).
ignore_type_relation_type_1_DOM(hlca,performed_by,podg).
ignore_type_relation_type_1_DOM(hlca,performed_by,popg).
ignore_type_relation_type_1_DOM(hlca,performed_by,prog).
ignore_type_relation_type_1_DOM(humn,interacts_with,bird).
ignore_type_relation_type_1_DOM(humn,interacts_with,humn).
ignore_type_relation_type_1_DOM(humn,interacts_with,mamm).
ignore_type_relation_type_1_DOM(humn,interacts_with,plnt).
ignore_type_relation_type_1_DOM(inpo,result_of,hlca).
ignore_type_relation_type_1_DOM(lbpr,affects,menp).
ignore_type_relation_type_1_DOM(lbpr,affects,moft).
ignore_type_relation_type_1_DOM(lbpr,isa,hlca).
ignore_type_relation_type_1_DOM(mamm,affected_by,menp).
ignore_type_relation_type_1_DOM(mamm,affected_by,moft).
ignore_type_relation_type_1_DOM(mamm,exhibits,socb).
ignore_type_relation_type_1_DOM(mamm,has_property,clna).
ignore_type_relation_type_1_DOM(mamm,interacts_with,bird).
ignore_type_relation_type_1_DOM(mamm,interacts_with,humn).
ignore_type_relation_type_1_DOM(mamm,interacts_with,plnt).
ignore_type_relation_type_1_DOM(menp,affected_by,chvf).
ignore_type_relation_type_1_DOM(menp,affected_by,lbpr).
ignore_type_relation_type_1_DOM(menp,affected_by,moft).
ignore_type_relation_type_1_DOM(menp,affected_by,socb).
ignore_type_relation_type_1_DOM(menp,affects,bird).
ignore_type_relation_type_1_DOM(menp,affects,clna).
ignore_type_relation_type_1_DOM(menp,affects,mamm).
ignore_type_relation_type_1_DOM(menp,affects,moft).
ignore_type_relation_type_1_DOM(menp,affects,plnt).
ignore_type_relation_type_1_DOM(menp,affects,socb).
ignore_type_relation_type_1_DOM(menp,coexists_with,moft).
ignore_type_relation_type_1_DOM(menp,follows,moft).
ignore_type_relation_type_1_DOM(menp,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(menp,has_location,gngm).
ignore_type_relation_type_1_DOM(menp,has_manifestation,clna).
ignore_type_relation_type_1_DOM(menp,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(menp,has_manifestation,socb).
ignore_type_relation_type_1_DOM(menp,has_measurement,clna).
ignore_type_relation_type_1_DOM(menp,has_process,moft).
ignore_type_relation_type_1_DOM(menp,has_result,clna).
ignore_type_relation_type_1_DOM(menp,has_result,moft).
ignore_type_relation_type_1_DOM(menp,has_result,socb).
ignore_type_relation_type_1_DOM(menp,precedes,moft).
ignore_type_relation_type_1_DOM(menp,process_of,alga). % GR 09/14/09
ignore_type_relation_type_1_DOM(menp,process_of,bact). % GR 09/16/09
ignore_type_relation_type_1_DOM(menp,process_of,fngs). % GR 09/14/09
ignore_type_relation_type_1_DOM(menp,process_of,plnt). % GR 09/16/09
ignore_type_relation_type_1_DOM(menp,process_of,rich). % GR 09/16/09
ignore_type_relation_type_1_DOM(menp,process_of,virs). % GR 09/16/09
ignore_type_relation_type_1_DOM(menp,process_of,biof).
%ignore_type_relation_type_1_DOM(menp,process_of,bird). % Unblocked 06/2015 by GR
ignore_type_relation_type_1_DOM(menp,process_of,dsyn).
ignore_type_relation_type_1_DOM(menp,process_of,genf).
%ignore_type_relation_type_1_DOM(menp,process_of,mamm). % Unblocked 06/2015 by GR
ignore_type_relation_type_1_DOM(menp,process_of,moft).
ignore_type_relation_type_1_DOM(menp,process_of,npop).
ignore_type_relation_type_1_DOM(menp,process_of,ortf).
ignore_type_relation_type_1_DOM(menp,process_of,phsf).
ignore_type_relation_type_1_DOM(menp,result_of,moft).
ignore_type_relation_type_1_DOM(menp,result_of,socb).
ignore_type_relation_type_1_DOM(mobd,affected_by,hlca).
ignore_type_relation_type_1_DOM(mobd,has_process,menp).
ignore_type_relation_type_1_DOM(mobd,has_process,moft).
ignore_type_relation_type_1_DOM(mobd,result_of,hlca).
ignore_type_relation_type_1_DOM(moft,affected_by,chvf).
ignore_type_relation_type_1_DOM(moft,affected_by,lbpr).
ignore_type_relation_type_1_DOM(moft,affected_by,menp).
ignore_type_relation_type_1_DOM(moft,affects,bird).
ignore_type_relation_type_1_DOM(moft,affects,clna).
ignore_type_relation_type_1_DOM(moft,affects,mamm).
ignore_type_relation_type_1_DOM(moft,affects,menp).
ignore_type_relation_type_1_DOM(moft,affects,plnt).
ignore_type_relation_type_1_DOM(moft,coexists_with,menp).
ignore_type_relation_type_1_DOM(moft,follows,menp).
ignore_type_relation_type_1_DOM(moft,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(moft,has_location,gngm).
ignore_type_relation_type_1_DOM(moft,has_manifestation,clna).
ignore_type_relation_type_1_DOM(moft,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(moft,has_measurement,clna).
ignore_type_relation_type_1_DOM(moft,has_part,nusq). % GR 08/2015
ignore_type_relation_type_1_DOM(moft,has_process,menp).
ignore_type_relation_type_1_DOM(moft,has_result,clna).
ignore_type_relation_type_1_DOM(moft,has_result,menp).
ignore_type_relation_type_1_DOM(moft,precedes,menp).
ignore_type_relation_type_1_DOM(moft,process_of,biof).
%ignore_type_relation_type_1_DOM(moft,process_of,bird). % Unblocked 06/2015 by GR
ignore_type_relation_type_1_DOM(moft,process_of,dsyn).
ignore_type_relation_type_1_DOM(moft,process_of,genf).
%ignore_type_relation_type_1_DOM(moft,process_of,mamm). % Unblocked 06/2015 by GR
ignore_type_relation_type_1_DOM(moft,process_of,menp).
ignore_type_relation_type_1_DOM(moft,process_of,npop).
ignore_type_relation_type_1_DOM(moft,process_of,ortf).
ignore_type_relation_type_1_DOM(moft,process_of,phsf).
ignore_type_relation_type_1_DOM(moft,process_of,plnt).
ignore_type_relation_type_1_DOM(moft,result_of,menp).
ignore_type_relation_type_1_DOM(neop,affected_by,hlca).
ignore_type_relation_type_1_DOM(alga,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,has_process,menp).
ignore_type_relation_type_1_DOM(neop,has_process,moft).
ignore_type_relation_type_1_DOM(neop,process_of,ortf).
ignore_type_relation_type_1_DOM(neop,process_of,phsf).
ignore_type_relation_type_1_DOM(neop,process_of,vtbt). % GR 08/2015
ignore_type_relation_type_1_DOM(neop,result_of,hlca).
ignore_type_relation_type_1_DOM(ocac,inverse_isa,hlca).
ignore_type_relation_type_1_DOM(ocdi,has_issue,hlca).
ignore_type_relation_type_1_DOM(ocdi,has_manifestation,socb). % GR 08/2015
ignore_type_relation_type_1_DOM(ocdi,has_method,hlca).
ignore_type_relation_type_1_DOM(orgf,affected_by,hlca).
ignore_type_relation_type_1_DOM(orgf,has_location,gngm).
ignore_type_relation_type_1_DOM(orgf,has_process,menp).
ignore_type_relation_type_1_DOM(orgf,has_process,moft).
ignore_type_relation_type_1_DOM(orgt,carries_out,hlca).
ignore_type_relation_type_1_DOM(orgt,location_of,hlca).
ignore_type_relation_type_1_DOM(orgt,location_of,topp). % GR 07/2015
ignore_type_relation_type_1_DOM(ortf,affected_by,hlca).
ignore_type_relation_type_1_DOM(ortf,has_location,gngm).
ignore_type_relation_type_1_DOM(patf,affected_by,hlca).
ignore_type_relation_type_1_DOM(patf,has_location,gngm).
ignore_type_relation_type_1_DOM(patf,has_process,menp).
ignore_type_relation_type_1_DOM(patf,has_process,moft).
ignore_type_relation_type_1_DOM(patf,result_of,hlca).
ignore_type_relation_type_1_DOM(phsf,affected_by,hlca).
ignore_type_relation_type_1_DOM(phsf,has_location,gngm).
ignore_type_relation_type_1_DOM(phsf,has_process,emod). % GR 08/2015
ignore_type_relation_type_1_DOM(phsf,has_process,menp).
ignore_type_relation_type_1_DOM(phsf,has_process,moft).
ignore_type_relation_type_1_DOM(plnt,affected_by,menp).
ignore_type_relation_type_1_DOM(plnt,affected_by,moft).
ignore_type_relation_type_1_DOM(plnt,coexists_with,phsf). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,has_process,mobd). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,has_process,moft).
ignore_type_relation_type_1_DOM(plnt,has_property,clna).
ignore_type_relation_type_1_DOM(plnt,interacts_with,bird).
ignore_type_relation_type_1_DOM(plnt,interacts_with,humn).
ignore_type_relation_type_1_DOM(plnt,interacts_with,mamm).
ignore_type_relation_type_1_DOM(podg,performs,hlca).
ignore_type_relation_type_1_DOM(popg,performs,hlca).
ignore_type_relation_type_1_DOM(prog,performs,hlca).
ignore_type_relation_type_1_DOM(pros,carries_out,hlca).
ignore_type_relation_type_1_DOM(pros,location_of,hlca).
ignore_type_relation_type_1_DOM(pros,location_of,topp). % GR 07/2015
ignore_type_relation_type_1_DOM(pros,produces,inpr). % GR 08/2015
ignore_type_relation_type_1_DOM(qlco,evaluation_of,hlca).
ignore_type_relation_type_1_DOM(shro,carries_out,hlca).
ignore_type_relation_type_1_DOM(shro,location_of,hlca).
ignore_type_relation_type_1_DOM(shro,location_of,topp). % GR 07/2015
ignore_type_relation_type_1_DOM(socb,affected_by,menp).
ignore_type_relation_type_1_DOM(socb,affects,menp).
ignore_type_relation_type_1_DOM(socb,exhibited_by,bird).
ignore_type_relation_type_1_DOM(socb,exhibited_by,mamm).
ignore_type_relation_type_1_DOM(socb,has_result,menp).
ignore_type_relation_type_1_DOM(socb,manifestation_of,menp).
ignore_type_relation_type_1_DOM(socb,result_of,menp).
ignore_type_relation_type_1_DOM(tisu,has_process,clna). % GR 06/2015
ignore_type_relation_type_1_DOM(tisu,location_of,acab).
ignore_type_relation_type_1_DOM(tisu,part_of,topp). % GR 08/2015
ignore_type_relation_type_1_DOM(topp,isa,hlca).
ignore_type_relation_type_1_DOM(topp,has_part,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(aapp,affects,menp).
%ignore_type_relation_type_1_DOM(aapp,affects,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(aapp,interacts_with,chvf).
ignore_type_relation_type_1_DOM(aapp,diagnosed_by,dsyn). % 07/2015
ignore_type_relation_type_1_DOM(acab,affects,bird).
ignore_type_relation_type_1_DOM(acab,affects,mamm).
ignore_type_relation_type_1_DOM(acab,affects,menp).
ignore_type_relation_type_1_DOM(acab,affects,moft).
ignore_type_relation_type_1_DOM(acab,affects,plnt).
%ignore_type_relation_type_1_DOM(acab,caused_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(acab,caused_by,mnob).
ignore_type_relation_type_1_DOM(acab,has_location,gngm).
ignore_type_relation_type_1_DOM(acab,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(acab,has_result,menp).
ignore_type_relation_type_1_DOM(acab,has_result,moft).
ignore_type_relation_type_1_DOM(acab,location_of,comd).
ignore_type_relation_type_1_DOM(acab,location_of,dsyn).
ignore_type_relation_type_1_DOM(acab,location_of,mobd).
ignore_type_relation_type_1_DOM(acab,location_of,patf).
ignore_type_relation_type_1_DOM(acab,manifestation_of,menp).
ignore_type_relation_type_1_DOM(acab,manifestation_of,moft).
ignore_type_relation_type_1_DOM(acab,part_of,bird).
ignore_type_relation_type_1_DOM(acab,part_of,invt).
ignore_type_relation_type_1_DOM(acab,part_of,plnt).
ignore_type_relation_type_1_DOM(acab,result_of,menp).
ignore_type_relation_type_1_DOM(acab,result_of,moft).
ignore_type_relation_type_1_DOM(acab,result_of,socb).
ignore_type_relation_type_1_DOM(acty,inverse_isa,socb).
ignore_type_relation_type_1_DOM(aggp,exhibits,socb).
ignore_type_relation_type_1_DOM(aggp,has_occurrence,emod). % GR 07/2015
ignore_type_relation_type_1_DOM(aggp,performs,socb).
ignore_type_relation_type_1_DOM(aggp,produced_by,menp).
ignore_type_relation_type_1_DOM(aggp,produces,inpr).
ignore_type_relation_type_1_DOM(aggp,produces,mnob).
ignore_type_relation_type_1_DOM(aggp,uses,inpr).
ignore_type_relation_type_1_DOM(aggp,uses,mnob).
ignore_type_relation_type_1_DOM(alga,affected_by,menp).
ignore_type_relation_type_1_DOM(alga,affected_by,moft).
ignore_type_relation_type_1_DOM(alga,has_process,celf). % GR 07/2015
ignore_type_relation_type_1_DOM(alga,has_process,menp). % GR 07/2015
ignore_type_relation_type_1_DOM(alga,has_process,moft).
ignore_type_relation_type_1_DOM(alga,has_property,clna).
ignore_type_relation_type_1_DOM(alga,interacts_with,bird).
ignore_type_relation_type_1_DOM(alga,interacts_with,humn).
ignore_type_relation_type_1_DOM(alga,interacts_with,mamm).
ignore_type_relation_type_1_DOM(alga,interacts_with,plnt).
ignore_type_relation_type_1_DOM(alga,isa,plnt).
ignore_type_relation_type_1_DOM(amas,result_of,menp).
ignore_type_relation_type_1_DOM(amph,affected_by,menp).
ignore_type_relation_type_1_DOM(amph,affected_by,moft).
ignore_type_relation_type_1_DOM(amph,exhibits,socb).
ignore_type_relation_type_1_DOM(amph,has_process,menp).
ignore_type_relation_type_1_DOM(amph,has_process,moft).
ignore_type_relation_type_1_DOM(amph,has_property,clna).
ignore_type_relation_type_1_DOM(amph,interacts_with,bird).
ignore_type_relation_type_1_DOM(amph,interacts_with,humn).
ignore_type_relation_type_1_DOM(amph,interacts_with,mamm).
ignore_type_relation_type_1_DOM(amph,interacts_with,plnt).
ignore_type_relation_type_1_DOM(anab,affects,bird).
ignore_type_relation_type_1_DOM(anab,affects,mamm).
ignore_type_relation_type_1_DOM(anab,affects,menp).
ignore_type_relation_type_1_DOM(anab,affects,moft).
ignore_type_relation_type_1_DOM(anab,affects,plnt).
%ignore_type_relation_type_1_DOM(anab,caused_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(anab,caused_by,mnob).
ignore_type_relation_type_1_DOM(anab,has_location,gngm).
ignore_type_relation_type_1_DOM(anab,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(anab,has_result,menp).
ignore_type_relation_type_1_DOM(anab,has_result,moft).
ignore_type_relation_type_1_DOM(anab,location_of,comd).
ignore_type_relation_type_1_DOM(anab,location_of,dsyn).
ignore_type_relation_type_1_DOM(anab,location_of,mobd).
ignore_type_relation_type_1_DOM(anab,location_of,patf).
ignore_type_relation_type_1_DOM(anab,manifestation_of,menp).
ignore_type_relation_type_1_DOM(anab,manifestation_of,moft).
ignore_type_relation_type_1_DOM(anab,part_of,bird).
ignore_type_relation_type_1_DOM(anab,part_of,invt).
ignore_type_relation_type_1_DOM(anab,part_of,plnt).
ignore_type_relation_type_1_DOM(anab,result_of,menp).
ignore_type_relation_type_1_DOM(anab,result_of,moft).
ignore_type_relation_type_1_DOM(anim,affected_by,menp).
ignore_type_relation_type_1_DOM(anim,affected_by,moft).
ignore_type_relation_type_1_DOM(anim,exhibits,socb).
ignore_type_relation_type_1_DOM(anim,has_process,emod). % GR 07/2015
%ignore_type_relation_type_1_DOM(anim,has_process,menp). % Unblocked 06/2015 by GR
%ignore_type_relation_type_1_DOM(anim,has_process,moft). % Unblocked 06/2015 by GR
ignore_type_relation_type_1_DOM(anim,has_property,clna).
ignore_type_relation_type_1_DOM(anim,interacts_with,bird).
ignore_type_relation_type_1_DOM(anim,interacts_with,humn).
ignore_type_relation_type_1_DOM(anim,interacts_with,mamm).
ignore_type_relation_type_1_DOM(anim,interacts_with,plnt).
ignore_type_relation_type_1_DOM(anim,inverse_isa,bird).
ignore_type_relation_type_1_DOM(anim,inverse_isa,mamm).
ignore_type_relation_type_1_DOM(anst,part_of,bird).
ignore_type_relation_type_1_DOM(anst,part_of,invt).
ignore_type_relation_type_1_DOM(anst,part_of,plnt).
ignore_type_relation_type_1_DOM(antb,affects,menp).
ignore_type_relation_type_1_DOM(antb,affects,moft).
ignore_type_relation_type_1_DOM(antb,complicates,menp).
ignore_type_relation_type_1_DOM(antb,complicates,moft).
ignore_type_relation_type_1_DOM(antb,disrupts,menp).
ignore_type_relation_type_1_DOM(antb,disrupts,moft).
ignore_type_relation_type_1_DOM(antb,interacts_with,chvf).
ignore_type_relation_type_1_DOM(antb,diagnosed_by,aapp). % GR 07/2015
ignore_type_relation_type_1_DOM(antb,isa,chvf).
ignore_type_relation_type_1_DOM(arch,affected_by,menp).
ignore_type_relation_type_1_DOM(arch,affected_by,moft).
ignore_type_relation_type_1_DOM(arch,has_process,menp).
ignore_type_relation_type_1_DOM(arch,has_process,moft).
ignore_type_relation_type_1_DOM(arch,has_property,clna).
ignore_type_relation_type_1_DOM(arch,interacts_with,bird).
ignore_type_relation_type_1_DOM(arch,interacts_with,humn).
ignore_type_relation_type_1_DOM(arch,interacts_with,mamm).
ignore_type_relation_type_1_DOM(arch,interacts_with,plnt).
ignore_type_relation_type_1_DOM(bacs,affects,menp).
ignore_type_relation_type_1_DOM(bacs,complicates,menp).
ignore_type_relation_type_1_DOM(bacs,complicates,moft).
ignore_type_relation_type_1_DOM(bacs,disrupts,menp).
ignore_type_relation_type_1_DOM(bacs,has_location,plnt).
ignore_type_relation_type_1_DOM(bacs,interacts_with,chvf).
ignore_type_relation_type_1_DOM(bacs,isa,chvf).
ignore_type_relation_type_1_DOM(bacs,produced_by,menp).
ignore_type_relation_type_1_DOM(bacs,produced_by,moft).
ignore_type_relation_type_1_DOM(bact,affected_by,menp).
ignore_type_relation_type_1_DOM(bact,affected_by,moft).
ignore_type_relation_type_1_DOM(bact,has_location,gngm).
ignore_type_relation_type_1_DOM(bact,has_process,moft).
ignore_type_relation_type_1_DOM(bact,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,menp). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_property,clna).
ignore_type_relation_type_1_DOM(bact,interacts_with,bird).
ignore_type_relation_type_1_DOM(bact,interacts_with,humn).
ignore_type_relation_type_1_DOM(bact,interacts_with,mamm).
ignore_type_relation_type_1_DOM(bact,interacts_with,plnt).
ignore_type_relation_type_1_DOM(bdsu,produced_by,menp).
ignore_type_relation_type_1_DOM(bdsu,produced_by,moft).
ignore_type_relation_type_1_DOM(bdsy,result_of,menp).
ignore_type_relation_type_1_DOM(bhvr,affected_by,menp).
ignore_type_relation_type_1_DOM(bhvr,affected_by,socb).
ignore_type_relation_type_1_DOM(bhvr,affects,menp).
ignore_type_relation_type_1_DOM(bhvr,affects,socb).
ignore_type_relation_type_1_DOM(bhvr,exhibited_by,bird).
ignore_type_relation_type_1_DOM(bhvr,exhibited_by,mamm).
ignore_type_relation_type_1_DOM(bhvr,has_result,menp).
ignore_type_relation_type_1_DOM(bhvr,inverse_isa,socb).
ignore_type_relation_type_1_DOM(bhvr,manifestation_of,menp).
ignore_type_relation_type_1_DOM(bhvr,result_of,menp).
ignore_type_relation_type_1_DOM(biof,affected_by,chvf).
ignore_type_relation_type_1_DOM(biof,affected_by,lbpr).
ignore_type_relation_type_1_DOM(biof,affected_by,menp).
ignore_type_relation_type_1_DOM(biof,affected_by,moft).
ignore_type_relation_type_1_DOM(biof,affects,bird).
ignore_type_relation_type_1_DOM(biof,affects,mamm).
ignore_type_relation_type_1_DOM(biof,affects,menp).
ignore_type_relation_type_1_DOM(biof,affects,moft).
ignore_type_relation_type_1_DOM(biof,affects,plnt).
ignore_type_relation_type_1_DOM(biof,has_evaluation,fndg).
%ignore_type_relation_type_1_DOM(biof,has_location,blor).
%ignore_type_relation_type_1_DOM(biof,has_location,bpoc).
ignore_type_relation_type_1_DOM(biof,has_location,bsoj).
ignore_type_relation_type_1_DOM(biof,has_location,celc).
ignore_type_relation_type_1_DOM(biof,has_location,cell).
ignore_type_relation_type_1_DOM(biof,has_location,emst).
ignore_type_relation_type_1_DOM(biof,has_location,ffas).
ignore_type_relation_type_1_DOM(biof,has_location,tisu).
ignore_type_relation_type_1_DOM(biof,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(arch,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(biof,has_process,biof).
ignore_type_relation_type_1_DOM(biof,has_process,celf).
ignore_type_relation_type_1_DOM(biof,has_process,comd).
ignore_type_relation_type_1_DOM(biof,has_process,dsyn).
ignore_type_relation_type_1_DOM(amph,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(biof,has_process,emod).
ignore_type_relation_type_1_DOM(biof,has_process,genf).
ignore_type_relation_type_1_DOM(alga,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(amph,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,mobd).
ignore_type_relation_type_1_DOM(biof,has_process,mobd).
ignore_type_relation_type_1_DOM(alga,has_process,neop). % GR 06/2015
%ignore_type_relation_type_1_DOM(amph,has_process,neop). %commented out 07/2015 as this is in the SN
ignore_type_relation_type_1_DOM(arch,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(biof,has_process,neop).
ignore_type_relation_type_1_DOM(alga,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(amph,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(biof,has_process,npop).
ignore_type_relation_type_1_DOM(alga,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(amph,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(biof,has_process,orgf).
ignore_type_relation_type_1_DOM(alga,has_process,ortf). % GR 06/2015
ignore_type_relation_type_1_DOM(amph,has_process,ortf). % GR 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,ortf). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,ortf). % GR 06/2015
ignore_type_relation_type_1_DOM(biof,has_process,ortf).
ignore_type_relation_type_1_DOM(alga,has_process,patf). % 06/2015
%ignore_type_relation_type_1_DOM(amph,has_process,patf). % commented out 07/2015 as this is OK
ignore_type_relation_type_1_DOM(arch,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(biof,has_process,patf).
ignore_type_relation_type_1_DOM(alga,has_process,phsf). % 06/2015
ignore_type_relation_type_1_DOM(amph,has_process,phsf). % 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,phsf). % 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,phsf). % 06/2015
ignore_type_relation_type_1_DOM(biof,has_process,phsf).
ignore_type_relation_type_1_DOM(biof,has_result,clna).
ignore_type_relation_type_1_DOM(biof,has_result,menp).
ignore_type_relation_type_1_DOM(biof,has_result,moft).
ignore_type_relation_type_1_DOM(biof,inverse_isa,menp).
ignore_type_relation_type_1_DOM(biof,inverse_isa,moft).
ignore_type_relation_type_1_DOM(biof,process_of,alga). % Added 09/11/09
%ignore_type_relation_type_1_DOM(biof,process_of,amph). % Added 09/11/09 // unblocked 06/2015
%ignore_type_relation_type_1_DOM(biof,process_of,anim). % Added 09/11/09 // unblocked 06/2015
ignore_type_relation_type_1_DOM(biof,process_of,arch). % Added 09/11/09
ignore_type_relation_type_1_DOM(biof,process_of,bact). % Added 09/11/09
ignore_type_relation_type_1_DOM(biof,process_of,emod). % GR 09/16/09
%ignore_type_relation_type_1_DOM(biof,process_of,fish). % Added 09/11/09 // unblocked 06/2015
ignore_type_relation_type_1_DOM(biof,process_of,fngs). % Added 09/11/09
%ignore_type_relation_type_1_DOM(biof,process_of,humn). % Added 09/11/09 // unblocked 06/2015
ignore_type_relation_type_1_DOM(biof,process_of,invt). % Added 09/11/09
ignore_type_relation_type_1_DOM(biof,process_of,orgm). % Added 09/22/09
%ignore_type_relation_type_1_DOM(biof,process_of,rept). % Added 09/11/09 // unblocked 06/2015
ignore_type_relation_type_1_DOM(biof,process_of,rich). % Added 09/11/09
ignore_type_relation_type_1_DOM(biof,process_of,virs). % Added 09/11/09
%ignore_type_relation_type_1_DOM(biof,process_of,vtbt). % Added 09/11/09 // unblocked 06/2015
ignore_type_relation_type_1_DOM(biof,process_of,biof).
%ignore_type_relation_type_1_DOM(biof,process_of,bird).
ignore_type_relation_type_1_DOM(biof,process_of,celf).
ignore_type_relation_type_1_DOM(biof,process_of,comd).
ignore_type_relation_type_1_DOM(biof,process_of,dsyn).
ignore_type_relation_type_1_DOM(biof,process_of,genf).
%ignore_type_relation_type_1_DOM(biof,process_of,mamm). % Added by CA unblocked 06/2015 GR
ignore_type_relation_type_1_DOM(biof,process_of,menp).
ignore_type_relation_type_1_DOM(biof,process_of,mobd).
ignore_type_relation_type_1_DOM(biof,process_of,moft).
ignore_type_relation_type_1_DOM(biof,process_of,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(biof,process_of,npop).
ignore_type_relation_type_1_DOM(biof,process_of,orgf).
ignore_type_relation_type_1_DOM(biof,process_of,ortf).
ignore_type_relation_type_1_DOM(biof,process_of,phsf).
ignore_type_relation_type_1_DOM(biof,process_of,plnt).
ignore_type_relation_type_1_DOM(biof,result_of,menp).
ignore_type_relation_type_1_DOM(biof,result_of,moft).
ignore_type_relation_type_1_DOM(bird,affected_by,acab).
ignore_type_relation_type_1_DOM(bird,affected_by,anab).
ignore_type_relation_type_1_DOM(bird,affected_by,biof).
ignore_type_relation_type_1_DOM(bird,affected_by,celf).
ignore_type_relation_type_1_DOM(bird,affected_by,cgab).
ignore_type_relation_type_1_DOM(bird,affected_by,comd).
ignore_type_relation_type_1_DOM(bird,affected_by,dsyn).
ignore_type_relation_type_1_DOM(bird,affected_by,emod).
ignore_type_relation_type_1_DOM(bird,affected_by,genf).
ignore_type_relation_type_1_DOM(bird,affected_by,mobd).
ignore_type_relation_type_1_DOM(bird,affected_by,neop).
ignore_type_relation_type_1_DOM(bird,affected_by,orgf).
ignore_type_relation_type_1_DOM(bird,affected_by,ortf).
ignore_type_relation_type_1_DOM(bird,affected_by,patf).
ignore_type_relation_type_1_DOM(bird,affected_by,phsf).
ignore_type_relation_type_1_DOM(bird,exhibits,bhvr).
ignore_type_relation_type_1_DOM(bird,exhibits,inbe).
ignore_type_relation_type_1_DOM(bird,has_part,acab).
ignore_type_relation_type_1_DOM(bird,has_part,anab).
ignore_type_relation_type_1_DOM(bird,has_part,anst).
ignore_type_relation_type_1_DOM(bird,has_part,bpoc).
ignore_type_relation_type_1_DOM(bird,has_part,celc).
ignore_type_relation_type_1_DOM(bird,has_part,cell).
ignore_type_relation_type_1_DOM(bird,has_part,cgab).
ignore_type_relation_type_1_DOM(bird,has_part,emst).
ignore_type_relation_type_1_DOM(bird,has_part,ffas).
ignore_type_relation_type_1_DOM(bird,has_part,gngm).
ignore_type_relation_type_1_DOM(bird,has_part,tisu).
%ignore_type_relation_type_1_DOM(bird,has_process,biof).
%ignore_type_relation_type_1_DOM(bird,has_process,celf). % Commented out by GR 06/2015
%ignore_type_relation_type_1_DOM(bird,has_process,comd). % Commented out by GR 06/2015
%ignore_type_relation_type_1_DOM(bird,has_process,dsyn).
ignore_type_relation_type_1_DOM(bird,has_process,emod).
%ignore_type_relation_type_1_DOM(bird,has_process,genf). % Commented out by GR 06/2015
%ignore_type_relation_type_1_DOM(bird,has_process,mobd). % Commented out by GR 06/2015
ignore_type_relation_type_1_DOM(bird,has_process,neop). % Commented out by GR 06/2015
%ignore_type_relation_type_1_DOM(bird,has_process,ortf).
%ignore_type_relation_type_1_DOM(bird,has_process,phsf).
ignore_type_relation_type_1_DOM(bird,has_property,orga).
ignore_type_relation_type_1_DOM(bird,interacts_with,alga).
ignore_type_relation_type_1_DOM(bird,interacts_with,amph).
ignore_type_relation_type_1_DOM(bird,interacts_with,anim).
ignore_type_relation_type_1_DOM(bird,interacts_with,arch).
ignore_type_relation_type_1_DOM(bird,interacts_with,bact).
ignore_type_relation_type_1_DOM(bird,interacts_with,bird).
ignore_type_relation_type_1_DOM(bird,interacts_with,fish).
ignore_type_relation_type_1_DOM(bird,interacts_with,fngs).
ignore_type_relation_type_1_DOM(bird,interacts_with,invt).
ignore_type_relation_type_1_DOM(bird,interacts_with,orgm).
ignore_type_relation_type_1_DOM(bird,interacts_with,rept).
ignore_type_relation_type_1_DOM(bird,interacts_with,rich).
ignore_type_relation_type_1_DOM(bird,interacts_with,virs).
ignore_type_relation_type_1_DOM(bird,interacts_with,vtbt).
ignore_type_relation_type_1_DOM(bird,isa,anim).
ignore_type_relation_type_1_DOM(bird,isa,enty).
ignore_type_relation_type_1_DOM(bird,isa,orgm).
ignore_type_relation_type_1_DOM(bird,isa,phob).
ignore_type_relation_type_1_DOM(bird,isa,vtbt).
ignore_type_relation_type_1_DOM(bird,issue_in,bmod).
ignore_type_relation_type_1_DOM(bird,issue_in,ocdi).
ignore_type_relation_type_1_DOM(blor,location_of,anab).
%ignore_type_relation_type_1_DOM(blor,location_of,biof). % GR 08/2015
ignore_type_relation_type_1_DOM(blor,location_of,celf).
ignore_type_relation_type_1_DOM(blor,location_of,cgab).
ignore_type_relation_type_1_DOM(blor,location_of,comd).
%ignore_type_relation_type_1_DOM(blor,location_of,dsyn). % GR 08/2015
ignore_type_relation_type_1_DOM(blor,location_of,genf).
%ignore_type_relation_type_1_DOM(blor,location_of,inpo). % GR 08/2015
ignore_type_relation_type_1_DOM(blor,location_of,menp).
ignore_type_relation_type_1_DOM(blor,location_of,mobd).
ignore_type_relation_type_1_DOM(blor,location_of,moft).
ignore_type_relation_type_1_DOM(blor,location_of,orgf).
ignore_type_relation_type_1_DOM(blor,location_of,ortf).
ignore_type_relation_type_1_DOM(blor,location_of,patf).
ignore_type_relation_type_1_DOM(blor,location_of,phsf).
ignore_type_relation_type_1_DOM(blor,location_of,topp).
ignore_type_relation_type_1_DOM(blor,result_of,menp).
ignore_type_relation_type_1_DOM(bmod,has_issue,bird).
ignore_type_relation_type_1_DOM(bmod,has_issue,chvf).
ignore_type_relation_type_1_DOM(bmod,has_issue,clna).
ignore_type_relation_type_1_DOM(bmod,has_issue,fndg).
ignore_type_relation_type_1_DOM(bmod,has_issue,inpr).
ignore_type_relation_type_1_DOM(bmod,has_issue,mamm).
ignore_type_relation_type_1_DOM(bmod,has_issue,menp).
ignore_type_relation_type_1_DOM(bmod,has_issue,mnob).
ignore_type_relation_type_1_DOM(bmod,has_issue,moft).
ignore_type_relation_type_1_DOM(bmod,has_issue,plnt).
ignore_type_relation_type_1_DOM(bmod,has_issue,socb).
ignore_type_relation_type_1_DOM(bodm,affects,menp).
ignore_type_relation_type_1_DOM(bodm,affects,moft).
ignore_type_relation_type_1_DOM(bodm,interacts_with,chvf).
ignore_type_relation_type_1_DOM(bodm,isa,chvf).
%ignore_type_relation_type_1_DOM(bpoc,location_of,anab). % GR 08/2015
%ignore_type_relation_type_1_DOM(bpoc,location_of,biof).
ignore_type_relation_type_1_DOM(bpoc,location_of,blor). % GR 08/2015
ignore_type_relation_type_1_DOM(bpoc,location_of,celf).
ignore_type_relation_type_1_DOM(bpoc,location_of,cgab).
ignore_type_relation_type_1_DOM(bpoc,location_of,comd).
%ignore_type_relation_type_1_DOM(bpoc,location_of,dsyn).
ignore_type_relation_type_1_DOM(bpoc,location_of,genf).
%ignore_type_relation_type_1_DOM(bpoc,location_of,inpo). % GR 08/2015
ignore_type_relation_type_1_DOM(bpoc,location_of,menp).
ignore_type_relation_type_1_DOM(bpoc,location_of,mobd).
ignore_type_relation_type_1_DOM(bpoc,location_of,moft).
ignore_type_relation_type_1_DOM(bpoc,location_of,orgf).
ignore_type_relation_type_1_DOM(bpoc,location_of,ortf).
ignore_type_relation_type_1_DOM(bpoc,location_of,phsf).
ignore_type_relation_type_1_DOM(bpoc,location_of,topp).
ignore_type_relation_type_1_DOM(bpoc,part_of,bird).
ignore_type_relation_type_1_DOM(bpoc,part_of,invt).
ignore_type_relation_type_1_DOM(bpoc,part_of,neop). % GR 06/2016
ignore_type_relation_type_1_DOM(bpoc,part_of,plnt).
ignore_type_relation_type_1_DOM(bsoj,location_of,anab).
ignore_type_relation_type_1_DOM(bsoj,location_of,biof).
ignore_type_relation_type_1_DOM(bsoj,location_of,celf).
ignore_type_relation_type_1_DOM(bsoj,location_of,cgab).
ignore_type_relation_type_1_DOM(bsoj,location_of,comd).
%ignore_type_relation_type_1_DOM(bsoj,location_of,dsyn). % GR 08/2015
ignore_type_relation_type_1_DOM(bsoj,location_of,genf).
%ignore_type_relation_type_1_DOM(bsoj,location_of,inpo). % GR 08/2015
ignore_type_relation_type_1_DOM(bsoj,location_of,menp).
ignore_type_relation_type_1_DOM(bsoj,location_of,mobd).
ignore_type_relation_type_1_DOM(bsoj,location_of,moft).
ignore_type_relation_type_1_DOM(bsoj,location_of,orgf).
ignore_type_relation_type_1_DOM(bsoj,location_of,ortf).
ignore_type_relation_type_1_DOM(bsoj,location_of,patf).
ignore_type_relation_type_1_DOM(bsoj,location_of,phsf).
ignore_type_relation_type_1_DOM(bsoj,location_of,topp).
ignore_type_relation_type_1_DOM(bsoj,result_of,menp).
ignore_type_relation_type_1_DOM(carb,affects,menp).
ignore_type_relation_type_1_DOM(carb,affects,moft).
ignore_type_relation_type_1_DOM(carb,interacts_with,chvf).
ignore_type_relation_type_1_DOM(celc,affects,menp).
ignore_type_relation_type_1_DOM(celc,affects,moft).
ignore_type_relation_type_1_DOM(celc,location_of,anab).
ignore_type_relation_type_1_DOM(celc,location_of,biof).
ignore_type_relation_type_1_DOM(celc,location_of,celf).
ignore_type_relation_type_1_DOM(celc,location_of,cgab).
ignore_type_relation_type_1_DOM(celc,location_of,comd).
ignore_type_relation_type_1_DOM(celc,location_of,dsyn).
ignore_type_relation_type_1_DOM(celc,location_of,genf).
ignore_type_relation_type_1_DOM(celc,location_of,inpo).
ignore_type_relation_type_1_DOM(celc,location_of,menp).
ignore_type_relation_type_1_DOM(celc,location_of,mobd).
ignore_type_relation_type_1_DOM(celc,location_of,moft).
ignore_type_relation_type_1_DOM(celc,location_of,orgf).
ignore_type_relation_type_1_DOM(celc,location_of,ortf).
ignore_type_relation_type_1_DOM(celc,location_of,patf).
ignore_type_relation_type_1_DOM(celc,location_of,phsf).
ignore_type_relation_type_1_DOM(celc,location_of,topp).
ignore_type_relation_type_1_DOM(celc,part_of,bird).
ignore_type_relation_type_1_DOM(celc,part_of,invt).
ignore_type_relation_type_1_DOM(celc,part_of,plnt).
ignore_type_relation_type_1_DOM(celf,affected_by,chvf).
ignore_type_relation_type_1_DOM(celf,affected_by,lbpr).
ignore_type_relation_type_1_DOM(celf,affected_by,menp).
ignore_type_relation_type_1_DOM(celf,affected_by,moft).
ignore_type_relation_type_1_DOM(celf,affects,bird).
ignore_type_relation_type_1_DOM(celf,affects,clna).
ignore_type_relation_type_1_DOM(celf,affects,mamm).
ignore_type_relation_type_1_DOM(celf,affects,menp).
ignore_type_relation_type_1_DOM(celf,affects,moft).
ignore_type_relation_type_1_DOM(celf,affects,plnt).
ignore_type_relation_type_1_DOM(celf,coexists_with,menp).
ignore_type_relation_type_1_DOM(celf,coexists_with,moft).
ignore_type_relation_type_1_DOM(celf,follows,menp).
ignore_type_relation_type_1_DOM(celf,follows,moft).
ignore_type_relation_type_1_DOM(celf,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(celf,has_location,blor).
ignore_type_relation_type_1_DOM(celf,has_location,bpoc).
ignore_type_relation_type_1_DOM(celf,has_location,bsoj).
ignore_type_relation_type_1_DOM(celf,has_location,celc).
%ignore_type_relation_type_1_DOM(celf,has_location,cell). %  Commented out GR 08/2015
ignore_type_relation_type_1_DOM(celf,has_location,emst).
ignore_type_relation_type_1_DOM(celf,has_location,ffas).
ignore_type_relation_type_1_DOM(celf,has_location,tisu).
ignore_type_relation_type_1_DOM(celf,has_manifestation,clna).
ignore_type_relation_type_1_DOM(celf,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(celf,has_measurement,clna).
ignore_type_relation_type_1_DOM(celf,has_process,biof).
ignore_type_relation_type_1_DOM(celf,has_process,celf).
ignore_type_relation_type_1_DOM(celf,has_process,comd).
ignore_type_relation_type_1_DOM(celf,has_process,dsyn).
ignore_type_relation_type_1_DOM(celf,has_process,emod).
ignore_type_relation_type_1_DOM(celf,has_process,genf).
ignore_type_relation_type_1_DOM(celf,has_process,mobd).
ignore_type_relation_type_1_DOM(celf,has_process,neop).
ignore_type_relation_type_1_DOM(celf,has_process,npop).
ignore_type_relation_type_1_DOM(celf,has_process,orgf).
ignore_type_relation_type_1_DOM(celf,has_process,ortf).
ignore_type_relation_type_1_DOM(celf,has_process,patf).
ignore_type_relation_type_1_DOM(celf,has_process,phsf).
ignore_type_relation_type_1_DOM(celf,has_result,clna).
ignore_type_relation_type_1_DOM(celf,has_result,menp).
ignore_type_relation_type_1_DOM(celf,has_result,moft).
ignore_type_relation_type_1_DOM(celf,precedes,menp).
ignore_type_relation_type_1_DOM(celf,precedes,moft).
ignore_type_relation_type_1_DOM(celf,process_of,alga). % GR 09/14/09
%ignore_type_relation_type_1_DOM(celf,process_of,amph). % GR 09/14/09 but it needs to be allowed 07/2015
%ignore_type_relation_type_1_DOM(celf,process_of,anim). % GR 09/14/09 and unblocked in 06/2015
ignore_type_relation_type_1_DOM(celf,process_of,arch). % GR 09/14/09
ignore_type_relation_type_1_DOM(celf,process_of,bact). % GR 09/14/09
%ignore_type_relation_type_1_DOM(celf,process_of,emod). % 09/11/09
%ignore_type_relation_type_1_DOM(celf,process_of,fish). % GR 09/14/09 and unblocked in 06/2015
ignore_type_relation_type_1_DOM(celf,process_of,fngs). % GR 09/14/09 and unblocked in 06/2015
ignore_type_relation_type_1_DOM(celf,process_of,invt). % GR 09/14/09
%%ignore_type_relation_type_1_GEN(celf,process_of,mamm).
%ignore_type_relation_type_1_DOM(celf,process_of,neop). % 09/11/09
ignore_type_relation_type_1_DOM(celf,process_of,orgm). % 09/14/09
%ignore_type_relation_type_1_DOM(celf,process_of,rept). % GR 09/22/09
ignore_type_relation_type_1_DOM(celf,process_of,rich). % GR 09/14/09
ignore_type_relation_type_1_DOM(celf,process_of,virs). % 09/11/09
%ignore_type_relation_type_1_DOM(celf,process_of,vtbt). % GR 09/14/09
ignore_type_relation_type_1_DOM(celf,process_of,biof).
%ignore_type_relation_type_1_DOM(celf,process_of,bird). %Unblocked in 06/2015
ignore_type_relation_type_1_DOM(celf,process_of,celf).
ignore_type_relation_type_1_DOM(celf,process_of,comd).
ignore_type_relation_type_1_DOM(celf,process_of,dsyn).
ignore_type_relation_type_1_DOM(celf,process_of,genf).
ignore_type_relation_type_1_DOM(celf,process_of,menp).
ignore_type_relation_type_1_DOM(celf,process_of,mobd).
ignore_type_relation_type_1_DOM(celf,process_of,moft).
ignore_type_relation_type_1_DOM(celf,process_of,npop).
ignore_type_relation_type_1_DOM(celf,process_of,orgf).
ignore_type_relation_type_1_DOM(celf,process_of,ortf).
ignore_type_relation_type_1_DOM(celf,process_of,phsf).
ignore_type_relation_type_1_DOM(celf,process_of,plnt).
ignore_type_relation_type_1_DOM(celf,result_of,menp).
ignore_type_relation_type_1_DOM(celf,result_of,moft).
ignore_type_relation_type_1_DOM(cell,has_part,cell).
ignore_type_relation_type_1_DOM(cell,location_of,anab).
ignore_type_relation_type_1_DOM(cell,location_of,biof).
%ignore_type_relation_type_1_DOM(cell,location_of,celf). % Commented out by GR 08/2015
ignore_type_relation_type_1_DOM(cell,location_of,cgab).
ignore_type_relation_type_1_DOM(cell,location_of,comd).
ignore_type_relation_type_1_DOM(cell,location_of,dsyn).
%ignore_type_relation_type_1_DOM(cell,location_of,genf). % Commented out by GR 09/2015
ignore_type_relation_type_1_DOM(cell,location_of,inpo).
ignore_type_relation_type_1_DOM(cell,location_of,menp).
ignore_type_relation_type_1_DOM(cell,location_of,mobd).
ignore_type_relation_type_1_DOM(cell,location_of,moft).
ignore_type_relation_type_1_DOM(cell,location_of,orgf).
ignore_type_relation_type_1_DOM(cell,location_of,ortf).
ignore_type_relation_type_1_DOM(cell,location_of,patf).
ignore_type_relation_type_1_DOM(cell,location_of,phsf).
ignore_type_relation_type_1_DOM(cell,location_of,topp).
ignore_type_relation_type_1_DOM(cell,part_of,bird).
ignore_type_relation_type_1_DOM(cell,part_of,cell).
ignore_type_relation_type_1_DOM(cell,part_of,invt).
ignore_type_relation_type_1_DOM(cell,part_of,plnt).
ignore_type_relation_type_1_DOM(cgab,affects,bird).
ignore_type_relation_type_1_DOM(cgab,affects,mamm).
ignore_type_relation_type_1_DOM(cgab,affects,menp).
ignore_type_relation_type_1_DOM(cgab,affects,moft).
ignore_type_relation_type_1_DOM(cgab,affects,plnt).
%ignore_type_relation_type_1_DOM(cgab,caused_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(cgab,caused_by,mnob).
ignore_type_relation_type_1_DOM(cgab,has_location,gngm).
ignore_type_relation_type_1_DOM(cgab,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(cgab,has_result,menp).
ignore_type_relation_type_1_DOM(cgab,has_result,moft).
ignore_type_relation_type_1_DOM(cgab,location_of,comd).
ignore_type_relation_type_1_DOM(cgab,location_of,dsyn).
ignore_type_relation_type_1_DOM(cgab,location_of,mobd).
ignore_type_relation_type_1_DOM(cgab,location_of,patf).
ignore_type_relation_type_1_DOM(cgab,manifestation_of,menp).
ignore_type_relation_type_1_DOM(cgab,manifestation_of,moft).
ignore_type_relation_type_1_DOM(cgab,part_of,bird).
ignore_type_relation_type_1_DOM(cgab,part_of,invt).
ignore_type_relation_type_1_DOM(cgab,part_of,plnt).
ignore_type_relation_type_1_DOM(cgab,result_of,menp).
ignore_type_relation_type_1_DOM(cgab,result_of,moft).
ignore_type_relation_type_1_DOM(chem,affects,menp).
ignore_type_relation_type_1_DOM(chem,affects,moft).
ignore_type_relation_type_1_DOM(chem,interacts_with,chvf).
ignore_type_relation_type_1_DOM(chem,inverse_isa,chvf).
ignore_type_relation_type_1_DOM(chvf,affected_by,orch). % GR 08/2015
ignore_type_relation_type_1_DOM(chvf,affects,biof).
ignore_type_relation_type_1_DOM(chvf,affects,celf).
ignore_type_relation_type_1_DOM(chvf,affects,comd).
%ignore_type_relation_type_1_DOM(chvf,affects,dsyn). % Aurelie
ignore_type_relation_type_1_DOM(chvf,affects,emod).
ignore_type_relation_type_1_DOM(chvf,affects,genf).
%ignore_type_relation_type_1_DOM(chvf,affects,mobd). % Aurelie
%ignore_type_relation_type_1_DOM(chvf,affects,neop). % Aurelie
ignore_type_relation_type_1_DOM(chvf,affects,npop).
ignore_type_relation_type_1_DOM(chvf,affects,orgf).
ignore_type_relation_type_1_DOM(chvf,affects,ortf).
%ignore_type_relation_type_1_DOM(chvf,affects,patf). % Aurelie
ignore_type_relation_type_1_DOM(chvf,affects,phsf).
ignore_type_relation_type_1_DOM(chvf,analyzed_by,diap).
ignore_type_relation_type_1_DOM(chvf,analyzed_by,lbpr).
ignore_type_relation_type_1_DOM(chvf,assessed_for_effect_by,diap).
ignore_type_relation_type_1_DOM(chvf,assessed_for_effect_by,lbpr).
%ignore_type_relation_type_1_DOM(chvf,causes,acab). % Aurelie
%ignore_type_relation_type_1_DOM(chvf,causes,anab). % Aurelie
%ignore_type_relation_type_1_DOM(chvf,causes,cgab). % Aurelie
ignore_type_relation_type_1_DOM(chvf,causes,comd).
%ignore_type_relation_type_1_DOM(chvf,causes,dsyn). % Aurelie
ignore_type_relation_type_1_DOM(chvf,causes,emod).
%ignore_type_relation_type_1_DOM(chvf,causes,inpo). % Aurelie
%ignore_type_relation_type_1_DOM(chvf,causes,mobd). % Aurelie
%ignore_type_relation_type_1_DOM(chvf,causes,neop). % Aurelie
%ignore_type_relation_type_1_DOM(chvf,causes,patf). % Aurelie
ignore_type_relation_type_1_DOM(chvf,has_measurement,lbtr).
ignore_type_relation_type_1_DOM(chvf,ingredient_of,clnd).
ignore_type_relation_type_1_DOM(chvf,interacts_with,aapp).
ignore_type_relation_type_1_DOM(chvf,interacts_with,antb).
ignore_type_relation_type_1_DOM(chvf,interacts_with,bacs).
ignore_type_relation_type_1_DOM(chvf,interacts_with,bodm).
ignore_type_relation_type_1_DOM(chvf,interacts_with,carb).
ignore_type_relation_type_1_DOM(chvf,interacts_with,chem).
ignore_type_relation_type_1_DOM(chvf,interacts_with,chvf).
ignore_type_relation_type_1_DOM(chvf,interacts_with,chvs).
ignore_type_relation_type_1_DOM(chvf,interacts_with,eico).
ignore_type_relation_type_1_DOM(chvf,interacts_with,elii).
%ignore_type_relation_type_1_DOM(chvf,interacts_with,enzy). % GR allows it on 06/2015 as per GS sentence 16372914.ab.5
ignore_type_relation_type_1_DOM(chvf,interacts_with,hops).
ignore_type_relation_type_1_DOM(chvf,interacts_with,horm).
ignore_type_relation_type_1_DOM(chvf,interacts_with,imft).
ignore_type_relation_type_1_DOM(chvf,interacts_with,inch).
ignore_type_relation_type_1_DOM(chvf,interacts_with,irda).
ignore_type_relation_type_1_DOM(chvf,interacts_with,lipd).
ignore_type_relation_type_1_DOM(chvf,interacts_with,nnon).
ignore_type_relation_type_1_DOM(chvf,interacts_with,nsba).
ignore_type_relation_type_1_DOM(chvf,interacts_with,opco).
ignore_type_relation_type_1_DOM(chvf,interacts_with,orch).
ignore_type_relation_type_1_DOM(chvf,interacts_with,phsu).
ignore_type_relation_type_1_DOM(chvf,interacts_with,rcpt).
ignore_type_relation_type_1_DOM(chvf,interacts_with,strd).
ignore_type_relation_type_1_DOM(chvf,interacts_with,vita).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,antb).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,bacs).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,bodm).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,enzy).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,hops).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,horm).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,imft).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,irda).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,nsba).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,phsu).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,rcpt).
ignore_type_relation_type_1_DOM(chvf,inverse_isa,vita).
ignore_type_relation_type_1_DOM(chvf,isa,chem).
ignore_type_relation_type_1_DOM(chvf,isa,enty).
ignore_type_relation_type_1_DOM(chvf,isa,phob).
ignore_type_relation_type_1_DOM(chvf,isa,sbst).
ignore_type_relation_type_1_DOM(chvf,issue_in,bmod).
ignore_type_relation_type_1_DOM(chvf,issue_in,ocdi).
ignore_type_relation_type_1_DOM(chvf,measured_by,diap).
ignore_type_relation_type_1_DOM(chvf,measured_by,lbpr).
ignore_type_relation_type_1_DOM(chvf,measured_by,mbrt).
ignore_type_relation_type_1_DOM(chvf,measured_by,resa).
ignore_type_relation_type_1_DOM(chvf,produces,inpr). % GR 06/2015
ignore_type_relation_type_1_DOM(chvf,produces,nsba). % GR 06/2015
ignore_type_relation_type_1_DOM(chvf,produces,rcpt). % GR 06/2015
ignore_type_relation_type_1_DOM(chvf,produces,vita). % GR 08/2015
ignore_type_relation_type_1_DOM(chvs,affects,menp).
ignore_type_relation_type_1_DOM(chvs,affects,moft).
ignore_type_relation_type_1_DOM(chvs,interacts_with,chvf).
ignore_type_relation_type_1_DOM(clas,isa,inpr).
ignore_type_relation_type_1_DOM(clas,used_by,podg). % GR 06/2015
ignore_type_relation_type_1_DOM(clna,affected_by,celf).
ignore_type_relation_type_1_DOM(clna,affected_by,genf).
ignore_type_relation_type_1_DOM(clna,affected_by,orgf).
ignore_type_relation_type_1_DOM(clna,affected_by,ortf).
ignore_type_relation_type_1_DOM(clna,affected_by,phsf).
ignore_type_relation_type_1_DOM(clna,degree_of,clna).
ignore_type_relation_type_1_DOM(clna,degree_of,orga).
ignore_type_relation_type_1_DOM(clna,has_degree,clna).
ignore_type_relation_type_1_DOM(clna,has_degree,orga).
ignore_type_relation_type_1_DOM(clna,has_evaluation,lbtr).
ignore_type_relation_type_1_DOM(clna,has_evaluation,sosy).
ignore_type_relation_type_1_DOM(clna,has_manifestation,orga). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,interacts_with,orgm). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,interacts_with,rept). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,interacts_with,rich). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,interacts_with,virs). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,interacts_with,vtbt). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,isa,cnce).
ignore_type_relation_type_1_DOM(clna,isa,enty).
ignore_type_relation_type_1_DOM(clna,isa,orga).
ignore_type_relation_type_1_DOM(clna,issue_in,bmod).
ignore_type_relation_type_1_DOM(clna,issue_in,ocdi).
ignore_type_relation_type_1_DOM(clna,manifestation_of,celf).
ignore_type_relation_type_1_DOM(clna,manifestation_of,genf).
ignore_type_relation_type_1_DOM(clna,manifestation_of,orgf).
ignore_type_relation_type_1_DOM(clna,manifestation_of,ortf).
ignore_type_relation_type_1_DOM(clna,manifestation_of,phsf).
ignore_type_relation_type_1_DOM(clna,measured_by,diap).
ignore_type_relation_type_1_DOM(clna,measured_by,lbpr).
ignore_type_relation_type_1_DOM(clna,measured_by,mbrt).
ignore_type_relation_type_1_DOM(clna,measured_by,resa).
ignore_type_relation_type_1_DOM(clna,measurement_of,celf).
ignore_type_relation_type_1_DOM(clna,measurement_of,genf).
ignore_type_relation_type_1_DOM(clna,measurement_of,orgf).
ignore_type_relation_type_1_DOM(clna,measurement_of,ortf).
ignore_type_relation_type_1_DOM(clna,measurement_of,phsf).
ignore_type_relation_type_1_DOM(clna,property_of,alga).
ignore_type_relation_type_1_DOM(clna,property_of,amph).
ignore_type_relation_type_1_DOM(clna,property_of,anim).
ignore_type_relation_type_1_DOM(clna,property_of,arch).
ignore_type_relation_type_1_DOM(clna,property_of,bact).
ignore_type_relation_type_1_DOM(clna,property_of,fish).
ignore_type_relation_type_1_DOM(clna,property_of,fngs).
ignore_type_relation_type_1_DOM(clna,property_of,humn).
ignore_type_relation_type_1_DOM(clna,property_of,invt).
ignore_type_relation_type_1_DOM(clna,property_of,orgm).
ignore_type_relation_type_1_DOM(clna,property_of,rept).
ignore_type_relation_type_1_DOM(clna,property_of,rich).
ignore_type_relation_type_1_DOM(clna,property_of,virs).
ignore_type_relation_type_1_DOM(clna,property_of,vtbt).
ignore_type_relation_type_1_DOM(clna,result_of,biof).
ignore_type_relation_type_1_DOM(clna,result_of,celf).
ignore_type_relation_type_1_DOM(clna,result_of,comd).
ignore_type_relation_type_1_DOM(clna,result_of,dsyn).
ignore_type_relation_type_1_DOM(clna,result_of,eehu).
ignore_type_relation_type_1_DOM(clna,result_of,emod).
ignore_type_relation_type_1_DOM(clna,result_of,genf).
ignore_type_relation_type_1_DOM(clna,result_of,hcpp).
ignore_type_relation_type_1_DOM(clna,result_of,inpo).
ignore_type_relation_type_1_DOM(clna,result_of,mobd).
ignore_type_relation_type_1_DOM(clna,result_of,neop).
ignore_type_relation_type_1_DOM(clna,result_of,npop).
ignore_type_relation_type_1_DOM(clna,result_of,orgf).
ignore_type_relation_type_1_DOM(clna,result_of,ortf).
ignore_type_relation_type_1_DOM(clna,result_of,patf).
ignore_type_relation_type_1_DOM(clna,result_of,phpr).
ignore_type_relation_type_1_DOM(clna,result_of,phsf).
ignore_type_relation_type_1_DOM(clnd,has_ingredient,chvf).
ignore_type_relation_type_1_DOM(clnd,isa,mnob).
ignore_type_relation_type_1_DOM(cnce,inverse_isa,clna).
ignore_type_relation_type_1_DOM(cnce,inverse_isa,fndg).
ignore_type_relation_type_1_DOM(cnce,inverse_isa,inpr).
ignore_type_relation_type_1_DOM(comd,affected_by,chvf).
ignore_type_relation_type_1_DOM(comd,affected_by,lbpr).
ignore_type_relation_type_1_DOM(comd,affected_by,menp).
ignore_type_relation_type_1_DOM(comd,affected_by,moft).
ignore_type_relation_type_1_DOM(comd,affects,bird).
ignore_type_relation_type_1_DOM(comd,affects,mamm).
ignore_type_relation_type_1_DOM(comd,affects,menp).
ignore_type_relation_type_1_DOM(comd,affects,moft).
ignore_type_relation_type_1_DOM(comd,affects,plnt).
ignore_type_relation_type_1_DOM(comd,caused_by,chvf).
ignore_type_relation_type_1_DOM(comd,caused_by,mnob).
ignore_type_relation_type_1_DOM(comd,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(comd,has_location,acab).
ignore_type_relation_type_1_DOM(comd,has_location,anab).
ignore_type_relation_type_1_DOM(comd,has_location,blor).
ignore_type_relation_type_1_DOM(comd,has_location,bpoc).
ignore_type_relation_type_1_DOM(comd,has_location,bsoj).
ignore_type_relation_type_1_DOM(comd,has_location,celc).
ignore_type_relation_type_1_DOM(comd,has_location,cell).
ignore_type_relation_type_1_DOM(comd,has_location,cgab).
ignore_type_relation_type_1_DOM(comd,has_location,emst).
ignore_type_relation_type_1_DOM(comd,has_location,ffas).
ignore_type_relation_type_1_DOM(clna,process_of,tisu). % GR 06/2015
ignore_type_relation_type_1_DOM(comd,has_location,tisu).
ignore_type_relation_type_1_DOM(comd,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(comd,has_process,biof).
ignore_type_relation_type_1_DOM(comd,has_process,celf).
ignore_type_relation_type_1_DOM(comd,has_process,comd).
ignore_type_relation_type_1_DOM(comd,has_process,dsyn).
ignore_type_relation_type_1_DOM(comd,has_process,emod).
ignore_type_relation_type_1_DOM(comd,has_process,genf).
ignore_type_relation_type_1_DOM(comd,has_process,mobd).
ignore_type_relation_type_1_DOM(comd,has_process,neop).
ignore_type_relation_type_1_DOM(comd,has_process,npop).
ignore_type_relation_type_1_DOM(comd,has_process,orgf).
ignore_type_relation_type_1_DOM(comd,has_process,ortf).
ignore_type_relation_type_1_DOM(comd,has_process,patf).
ignore_type_relation_type_1_DOM(comd,has_process,phsf).
ignore_type_relation_type_1_DOM(comd,has_result,clna).
ignore_type_relation_type_1_DOM(comd,has_result,menp).
ignore_type_relation_type_1_DOM(comd,has_result,moft).
ignore_type_relation_type_1_DOM(comd,manifestation_of,menp).
ignore_type_relation_type_1_DOM(comd,manifestation_of,moft).
ignore_type_relation_type_1_DOM(comd,process_of,alga). % GR 09/14/09
%ignore_type_relation_type_1_DOM(comd,process_of,amph). % Unblocked 06/2015
%ignore_type_relation_type_1_DOM(comd,process_of,anim). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(comd,process_of,arch). % GR 09/14/09
ignore_type_relation_type_1_DOM(comd,process_of,bact). % GR 09/14/09
%ignore_type_relation_type_1_DOM(comd,process_of,emod). % GR 09/22/09
%ignore_type_relation_type_1_DOM(comd,process_of,fish). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(comd,process_of,fngs). % GR 09/14/09
%ignore_type_relation_type_1_DOM(comd,process_of,humn). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(comd,process_of,invt). % GR 09/14/09
ignore_type_relation_type_1_DOM(comd,process_of,neop). % GR 09/14/09
ignore_type_relation_type_1_DOM(comd,process_of,orgm). % GR 09/14/09
%ignore_type_relation_type_1_DOM(comd,process_of,rept). %Unblocked 06/2015
ignore_type_relation_type_1_DOM(comd,process_of,rich). % GR 09/14/09
%ignore_type_relation_type_1_DOM(comd,process_of,vtbt). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(comd,process_of,virs). % GR 09/14/09
ignore_type_relation_type_1_DOM(comd,process_of,biof).
%ignore_type_relation_type_1_DOM(comd,process_of,bird). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(comd,process_of,celf).
ignore_type_relation_type_1_DOM(comd,process_of,comd).
ignore_type_relation_type_1_DOM(comd,process_of,dsyn).
ignore_type_relation_type_1_DOM(comd,process_of,genf).
%ignore_type_relation_type_1_DOM(comd,process_of,mamm). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(comd,process_of,menp).
ignore_type_relation_type_1_DOM(comd,process_of,mobd).
ignore_type_relation_type_1_DOM(comd,process_of,moft).
ignore_type_relation_type_1_DOM(comd,process_of,npop).
ignore_type_relation_type_1_DOM(comd,process_of,orgf).
ignore_type_relation_type_1_DOM(comd,process_of,ortf).
ignore_type_relation_type_1_DOM(comd,process_of,phsf).
ignore_type_relation_type_1_DOM(comd,process_of,plnt).
ignore_type_relation_type_1_DOM(comd,result_of,menp).
ignore_type_relation_type_1_DOM(comd,result_of,moft).
ignore_type_relation_type_1_DOM(comd,result_of,socb).
ignore_type_relation_type_1_DOM(crbs,result_of,menp).
ignore_type_relation_type_1_DOM(diap,affects,menp).
ignore_type_relation_type_1_DOM(diap,affects,moft).
ignore_type_relation_type_1_DOM(diap,analyzes,chvf).
ignore_type_relation_type_1_DOM(diap,assesses_effect_of,chvf).
ignore_type_relation_type_1_DOM(diap,has_conceptual_part,inpr).
ignore_type_relation_type_1_DOM(diap,measures,chvf).
ignore_type_relation_type_1_DOM(diap,measures,clna).
ignore_type_relation_type_1_DOM(diap,measures,menp).
ignore_type_relation_type_1_DOM(diap,measures,moft).
ignore_type_relation_type_1_DOM(diap,uses,mnob).
ignore_type_relation_type_1_DOM(drdd,isa,mnob).
%ignore_type_relation_type_1_DOM(dsyn,affected_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(dsyn,affected_by,lbpr).
ignore_type_relation_type_1_DOM(dsyn,affected_by,menp).
ignore_type_relation_type_1_DOM(dsyn,affected_by,moft).
ignore_type_relation_type_1_DOM(dsyn,affects,bird).
ignore_type_relation_type_1_DOM(dsyn,affects,mamm).
ignore_type_relation_type_1_DOM(dsyn,affects,menp).
ignore_type_relation_type_1_DOM(dsyn,affects,moft).
ignore_type_relation_type_1_DOM(dsyn,affects,plnt).
%ignore_type_relation_type_1_DOM(dsyn,caused_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(dsyn,caused_by,mnob).
ignore_type_relation_type_1_DOM(dsyn,diagnosed_by,aapp). % GR 07/2015
ignore_type_relation_type_1_DOM(dsyn,diagnosed_by,antb). % GR 07/2015
ignore_type_relation_type_1_DOM(dsyn,diagnosed_by,patf). % GR 07/2015
ignore_type_relation_type_1_DOM(dsyn,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(dsyn,has_location,gngm).
ignore_type_relation_type_1_DOM(dsyn,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,patf). % GR 07/2015
ignore_type_relation_type_1_DOM(dsyn,has_process,biof).
ignore_type_relation_type_1_DOM(dsyn,has_process,celf).
ignore_type_relation_type_1_DOM(dsyn,has_process,comd).
ignore_type_relation_type_1_DOM(dsyn,has_process,dsyn).
ignore_type_relation_type_1_DOM(dsyn,has_process,emod).
ignore_type_relation_type_1_DOM(dsyn,has_process,genf).
ignore_type_relation_type_1_DOM(dsyn,has_process,mobd).
ignore_type_relation_type_1_DOM(dsyn,has_process,neop).
ignore_type_relation_type_1_DOM(dsyn,has_process,npop).
ignore_type_relation_type_1_DOM(dsyn,has_process,orgf).
ignore_type_relation_type_1_DOM(dsyn,has_process,ortf).
ignore_type_relation_type_1_DOM(dsyn,has_process,patf).
ignore_type_relation_type_1_DOM(dsyn,has_process,phsf).
ignore_type_relation_type_1_DOM(dsyn,has_result,clna).
ignore_type_relation_type_1_DOM(dsyn,has_result,menp).
ignore_type_relation_type_1_DOM(dsyn,has_result,moft).
ignore_type_relation_type_1_DOM(dsyn,manifestation_of,menp).
ignore_type_relation_type_1_DOM(dsyn,manifestation_of,moft).
ignore_type_relation_type_1_DOM(dsyn,process_of,alga). % GR
%ignore_type_relation_type_1_DOM(dsyn,process_of,anim).
ignore_type_relation_type_1_DOM(dsyn,process_of,arch). % GR
ignore_type_relation_type_1_DOM(dsyn,process_of,bact). % GR
ignore_type_relation_type_1_DOM(dsyn,process_of,biof).
%ignore_type_relation_type_1_DOM(dsyn,process_of,bird).
ignore_type_relation_type_1_DOM(dsyn,process_of,comd).
ignore_type_relation_type_1_DOM(dsyn,process_of,dsyn).
%ignore_type_relation_type_1_DOM(dsyn,process_of,emod).
ignore_type_relation_type_1_DOM(dsyn,process_of,fngs). % GR
ignore_type_relation_type_1_DOM(dsyn,process_of,invt). % GR
%ignore_type_relation_type_1_DOM(dsyn,process_of,mamm). %Unblocked 06/2015
ignore_type_relation_type_1_DOM(dsyn,process_of,menp).
ignore_type_relation_type_1_DOM(dsyn,process_of,mobd).
ignore_type_relation_type_1_DOM(dsyn,process_of,moft).
ignore_type_relation_type_1_DOM(dsyn,process_of,neop). % GR August 31 `09
ignore_type_relation_type_1_DOM(dsyn,process_of,npop).
ignore_type_relation_type_1_DOM(dsyn,process_of,orgf).
ignore_type_relation_type_1_DOM(dsyn,process_of,patf).
%ignore_type_relation_type_1_DOM(dsyn,process_of,plnt). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(dsyn,process_of,orgm). % GR
ignore_type_relation_type_1_DOM(dsyn,process_of,rich). % GR
ignore_type_relation_type_1_DOM(dsyn,process_of,virs). % GR
ignore_type_relation_type_1_DOM(dsyn,result_of,menp).
ignore_type_relation_type_1_DOM(dsyn,result_of,moft).
ignore_type_relation_type_1_DOM(dsyn,result_of,socb).
ignore_type_relation_type_1_DOM(eehu,has_result,clna).
ignore_type_relation_type_1_DOM(eehu,has_result,menp).
ignore_type_relation_type_1_DOM(eehu,has_result,moft).
ignore_type_relation_type_1_DOM(eehu,result_of,menp).
ignore_type_relation_type_1_DOM(eehu,result_of,moft).
ignore_type_relation_type_1_DOM(eico,affects,menp).
ignore_type_relation_type_1_DOM(eico,affects,moft).
ignore_type_relation_type_1_DOM(eico,interacts_with,chvf).
ignore_type_relation_type_1_DOM(elii,affects,menp).
ignore_type_relation_type_1_DOM(elii,affects,moft).
ignore_type_relation_type_1_DOM(elii,interacts_with,chvf).
ignore_type_relation_type_1_DOM(emod,affected_by,chvf).
ignore_type_relation_type_1_DOM(emod,affected_by,lbpr).
ignore_type_relation_type_1_DOM(emod,affected_by,menp).
ignore_type_relation_type_1_DOM(emod,affected_by,moft).
ignore_type_relation_type_1_DOM(emod,affects,bird).
ignore_type_relation_type_1_DOM(emod,affects,mamm).
ignore_type_relation_type_1_DOM(emod,affects,menp).
ignore_type_relation_type_1_DOM(emod,affects,moft).
ignore_type_relation_type_1_DOM(emod,affects,plnt).
ignore_type_relation_type_1_DOM(emod,caused_by,chvf).
ignore_type_relation_type_1_DOM(emod,caused_by,mnob).
ignore_type_relation_type_1_DOM(emod,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(emod,has_location,gngm).
ignore_type_relation_type_1_DOM(emod,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(emod,has_process,menp).
ignore_type_relation_type_1_DOM(emod,has_process,moft).
ignore_type_relation_type_1_DOM(emod,has_result,clna).
ignore_type_relation_type_1_DOM(emod,has_result,menp).
ignore_type_relation_type_1_DOM(emod,has_result,moft).
ignore_type_relation_type_1_DOM(emod,manifestation_of,menp).
ignore_type_relation_type_1_DOM(emod,manifestation_of,moft).
ignore_type_relation_type_1_DOM(emod,process_of,alga). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,amph). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,anim). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,arch). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,bact). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,emod). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,fish). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,fngs). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,humn). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,invt). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,neop). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,orgm). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,rept). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,rich). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,vtbt). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,virs). % 09/11/09
ignore_type_relation_type_1_DOM(emod,process_of,biof).
ignore_type_relation_type_1_DOM(emod,process_of,bird).
ignore_type_relation_type_1_DOM(emod,process_of,celf).
ignore_type_relation_type_1_DOM(emod,process_of,comd).
ignore_type_relation_type_1_DOM(emod,process_of,dsyn).
ignore_type_relation_type_1_DOM(emod,process_of,genf).
ignore_type_relation_type_1_DOM(emod,process_of,mamm).
ignore_type_relation_type_1_DOM(emod,process_of,menp).
ignore_type_relation_type_1_DOM(emod,process_of,mobd).
ignore_type_relation_type_1_DOM(emod,process_of,moft).
ignore_type_relation_type_1_DOM(emod,process_of,npop).
ignore_type_relation_type_1_DOM(emod,process_of,orgf).
ignore_type_relation_type_1_DOM(emod,process_of,ortf).
ignore_type_relation_type_1_DOM(emod,process_of,patf).
ignore_type_relation_type_1_DOM(emod,process_of,phsf).
ignore_type_relation_type_1_DOM(emod,process_of,plnt).
ignore_type_relation_type_1_DOM(emod,result_of,menp).
ignore_type_relation_type_1_DOM(emod,result_of,moft).
ignore_type_relation_type_1_DOM(emod,result_of,socb).
ignore_type_relation_type_1_DOM(emst,location_of,biof).
ignore_type_relation_type_1_DOM(emst,location_of,celf).
ignore_type_relation_type_1_DOM(emst,location_of,comd).
ignore_type_relation_type_1_DOM(emst,location_of,dsyn).
ignore_type_relation_type_1_DOM(emst,location_of,genf).
ignore_type_relation_type_1_DOM(emst,location_of,menp).
ignore_type_relation_type_1_DOM(emst,location_of,mobd).
ignore_type_relation_type_1_DOM(emst,location_of,moft).
ignore_type_relation_type_1_DOM(emst,location_of,orgf).
ignore_type_relation_type_1_DOM(emst,location_of,ortf).
ignore_type_relation_type_1_DOM(emst,location_of,patf).
ignore_type_relation_type_1_DOM(emst,location_of,phsf).
ignore_type_relation_type_1_DOM(emst,part_of,bird).
ignore_type_relation_type_1_DOM(emst,part_of,invt).
ignore_type_relation_type_1_DOM(emst,part_of,plnt).
ignore_type_relation_type_1_DOM(enty,inverse_isa,bird).
ignore_type_relation_type_1_DOM(enty,inverse_isa,chvf).
ignore_type_relation_type_1_DOM(enty,inverse_isa,clna).
ignore_type_relation_type_1_DOM(enty,inverse_isa,fndg).
ignore_type_relation_type_1_DOM(enty,inverse_isa,inpr).
ignore_type_relation_type_1_DOM(enty,inverse_isa,mamm).
ignore_type_relation_type_1_DOM(enty,inverse_isa,mnob).
ignore_type_relation_type_1_DOM(enty,inverse_isa,plnt).
ignore_type_relation_type_1_DOM(enzy,affects,menp).
%ignore_type_relation_type_1_DOM(enzy,affects,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(enzy,complicates,menp).
ignore_type_relation_type_1_DOM(enzy,complicates,moft).
ignore_type_relation_type_1_DOM(enzy,disrupts,gngm).
ignore_type_relation_type_1_DOM(enzy,disrupts,menp).
%ignore_type_relation_type_1_DOM(enzy,disrupts,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(enzy,has_location,plnt).
%ignore_type_relation_type_1_DOM(enzy,interacts_with,chvf). % GR allows this on 06/2015 as per GS
ignore_type_relation_type_1_DOM(enzy,isa,chvf).
ignore_type_relation_type_1_DOM(enzy,produced_by,menp).
ignore_type_relation_type_1_DOM(enzy,produced_by,moft).
ignore_type_relation_type_1_DOM(evnt,inverse_isa,menp).
ignore_type_relation_type_1_DOM(evnt,inverse_isa,moft).
ignore_type_relation_type_1_DOM(evnt,inverse_isa,socb).
ignore_type_relation_type_1_DOM(famg,exhibits,socb).
ignore_type_relation_type_1_DOM(famg,performs,socb).
ignore_type_relation_type_1_DOM(famg,produced_by,menp).
ignore_type_relation_type_1_DOM(famg,produces,inpr).
ignore_type_relation_type_1_DOM(famg,produces,mnob).
ignore_type_relation_type_1_DOM(famg,uses,inpr).
ignore_type_relation_type_1_DOM(famg,uses,mnob).
ignore_type_relation_type_1_DOM(ffas,location_of,anab).
ignore_type_relation_type_1_DOM(ffas,location_of,biof).
ignore_type_relation_type_1_DOM(ffas,location_of,celf).
ignore_type_relation_type_1_DOM(ffas,location_of,cgab).
ignore_type_relation_type_1_DOM(ffas,location_of,comd).
ignore_type_relation_type_1_DOM(ffas,location_of,dsyn).
ignore_type_relation_type_1_DOM(ffas,location_of,genf).
ignore_type_relation_type_1_DOM(ffas,location_of,inpo).
ignore_type_relation_type_1_DOM(ffas,location_of,menp).
ignore_type_relation_type_1_DOM(ffas,location_of,mobd).
ignore_type_relation_type_1_DOM(ffas,location_of,moft).
ignore_type_relation_type_1_DOM(ffas,location_of,orgf).
ignore_type_relation_type_1_DOM(ffas,location_of,ortf).
ignore_type_relation_type_1_DOM(ffas,location_of,patf).
ignore_type_relation_type_1_DOM(ffas,location_of,phsf).
ignore_type_relation_type_1_DOM(ffas,part_of,bird).
ignore_type_relation_type_1_DOM(ffas,part_of,invt).
ignore_type_relation_type_1_DOM(ffas,part_of,plnt).
ignore_type_relation_type_1_DOM(fish,affected_by,menp).
ignore_type_relation_type_1_DOM(fish,affected_by,moft).
ignore_type_relation_type_1_DOM(fish,exhibits,socb).
ignore_type_relation_type_1_DOM(fish,has_process,menp).
ignore_type_relation_type_1_DOM(fish,has_process,moft).
ignore_type_relation_type_1_DOM(fish,has_property,clna).
ignore_type_relation_type_1_DOM(fish,interacts_with,bird).
ignore_type_relation_type_1_DOM(fish,interacts_with,humn).
ignore_type_relation_type_1_DOM(fish,interacts_with,mamm).
ignore_type_relation_type_1_DOM(fish,interacts_with,plnt).
ignore_type_relation_type_1_DOM(fndg,coexists_with,fndg).
ignore_type_relation_type_1_DOM(fndg,coexists_with,lbtr).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,biof).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,celf).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,comd).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,dsyn).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,emod).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,genf).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,mobd).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,neop).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,orga).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,orgf).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,ortf).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,patf).
ignore_type_relation_type_1_DOM(fndg,evaluation_of,phsf).
ignore_type_relation_type_1_DOM(fndg,inverse_isa,lbtr).
ignore_type_relation_type_1_DOM(fndg,inverse_isa,sosy).
ignore_type_relation_type_1_DOM(fndg,isa,cnce).
ignore_type_relation_type_1_DOM(fndg,isa,enty).
ignore_type_relation_type_1_DOM(fndg,issue_in,bmod).
ignore_type_relation_type_1_DOM(fndg,issue_in,ocdi).
ignore_type_relation_type_1_DOM(fndg,location_of,orgf). % 07/2015
ignore_type_relation_type_1_DOM(fndg,location_of,ortf). % 07/2015
ignore_type_relation_type_1_DOM(fndg,location_of,patf). % 07/2015
ignore_type_relation_type_1_DOM(fndg,location_of,phsf). % 07/2015
ignore_type_relation_type_1_DOM(fndg,manifestation_of,acab).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,anab).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,biof).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,celf).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,cgab).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,comd).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,dsyn).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,emod).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,genf).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,inpo).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,mobd).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,neop).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,orgf).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,ortf).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,patf).
ignore_type_relation_type_1_DOM(fndg,manifestation_of,phsf).
ignore_type_relation_type_1_DOM(fndg,process_of,orgm). % GR 08/2015
ignore_type_relation_type_1_DOM(fngs,affected_by,menp).
ignore_type_relation_type_1_DOM(fngs,affected_by,moft).
ignore_type_relation_type_1_DOM(fngs,has_location,gngm).
ignore_type_relation_type_1_DOM(fngs,has_process,menp). % GR 07/2015
ignore_type_relation_type_1_DOM(fngs,has_process,moft).
ignore_type_relation_type_1_DOM(fngs,has_property,clna).
ignore_type_relation_type_1_DOM(fngs,interacts_with,bird).
ignore_type_relation_type_1_DOM(fngs,interacts_with,humn).
ignore_type_relation_type_1_DOM(fngs,interacts_with,mamm).
ignore_type_relation_type_1_DOM(fngs,interacts_with,plnt).
ignore_type_relation_type_1_DOM(food,affects,menp).
ignore_type_relation_type_1_DOM(food,affects,moft).
ignore_type_relation_type_1_DOM(ftcn,result_of,menp).
ignore_type_relation_type_1_DOM(genf,affected_by,chvf).
ignore_type_relation_type_1_DOM(genf,affected_by,lbpr).
ignore_type_relation_type_1_DOM(genf,affected_by,menp).
ignore_type_relation_type_1_DOM(genf,affected_by,moft).
ignore_type_relation_type_1_DOM(genf,affects,bird).
ignore_type_relation_type_1_DOM(genf,affects,clna).
ignore_type_relation_type_1_DOM(genf,affects,mamm).
ignore_type_relation_type_1_DOM(genf,affects,menp).
ignore_type_relation_type_1_DOM(genf,affects,moft).
ignore_type_relation_type_1_DOM(genf,affects,plnt).
ignore_type_relation_type_1_DOM(genf,coexists_with,menp).
ignore_type_relation_type_1_DOM(genf,coexists_with,moft).
ignore_type_relation_type_1_DOM(genf,follows,menp).
ignore_type_relation_type_1_DOM(genf,follows,moft).
ignore_type_relation_type_1_DOM(genf,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(genf,has_location,blor).
ignore_type_relation_type_1_DOM(genf,has_location,bpoc).
ignore_type_relation_type_1_DOM(genf,has_location,bsoj).
ignore_type_relation_type_1_DOM(genf,has_location,celc).
%ignore_type_relation_type_1_DOM(genf,has_location,cell). % GR comments it out in 09/2015
ignore_type_relation_type_1_DOM(genf,has_location,emst).
ignore_type_relation_type_1_DOM(genf,has_location,ffas).
ignore_type_relation_type_1_DOM(genf,has_location,tisu).
ignore_type_relation_type_1_DOM(genf,has_manifestation,clna).
ignore_type_relation_type_1_DOM(genf,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(genf,has_measurement,clna).
ignore_type_relation_type_1_DOM(genf,has_process,comd). % GR 07/2015
ignore_type_relation_type_1_DOM(genf,has_process,menp).
ignore_type_relation_type_1_DOM(genf,has_process,moft).
ignore_type_relation_type_1_DOM(genf,has_result,clna).
ignore_type_relation_type_1_DOM(genf,has_result,menp).
ignore_type_relation_type_1_DOM(genf,has_result,moft).
ignore_type_relation_type_1_DOM(genf,isa,moft).
ignore_type_relation_type_1_DOM(genf,precedes,menp).
ignore_type_relation_type_1_DOM(genf,precedes,moft).
ignore_type_relation_type_1_DOM(genf,process_of,alga). % GR 09/14/09
%ignore_type_relation_type_1_DOM(genf,process_of,amph). % GR 09/14/09
%ignore_type_relation_type_1_DOM(genf,process_of,anim). % GR 09/14/09 and unblocked in 06/2015
ignore_type_relation_type_1_DOM(genf,process_of,arch). % GR 09/14/09
ignore_type_relation_type_1_DOM(genf,process_of,bact). % GR 09/14/09
%ignore_type_relation_type_1_DOM(genf,process_of,emod). % GR 09/14/09 and unblocked in 06/2015
%ignore_type_relation_type_1_DOM(genf,process_of,fish). % GR 09/14/09 and unblocked in 06/2015
ignore_type_relation_type_1_DOM(genf,process_of,fngs). % GR 09/14/09
ignore_type_relation_type_1_DOM(genf,process_of,invt). % GR 09/14/09
%%ignore_type_relation_type_1_GEN(genf,process_of,mamm).
ignore_type_relation_type_1_DOM(genf,process_of,neop). % GR 09/14/09
ignore_type_relation_type_1_DOM(genf,process_of,orgm). % GR 09/14/09
%ignore_type_relation_type_1_DOM(genf,process_of,rept). % GR 09/14/09 and unblocked in 06/2015
ignore_type_relation_type_1_DOM(genf,process_of,rich). % GR 09/14/09
ignore_type_relation_type_1_DOM(genf,process_of,vtbt). % GR 09/14/09
ignore_type_relation_type_1_DOM(genf,process_of,virs). % GR 09/14/09
ignore_type_relation_type_1_DOM(genf,process_of,biof).
%ignore_type_relation_type_1_DOM(genf,process_of,bird). % GR 09/14/09 and unblocked in 06/2015
ignore_type_relation_type_1_DOM(genf,process_of,celf).
ignore_type_relation_type_1_DOM(genf,process_of,comd).
ignore_type_relation_type_1_DOM(genf,process_of,dsyn).
ignore_type_relation_type_1_DOM(genf,process_of,menp).
ignore_type_relation_type_1_DOM(genf,process_of,mobd).
ignore_type_relation_type_1_DOM(genf,process_of,moft).
ignore_type_relation_type_1_DOM(genf,process_of,npop).
ignore_type_relation_type_1_DOM(genf,process_of,ortf).
ignore_type_relation_type_1_DOM(genf,process_of,plnt).
ignore_type_relation_type_1_DOM(genf,result_of,menp).
ignore_type_relation_type_1_DOM(genf,result_of,moft).
ignore_type_relation_type_1_DOM(geoa,result_of,menp).
ignore_type_relation_type_1_DOM(gngm,affects,menp).
%ignore_type_relation_type_1_DOM(gngm,affects,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(gngm,carries_out,moft).
ignore_type_relation_type_1_DOM(gngm,has_process,dsyn). % 07/2015
ignore_type_relation_type_1_DOM(gngm,part_of,bird).
ignore_type_relation_type_1_DOM(gngm,part_of,invt).
ignore_type_relation_type_1_DOM(gngm,part_of,plnt).
ignore_type_relation_type_1_DOM(grup,exhibits,socb).
ignore_type_relation_type_1_DOM(grup,performs,socb).
ignore_type_relation_type_1_DOM(grup,produces,inpr).
ignore_type_relation_type_1_DOM(grup,produces,mnob).
ignore_type_relation_type_1_DOM(grup,uses,inpr).
ignore_type_relation_type_1_DOM(grup,uses,mnob).
ignore_type_relation_type_1_DOM(hcpp,has_result,clna).
ignore_type_relation_type_1_DOM(hcpp,has_result,menp).
ignore_type_relation_type_1_DOM(hcpp,has_result,moft).
ignore_type_relation_type_1_DOM(hcpp,result_of,menp).
ignore_type_relation_type_1_DOM(hcpp,result_of,moft).
%ignore_type_relation_type_1_DOM(hcro,location_of,edac). % GR 08/2015
ignore_type_relation_type_1_DOM(hcro,location_of,gora).
%ignore_type_relation_type_1_DOM(hcro,location_of,ocac). % GR 08/2015
ignore_type_relation_type_1_DOM(hcro,location_of,topp).
ignore_type_relation_type_1_DOM(hcro,produces,inpr).
ignore_type_relation_type_1_DOM(hops,affects,menp).
ignore_type_relation_type_1_DOM(hops,affects,moft).
ignore_type_relation_type_1_DOM(hops,complicates,menp).
ignore_type_relation_type_1_DOM(hops,complicates,moft).
ignore_type_relation_type_1_DOM(hops,disrupts,gngm).
ignore_type_relation_type_1_DOM(hops,disrupts,menp).
ignore_type_relation_type_1_DOM(hops,interacts_with,chvf).
ignore_type_relation_type_1_DOM(hops,isa,chvf).
ignore_type_relation_type_1_DOM(horm,affects,menp).
%ignore_type_relation_type_1_DOM(horm,affects,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(horm,complicates,menp).
ignore_type_relation_type_1_DOM(horm,complicates,moft).
ignore_type_relation_type_1_DOM(horm,disrupts,gngm).
ignore_type_relation_type_1_DOM(horm,disrupts,menp).
%ignore_type_relation_type_1_DOM(horm,disrupts,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(horm,has_location,plnt).
ignore_type_relation_type_1_DOM(horm,interacts_with,chvf).
ignore_type_relation_type_1_DOM(horm,isa,chvf).
ignore_type_relation_type_1_DOM(horm,produced_by,menp).
ignore_type_relation_type_1_DOM(horm,produced_by,moft).
ignore_type_relation_type_1_DOM(humn,affected_by,menp).
ignore_type_relation_type_1_DOM(humn,affected_by,moft).
ignore_type_relation_type_1_DOM(humn,exhibits,socb).
%ignore_type_relation_type_1_DOM(humn,has_process,menp). % GR 08/2015
ignore_type_relation_type_1_DOM(humn,has_property,clna).
ignore_type_relation_type_1_DOM(humn,interacts_with,alga).
ignore_type_relation_type_1_DOM(humn,interacts_with,amph).
ignore_type_relation_type_1_DOM(humn,interacts_with,anim).
ignore_type_relation_type_1_DOM(humn,interacts_with,arch).
ignore_type_relation_type_1_DOM(humn,interacts_with,bact).
ignore_type_relation_type_1_DOM(humn,interacts_with,fish).
ignore_type_relation_type_1_DOM(humn,interacts_with,fngs).
ignore_type_relation_type_1_DOM(humn,interacts_with,invt).
ignore_type_relation_type_1_DOM(humn,interacts_with,orgm).
ignore_type_relation_type_1_DOM(humn,interacts_with,rept).
ignore_type_relation_type_1_DOM(humn,interacts_with,rich).
ignore_type_relation_type_1_DOM(humn,interacts_with,virs).
ignore_type_relation_type_1_DOM(humn,interacts_with,vtbt).
ignore_type_relation_type_1_DOM(humn,isa,mamm).
ignore_type_relation_type_1_DOM(idcn,result_of,menp).
ignore_type_relation_type_1_DOM(imft,affects,menp).
ignore_type_relation_type_1_DOM(imft,affects,moft).
ignore_type_relation_type_1_DOM(imft,complicates,menp).
ignore_type_relation_type_1_DOM(imft,complicates,moft).
ignore_type_relation_type_1_DOM(imft,disrupts,gngm).
ignore_type_relation_type_1_DOM(imft,disrupts,menp).
ignore_type_relation_type_1_DOM(imft,has_location,plnt).
ignore_type_relation_type_1_DOM(imft,interacts_with,chvf).
ignore_type_relation_type_1_DOM(imft,isa,chvf).
ignore_type_relation_type_1_DOM(imft,produced_by,menp).
ignore_type_relation_type_1_DOM(imft,produced_by,moft).
ignore_type_relation_type_1_DOM(inbe,affected_by,menp).
ignore_type_relation_type_1_DOM(inbe,affected_by,socb).
ignore_type_relation_type_1_DOM(inbe,affects,menp).
ignore_type_relation_type_1_DOM(inbe,affects,socb).
ignore_type_relation_type_1_DOM(inbe,exhibited_by,bird).
ignore_type_relation_type_1_DOM(inbe,exhibited_by,mamm).
ignore_type_relation_type_1_DOM(inbe,has_result,menp).
ignore_type_relation_type_1_DOM(inbe,manifestation_of,menp).
ignore_type_relation_type_1_DOM(inbe,process_of,socb).
ignore_type_relation_type_1_DOM(inbe,result_of,menp).
ignore_type_relation_type_1_DOM(inch,affects,menp).
ignore_type_relation_type_1_DOM(inch,affects,moft).
ignore_type_relation_type_1_DOM(inch,interacts_with,chvf).
%ignore_type_relation_type_1_DOM(inpo,caused_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(inpo,caused_by,mnob).
ignore_type_relation_type_1_DOM(inpo,disrupts,menp).
ignore_type_relation_type_1_DOM(inpo,disrupts,moft).
ignore_type_relation_type_1_DOM(inpo,disrupts,gngm). % GR 08/2015
ignore_type_relation_type_1_DOM(inpo,has_location,gngm).
ignore_type_relation_type_1_DOM(inpo,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(inpo,has_result,clna).
ignore_type_relation_type_1_DOM(inpo,has_result,menp).
ignore_type_relation_type_1_DOM(inpo,has_result,moft).
ignore_type_relation_type_1_DOM(inpo,result_of,menp).
ignore_type_relation_type_1_DOM(inpo,result_of,moft).
ignore_type_relation_type_1_DOM(inpo,result_of,socb).
ignore_type_relation_type_1_DOM(inpr,affects,socb). % 07/2015
ignore_type_relation_type_1_DOM(inpr,affects,ortf). % 07/2015
ignore_type_relation_type_1_DOM(inpr,conceptual_part_of,diap).
ignore_type_relation_type_1_DOM(inpr,conceptual_part_of,inpr).
ignore_type_relation_type_1_DOM(inpr,has_conceptual_part,inpr).
ignore_type_relation_type_1_DOM(inpr,inverse_isa,clas).
ignore_type_relation_type_1_DOM(inpr,inverse_isa,rnlw).
ignore_type_relation_type_1_DOM(inpr,isa,cnce).
ignore_type_relation_type_1_DOM(inpr,isa,enty).
ignore_type_relation_type_1_DOM(inpr,issue_in,bmod).
ignore_type_relation_type_1_DOM(inpr,issue_in,ocdi).
ignore_type_relation_type_1_DOM(inpr,produced_by,aggp).
ignore_type_relation_type_1_DOM(inpr,produced_by,famg).
ignore_type_relation_type_1_DOM(inpr,produced_by,grup).
ignore_type_relation_type_1_DOM(inpr,produced_by,hcro).
ignore_type_relation_type_1_DOM(inpr,produced_by,orgt).
ignore_type_relation_type_1_DOM(inpr,produced_by,podg).
ignore_type_relation_type_1_DOM(inpr,produced_by,popg).
ignore_type_relation_type_1_DOM(inpr,produced_by,prog).
ignore_type_relation_type_1_DOM(inpr,produced_by,pros).
ignore_type_relation_type_1_DOM(inpr,produced_by,shro).
ignore_type_relation_type_1_DOM(inpr,produced_by,chvf). % GR 07/2015
ignore_type_relation_type_1_DOM(inpr,used_by,aggp).
ignore_type_relation_type_1_DOM(inpr,used_by,famg).
ignore_type_relation_type_1_DOM(inpr,used_by,grup).
ignore_type_relation_type_1_DOM(inpr,used_by,podg).
ignore_type_relation_type_1_DOM(inpr,used_by,popg).
ignore_type_relation_type_1_DOM(inpr,used_by,prog).
ignore_type_relation_type_1_DOM(invt,affected_by,menp).
ignore_type_relation_type_1_DOM(invt,affected_by,moft).
ignore_type_relation_type_1_DOM(invt,exhibits,socb).
ignore_type_relation_type_1_DOM(invt,has_part,acab).
ignore_type_relation_type_1_DOM(invt,has_part,anab).
ignore_type_relation_type_1_DOM(invt,has_part,anst).
ignore_type_relation_type_1_DOM(invt,has_part,bpoc).
ignore_type_relation_type_1_DOM(invt,has_part,celc).
ignore_type_relation_type_1_DOM(invt,has_part,cell).
ignore_type_relation_type_1_DOM(invt,has_part,cgab).
ignore_type_relation_type_1_DOM(invt,has_part,emst).
ignore_type_relation_type_1_DOM(invt,has_part,ffas).
ignore_type_relation_type_1_DOM(invt,has_part,gngm).
ignore_type_relation_type_1_DOM(invt,has_part,tisu).
ignore_type_relation_type_1_DOM(invt,has_process,menp).
ignore_type_relation_type_1_DOM(invt,has_process,moft).
ignore_type_relation_type_1_DOM(invt,has_property,clna).
ignore_type_relation_type_1_DOM(invt,interacts_with,bird).
ignore_type_relation_type_1_DOM(invt,interacts_with,humn).
ignore_type_relation_type_1_DOM(invt,interacts_with,mamm).
ignore_type_relation_type_1_DOM(invt,interacts_with,plnt).
ignore_type_relation_type_1_DOM(irda,affects,menp).
ignore_type_relation_type_1_DOM(irda,affects,moft).
ignore_type_relation_type_1_DOM(irda,interacts_with,chvf).
ignore_type_relation_type_1_DOM(irda,isa,chvf).
ignore_type_relation_type_1_DOM(lbpr,affects,biof).
ignore_type_relation_type_1_DOM(lbpr,affects,celf).
ignore_type_relation_type_1_DOM(lbpr,affects,comd).
ignore_type_relation_type_1_DOM(lbpr,affects,dsyn).
ignore_type_relation_type_1_DOM(lbpr,affects,emod).
ignore_type_relation_type_1_DOM(lbpr,affects,genf).
ignore_type_relation_type_1_DOM(lbpr,affects,mobd).
ignore_type_relation_type_1_DOM(lbpr,affects,neop).
ignore_type_relation_type_1_DOM(lbpr,affects,orgf).
ignore_type_relation_type_1_DOM(lbpr,affects,ortf).
ignore_type_relation_type_1_DOM(lbpr,affects,patf).
ignore_type_relation_type_1_DOM(lbpr,affects,phsf).
ignore_type_relation_type_1_DOM(lbpr,analyzes,chvf).
ignore_type_relation_type_1_DOM(lbpr,assesses_effect_of,chvf).
ignore_type_relation_type_1_DOM(lbpr,assesses_effect_of,menp).
ignore_type_relation_type_1_DOM(lbpr,assesses_effect_of,moft).
ignore_type_relation_type_1_DOM(lbpr,measures,chvf).
ignore_type_relation_type_1_DOM(lbpr,measures,clna).
ignore_type_relation_type_1_DOM(lbpr,measures,menp).
ignore_type_relation_type_1_DOM(lbpr,measures,moft).
ignore_type_relation_type_1_DOM(lbtr,coexists_with,fndg).
ignore_type_relation_type_1_DOM(lbtr,evaluation_of,clna).
ignore_type_relation_type_1_DOM(lbtr,evaluation_of,menp).
ignore_type_relation_type_1_DOM(lbtr,evaluation_of,moft).
ignore_type_relation_type_1_DOM(lbtr,indicates,menp).
ignore_type_relation_type_1_DOM(lbtr,indicates,moft).
ignore_type_relation_type_1_DOM(lbtr,isa,fndg).
ignore_type_relation_type_1_DOM(lbtr,manifestation_of,menp).
ignore_type_relation_type_1_DOM(lbtr,manifestation_of,moft).
ignore_type_relation_type_1_DOM(lbtr,measurement_of,chvf).
ignore_type_relation_type_1_DOM(lbtr,measurement_of,menp).
ignore_type_relation_type_1_DOM(lbtr,measurement_of,moft).
ignore_type_relation_type_1_DOM(lipd,affects,menp).
ignore_type_relation_type_1_DOM(lipd,affects,moft).
ignore_type_relation_type_1_DOM(lipd,interacts_with,chvf).
ignore_type_relation_type_1_DOM(mamm,affected_by,acab).
ignore_type_relation_type_1_DOM(mamm,affected_by,anab).
ignore_type_relation_type_1_DOM(mamm,affected_by,biof).
ignore_type_relation_type_1_DOM(mamm,affected_by,celf).
ignore_type_relation_type_1_DOM(mamm,affected_by,cgab).
ignore_type_relation_type_1_DOM(mamm,affected_by,comd).
ignore_type_relation_type_1_DOM(mamm,affected_by,dsyn).
ignore_type_relation_type_1_DOM(mamm,affected_by,emod).
ignore_type_relation_type_1_DOM(mamm,affected_by,genf).
ignore_type_relation_type_1_DOM(mamm,affected_by,mobd).
ignore_type_relation_type_1_DOM(mamm,affected_by,neop).
ignore_type_relation_type_1_DOM(mamm,affected_by,orgf).
ignore_type_relation_type_1_DOM(mamm,affected_by,ortf).
ignore_type_relation_type_1_DOM(mamm,affected_by,patf).
ignore_type_relation_type_1_DOM(mamm,affected_by,phsf).
ignore_type_relation_type_1_DOM(mamm,disrupted_by,inpo). % GR 07/2015
ignore_type_relation_type_1_DOM(mamm,exhibits,bhvr).
ignore_type_relation_type_1_DOM(mamm,exhibits,inbe).
ignore_type_relation_type_1_DOM(emod,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(fish,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(fngs,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(fngs,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(humn,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(mamm,has_process,emod).
%ignore_type_relation_type_1_DOM(mamm,has_process,moft).
ignore_type_relation_type_1_DOM(mamm,has_property,orga).
ignore_type_relation_type_1_DOM(mamm,interacts_with,alga).
ignore_type_relation_type_1_DOM(mamm,interacts_with,amph).
ignore_type_relation_type_1_DOM(mamm,interacts_with,anim).
ignore_type_relation_type_1_DOM(mamm,interacts_with,arch).
ignore_type_relation_type_1_DOM(mamm,interacts_with,bact).
ignore_type_relation_type_1_DOM(mamm,interacts_with,fish).
ignore_type_relation_type_1_DOM(mamm,interacts_with,fngs).
ignore_type_relation_type_1_DOM(mamm,interacts_with,invt).
ignore_type_relation_type_1_DOM(mamm,interacts_with,mamm).
ignore_type_relation_type_1_DOM(mamm,interacts_with,orgm).
ignore_type_relation_type_1_DOM(mamm,interacts_with,rept).
ignore_type_relation_type_1_DOM(mamm,interacts_with,rich).
ignore_type_relation_type_1_DOM(mamm,interacts_with,virs).
ignore_type_relation_type_1_DOM(mamm,interacts_with,vtbt).
ignore_type_relation_type_1_DOM(mamm,inverse_isa,humn).
ignore_type_relation_type_1_DOM(mamm,isa,anim).
ignore_type_relation_type_1_DOM(mamm,isa,enty).
ignore_type_relation_type_1_DOM(mamm,isa,orgm).
ignore_type_relation_type_1_DOM(mamm,isa,phob).
ignore_type_relation_type_1_DOM(mamm,isa,vtbt).
ignore_type_relation_type_1_DOM(mamm,issue_in,bmod).
ignore_type_relation_type_1_DOM(mamm,issue_in,ocdi).
ignore_type_relation_type_1_DOM(mbrt,affects,menp).
ignore_type_relation_type_1_DOM(mbrt,measures,chvf).
ignore_type_relation_type_1_DOM(mbrt,measures,clna).
ignore_type_relation_type_1_DOM(mbrt,measures,menp).
ignore_type_relation_type_1_DOM(mbrt,measures,moft).
ignore_type_relation_type_1_DOM(medd,isa,mnob).
ignore_type_relation_type_1_DOM(menp,affected_by,aapp).
ignore_type_relation_type_1_DOM(menp,affected_by,acab).
ignore_type_relation_type_1_DOM(menp,affected_by,anab).
ignore_type_relation_type_1_DOM(menp,affected_by,antb).
ignore_type_relation_type_1_DOM(menp,affected_by,bacs).
ignore_type_relation_type_1_DOM(menp,affected_by,bhvr).
ignore_type_relation_type_1_DOM(menp,affected_by,biof).
ignore_type_relation_type_1_DOM(menp,affected_by,bodm).
ignore_type_relation_type_1_DOM(menp,affected_by,carb).
ignore_type_relation_type_1_DOM(menp,affected_by,celc).
ignore_type_relation_type_1_DOM(menp,affected_by,celf).
ignore_type_relation_type_1_DOM(menp,affected_by,cgab).
ignore_type_relation_type_1_DOM(menp,affected_by,chem).
ignore_type_relation_type_1_DOM(menp,affected_by,chvs).
ignore_type_relation_type_1_DOM(menp,affected_by,comd).
ignore_type_relation_type_1_DOM(menp,affected_by,diap).
ignore_type_relation_type_1_DOM(menp,affected_by,dsyn).
ignore_type_relation_type_1_DOM(menp,affected_by,eico).
ignore_type_relation_type_1_DOM(menp,affected_by,elii).
ignore_type_relation_type_1_DOM(menp,affected_by,emod).
ignore_type_relation_type_1_DOM(menp,affected_by,enzy).
ignore_type_relation_type_1_DOM(menp,affected_by,food).
ignore_type_relation_type_1_DOM(menp,affected_by,genf).
ignore_type_relation_type_1_DOM(menp,affected_by,gngm).
ignore_type_relation_type_1_DOM(menp,affected_by,hops).
ignore_type_relation_type_1_DOM(menp,affected_by,horm).
ignore_type_relation_type_1_DOM(menp,affected_by,imft).
ignore_type_relation_type_1_DOM(menp,affected_by,inbe).
ignore_type_relation_type_1_DOM(menp,affected_by,inch).
ignore_type_relation_type_1_DOM(menp,affected_by,irda).
ignore_type_relation_type_1_DOM(menp,affected_by,lipd).
ignore_type_relation_type_1_DOM(menp,affected_by,mbrt).
ignore_type_relation_type_1_DOM(menp,affected_by,menp).
ignore_type_relation_type_1_DOM(menp,affected_by,mobd).
ignore_type_relation_type_1_DOM(menp,affected_by,neop).
ignore_type_relation_type_1_DOM(menp,affected_by,nnon).
ignore_type_relation_type_1_DOM(menp,affected_by,npop).
ignore_type_relation_type_1_DOM(menp,affected_by,nsba).
ignore_type_relation_type_1_DOM(menp,affected_by,opco).
ignore_type_relation_type_1_DOM(menp,affected_by,orch).
ignore_type_relation_type_1_DOM(menp,affected_by,orgf).
ignore_type_relation_type_1_DOM(menp,affected_by,ortf).
ignore_type_relation_type_1_DOM(menp,affected_by,patf).
ignore_type_relation_type_1_DOM(menp,affected_by,phsf).
ignore_type_relation_type_1_DOM(menp,affected_by,phsu).
ignore_type_relation_type_1_DOM(menp,affected_by,rcpt).
ignore_type_relation_type_1_DOM(menp,affected_by,resa).
ignore_type_relation_type_1_DOM(menp,affected_by,strd).
ignore_type_relation_type_1_DOM(menp,affected_by,topp).
ignore_type_relation_type_1_DOM(menp,affected_by,vita).
ignore_type_relation_type_1_DOM(menp,affects,alga).
ignore_type_relation_type_1_DOM(menp,affects,amph).
ignore_type_relation_type_1_DOM(menp,affects,anim).
ignore_type_relation_type_1_DOM(menp,affects,arch).
ignore_type_relation_type_1_DOM(menp,affects,bact).
ignore_type_relation_type_1_DOM(menp,affects,bhvr).
ignore_type_relation_type_1_DOM(menp,affects,biof).
ignore_type_relation_type_1_DOM(menp,affects,celf).
ignore_type_relation_type_1_DOM(menp,affects,comd).
ignore_type_relation_type_1_DOM(menp,affects,dsyn).
ignore_type_relation_type_1_DOM(menp,affects,emod).
ignore_type_relation_type_1_DOM(menp,affects,fish).
ignore_type_relation_type_1_DOM(menp,affects,fngs).
ignore_type_relation_type_1_DOM(menp,affects,genf).
ignore_type_relation_type_1_DOM(menp,affects,humn).
ignore_type_relation_type_1_DOM(menp,affects,inbe).
ignore_type_relation_type_1_DOM(menp,affects,invt).
ignore_type_relation_type_1_DOM(menp,affects,menp).
ignore_type_relation_type_1_DOM(menp,affects,mobd).
ignore_type_relation_type_1_DOM(menp,affects,neop).
ignore_type_relation_type_1_DOM(menp,affects,npop).
ignore_type_relation_type_1_DOM(menp,affects,orga).
ignore_type_relation_type_1_DOM(menp,affects,orgf).
ignore_type_relation_type_1_DOM(menp,affects,orgm).
ignore_type_relation_type_1_DOM(menp,affects,ortf).
ignore_type_relation_type_1_DOM(menp,affects,patf).
ignore_type_relation_type_1_DOM(menp,affects,phsf).
ignore_type_relation_type_1_DOM(menp,affects,rept).
ignore_type_relation_type_1_DOM(menp,affects,rich).
ignore_type_relation_type_1_DOM(menp,affects,virs).
ignore_type_relation_type_1_DOM(menp,affects,vtbt).
ignore_type_relation_type_1_DOM(menp,assessed_for_effect_by,lbpr).
ignore_type_relation_type_1_DOM(menp,coexists_with,celf).
ignore_type_relation_type_1_DOM(menp,coexists_with,genf).
ignore_type_relation_type_1_DOM(menp,coexists_with,menp).
ignore_type_relation_type_1_DOM(menp,coexists_with,orgf).
ignore_type_relation_type_1_DOM(menp,coexists_with,ortf).
ignore_type_relation_type_1_DOM(menp,coexists_with,phsf).
ignore_type_relation_type_1_DOM(menp,complicated_by,antb).
ignore_type_relation_type_1_DOM(menp,complicated_by,bacs).
ignore_type_relation_type_1_DOM(menp,complicated_by,enzy).
ignore_type_relation_type_1_DOM(menp,complicated_by,hops).
ignore_type_relation_type_1_DOM(menp,complicated_by,horm).
ignore_type_relation_type_1_DOM(menp,complicated_by,imft).
ignore_type_relation_type_1_DOM(menp,complicated_by,nsba).
ignore_type_relation_type_1_DOM(menp,complicated_by,phsu).
ignore_type_relation_type_1_DOM(menp,complicated_by,rcpt).
ignore_type_relation_type_1_DOM(menp,complicated_by,topp).
ignore_type_relation_type_1_DOM(menp,complicated_by,vita).
ignore_type_relation_type_1_DOM(menp,degree_of,menp).
ignore_type_relation_type_1_DOM(menp,degree_of,orgf).
ignore_type_relation_type_1_DOM(menp,disrupted_by,antb).
ignore_type_relation_type_1_DOM(menp,disrupted_by,bacs).
ignore_type_relation_type_1_DOM(menp,disrupted_by,enzy).
ignore_type_relation_type_1_DOM(menp,disrupted_by,hops).
ignore_type_relation_type_1_DOM(menp,disrupted_by,horm).
ignore_type_relation_type_1_DOM(menp,disrupted_by,imft).
ignore_type_relation_type_1_DOM(menp,disrupted_by,inpo).
ignore_type_relation_type_1_DOM(menp,disrupted_by,nsba).
ignore_type_relation_type_1_DOM(menp,disrupted_by,phsu).
ignore_type_relation_type_1_DOM(menp,disrupted_by,rcpt).
ignore_type_relation_type_1_DOM(menp,disrupted_by,vita).
ignore_type_relation_type_1_DOM(menp,follows,celf).
ignore_type_relation_type_1_DOM(menp,follows,genf).
ignore_type_relation_type_1_DOM(menp,follows,menp).
ignore_type_relation_type_1_DOM(menp,follows,orgf).
ignore_type_relation_type_1_DOM(menp,follows,ortf).
ignore_type_relation_type_1_DOM(menp,follows,phsf).
ignore_type_relation_type_1_DOM(menp,has_degree,menp).
ignore_type_relation_type_1_DOM(menp,has_degree,orgf).
ignore_type_relation_type_1_DOM(menp,has_evaluation,lbtr).
ignore_type_relation_type_1_DOM(menp,has_evaluation,sosy).
ignore_type_relation_type_1_DOM(menp,has_location,blor).
ignore_type_relation_type_1_DOM(menp,has_location,bpoc).
ignore_type_relation_type_1_DOM(menp,has_location,bsoj).
ignore_type_relation_type_1_DOM(menp,has_location,celc).
ignore_type_relation_type_1_DOM(menp,has_location,cell).
ignore_type_relation_type_1_DOM(menp,has_location,emst).
ignore_type_relation_type_1_DOM(menp,has_location,ffas).
ignore_type_relation_type_1_DOM(menp,has_location,tisu).
ignore_type_relation_type_1_DOM(menp,has_manifestation,acab).
ignore_type_relation_type_1_DOM(menp,has_manifestation,anab).
ignore_type_relation_type_1_DOM(menp,has_manifestation,bhvr).
ignore_type_relation_type_1_DOM(menp,has_manifestation,cgab).
ignore_type_relation_type_1_DOM(menp,has_manifestation,comd).
ignore_type_relation_type_1_DOM(menp,has_manifestation,dsyn).
ignore_type_relation_type_1_DOM(menp,has_manifestation,emod).
ignore_type_relation_type_1_DOM(menp,has_manifestation,inbe).
ignore_type_relation_type_1_DOM(menp,has_manifestation,lbtr).
ignore_type_relation_type_1_DOM(menp,has_manifestation,mobd).
ignore_type_relation_type_1_DOM(menp,has_manifestation,neop).
ignore_type_relation_type_1_DOM(menp,has_manifestation,orga).
ignore_type_relation_type_1_DOM(menp,has_manifestation,patf).
ignore_type_relation_type_1_DOM(menp,has_manifestation,sosy).
ignore_type_relation_type_1_DOM(menp,has_measurement,lbtr).
ignore_type_relation_type_1_DOM(menp,has_measurement,orga).
ignore_type_relation_type_1_DOM(menp,has_measurement,qnco).
ignore_type_relation_type_1_DOM(menp,has_occurrence,ortf).
ignore_type_relation_type_1_DOM(emod,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(fngs,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(menp,has_process,biof).
ignore_type_relation_type_1_DOM(menp,has_process,celf).
ignore_type_relation_type_1_DOM(menp,has_process,comd).
ignore_type_relation_type_1_DOM(menp,has_process,dsyn).
ignore_type_relation_type_1_DOM(menp,has_process,emod).
ignore_type_relation_type_1_DOM(menp,has_process,genf).
ignore_type_relation_type_1_DOM(menp,has_process,menp).
ignore_type_relation_type_1_DOM(emod,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(fngs,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(menp,has_process,mobd).
ignore_type_relation_type_1_DOM(fngs,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(menp,has_process,neop).
ignore_type_relation_type_1_DOM(emod,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(fngs,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(menp,has_process,npop).
ignore_type_relation_type_1_DOM(emod,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(fngs,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(menp,has_process,orgf).
ignore_type_relation_type_1_DOM(fngs,has_process,ortf).
ignore_type_relation_type_1_DOM(genf,has_process,ortf).
ignore_type_relation_type_1_DOM(invt,has_process,ortf). % GR 06/2015
ignore_type_relation_type_1_DOM(menp,has_process,ortf).
ignore_type_relation_type_1_DOM(fngs,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(menp,has_process,patf).
ignore_type_relation_type_1_DOM(fngs,has_process,phsf). % 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,phsf). % 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,phsf). % 06/2015
ignore_type_relation_type_1_DOM(menp,has_process,phsf).
ignore_type_relation_type_1_DOM(menp,has_result,acab).
ignore_type_relation_type_1_DOM(menp,has_result,amas).
ignore_type_relation_type_1_DOM(menp,has_result,anab).
ignore_type_relation_type_1_DOM(menp,has_result,bdsy).
ignore_type_relation_type_1_DOM(menp,has_result,bhvr).
ignore_type_relation_type_1_DOM(menp,has_result,biof).
ignore_type_relation_type_1_DOM(menp,has_result,blor).
ignore_type_relation_type_1_DOM(menp,has_result,bsoj).
ignore_type_relation_type_1_DOM(menp,has_result,celf).
ignore_type_relation_type_1_DOM(menp,has_result,cgab).
ignore_type_relation_type_1_DOM(menp,has_result,comd).
ignore_type_relation_type_1_DOM(menp,has_result,crbs).
ignore_type_relation_type_1_DOM(menp,has_result,dsyn).
ignore_type_relation_type_1_DOM(menp,has_result,eehu).
ignore_type_relation_type_1_DOM(menp,has_result,emod).
ignore_type_relation_type_1_DOM(menp,has_result,ftcn).
ignore_type_relation_type_1_DOM(menp,has_result,genf).
ignore_type_relation_type_1_DOM(menp,has_result,geoa).
ignore_type_relation_type_1_DOM(menp,has_result,hcpp).
ignore_type_relation_type_1_DOM(menp,has_result,idcn).
ignore_type_relation_type_1_DOM(menp,has_result,inbe).
ignore_type_relation_type_1_DOM(menp,has_result,inpo).
ignore_type_relation_type_1_DOM(menp,has_result,menp).
ignore_type_relation_type_1_DOM(menp,has_result,mobd).
ignore_type_relation_type_1_DOM(menp,has_result,mosq).
ignore_type_relation_type_1_DOM(menp,has_result,neop).
ignore_type_relation_type_1_DOM(menp,has_result,npop).
ignore_type_relation_type_1_DOM(menp,has_result,nusq).
ignore_type_relation_type_1_DOM(menp,has_result,orga).
ignore_type_relation_type_1_DOM(menp,has_result,orgf).
ignore_type_relation_type_1_DOM(menp,has_result,ortf).
ignore_type_relation_type_1_DOM(menp,has_result,patf).
ignore_type_relation_type_1_DOM(menp,has_result,phpr).
ignore_type_relation_type_1_DOM(menp,has_result,phsf).
ignore_type_relation_type_1_DOM(menp,has_result,qlco).
ignore_type_relation_type_1_DOM(menp,has_result,qnco).
ignore_type_relation_type_1_DOM(menp,has_result,spco).
ignore_type_relation_type_1_DOM(menp,has_result,tmco).
ignore_type_relation_type_1_DOM(menp,indicated_by,lbtr).
ignore_type_relation_type_1_DOM(menp,isa,biof).
ignore_type_relation_type_1_DOM(menp,isa,evnt).
ignore_type_relation_type_1_DOM(menp,isa,npop).
ignore_type_relation_type_1_DOM(menp,isa,orgf).
ignore_type_relation_type_1_DOM(menp,isa,phpr).
ignore_type_relation_type_1_DOM(menp,isa,phsf).
ignore_type_relation_type_1_DOM(menp,issue_in,bmod).
ignore_type_relation_type_1_DOM(menp,issue_in,ocdi).
ignore_type_relation_type_1_DOM(menp,measured_by,diap).
ignore_type_relation_type_1_DOM(menp,measured_by,lbpr).
ignore_type_relation_type_1_DOM(menp,measured_by,mbrt).
ignore_type_relation_type_1_DOM(menp,measured_by,resa).
ignore_type_relation_type_1_DOM(menp,occurs_in,tmco).
ignore_type_relation_type_1_DOM(menp,precedes,celf).
ignore_type_relation_type_1_DOM(menp,precedes,genf).
ignore_type_relation_type_1_DOM(menp,precedes,menp).
ignore_type_relation_type_1_DOM(menp,precedes,orgf).
ignore_type_relation_type_1_DOM(menp,precedes,ortf).
ignore_type_relation_type_1_DOM(menp,precedes,phsf).
ignore_type_relation_type_1_DOM(menp,process_of,amph).
%ignore_type_relation_type_1_DOM(menp,process_of,anim). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(menp,process_of,arch).
ignore_type_relation_type_1_DOM(menp,process_of,celf).
ignore_type_relation_type_1_DOM(menp,process_of,comd).
ignore_type_relation_type_1_DOM(menp,process_of,emod).
ignore_type_relation_type_1_DOM(menp,process_of,fish).
%ignore_type_relation_type_1_DOM(menp,process_of,humn). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(menp,process_of,invt).
ignore_type_relation_type_1_DOM(menp,process_of,menp).
ignore_type_relation_type_1_DOM(menp,process_of,mobd).
ignore_type_relation_type_1_DOM(menp,process_of,neop).
ignore_type_relation_type_1_DOM(menp,process_of,orgf).
ignore_type_relation_type_1_DOM(menp,process_of,orgm).
ignore_type_relation_type_1_DOM(menp,process_of,patf).
ignore_type_relation_type_1_DOM(menp,process_of,rept).
ignore_type_relation_type_1_DOM(menp,process_of,vtbt).
ignore_type_relation_type_1_DOM(menp,produces,aggp).
ignore_type_relation_type_1_DOM(menp,produces,bacs).
ignore_type_relation_type_1_DOM(menp,produces,bdsu).
ignore_type_relation_type_1_DOM(menp,produces,enzy).
ignore_type_relation_type_1_DOM(menp,produces,famg).
ignore_type_relation_type_1_DOM(menp,produces,horm).
ignore_type_relation_type_1_DOM(menp,produces,imft).
ignore_type_relation_type_1_DOM(menp,produces,nsba).
ignore_type_relation_type_1_DOM(menp,produces,rcpt).
ignore_type_relation_type_1_DOM(menp,produces,vita).
ignore_type_relation_type_1_DOM(menp,result_of,acab).
ignore_type_relation_type_1_DOM(menp,result_of,anab).
ignore_type_relation_type_1_DOM(menp,result_of,bhvr).
ignore_type_relation_type_1_DOM(menp,result_of,biof).
ignore_type_relation_type_1_DOM(menp,result_of,celf).
ignore_type_relation_type_1_DOM(menp,result_of,cgab).
ignore_type_relation_type_1_DOM(menp,result_of,comd).
ignore_type_relation_type_1_DOM(menp,result_of,dsyn).
ignore_type_relation_type_1_DOM(menp,result_of,eehu).
ignore_type_relation_type_1_DOM(menp,result_of,emod).
ignore_type_relation_type_1_DOM(menp,result_of,genf).
ignore_type_relation_type_1_DOM(menp,result_of,hcpp).
ignore_type_relation_type_1_DOM(menp,result_of,inbe).
ignore_type_relation_type_1_DOM(menp,result_of,inpo).
ignore_type_relation_type_1_DOM(menp,result_of,menp).
ignore_type_relation_type_1_DOM(menp,result_of,mobd).
ignore_type_relation_type_1_DOM(menp,result_of,neop).
ignore_type_relation_type_1_DOM(menp,result_of,npop).
ignore_type_relation_type_1_DOM(menp,result_of,orgf).
ignore_type_relation_type_1_DOM(menp,result_of,ortf).
ignore_type_relation_type_1_DOM(menp,result_of,patf).
ignore_type_relation_type_1_DOM(menp,result_of,phpr).
ignore_type_relation_type_1_DOM(menp,result_of,phsf).
ignore_type_relation_type_1_DOM(mnob,affects,rich). % 07/2015
ignore_type_relation_type_1_DOM(mnob,affects,virs). % 07/2015
ignore_type_relation_type_1_DOM(mnob,causes,acab).
ignore_type_relation_type_1_DOM(mnob,causes,anab).
ignore_type_relation_type_1_DOM(mnob,causes,cgab).
ignore_type_relation_type_1_DOM(mnob,causes,comd).
ignore_type_relation_type_1_DOM(mnob,causes,dsyn).
ignore_type_relation_type_1_DOM(mnob,causes,emod).
ignore_type_relation_type_1_DOM(mnob,causes,inpo).
ignore_type_relation_type_1_DOM(mnob,causes,mobd).
ignore_type_relation_type_1_DOM(mnob,causes,neop).
ignore_type_relation_type_1_DOM(mnob,causes,patf).
ignore_type_relation_type_1_DOM(mnob,inverse_isa,clnd).
ignore_type_relation_type_1_DOM(mnob,inverse_isa,drdd).
ignore_type_relation_type_1_DOM(mnob,inverse_isa,medd).
ignore_type_relation_type_1_DOM(mnob,inverse_isa,resd).
ignore_type_relation_type_1_DOM(mnob,isa,enty).
ignore_type_relation_type_1_DOM(mnob,isa,phob).
ignore_type_relation_type_1_DOM(mnob,issue_in,bmod).
ignore_type_relation_type_1_DOM(mnob,issue_in,ocdi).
ignore_type_relation_type_1_DOM(mnob,produced_by,aggp).
ignore_type_relation_type_1_DOM(mnob,produced_by,famg).
ignore_type_relation_type_1_DOM(mnob,produced_by,grup).
ignore_type_relation_type_1_DOM(mnob,produced_by,podg).
ignore_type_relation_type_1_DOM(mnob,produced_by,popg).
ignore_type_relation_type_1_DOM(mnob,produced_by,prog).
ignore_type_relation_type_1_DOM(mnob,produced_by,pros). % 07/2015
ignore_type_relation_type_1_DOM(mnob,used_by,aggp).
ignore_type_relation_type_1_DOM(mnob,used_by,diap).
ignore_type_relation_type_1_DOM(mnob,used_by,famg).
ignore_type_relation_type_1_DOM(mnob,used_by,grup).
ignore_type_relation_type_1_DOM(mnob,used_by,podg).
ignore_type_relation_type_1_DOM(mnob,used_by,popg).
ignore_type_relation_type_1_DOM(mnob,used_by,prog).
ignore_type_relation_type_1_DOM(mnob,used_by,topp).
%ignore_type_relation_type_1_DOM(mobd,affected_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(mobd,affected_by,lbpr).
ignore_type_relation_type_1_DOM(mobd,affected_by,menp).
ignore_type_relation_type_1_DOM(mobd,affected_by,moft).
ignore_type_relation_type_1_DOM(mobd,affects,bird).
ignore_type_relation_type_1_DOM(mobd,affects,mamm).
ignore_type_relation_type_1_DOM(mobd,affects,menp).
ignore_type_relation_type_1_DOM(mobd,affects,moft).
ignore_type_relation_type_1_DOM(mobd,affects,plnt).
ignore_type_relation_type_1_DOM(mobd,affects,socb).
%ignore_type_relation_type_1_DOM(mobd,caused_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(mobd,caused_by,mnob).
ignore_type_relation_type_1_DOM(mobd,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(mobd,has_location,gngm).
ignore_type_relation_type_1_DOM(mobd,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(mobd,has_manifestation,socb).
ignore_type_relation_type_1_DOM(mobd,has_process,biof).
ignore_type_relation_type_1_DOM(mobd,has_process,celf).
ignore_type_relation_type_1_DOM(mobd,has_process,comd).
ignore_type_relation_type_1_DOM(mobd,has_process,dsyn).
ignore_type_relation_type_1_DOM(mobd,has_process,emod).
ignore_type_relation_type_1_DOM(mobd,has_process,genf).
ignore_type_relation_type_1_DOM(mobd,has_process,mobd).
ignore_type_relation_type_1_DOM(mobd,has_process,neop).
ignore_type_relation_type_1_DOM(mobd,has_process,npop).
ignore_type_relation_type_1_DOM(mobd,has_process,orgf).
ignore_type_relation_type_1_DOM(mobd,has_process,ortf).
ignore_type_relation_type_1_DOM(mobd,has_process,patf).
ignore_type_relation_type_1_DOM(mobd,has_process,phsf).
ignore_type_relation_type_1_DOM(mobd,has_result,clna).
ignore_type_relation_type_1_DOM(mobd,has_result,menp).
ignore_type_relation_type_1_DOM(mobd,has_result,moft).
ignore_type_relation_type_1_DOM(mobd,manifestation_of,menp).
ignore_type_relation_type_1_DOM(mobd,manifestation_of,moft).
ignore_type_relation_type_1_DOM(mobd,process_of,alga). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,amph). % 09/22/09
%ignore_type_relation_type_1_DOM(mobd,process_of,anim). % 09/22/09 // Unblocked 06/2015
ignore_type_relation_type_1_DOM(mobd,process_of,arch). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,bact). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,emod). % 09/16/09
%ignore_type_relation_type_1_DOM(mobd,process_of,fish). % 09/22/09 // Unblocked 06/2015
ignore_type_relation_type_1_DOM(mobd,process_of,fngs). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,invt). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,neop). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,orgm). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,plnt). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,rept). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,rich). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,virs). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,vtbt). % 09/22/09
ignore_type_relation_type_1_DOM(mobd,process_of,biof).
%ignore_type_relation_type_1_DOM(mobd,process_of,bird). % 09/22/09 // Unblocked 06/2015
ignore_type_relation_type_1_DOM(mobd,process_of,celf).
ignore_type_relation_type_1_DOM(mobd,process_of,comd).
ignore_type_relation_type_1_DOM(mobd,process_of,dsyn).
ignore_type_relation_type_1_DOM(mobd,process_of,genf).
%ignore_type_relation_type_1_DOM(mobd,process_of,mamm). % unblocked 06/2015
ignore_type_relation_type_1_DOM(mobd,process_of,menp).
ignore_type_relation_type_1_DOM(mobd,process_of,mobd).
ignore_type_relation_type_1_DOM(mobd,process_of,moft).
ignore_type_relation_type_1_DOM(mobd,process_of,npop).
ignore_type_relation_type_1_DOM(mobd,process_of,orgf).
ignore_type_relation_type_1_DOM(mobd,process_of,ortf).
ignore_type_relation_type_1_DOM(mobd,process_of,patf).
ignore_type_relation_type_1_DOM(mobd,process_of,phsf).
ignore_type_relation_type_1_DOM(mobd,result_of,menp).
ignore_type_relation_type_1_DOM(mobd,result_of,moft).
ignore_type_relation_type_1_DOM(mobd,result_of,socb).
%ignore_type_relation_type_1_DOM(moft,affected_by,aapp).
ignore_type_relation_type_1_DOM(moft,affected_by,acab).
ignore_type_relation_type_1_DOM(moft,affected_by,anab).
ignore_type_relation_type_1_DOM(moft,affected_by,antb). % GR 08/2015; to leave the one in SN
ignore_type_relation_type_1_DOM(moft,affected_by,biof).
ignore_type_relation_type_1_DOM(moft,affected_by,bodm).
ignore_type_relation_type_1_DOM(moft,affected_by,carb).
ignore_type_relation_type_1_DOM(moft,affected_by,celc).
ignore_type_relation_type_1_DOM(moft,affected_by,celf).
ignore_type_relation_type_1_DOM(moft,affected_by,cgab).
ignore_type_relation_type_1_DOM(moft,affected_by,chem).
ignore_type_relation_type_1_DOM(moft,affected_by,chvs).
ignore_type_relation_type_1_DOM(moft,affected_by,comd).
ignore_type_relation_type_1_DOM(moft,affected_by,diap).
ignore_type_relation_type_1_DOM(moft,affected_by,dsyn).
ignore_type_relation_type_1_DOM(moft,affected_by,eico).
ignore_type_relation_type_1_DOM(moft,affected_by,elii).
ignore_type_relation_type_1_DOM(moft,affected_by,emod).
%ignore_type_relation_type_1_DOM(moft,affected_by,enzy). % GR 08/2015; to leave the one in SN
ignore_type_relation_type_1_DOM(moft,affected_by,food).
ignore_type_relation_type_1_DOM(moft,affected_by,genf).
%ignore_type_relation_type_1_DOM(moft,affected_by,gngm). % GR 08/2015; to leave the one in SN
ignore_type_relation_type_1_DOM(moft,affected_by,hops).
%ignore_type_relation_type_1_DOM(moft,affected_by,horm). % GR 08/2015; to leave the one in SN
ignore_type_relation_type_1_DOM(moft,affected_by,imft).
ignore_type_relation_type_1_DOM(moft,affected_by,inch).
ignore_type_relation_type_1_DOM(moft,affected_by,irda).
ignore_type_relation_type_1_DOM(moft,affected_by,lipd).
ignore_type_relation_type_1_DOM(moft,affected_by,mobd).
ignore_type_relation_type_1_DOM(moft,affected_by,moft).
ignore_type_relation_type_1_DOM(moft,affected_by,neop).
ignore_type_relation_type_1_DOM(moft,affected_by,nnon).
ignore_type_relation_type_1_DOM(moft,affected_by,npop).
ignore_type_relation_type_1_DOM(moft,affected_by,opco).
ignore_type_relation_type_1_DOM(moft,affected_by,orch).
ignore_type_relation_type_1_DOM(moft,affected_by,orgf).
ignore_type_relation_type_1_DOM(moft,affected_by,ortf).
ignore_type_relation_type_1_DOM(moft,affected_by,patf).
ignore_type_relation_type_1_DOM(moft,affected_by,phsf).
%ignore_type_relation_type_1_DOM(moft,affected_by,phsu). % GR 08/2015; to leave the one in SN
%ignore_type_relation_type_1_DOM(moft,affected_by,strd). % GR 08/2015; to leave the one in SN
%ignore_type_relation_type_1_DOM(moft,affected_by,vita). % GR 08/2015; to leave the one in SN
ignore_type_relation_type_1_DOM(moft,affects,alga).
ignore_type_relation_type_1_DOM(moft,affects,amph).
ignore_type_relation_type_1_DOM(moft,affects,anim).
ignore_type_relation_type_1_DOM(moft,affects,arch).
ignore_type_relation_type_1_DOM(moft,affects,bact).
ignore_type_relation_type_1_DOM(moft,affects,biof).
ignore_type_relation_type_1_DOM(moft,affects,celf).
ignore_type_relation_type_1_DOM(moft,affects,comd).
ignore_type_relation_type_1_DOM(moft,affects,dsyn).
ignore_type_relation_type_1_DOM(moft,affects,emod).
ignore_type_relation_type_1_DOM(moft,affects,fish).
ignore_type_relation_type_1_DOM(moft,affects,fngs).
ignore_type_relation_type_1_DOM(moft,affects,genf).
ignore_type_relation_type_1_DOM(moft,affects,humn).
ignore_type_relation_type_1_DOM(moft,affects,invt).
ignore_type_relation_type_1_DOM(moft,affects,mobd).
ignore_type_relation_type_1_DOM(moft,affects,moft).
ignore_type_relation_type_1_DOM(moft,affects,neop).
ignore_type_relation_type_1_DOM(moft,affects,npop).
ignore_type_relation_type_1_DOM(moft,affects,orga).
ignore_type_relation_type_1_DOM(moft,affects,orgm).
ignore_type_relation_type_1_DOM(moft,affects,ortf).
ignore_type_relation_type_1_DOM(moft,affects,patf).
ignore_type_relation_type_1_DOM(moft,affects,phsf).
ignore_type_relation_type_1_DOM(moft,affects,rept).
ignore_type_relation_type_1_DOM(moft,affects,rich).
ignore_type_relation_type_1_DOM(moft,affects,virs).
ignore_type_relation_type_1_DOM(moft,affects,vtbt).
ignore_type_relation_type_1_DOM(moft,assessed_for_effect_by,lbpr).
ignore_type_relation_type_1_DOM(moft,carried_out_by,gngm).
ignore_type_relation_type_1_DOM(moft,coexists_with,celf).
ignore_type_relation_type_1_DOM(moft,coexists_with,genf).
ignore_type_relation_type_1_DOM(moft,coexists_with,moft).
ignore_type_relation_type_1_DOM(moft,coexists_with,orgf).
ignore_type_relation_type_1_DOM(moft,coexists_with,ortf).
ignore_type_relation_type_1_DOM(moft,coexists_with,phsf).
ignore_type_relation_type_1_DOM(moft,complicated_by,antb).
ignore_type_relation_type_1_DOM(moft,complicated_by,bacs).
ignore_type_relation_type_1_DOM(moft,complicated_by,enzy).
ignore_type_relation_type_1_DOM(moft,complicated_by,hops).
ignore_type_relation_type_1_DOM(moft,complicated_by,horm).
ignore_type_relation_type_1_DOM(moft,complicated_by,imft).
ignore_type_relation_type_1_DOM(moft,complicated_by,nsba).
ignore_type_relation_type_1_DOM(moft,complicated_by,phsu).
ignore_type_relation_type_1_DOM(moft,complicated_by,rcpt).
ignore_type_relation_type_1_DOM(moft,complicated_by,topp).
ignore_type_relation_type_1_DOM(moft,complicated_by,vita).
ignore_type_relation_type_1_DOM(moft,disrupted_by,antb).
%ignore_type_relation_type_1_DOM(moft,disrupted_by,enzy). % 08/2015 to leave the one in locsemnet
%ignore_type_relation_type_1_DOM(moft,disrupted_by,horm). % 08/2015 to leave the one in locsemnet
ignore_type_relation_type_1_DOM(moft,disrupted_by,inpo).
ignore_type_relation_type_1_DOM(moft,disrupted_by,vita).
ignore_type_relation_type_1_DOM(moft,follows,celf).
ignore_type_relation_type_1_DOM(moft,follows,genf).
ignore_type_relation_type_1_DOM(moft,follows,moft).
ignore_type_relation_type_1_DOM(moft,follows,orgf).
ignore_type_relation_type_1_DOM(moft,follows,ortf).
ignore_type_relation_type_1_DOM(moft,follows,phsf).
ignore_type_relation_type_1_DOM(moft,has_evaluation,lbtr).
ignore_type_relation_type_1_DOM(moft,has_evaluation,sosy).
ignore_type_relation_type_1_DOM(moft,has_location,blor).
ignore_type_relation_type_1_DOM(moft,has_location,bpoc).
ignore_type_relation_type_1_DOM(moft,has_location,bsoj).
ignore_type_relation_type_1_DOM(moft,has_location,celc).
ignore_type_relation_type_1_DOM(moft,has_location,cell).
ignore_type_relation_type_1_DOM(moft,has_location,emst).
ignore_type_relation_type_1_DOM(moft,has_location,ffas).
ignore_type_relation_type_1_DOM(moft,has_location,tisu).
ignore_type_relation_type_1_DOM(moft,has_manifestation,acab).
ignore_type_relation_type_1_DOM(moft,has_manifestation,anab).
ignore_type_relation_type_1_DOM(moft,has_manifestation,cgab).
ignore_type_relation_type_1_DOM(moft,has_manifestation,comd).
ignore_type_relation_type_1_DOM(moft,has_manifestation,dsyn).
ignore_type_relation_type_1_DOM(moft,has_manifestation,emod).
ignore_type_relation_type_1_DOM(moft,has_manifestation,lbtr).
ignore_type_relation_type_1_DOM(moft,has_manifestation,mobd).
ignore_type_relation_type_1_DOM(moft,has_manifestation,neop).
ignore_type_relation_type_1_DOM(moft,has_manifestation,orga).
ignore_type_relation_type_1_DOM(moft,has_manifestation,patf).
ignore_type_relation_type_1_DOM(moft,has_manifestation,sosy).
ignore_type_relation_type_1_DOM(moft,has_measurement,lbtr).
ignore_type_relation_type_1_DOM(moft,has_measurement,orga).
ignore_type_relation_type_1_DOM(moft,has_measurement,qnco).
ignore_type_relation_type_1_DOM(moft,has_occurrence,ortf). % GR 07/2015
ignore_type_relation_type_1_DOM(moft,has_process,biof).
ignore_type_relation_type_1_DOM(moft,has_process,celf).
ignore_type_relation_type_1_DOM(moft,has_process,comd).
ignore_type_relation_type_1_DOM(moft,has_process,dsyn).
ignore_type_relation_type_1_DOM(moft,has_process,emod).
ignore_type_relation_type_1_DOM(moft,has_process,genf).
ignore_type_relation_type_1_DOM(moft,has_process,mobd).
ignore_type_relation_type_1_DOM(moft,has_process,moft).
ignore_type_relation_type_1_DOM(moft,has_process,neop).
ignore_type_relation_type_1_DOM(moft,has_process,npop).
ignore_type_relation_type_1_DOM(moft,has_process,orgf).
ignore_type_relation_type_1_DOM(moft,has_process,ortf).
ignore_type_relation_type_1_DOM(moft,has_process,patf).
ignore_type_relation_type_1_DOM(moft,has_process,phsf).
ignore_type_relation_type_1_DOM(moft,has_result,acab).
ignore_type_relation_type_1_DOM(moft,has_result,anab).
ignore_type_relation_type_1_DOM(moft,has_result,biof).
ignore_type_relation_type_1_DOM(moft,has_result,celf).
ignore_type_relation_type_1_DOM(moft,has_result,cgab).
ignore_type_relation_type_1_DOM(moft,has_result,comd).
ignore_type_relation_type_1_DOM(moft,has_result,dsyn).
ignore_type_relation_type_1_DOM(moft,has_result,eehu).
ignore_type_relation_type_1_DOM(moft,has_result,emod).
ignore_type_relation_type_1_DOM(moft,has_result,genf).
ignore_type_relation_type_1_DOM(moft,has_result,hcpp).
ignore_type_relation_type_1_DOM(moft,has_result,inpo).
ignore_type_relation_type_1_DOM(moft,has_result,mobd).
ignore_type_relation_type_1_DOM(moft,has_result,moft).
ignore_type_relation_type_1_DOM(moft,has_result,neop).
ignore_type_relation_type_1_DOM(moft,has_result,npop).
ignore_type_relation_type_1_DOM(moft,has_result,orga).
ignore_type_relation_type_1_DOM(moft,has_result,orgf).
ignore_type_relation_type_1_DOM(moft,has_result,ortf).
ignore_type_relation_type_1_DOM(moft,has_result,patf).
ignore_type_relation_type_1_DOM(moft,has_result,phpr).
ignore_type_relation_type_1_DOM(moft,has_result,phsf).
ignore_type_relation_type_1_DOM(moft,indicated_by,lbtr).
ignore_type_relation_type_1_DOM(moft,interacts_with,opco). % GR 08/2015
ignore_type_relation_type_1_DOM(moft,interacts_with,orch). % GR 08/2015
ignore_type_relation_type_1_DOM(moft,interacts_with,phsu). % GR 08/2015
ignore_type_relation_type_1_DOM(moft,interacts_with,strd). % GR 08/2015
ignore_type_relation_type_1_DOM(moft,inverse_isa,genf).
ignore_type_relation_type_1_DOM(moft,isa,biof).
ignore_type_relation_type_1_DOM(moft,isa,evnt).
ignore_type_relation_type_1_DOM(moft,isa,npop).
ignore_type_relation_type_1_DOM(moft,isa,phpr).
ignore_type_relation_type_1_DOM(moft,isa,phsf).
ignore_type_relation_type_1_DOM(moft,issue_in,bmod).
ignore_type_relation_type_1_DOM(moft,issue_in,ocdi).
ignore_type_relation_type_1_DOM(moft,location_of,topp). % GR 07/2015
ignore_type_relation_type_1_DOM(moft,location_of,rcpt). % GR 07/2015
ignore_type_relation_type_1_DOM(moft,location_of,rich). % GR 07/2015
ignore_type_relation_type_1_DOM(moft,location_of,vita). % GR 07/2015
ignore_type_relation_type_1_DOM(moft,location_of,virs). % GR 07/2015
ignore_type_relation_type_1_DOM(moft,location_of,nsba). % GR 07/2015
ignore_type_relation_type_1_DOM(moft,measured_by,diap).
ignore_type_relation_type_1_DOM(moft,measured_by,lbpr).
ignore_type_relation_type_1_DOM(moft,measured_by,mbrt).
ignore_type_relation_type_1_DOM(moft,measured_by,resa).
ignore_type_relation_type_1_DOM(moft,occurs_in,tmco).
ignore_type_relation_type_1_DOM(moft,precedes,celf).
ignore_type_relation_type_1_DOM(moft,precedes,genf).
ignore_type_relation_type_1_DOM(moft,precedes,moft).
ignore_type_relation_type_1_DOM(moft,precedes,orgf).
ignore_type_relation_type_1_DOM(moft,precedes,ortf).
ignore_type_relation_type_1_DOM(moft,precedes,phsf).
ignore_type_relation_type_1_DOM(moft,process_of,alga).
ignore_type_relation_type_1_DOM(moft,process_of,amph).
%ignore_type_relation_type_1_DOM(moft,process_of,anim). % 07/2015
ignore_type_relation_type_1_DOM(moft,process_of,arch).
ignore_type_relation_type_1_DOM(moft,process_of,bact).
ignore_type_relation_type_1_DOM(moft,process_of,celf).
ignore_type_relation_type_1_DOM(moft,process_of,comd).
ignore_type_relation_type_1_DOM(moft,process_of,emod).
ignore_type_relation_type_1_DOM(moft,process_of,fish).
ignore_type_relation_type_1_DOM(moft,process_of,fngs).
%ignore_type_relation_type_1_DOM(moft,process_of,humn). % 07/2015
ignore_type_relation_type_1_DOM(moft,process_of,invt).
ignore_type_relation_type_1_DOM(moft,process_of,mobd).
ignore_type_relation_type_1_DOM(moft,process_of,moft).
ignore_type_relation_type_1_DOM(moft,process_of,neop).
ignore_type_relation_type_1_DOM(moft,process_of,orgf).
ignore_type_relation_type_1_DOM(moft,process_of,orgm).
ignore_type_relation_type_1_DOM(moft,process_of,patf).
%ignore_type_relation_type_1_DOM(moft,process_of,rept). % 07/2015
ignore_type_relation_type_1_DOM(moft,process_of,rich).
ignore_type_relation_type_1_DOM(moft,process_of,virs).
%ignore_type_relation_type_1_DOM(moft,process_of,vtbt). % 07/2015
ignore_type_relation_type_1_DOM(moft,produces,bacs).
ignore_type_relation_type_1_DOM(moft,produces,bdsu).
ignore_type_relation_type_1_DOM(moft,produces,enzy).
ignore_type_relation_type_1_DOM(moft,produces,horm).
ignore_type_relation_type_1_DOM(moft,produces,imft).
ignore_type_relation_type_1_DOM(moft,produces,nsba).
ignore_type_relation_type_1_DOM(moft,produces,rcpt).
ignore_type_relation_type_1_DOM(moft,produces,vita).
ignore_type_relation_type_1_DOM(moft,result_of,acab).
ignore_type_relation_type_1_DOM(moft,result_of,anab).
ignore_type_relation_type_1_DOM(moft,result_of,biof).
ignore_type_relation_type_1_DOM(moft,result_of,celf).
ignore_type_relation_type_1_DOM(moft,result_of,cgab).
ignore_type_relation_type_1_DOM(moft,result_of,comd).
ignore_type_relation_type_1_DOM(moft,result_of,dsyn).
ignore_type_relation_type_1_DOM(moft,result_of,eehu).
ignore_type_relation_type_1_DOM(moft,result_of,emod).
ignore_type_relation_type_1_DOM(moft,result_of,genf).
ignore_type_relation_type_1_DOM(moft,result_of,hcpp).
ignore_type_relation_type_1_DOM(moft,result_of,inpo).
ignore_type_relation_type_1_DOM(moft,result_of,mobd).
ignore_type_relation_type_1_DOM(moft,result_of,moft).
ignore_type_relation_type_1_DOM(moft,result_of,neop).
ignore_type_relation_type_1_DOM(moft,result_of,npop).
ignore_type_relation_type_1_DOM(moft,result_of,orgf).
ignore_type_relation_type_1_DOM(moft,result_of,ortf).
ignore_type_relation_type_1_DOM(moft,result_of,patf).
ignore_type_relation_type_1_DOM(moft,result_of,phpr).
ignore_type_relation_type_1_DOM(moft,result_of,phsf).
ignore_type_relation_type_1_DOM(mosq,result_of,menp).
%ignore_type_relation_type_1_DOM(neop,affected_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(neop,affected_by,lbpr).
ignore_type_relation_type_1_DOM(neop,affected_by,menp).
ignore_type_relation_type_1_DOM(neop,affected_by,moft).
ignore_type_relation_type_1_DOM(neop,affects,bird).
ignore_type_relation_type_1_DOM(neop,affects,mamm).
ignore_type_relation_type_1_DOM(neop,affects,menp).
ignore_type_relation_type_1_DOM(neop,affects,moft).
ignore_type_relation_type_1_DOM(neop,affects,plnt).
%ignore_type_relation_type_1_DOM(neop,caused_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(neop,caused_by,mnob).
ignore_type_relation_type_1_DOM(neop,has_evaluation,fndg).
ignore_type_relation_type_1_DOM(neop,has_location,gngm).
ignore_type_relation_type_1_DOM(neop,has_manifestation,fndg).
ignore_type_relation_type_1_DOM(neop,has_process,biof).
%ignore_type_relation_type_1_DOM(neop,has_process,celf). % Unblocked 09/2015
ignore_type_relation_type_1_DOM(neop,has_process,comd).
ignore_type_relation_type_1_DOM(neop,has_process,emod).
ignore_type_relation_type_1_DOM(neop,has_process,genf).
ignore_type_relation_type_1_DOM(neop,has_process,mobd).
ignore_type_relation_type_1_DOM(neop,has_process,neop).
ignore_type_relation_type_1_DOM(neop,has_process,npop).
ignore_type_relation_type_1_DOM(neop,has_process,orgf).
ignore_type_relation_type_1_DOM(neop,has_process,ortf).
ignore_type_relation_type_1_DOM(neop,has_process,patf).
ignore_type_relation_type_1_DOM(neop,has_process,phsf).
ignore_type_relation_type_1_DOM(neop,has_result,clna).
ignore_type_relation_type_1_DOM(neop,has_result,menp).
ignore_type_relation_type_1_DOM(neop,has_result,moft).
ignore_type_relation_type_1_DOM(neop,manifestation_of,menp).
ignore_type_relation_type_1_DOM(neop,manifestation_of,moft).
ignore_type_relation_type_1_DOM(neop,process_of,alga). % 09/16/09
%ignore_type_relation_type_1_DOM(neop,process_of,anim). % 09/16/09
ignore_type_relation_type_1_DOM(neop,process_of,arch). % 09/16/09
ignore_type_relation_type_1_DOM(neop,process_of,bact). % 09/16/09
ignore_type_relation_type_1_DOM(neop,process_of,bird).
ignore_type_relation_type_1_DOM(neop,process_of,celf).
ignore_type_relation_type_1_DOM(neop,process_of,comd).
ignore_type_relation_type_1_DOM(neop,process_of,dsyn).
%ignore_type_relation_type_1_DOM(neop,process_of,emod). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(neop,process_of,fngs). % 09/16/09
ignore_type_relation_type_1_DOM(neop,process_of,invt). % 09/16/09
ignore_type_relation_type_1_DOM(neop,process_of,neop). % GR 09/21/09
ignore_type_relation_type_1_DOM(neop,process_of,orgm). % 09/16/09
ignore_type_relation_type_1_DOM(neop,process_of,rich). % 09/16/09
ignore_type_relation_type_1_DOM(neop,process_of,virs). % 09/16/09
%ignore_type_relation_type_1_DOM(neop,process_of,mamm). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(neop,process_of,menp).
ignore_type_relation_type_1_DOM(neop,process_of,mobd).
ignore_type_relation_type_1_DOM(neop,process_of,moft).
ignore_type_relation_type_1_DOM(neop,process_of,npop).
ignore_type_relation_type_1_DOM(neop,process_of,orgf).
ignore_type_relation_type_1_DOM(neop,process_of,patf).
ignore_type_relation_type_1_DOM(neop,process_of,plnt).
ignore_type_relation_type_1_DOM(neop,result_of,menp).
ignore_type_relation_type_1_DOM(neop,result_of,moft).
ignore_type_relation_type_1_DOM(neop,result_of,socb).
ignore_type_relation_type_1_DOM(nnon,affects,menp).
ignore_type_relation_type_1_DOM(nnon,affects,moft).
ignore_type_relation_type_1_DOM(nnon,interacts_with,chvf).
ignore_type_relation_type_1_DOM(npop,affected_by,chvf).
ignore_type_relation_type_1_DOM(npop,affected_by,menp).
ignore_type_relation_type_1_DOM(npop,affected_by,moft).
ignore_type_relation_type_1_DOM(npop,affects,menp).
ignore_type_relation_type_1_DOM(npop,affects,moft).
ignore_type_relation_type_1_DOM(npop,has_process,biof). % GR 07/2015
ignore_type_relation_type_1_DOM(npop,has_process,celf). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,has_process,emod). % GR 07/2015
ignore_type_relation_type_1_DOM(npop,has_process,menp).
ignore_type_relation_type_1_DOM(npop,has_process,moft).
ignore_type_relation_type_1_DOM(npop,has_process,neop). % GR 07/2015
ignore_type_relation_type_1_DOM(npop,has_process,npop). % GR 08/2015
ignore_type_relation_type_1_DOM(npop,has_process,phsf). % GR 07/2015
ignore_type_relation_type_1_DOM(npop,has_result,clna).
ignore_type_relation_type_1_DOM(npop,has_result,menp).
ignore_type_relation_type_1_DOM(npop,has_result,moft).
ignore_type_relation_type_1_DOM(npop,inverse_isa,menp).
ignore_type_relation_type_1_DOM(npop,inverse_isa,moft).
ignore_type_relation_type_1_DOM(npop,process_of,alga). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,amph). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,arch). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,bact). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,biof).
ignore_type_relation_type_1_DOM(npop,process_of,celf).
ignore_type_relation_type_1_DOM(npop,process_of,comd).
ignore_type_relation_type_1_DOM(npop,process_of,dsyn).
ignore_type_relation_type_1_DOM(npop,process_of,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,fngs). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,genf).
ignore_type_relation_type_1_DOM(npop,process_of,invt). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,menp).
ignore_type_relation_type_1_DOM(npop,process_of,mobd).
ignore_type_relation_type_1_DOM(npop,process_of,moft).
ignore_type_relation_type_1_DOM(npop,process_of,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,npop).
ignore_type_relation_type_1_DOM(npop,process_of,orgf).
ignore_type_relation_type_1_DOM(npop,process_of,orgm). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,ortf).
ignore_type_relation_type_1_DOM(npop,process_of,patf).
ignore_type_relation_type_1_DOM(npop,result_of,phsf).
ignore_type_relation_type_1_DOM(npop,result_of,menp).
ignore_type_relation_type_1_DOM(nsba,affects,menp).
ignore_type_relation_type_1_DOM(nsba,complicates,moft).
ignore_type_relation_type_1_DOM(nsba,complicates,menp).
ignore_type_relation_type_1_DOM(nsba,disrupts,menp).
ignore_type_relation_type_1_DOM(nsba,has_location,moft).
ignore_type_relation_type_1_DOM(nsba,interacts_with,chvf). % GR 08/2015
ignore_type_relation_type_1_DOM(nsba,interacts_with,plnt).
ignore_type_relation_type_1_DOM(nsba,isa,chvf).
ignore_type_relation_type_1_DOM(nsba,produced_by,chvf).
ignore_type_relation_type_1_DOM(nsba,produced_by,menp).
ignore_type_relation_type_1_DOM(nsba,produced_by,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(nusq,part_of,moft).
ignore_type_relation_type_1_DOM(nusq,result_of,nusq).
ignore_type_relation_type_1_DOM(ocdi,has_issue,bird).
ignore_type_relation_type_1_DOM(ocdi,has_issue,chvf).
ignore_type_relation_type_1_DOM(ocdi,has_issue,clna).
ignore_type_relation_type_1_DOM(ocdi,has_issue,fndg).
ignore_type_relation_type_1_DOM(ocdi,has_issue,inpr).
ignore_type_relation_type_1_DOM(ocdi,has_issue,mamm).
ignore_type_relation_type_1_DOM(ocdi,has_issue,menp).
ignore_type_relation_type_1_DOM(ocdi,has_issue,mnob).
ignore_type_relation_type_1_DOM(ocdi,has_issue,moft).
ignore_type_relation_type_1_DOM(ocdi,has_issue,plnt).
ignore_type_relation_type_1_DOM(opco,affects,socb).
ignore_type_relation_type_1_DOM(opco,affects,menp).
ignore_type_relation_type_1_DOM(opco,affects,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(opco,interacts_with,chvf). % GR 08/2015
ignore_type_relation_type_1_DOM(opco,interacts_with,moft).
ignore_type_relation_type_1_DOM(orch,affects,chvf).
ignore_type_relation_type_1_DOM(orch,affects,menp).
ignore_type_relation_type_1_DOM(orch,affects,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(orch,interacts_with,chvf). % GR 08/2015
ignore_type_relation_type_1_DOM(orch,interacts_with,moft).
ignore_type_relation_type_1_DOM(orga,affected_by,chvf).
ignore_type_relation_type_1_DOM(orga,affected_by,menp).
ignore_type_relation_type_1_DOM(orga,degree_of,moft).
ignore_type_relation_type_1_DOM(orga,has_degree,clna).
ignore_type_relation_type_1_DOM(orga,has_evaluation,clna).
ignore_type_relation_type_1_DOM(orga,interacts_with,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(orga,inverse_isa,fndg).
ignore_type_relation_type_1_DOM(orga,manifestation_of,clna).
ignore_type_relation_type_1_DOM(orga,manifestation_of,menp).
ignore_type_relation_type_1_DOM(orga,manifestation_of,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(orga,measurement_of,moft).
ignore_type_relation_type_1_DOM(orga,measurement_of,menp).
ignore_type_relation_type_1_DOM(orga,property_of,moft).
ignore_type_relation_type_1_DOM(orga,property_of,bird).
ignore_type_relation_type_1_DOM(orga,property_of,mamm).
ignore_type_relation_type_1_DOM(orga,result_of,plnt).
ignore_type_relation_type_1_DOM(orga,result_of,menp).
%ignore_type_relation_type_1_DOM(orgf,affected_by,moft). % GR 08/2015
ignore_type_relation_type_1_DOM(orgf,affected_by,chvf).
ignore_type_relation_type_1_DOM(orgf,affected_by,lbpr).
ignore_type_relation_type_1_DOM(orgf,affected_by,menp).
ignore_type_relation_type_1_DOM(orgf,affects,bird).
ignore_type_relation_type_1_DOM(orgf,affects,clna).
ignore_type_relation_type_1_DOM(orgf,affects,mamm).
ignore_type_relation_type_1_DOM(orgf,affects,menp).
ignore_type_relation_type_1_DOM(orgf,affects,moft).
ignore_type_relation_type_1_DOM(orgf,affects,plnt). % GR 07/2015
ignore_type_relation_type_1_DOM(orgf,coexists_with,plnt).
ignore_type_relation_type_1_DOM(orgf,coexists_with,menp).
ignore_type_relation_type_1_DOM(orgf,coexists_with,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(orgf,degree_of,moft).
ignore_type_relation_type_1_DOM(orgf,follows,menp).
ignore_type_relation_type_1_DOM(orgf,follows,moft). % GR 08/2015
ignore_type_relation_type_1_DOM(orgf,has_degree,moft).
ignore_type_relation_type_1_DOM(orgf,has_evaluation,menp).
ignore_type_relation_type_1_DOM(orgf,has_location,fndg).
ignore_type_relation_type_1_DOM(orgf,has_location,blor).
ignore_type_relation_type_1_DOM(orgf,has_location,bpoc).
ignore_type_relation_type_1_DOM(orgf,has_location,bsoj).
ignore_type_relation_type_1_DOM(orgf,has_location,celc).
ignore_type_relation_type_1_DOM(orgf,has_location,cell).
ignore_type_relation_type_1_DOM(orgf,has_location,emst).
ignore_type_relation_type_1_DOM(orgf,has_location,ffas).
ignore_type_relation_type_1_DOM(orgf,has_manifestation,tisu).
ignore_type_relation_type_1_DOM(orgf,has_manifestation,clna).
ignore_type_relation_type_1_DOM(orgf,has_manifestation,fndg). % GR 08/2015
ignore_type_relation_type_1_DOM(orgf,has_measurement,fndg).
ignore_type_relation_type_1_DOM(orgf,has_occurrence,dsyn). % GR 07/2015
ignore_type_relation_type_1_DOM(orgf,has_occurrence,fndg). % GR 08/2015
ignore_type_relation_type_1_DOM(orgf,has_occurrence,phsf). % GR 07/2015
ignore_type_relation_type_1_DOM(orgf,has_process,dsyn). % GR 07/2015
ignore_type_relation_type_1_DOM(orgf,has_process,patf). % GR 07/2015
ignore_type_relation_type_1_DOM(npop,has_process,clna).
ignore_type_relation_type_1_DOM(npop,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,has_process,ortf). % GR 06/2015
ignore_type_relation_type_1_DOM(orgf,has_process,biof).
ignore_type_relation_type_1_DOM(orgf,has_process,celf).
ignore_type_relation_type_1_DOM(orgf,has_process,comd).
ignore_type_relation_type_1_DOM(npop,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(orgm,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(orgf,has_process,emod).
ignore_type_relation_type_1_DOM(npop,has_process,genf). % 06/2015
ignore_type_relation_type_1_DOM(orgf,has_process,genf). % GR 08/2015
ignore_type_relation_type_1_DOM(orgf,has_process,mobd).
ignore_type_relation_type_1_DOM(npop,has_process,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(orgf,has_process,neop).
ignore_type_relation_type_1_DOM(orgf,has_process,npop).
ignore_type_relation_type_1_DOM(orgf,has_process,orgf).
ignore_type_relation_type_1_DOM(orgf,has_process,ortf).
ignore_type_relation_type_1_DOM(orgf,has_process,phsf).
ignore_type_relation_type_1_DOM(orgf,has_result,phsf).
ignore_type_relation_type_1_DOM(orgf,has_result,clna).
ignore_type_relation_type_1_DOM(orgf,has_result,menp).
ignore_type_relation_type_1_DOM(orgf,inverse_isa,moft).
ignore_type_relation_type_1_DOM(orgf,precedes,menp).
ignore_type_relation_type_1_DOM(orgf,precedes,moft). % GR 08/2015
ignore_type_relation_type_1_DOM(orgf,process_of,moft). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,alga). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,amph). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,arch). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,bact). % GR 09/16/09
ignore_type_relation_type_1_DOM(orgf,process_of,emod). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,fngs). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,invt). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,neop). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,orgm). % GR 09/22/09 and unblocked 06/2015
ignore_type_relation_type_1_DOM(orgf,process_of,rept). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,rich). % GR 09/15/09
ignore_type_relation_type_1_DOM(orgf,process_of,virs).
ignore_type_relation_type_1_DOM(orgf,process_of,biof).
ignore_type_relation_type_1_DOM(orgf,process_of,celf).
ignore_type_relation_type_1_DOM(orgf,process_of,comd).
ignore_type_relation_type_1_DOM(orgf,process_of,dsyn).
ignore_type_relation_type_1_DOM(orgf,process_of,genf).
ignore_type_relation_type_1_DOM(orgf,process_of,menp).
ignore_type_relation_type_1_DOM(orgf,process_of,mobd).
ignore_type_relation_type_1_DOM(orgf,process_of,npop).
ignore_type_relation_type_1_DOM(orgf,process_of,orgf).
ignore_type_relation_type_1_DOM(orgf,process_of,ortf).
ignore_type_relation_type_1_DOM(orgf,process_of,patf).
ignore_type_relation_type_1_DOM(orgf,process_of,phsf).
ignore_type_relation_type_1_DOM(orgf,result_of,plnt).
ignore_type_relation_type_1_DOM(orgf,result_of,menp).
ignore_type_relation_type_1_DOM(orgm,affected_by,moft).
ignore_type_relation_type_1_DOM(orgm,affected_by,menp).
ignore_type_relation_type_1_DOM(orgm,has_process,moft).
ignore_type_relation_type_1_DOM(orgm,has_process,menp).
ignore_type_relation_type_1_DOM(orgm,has_process,biof). % GR 07/2015
ignore_type_relation_type_1_DOM(orgm,has_process,dsyn). % GR 07/2015
ignore_type_relation_type_1_DOM(orgm,has_process,orgf). % GR 07/2015
ignore_type_relation_type_1_DOM(orgm,has_process,ortf). % GR 07/2015
ignore_type_relation_type_1_DOM(orgm,has_process,phsf). % GR 07/2015
ignore_type_relation_type_1_DOM(orgm,has_property,moft).
ignore_type_relation_type_1_DOM(orgm,interacts_with,clna).
ignore_type_relation_type_1_DOM(orgm,interacts_with,bird).
ignore_type_relation_type_1_DOM(orgm,interacts_with,humn).
ignore_type_relation_type_1_DOM(orgm,interacts_with,mamm).
ignore_type_relation_type_1_DOM(orgm,interacts_with,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(orgm,inverse_isa,plnt).
ignore_type_relation_type_1_DOM(orgm,inverse_isa,bird).
ignore_type_relation_type_1_DOM(orgm,inverse_isa,mamm).
ignore_type_relation_type_1_DOM(orgt,location_of,plnt).
ignore_type_relation_type_1_DOM(orgt,location_of,gora).
ignore_type_relation_type_1_DOM(orgt,location_of,ocac).
ignore_type_relation_type_1_DOM(orgt,produces,inpr). % 07/2015
ignore_type_relation_type_1_DOM(orgt,produces,topp).
ignore_type_relation_type_1_DOM(ortf,affected_by,inpr).
ignore_type_relation_type_1_DOM(ortf,affected_by,chvf).
ignore_type_relation_type_1_DOM(ortf,affected_by,lbpr).
ignore_type_relation_type_1_DOM(ortf,affects,moft).
ignore_type_relation_type_1_DOM(ortf,affects,bird).
ignore_type_relation_type_1_DOM(ortf,affects,clna).
ignore_type_relation_type_1_DOM(ortf,affects,mamm).
ignore_type_relation_type_1_DOM(ortf,affects,menp).
ignore_type_relation_type_1_DOM(ortf,affects,plnt). % 07/2015
ignore_type_relation_type_1_DOM(ortf,coexists_with,plnt).
ignore_type_relation_type_1_DOM(ortf,coexists_with,menp).
ignore_type_relation_type_1_DOM(ortf,coexists_with,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(ortf,follows,moft).
ignore_type_relation_type_1_DOM(ortf,follows,menp).
ignore_type_relation_type_1_DOM(ortf,has_evaluation,moft).
ignore_type_relation_type_1_DOM(ortf,has_location,fndg).
ignore_type_relation_type_1_DOM(ortf,has_location,blor).
ignore_type_relation_type_1_DOM(ortf,has_location,bpoc).
ignore_type_relation_type_1_DOM(ortf,has_location,bsoj).
ignore_type_relation_type_1_DOM(ortf,has_location,celc).
ignore_type_relation_type_1_DOM(ortf,has_location,cell).
ignore_type_relation_type_1_DOM(ortf,has_location,emst).
ignore_type_relation_type_1_DOM(ortf,has_location,ffas).
ignore_type_relation_type_1_DOM(ortf,has_manifestation,fndg). % GR 08/2015
ignore_type_relation_type_1_DOM(ortf,has_manifestation,tisu).
ignore_type_relation_type_1_DOM(ortf,has_manifestation,clna).
ignore_type_relation_type_1_DOM(ortf,has_measurement,fndg).
ignore_type_relation_type_1_DOM(ortf,has_process,clna). % 06/2015
ignore_type_relation_type_1_DOM(ortf,has_process,celf).
ignore_type_relation_type_1_DOM(ortf,has_process,emod). % GR 07/2015
ignore_type_relation_type_1_DOM(ortf,has_process,menp).
ignore_type_relation_type_1_DOM(ortf,has_process,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(ortf,has_process,npop). % GR 07/2015
ignore_type_relation_type_1_DOM(ortf,has_process,ortf). % GR 07/2015
ignore_type_relation_type_1_DOM(ortf,has_result,moft).
ignore_type_relation_type_1_DOM(ortf,has_result,clna).
ignore_type_relation_type_1_DOM(ortf,has_result,menp).
ignore_type_relation_type_1_DOM(ortf,occurs_in,moft).
ignore_type_relation_type_1_DOM(ortf,precedes,menp).
ignore_type_relation_type_1_DOM(ortf,precedes,moft). % GR 08/2015
ignore_type_relation_type_1_DOM(ortf,process_of,moft). % 09/14/09
ignore_type_relation_type_1_DOM(ortf,process_of,alga). % 09/14/09
ignore_type_relation_type_1_DOM(ortf,process_of,amph). % 09/22/09 and unblocked 06/2015
%ignore_type_relation_type_1_DOM(ortf,process_of,anim). % 09/14/09
ignore_type_relation_type_1_DOM(ortf,process_of,arch). % 09/14/09
ignore_type_relation_type_1_DOM(ortf,process_of,bact). % 09/16/09
%ignore_type_relation_type_1_DOM(ortf,process_of,emod). % 09/14/09 and unblocked 06/2015
%ignore_type_relation_type_1_DOM(ortf,process_of,fish). % 09/14/09 but it needs to be allowed 07/2015
ignore_type_relation_type_1_DOM(ortf,process_of,fngs). % 09/14/09
ignore_type_relation_type_1_DOM(ortf,process_of,invt).
ignore_type_relation_type_1_DOM(ortf,process_of,neop). % 09/22/09
ignore_type_relation_type_1_DOM(ortf,process_of,orgm). % 09/22/09 and unblocked 06/2015
%ignore_type_relation_type_1_DOM(ortf,process_of,rept). % 09/14/09
ignore_type_relation_type_1_DOM(ortf,process_of,rich). % 09/14/09
%ignore_type_relation_type_1_DOM(ortf,process_of,vtbt). % 09/14/09
ignore_type_relation_type_1_DOM(ortf,process_of,virs).
ignore_type_relation_type_1_DOM(ortf,process_of,biof). % unblocked 06/2015
%ignore_type_relation_type_1_DOM(ortf,process_of,bird).
ignore_type_relation_type_1_DOM(ortf,process_of,celf).
ignore_type_relation_type_1_DOM(ortf,process_of,comd).
ignore_type_relation_type_1_DOM(ortf,process_of,dsyn).
ignore_type_relation_type_1_DOM(ortf,process_of,genf).
ignore_type_relation_type_1_DOM(ortf,process_of,menp).
ignore_type_relation_type_1_DOM(ortf,process_of,mobd).
ignore_type_relation_type_1_DOM(ortf,process_of,npop).
ignore_type_relation_type_1_DOM(ortf,process_of,orgf).
ignore_type_relation_type_1_DOM(ortf,process_of,ortf).
ignore_type_relation_type_1_DOM(ortf,process_of,patf).
ignore_type_relation_type_1_DOM(ortf,process_of,phsf).
ignore_type_relation_type_1_DOM(ortf,result_of,plnt).
ignore_type_relation_type_1_DOM(ortf,result_of,menp).
ignore_type_relation_type_1_DOM(patf,affected_by,moft).
%ignore_type_relation_type_1_DOM(patf,affected_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(patf,affected_by,lbpr).
ignore_type_relation_type_1_DOM(patf,affected_by,menp).
ignore_type_relation_type_1_DOM(patf,affects,bird).
ignore_type_relation_type_1_DOM(patf,affects,mamm).
ignore_type_relation_type_1_DOM(patf,affects,menp).
ignore_type_relation_type_1_DOM(patf,affects,moft).
ignore_type_relation_type_1_DOM(patf,affects,plnt). % GR 07/2015
ignore_type_relation_type_1_DOM(patf,caused_by,plnt).
%ignore_type_relation_type_1_DOM(patf,caused_by,chvf). % Aurelie
ignore_type_relation_type_1_DOM(patf,caused_by,mnob). % GR 07/2015
ignore_type_relation_type_1_DOM(patf,has_evaluation,mnob).
ignore_type_relation_type_1_DOM(patf,has_location,fndg).
ignore_type_relation_type_1_DOM(patf,has_location,acab).
ignore_type_relation_type_1_DOM(patf,has_location,anab).
ignore_type_relation_type_1_DOM(patf,has_location,blor).
ignore_type_relation_type_1_DOM(patf,has_location,bsoj).
ignore_type_relation_type_1_DOM(patf,has_location,celc).
ignore_type_relation_type_1_DOM(patf,has_location,cell).
ignore_type_relation_type_1_DOM(patf,has_location,cgab).
ignore_type_relation_type_1_DOM(patf,has_location,emst).
ignore_type_relation_type_1_DOM(patf,has_location,ffas).
ignore_type_relation_type_1_DOM(patf,has_manifestation,fndg). % GR 08/2015
ignore_type_relation_type_1_DOM(patf,has_manifestation,tisu).
ignore_type_relation_type_1_DOM(orgm,has_process,fndg). % GR 06/2015
ignore_type_relation_type_1_DOM(ortf,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,biof).
ignore_type_relation_type_1_DOM(patf,has_process,celf).
ignore_type_relation_type_1_DOM(ortf,has_process,comd). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,comd). % GR 07/2015
ignore_type_relation_type_1_DOM(patf,has_process,emod).
ignore_type_relation_type_1_DOM(orgm,has_process,genf). % 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,genf). % GR 07/2015
ignore_type_relation_type_1_DOM(ortf,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,mobd).
ignore_type_relation_type_1_DOM(orgm,has_process,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(ortf,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,neop).
ignore_type_relation_type_1_DOM(orgm,has_process,neop).
ignore_type_relation_type_1_DOM(orgm,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(ortf,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,orgf).
ignore_type_relation_type_1_DOM(patf,has_process,ortf).
ignore_type_relation_type_1_DOM(orgm,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(ortf,has_process,phsf). % 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,phsf).
ignore_type_relation_type_1_DOM(patf,has_result,phsf).
ignore_type_relation_type_1_DOM(patf,has_result,clna).
ignore_type_relation_type_1_DOM(patf,has_result,menp).
ignore_type_relation_type_1_DOM(patf,diagnosed_by,sosy). % 07/2015
ignore_type_relation_type_1_DOM(patf,diagnosed_by,patf). % 07/2015
ignore_type_relation_type_1_DOM(patf,manifestation_of,moft).
ignore_type_relation_type_1_DOM(patf,manifestation_of,menp).
ignore_type_relation_type_1_DOM(patf,process_of,moft). % 09/22/09
ignore_type_relation_type_1_DOM(patf,process_of,alga).
%ignore_type_relation_type_1_DOM(patf,process_of,amph). % GR commented it out 07/2015
ignore_type_relation_type_1_DOM(patf,process_of,arch).
ignore_type_relation_type_1_DOM(patf,process_of,bact).
%ignore_type_relation_type_1_DOM(patf,process_of,emod). % 09/22/09
ignore_type_relation_type_1_DOM(patf,process_of,fngs). % 09/22/09
ignore_type_relation_type_1_DOM(patf,process_of,invt). % 09/11/09
ignore_type_relation_type_1_DOM(patf,process_of,neop). % 09/22/09
ignore_type_relation_type_1_DOM(patf,process_of,orgm). % 09/22/09
ignore_type_relation_type_1_DOM(patf,process_of,rich). % 09/22/09
ignore_type_relation_type_1_DOM(patf,process_of,virs).
ignore_type_relation_type_1_DOM(patf,process_of,biof).
ignore_type_relation_type_1_DOM(patf,process_of,celf).
ignore_type_relation_type_1_DOM(patf,process_of,comd).
ignore_type_relation_type_1_DOM(patf,process_of,dsyn).
ignore_type_relation_type_1_DOM(patf,process_of,genf).
ignore_type_relation_type_1_DOM(patf,process_of,menp).
ignore_type_relation_type_1_DOM(patf,process_of,mobd).
ignore_type_relation_type_1_DOM(patf,process_of,npop).
ignore_type_relation_type_1_DOM(patf,process_of,ortf).
ignore_type_relation_type_1_DOM(patf,result_of,plnt).
ignore_type_relation_type_1_DOM(patf,result_of,menp).
ignore_type_relation_type_1_DOM(patf,result_of,moft).
ignore_type_relation_type_1_DOM(phob,inverse_isa,socb).
ignore_type_relation_type_1_DOM(phob,inverse_isa,bird).
ignore_type_relation_type_1_DOM(phob,inverse_isa,chvf).
ignore_type_relation_type_1_DOM(phob,inverse_isa,mamm).
ignore_type_relation_type_1_DOM(phob,inverse_isa,mnob).
ignore_type_relation_type_1_DOM(phpr,has_result,plnt).
ignore_type_relation_type_1_DOM(phpr,has_result,clna).
ignore_type_relation_type_1_DOM(phpr,has_result,menp).
ignore_type_relation_type_1_DOM(phpr,inverse_isa,moft).
ignore_type_relation_type_1_DOM(phpr,inverse_isa,menp).
ignore_type_relation_type_1_DOM(phpr,result_of,moft).
ignore_type_relation_type_1_DOM(phpr,result_of,menp).
ignore_type_relation_type_1_DOM(phsf,affected_by,moft).
ignore_type_relation_type_1_DOM(phsf,affected_by,chvf).
ignore_type_relation_type_1_DOM(phsf,affected_by,lbpr).
ignore_type_relation_type_1_DOM(phsf,affected_by,menp).
ignore_type_relation_type_1_DOM(phsf,affects,bird).
ignore_type_relation_type_1_DOM(phsf,affects,clna).
ignore_type_relation_type_1_DOM(phsf,affects,mamm).
ignore_type_relation_type_1_DOM(phsf,affects,menp).
ignore_type_relation_type_1_DOM(phsf,affects,moft).
ignore_type_relation_type_1_DOM(phsf,coexists_with,plnt).
ignore_type_relation_type_1_DOM(phsf,coexists_with,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(phsf,coexists_with,menp).
ignore_type_relation_type_1_DOM(phsf,follows,moft).
ignore_type_relation_type_1_DOM(phsf,follows,menp).
ignore_type_relation_type_1_DOM(phsf,has_evaluation,moft).
ignore_type_relation_type_1_DOM(phsf,has_location,fndg).
ignore_type_relation_type_1_DOM(phsf,has_location,blor).
ignore_type_relation_type_1_DOM(phsf,has_location,bpoc).
ignore_type_relation_type_1_DOM(phsf,has_location,bsoj).
ignore_type_relation_type_1_DOM(phsf,has_location,celc).
ignore_type_relation_type_1_DOM(phsf,has_location,cell).
ignore_type_relation_type_1_DOM(phsf,has_location,emst).
ignore_type_relation_type_1_DOM(phsf,has_location,ffas).
ignore_type_relation_type_1_DOM(phsf,has_manifestation,fndg). % GR 08/2015
ignore_type_relation_type_1_DOM(phsf,has_manifestation,tisu).
ignore_type_relation_type_1_DOM(phsf,has_manifestation,clna).
ignore_type_relation_type_1_DOM(phsf,has_measurement,fndg).
ignore_type_relation_type_1_DOM(phsf,has_part,plnt). % 07/2015
ignore_type_relation_type_1_DOM(phsf,has_process,clna).
ignore_type_relation_type_1_DOM(phsf,has_process,biof).
ignore_type_relation_type_1_DOM(phsf,has_process,celf).
ignore_type_relation_type_1_DOM(phsf,has_process,comd).
ignore_type_relation_type_1_DOM(phsf,has_process,dsyn).
ignore_type_relation_type_1_DOM(phsf,has_process,genf).
ignore_type_relation_type_1_DOM(phsf,has_process,mobd).
ignore_type_relation_type_1_DOM(phsf,has_process,neop). % 07/2015
ignore_type_relation_type_1_DOM(phsf,has_process,orgf).
ignore_type_relation_type_1_DOM(phsf,has_process,ortf).
ignore_type_relation_type_1_DOM(phsf,has_process,patf).
ignore_type_relation_type_1_DOM(phsf,has_process,phsf).
ignore_type_relation_type_1_DOM(phsf,has_result,phsf).
ignore_type_relation_type_1_DOM(phsf,has_result,clna).
ignore_type_relation_type_1_DOM(phsf,has_result,menp).
ignore_type_relation_type_1_DOM(phsf,inverse_isa,moft).
ignore_type_relation_type_1_DOM(phsf,inverse_isa,menp).
ignore_type_relation_type_1_DOM(phsf,part_of,plnt). % Gr 07/2015
ignore_type_relation_type_1_DOM(phsf,precedes,moft).
ignore_type_relation_type_1_DOM(phsf,precedes,menp).
ignore_type_relation_type_1_DOM(phsf,process_of,moft). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,alga). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,amph). % 09/22/09 and unblocked 06/2015
%ignore_type_relation_type_1_DOM(phsf,process_of,anim). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,arch). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,bact). % 09/11/09
%ignore_type_relation_type_1_DOM(phsf,process_of,emod). % 09/15/09 and unblocked 06/2015
%ignore_type_relation_type_1_DOM(phsf,process_of,fish). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,fngs). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,invt). % Unblocked 09/15/09
%ignore_type_relation_type_1_DOM(phsf,process_of,mamm). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,neop). % 09/22/09
ignore_type_relation_type_1_DOM(phsf,process_of,orgm). % 09/15/09 & unblocked 06/2015
ignore_type_relation_type_1_DOM(phsf,process_of,rept). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,rich). % 09/15/09
ignore_type_relation_type_1_DOM(phsf,process_of,virs). % 09/15/09 and unblocked 06/2015
%ignore_type_relation_type_1_DOM(phsf,process_of,vtbt).
%ignore_type_relation_type_1_DOM(phsf,process_of,bird). % Unblocked 06/2015
ignore_type_relation_type_1_DOM(phsf,process_of,biof).
ignore_type_relation_type_1_DOM(phsf,process_of,celf).
ignore_type_relation_type_1_DOM(phsf,process_of,comd).
ignore_type_relation_type_1_DOM(phsf,process_of,dsyn).
ignore_type_relation_type_1_DOM(phsf,process_of,genf).
ignore_type_relation_type_1_DOM(phsf,process_of,menp).
ignore_type_relation_type_1_DOM(phsf,process_of,mobd).
ignore_type_relation_type_1_DOM(phsf,process_of,npop).
ignore_type_relation_type_1_DOM(phsf,process_of,orgf).
ignore_type_relation_type_1_DOM(phsf,process_of,ortf).
ignore_type_relation_type_1_DOM(phsf,process_of,patf).
ignore_type_relation_type_1_DOM(phsf,process_of,phsf).
ignore_type_relation_type_1_DOM(phsf,result_of,plnt).
ignore_type_relation_type_1_DOM(phsf,result_of,menp).
%ignore_type_relation_type_1_DOM(phsu,affects,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(phsu,affects,menp).
ignore_type_relation_type_1_DOM(phsu,complicates,moft).
ignore_type_relation_type_1_DOM(phsu,complicates,menp).
ignore_type_relation_type_1_DOM(phsu,disrupts,menp).
ignore_type_relation_type_1_DOM(phsu,interacts_with,chvf). % GR 08/2015
ignore_type_relation_type_1_DOM(phsu,interacts_with,moft).
ignore_type_relation_type_1_DOM(phsu,isa,chvf).
ignore_type_relation_type_1_DOM(plnt,affected_by,chvf).
ignore_type_relation_type_1_DOM(plnt,affected_by,acab).
ignore_type_relation_type_1_DOM(plnt,affected_by,anab).
ignore_type_relation_type_1_DOM(plnt,affected_by,biof).
ignore_type_relation_type_1_DOM(plnt,affected_by,celf).
ignore_type_relation_type_1_DOM(plnt,affected_by,cgab).
ignore_type_relation_type_1_DOM(plnt,affected_by,comd).
ignore_type_relation_type_1_DOM(plnt,affected_by,dsyn).
ignore_type_relation_type_1_DOM(plnt,affected_by,emod).
ignore_type_relation_type_1_DOM(plnt,affected_by,genf).
ignore_type_relation_type_1_DOM(plnt,affected_by,mobd).
ignore_type_relation_type_1_DOM(plnt,affected_by,neop).
ignore_type_relation_type_1_DOM(plnt,affected_by,orgf).
ignore_type_relation_type_1_DOM(plnt,affected_by,ortf).
ignore_type_relation_type_1_DOM(plnt,affected_by,patf).
ignore_type_relation_type_1_DOM(plnt,causes,patf). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,coexists_with,orgf). % GR 07/2015
ignore_type_relation_type_1_DOM(plnt,coexists_with,ortf). % GR 07/2015
ignore_type_relation_type_1_DOM(plnt,has_part,phsf).
ignore_type_relation_type_1_DOM(plnt,has_part,acab).
ignore_type_relation_type_1_DOM(plnt,has_part,anab).
ignore_type_relation_type_1_DOM(plnt,has_part,anst).
ignore_type_relation_type_1_DOM(plnt,has_part,bpoc).
ignore_type_relation_type_1_DOM(plnt,has_part,celc).
ignore_type_relation_type_1_DOM(plnt,has_part,cell).
ignore_type_relation_type_1_DOM(plnt,has_part,cgab).
ignore_type_relation_type_1_DOM(plnt,has_part,emst).
ignore_type_relation_type_1_DOM(plnt,has_part,ffas).
ignore_type_relation_type_1_DOM(plnt,has_part,gngm).
ignore_type_relation_type_1_DOM(plnt,has_process,tisu).
ignore_type_relation_type_1_DOM(plnt,has_process,biof).
ignore_type_relation_type_1_DOM(plnt,has_process,celf).
ignore_type_relation_type_1_DOM(plnt,has_process,comd).
%ignore_type_relation_type_1_DOM(plnt,has_process,dsyn).
ignore_type_relation_type_1_DOM(plnt,has_process,emod).
ignore_type_relation_type_1_DOM(plnt,has_process,genf).
ignore_type_relation_type_1_DOM(plnt,has_process,menp). % Gr 07/2015
ignore_type_relation_type_1_DOM(plnt,has_process,neop).
ignore_type_relation_type_1_DOM(plnt,has_process,orgf).
ignore_type_relation_type_1_DOM(plnt,has_process,ortf).
ignore_type_relation_type_1_DOM(plnt,has_property,phsf).
ignore_type_relation_type_1_DOM(plnt,interacts_with,orga).
ignore_type_relation_type_1_DOM(plnt,interacts_with,alga).
ignore_type_relation_type_1_DOM(plnt,interacts_with,amph).
ignore_type_relation_type_1_DOM(plnt,interacts_with,anim).
ignore_type_relation_type_1_DOM(plnt,interacts_with,arch).
ignore_type_relation_type_1_DOM(plnt,interacts_with,bact).
ignore_type_relation_type_1_DOM(plnt,interacts_with,fish).
ignore_type_relation_type_1_DOM(plnt,interacts_with,fngs).
ignore_type_relation_type_1_DOM(plnt,interacts_with,invt).
ignore_type_relation_type_1_DOM(plnt,interacts_with,nsba). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,interacts_with,orgm).
ignore_type_relation_type_1_DOM(plnt,interacts_with,plnt).
ignore_type_relation_type_1_DOM(plnt,interacts_with,rcpt). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,interacts_with,rept).
ignore_type_relation_type_1_DOM(plnt,interacts_with,rich).
ignore_type_relation_type_1_DOM(plnt,interacts_with,virs).
ignore_type_relation_type_1_DOM(plnt,interacts_with,vita). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,inverse_isa,vtbt).
ignore_type_relation_type_1_DOM(plnt,isa,alga).
ignore_type_relation_type_1_DOM(plnt,isa,enty).
ignore_type_relation_type_1_DOM(plnt,isa,orgm).
ignore_type_relation_type_1_DOM(plnt,issue_in,phob).
ignore_type_relation_type_1_DOM(plnt,issue_in,bmod).
ignore_type_relation_type_1_DOM(plnt,location_of,ocdi).
ignore_type_relation_type_1_DOM(plnt,location_of,bacs).
ignore_type_relation_type_1_DOM(plnt,location_of,enzy).
ignore_type_relation_type_1_DOM(plnt,location_of,horm).
ignore_type_relation_type_1_DOM(plnt,location_of,imft).
ignore_type_relation_type_1_DOM(plnt,location_of,nsba).
ignore_type_relation_type_1_DOM(plnt,location_of,rcpt).
ignore_type_relation_type_1_DOM(plnt,part_of,phsf). % Gr 07/2015
ignore_type_relation_type_1_DOM(podg,exhibits,vita).
ignore_type_relation_type_1_DOM(podg,performs,socb).
ignore_type_relation_type_1_DOM(podg,produces,socb).
ignore_type_relation_type_1_DOM(podg,produces,inpr).
ignore_type_relation_type_1_DOM(podg,produces,mnob). % GR 07/2015
ignore_type_relation_type_1_DOM(podg,uses,mnob). % GR 06/2015
ignore_type_relation_type_1_DOM(podg,uses,clas).
ignore_type_relation_type_1_DOM(podg,uses,inpr).
ignore_type_relation_type_1_DOM(popg,exhibits,mnob).
ignore_type_relation_type_1_DOM(popg,performs,socb).
ignore_type_relation_type_1_DOM(popg,produces,socb).
ignore_type_relation_type_1_DOM(popg,produces,inpr).
ignore_type_relation_type_1_DOM(popg,produces,mnob). % GR 07/2015
ignore_type_relation_type_1_DOM(popg,uses,mnob).
ignore_type_relation_type_1_DOM(popg,uses,inpr).
ignore_type_relation_type_1_DOM(prog,coexists_with,sosy). % Gr 07/2015
ignore_type_relation_type_1_DOM(prog,exhibits,mnob).
ignore_type_relation_type_1_DOM(prog,performs,socb).
ignore_type_relation_type_1_DOM(prog,produces,socb).
ignore_type_relation_type_1_DOM(prog,produces,inpr).
ignore_type_relation_type_1_DOM(prog,produces,mnob). % GR 07/2015
ignore_type_relation_type_1_DOM(prog,uses,mnob).
ignore_type_relation_type_1_DOM(prog,uses,inpr).
ignore_type_relation_type_1_DOM(pros,location_of,mnob).
%ignore_type_relation_type_1_DOM(pros,location_of,edac). % GR 08/2015
ignore_type_relation_type_1_DOM(pros,location_of,gora).
%ignore_type_relation_type_1_DOM(pros,location_of,ocac). % GR 08/2015
%ignore_type_relation_type_1_DOM(pros,location_of,resa). % GR 08/2015
ignore_type_relation_type_1_DOM(pros,produces,topp).
ignore_type_relation_type_1_DOM(pros,produces,mnob). % GR 07/2015
ignore_type_relation_type_1_DOM(qlco,evaluation_of,inpr).
ignore_type_relation_type_1_DOM(qlco,result_of,socb).
ignore_type_relation_type_1_DOM(qnco,measurement_of,menp).
ignore_type_relation_type_1_DOM(qnco,result_of,moft).
ignore_type_relation_type_1_DOM(rcpt,affects,menp).
ignore_type_relation_type_1_DOM(rcpt,complicates,moft).
ignore_type_relation_type_1_DOM(rcpt,complicates,menp).
ignore_type_relation_type_1_DOM(rcpt,disrupts,menp).
ignore_type_relation_type_1_DOM(rcpt,has_location,moft).
ignore_type_relation_type_1_DOM(rcpt,interacts_with,chvf). % GR 08/2015
ignore_type_relation_type_1_DOM(rcpt,interacts_with,plnt).
ignore_type_relation_type_1_DOM(rcpt,isa,chvf).
ignore_type_relation_type_1_DOM(rcpt,produced_by,chvf).
ignore_type_relation_type_1_DOM(rcpt,produced_by,menp).
ignore_type_relation_type_1_DOM(rcpt,produced_by,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(rept,affected_by,moft).
ignore_type_relation_type_1_DOM(rept,affected_by,menp).
ignore_type_relation_type_1_DOM(rept,exhibits,moft).
ignore_type_relation_type_1_DOM(rept,has_process,socb).
ignore_type_relation_type_1_DOM(rept,has_process,emod). % GR 07/2015
ignore_type_relation_type_1_DOM(rept,has_process,menp).
ignore_type_relation_type_1_DOM(rept,has_process,orgf). % GR 07/2015
ignore_type_relation_type_1_DOM(rept,has_process,phsf). % GR 07/2015
ignore_type_relation_type_1_DOM(rept,has_property,moft).
ignore_type_relation_type_1_DOM(rept,interacts_with,clna).
ignore_type_relation_type_1_DOM(rept,interacts_with,bird).
ignore_type_relation_type_1_DOM(rept,interacts_with,humn).
ignore_type_relation_type_1_DOM(rept,interacts_with,mamm).
ignore_type_relation_type_1_DOM(rept,interacts_with,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(resa,affects,menp).
ignore_type_relation_type_1_DOM(resa,affects,plnt).
ignore_type_relation_type_1_DOM(resa,affects,socb). % GR 08/2015
ignore_type_relation_type_1_DOM(resa,measures,menp).
ignore_type_relation_type_1_DOM(resa,measures,chvf).
ignore_type_relation_type_1_DOM(resa,measures,clna).
ignore_type_relation_type_1_DOM(resd,isa,moft).
ignore_type_relation_type_1_DOM(rich,affected_by,mnob).
ignore_type_relation_type_1_DOM(rich,affected_by,menp).
ignore_type_relation_type_1_DOM(rich,has_location,moft).
ignore_type_relation_type_1_DOM(rich,has_process,gngm).
ignore_type_relation_type_1_DOM(rich,has_process,dsyn). % GR 07/2015
ignore_type_relation_type_1_DOM(rich,has_process,menp). % GR 07/2015
ignore_type_relation_type_1_DOM(rich,has_process,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(rich,has_process,ortf). % GR 07/2015
ignore_type_relation_type_1_DOM(rich,has_process,patf). % GR 07/2015
ignore_type_relation_type_1_DOM(rich,has_process,phsf). % GR 07/2015
ignore_type_relation_type_1_DOM(rich,has_property,moft).
ignore_type_relation_type_1_DOM(rich,interacts_with,clna).
ignore_type_relation_type_1_DOM(rich,interacts_with,bird).
ignore_type_relation_type_1_DOM(rich,interacts_with,humn).
ignore_type_relation_type_1_DOM(rich,interacts_with,mamm).
ignore_type_relation_type_1_DOM(rich,interacts_with,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(rnlw,isa,plnt).
ignore_type_relation_type_1_DOM(sbst,inverse_isa,inpr).
ignore_type_relation_type_1_DOM(shro,location_of,chvf).
ignore_type_relation_type_1_DOM(shro,location_of,gora).
ignore_type_relation_type_1_DOM(shro,location_of,ocac).
ignore_type_relation_type_1_DOM(shro,produces,inpr). % 07/2015
ignore_type_relation_type_1_DOM(shro,produces,topp).
ignore_type_relation_type_1_DOM(socb,affected_by,inpr).
ignore_type_relation_type_1_DOM(socb,affected_by,bhvr).
ignore_type_relation_type_1_DOM(socb,affected_by,inbe).
ignore_type_relation_type_1_DOM(socb,affected_by,mobd).
ignore_type_relation_type_1_DOM(socb,affected_by,opco). % GR 08/2015
ignore_type_relation_type_1_DOM(socb,affects,socb).
ignore_type_relation_type_1_DOM(socb,affects,bhvr).
ignore_type_relation_type_1_DOM(socb,affects,inbe).
ignore_type_relation_type_1_DOM(socb,conceptual_part_of,socb).
ignore_type_relation_type_1_DOM(socb,exhibited_by,socb).
ignore_type_relation_type_1_DOM(socb,exhibited_by,aggp).
ignore_type_relation_type_1_DOM(socb,exhibited_by,amph).
ignore_type_relation_type_1_DOM(socb,exhibited_by,anim).
ignore_type_relation_type_1_DOM(socb,exhibited_by,famg).
ignore_type_relation_type_1_DOM(socb,exhibited_by,fish).
ignore_type_relation_type_1_DOM(socb,exhibited_by,grup).
ignore_type_relation_type_1_DOM(socb,exhibited_by,humn).
ignore_type_relation_type_1_DOM(socb,exhibited_by,invt).
ignore_type_relation_type_1_DOM(socb,exhibited_by,podg).
ignore_type_relation_type_1_DOM(socb,exhibited_by,popg).
ignore_type_relation_type_1_DOM(socb,exhibited_by,prog).
ignore_type_relation_type_1_DOM(socb,exhibited_by,rept).
ignore_type_relation_type_1_DOM(socb,has_conceptual_part,vtbt).
ignore_type_relation_type_1_DOM(socb,has_evaluation,socb).
ignore_type_relation_type_1_DOM(socb,has_process,inbe). % GR 07/2015
ignore_type_relation_type_1_DOM(socb,has_process,qlco).
ignore_type_relation_type_1_DOM(socb,has_result,inbe).
ignore_type_relation_type_1_DOM(socb,has_result,acab).
ignore_type_relation_type_1_DOM(socb,has_result,comd).
ignore_type_relation_type_1_DOM(socb,has_result,dsyn).
ignore_type_relation_type_1_DOM(socb,has_result,emod).
ignore_type_relation_type_1_DOM(socb,has_result,inpo).
ignore_type_relation_type_1_DOM(socb,has_result,mobd).
ignore_type_relation_type_1_DOM(socb,has_result,neop).
ignore_type_relation_type_1_DOM(socb,isa,patf).
ignore_type_relation_type_1_DOM(socb,isa,acty).
ignore_type_relation_type_1_DOM(socb,isa,bhvr).
ignore_type_relation_type_1_DOM(socb,issue_in,evnt).
ignore_type_relation_type_1_DOM(socb,issue_in,bmod).
ignore_type_relation_type_1_DOM(socb,manifestation_of,ocdi).
ignore_type_relation_type_1_DOM(socb,manifestation_of,mobd). % GR 07/2015
ignore_type_relation_type_1_DOM(socb,performed_by,mobd).
ignore_type_relation_type_1_DOM(socb,performed_by,aggp).
ignore_type_relation_type_1_DOM(socb,performed_by,famg).
ignore_type_relation_type_1_DOM(socb,performed_by,grup).
ignore_type_relation_type_1_DOM(socb,performed_by,podg).
ignore_type_relation_type_1_DOM(socb,performed_by,popg).
ignore_type_relation_type_1_DOM(socb,produced_by,podg). % GR 07/2015
ignore_type_relation_type_1_DOM(socb,produced_by,prog). % GR 07/2015
ignore_type_relation_type_1_DOM(socb,produced_by,popg). % GR 07/2015
ignore_type_relation_type_1_DOM(sosy,coexists_with,prog).
ignore_type_relation_type_1_DOM(sosy,evaluation_of,fndg).
ignore_type_relation_type_1_DOM(sosy,evaluation_of,clna).
ignore_type_relation_type_1_DOM(sosy,evaluation_of,menp).
ignore_type_relation_type_1_DOM(sosy,isa,moft).
ignore_type_relation_type_1_DOM(sosy,manifestation_of,fndg).
ignore_type_relation_type_1_DOM(sosy,manifestation_of,menp).
ignore_type_relation_type_1_DOM(sosy,manifestation_of,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(spco,result_of,moft).
ignore_type_relation_type_1_DOM(strd,affects,menp).
%ignore_type_relation_type_1_DOM(strd,affects,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(strd,interacts_with,chvf). % GR 08/2015
ignore_type_relation_type_1_DOM(strd,interacts_with,moft).
ignore_type_relation_type_1_DOM(tisu,location_of,chvf).
ignore_type_relation_type_1_DOM(tisu,location_of,anab).
ignore_type_relation_type_1_DOM(tisu,location_of,biof).
ignore_type_relation_type_1_DOM(tisu,location_of,celf).
ignore_type_relation_type_1_DOM(tisu,location_of,cgab).
ignore_type_relation_type_1_DOM(tisu,location_of,comd).
%ignore_type_relation_type_1_DOM(tisu,location_of,dsyn). % GR 08/2015
ignore_type_relation_type_1_DOM(tisu,location_of,genf).
%ignore_type_relation_type_1_DOM(tisu,location_of,inpo). % GR 08/2015
ignore_type_relation_type_1_DOM(tisu,location_of,menp).
ignore_type_relation_type_1_DOM(tisu,location_of,mobd).
ignore_type_relation_type_1_DOM(tisu,location_of,moft).
ignore_type_relation_type_1_DOM(tisu,location_of,orgf).
ignore_type_relation_type_1_DOM(tisu,location_of,ortf).
ignore_type_relation_type_1_DOM(tisu,location_of,patf).
ignore_type_relation_type_1_DOM(tisu,location_of,phsf).
ignore_type_relation_type_1_DOM(tisu,manifestation_of,orgf). % GR 07/2015
ignore_type_relation_type_1_DOM(tisu,manifestation_of,ortf). % GR 07/2015
ignore_type_relation_type_1_DOM(tisu,manifestation_of,patf). % GR 07/2015
ignore_type_relation_type_1_DOM(tisu,manifestation_of,phsf). % GR 07/2015
ignore_type_relation_type_1_DOM(tisu,part_of,bird).
ignore_type_relation_type_1_DOM(tisu,part_of,invt).
ignore_type_relation_type_1_DOM(tmco,has_occurrence,plnt).
ignore_type_relation_type_1_DOM(tmco,has_occurrence,menp).
ignore_type_relation_type_1_DOM(tmco,has_occurrence,ortf). % GR 07/2015
ignore_type_relation_type_1_DOM(tmco,has_occurrence,phsf). % GR 07/2015
ignore_type_relation_type_1_DOM(tmco,result_of,moft).
ignore_type_relation_type_1_DOM(topp,affects,menp).
ignore_type_relation_type_1_DOM(topp,complicates,moft).
ignore_type_relation_type_1_DOM(topp,complicates,menp).
ignore_type_relation_type_1_DOM(topp,has_location,moft).
ignore_type_relation_type_1_DOM(topp,has_location,blor).
ignore_type_relation_type_1_DOM(topp,has_location,bpoc).
ignore_type_relation_type_1_DOM(topp,has_location,bsoj).
ignore_type_relation_type_1_DOM(topp,has_location,celc).
ignore_type_relation_type_1_DOM(topp,has_location,cell).
ignore_type_relation_type_1_DOM(topp,has_location,hcro).
ignore_type_relation_type_1_DOM(topp,has_location,orgt).
ignore_type_relation_type_1_DOM(topp,has_location,pros).
ignore_type_relation_type_1_DOM(topp,has_location,shro).
ignore_type_relation_type_1_DOM(topp,produced_by,shro). % Gr 07/2015
ignore_type_relation_type_1_DOM(topp,produced_by,pros). % GR 07/2015
ignore_type_relation_type_1_DOM(topp,produced_by,orgt). % GR 07/2015
ignore_type_relation_type_1_DOM(topp,uses,mnob). % GR 08/2015
ignore_type_relation_type_1_DOM(topp,uses,tisu).
ignore_type_relation_type_1_DOM(tisu,used_by,topp). % GR 08/2015
ignore_type_relation_type_1_DOM(virs,affected_by,mnob).
ignore_type_relation_type_1_DOM(virs,affected_by,menp).
ignore_type_relation_type_1_DOM(virs,has_location,moft).
ignore_type_relation_type_1_DOM(virs,has_process,gngm). % GR 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,dsyn).
ignore_type_relation_type_1_DOM(virs,has_process,menp). % GR 07/2015
ignore_type_relation_type_1_DOM(virs,has_process,moft). % GR 07/2015
ignore_type_relation_type_1_DOM(virs,has_property,moft).
ignore_type_relation_type_1_DOM(virs,interacts_with,clna).
ignore_type_relation_type_1_DOM(virs,interacts_with,bird).
ignore_type_relation_type_1_DOM(virs,interacts_with,humn).
ignore_type_relation_type_1_DOM(virs,interacts_with,mamm).
ignore_type_relation_type_1_DOM(vita,affects,plnt).
ignore_type_relation_type_1_DOM(vita,affects,menp).
%ignore_type_relation_type_1_DOM(vita,affects,moft). % GR comments it out in 08/2015 to leave the one in Locsemnet
ignore_type_relation_type_1_DOM(vita,complicates,moft).
ignore_type_relation_type_1_DOM(vita,complicates,menp).
ignore_type_relation_type_1_DOM(vita,disrupts,moft).
ignore_type_relation_type_1_DOM(vita,disrupts,menp).
ignore_type_relation_type_1_DOM(vita,has_location,moft).
ignore_type_relation_type_1_DOM(vita,interacts_with,plnt).
ignore_type_relation_type_1_DOM(vita,isa,chvf).
ignore_type_relation_type_1_DOM(vita,produced_by,chvf).
ignore_type_relation_type_1_DOM(vita,produced_by,menp).
ignore_type_relation_type_1_DOM(vita,produced_by,moft). % GR 07/
ignore_type_relation_type_1_DOM(vtbt,affected_by,moft).
ignore_type_relation_type_1_DOM(vtbt,affected_by,menp).
ignore_type_relation_type_1_DOM(vtbt,exhibits,moft).
ignore_type_relation_type_1_DOM(rich,has_process,socb). % 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,phsf). % GR 06/2015
%ignore_type_relation_type_1_DOM(vtbt,has_process,phsf). % GR 08/2015
ignore_type_relation_type_1_DOM(vtbt,has_process,menp).
ignore_type_relation_type_1_DOM(vtbt,has_process,mobd). % GR 07/2015
ignore_type_relation_type_1_DOM(vtbt,has_property,moft).
ignore_type_relation_type_1_DOM(vtbt,interacts_with,clna).
ignore_type_relation_type_1_DOM(vtbt,interacts_with,bird).
ignore_type_relation_type_1_DOM(vtbt,interacts_with,humn).
ignore_type_relation_type_1_DOM(vtbt,interacts_with,mamm).
ignore_type_relation_type_1_DOM(vtbt,inverse_isa,plnt).
ignore_type_relation_type_1_DOM(vtbt,inverse_isa,bird).
ignore_type_relation_type_1_DOM(inpo,disrupts,mamm).
ignore_type_relation_type_1_DOM(dsyn,process_of,gngm).
ignore_type_relation_type_1_DOM(phsu,diagnoses,celf). % The following by GR after meeting with Tom
ignore_type_relation_type_1_DOM(phsu,diagnoses,dsyn).
ignore_type_relation_type_1_DOM(phsu,diagnoses,patf).
ignore_type_relation_type_1_DOM(phsu,diagnoses,emod).
ignore_type_relation_type_1_DOM(phsu,diagnoses,neop).
ignore_type_relation_type_1_DOM(phsu,diagnoses,mobd).
ignore_type_relation_type_1_DOM(phsu,diagnoses,comd).
ignore_type_relation_type_1_DOM(phsu,diagnoses,sosy).
ignore_type_relation_type_1_DOM(phsu,diagnoses,inpo).
ignore_type_relation_type_1_DOM(dsyn,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(patf,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(emod,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(neop,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(mobd,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(comd,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(sosy,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(inpo,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(celf,diagnosed_by,phsu).
ignore_type_relation_type_1_DOM(antb,diagnoses,fndg).
ignore_type_relation_type_1_DOM(antb,diagnoses,dsyn).
ignore_type_relation_type_1_DOM(antb,diagnoses,patf).
ignore_type_relation_type_1_DOM(antb,diagnoses,emod).
ignore_type_relation_type_1_DOM(antb,diagnoses,neop).
ignore_type_relation_type_1_DOM(antb,diagnoses,mobd).
ignore_type_relation_type_1_DOM(antb,diagnoses,comd).
ignore_type_relation_type_1_DOM(antb,diagnoses,sosy).
ignore_type_relation_type_1_DOM(antb,diagnoses,inpo).
ignore_type_relation_type_1_DOM(dsyn,diagnosed_by,fndg).
ignore_type_relation_type_1_DOM(patf,diagnosed_by,antb).
ignore_type_relation_type_1_DOM(emod,diagnosed_by,antb).
ignore_type_relation_type_1_DOM(neop,diagnosed_by,antb).
ignore_type_relation_type_1_DOM(mobd,diagnosed_by,antb).
ignore_type_relation_type_1_DOM(comd,diagnosed_by,antb).
ignore_type_relation_type_1_DOM(sosy,diagnosed_by,antb).
ignore_type_relation_type_1_DOM(inpo,diagnosed_by,antb).
ignore_type_relation_type_1_DOM(fndg,diagnosed_by,antb).
ignore_type_relation_type_1_DOM(aapp,diagnoses,antb).
ignore_type_relation_type_1_DOM(aapp,diagnoses,dsyn).
ignore_type_relation_type_1_DOM(aapp,diagnoses,patf).
ignore_type_relation_type_1_DOM(aapp,diagnoses,emod).
ignore_type_relation_type_1_DOM(aapp,diagnoses,fndg). % 07/2015
ignore_type_relation_type_1_DOM(aapp,diagnoses,neop).
ignore_type_relation_type_1_DOM(aapp,diagnoses,mobd).
ignore_type_relation_type_1_DOM(aapp,diagnoses,comd).
ignore_type_relation_type_1_DOM(aapp,diagnoses,sosy).
ignore_type_relation_type_1_DOM(aapp,diagnoses,inpo).
ignore_type_relation_type_1_DOM(patf,diagnosed_by,aapp).
ignore_type_relation_type_1_DOM(emod,diagnosed_by,aapp).
ignore_type_relation_type_1_DOM(neop,diagnosed_by,aapp).
ignore_type_relation_type_1_DOM(mobd,diagnosed_by,aapp).
ignore_type_relation_type_1_DOM(comd,diagnosed_by,aapp).
ignore_type_relation_type_1_DOM(sosy,diagnosed_by,aapp).
ignore_type_relation_type_1_DOM(inpo,diagnosed_by,aapp).
ignore_type_relation_type_1_DOM(fndg,diagnoses,dsyn). % 07/2015
ignore_type_relation_type_1_DOM(fndg,diagnosed_by,aapp).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,aapp).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,patf).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,emod).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,neop).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,mobd).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,comd).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,sosy).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,inpo).
ignore_type_relation_type_1_DOM(dsyn,diagnoses,dsyn). % 07/2015
ignore_type_relation_type_1_DOM(dsyn,diagnoses,fndg).
ignore_type_relation_type_1_DOM(dsyn,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(patf,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(emod,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(neop,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(mobd,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(comd,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(sosy,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(inpo,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(fndg,diagnosed_by,dsyn).
ignore_type_relation_type_1_DOM(patf,diagnoses,dsyn).
ignore_type_relation_type_1_DOM(patf,diagnoses,patf).
ignore_type_relation_type_1_DOM(patf,diagnoses,emod).
ignore_type_relation_type_1_DOM(patf,diagnoses,neop).
ignore_type_relation_type_1_DOM(patf,diagnoses,mobd).
ignore_type_relation_type_1_DOM(patf,diagnoses,comd).
ignore_type_relation_type_1_DOM(patf,diagnoses,sosy).
ignore_type_relation_type_1_DOM(patf,diagnoses,inpo).
ignore_type_relation_type_1_DOM(patf,diagnoses,fndg).
ignore_type_relation_type_1_DOM(emod,diagnosed_by,patf).
ignore_type_relation_type_1_DOM(neop,diagnosed_by,patf).
ignore_type_relation_type_1_DOM(mobd,diagnosed_by,patf).
ignore_type_relation_type_1_DOM(comd,diagnosed_by,patf).
ignore_type_relation_type_1_DOM(sosy,diagnosed_by,patf).
ignore_type_relation_type_1_DOM(inpo,diagnosed_by,patf).
ignore_type_relation_type_1_DOM(fndg,diagnosed_by,patf).
ignore_type_relation_type_1_DOM(sosy,diagnoses,dsyn).
ignore_type_relation_type_1_DOM(sosy,diagnoses,patf).
ignore_type_relation_type_1_DOM(sosy,diagnoses,emod).
ignore_type_relation_type_1_DOM(sosy,diagnoses,neop).
ignore_type_relation_type_1_DOM(sosy,diagnoses,mobd).
ignore_type_relation_type_1_DOM(sosy,diagnoses,comd).
ignore_type_relation_type_1_DOM(sosy,diagnoses,sosy).
ignore_type_relation_type_1_DOM(sosy,diagnoses,inpo).
ignore_type_relation_type_1_DOM(sosy,diagnoses,fndg).
ignore_type_relation_type_1_DOM(emod,diagnosed_by,sosy).
ignore_type_relation_type_1_DOM(neop,diagnosed_by,sosy).
ignore_type_relation_type_1_DOM(mobd,diagnosed_by,sosy).
ignore_type_relation_type_1_DOM(comd,diagnosed_by,sosy).
ignore_type_relation_type_1_DOM(sosy,diagnosed_by,sosy).
ignore_type_relation_type_1_DOM(inpo,diagnosed_by,sosy).
ignore_type_relation_type_1_DOM(fndg,diagnosed_by,sosy).
ignore_type_relation_type_1_DOM(dsyn,diagnosed_by,sosy).
ignore_type_relation_type_1_DOM(comd,occurs_in,aggp).
ignore_type_relation_type_1_DOM(comd,occurs_in,famg).
ignore_type_relation_type_1_DOM(comd,occurs_in,grup).
ignore_type_relation_type_1_DOM(comd,occurs_in,podg).
ignore_type_relation_type_1_DOM(comd,occurs_in,popg).
ignore_type_relation_type_1_DOM(comd,occurs_in,prog).
ignore_type_relation_type_1_DOM(comd,occurs_in,dsyn).
ignore_type_relation_type_1_DOM(comd,occurs_in,inpo).
ignore_type_relation_type_1_DOM(comd,occurs_in,mobd).
ignore_type_relation_type_1_DOM(emod,occurs_in,aggp).
ignore_type_relation_type_1_DOM(emod,occurs_in,famg).
ignore_type_relation_type_1_DOM(emod,occurs_in,grup).
ignore_type_relation_type_1_DOM(emod,occurs_in,podg).
ignore_type_relation_type_1_DOM(emod,occurs_in,popg).
ignore_type_relation_type_1_DOM(emod,occurs_in,prog).
ignore_type_relation_type_1_DOM(emod,occurs_in,dsyn).
ignore_type_relation_type_1_DOM(emod,occurs_in,inpo).
ignore_type_relation_type_1_DOM(emod,occurs_in,mobd).
ignore_type_relation_type_1_DOM(emod,occurs_in,neop).
ignore_type_relation_type_1_DOM(genf,occurs_in,tmco).
ignore_type_relation_type_1_DOM(celf,occurs_in,tmco).
ignore_type_relation_type_1_DOM(phsf,occurs_in,tmco).
ignore_type_relation_type_1_DOM(ortf,occurs_in,tmco).
ignore_type_relation_type_1_DOM(dsyn,occurs_in,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,occurs_in,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,occurs_in,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,occurs_in,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,occurs_in,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(fndg,occurs_in,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(fndg,occurs_in,fndg). % GR 06/2015
ignore_type_relation_type_1_DOM(fndg,occurs_in,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(fndg,occurs_in,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(fndg,occurs_in,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(fndg,occurs_in,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(fndg,occurs_in,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,occurs_in,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,occurs_in,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,occurs_in,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,occurs_in,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,occurs_in,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,occurs_in,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,occurs_in,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,occurs_in,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,occurs_in,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,occurs_in,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,occurs_in,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,occurs_in,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,occurs_in,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,occurs_in,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,occurs_in,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,occurs_in,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,occurs_in,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,occurs_in,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,occurs_in,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(aggp,has_occurrence,patf).
ignore_type_relation_type_1_DOM(famg,has_occurrence,comd).
ignore_type_relation_type_1_DOM(grup,has_occurrence,comd).
ignore_type_relation_type_1_DOM(podg,has_occurrence,comd).
ignore_type_relation_type_1_DOM(popg,has_occurrence,comd).
ignore_type_relation_type_1_DOM(prog,has_occurrence,comd).
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,comd).
ignore_type_relation_type_1_DOM(inpo,has_occurrence,comd).
ignore_type_relation_type_1_DOM(mobd,has_occurrence,comd).
ignore_type_relation_type_1_DOM(neop,has_occurrence,comd).
ignore_type_relation_type_1_DOM(aggp,has_occurrence,comd).
ignore_type_relation_type_1_DOM(famg,has_occurrence,emod).
ignore_type_relation_type_1_DOM(grup,has_occurrence,emod).
ignore_type_relation_type_1_DOM(podg,has_occurrence,emod).
ignore_type_relation_type_1_DOM(popg,has_occurrence,emod).
ignore_type_relation_type_1_DOM(prog,has_occurrence,emod).
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,emod).
ignore_type_relation_type_1_DOM(inpo,has_occurrence,emod).
ignore_type_relation_type_1_DOM(mobd,has_occurrence,emod).
ignore_type_relation_type_1_DOM(neop,has_occurrence,emod).
ignore_type_relation_type_1_DOM(tmco,has_occurrence,genf).
ignore_type_relation_type_1_DOM(tmco,has_occurrence,celf).
ignore_type_relation_type_1_DOM(tmco,has_occurrence,moft).
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,ortf). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,has_occurrence,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,has_occurrence,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_occurrence,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,has_occurrence,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,has_occurrence,fndg). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,has_occurrence,fndg). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_occurrence,fndg). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,has_occurrence,fndg). % GR 06/2015
ignore_type_relation_type_1_DOM(fndg,has_occurrence,fndg). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,fndg). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,has_occurrence,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,has_occurrence,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,has_occurrence,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_occurrence,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,inpo). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,has_occurrence,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,has_occurrence,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,has_occurrence,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_occurrence,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,mobd). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,has_occurrence,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,has_occurrence,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,has_occurrence,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_occurrence,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(dsyn,has_occurrence,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(inpo,has_occurrence,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(mobd,has_occurrence,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,has_occurrence,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_occurrence,patf). % GR 06/2015
ignore_type_relation_type_1_DOM(rept,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(rich,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(vtbt,has_process,neop). % GR 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,neop). % GR 06/2015
%ignore_type_relation_type_1_DOM(rept,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(rich,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(vtbt,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,emod). % GR 06/2015
ignore_type_relation_type_1_DOM(alga,has_process,emod). % GR 06/2015
%ignore_type_relation_type_1_DOM(amph,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,genf). % GR 06/2015
%ignore_type_relation_type_1_DOM(rept,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(rich,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(vtbt,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(ortf,has_process,genf). % GR 06/2015
ignore_type_relation_type_1_DOM(alga,has_process,genf).
ignore_type_relation_type_1_DOM(arch,has_process,celf).
ignore_type_relation_type_1_DOM(bact,has_process,celf).
ignore_type_relation_type_1_DOM(fngs,has_process,celf).
ignore_type_relation_type_1_DOM(genf,has_process,celf).
ignore_type_relation_type_1_DOM(invt,has_process,celf).
ignore_type_relation_type_1_DOM(orgm,has_process,celf).
ignore_type_relation_type_1_DOM(rich,has_process,celf).
ignore_type_relation_type_1_DOM(virs,has_process,celf).
ignore_type_relation_type_1_DOM(alga,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(neop,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(fngs,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(ortf,has_process,dsyn). % GR 06/2015
ignore_type_relation_type_1_DOM(genf,has_process,dsyn). % 06/2015
ignore_type_relation_type_1_DOM(rich,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(fngs,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(arch,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(alga,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(bact,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(invt,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(orgm,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(npop,has_process,comd). % 06/2015
ignore_type_relation_type_1_DOM(npop,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(ortf,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,patf). % 06/2015
%ignore_type_relation_type_1_DOM(rept,has_process,patf). % 07/2015 commented out
ignore_type_relation_type_1_DOM(virs,has_process,patf). % 06/2015
ignore_type_relation_type_1_DOM(plnt,has_process,patf).
ignore_type_relation_type_1_DOM(rept,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(rich,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,mobd). % 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(rich,has_process,biof). % GR 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,ortf). % GR 06/2015
ignore_type_relation_type_1_DOM(rich,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,orgf). % GR 06/2015
ignore_type_relation_type_1_DOM(patf,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(phsf,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(plnt,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(rept,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(rich,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(virs,has_process,npop). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,phsf). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,rept). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,rich). % GR 06/2015
ignore_type_relation_type_1_DOM(npop,process_of,virs). % GR 06/2015
%ignore_type_relation_type_1_DOM(patf,process_of,rept). % 07/2015 commented out
ignore_type_relation_type_1_DOM(patf,process_of,plnt).
ignore_type_relation_type_1_DOM(socb,affected_by,socb). % GR 08/2015
ignore_type_relation_type_1_DOM(virs,affected_by,moft). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,affected_by,vita). % GR 08/2015
ignore_type_relation_type_1_DOM(orga,affected_by,moft). % GR 08/2015
ignore_type_relation_type_1_DOM(rich,affected_by,moft). % GR 08/2015
ignore_type_relation_type_1_DOM(socb,affected_by,resa). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,affected_by,resa). % GR 08/2015
ignore_type_relation_type_1_DOM(ortf,affected_by,moft). % GR 08/2015
ignore_type_relation_type_1_DOM(ortf,affected_by,menp). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,process_of,phsf). % GR 08/2015
ignore_type_relation_type_1_DOM(dsyn,process_of,celf). % GR 08/2015
ignore_type_relation_type_1_DOM(gngm,process_of,rich). % GR 08/2015
ignore_type_relation_type_1_DOM(gngm,process_of,virs). % GR 08/2015
ignore_type_relation_type_1_DOM(neop,process_of,rept). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,process_of,ortf). % GR 08/2015
ignore_type_relation_type_1_DOM(tisu,process_of,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(qlco,process_of,socb). % GR 08/2015
ignore_type_relation_type_1_DOM(npop,process_of,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(orgf,process_of,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(socb,process_of,rept). % GR 08/2015
ignore_type_relation_type_1_DOM(socb,process_of,rich). % GR 08/2015
ignore_type_relation_type_1_DOM(ortf,process_of,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(clna,process_of,npop). % GR 08/2015
ignore_type_relation_type_1_DOM(dsyn,has_location,acab). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,acab). % GR 08/2015
ignore_type_relation_type_1_DOM(dsyn,has_location,anab). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,anab). % GR 08/2015
ignore_type_relation_type_1_DOM(acab,has_location,blor). % GR 08/2015
ignore_type_relation_type_1_DOM(anab,has_location,blor). % GR 08/2015
ignore_type_relation_type_1_DOM(cgab,has_location,blor). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,blor). % GR 08/2015
ignore_type_relation_type_1_DOM(acab,has_location,bpoc). % GR 08/2015
ignore_type_relation_type_1_DOM(blor,has_location,bpoc). % GR 08/2015
ignore_type_relation_type_1_DOM(cgab,has_location,bpoc). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,bpoc). % GR 08/2015
ignore_type_relation_type_1_DOM(acab,has_location,bsoj). % GR 08/2015
ignore_type_relation_type_1_DOM(anab,has_location,bsoj). % GR 08/2015
ignore_type_relation_type_1_DOM(cgab,has_location,bsoj). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,bsoj). % GR 08/2015
ignore_type_relation_type_1_DOM(acab,has_location,celc). % GR 08/2015
ignore_type_relation_type_1_DOM(anab,has_location,celc). % GR 08/2015
ignore_type_relation_type_1_DOM(cgab,has_location,celc). % GR 08/2015
ignore_type_relation_type_1_DOM(dsyn,has_location,celc). % GR 08/2015
ignore_type_relation_type_1_DOM(inpo,has_location,celc). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,celc). % GR 08/2015
ignore_type_relation_type_1_DOM(acab,has_location,cell). % GR 08/2015
ignore_type_relation_type_1_DOM(anab,has_location,cell). % GR 08/2015
ignore_type_relation_type_1_DOM(cgab,has_location,cell). % GR 08/2015
ignore_type_relation_type_1_DOM(dsyn,has_location,cell). % GR 08/2015
ignore_type_relation_type_1_DOM(inpo,has_location,cell). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,cell). % GR 08/2015
ignore_type_relation_type_1_DOM(dsyn,has_location,cgab). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,cgab). % GR 08/2015
ignore_type_relation_type_1_DOM(dsyn,has_location,emst). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,emst). % GR 08/2015
ignore_type_relation_type_1_DOM(acab,has_location,ffas). % GR 08/2015
ignore_type_relation_type_1_DOM(anab,has_location,ffas). % GR 08/2015
ignore_type_relation_type_1_DOM(cgab,has_location,ffas). % GR 08/2015
ignore_type_relation_type_1_DOM(dsyn,has_location,ffas). % GR 08/2015
ignore_type_relation_type_1_DOM(inpo,has_location,ffas). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,ffas). % GR 08/2015
ignore_type_relation_type_1_DOM(rich,has_location,gngm). % GR 08/2015
ignore_type_relation_type_1_DOM(virs,has_location,gngm). % GR 08/2015
ignore_type_relation_type_1_DOM(nsba,has_location,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(ocdi,has_location,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(rcpt,has_location,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(acab,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(anab,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(cgab,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(chvf,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(mobd,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(orgf,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(ortf,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(patf,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(phsf,has_location,tisu). % GR 08/2015
ignore_type_relation_type_1_DOM(gora,has_location,hcro). % GR 08/2015
ignore_type_relation_type_1_DOM(gora,has_location,orgt). % GR 08/2015
ignore_type_relation_type_1_DOM(ocac,has_location,orgt). % GR 08/2015
ignore_type_relation_type_1_DOM(plnt,has_location,orgt). % GR 08/2015
ignore_type_relation_type_1_DOM(gora,has_location,pros). % GR 08/2015
ignore_type_relation_type_1_DOM(mnob,has_location,pros). % GR 08/2015
ignore_type_relation_type_1_DOM(chvf,has_location,shro). % GR 08/2015
ignore_type_relation_type_1_DOM(ocac,has_location,shro). % GR 08/2015
ignore_type_relation_type_1_DOM(gora,has_location,shro). % GR 08/2015
ignore_type_relation_type_1_DOM(virs,interacts_with,plnt). % GR 08/2015
ignore_type_relation_type_1_DOM(vita,interacts_with,chvf). % GR 08/2015
ignore_type_relation_type_1_DOM(neop,has_part,bpoc). % GR 06/2016
% Graciela's non-compositional phrase list.
% This is a list of strings that essentially act like the string in the list
% above, but qualitatively they are different, so I am keeping them in a
% separate list for now. We may find a better solution for these within

