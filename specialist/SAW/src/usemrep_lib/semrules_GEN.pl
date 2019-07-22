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
%      Do Not Modify This File    %
%     It is machine generated.    %
:- module(semrules_GEN,	[
	word_corresponds_to_semnet_relation_GEN/4,
	multiphrase_corresponds_to_semnet_relation_GEN/6,
	phrase_corresponds_to_semnet_relation_GEN/6
]).

multiphrase_corresponds_to_semnet_relation_GEN(increase,[odds],verb,[noun],for,predisposes). % HK 2018
multiphrase_corresponds_to_semnet_relation_GEN(increase,[odds],verb,[noun],_,predisposes). % HK 2018
multiphrase_corresponds_to_semnet_relation_GEN(increase,[risk],verb,[noun],for,predisposes). % GR 2018
multiphrase_corresponds_to_semnet_relation_GEN(increase,[risk],verb,[noun],_,predisposes). % GR 2018
multiphrase_corresponds_to_semnet_relation_GEN(reduce,[risk], verb,[noun], for,prevents). % FML 10/29/2015
multiphrase_corresponds_to_semnet_relation_GEN(reduce,[risk], verb,[noun], _,prevents).
multiphrase_corresponds_to_semnet_relation_GEN(decrease,[risk], verb,[noun], for,prevents). % FML 10/29/2015
multiphrase_corresponds_to_semnet_relation_GEN(decrease,[risk], verb,[noun], _,prevents). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(lower,[risk], verb,[noun], for,prevents). % FML 10/29/2015
multiphrase_corresponds_to_semnet_relation_GEN(lower,[risk], verb,[noun], _,prevents). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(minimize,[risk], verb,[noun], for,prevents). % FML 10/29/2015
multiphrase_corresponds_to_semnet_relation_GEN(minimize,[risk], verb,[noun], _,prevents). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(diminish,[risk], verb,[noun], for,prevents). % GR 06/21/2017
multiphrase_corresponds_to_semnet_relation_GEN(diminish,[risk], verb,[noun], _,prevents). % GR 06/21/2017
multiphrase_corresponds_to_semnet_relation_GEN(stimulate,[differentiation],verb,[noun],_,affects). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(decrease,[increase],verb,[noun],in,affects). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(play,[role],verb,[noun],in,affects). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(produce,[reduction],verb,[noun],_,affects). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(produce,[reduction],verb,[noun],_,treats). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(cause,[loss],verb,[noun],_,disrupts). % GR 07/2015
multiphrase_corresponds_to_semnet_relation_GEN(cause,[deregulation],verb,[noun],of,affects). % GR 05/23/2017 --from Negation project
multiphrase_corresponds_to_semnet_relation_GEN(cause,[decrease],verb,[noun],in,affects). % GR 04/2018 from Negation project
multiphrase_corresponds_to_semnet_relation_GEN(cause,[decrease],verb,[noun],of,affects). % GR 04/2018 --from Negation project
multiphrase_corresponds_to_semnet_relation_GEN(cause,[increase],verb,[noun],in,affects). % GR 04/2018 from Negation project
multiphrase_corresponds_to_semnet_relation_GEN(cause,[increase],verb,[noun],of,affects). % GR 04/2018 --from Negation project
multiphrase_corresponds_to_semnet_relation_GEN(cause,[change],verb,[noun],in,affects). % GR 04/2018 
multiphrase_corresponds_to_semnet_relation_GEN(cause,[fluctuation],verb,[noun],in,affects). % GR 04/2018 
multiphrase_corresponds_to_semnet_relation_GEN(cause,[variation],verb,[noun],in,affects). % GR 04/2018 
multiphrase_corresponds_to_semnet_relation_GEN(slow,[progression],verb,[noun],_,treats). % GR 09/2015
multiphrase_corresponds_to_semnet_relation_GEN(slow,[progression],verb,[noun],_,disrupts). % GR 09/2015
multiphrase_corresponds_to_semnet_relation_GEN(achieve,[eradication],verb,[noun],of,treats). % GR 05/23/2017 --from GS project
multiphrase_corresponds_to_semnet_relation_GEN(contribute,[development],verb,[noun],of,predisposes). % GR 04/11/2018 Negation Project 
multiphrase_corresponds_to_semnet_relation_GEN(contribute,[risk],verb,[noun],for,predisposes). % GR 04/11/2018 Negation Project 
multiphrase_corresponds_to_semnet_relation_GEN(become,[ill],verb,[noun],with,has_process). % GR 04/12/2018 PP attachment project
multiphrase_corresponds_to_semnet_relation_GEN(become,[ill],verb,[adv],with,has_process). % GR 04/12/2018 PP attachment project
multiphrase_corresponds_to_semnet_relation_GEN(confirm,[diagnosis],verb,[noun],of,diagnoses). % GR 04/12/2018 
multiphrase_corresponds_to_semnet_relation_GEN(receive,[diagnosis],verb,[noun],of,has_process). % GR 04/12/2018 
% From Graciela, 5/7/2012, generalized from domain migration projects
multiphrase_corresponds_to_semnet_relation_GEN(bring,[together],verb,[adv],_,uses).
multiphrase_corresponds_to_semnet_relation_GEN(engage,[use],verb,[noun],in,uses).
multiphrase_corresponds_to_semnet_relation_GEN(make,[use],verb,[noun],of,uses).
multiphrase_corresponds_to_semnet_relation_GEN(set,[aside],verb,[adv],for,used_by).
% From SemGen, 5/23/2014 
multiphrase_corresponds_to_semnet_relation_GEN(regulate,[negatively],verb,[adv],_,inhibits).
multiphrase_corresponds_to_semnet_relation_GEN(regulate,[negatively],verb,[adv],_,disrupts).
multiphrase_corresponds_to_semnet_relation_GEN(regulate,[positively],verb,[adv],_,stimulates).
multiphrase_corresponds_to_semnet_relation_GEN(regulate,[positively],verb,[adv],_,augments).
phrase_corresponds_to_semnet_relation_GEN(risk,[reduced],noun,[adj],_,prevents).
phrase_corresponds_to_semnet_relation_GEN(risk,[decreased],noun,[adj],_,prevents).
phrase_corresponds_to_semnet_relation_GEN(effect,[protective],noun,[adj],_,prevents). % GR 10/2015
phrase_corresponds_to_semnet_relation_GEN(result,[as],noun,[prep],of,caused_by). % GR 07/2015
phrase_corresponds_to_semnet_relation_GEN(destruction,[chemotherapeutic],noun,[noun],_,caused_by). % GR 10/2015
phrase_corresponds_to_semnet_relation_GEN(risk,[decreasing],noun,[noun],of,prevents). %GR 11/20/2017 --As per Halil
phrase_corresponds_to_semnet_relation_GEN(risk,[decreasing],noun,[noun],_,prevents). %GR 11/20/2017 --As per Halil
phrase_corresponds_to_semnet_relation_GEN(risk,[reducing],noun,[noun],of,prevents). %GR 11/09/2017 --As per Halil
phrase_corresponds_to_semnet_relation_GEN(risk,[reducing],noun,[noun],_,prevents). %GR 11/09/2017 --As per Halil
phrase_corresponds_to_semnet_relation_GEN(risk,[decreasing],noun,[adj],of,prevents). %GR 11/20/2017 --As per Halil
phrase_corresponds_to_semnet_relation_GEN(risk,[decreasing],noun,[adj],_,prevents). %GR 11/20/2017 --As per Halil
phrase_corresponds_to_semnet_relation_GEN(risk,[reducing],noun,[adj],of,prevents). %GR 11/20/2017 --As per Halil
phrase_corresponds_to_semnet_relation_GEN(risk,[reducing],noun,[adj],_,prevents). %GR 11/20/2017 --As per Halil
phrase_corresponds_to_semnet_relation_GEN(effect,[inhibitory],noun,[adj],_,disrupts). % GR 07/2015
phrase_corresponds_to_semnet_relation_GEN(effect,[damaging],noun,[adj],_,disrupts). % GR 07/2015
phrase_corresponds_to_semnet_relation_GEN(effect,[inhibitory],noun,[adj],_,inhibits). % GR 07/2015
phrase_corresponds_to_semnet_relation_GEN(effect,[suppressive],noun,[adj],_,inhibits). % GR 07/2015
phrase_corresponds_to_semnet_relation_GEN(effect,[beneficial],noun,[adj],_,treats). % GR 06/14/2017 - -Must keep under observation
phrase_corresponds_to_semnet_relation_GEN(action,[therapeutic],noun,[adj],_,treats). % GR Negation project 08/2018
phrase_corresponds_to_semnet_relation_GEN(effect,[therapeutic],noun,[adj],_,treats). % GR Negation project
phrase_corresponds_to_semnet_relation_GEN(role,[therapeutic],noun,[adj],_,treats). % GR Negation project
phrase_corresponds_to_semnet_relation_GEN(impact,[inhibitory],noun,[adj],_,disrupts). % GR 10/03/2017
phrase_corresponds_to_semnet_relation_GEN(impact,[inhibitory],noun,[adj],_,inhibits). % GR 10/03/2017
phrase_corresponds_to_semnet_relation_GEN(activity,[decreased],noun,[adj],_,inhibits). % GR 10/2015
phrase_corresponds_to_semnet_relation_GEN(loss,[complete],noun,[adj],of,affects). % GR 06/13/2017 --from Negation project -Must keep under observation
phrase_corresponds_to_semnet_relation_GEN(loss,[total],noun,[adj],of,affects). % GR 06/13/2017 --from Negation project -Must keep under observation
phrase_corresponds_to_semnet_relation_GEN(function,[repressive],noun,[adj],over,disrupts). % GR 06/23/2017 --GENIA
phrase_corresponds_to_semnet_relation_GEN(function,[suppressive],noun,[adj],over,disrupts). % GR 06/23/2017
phrase_corresponds_to_semnet_relation_GEN(propensity,[high],noun,[adj], _, predisposes). % GR Negation corpus PMID 25061993
% to handle weird bracketing returned by MetaMap
% down-regulate -> INHIBITS
phrase_corresponds_to_semnet_relation_GEN(regulate,[down],verb,[verb],_,inhibits).
phrase_corresponds_to_semnet_relation_GEN(regulate,[[down,-]],verb,[verb],_,inhibits).
phrase_corresponds_to_semnet_relation_GEN(regulates,[down],verb,[verb],_,inhibits).
phrase_corresponds_to_semnet_relation_GEN(regulates,[[down,-]],verb,[verb],_,inhibits).
phrase_corresponds_to_semnet_relation_GEN(regulated,[down],verb,[verb],_,inhibits).
phrase_corresponds_to_semnet_relation_GEN(regulated,[[down,-]],verb,[verb],_,inhibits).
phrase_corresponds_to_semnet_relation_GEN(modulate,[[down,-]],verb,[verb],_,inhibits). % GR 06/22/2017 source: GENIA
% down-regulate -> DISRUPTS
phrase_corresponds_to_semnet_relation_GEN(regulate,[down],verb,[verb],_,disrupts).
phrase_corresponds_to_semnet_relation_GEN(regulate,[[down,-]],verb,[verb],_,disrupts).
phrase_corresponds_to_semnet_relation_GEN(regulates,[down],verb,[verb],_,disrupts).
phrase_corresponds_to_semnet_relation_GEN(regulates,[[down,-]],verb,[verb],_,disrupts).
phrase_corresponds_to_semnet_relation_GEN(regulated,[down],verb,[verb],_,disrupts).
phrase_corresponds_to_semnet_relation_GEN(regulated,[[down,-]],verb,[verb],_,disrupts).
phrase_corresponds_to_semnet_relation_GEN(modulate,[[down,-]],verb,[verb],_,disrupts). % GR 06/22/2017 source: GENIA
% up-regulate -> STIMULATES
phrase_corresponds_to_semnet_relation_GEN(regulate,[up],verb,[verb],_,stimulates).
phrase_corresponds_to_semnet_relation_GEN(regulate,[[up,-]],verb,[verb],_,stimulates).
phrase_corresponds_to_semnet_relation_GEN(regulates,[up],verb,[verb],_,stimulates).
phrase_corresponds_to_semnet_relation_GEN(regulates,[[up,-]],verb,[verb],_,stimulates).
phrase_corresponds_to_semnet_relation_GEN(regulated,[up],verb,[verb],_,stimulates).
phrase_corresponds_to_semnet_relation_GEN(regulated,[[up,-]],verb,[verb],_,stimulates).
phrase_corresponds_to_semnet_relation_GEN(activate,[[trans,-]],verb,[verb],_,stimulates). % GR 06/01/2017 source: GENIA 7964616_S5 EBNA-2 is able to trans-activate the expression of the LMP gene in several cell lines. When cotransfected with the HIV LTR CAT into CV-1 cells, both the pCD41 and pGD41 clones trans-activated the HIV LTR.
phrase_corresponds_to_semnet_relation_GEN(activate,[[trans,-]],verb,[verb],_,affects). % GR It could be affects as well
% up-regulate -> AUGMENTS
phrase_corresponds_to_semnet_relation_GEN(regulate,[up],verb,[verb],_,augments).
phrase_corresponds_to_semnet_relation_GEN(regulate,[[up,-]],verb,[verb],_,augments).
phrase_corresponds_to_semnet_relation_GEN(regulates,[up],verb,[verb],_,augments).
phrase_corresponds_to_semnet_relation_GEN(regulates,[[up,-]],verb,[verb],_,augments).
phrase_corresponds_to_semnet_relation_GEN(regulated,[up],verb,[verb],_,augments).
phrase_corresponds_to_semnet_relation_GEN(regulated,[[up,-]],verb,[verb],_,augments).
phrase_corresponds_to_semnet_relation_GEN(activate,[[trans,-]],verb,[verb],_,augments). % GR 06/2017 source: GENIA 7964616_S5 See previous example sentences for stimulates
% cross-linking -> INTERACTS_WITH
phrase_corresponds_to_semnet_relation_GEN(link,[[cross,-]],verb,[verb],_,interacts_with). % GR source: GENIA 9119999_S2 The T cell tropic strain of HIV, LAI, does not replicate in naive CD4 T cells stimulated by cross-linking CD3 and CD28.
phrase_corresponds_to_semnet_relation_GEN(linking,[[cross,-]],verb,[verb],_,interacts_with).
word_corresponds_to_semnet_relation_GEN([due, to],prep,_,caused_by).
word_corresponds_to_semnet_relation_GEN([because, of],prep,_,caused_by).
word_corresponds_to_semnet_relation_GEN([owing,to],prep,_,caused_by). % GR 10/06/2017
word_corresponds_to_semnet_relation_GEN([in,concert,with],prep,_,coexists_with). % GR 06/05/2017 GENIA 1583734_S1 and 1972889_S5 Specific NF-kappa B subunits act in concert with Tat to stimulate human immunodeficiency virus type 1 transcription. This inducer can act in concert with the HIV-2 tat gene and T-cell activation.
word_corresponds_to_semnet_relation_GEN([in,concert,with],prep,_,interacts_with). % GR 06/05/2017 GENIA 1583734_S1 and 1972889_S5 Specific NF-kappa B subunits act in concert with Tat to stimulate human immunodeficiency virus type 1 transcription. This inducer can act in concert with the HIV-2 tat gene and T-cell activation in enhancing gene expression in human CD4+ lymphocytes.
word_corresponds_to_semnet_relation_GEN([in,tandem,with],prep,_,coexists_with). % GR 08/28/2018 to complete this small paradigm
word_corresponds_to_semnet_relation_GEN([in,relation,to],prep,_,associated_with). % GR 10/2015
word_corresponds_to_semnet_relation_GEN([in,relation,to],prep,_,interacts_with). % GR 10/2015
word_corresponds_to_semnet_relation_GEN([in,response,to],prep,_,affected_by). % GR 06/01/2017 GENIA 9817603_S5 and 7739562_S4 The beta-casein gene is expressed in a cytotoxic T cell line, CTLL-2, in response to interleukin-2 (IL-2), which activates STAT5. This cytoplasmic inhibitor is rapidly phosphorylated and degraded in response to a diverse set of NF-kappa B-inducing agents...
%word_corresponds_to_semnet_relation_GEN([in,the,absence,of],prep,_,neg_coexists_with). % GR 06/13/2017
%word_corresponds_to_semnet_relation_GEN([in,the,absence,of],prep,_,neg_process_of). % GR 06/13/2017
word_corresponds_to_semnet_relation_GEN([risk, factor],noun,for,predisposes).
word_corresponds_to_semnet_relation_GEN([risk, factor],noun,_,predisposes).
word_corresponds_to_semnet_relation_GEN([risk, factors],noun,for,predisposes). % GR 11/09/2017
word_corresponds_to_semnet_relation_GEN([risk, factors],noun,_,predisposes). % GR 11/09/2017
word_corresponds_to_semnet_relation_GEN([risk, marker],noun,for,predisposes). % GR 10/20/2017 
word_corresponds_to_semnet_relation_GEN([risk, marker],noun,_,predisposes). % GR 10/20/2017 
word_corresponds_to_semnet_relation_GEN([risk, markers],noun,for,predisposes). % GR 11/09/2017
word_corresponds_to_semnet_relation_GEN([risk, markers],noun,_,predisposes). % GR 11/09/2017
word_corresponds_to_semnet_relation_GEN([risk,behavior],noun,for,predisposes).
word_corresponds_to_semnet_relation_GEN([risk,behavior],noun,_,predisposes).
%word_corresponds_to_semnet_relation_GEN([no,history,of],prep,_,neg_process_of). % GR 06/13/2017
word_corresponds_to_semnet_relation_GEN([non,-,producing],adj,_,neg_produced_by). % GR 06/13/2017
word_corresponds_to_semnet_relation_GEN([non,-,secreting],adj,_,neg_produced_by). % GR 06/13/2017
word_corresponds_to_semnet_relation_GEN([co,-,ligation],noun,_,interacts_with). % GR 06/22/2017 Co-ligation of surface immunoglobulin results in tyrosine phosphorylation. 
word_corresponds_to_semnet_relation_GEN([co,-,stimulate],verb,_,augments). % GR 06/22/2017 
word_corresponds_to_semnet_relation_GEN([co,-,stimulate],verb,_,stimulates). % GR 06/22/2017 
word_corresponds_to_semnet_relation_GEN([down,-,modulation],noun,_,disrupts). % GR 06/22/2017 source: GENIA
word_corresponds_to_semnet_relation_GEN([down,-,modulation],noun,_,inhibits). % GR 06/22/2017 source: GENIA
word_corresponds_to_semnet_relation_GEN([co,-,incubate],verb,with,interacts_with). % GR 06/23/2017 source: GENIA
word_corresponds_to_semnet_relation_GEN([co,-,incubate],verb,with,has_administration). % GR 06/23/2017 source: GENIA
word_corresponds_to_semnet_relation_GEN([co,-,incubation],noun,with,interacts_with). % GR 06/23/2017 source: GENIA
word_corresponds_to_semnet_relation_GEN([co,-,incubation],noun,with,has_administration). % GR 06/23/2017 source: GENIA
word_corresponds_to_semnet_relation_GEN([cross,-,sensitivity],noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN([cross,-,sensitivity],noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN([with,a,diagnosis,of],prep,_,has_process). % GR 01/24/2018
word_corresponds_to_semnet_relation_GEN([become,ill],verb,with,has_process). % GR 01/23/2018 PP attachment project
word_corresponds_to_semnet_relation_GEN([co,-,treatment],noun,with,treated_by). % GR 01/24/2018 Negation project
% Set of current direct semantic types from UMLS Semantic Network
% Needs to be manually updated each year
% word_corresponds_to_semnet_relation_GEN( ?Word, ?POS, ?Cue, ?Relation )
%word_corresponds_to_semnet_relation_GEN(be,aux,_,unspecified_relation). %  !!!!!!! - tcr
%word_corresponds_to_semnet_relation_GEN(agent,noun,for,affects).
% word_corresponds_to_semnet_relation_GEN(risk,           verb, _,          affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN(risk,           noun, of-for,     affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN(risk,           noun, for,        affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN('risk factor',  noun, of-for,     affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN('risk factor',  noun, for,        affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN('relative risk',  noun, of-for,     affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN('relative risk',  noun, for,        affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN('low risk',  noun, of-for,     affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN('low risk',  noun, for,        affects). % CA chg to predisposes
% word_corresponds_to_semnet_relation_GEN(on, prep, _, affects).   % tcr-very provisional
% word_corresponds_to_semnet_relation_GEN('has a role in',  affects).
% ----- BRANCH_OF / HAS_BRANCH
%word_corresponds_to_semnet_relation_GEN(arise,verb,from,branch_of). % x arises from y,  tcr
%word_corresponds_to_semnet_relation_GEN(arise,verb,of,branch_of). % x arises off of y,  tcr
%word_corresponds_to_semnet_relation_GEN(arise,verb,off,branch_of). % x arises off y,  tcr
%word_corresponds_to_semnet_relation_GEN(bifurcate,verb,into,has_branch). % x bifurcates into y & z,  tcr
%word_corresponds_to_semnet_relation_GEN(bifurcate,verb,to,has_branch). % x bifurcates into y & z,  tcr
%word_corresponds_to_semnet_relation_GEN(branch,verb,_,has_branch). % test case
%word_corresponds_to_semnet_relation_GEN(give,verb,off,has_branch). % x gives off y,  tcr
%word_corresponds_to_semnet_relation_GEN(giving,noun,off,has_branch). % x giving off y,  tcr
%word_corresponds_to_semnet_relation_GEN(rise,noun,_,has_branch). % x gives rise to y,  tcr
%word_corresponds_to_semnet_relation_GEN(takeoff,noun,from,branch_of). % x's takeoff from y,  tcr
%word_corresponds_to_semnet_relation_GEN(takeoff,noun,of,has_branch). % NOM untouched
% e.g.x ...prior to the takeoff of y,  tcr
% unless "rise" is specified this causes problems.
% word_corresponds_to_semnet_relation_GEN(give, verb, to, has_branch).        % x gives rise to y,  tcr
% ----- CARRIES_OUT
%word_corresponds_to_semnet_relation_GEN(carry,verb,out,carries_out).
%word_corresponds_to_semnet_relation_GEN(execute,verb,_,carries_out).
%word_corresponds_to_semnet_relation_GEN(handle,verb,_,carries_out).
%word_corresponds_to_semnet_relation_GEN(operate,verb,on,carries_out).
%word_corresponds_to_semnet_relation_GEN(transact,verb,_,carries_out).
% ----- CAUSES / CAUSED_BY
word_corresponds_to_semnet_relation_GEN(account,verb,for,causes). % GR 10/28/08
%word_corresponds_to_semnet_relation_GEN(aetiology,noun,of,causes). % NOM remove
word_corresponds_to_semnet_relation_GEN(aetiology,noun,_,causes). % NOM add
word_corresponds_to_semnet_relation_GEN(attributable,adj,to,caused_by). % GR  01/11/08
%word_corresponds_to_semnet_relation_GEN('because of',prep,_,caused_by). % GR  01/15/08 
word_corresponds_to_semnet_relation_GEN(causative,adj,of,causes). %  GR 04/29/09
word_corresponds_to_semnet_relation_GEN(causative,adj,_,causes). %  GR 04/29/09
%word_corresponds_to_semnet_relation_GEN(cause,noun,of,causes). % NOM remove
word_corresponds_to_semnet_relation_GEN(cause,noun,_,causes). % NOM add
word_corresponds_to_semnet_relation_GEN(cause,verb,_,causes).
word_corresponds_to_semnet_relation_GEN(causing,verb,_,causes). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(causing,adj,_,caused_by). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(causing,noun,_,caused_by). % GR 09/2015
word_corresponds_to_semnet_relation_GEN(consequence,noun,of,caused_by). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(culminate,verb,in,causes). % GR 05/31/2017 GENIA 7506531_S11  We thus conclude that PTPases culminate in T-lymphocyte activation
word_corresponds_to_semnet_relation_GEN(driven,adj,_,caused_by). % GR 06/02/2017 as it seems pastpart may not work
word_corresponds_to_semnet_relation_GEN(driven,noun,_,caused_by). % GR  01/15/08
word_corresponds_to_semnet_relation_GEN(driven,verb,_,caused_by). % GR 06/02/2017, on 11/20/2017 GR changed POS from pastpart to verb
word_corresponds_to_semnet_relation_GEN(effect,verb,_,causes).
%  word_corresponds_to_semnet_relation_GEN(derive,    verb, _,   causes). % VS  //Bad rule. GR commented it out  01/11/08
word_corresponds_to_semnet_relation_GEN(etiology,noun,in,causes). % GR 02/05/09
word_corresponds_to_semnet_relation_GEN(etiology,noun,for,causes). % GR 02/05/09
%word_corresponds_to_semnet_relation_GEN(etiology,noun,of,causes). % NOM remove
word_corresponds_to_semnet_relation_GEN(etiology,noun,_,causes). % NOM add
word_corresponds_to_semnet_relation_GEN(evoke,verb,_,causes).
%word_corresponds_to_semnet_relation_GEN(genesis,noun,of,causes). % GR suggested and MF and TCR accepted in 09/04/07 % NOM remove
word_corresponds_to_semnet_relation_GEN(genesis,noun,_,causes). % NOM add
word_corresponds_to_semnet_relation_GEN(generate,verb,_,causes). % VS
word_corresponds_to_semnet_relation_GEN(induce,verb,_,causes).
word_corresponds_to_semnet_relation_GEN(induced,adj,_,causes). % GR 06/16/08
word_corresponds_to_semnet_relation_GEN(induced,verb,_,causes). % GR
word_corresponds_to_semnet_relation_GEN(induced,adj,by,caused_by). % on 11/20/2017 GR changed POS from pastpart to adj
word_corresponds_to_semnet_relation_GEN(induced,verb,by,caused_by). %Added on 11/20/2017 GR 
%word_corresponds_to_semnet_relation_GEN(induction,noun,of-by,causes). % NOM remove
word_corresponds_to_semnet_relation_GEN(induction,noun,_,causes). % NOM add
word_corresponds_to_semnet_relation_GEN(lead,verb,to,causes). % VS
%word_corresponds_to_semnet_relation_GEN(mod_head,_,_,causes). % under review
%word_corresponds_to_semnet_relation_GEN(pathogenesis,noun,of,causes). % GR 04/29/09 % NOM remove
word_corresponds_to_semnet_relation_GEN(pathogenesis,noun,_,causes). % NOM add
word_corresponds_to_semnet_relation_GEN(produce,verb,_,causes). % VS
word_corresponds_to_semnet_relation_GEN(provoke,verb,_,causes). % GR  01/11/08
word_corresponds_to_semnet_relation_GEN(result,verb,in,causes). % VS
%word_corresponds_to_semnet_relation_GEN(result,verb,_,causes). % VS  GR commented out 08/05/09
word_corresponds_to_semnet_relation_GEN(resulting,adj,from,caused_by). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(secondary,adj,to,caused_by). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(trigger,verb,_,causes). % VS
%word_corresponds_to_semnet_relation_GEN(trigger,noun,of,causes). % GR suggested  and MF and TCR accepted in 09/04/07 % NOM remove
word_corresponds_to_semnet_relation_GEN(trigger,noun,_,causes). % NOM add
word_corresponds_to_semnet_relation_GEN(trigger,noun,for,causes). % GR 
word_corresponds_to_semnet_relation_GEN(caused,pastpart,by,caused_by). % GR 08/06/09
word_corresponds_to_semnet_relation_GEN(caused,pastpart,_,causes). %GR changed from passive to active on 08/13/09
%word_corresponds_to_semnet_relation_GEN('due to',prep,_,caused_by). % GR
word_corresponds_to_semnet_relation_GEN(due,adj,to,caused_by). %CA added 'due to' multi word. %MF and TCR changed in 09/04/07
%word_corresponds_to_semnet_relation_GEN(cause,noun,of-aux,causes).
% word_corresponds_to_semnet_relation_GEN(mod_head, _,    _,      caused_by).
% These ones come from RESULT_OF predicate. We decided to make them mean CAUSE. RESULT_OF ceases to exist as of 08/29/2007
%word_corresponds_to_semnet_relation_GEN(completion,noun,of,result_of). %MF and TCR decided to exclude in 09/04/07
%word_corresponds_to_semnet_relation_GEN(culmination,noun,of,result_of). %MF and TCR decided to exclude in 09/04/07
%word_corresponds_to_semnet_relation_GEN(effect,noun,of,caused_by). %CA added %MF and TCR decided to keep in 09/04/07 %GR
%word_corresponds_to_semnet_relation_GEN(outcome,noun,of,result_of). %MF and TCR excluded in 09/04/07
%word_corresponds_to_semnet_relation_GEN(product,noun,of,result_of). %MF and TCR decided to include as the inverse produced_by in 09/04/07
%word_corresponds_to_semnet_relation_GEN(result,noun,of,caused_by). %MF and TCR decided to include with inverse caused_by in 09/04/07 % NOM remove
word_corresponds_to_semnet_relation_GEN(result,noun,_,caused_by). % NOM add?
%word_corresponds_to_semnet_relation_GEN(sequel,noun,of,caused_by). %MF and TCR decided to include in 09/04/07 % NOM remove
word_corresponds_to_semnet_relation_GEN(sequel,noun,_,caused_by). % NOM add?
% ----- COMPLICATES
word_corresponds_to_semnet_relation_GEN(aggravate,verb,_,complicates). % GR suggested and MF and TCR accepted in 09/04/07
word_corresponds_to_semnet_relation_GEN(complicate,verb,_,complicates).
word_corresponds_to_semnet_relation_GEN(complicated,pastpart,_,complicates). % tcr
word_corresponds_to_semnet_relation_GEN(worsen,verb,_,complicates). % GR
%word_corresponds_to_semnet_relation_GEN(aggravation,noun,of,complicates). % GR 01/11/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(aggravation,noun,of-by,complicated_by). % GR 01/16/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(aggravation,noun,_,complicates). % NOM add
% ----- CONCEPTUAL PART OF %MF ceases to exist as of 08/29/2007
%word_corresponds_to_semnet_relation_GEN(in,prep,_,conceptual_part_of).
% ----- CONSISTS OF %MF ceases to exist as of 08/29/2007
% word_corresponds_to_semnet_relation_GEN(mod_head, _,    _, consists_of). % tcr-spat-provisional
% word_corresponds_to_semnet_relation_GEN(in,     prep, _, constitutes). % tcr-spat-provisional
% ----- CONTAINS
%word_corresponds_to_semnet_relation_GEN(contain,verb,_,contains).
%word_corresponds_to_semnet_relation_GEN(hold,verb,_,contains).
%% word_corresponds_to_semnet_relation_GEN(is filled with, , , contains).
%% word_corresponds_to_semnet_relation_GEN(is occupied by, , , contains).
% ----- DEGREE OF
%word_corresponds_to_semnet_relation_GEN(degree,noun,of,degree_of). % GR 04/29/09
% ----- DERIVATIVE OF
%%word_corresponds_to_semnet_relation_GEN(derivative, noun, of, derivative_of). % NOM remove
%word_corresponds_to_semnet_relation_GEN(derivative,noun,_,derivative_of). % NOM add
% ----- DIAGNOSES
word_corresponds_to_semnet_relation_GEN(confirm,verb,_,diagnoses). % GR suggested and MF and TCR accepted in 09/04/07
word_corresponds_to_semnet_relation_GEN(demonstrate,verb,_,diagnoses). % GR 01/15/08 
word_corresponds_to_semnet_relation_GEN(detect,verb,_,diagnoses). % GR 01/11/08
%word_corresponds_to_semnet_relation_GEN(detection,noun,of-with,diagnosed_by). % GR 10/28/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(detection,noun,of-by,diagnosed_by). % GR 04/28/09 % NOM remove
%word_corresponds_to_semnet_relation_GEN(detection,noun,of,diagnoses). % GR 10/28/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(detection,noun,_,diagnoses). % NOM add
word_corresponds_to_semnet_relation_GEN(detecting,noun,_,diagnoses). % GR 11/20/2017
word_corresponds_to_semnet_relation_GEN(detecting,verb,_,diagnoses). % GR 11/20/2017
word_corresponds_to_semnet_relation_GEN(determined,adj,by,diagnosed_by). % 05/23/2017 source: GS
word_corresponds_to_semnet_relation_GEN(diagnose,verb,_,diagnoses).
word_corresponds_to_semnet_relation_GEN(diagnosis,noun,_,diagnoses).
word_corresponds_to_semnet_relation_GEN(diagnosed,adj,_,diagnoses). % GR 11/20/2017
word_corresponds_to_semnet_relation_GEN(diagnosed,verb,_,diagnoses).
word_corresponds_to_semnet_relation_GEN(diagnosed,verb,by,diagnosed_by).
word_corresponds_to_semnet_relation_GEN(diagnosing,noun,_,diagnoses). % GR 11/20/2017
word_corresponds_to_semnet_relation_GEN(diagnosing,adj,_,diagnoses). % GR 11/20/2017
word_corresponds_to_semnet_relation_GEN(disclose,verb,_,diagnoses). % GR 01/15/08  
word_corresponds_to_semnet_relation_GEN(identify,verb,_,diagnoses). % GR 10/28/08
%word_corresponds_to_semnet_relation_GEN(identification,noun,of,diagnoses). % GR 10/28/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(identification,noun,_,diagnoses). % NOM add
word_corresponds_to_semnet_relation_GEN(in,prep,_,diagnoses). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(indicative,adj,of,diagnoses). % GR 04/29/09
word_corresponds_to_semnet_relation_GEN(predict,verb,_,diagnoses). % GR 01/15/08 
%word_corresponds_to_semnet_relation_GEN(prediction,noun,of-by,diagnosed_by). % GR 04/15/09 % NOM remove
%word_corresponds_to_semnet_relation_GEN(prediction,noun,of,diagnoses). % GR 01/15/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(prediction,noun,_,diagnoses). % NOM add
word_corresponds_to_semnet_relation_GEN(reveal,verb,_,diagnoses). % tcr
word_corresponds_to_semnet_relation_GEN(show,verb,_,diagnoses). % tcr 
word_corresponds_to_semnet_relation_GEN(screen,verb,_,diagnoses). % GR
word_corresponds_to_semnet_relation_GEN(screen,verb,with,diagnosed_by). % GR
word_corresponds_to_semnet_relation_GEN(screen,verb,for,diagnosed_by). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(screen,verb,for-with,diagnosed_by). % GR
word_corresponds_to_semnet_relation_GEN(screening,noun,for,diagnoses). % GR 10/28/08
%word_corresponds_to_semnet_relation_GEN(screening,noun,for-with,diagnosed_by). % GR 10/28/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(suggestive,adj,of,diagnoses). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(test,verb,for-with,diagnosed_by). % GR 05/06/09
word_corresponds_to_semnet_relation_GEN(test,verb,for,diagnoses). % GR 05/06/09
% GR 12/28/09
%word_corresponds_to_semnet_relation_GEN(diagnosis,noun,of-by,diagnosed_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(predictive,adj,for,diagnoses).
%word_corresponds_to_semnet_relation_GEN(confirmation,noun,of,diagnoses). % NOM remove 
%word_corresponds_to_semnet_relation_GEN(confirmation,noun,of-by,diagnosed_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(confirmation,noun,_,diagnoses). % NOM add
word_corresponds_to_semnet_relation_GEN(compatible,adj,with,diagnoses). % 05/26/2017 source: GS PMID 15763007 Patients, aged between 18 and 48 years, with initial diagnosis compatible with non-obstructive azoospermia
% word_corresponds_to_semnet_relation_GEN(mod_head, _,    _, disrupted_by). % tcr-spat-provisional
% ----- EVALUATION OF
%word_corresponds_to_semnet_relation_GEN(evaluation,noun,of,evaluation_of). % GR commented it out 04/29/09
% ----- EXHIBITS / EXHIBITED BY
%word_corresponds_to_semnet_relation_GEN(demonstrate,verb,_,exhibits).
%word_corresponds_to_semnet_relation_GEN(exhibit,verb,_,exhibits).
%word_corresponds_to_semnet_relation_GEN(show,verb,_,exhibits).
%% word_corresponds_to_semnet_relation_GEN(in,          prep, _, exhibited_by). % tcr-spat-provisional %CA
% ----- HAS ISSUE %MF ceases to exist as of 08/29/2007
%word_corresponds_to_semnet_relation_GEN(mod_head,_,_,has_issue). % tcr-spat-provisional
% ----- HAS METHOD
% word_corresponds_to_semnet_relation_GEN(mod_head, _,    _, has_method). % tcr-spat-provisional
word_corresponds_to_semnet_relation_GEN(by,prep,_,has_method). % tcr-provisional
% ----- INDICATES
%word_corresponds_to_semnet_relation_GEN(indicate,verb,_,indicates).
% ----- INTERACTS WITH
% word_corresponds_to_semnet_relation_GEN(activate,   verb, _,    interacts_with). % tcr-provisional % CA stim
% word_corresponds_to_semnet_relation_GEN(antagonize, verb, _,    interacts_with). % tcr-spat-provisional % CA inhibits
word_corresponds_to_semnet_relation_GEN(bind,verb,to,interacts_with). % tcr-spat-provisional
% word_corresponds_to_semnet_relation_GEN(block,      verb, _,    interacts_with). % tcr--provisional % CA inhibits
% word_corresponds_to_semnet_relation_GEN(increase,   verb, _,    interacts_with). % tcr-spat-provisional % CA nuked this rule
word_corresponds_to_semnet_relation_GEN(infect,verb,_,interacts_with). % tcr-spat-provisional
% word_corresponds_to_semnet_relation_GEN(inhibit,    verb, _,    interacts_with). % tcr-spat-provisional % CA inhibits
word_corresponds_to_semnet_relation_GEN(interact,verb,_,interacts_with). % tcr-spat-provisional %CA with messes either/or
% word_corresponds_to_semnet_relation_GEN(on, prep, _, interacts_with). % tcr-spat-provisional
% word_corresponds_to_semnet_relation_GEN(decrease, verb, _, interacts_with). % tcr-spat-provisional
% ----- INTERCONNECTS
%word_corresponds_to_semnet_relation_GEN(articulate,verb,_,interconnects).
%word_corresponds_to_semnet_relation_GEN(bridge,verb,_,interconnects).
%word_corresponds_to_semnet_relation_GEN(conjoin,verb,_,interconnects).
%word_corresponds_to_semnet_relation_GEN(interconnect,verb,_,interconnects).
%word_corresponds_to_semnet_relation_GEN(join,verb,_,interconnects).
%word_corresponds_to_semnet_relation_GEN(link,verb,_,interconnects).
%word_corresponds_to_semnet_relation_GEN(separate,verb,_,interconnects).
% ----- ISSUE IN %MF ceases to exist as of 08/29/2007
%word_corresponds_to_semnet_relation_GEN(issue,noun,in,issue_in).
% ----- MANAGES / MANAGED BY
%word_corresponds_to_semnet_relation_GEN(in,prep,_,managed_by). % tcr-spat-provisional
%word_corresponds_to_semnet_relation_GEN(manage,verb,_,manages).
% ----- MANIFESTATION OF
%word_corresponds_to_semnet_relation_GEN(causes,noun,of,has_manifestation). % NOM remove?
word_corresponds_to_semnet_relation_GEN(cause,noun,_,has_manifestation). % NOM add?
%word_corresponds_to_semnet_relation_GEN(display,noun,of,manifestation_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(display,noun,_,manifestation_of). % NOM add
%word_corresponds_to_semnet_relation_GEN('due to',prep,_,manifestation_of).
%word_corresponds_to_semnet_relation_GEN(exhibition,noun,of,manifestation_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(exhibition,noun,_,manifestation_of). % NOM add
%word_corresponds_to_semnet_relation_GEN(expression,noun,of,manifestation_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(expression,noun,_,manifestation_of). % NOM add
%word_corresponds_to_semnet_relation_GEN(mechanism,noun,of,manifestation_of). % GR 01/11/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(mechanism,noun,_,manifestation_of). % NOM add
% word_corresponds_to_semnet_relation_GEN(of, prep,_, manifestation_of). % CA - bad rule
% word_corresponds_to_semnet_relation_GEN(of,       prep, _,  manifestation_of). % tcr-spat
% word_corresponds_to_semnet_relation_GEN(in,       prep, _,  manifestation_of). % MF
word_corresponds_to_semnet_relation_GEN(present,verb,as,has_manifestation). % GR 01/11/08
% ----- MEASURES / MEASURED_BY
%%word_corresponds_to_semnet_relation_GEN(measurement, noun, of, measurement_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(assess,verb,_,measured_by). % new GR 06/13/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(assessment,noun,_,measures). % new GR 06/13/2017 as per conversation with Halil
word_corresponds_to_semnet_relation_GEN(determine,verb,_,measures). % new GR 06/13/2017 as per conversation with Halil
word_corresponds_to_semnet_relation_GEN(determined,adj,by,measured_by). % new GR 06/13/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(measurement,noun,_,measured_by). % NOM add - Uncommented by GR 06/13/2017 as per conversation with Halil
word_corresponds_to_semnet_relation_GEN(measure,verb,_,measures). % Uncommented by GR 06/13/2017 as per conversation with Halil
% ----- METHOD OF
%word_corresponds_to_semnet_relation_GEN(method,noun,of,method_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(method,noun,for,method_of). % GR 01/23/2018
word_corresponds_to_semnet_relation_GEN(method,noun,_,method_of). % NOM add
word_corresponds_to_semnet_relation_GEN(for,prep,_,method_of).
% word_corresponds_to_semnet_relation_GEN(mod_head, _,    _,  method_of). % deleted by MF & TCR also deleted in his NP_semrules
% ----- OCCURS_IN / HAS_OCCURRENCE
% word_corresponds_to_semnet_relation_GEN(affect,     verb, _,  occurs_in). % tcr-spat
% word_corresponds_to_semnet_relation_GEN(appear,     verb, in, occurs_in).% CA
% word_corresponds_to_semnet_relation_GEN(exist,      verb, in, occurs_in).% CA
% word_corresponds_to_semnet_relation_GEN(have,       aux,  _,  occurs_in). % <% CA
% word_corresponds_to_semnet_relation_GEN(have,       verb, _,  occurs_in). % <% CA
% word_corresponds_to_semnet_relation_GEN(in,         prep, _,  occurs_in). % tcr-spat% CA
% word_corresponds_to_semnet_relation_GEN(mod_head,   _,    _,  occurs_in). % tcr-spat% CA
% word_corresponds_to_semnet_relation_GEN(occurs,     verb, in, occurs_in).% CA
% word_corresponds_to_semnet_relation_GEN(of,         prep, _,  occurs_in). % tcr-spat% CA
% word_corresponds_to_semnet_relation_GEN(transpire,  verb, _,  occurs_in).% CA
% word_corresponds_to_semnet_relation_GEN(with,       prep, _,  has_occurrence). % <% CA
% word_corresponds_to_semnet_relation_GEN(comes about, % CA
% word_corresponds_to_semnet_relation_GEN(is present in, % CA
% word_corresponds_to_semnet_relation_GEN(develop,  verb, _, has_occurrence). % tcr-spat
% word_corresponds_to_semnet_relation_GEN(die,      verb, _, has_occurrence). % tcr
% word_corresponds_to_semnet_relation_GEN(get,      verb, _, has_occurrence). % tcr
% word_corresponds_to_semnet_relation_GEN(have,     aux,  _, has_occurrence). % tcr
% word_corresponds_to_semnet_relation_GEN(have,     verb, _, has_occurrence). % tcr
% word_corresponds_to_semnet_relation_GEN(mod_head, _,    _, has_occurrence). % tcr-spat
% word_corresponds_to_semnet_relation_GEN(suffer,   verb, _, has_occurrence). % tcr
% word_corresponds_to_semnet_relation_GEN(with,   prep, _, has_occurrence). % tcr-spat
% ----- OCCURS_IN
%word_corresponds_to_semnet_relation_GEN(incidence,noun,of-among,occurs_in). % NOM remove
word_corresponds_to_semnet_relation_GEN(incidence,noun,among,occurs_in). % NOM add
%word_corresponds_to_semnet_relation_GEN(incidence,noun,of-in,occurs_in). % NOM remove
word_corresponds_to_semnet_relation_GEN(incidence,noun,in,occurs_in). % NOM add
%word_corresponds_to_semnet_relation_GEN(occurrence,noun,of-among,occurs_in). % NOM remove
word_corresponds_to_semnet_relation_GEN(occurrence,noun,among,occurs_in). % NOM add
%word_corresponds_to_semnet_relation_GEN(occurrence,noun,of-in,occurs_in). % NOM remove
word_corresponds_to_semnet_relation_GEN(occurrence,noun,in,occurs_in). % NOM add
word_corresponds_to_semnet_relation_GEN(occur,verb,in,occurs_in).
word_corresponds_to_semnet_relation_GEN(occur,verb,among,occurs_in).
%word_corresponds_to_semnet_relation_GEN(predominance,noun,of-among,occurs_in). % NOM remove
word_corresponds_to_semnet_relation_GEN(predominance,noun,among,occurs_in). % NOM add
%word_corresponds_to_semnet_relation_GEN(predominance,noun,of-in,occurs_in). % NOM remove
word_corresponds_to_semnet_relation_GEN(predominance,noun,in,occurs_in). % NOM add
word_corresponds_to_semnet_relation_GEN(predominant,adj,among,occurs_in).
word_corresponds_to_semnet_relation_GEN(predominant,adj,in,occurs_in).
%word_corresponds_to_semnet_relation_GEN(prevalence,noun,of-among,occurs_in). % NOM remove
word_corresponds_to_semnet_relation_GEN(prevalence,noun,among,occurs_in). % NOM add
%word_corresponds_to_semnet_relation_GEN(prevalence,noun,of-in,occurs_in). % NOM remove()
word_corresponds_to_semnet_relation_GEN(prevalence,noun,in,occurs_in). % NOM add
% ----- PRECEDES / FOLLOWS
word_corresponds_to_semnet_relation_GEN(after,prep,_,follows). % tcr
word_corresponds_to_semnet_relation_GEN(precede,verb,_,precedes). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(prior,prep,to,precedes). % GR 06/27/2017 Source: GS: Incubation of Jurkat T cells (1 x 10(6) cells/ml) with a natural thiol antioxidant, alpha-lipoic acid, prior to the stimulation of cells.
word_corresponds_to_semnet_relation_GEN(prior,adj,to,precedes). % GR Negation corpus
word_corresponds_to_semnet_relation_GEN(follow,verb,_,follows). % GR 04/12/2018
word_corresponds_to_semnet_relation_GEN(followed,adj,by,precedes). % GR 04/12/2018
% ----- PROCESS_OF / HAS_PROCESS
word_corresponds_to_semnet_relation_GEN(appear,verb,in,process_of). % tcr
word_corresponds_to_semnet_relation_GEN(exist,verb,in,process_of).
word_corresponds_to_semnet_relation_GEN(experience,verb,_,has_process). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(experiencing,adj,_,has_process). % new GR 06/14/2017 as per conversation with Halil - keep under observation
word_corresponds_to_semnet_relation_GEN(in,prep,_,process_of). % tcr
word_corresponds_to_semnet_relation_GEN(mod_head,_,_,process_of). % tcr
word_corresponds_to_semnet_relation_GEN(develop,verb,_,has_process). % tcr
word_corresponds_to_semnet_relation_GEN(developed,adj,_,has_process). % GR
word_corresponds_to_semnet_relation_GEN(developed,pastpart,_,has_process). % GR
%word_corresponds_to_semnet_relation_GEN(die,verb,_,has_process). % tcr %MF excluded 
word_corresponds_to_semnet_relation_GEN(have,verb,_,has_process). % tcr
word_corresponds_to_semnet_relation_GEN(mod_head,_,_,has_process). % tcr
%word_corresponds_to_semnet_relation_GEN(present,verb,_,has_process).  % GR 01/11/08 then it was commented out in 05/09
word_corresponds_to_semnet_relation_GEN(present,verb,with,has_process). % GR 01/15/08  and GR commented it out on 05/07/09 and uncommented again in 12/2017
word_corresponds_to_semnet_relation_GEN(presenting,adj,with,has_process). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(presenting,verb,with,has_process). % GR /12/01/2017
word_corresponds_to_semnet_relation_GEN(presented,verb,with,has_process). % GR /12/01/2017
word_corresponds_to_semnet_relation_GEN(recuperate,verb,from,has_process). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(suffer,verb,_,has_process). % tcr
word_corresponds_to_semnet_relation_GEN(suffer,verb,from,has_process). % GR 01/15/08 
word_corresponds_to_semnet_relation_GEN(sustain,verb,_,has_process). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(sustaining,adj,_,has_process). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(treated,adj,for,has_process). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(treated,verb,for,has_process). % GR /11/20/()2017
word_corresponds_to_semnet_relation_GEN(with,prep,_,has_process). % tcr
word_corresponds_to_semnet_relation_GEN(have,aux,_,has_process). % tcr CA excluded?
%word_corresponds_to_semnet_relation_GEN(of,prep,_,_). %MF and TCR excluded in 09/04/07
% ----- PART_OF / HAS_PART
word_corresponds_to_semnet_relation_GEN(component,noun,of,part_of). % NOM untouched
word_corresponds_to_semnet_relation_GEN(division,noun,of,part_of). % NOM untouched
word_corresponds_to_semnet_relation_GEN(fragment,noun,of,part_of). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(in,prep,_,part_of). % tcr-spat-provisional %GR? %MF and TCR decided to exclude in 09/04/07 
word_corresponds_to_semnet_relation_GEN(layer,noun,of,part_of). % NOM untouched
word_corresponds_to_semnet_relation_GEN(of,prep,_,part_of). % tcr-spat-provisional %GR? %MF and TCR decided to keep in 09/04/07 
word_corresponds_to_semnet_relation_GEN(part,noun,of,part_of). % NOM untouched
word_corresponds_to_semnet_relation_GEN(portion,noun,of,part_of). % NOM untouched
word_corresponds_to_semnet_relation_GEN(section,noun,of,part_of). % NOM untouched
word_corresponds_to_semnet_relation_GEN(contain,verb,_,has_part). % tcr-spat-provisional
word_corresponds_to_semnet_relation_GEN(mod_head,_,_,has_part). % tcr-spat
word_corresponds_to_semnet_relation_GEN(with,prep,_,has_part). % tcr-spat %GR?
% ----- PRACTICES
%word_corresponds_to_semnet_relation_GEN(practice,verb,_,practices).
%----PREVENTS % was an extinguished predicate by Caroline. %MF ressuciated it 09/04/07. Lots of IR are still taken out as Caroline did such as reduce and decrease. I don't think they mean prevent either. But are very close. Maybe another predicate. For now I am liberating them to mean it.  
word_corresponds_to_semnet_relation_GEN(decrease,verb,_,prevents). %AR 
word_corresponds_to_semnet_relation_GEN(decreasing,noun,_,prevents). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(decreasing,adj,_,prevents). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(decrease,verb,from,prevents). %AR
word_corresponds_to_semnet_relation_GEN(decrease,verb,of,prevents). %AR
%word_corresponds_to_semnet_relation_GEN(immunization,noun,against,prevents). % GR 04/29/09 % NOM remove
word_corresponds_to_semnet_relation_GEN(immunization,noun,_,prevents). % NOM add
word_corresponds_to_semnet_relation_GEN(immunize,verb,against,prevents). % GR 04/29/09
word_corresponds_to_semnet_relation_GEN(minimize,verb,_,prevents). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(neuroprotective,adj,for,prevents). % GR 05/24/2017 source: GS PMID 15473914
word_corresponds_to_semnet_relation_GEN(neuroprotective,adj,_,prevents). % GR 05/24/2017 source: GS PMID 15473914
word_corresponds_to_semnet_relation_GEN(prevent,verb,_,prevents). % CA - disrupts %MF put it back
word_corresponds_to_semnet_relation_GEN(preventing,noun,_,prevents). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(preventing,adj,_,prevents). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(prevention,noun,_,prevents). %AR
%word_corresponds_to_semnet_relation_GEN(prevention,noun,of,prevents). %AR % NOM remove
%word_corresponds_to_semnet_relation_GEN(prevention,noun,of-by,prevents). %MF % NOM remove
%word_corresponds_to_semnet_relation_GEN(prevention,noun,of-with,prevents). %MF % NOM remove
word_corresponds_to_semnet_relation_GEN(protect,verb,from,prevents). %MF This was the only rule for prevents with CA.  MF moved it to be here.
word_corresponds_to_semnet_relation_GEN(protect,verb,against,prevents). % GR % CA - disrupts %MF put it back
word_corresponds_to_semnet_relation_GEN(protecting,noun,against,prevents). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(protecting,adj,against,prevents). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(protecting,noun,from,prevents). % GR 11/20/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(protecting,adj,from,prevents). % GR 11/20/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(protective,adj,for,prevents). % GR 10/26/2017
word_corresponds_to_semnet_relation_GEN(protective,adj,_,prevents). % GR 10/26/2017
word_corresponds_to_semnet_relation_GEN(reduce,verb,_,prevents). %AR
word_corresponds_to_semnet_relation_GEN(reducing,noun,_,prevents). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(reducing,adj,_,prevents). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(reduce,verb,among,prevents). %AR
word_corresponds_to_semnet_relation_GEN(reduce,verb,from,prevents). %AR
word_corresponds_to_semnet_relation_GEN(reduce,verb,of,prevents). %AR
word_corresponds_to_semnet_relation_GEN(reduction,noun,_,prevents). %AR
%word_corresponds_to_semnet_relation_GEN(reduction,noun,of,prevents). %AR % NOM remove
%%word_corresponds_to_semnet_relation_GEN(effect,noun,against,prevents). % GR 06/04/08  Provisionally blocked Feb 09
word_corresponds_to_semnet_relation_GEN(immunity,noun,against,prevents). % GR 06/16/08
%word_corresponds_to_semnet_relation_GEN(protection,noun,against,prevents). % GR 06/16/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(protection,noun,_,prevents). % NOM add
word_corresponds_to_semnet_relation_GEN(spare,verb,_,prevents). % GR 06/13/2017
% ----- PRODUCES
word_corresponds_to_semnet_relation_GEN(biosynthesize,verb,_,produces).
word_corresponds_to_semnet_relation_GEN(biosynthesis,noun,_,produced_by). % GR 06/14/2017 motivated by verb biosynthesize - keep under observation
word_corresponds_to_semnet_relation_GEN(create,verb,_,produces).
word_corresponds_to_semnet_relation_GEN(discharge,verb,_,produces).
word_corresponds_to_semnet_relation_GEN(emit,verb,_,produces).
word_corresponds_to_semnet_relation_GEN(express,verb,_,produces). % GR 06/10/09 commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(expression,noun,in,produces). % GR 07/2015 commented out by GR on 06/23/2017 as per conversation with Halil
word_corresponds_to_semnet_relation_GEN(generate,verb,_,produces).
word_corresponds_to_semnet_relation_GEN(generating,noun,_,produces). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(generating,adj,_,produces). % GR 11/16/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(overexpress,verb,_,produces). % GR 07/2015 commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(overexpressing,adj,_,produced_by). % GR 06/14/2017 motivated by verb overexpress - commented out by GR on 06/23/2017 as per conversation with Halil
% Commented out per GR 12/17/2015 e-mail
% word_corresponds_to_semnet_relation_GEN(overexpression,noun,_,produces). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(release,verb,_,produces).
word_corresponds_to_semnet_relation_GEN(releasing,noun,_,produces). % GR 11/20/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(releasing,adj,_,produces). % GR 11/20/2017 as per fix of Bug 32
word_corresponds_to_semnet_relation_GEN(secrete,verb,_,produces).
word_corresponds_to_semnet_relation_GEN(synthesize,verb,_,produces). % GR 05/24/2017 motivated by noun synthesis PMID 28469708 and 20469705
word_corresponds_to_semnet_relation_GEN(yield,verb,_,produces).
word_corresponds_to_semnet_relation_GEN(produce,verb,_,produces). %MF TCR added produce verb here "MAPK produces p42-MAPK protein" works in 09/04/2007
word_corresponds_to_semnet_relation_GEN(producing,adj,_,produced_by). % GR 06/14/2017 motivated by verb produce - keep under observation
%word_corresponds_to_semnet_relation_GEN(product,noun,of,produced_by). %MF TCR from result_of as produced_by "p42-MAPK protein a product of MAPK" in 09/04/2007 % NOM remove
word_corresponds_to_semnet_relation_GEN(product,noun,_,produced_by).
word_corresponds_to_semnet_relation_GEN(synthesis,noun,_,produced_by). % GR 05/24/2017 motivated by PMID 28469708 and 20469705
% word_corresponds_to_semnet_relation_GEN(Brings forth, 
% ----- PROPERTY OF
%word_corresponds_to_semnet_relation_GEN(in,prep,_,property_of).
%word_corresponds_to_semnet_relation_GEN(mod_head,_,_,property_of). %GR? %MF and TCR decided to keep in 09/04/07
%word_corresponds_to_semnet_relation_GEN(property,noun,of,property_of). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(with,prep,_,property_of). % GR 01/11/08
% ----- RESULT_OF / HAS_RESULT This has been collpased with causes. Some indicator rules were excluded
%word_corresponds_to_semnet_relation_GEN(completion,noun,of,result_of).
%word_corresponds_to_semnet_relation_GEN(culmination,noun,of,result_of).
%word_corresponds_to_semnet_relation_GEN(effect,noun,of,result_of). % CA
%word_corresponds_to_semnet_relation_GEN(outcome,noun,of,result_of).
%word_corresponds_to_semnet_relation_GEN(product,noun,of,result_of).
%word_corresponds_to_semnet_relation_GEN(result,noun,of,result_of).
%word_corresponds_to_semnet_relation_GEN(sequel,noun,of,result_of).
%word_corresponds_to_semnet_relation_GEN(result,verb,in,has_result).
%%word_corresponds_to_semnet_relation_GEN(induce,     verb, _,  has_result). % tcr-provisional
% ----- SURROUNDS / SURROUNDED_BY
%word_corresponds_to_semnet_relation_GEN(bound,verb,_,surrounds).
%word_corresponds_to_semnet_relation_GEN(circumscribe,verb,_,surrounds).
%word_corresponds_to_semnet_relation_GEN(confine,verb,_,surrounds).
%word_corresponds_to_semnet_relation_GEN(enclose,verb,_,surrounds).
%%word_corresponds_to_semnet_relation_GEN(limit,       verb, _, surrounds). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(surround,verb,_,surrounds).
%%word_corresponds_to_semnet_relation_GEN(in,          prep, _, surrounded_by). % GR commented it out
% ----- TREATS / TREATED BY
word_corresponds_to_semnet_relation_GEN(alleviate,verb,_,treats). % tcr-provisional
word_corresponds_to_semnet_relation_GEN(ameliorate,verb,_,treats). % GR
%word_corresponds_to_semnet_relation_GEN(amelioration,noun,of,treats). % GR 01/15/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(amelioration,noun,_,treats). % NOM add
%word_corresponds_to_semnet_relation_GEN(among,prep,_,treats). % AR added 4/8 %MF and TCR excluded in 09/04/07
word_corresponds_to_semnet_relation_GEN(attenuate,verb,_,treats). % MF
%word_corresponds_to_semnet_relation_GEN(attenuation,noun,of-by,treated_by). % GR 01/15/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(attenuation,noun,of,treats). % GR 01/15/08  % NOM remove
word_corresponds_to_semnet_relation_GEN(attenuation,noun,_,treats). % NOM add
word_corresponds_to_semnet_relation_GEN(benefit,verb,from,treated_by). % GR 04/29/09
word_corresponds_to_semnet_relation_GEN(combat,verb,_,treats). % GR
word_corresponds_to_semnet_relation_GEN(control,verb,_,treats). % GR
%word_corresponds_to_semnet_relation_GEN(control,noun,of,treats). % GR 01/15/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(control,noun,_,treats). % NOM add
word_corresponds_to_semnet_relation_GEN(cure,verb,_,treats).
word_corresponds_to_semnet_relation_GEN(ease,verb,_,treats). % GR
% word_corresponds_to_semnet_relation_GEN(effect,     noun,     of-on,      treats). % tcr-provisional %CA - bad rule
word_corresponds_to_semnet_relation_GEN(eliminate,verb,_,treats). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(elimination,noun,_,treats). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(eradicate,verb,_,treats). % 05/23/2017 source: GS
word_corresponds_to_semnet_relation_GEN(eradication,noun,_,treats).
word_corresponds_to_semnet_relation_GEN(fight,verb,_,treats). % GR
word_corresponds_to_semnet_relation_GEN(for,prep,_,treats).
word_corresponds_to_semnet_relation_GEN(generate,verb,against,treats). % GR keep under observation 06/17/08
word_corresponds_to_semnet_relation_GEN(heal,verb,_,treats). % GR
word_corresponds_to_semnet_relation_GEN(improve,verb,of,treats). % MF 
word_corresponds_to_semnet_relation_GEN(improve,verb,_,treats). % CA
%word_corresponds_to_semnet_relation_GEN(improvement,noun,of-with,treats). % GR 01/15/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(improvement,noun,of,treats). % GR 01/15/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(improvement,noun,_,treats). % GR 01/15/08 % NOM add
word_corresponds_to_semnet_relation_GEN(indicate,verb,for,treats). % GR 11/20/2017 to circumvent right-association rule
word_corresponds_to_semnet_relation_GEN(indicated,adj,for,treats). % GR 11/16/2017 to circumvent right-association rule
word_corresponds_to_semnet_relation_GEN(indicated,verb,for,treats). % GR 11/20/2017 to circumvent right-association rule
%word_corresponds_to_semnet_relation_GEN(in,prep,_,treats). % dangerous rule
word_corresponds_to_semnet_relation_GEN(manage,verb,_,treats). % tcr
word_corresponds_to_semnet_relation_GEN(mitigate,verb,_,treats). %GR suggested MF TCR accepted in 09/04/2007
%word_corresponds_to_semnet_relation_GEN(mitigation,noun,of-by,treated_by). % GR 01/15/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(mitigation,noun,of-with,treated_by). % GR 01/15/09 % NOM remove
%word_corresponds_to_semnet_relation_GEN(mitigation,noun,of,treats). % GR 01/15/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(mitigation,noun,_,treats). % GR 01/15/08 % NOM add
%word_corresponds_to_semnet_relation_GEN(management,noun,of-with,treats). % NOM remove
word_corresponds_to_semnet_relation_GEN(management,noun,_,treats). % NOM add
%word_corresponds_to_semnet_relation_GEN(on,prep,_,treats). % MF dangerous rule
word_corresponds_to_semnet_relation_GEN(prescribe,verb,_,treats). % GR 11/16/2017 
word_corresponds_to_semnet_relation_GEN(prescribed,adj,for,treats). % GR 11/16/2017 to circumvent right-association rule
word_corresponds_to_semnet_relation_GEN(pretreat,verb,_,treats). % GR 01/15/08
word_corresponds_to_semnet_relation_GEN(pretreat,verb,with,treated_by). % GR 01/15/08
word_corresponds_to_semnet_relation_GEN(pretreatment,noun,with,treated_by). % GR 01/15/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(pretreatment,noun,of-with,treats). % GR 01/15/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(pretreatment,noun,_,treats). % GR 01/15/08 % NOM add
%word_corresponds_to_semnet_relation_GEN(relief,noun,of-by,treats). % GR 01/15/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(relief,noun,of-with,treats). % GR 01/15/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(relief,noun,of,treats). % GR 01/15/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(relief,noun,_,treats). % GR 01/15/08 % NOM add
word_corresponds_to_semnet_relation_GEN(relieve,verb,_,treats). % tcr-provisional
word_corresponds_to_semnet_relation_GEN(resolve,verb,_,treats). % GR to complete this paradigm (which follows)
word_corresponds_to_semnet_relation_GEN(resolution,noun,of-by,treats). % GR-provisional % NOM untouched % into problem
word_corresponds_to_semnet_relation_GEN(resolution,noun,of,treats). % GR provisional % NOM untouched %into problem
word_corresponds_to_semnet_relation_GEN(reverse,verb,_,treats). % GR
word_corresponds_to_semnet_relation_GEN(therapy,noun,for,treats). % AR
word_corresponds_to_semnet_relation_GEN(treat,verb,with,treated_by). % CA added
word_corresponds_to_semnet_relation_GEN(treat,verb,_,treats).
word_corresponds_to_semnet_relation_GEN(treated,adj,with,treated_by). % GR 11/16/2017 changed POS from paspart to adj
word_corresponds_to_semnet_relation_GEN(treated,verb,_,treats).
word_corresponds_to_semnet_relation_GEN(treated,pastpart,_,treats).
word_corresponds_to_semnet_relation_GEN(treated,adj,_,treated_by). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(treating,adj,with,treated_by).
word_corresponds_to_semnet_relation_GEN(treatment,noun,with,treated_by).
word_corresponds_to_semnet_relation_GEN(treatment,noun,_,treats).
%word_corresponds_to_semnet_relation_GEN(treatment,noun,for,treats). % AR % NOM remove
%word_corresponds_to_semnet_relation_GEN(treatment,noun,for-with,treats). % AR Would this work? % NOM remove
%word_corresponds_to_semnet_relation_GEN(treatment,noun,of,treats). % NOM remove
%word_corresponds_to_semnet_relation_GEN(treatment,noun,of-by,treats). % AR w/tcr % NOM remove
%word_corresponds_to_semnet_relation_GEN(treatment,noun,of-with,treats). % AR w/tcr % NOM remove
word_corresponds_to_semnet_relation_GEN(therapy,noun,with,treated_by). % tcr % NOM remove
word_corresponds_to_semnet_relation_GEN(therapy,noun,_,treats). % tcr % NOM add
%word_corresponds_to_semnet_relation_GEN(treatment,noun,with,treated_by). % AR % NOM remove
%word_corresponds_to_semnet_relation_GEN(treatment,noun,with,treated_by). % AR 4/10 concl.s. % NOM remove 
%word_corresponds_to_semnet_relation_GEN(treatment,noun,with-among,treated_by). % AR 4/10  % NOM remove
word_corresponds_to_semnet_relation_GEN(treatment,noun,among,treats). % AR 4/10  % NOM add
%word_corresponds_to_semnet_relation_GEN(treatment,noun,with-for,treated_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(treatment,noun,with-in,treated_by). % AR 4/10 no example yet % NOM remove
word_corresponds_to_semnet_relation_GEN(treatment,noun,in,treats). % AR 4/10 no example yet % NOM add
%word_corresponds_to_semnet_relation_GEN(treatment,noun,with-of,treated_by). % AR 4/10 concl.s.70 % NOM remove
word_corresponds_to_semnet_relation_GEN(undergo,verb,_,treated_by). % tcr-provisional
word_corresponds_to_semnet_relation_GEN(undergoing,adj,_,treated_by). % tcr-provisional
% word_corresponds_to_semnet_relation_GEN(for,      prep,     with,       treats). % AR
% word_corresponds_to_semnet_relation_GEN(of,       prep,     _,          treats). % MF dangerous rule
% word_corresponds_to_semnet_relation_GEN(to,       prep,     _,          treats). % MF dangerous rule
% word_corresponds_to_semnet_relation_GEN(against,  prep,     _,          treats). % MF dangerous rule
% word_corresponds_to_semnet_relation_GEN(reduce,   verb,     _,          treats). % AR
% ----- USES / USED_BY
word_corresponds_to_semnet_relation_GEN(administer,verb,_,uses). % tcr-spat 
word_corresponds_to_semnet_relation_GEN(apply,verb,_,uses).
word_corresponds_to_semnet_relation_GEN(avail,verb,_,uses).
word_corresponds_to_semnet_relation_GEN(employ,verb,_,uses).
word_corresponds_to_semnet_relation_GEN(use,verb,_,uses). 
word_corresponds_to_semnet_relation_GEN(use,verb,as,uses). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(utilize,verb,_,uses).
word_corresponds_to_semnet_relation_GEN(with,prep,_,uses). % tcr-spat-provisional
% word_corresponds_to_semnet_relation_GEN(of,       prep, _, uses). % tcr-spat-provisional
word_corresponds_to_semnet_relation_GEN(for,prep,_,used_by). % tcr-spat
%%% word_corresponds_to_semnet_relation_GEN(in,         prep, _, used_by). % tcr-spat-provisional
word_corresponds_to_semnet_relation_GEN(mod_head,_,_,used_by). % tcr-spat
%%%% Caroline's additions for Pharmacogenomic SemRep %%%%%%
% ADMINISTERED_TO/HAS_ADMINISTRATION
word_corresponds_to_semnet_relation_GEN(administer,verb,to,administered_to).
% word_corresponds_to_semnet_relation_GEN(administered,adj,_,administered_to).
word_corresponds_to_semnet_relation_GEN(administered,adj,_,has_administration).
word_corresponds_to_semnet_relation_GEN(administer,verb,to,administered_to).
word_corresponds_to_semnet_relation_GEN(administer,verb,_,has_administration).
%word_corresponds_to_semnet_relation_GEN(administration,noun,to,administered_to). % NOM remove
word_corresponds_to_semnet_relation_GEN(administration,noun,_,administered_to). % NOM add
word_corresponds_to_semnet_relation_GEN(cotransfect,verb,_,administered_to). % GR 06/01/2017 source: GENIA 
word_corresponds_to_semnet_relation_GEN(cotransfected,adj,with,has_administration). % GR 06/23/2017 source: GENIA 9352360_S4
word_corresponds_to_semnet_relation_GEN(cotransfection,noun,_,has_administration). % GR 06/01/2017 source: GENIA 7486667_S2 
word_corresponds_to_semnet_relation_GEN(deliver,verb,to,administered_to). % GR 06/01/2017 source: GENIA 7486667_S2 The transcriptional activity of the IL-2 promoter requires T-cell costimulation delivered by the TCR and the auxiliary receptor CD28.
word_corresponds_to_semnet_relation_GEN(delivered,adj,to,administered_to). % GR 06/22/2017 source: GENIA 
word_corresponds_to_semnet_relation_GEN(give,verb,to,administered_to).
word_corresponds_to_semnet_relation_GEN(give,verb,_,has_administration).
word_corresponds_to_semnet_relation_GEN(given,adj,to,administered_to).
word_corresponds_to_semnet_relation_GEN(given,adj,_,has_administration).
%word_corresponds_to_semnet_relation_GEN(given,noun,_,has_administration). % GR commented it out
word_corresponds_to_semnet_relation_GEN(have,verb,_,has_administration). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(immunize,verb,with,has_administration). % GR 06/14/2017 
word_corresponds_to_semnet_relation_GEN(immunization,noun,with,has_administration). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(inject,verb,with,has_administration). % GR 12/11/2017 
word_corresponds_to_semnet_relation_GEN(inject,verb,_,administered_to). % GR 12/11/2017 
word_corresponds_to_semnet_relation_GEN(injected,adj,with,has_administration). % GR 12/11/2017 
%word_corresponds_to_semnet_relation_GEN(injection,noun,of-in,administered_to). % NOM remove
word_corresponds_to_semnet_relation_GEN(injection,noun,in,administered_to). % NOM add
word_corresponds_to_semnet_relation_GEN(infuse,verb,into,administered_to). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(infuse,verb,with,has_administration). % GR 06/14/2017 
%word_corresponds_to_semnet_relation_GEN(infusion,noun,of-to,administered_to). % NOM remove
word_corresponds_to_semnet_relation_GEN(infusion,noun,to,administered_to). % NOM add
word_corresponds_to_semnet_relation_GEN(into,prep,_,administered_to).
word_corresponds_to_semnet_relation_GEN(incubate,verb,with,administered_to). % GR 05/25/2017 source: GS & GENIA - PMID 15561572; 8605587_S6; 9974401_S3 and more
word_corresponds_to_semnet_relation_GEN(incubation,noun,with,has_administration). % GR 05/25/2017 source: GS & GENIA - PMID 15561572; 8605587_S6; 9974401_S3 and more
word_corresponds_to_semnet_relation_GEN(inoculate,verb,with,administered_to). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(inoculation,noun,with,has_administration). % GR 07/2015
%word_corresponds_to_semnet_relation_GEN(management,noun,with,has_administration). %GR 01/11/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(management,noun,_,administered_to).%GR 01/11/08 % NOM add
%word_corresponds_to_semnet_relation_GEN(metabolizer,noun,_,has_administration). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(metabolizer,noun,_,administered_to). % GR commented it out
word_corresponds_to_semnet_relation_GEN(monitor,verb,with,administered_to). % GR 01/15/08
%word_corresponds_to_semnet_relation_GEN(perform,verb,_,has_administration). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(perform,verb,in,administered_to). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(perform,verb,on,administered_to). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(perform,verb,_,has_administration). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(randomize,verb,to,administered_to). % GR 01/11/08 It may need to be constrained
word_corresponds_to_semnet_relation_GEN(receive,verb,_,has_administration).
word_corresponds_to_semnet_relation_GEN(receiving,verb,_,has_administration). 
word_corresponds_to_semnet_relation_GEN(receiving,adj,_,has_administration). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(receiving,noun,_,has_administration). % GR 08/2018
word_corresponds_to_semnet_relation_GEN(submit,verb,to,has_administration). % GR 05/24/2017 source: GS PMID 15488431
word_corresponds_to_semnet_relation_GEN(take,verb,_,has_administration).
word_corresponds_to_semnet_relation_GEN(taking,verb,_,has_administration).
word_corresponds_to_semnet_relation_GEN(taking,adj,_,has_administration). % GR 06/14/2017 
word_corresponds_to_semnet_relation_GEN(test,verb,with,administered_to).
word_corresponds_to_semnet_relation_GEN(test,verb,with,has_administration).
word_corresponds_to_semnet_relation_GEN(transfect,verb,with,has_administration). 
word_corresponds_to_semnet_relation_GEN(transfect,verb,_,administered_to). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(transfection,noun,with,has_administration). 
word_corresponds_to_semnet_relation_GEN(transfection,noun,_,has_administration). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(undergo,verb,_,has_administration). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(undergoing,adj,_,has_administration). % GR 08/22/0
word_corresponds_to_semnet_relation_GEN(underwent,verb,_,has_administration). % GR 01/15/08 Problem with the past tense
word_corresponds_to_semnet_relation_GEN(use,verb,_,has_administration).
word_corresponds_to_semnet_relation_GEN(using,adj,_,has_administration).
word_corresponds_to_semnet_relation_GEN(vaccinate,verb,with,has_administration). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(vaccination,noun,with,has_administration). % GR 06/13/2017  per conversation with Halil
word_corresponds_to_semnet_relation_GEN(vaccination,noun,_,administered_to). % GR 06/13/2017  per conversation with Halil
% word_corresponds_to_semnet_relation_GEN(with,prep,_,has_administration).
%word_corresponds_to_semnet_relation_GEN(mod_head,_,_,administered_to). % GR 04/28/09 provisionally % Commented out based on email exchange on 8/13/12
% ASSOCIATED_WITH
word_corresponds_to_semnet_relation_GEN(associate,verb,_,associated_with_r). % GR
word_corresponds_to_semnet_relation_GEN(in,prep,_,associated_with). % tcr-spat
word_corresponds_to_semnet_relation_GEN(associate,verb,_,associated_with). % provisional.
word_corresponds_to_semnet_relation_GEN(associate,verb,with,associated_with). % GR  01/11/08
word_corresponds_to_semnet_relation_GEN(associated,adj,with,associated_with). % GR 01/24/2018 
%word_corresponds_to_semnet_relation_GEN(association,noun,of-with,associated_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(association,noun,of,associated_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(association,noun,with,associated_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(association,noun,_,associated_with). % NOM add 
%word_corresponds_to_semnet_relation_GEN(associations,noun,with,associated_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(association,noun,with-in,associated_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(association,noun,in,associated_with). % NOM add
word_corresponds_to_semnet_relation_GEN(association,noun,with-with,associated_with). % NOM untouched  % Strange one
word_corresponds_to_semnet_relation_GEN(association,noun,between-and,associated_with). % GR new 10/26/2017
word_corresponds_to_semnet_relation_GEN(association,noun,between-and,associated_with_r). % GR new 10/26/2017
%word_corresponds_to_semnet_relation_GEN('candidate gene',noun,in,associated_with). % GR commented it out
%word_corresponds_to_semnet_relation_GEN('candidate gene',noun,for,associated_with). % GR commented it out
word_corresponds_to_semnet_relation_GEN(carrier,noun,with,associated_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(characterise,verb,_,associated_with).
word_corresponds_to_semnet_relation_GEN(characterize,verb,_,associated_with). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(compatible,adj,with,associated_with). %GR 01/11/08
word_corresponds_to_semnet_relation_GEN(correlate,verb,with,associated_with). % GR  01/11/08
%word_corresponds_to_semnet_relation_GEN(correlation,noun,of-with,associated_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(correlation,noun,_,associated_with). % NOM add
word_corresponds_to_semnet_relation_GEN(correlation,noun,between-and,associated_with). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(correlation,noun,between-and,associated_with_r). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(crosslink,verb,_,associated_with). % GR 06/01/2017 Source: GENIA 7525701_S16 The T cell tropic strain of HIV, LAI, does not replicate in naive CD4 T cells stimulated by cross-linking CD3 and CD28.               
word_corresponds_to_semnet_relation_GEN(crosslink,noun,between-and,associated_with). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(crosslink,noun,between-and,associated_with_r). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(crosslink,noun,_,associated_with). 
word_corresponds_to_semnet_relation_GEN(decrease,verb,in,associated_with). %decreased in phenomenon
word_corresponds_to_semnet_relation_GEN(decrease,verb,in,associated_with_r).
word_corresponds_to_semnet_relation_GEN(decreased,verb,in,associated_with).
word_corresponds_to_semnet_relation_GEN(decreased,verb,in,associated_with_r).
%word_corresponds_to_semnet_relation_GEN('genetic marker',noun,for,associated_with). % GR commented it out
word_corresponds_to_semnet_relation_GEN(interplay,noun,_,associated_with). % GR 06/01/2017 Source: GENIA 10329626_S11 The potential for an interplay between the Notch and NF-kappaB signaling pathways in the immune system.               
word_corresponds_to_semnet_relation_GEN(interplay,noun,between-and,associated_with). % GR new 10/27/2017 GENIA 10329626_S11 The potential for an interplay between the Notch and NF-kappaB signaling pathways in the immune system. 
word_corresponds_to_semnet_relation_GEN(interplay,noun,between-and,associated_with_r). % GR new 10/27/2017 GENIA 10329626_S11 The potential for an interplay between the Notch and NF-kappaB signaling pathways in the immune system. 
word_corresponds_to_semnet_relation_GEN(involve,verb,in,associated_with).
word_corresponds_to_semnet_relation_GEN(involve,verb,_,associated_with).
word_corresponds_to_semnet_relation_GEN(involved,pastpart,in,associated_with).
word_corresponds_to_semnet_relation_GEN('in relation to',prep,_,associated_with).
word_corresponds_to_semnet_relation_GEN(incidence,noun,of-with,associated_with). % NOM untouched % by problem
%word_corresponds_to_semnet_relation_GEN(influence,noun,on,associated_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(influence,noun,_,associated_with). % NOM add
word_corresponds_to_semnet_relation_GEN(influence,verb,_,associated_with).
word_corresponds_to_semnet_relation_GEN(link,verb,with,associated_with).
word_corresponds_to_semnet_relation_GEN(link,verb,to,associated_with).
word_corresponds_to_semnet_relation_GEN(link,noun,to,associated_with). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(link,noun,with,associated_with). % GR 01/11/08 % NOM remove?
word_corresponds_to_semnet_relation_GEN(link,noun,_,associated_with). % GR 01/11/08 % NOM add?
word_corresponds_to_semnet_relation_GEN(link,noun,between-and,associated_with). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(link,noun,between-and,associated_with_r). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(linkage,noun,to,associated_with). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(linkage,noun,with,associated_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(linkage,noun,_,associated_with). % NOM add
%word_corresponds_to_semnet_relation_GEN(marker,noun,of,associated_with).
%word_corresponds_to_semnet_relation_GEN(marker,noun,for,associated_with).
word_corresponds_to_semnet_relation_GEN(mediated,pastpart,_,associated_with).
word_corresponds_to_semnet_relation_GEN(mediate,verb,_,associated_with).
word_corresponds_to_semnet_relation_GEN(mediation,verb,of-by,associated_with_r).
word_corresponds_to_semnet_relation_GEN(occur,verb,with,associated_with).
word_corresponds_to_semnet_relation_GEN(occurrence,noun,of-with,associated_with). % NOM untouched % by problem
%word_corresponds_to_semnet_relation_GEN(of,prep,_,associated_with). %MF and TCR excluded in 09/04/07 
word_corresponds_to_semnet_relation_GEN(parallel,verb,_,associated_with). % GR 06/02/2017 GENIA 7542286_S5 This inhibition was paralleled by reduced monocyte adhesion to endothelial monolayers in nonstatic assays
word_corresponds_to_semnet_relation_GEN(relate,verb,with,associated_with).
word_corresponds_to_semnet_relation_GEN(relate,verb,to,associated_with).
word_corresponds_to_semnet_relation_GEN(related,pastpart,with,associated_with).
word_corresponds_to_semnet_relation_GEN(related,pastpart,to,associated_with).
word_corresponds_to_semnet_relation_GEN(relating,pastpart,to,associated_with).
%word_corresponds_to_semnet_relation_GEN(relation,noun,of-with,associated_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(relation,noun,with,associated_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(relation,noun,to,associated_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(relation,noun,between-and,associated_with). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(relation,noun,between-and,associated_with_r). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(relation,noun,_,associated_with). % NOM add
word_corresponds_to_semnet_relation_GEN(role,noun,against,associated_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(relating,pastpart,to,associated_with).
word_corresponds_to_semnet_relation_GEN(in,prep,_,associated_with).
word_corresponds_to_semnet_relation_GEN(relate,verb,with,associated_with_r).
word_corresponds_to_semnet_relation_GEN(restore,verb,_,associated_with). %GR 01/11/08
word_corresponds_to_semnet_relation_GEN(trigger,verb,to,associated_with).
word_corresponds_to_semnet_relation_GEN(role,noun,for-in,associated_with_r). % GR  06/25/08 % NOM untouched % associated_with or associated_with_r
% GR word_corresponds_to_semnet_relation_GEN(role,noun,for-in,associated_with). % CA: old tcr rule for co-occurs_with
%word_corresponds_to_semnet_relation_GEN(role,noun,of-in,associated_with). % CA: old tcr rule for co-occurs_with % NOM remove
word_corresponds_to_semnet_relation_GEN(role,noun,in,associated_with). % CA: old tcr rule for co-occurs_with
word_corresponds_to_semnet_relation_GEN(involve,verb,_,associated_with_r).
word_corresponds_to_semnet_relation_GEN(linked,pastpart,to,associated_with_r).
%word_corresponds_to_semnet_relation_GEN(relation,noun,with,associated_with_r). % NOM remove
%word_corresponds_to_semnet_relation_GEN(relationship,noun,of-with,associated_with_r). % NOM remove
word_corresponds_to_semnet_relation_GEN(relationship,noun,between-and,associated_with). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(relationship,noun,between-and,associated_with_r). % GR new 10/27/2017
word_corresponds_to_semnet_relation_GEN(relationship,noun,_,associated_with). % NOM add
% A_W for GENOME
word_corresponds_to_semnet_relation_GEN(implicate,verb,in,associated_with_r).
% FML 11/09/2006 Thursday @ 15:53:13 per CA e-mail
word_corresponds_to_semnet_relation_GEN(implicate,verb,in,interacts_with).
% CAUSES
%word_corresponds_to_semnet_relation_GEN(cause,noun,of,causes). % NOM remove
word_corresponds_to_semnet_relation_GEN(cause,noun,_,causes). % NOM	add      	   
word_corresponds_to_semnet_relation_GEN(evoked,pastpart,_,causes).
%word_corresponds_to_semnet_relation_GEN(lead,verb,to,causes).
word_corresponds_to_semnet_relation_GEN(leading,pastpart,to,causes). % GR	   
word_corresponds_to_semnet_relation_GEN(precipitate,verb,_,causes).
word_corresponds_to_semnet_relation_GEN(responsible,adj,for,causes).
word_corresponds_to_semnet_relation_GEN(result,verb,from,caused_by).
word_corresponds_to_semnet_relation_GEN(resulting,adj,from,caused_by).
word_corresponds_to_semnet_relation_GEN(resulting,noun,from,caused_by). % HK, 04-11-14 (to complement the previous relation)
word_corresponds_to_semnet_relation_GEN(resulting,adj,in,causes). %HK, 04-11-14 ditto
word_corresponds_to_semnet_relation_GEN(resulting,noun,in,causes). %HK, 04-11-14 ditto
%word_corresponds_to_semnet_relation_GEN('side effect',noun,of,caused_by). % CA prov
% COEXISTS_WITH
% word_corresponds_to_semnet_relation_GEN(in,prep,_,coexists_with).
% word_corresponds_to_semnet_relation_GEN(exhibit,verb,_,coexists_with).
% word_corresponds_to_semnet_relation_GEN(role,noun,for-in,coexists_with).
% word_corresponds_to_semnet_relation_GEN(role,noun,of-in,coexists_with).
% word_corresponds_to_semnet_relation_GEN(leading,pastpart,to,coexists_with).
% word_corresponds_to_semnet_relation_GEN(precipitated,adj,_,coexists_with).
% word_corresponds_to_semnet_relation_GEN(precipitate,verb,_,coexists_with).
% INHIBITS
word_corresponds_to_semnet_relation_GEN(abolish,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(abrogate,verb,_,inhibits). % GR 06/01/2017 PMID 17352756  Keratinocyte conditioned medium abrogates the modulatory effects of IGF-1 and TGF-beta1 on collagenase expression in dermal fibroblasts.
word_corresponds_to_semnet_relation_GEN(abrogation,noun,_,inhibits). % GR 06/01/2017 GENIA 8898960_S3 Class II molecules are not expressed in plasma cells because of an active suppression resulting in the abrogation of class II gene transcription.
word_corresponds_to_semnet_relation_GEN(antagonist,noun,of,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN(antagonist,noun,_,inhibited_by).
%word_corresponds_to_semnet_relation_GEN(antagonism,noun,of-by,inhibited_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(antagonism,noun,of-with,inhibited_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(antagonism,noun,by,inhibited_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(antagonism,noun,_,inhibited_by). % NOM add? % inhibits?
word_corresponds_to_semnet_relation_GEN(antagonize,verb,_,inhibits). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(arrest,verb,_,inhibits). % GR 05/23/2017 --from Negation project
% Commented out per GR 12/17/2015 e-mail
% word_corresponds_to_semnet_relation_GEN(antagonizing,adj,_,inhibits). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(attenuate,verb,_,inhibits).
%word_corresponds_to_semnet_relation_GEN(attenuation,noun,of-by,inhibited_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(attenuation,noun,_,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN(attenuator,noun,of,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN(block,verb,_,inhibits).
%word_corresponds_to_semnet_relation_GEN(block,noun,of-by,inhibits). % NOM remove
word_corresponds_to_semnet_relation_GEN(block,noun,_,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN(block,verb,of-with,inhibits).
%word_corresponds_to_semnet_relation_GEN(blockade,noun,with,inhibited_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(blockade,noun,of,inhibits). % NOM remove
word_corresponds_to_semnet_relation_GEN(blockade,noun,_,inhibits). % NOM add
%word_corresponds_to_semnet_relation_GEN(blocker,noun,of,inhibits). % NOM remove
%word_corresponds_to_semnet_relation_GEN(blocker,noun,_,inhibited_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(blocker,noun,_,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN(counteract,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(counteracted,adj,_,inhibits).
word_corresponds_to_semnet_relation_GEN(decrease,verb,in,inhibited_by).
%word_corresponds_to_semnet_relation_GEN(decrease,verb,by,inhibited_by).
%word_corresponds_to_semnet_relation_GEN(decrease,verb,with,inhibited_by).
word_corresponds_to_semnet_relation_GEN(decrease,noun,in,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN('decreased activity',noun,_,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN(decreased,adj,_,inhibits). % CA - adj
word_corresponds_to_semnet_relation_GEN(decrease,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(degradation,noun,_,inhibited_by). % 05/31/2017 source: GENIA -  IkappaB-alpha degradation by IL-2 receptor.
word_corresponds_to_semnet_relation_GEN(degrade,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(diminish,verb,_,inhibits). % GR 06/21/2017 
word_corresponds_to_semnet_relation_GEN(disrupt,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(downregulate,verb,_,inhibits).
%word_corresponds_to_semnet_relation_GEN(downregulate,noun,_,inhibits). % NOM untouched % Probably remove
word_corresponds_to_semnet_relation_GEN(decline,noun,in,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN(disruption,noun,_,inhibits). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(downmodulate,verb,_,inhibits). % GR 06/02/2017 GENIA 8298127_S1 Retinoic acid downregulates erythroid differentiation and GATA1 expression.. 
word_corresponds_to_semnet_relation_GEN(downmodulation,noun,_,inhibits). % GR 06/02/2017 GENIA 10233888_S9 Prolonged treatment of the untransformed T cell clone Ar-5 with phorbol esters results in downmodulation of the alpha and beta isozymes of PKC. 
%word_corresponds_to_semnet_relation_GEN(down-regulate,verb,_,inhibits). % Did not work, Halil
word_corresponds_to_semnet_relation_GEN(downregulate,verb,_,inhibits).
%word_corresponds_to_semnet_relation_GEN(downregulation,noun,of,inhibits). % NOM remove
word_corresponds_to_semnet_relation_GEN(downregulation,noun,_,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN([down,-,regulation],noun,_,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN(downregulation,noun,in,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN([down,-,regulation],noun,in,inhibits). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(inactivation,noun,of-by,inhibited_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(inactivation,noun,of-with,inhibited_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(inactivation,noun,by,inhibited_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(inactivation,noun,_,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN(inactivate,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(inactivator,noun,of,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN(inhibit,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(inhibited,pastpart,_,inhibits).
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,of-by,inhibited_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,of-against,inhibited_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(inhibition,noun,against,inhibits). % NOM add
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,of,inhibits). % NOM remove
word_corresponds_to_semnet_relation_GEN(inhibition,noun,_,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN(inhibition,noun,on,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN(inhibition,noun,during,inhibited_by). % NOM untouched %inhibits?
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,with,inhibited_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,by-of,inhibited_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,_,inhibited_by). % NOM remove
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,of,inhibits). % too many nominalization errors
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,against,inhibits). % too many nominalization errors
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,as,inhibited_by). % too many nominalization errors
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,for,inhibits). % too many nominalization errors
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,to,inhibits).% too many nominalization errors
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,_,inhibited_by).% too many nominalization errors
word_corresponds_to_semnet_relation_GEN(inhibitory,adj,on,inhibits).
word_corresponds_to_semnet_relation_GEN(inhibitory,adj,to,inhibits).
word_corresponds_to_semnet_relation_GEN(interfere,verb,with,inhibits). % GR 01/19/2018 from Negation corpus
word_corresponds_to_semnet_relation_GEN(lower,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(lowering,adj,_,inhibits).
word_corresponds_to_semnet_relation_GEN(negate,verb,_,inhibits). % GR 05/23/2017 --from Negation project
word_corresponds_to_semnet_relation_GEN(neutralization,noun,_,inhibits). % GR 05/31/2017
word_corresponds_to_semnet_relation_GEN(neutralize,verb,_,inhibits). % GR 05/31/2017 adiY mutants failed to neutralize acid in the presence of exogenous lysine or arginine. 
word_corresponds_to_semnet_relation_GEN(oppose,verb,_,inhibits).
%word_corresponds_to_semnet_relation_GEN(opposition,noun,of-to,inhibits). % NOM remove
word_corresponds_to_semnet_relation_GEN(opposition,noun,_,inhibits). % NOM add
%word_corresponds_to_semnet_relation_GEN(prevent,verb,_,inhibits). %MF prevent means prevent and not inhibits as of 09/07/07
word_corresponds_to_semnet_relation_GEN(reduction,noun,in,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN(reduce,verb,in,inhibited_by).
word_corresponds_to_semnet_relation_GEN(reduce,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(reduced,adj,in,inhibited_by).
word_corresponds_to_semnet_relation_GEN(reduced,adj,_,inhibits).
word_corresponds_to_semnet_relation_GEN(reduced,noun,_,inhibits). % NOM untouched % probably wrong
word_corresponds_to_semnet_relation_GEN(repress,verb,_,inhibits).
%word_corresponds_to_semnet_relation_GEN(repression,noun,of,inhibits). % NOM remove
%word_corresponds_to_semnet_relation_GEN(repression,noun,of-by,inhibits). % NOM remove
%word_corresponds_to_semnet_relation_GEN(repression,noun,of-with,inhibits). % NOM remove
word_corresponds_to_semnet_relation_GEN(repression,noun,_,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN(repressor,noun,of,inhibits). % NOM untouched
word_corresponds_to_semnet_relation_GEN(scavenge,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(scavenging,adj,_,inhibits).
word_corresponds_to_semnet_relation_GEN(silencing,adj,_,inhibits). % GR 05/23/2017 --from Negation project
word_corresponds_to_semnet_relation_GEN(silencing,pastpart,_,inhibits). % GR 05/23/2017 --from Negation project
word_corresponds_to_semnet_relation_GEN(slow,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(slowed,pastpart,_,inhibits).
word_corresponds_to_semnet_relation_GEN(suppress,verb,_,inhibits).
%word_corresponds_to_semnet_relation_GEN(suppression,noun,of,inhibits). % NOM remove
word_corresponds_to_semnet_relation_GEN(suppression,noun,_,inhibits). % NOM add
word_corresponds_to_semnet_relation_GEN(uncoupling,noun,_,inhibits). % GR 05/23/2017 --from Negation project
%word_corresponds_to_semnet_relation_GEN(uncoupling,pastpart,_,inhibits). % GR 05/23/2017 --from Negation project
word_corresponds_to_semnet_relation_GEN(uncouple,verb,_,inhibits). % GR 04/12/2018 --from Negation project
%  INTERACTS_WITH
word_corresponds_to_semnet_relation_GEN(acetylate,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(acetylating,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(act,verb,on,interacts_with).
word_corresponds_to_semnet_relation_GEN(action,noun,of-on,interacts_with).
word_corresponds_to_semnet_relation_GEN(action,noun,_,interacts_with). % GR 08/28/2018 to complete this paradigm
word_corresponds_to_semnet_relation_GEN(affect,verb,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(affinity,noun,of-for,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(affinity,noun,for,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(affinity,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(affinity,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(affinity,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(alter,verb,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(alteration,noun,of,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(alteration,noun,_,interacts_with). % NOM add
%word_corresponds_to_semnet_relation_GEN(analysis,noun,with,interacts_with). % GR commented it out
word_corresponds_to_semnet_relation_GEN(attack,verb,_,interacts_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(bind,verb,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(binding,noun,of-to,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(binding,noun,to,interacts_with). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(binding,noun,of,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(binding,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(binding,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(binding,noun,_,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(biotransform,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(biotransformed,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(bridging,adj,_,interacts_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(catabolize,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(catalyse,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(catalyze,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(catalysed,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(catalyzed,pastpart,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(catalyzation,noun,_,interacts_with). % GR 04/12/2018 to complete the paradigm
word_corresponds_to_semnet_relation_GEN(cofactor,noun,for,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(coincubate,verb,with,interacts_with). % GR 06/05/2017 Source: GENIA  9974401_S3 We incubated sodium oleate with human umbilical vein endothelial cells for 0 to 72 hours, followed by coincubation of oleate with human recombinant tumor necrosis factor, interleukin (IL)-1alpha, IL-1beta, IL-4.
word_corresponds_to_semnet_relation_GEN(coincubation,noun,_,interacts_with). % GR 06/05/2017 Source: GENIA 9285527_S6  This enhanced DNA binding activity was inhibited by coincubation of CD36 transfected cells with the human CD36-specific antibody OKM5.
word_corresponds_to_semnet_relation_GEN(coligation,noun,_,interacts_with). % GR 06/02/2017 Source: GENIA 10352279_S9 Co-ligation of surface immunoglobulin results in tyrosine phosphorylation. Coligation of CD28 leads to synergistic enhancement of IL-2 secretion.
%word_corresponds_to_semnet_relation_GEN(competition,noun,of-with,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(competition,noun,of-at,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(competition,noun,at,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(competition,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(conjugate,verb,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(conjugation,noun,of-with,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(conjugation,noun,with,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(conjugation,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(control,verb,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(control,noun,of-by,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(control,noun,of,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(control,noun,_,interacts_with). % NOM add
%word_corresponds_to_semnet_relation_GEN(conversion,noun,of-by,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(conversion,noun,to-by,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(conversion,noun,of,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(conversion,noun,_,interacts_with). % NOM add
%word_corresponds_to_semnet_relation_GEN(convert,verb,_,interacts_with). % GR commented it out
word_corresponds_to_semnet_relation_GEN(cross-coupling,noun,of-to,interacts_with). % GR 06/01/2017  A nickel-catalyzed cross-coupling between (hetero)arylborons and unactivated 1-bromo-1,1-difluoroalkanes has been developed.
word_corresponds_to_semnet_relation_GEN(cross-coupling,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(cross-coupling,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(cross-coupling,noun,_,interacts_with). % 06/01/2017  A nickel-catalyzed cross-coupling between (hetero)arylborons and unactivated 1-bromo-1,1-difluoroalkanes has been developed.
word_corresponds_to_semnet_relation_GEN(crosslinkage,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(crosslinkage,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(crosslinkage,noun,_,interacts_with). % GR 06/01/2017 Source: GENIA 8098881_S3 We have demonstrated earlier that the crosslinkage of the CD3/TCR complex with the CD2 antigen results in the proliferation of normal human T cells.
word_corresponds_to_semnet_relation_GEN(demethylate,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(demethylating,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(demethylation,noun,_,interacts_with). % GR 04/12/2018 to complete the paradigm
word_corresponds_to_semnet_relation_GEN(denature,verb,_,interacts_with). % GR 05/23/2017 --from Negation project
%word_corresponds_to_semnet_relation_GEN(dependence,noun,of-on,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(dependence,noun,of-by,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(dependence,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(desensitize,verb,with,interacts_with). % GR 04/29/09
word_corresponds_to_semnet_relation_GEN(desensitize,verb,_,interacts_with). % GR 04/29/09
%word_corresponds_to_semnet_relation_GEN(desensitization,noun,of-by,interacts_with). % GR 04/29/09 % NOM remove
%word_corresponds_to_semnet_relation_GEN(desensitization,noun,of-with,interacts_with). % GR 04/29/09 % NOM remove
word_corresponds_to_semnet_relation_GEN(desensitization,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(determined,adj,by,interacts_with).
word_corresponds_to_semnet_relation_GEN(differentiate,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(dimerize,verb,with,interacts_with).
word_corresponds_to_semnet_relation_GEN(dimerization,noun,_,interacts_with). % GR 06/14/2017
%word_corresponds_to_semnet_relation_GEN(effect,noun,of-on,interacts_with). % this one & next 3 provisionally blocked feb 09 % NOM remove
word_corresponds_to_semnet_relation_GEN(effect,noun,in,interacts_with). % NOM add % Halil on 4/3/10 o nTom's request
word_corresponds_to_semnet_relation_GEN(effect,noun,_,interacts_with). % NOM add
%%word_corresponds_to_semnet_relation_GEN(effect,noun,of-to,interacts_with).
%%word_corresponds_to_semnet_relation_GEN(effect,noun,on,interacts_with).
%%word_corresponds_to_semnet_relation_GEN(effect,noun,of,interacts_with).
word_corresponds_to_semnet_relation_GEN(engagement,noun,_,interacts_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(express,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(expressing,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(expressed,adj,_,interacts_with).
% word_corresponds_to_semnet_relation_GEN(form,verb,with,interacts_with). 
word_corresponds_to_semnet_relation_GEN(heterodimerize,verb,with,interacts_with). % GR 06/02/2017 Source: GENIA 10446999_S7 VDR is functionally active in ATRA-treated Kasumi-1 cells because it efficiently heterodimerizes with retinoid X receptor…
word_corresponds_to_semnet_relation_GEN(heterodimerization,noun,_,interacts_with). % GR 04/12/2018 to complete the paradigm
word_corresponds_to_semnet_relation_GEN(homodimerize,verb,with,interacts_with). % GR 06/22/2017 Source: GENIA 
word_corresponds_to_semnet_relation_GEN(homodimerization,noun,_,interacts_with). % GR 04/12/2018 to complete the paradigm
word_corresponds_to_semnet_relation_GEN(hydrolyze,verb,_,interacts_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(hydrolyzation,noun,_,interacts_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(hydroxylate,verb,_,interacts_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(hydroxylating,adj,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(hydroxylation,noun,by,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(hydroxylation,noun,of,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(hydroxylation,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(immobilization,noun,_,interacts_with). % GR 05/23/2017 --from Negation project
word_corresponds_to_semnet_relation_GEN(impact,verb,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(impact,noun,of-on,interacts_with). %NOM remove
word_corresponds_to_semnet_relation_GEN(impact,noun,on,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(impact,noun,_,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(important,adj,for,interacts_with).
% FML 11/09/2006 Thursday @ 15:52:49 per CA e-mail
% word_corresponds_to_semnet_relation_GEN(in,prep,_,interacts_with).
word_corresponds_to_semnet_relation_GEN('in relation to',prep,_,interacts_with). % GR
word_corresponds_to_semnet_relation_GEN(incubate,verb,with,interacts_with). % GR 05/25/2017 source: GS & GENIA - PMID 15561572; 8605587_S6; 9974401_S3 and more
word_corresponds_to_semnet_relation_GEN(incubation,noun,with,interacts_with). % GR 05/25/2017 source: GS & GENIA - PMID 15561572; 8605587_S6; 9974401_S3 and more
word_corresponds_to_semnet_relation_GEN(influence,verb,_,interacts_with). 
%word_corresponds_to_semnet_relation_GEN(influence,noun,of-on,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(influence,noun,_,interacts_with). % NOM add
% word_corresponds_to_semnet_relation_GEN(insensitive,adj,to,neg_interacts_with).
% word_corresponds_to_semnet_relation_GEN(insensitive,adj,_,neg_interacts_with).
word_corresponds_to_semnet_relation_GEN(interaction,noun,at,interacts_with). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(interaction,noun,with,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(interaction,noun,of-with,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(interaction,noun,at,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(interaction,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(interaction,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(interaction,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(interacting,adj,_,interacts_with). % GR 06/14/2017 
word_corresponds_to_semnet_relation_GEN(interplay,noun,between-and,interacts_with). % GR 10/27/2017 GENIA 10329626_S11 The potential for an interplay between the Notch and NF-kappaB signaling pathways in the immune system.
word_corresponds_to_semnet_relation_GEN(interplay,noun,between-and,interacts_with_r). % GR 10/27/2017 GENIA 10329626_S11 The potential for an interplay between the Notch and NF-kappaB signaling pathways in the immune system.
word_corresponds_to_semnet_relation_GEN(interplay,noun,_,interacts_with). % GR 06/14/2017 Source: GENIA 10329626_S11 The potential for an interplay between the Notch and NF-kappaB signaling pathways in the immune system.               
word_corresponds_to_semnet_relation_GEN(involve,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(involved,pastpart,in,interacts_with).
%word_corresponds_to_semnet_relation_GEN(involvement,noun,of-with,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(involvement,noun,in,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(involvement,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(involving,adj,_,interacts_with). % GR 06/14/2017 
word_corresponds_to_semnet_relation_GEN(label,verb,with,interacts_with). % GR 05/25/2017 source: GS - PMID 15561572
word_corresponds_to_semnet_relation_GEN(labeled,pastpart,_,interacts_with). % GR 05/25/2017 source: GS - PMID 15561572
word_corresponds_to_semnet_relation_GEN(labelled,pastpart,_,interacts_with). % GR 05/25/2017 source: GS - PMID 15561572
word_corresponds_to_semnet_relation_GEN(label,noun,for,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(label,noun,of,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(marker,noun,for,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(marker,noun,of,interacts_with). % NOM untouched
% word_corresponds_to_semnet_relation_GEN(measure,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(measured,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(mediate,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(mediated,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(mediated,noun,_,interacts_with). % tagger % NOM untouched
word_corresponds_to_semnet_relation_GEN(mediated,pastpart,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(mediating,adj,_,interacts_with). % GR 06/14/2017 
word_corresponds_to_semnet_relation_GEN(metabolize,verb,by,interacts_with).
word_corresponds_to_semnet_relation_GEN(metabolize,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(metabolized,pastpart,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(metabolizing,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(metabolizing,noun,_,interacts_with). % tagger % NOM untouched 
word_corresponds_to_semnet_relation_GEN(methylate,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(methylating,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(modify,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(modification,noun,_,interacts_with). % GR 06/14/2017 
word_corresponds_to_semnet_relation_GEN(modifying,adj,_,interacts_with). % GR 06/14/2017 
word_corresponds_to_semnet_relation_GEN(modulate,verb,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(modulation,noun,of,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(modulation,noun,of-by,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(modulation,noun,of-with,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(modulation,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(modulation,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(modulation,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(modulator,noun,of,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(normalize,verb,_,interacts_with).
% word_corresponds_to_semnet_relation_GEN(on,prep,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(oxidize,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(oxidizing,adj,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(oxidation,noun,of-by,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(oxidation,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(probe,noun,for,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(probe,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(protect,verb,against,interacts_with).
word_corresponds_to_semnet_relation_GEN(protect,verb,from,interacts_with).
%word_corresponds_to_semnet_relation_GEN(protection,noun,of-with,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(protection,noun,_,interacts_with). % NOM add
%word_corresponds_to_semnet_relation_GEN(recognize,verb,_,interacts_with). % commented out by GR
%word_corresponds_to_semnet_relation_GEN(recognition,noun,of-by,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(recognition,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(regulate,verb,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(regulation,noun,of-by,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(regulation,noun,of-by,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(regulation,noun,of-with,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(regulation,noun,of,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(regulation,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(regulation,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(regulation,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(regulator,noun,of,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(regulator,noun,_,interacts_with). % GR 07/2015
%word_corresponds_to_semnet_relation_GEN(response,noun,of-to,interacts_with).
%word_corresponds_to_semnet_relation_GEN(response,noun,to,interacts_with).
%word_corresponds_to_semnet_relation_GEN(responsible,adj,for,interacts_with).
%word_corresponds_to_semnet_relation_GEN(responsible,noun,for,interacts_with). % tagger  ?MF took out. Response is complex viz a viz Tom's Analysis
%word_corresponds_to_semnet_relation_GEN(responsibility,noun,of-for,interacts_with). % tagger ?MF took out. Response is complex viz a viz Tom's Analysis
word_corresponds_to_semnet_relation_GEN(resynthesis,noun,_,interacts_with). % 06/01/2017 source: GENIA -27658346_S2 Monoacylglycerol acyltransferase (MGAT) 2 is important for the resynthesis of triacylglycerol in the intestine.
word_corresponds_to_semnet_relation_GEN(resynthesize,verb,_,interacts_with). % GR 06/14/2017 
%word_corresponds_to_semnet_relation_GEN(role,noun,for-in,interacts_with). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(role,noun,for,interacts_with). % Commented out by GR 06_2008
% FML 11/09/2006 Thursday @ 15:53:40 per CA e-mail
word_corresponds_to_semnet_relation_GEN(role,noun,in,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(sensitive,adj,to,interacts_with).
word_corresponds_to_semnet_relation_GEN(sensitive,adj,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(sensitive,adv,to,interacts_with).
%word_corresponds_to_semnet_relation_GEN(sensitivity,noun,of-to,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(sensitivity,noun,of-for,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(sensitivity,noun,to,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(sensitivity,noun,for,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(sensitivity,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(sensitivity,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(sensitivity,noun,_,interacts_with). % NOM add
%word_corresponds_to_semnet_relation_GEN(specificity,noun,of-for,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(specificity,noun,for,interacts_with). % NOM add
%word_corresponds_to_semnet_relation_GEN(substrate,noun,of,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(substrate,noun,for,interacts_with). % NOM untouched
% word_corresponds_to_semnet_relation_GEN(substrate,noun,include,interacts_with).
word_corresponds_to_semnet_relation_GEN(substrate,noun,_,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(stabilization,noun,_,interacts_with). % GR 06/01/2017 source: GENIA 9872676_S6  We hypothesized that IL-10 prevents human monocyte NF-kappaB activation and resultant TNF-alpha production by stabilization of IkappaB-alpha.
word_corresponds_to_semnet_relation_GEN(stabilize,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(synergize,verb,with,interacts_with). % GR 06/05/2017 source: GENIA 7594468_S2 The Ca(2+)-dependent phosphatase calcineurin, a target of FK506 and CsA, synergizes with PKC-induced activation of nuclear factor (NF)-kappa B in T cell lines.
word_corresponds_to_semnet_relation_GEN(synergy,noun,with,interacts_with). % GR 06/14/2017 
word_corresponds_to_semnet_relation_GEN(synergy,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(synergy,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(synthesis,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(synthesis,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(synthesis,noun,_,interacts_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(synthesize,verb,_,interacts_with). % GR 05/24/2017 motivated by noun synthesis PMID 28469708 and 20469705
word_corresponds_to_semnet_relation_GEN(target,noun,of,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(target,noun,for,interacts_with). % NOM untouched
word_corresponds_to_semnet_relation_GEN(target,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(tether,verb,_,interacts_with). % GR 06/05/2017 source: GENIA 7537762_S3 & 9170401_S2 Adhesion molecules that tether circulating leukocytes to endothelial cells may also transduce or modulate outside-in signals for cellular activation, providing an initial regulatory point in the inflammatory response.  Activated platelets tether and activate myeloid leukocytes.
word_corresponds_to_semnet_relation_GEN(transduce,verb,_,interacts_with). % GR 06/05/2017 source: GENIA 7537762_S3 & 9159166_S6 Adhesion molecules that tether circulating leukocytes to endothelial cells may also transduce or modulate outside-in signals for cellular activation.  Alternatively, the IL-4Ralpha chain may transduce intracellular signals that lead to Cepsilon gene transcription.
word_corresponds_to_semnet_relation_GEN(transduction,noun,between-and,interacts_with). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(transduction,noun,between-and,interacts_with_r). % GR 10/27/2017 
word_corresponds_to_semnet_relation_GEN(transduction,noun,_,interacts_with). % GR 06/05/2017 source: GENIA 7506531_S2 & 8158122_S3 Rapid tyrosine phosphorylation of key cellular proteins is a crucial event in the transduction of activation signals to T-lymphocytes. NF-kappa B translocates from cytosol to nucleus as a result of transduction by tumor necrosis factor alpha (TNF alpha), phorbol ester, and other polyclonal signals.
word_corresponds_to_semnet_relation_GEN(transform,verb,by,interacts_with).
word_corresponds_to_semnet_relation_GEN(transform,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(transformed,pastpart,_,interacts_with).
%word_corresponds_to_semnet_relation_GEN(transformation,noun,of-by,interacts_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(transformation,noun,of-with,interacts_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(transformation,noun,_,interacts_with). % NOM add
word_corresponds_to_semnet_relation_GEN(transport,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(trigger,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(useful,adj,for,interacts_with).
%word_corresponds_to_semnet_relation_GEN(phenotype,verb,for-with,interacts_with). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(genotype,verb,for-with,interacts_with). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(phenotype,verb,for-by,interacts_with). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(genotype,verb,for-by,interacts_with). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(desensitization,noun,of-by,interacts_with). % GR 06/04/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(desensitize,verb,_,interacts_with). % GR
word_corresponds_to_semnet_relation_GEN(ubiquinate,verb,_,interacts_with). % GR 06/02/2017  Cullin proteins function to ubiquinate many phosphorylated  substrate proteins, such as cyclin G.
word_corresponds_to_semnet_relation_GEN(ubiquination,noun,_,interacts_with). % GR 06/02/2017 source: GENIA 9095577_S6 This transcription factor is activated via the selective phosphorylation, ubiquination and degradation of its inhibitor protein I-kB.
% ----- COEXISTS WITH
word_corresponds_to_semnet_relation_GEN(accompany,verb,_,coexists_with). % tcr
word_corresponds_to_semnet_relation_GEN(accompanied,adj,by,coexists_with). % GR 01/22/2018 to complete the paradigm
word_corresponds_to_semnet_relation_GEN(associate,verb,with,coexists_with).
%word_corresponds_to_semnet_relation_GEN(association,noun,of-with,coexists_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(association,noun,with,coexists_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(associations,noun,with,coexists_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(association,noun,_,coexists_with). % NOM add
word_corresponds_to_semnet_relation_GEN(characterise,verb,_,coexists_with). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(characterize,verb,_,coexists_with). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(coexist,verb,with,coexists_with).
word_corresponds_to_semnet_relation_GEN(coincide,verb,with,coexists_with). % GR 06/01/2017 GENIA 7516328_S8 This increase in p50 homodimers coincides with an increase in p105 mRNA, suggestive of a transcriptional up-regulation of p50.
word_corresponds_to_semnet_relation_GEN(coincident,adj,with,coexists_with). % GR 06/05/2017 source: GENIA 9374642_S3 Tissue hypoxia is coincident with many inflammatory diseases.
word_corresponds_to_semnet_relation_GEN(combine,verb,with,coexists_with).
%word_corresponds_to_semnet_relation_GEN(combination,noun,with,coexists_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(combination,noun,_,coexists_with). % NOM add
word_corresponds_to_semnet_relation_GEN(correlate,verb,with,coexists_with).
word_corresponds_to_semnet_relation_GEN(correlate,verb,to,coexists_with).
%word_corresponds_to_semnet_relation_GEN(correlation,noun,of-with,coexists_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(correlation,noun,_,coexists_with). % NOM add
%word_corresponds_to_semnet_relation_GEN(couple,verb,_,coexists_with). % GR commented it out 04/29/09
%word_corresponds_to_semnet_relation_GEN(coupled,adj,_,coexists_with). % GR commented it out 04/29/09
word_corresponds_to_semnet_relation_GEN(couple,verb,with,coexists_with). % GR  04/29/09
word_corresponds_to_semnet_relation_GEN(coupled,adj,with,coexists_with). % GR   04/29/09
word_corresponds_to_semnet_relation_GEN(during,prep,_,coexists_with). % GR 07/2015
%word_corresponds_to_semnet_relation_GEN(evaluate,verb,with,coexists_with). % GR  commented it out 04/29/09
word_corresponds_to_semnet_relation_GEN(give,verb,with,coexists_with).
word_corresponds_to_semnet_relation_GEN(implicate,verb,as,coexists_with). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(implicate,verb,in,coexists_with). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(in,prep,_,coexists_with). % %GR?  %MF and TCR decided to keep in 09/04/07
word_corresponds_to_semnet_relation_GEN(interplay,noun,_,coexists_with). % GR 06/01/2017 Source: GENIA 10329626_S11 The potential for an interplay between the Notch and NF-kappaB signaling pathways in the immune system.               
word_corresponds_to_semnet_relation_GEN(occur,verb,with,coexists_with).
word_corresponds_to_semnet_relation_GEN(occurring,verb,with,coexists_with).
word_corresponds_to_semnet_relation_GEN(parallel,verb,_,coexists_with). % GR 06/02/2017 Source: GENIA 7542286_S5 This inhibition was paralleled by reduced monocyte adhesion to endothelial monolayers in nonstatic assays               
%word_corresponds_to_semnet_relation_GEN(presence,noun,of-with,coexists_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(presence,noun,of,coexists_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(presence,noun,_,coexists_with). % NOM add
word_corresponds_to_semnet_relation_GEN(present,verb,with,coexists_with). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(reflect,verb,_,coexists_with). % ntcr-spat
word_corresponds_to_semnet_relation_GEN(reflected,adj,by,coexists_with). % ntcr-spat
word_corresponds_to_semnet_relation_GEN(relate,verb,with,coexists_with).
word_corresponds_to_semnet_relation_GEN(relate,verb,to,coexists_with).
word_corresponds_to_semnet_relation_GEN(related,pastpart,with,coexists_with).
word_corresponds_to_semnet_relation_GEN(related,pastpart,to,coexists_with).
%word_corresponds_to_semnet_relation_GEN(relation,noun,to,coexists_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(relation,noun,with,coexists_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(relation,noun,of-with,coexists_with). % NOM remove
%word_corresponds_to_semnet_relation_GEN(relation,noun,of-to,coexists_with). % NOM remove
word_corresponds_to_semnet_relation_GEN(relation,noun,_,coexists_with). % NOM add
word_corresponds_to_semnet_relation_GEN(useful,adj,for,coexists_with). %MF?
% word_corresponds_to_semnet_relation_GEN(with,      prep, _, coexists_with).  %MF? should it be back?
% PART_OF
word_corresponds_to_semnet_relation_GEN(mod_head,_,_,has_part).
word_corresponds_to_semnet_relation_GEN(carrier,noun,of,has_part). % NOM untouched
word_corresponds_to_semnet_relation_GEN(carry,verb,_,has_part).
%word_corresponds_to_semnet_relation_GEN(express,verb,in,part_of). % commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(express,verb,_,has_part). % commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(expressing,adj,_,has_part). % commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(expressed,adj,_,part_of). % commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(expression,noun,of-by,has_part). % NOM remove
%word_corresponds_to_semnet_relation_GEN(expression,noun,of-in,has_part). % NOM remove
%word_corresponds_to_semnet_relation_GEN(expression,noun,in,part_of). % NOM add? part_of? commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(expression,noun,_,part_of). % NOM add? part_of? commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(overexpress,verb,in,part_of). % commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(overexpression,noun,of-by,part_of). % NOM remove
%word_corresponds_to_semnet_relation_GEN(overexpression,noun,of-in,part_of). % NOM remove
%word_corresponds_to_semnet_relation_GEN(overexpression,noun,in,part_of). % NOM add commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN(overexpression,noun,_,part_of). % NOM add  commented out by GR on 06/23/2017 as per conversation with Halil
word_corresponds_to_semnet_relation_GEN(have,verb,_,has_part).
word_corresponds_to_semnet_relation_GEN(infect,verb,with,has_part).
word_corresponds_to_semnet_relation_GEN(infected,adj,with,has_part).
word_corresponds_to_semnet_relation_GEN(infected,adj,_,part_of).
%word_corresponds_to_semnet_relation_GEN(phenotype,verb,with,has_part).
%word_corresponds_to_semnet_relation_GEN(phenotype,verb,for,has_part).
%word_corresponds_to_semnet_relation_GEN(genotype,verb,with,has_part).
%word_corresponds_to_semnet_relation_GEN(genotype,verb,for,has_part).
% PART_OF/GENOME
%word_corresponds_to_semnet_relation_GEN(linkage,noun,with,part_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(linkage,noun,_,part_of). % NOM add
word_corresponds_to_semnet_relation_GEN(linkage,noun,to,part_of). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(localization,noun,of-to,part_of). % NOM remove
%word_corresponds_to_semnet_relation_GEN(localization,noun,of-on,part_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(localization,noun,to,part_of). % NOM add
word_corresponds_to_semnet_relation_GEN(localization,noun,on,part_of). % NOM add
word_corresponds_to_semnet_relation_GEN(localize,verb,on,part_of).
word_corresponds_to_semnet_relation_GEN(localize,verb,to,part_of).
word_corresponds_to_semnet_relation_GEN(localizes,noun,on,part_of). % don't ask, c'est vrai % NOM untouched
word_corresponds_to_semnet_relation_GEN(localizes,noun,to,part_of). % don't ask, c'est vrai % NOM untouched
word_corresponds_to_semnet_relation_GEN(locate,verb,on,part_of).
word_corresponds_to_semnet_relation_GEN(locate,verb,at,part_of).
word_corresponds_to_semnet_relation_GEN(locate,adj,at,part_of).
word_corresponds_to_semnet_relation_GEN(located,adj,on,part_of).
word_corresponds_to_semnet_relation_GEN(map,verb,_,part_of).
word_corresponds_to_semnet_relation_GEN(mapped,adj,_,part_of).
word_corresponds_to_semnet_relation_GEN(mapped,pastpart,_,part_of).
word_corresponds_to_semnet_relation_GEN(map,verb,of-to,part_of).
%word_corresponds_to_semnet_relation_GEN(map,verb,to,part_of).
word_corresponds_to_semnet_relation_GEN(mapping,noun,of-to,part_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(mapping,noun,to,part_of). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(at,prep,_,part_of). %GR? %MF and TCR decided to exclude in 09/04/07
%word_corresponds_to_semnet_relation_GEN(on,prep,_,part_of). %GR? %MF and TCR decided to exclude in 09/04/07
% PROCESS_OF
%word_corresponds_to_semnet_relation_GEN(of,prep,_,has_process). %MF and TCR excluded in 09/04/07
word_corresponds_to_semnet_relation_GEN(among,prep,_,process_of). %MF and This one will stay
word_corresponds_to_semnet_relation_GEN(appear,verb,in,process_of).
word_corresponds_to_semnet_relation_GEN(exist,verb,in,process_of).
%word_corresponds_to_semnet_relation_GEN(have,aux,_,process_of).
word_corresponds_to_semnet_relation_GEN(have,verb,_,process_of).
word_corresponds_to_semnet_relation_GEN(mod_head,_,_,process_of).
%word_corresponds_to_semnet_relation_GEN(occur,verb,in,process_of). %Removed per GR request 10/01/09
%word_corresponds_to_semnet_relation_GEN(transpire,verb,_,occurs_in).
%word_corresponds_to_semnet_relation_GEN(with,prep,_,has_process).
%word_corresponds_to_semnet_relation_GEN(get,verb,_,has_process). %MF and TCR excluded in 09/04/07
%word_corresponds_to_semnet_relation_GEN(have,aux,_,has_process). % GR Already excluded above, why does it appear again?
word_corresponds_to_semnet_relation_GEN(have,verb,_,has_process).
word_corresponds_to_semnet_relation_GEN(exhibit,verb,_,has_process). % GR 05/25/2017 as per GS - PMID 15592935 Vascular endothelium, erythrocytes, smooth muscle and inflammatory cells (except macrophages) in the allergic patients exhibited stronger HO-1 immunoreaction compared to the control.
word_corresponds_to_semnet_relation_GEN(mod_head,_,_,has_process).
word_corresponds_to_semnet_relation_GEN(diagnose,verb,with,has_process). % GR 01/22/2018 to fire when one of the arguments is animate
word_corresponds_to_semnet_relation_GEN(diagnosed,adj,with,has_process). % GR 01/22/2018 to fire when one of the arguments is animate
% PREDISPOSES
word_corresponds_to_semnet_relation_GEN(contribute,verb,to,predisposes).
%word_corresponds_to_semnet_relation_GEN(contribution,noun,of-to,predisposes). %MF changed rule % NOM remove
word_corresponds_to_semnet_relation_GEN(contribution,noun,to,predisposes). %MF changed rule % NOM add (lexicon bug?)
%word_corresponds_to_semnet_relation_GEN(contribution,noun,of-by,predisposed_by). %MF changed rule % NOM remove
word_corresponds_to_semnet_relation_GEN(contribution,noun,of-by,predisposes). % NOM add % Need this due to 2006 lexicon bug -- no complementation for 'contribution'!
word_corresponds_to_semnet_relation_GEN(determinant,noun,of,predisposes). % NOM untouched
word_corresponds_to_semnet_relation_GEN(predisposition,noun,to,predisposes). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(predisposition,noun,of-to,predisposes). % NOM remove
word_corresponds_to_semnet_relation_GEN(predispose,verb,to,predisposes).
word_corresponds_to_semnet_relation_GEN(predispose,verb,_,predisposes).
word_corresponds_to_semnet_relation_GEN(predisposing,adj,_,predisposes).
word_corresponds_to_semnet_relation_GEN(predisposing,noun,_,predisposes). %HK, 04-11-14 
word_corresponds_to_semnet_relation_GEN(promote,verb,_,predisposes).
%word_corresponds_to_semnet_relation_GEN(promotion,noun,of-by,predisposed_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(promotion,noun,_,predisposed_by). % NOM add % predispose? 
%word_corresponds_to_semnet_relation_GEN(indicator,noun,for,predisposes). %MF RF project 
%word_corresponds_to_semnet_relation_GEN(component,noun,of,predisposes). %MF RF project
word_corresponds_to_semnet_relation_GEN(marker,noun,of,predisposes). %MF RF project % NOM untouched
word_corresponds_to_semnet_relation_GEN(marker,noun,for,predisposes). %MF RF project % NOM untouched
word_corresponds_to_semnet_relation_GEN(marker,noun,_,predisposes). %MF RF project % NOM untouched
word_corresponds_to_semnet_relation_GEN(biomarker,noun,of,predisposes). %MF RF project added after AMIA % NOM untouched
word_corresponds_to_semnet_relation_GEN(biomarker,noun,for,predisposes). %MF RF project added after AMIA % NOM untouched
word_corresponds_to_semnet_relation_GEN(biomarker,noun,_,predisposes). %MF RF project added after AMIA % NOM untouched
% The risk issue: risk rules functions as heads and modifiers.
% Ideally the multiword "risk factor" should work, but it is not. So what Tom did for now is to allow the modifiers of a multiword that is in the lexicon to work. So if a multi word such as "risk factor" is in the lexicon it will use risk as a modifier as an indicator rule. Factor can not be an indicator. This should work for any multi word that satisfies this pre-conditions.
word_corresponds_to_semnet_relation_GEN(risk,noun,for,predisposes). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(risk,noun,of,predisposes). %MF RF project % NOM remove
word_corresponds_to_semnet_relation_GEN(risk,noun,_,predisposes). %MF RF project % NOM untouched
%word_corresponds_to_semnet_relation_GEN('risk factor',noun,of,predisposes). %MF RF project (Multi-word issue) not being used
%word_corresponds_to_semnet_relation_GEN('risk factor',noun,for,predisposes). %MF RF project (Multi-word issue) not being used
%word_corresponds_to_semnet_relation_GEN('genetic risk factor',noun,for,predisposes). %MF RF project (Multi-word issue)
%word_corresponds_to_semnet_relation_GEN('predictive factor',noun,for,predisposes). %MF RF project (Multi-word issue)
%word_corresponds_to_semnet_relation_GEN(susceptibility,noun,to,predisposes). %MF wrong  for predisposes
word_corresponds_to_semnet_relation_GEN(predict,verb,_,predisposes). %MF RF project
word_corresponds_to_semnet_relation_GEN(predictor,noun,of,predisposes). %MF RF project % NOM untouched
word_corresponds_to_semnet_relation_GEN(predictor,noun,for,predisposes). %MF RF project % NOM untouched
word_corresponds_to_semnet_relation_GEN(predictor,noun,for,predisposes). %MF RF project % NOM remove
word_corresponds_to_semnet_relation_GEN(predictor,noun,_,predisposes). %MF RF project % NOM untouched
word_corresponds_to_semnet_relation_GEN(predictive,adj,_,predisposes). %MF RF project
word_corresponds_to_semnet_relation_GEN(predictive,adj,for,predisposes). %MF RF project
word_corresponds_to_semnet_relation_GEN(predictive,adj,of,predisposes). %MF RF project
word_corresponds_to_semnet_relation_GEN(prediction,noun,of,predisposes). %MF RF project % NOM remove
word_corresponds_to_semnet_relation_GEN(predicting,adj,_,predisposes). %MF RF project
word_corresponds_to_semnet_relation_GEN(prediction,noun,of-with,predisposed_by). %MF RF project % NOM remove
word_corresponds_to_semnet_relation_GEN(prediction,noun,of-by,predisposed_by). %MF RF project  % NOM remove
word_corresponds_to_semnet_relation_GEN(prediction,noun,_,predisposes). % NOM add
word_corresponds_to_semnet_relation_GEN(propensity,noun,to,predisposes). % Negation project
% STIMULATES
word_corresponds_to_semnet_relation_GEN(accelerate,verb,_,stimulates). % GR source: GENIA 1851861_S2 Human herpesvirus 6 (HHV-6) can activate the human immunodeficiency virus (HIV) promoter and accelerate cytopathic effects in HIV-infected human T cells.
word_corresponds_to_semnet_relation_GEN(activate,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(activator,noun,of,stimulates). % NOM untouched
word_corresponds_to_semnet_relation_GEN(activated,adj,_,stimulates). %GR 03/21/08
%word_corresponds_to_semnet_relation_GEN(activation,noun,of-by,stimulated_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(activation,noun,of-with,stimulated_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(activation,noun,by,stimulated_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(activation,noun,_,stimulates).
%word_corresponds_to_semnet_relation_GEN(agonist,noun,of,stimulates). % NOM remove
%word_corresponds_to_semnet_relation_GEN(agonist,noun,_,stimulated_by). % NOM remove?
word_corresponds_to_semnet_relation_GEN(agonist,noun,_,stimulates). % NOM add?
word_corresponds_to_semnet_relation_GEN(agonize,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(accelerate,verb,_,stimulates).
%word_corresponds_to_semnet_relation_GEN(activator,noun,of,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN(amplify,verb,_,stimulates). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(augment,verb,_,stimulates).
%word_corresponds_to_semnet_relation_GEN(augmentation,noun,of,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN(augmentation,noun,_,stimulates). % NOM add
word_corresponds_to_semnet_relation_GEN(contribute,verb,to,stimulates).
word_corresponds_to_semnet_relation_GEN(contributor,noun,to,stimulates). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(contribution,noun,of-to,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN(contribution,noun,to,stimulates). % NOM add ( lexicon bug?)
word_corresponds_to_semnet_relation_GEN(costimulate,verb,_,stimulates). % GR 06/22/2017 source: GENIA 
word_corresponds_to_semnet_relation_GEN(costimulation,noun,of-with,stimulates). % GR 06/01/2017 source: GENIA 7543515_S1 Costimulation of human CD4+ T cells with LFA-3 and B7 induce distinct effects on AP-1 and NF-kappa B transcription factors.
word_corresponds_to_semnet_relation_GEN(depend,verb,upon,stimulated_by).
word_corresponds_to_semnet_relation_GEN(depend,verb,on,stimulated_by).
word_corresponds_to_semnet_relation_GEN(dependent,adj,upon,stimulated_by).
word_corresponds_to_semnet_relation_GEN(dependent,adj,on,stimulated_by).
word_corresponds_to_semnet_relation_GEN(dependant,adj,_,stimulates). % 
word_corresponds_to_semnet_relation_GEN(dependent,adj,_,stimulates). % 
word_corresponds_to_semnet_relation_GEN(dependent,noun,upon,stimulated_by). % NOM untouched
word_corresponds_to_semnet_relation_GEN(dependent,noun,on,stimulated_by). % NOM untouched
word_corresponds_to_semnet_relation_GEN(dependent,noun,_,stimulates). %  NOM untouched
%word_corresponds_to_semnet_relation_GEN(dependence,noun,of-on,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN(dependence,noun,_,stimulates). % NOM add
word_corresponds_to_semnet_relation_GEN(drive,verb,_,stimulates). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(elevation,noun,in,stimulates). % NOM untouched
word_corresponds_to_semnet_relation_GEN(elicit,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(enhance,verb,_,stimulates).
%word_corresponds_to_semnet_relation_GEN(enhancement,noun,of,stimulates). % NOM remove
%word_corresponds_to_semnet_relation_GEN(enhancement,noun,of-by,stimulated_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(enhancement,noun,in-by,stimulates). % foible due to in-by % NOM remove
word_corresponds_to_semnet_relation_GEN(enhancement,noun,_,stimulates). % NOM add
word_corresponds_to_semnet_relation_GEN(enhancement,noun,in,stimulates). % NOM add
word_corresponds_to_semnet_relation_GEN(facilitate,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(increase,verb,with,stimulated_by).
word_corresponds_to_semnet_relation_GEN(increase,verb,during,stimulated_by).
word_corresponds_to_semnet_relation_GEN(increase,noun,after,stimulated_by). % NOM untouched
word_corresponds_to_semnet_relation_GEN(increase,verb,after,stimulated_by).
word_corresponds_to_semnet_relation_GEN(increase,verb,_,stimulates).
%word_corresponds_to_semnet_relation_GEN(increase,noun,in-by,stimulates). % foible due to in-by % NOM remove
%word_corresponds_to_semnet_relation_GEN(increase,noun,in,stimulates). % NOM remove
%word_corresponds_to_semnet_relation_GEN(increase,noun,of,stimulates). % NOM remove
%word_corresponds_to_semnet_relation_GEN(increase,noun,of-by,stimulated_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(increase,noun,_,stimulates). % NOM untouched
% word_corresponds_to_semnet_relation_GEN(increased,adj,_,stimulates).
word_corresponds_to_semnet_relation_GEN(increased,pastpart,with,stimulated_by).
word_corresponds_to_semnet_relation_GEN(increasing,adj,_,stimulates).
%word_corresponds_to_semnet_relation_GEN(induction,noun,of-by,stimulated_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(induction,noun,by,stimulated_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(induction,noun,of,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN(induction,noun,_,stimulates). % NOM add
word_corresponds_to_semnet_relation_GEN(inducer,noun,of,stimulates). % NOM untouched
word_corresponds_to_semnet_relation_GEN(induced,adj,_,stimulates).
word_corresponds_to_semnet_relation_GEN(induce,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(inducible,noun,by,stimulated_by). % called head - ergo noun % NOM untouched
word_corresponds_to_semnet_relation_GEN(potentiate,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(potentiation,verb,of-by,stimulates).
word_corresponds_to_semnet_relation_GEN(promote,verb,_,stimulates).
%word_corresponds_to_semnet_relation_GEN(promotion,noun,of-by,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN(promotion,noun,_,stimulates). % NOM add
%word_corresponds_to_semnet_relation_GEN(require,verb,_,stimulated_by). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(requirement,noun,of-for,stimulated_by). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(required,adj,_,stimulated_by). % GR commented it out
word_corresponds_to_semnet_relation_GEN(stimulate,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(stimulated,adj,_,stimulates). % GR 06/02/2017 source: GENIA 9374467_S7 JNK activation in calcineurin-stimulated cells caused nuclear exclusion of NFAT4.
word_corresponds_to_semnet_relation_GEN(stimulating,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(stimulating,noun,_,stimulates). % CA - tagger issues % NOM untouched
word_corresponds_to_semnet_relation_GEN(stimulating,adj,_,stimulates). % CA - tagger issues  % NOM untouched
%word_corresponds_to_semnet_relation_GEN(stimulation,noun,of,stimulates). % NOM remove
%word_corresponds_to_semnet_relation_GEN(stimulation,noun,of-by,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN(stimulation,noun,_,stimulates). % NOM add
word_corresponds_to_semnet_relation_GEN(transactivate,verb,_,stimulates). % GR 06/01/2017 source: GENIA A truncated Tat protein (Tat1-72), known to transactivate the HIV-1 long terminal repeat (LTR), no longer affected Mn-SOD expression, the cellular redox state or TNF-mediated cytotoxicity.
word_corresponds_to_semnet_relation_GEN(transactivation,noun,_,stimulates). % GR 06/01/2017 source: GENIA A truncated Tat protein (Tat1-72), known to transactivate the HIV-1 long terminal repeat (LTR), no longer affected Mn-SOD expression, the cellular redox state or TNF-mediated cytotoxicity.
%word_corresponds_to_semnet_relation_GEN(up-regulate,verb,_,stimulates). % Did not work, Halil
word_corresponds_to_semnet_relation_GEN(upregulate,verb,_,stimulates).
%word_corresponds_to_semnet_relation_GEN(up-regulates,verb,_,stimulates). % Did not work, Halil
%word_corresponds_to_semnet_relation_GEN(upregulation,noun,by,stimulated_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(upregulation,noun,in,stimulates). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(upregulation,noun,of-by,stimulated_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(upregulation,noun,of,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN([up,-,regulation],noun,in,stimulates). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(up-regulation,noun,of,stimulates). % NOM remove
word_corresponds_to_semnet_relation_GEN(upregulation,noun,_,stimulates).
word_corresponds_to_semnet_relation_GEN([up,-,regulation],noun,_,stimulates). % NOM add
% ----- AFFECTS / AFFECTED_BY - OLD ONES
word_corresponds_to_semnet_relation_GEN(affect,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(alter,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(attenuate,verb,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(attenuation,verb,of_by,affects). % GR 01/15/08
word_corresponds_to_semnet_relation_GEN(block,verb,_,affects). % GR
word_corresponds_to_semnet_relation_GEN(blunt,verb,_,affects). % GR 05/23/2017 --from Negation project
word_corresponds_to_semnet_relation_GEN(boost,verb,_,affects). % GR
word_corresponds_to_semnet_relation_GEN(catalyse,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(catalyze,verb,_,affects). % 06/02/2017 source: GENIA 2123553_S9 FKBP has been shown to catalyze the interconversion of the cis- and trans-rotamers of the peptidyl-prolyl amide bond of peptide substrates.
word_corresponds_to_semnet_relation_GEN(change,verb,_,affects). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(consequence,noun,for,affects). % GR 05/24/2017 --from Negation project PMID 1532362
word_corresponds_to_semnet_relation_GEN(control,verb,_,affects).
%word_corresponds_to_semnet_relation_GEN(control,noun,of-by,affected_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(control,noun,of,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(control,noun,_,affects). % NOM add
word_corresponds_to_semnet_relation_GEN(correlate,verb,with,affects).
%word_corresponds_to_semnet_relation_GEN(correlation,noun,of-with,affected_by). % NOM remove?
%word_corresponds_to_semnet_relation_GEN(correlation,noun,of-to,affected_by). % NOM remove?
word_corresponds_to_semnet_relation_GEN(correlation,noun,to,affected_by). % NOM add?
word_corresponds_to_semnet_relation_GEN(correlation,noun,_,affected_by). % NOM add?
word_corresponds_to_semnet_relation_GEN(contribute,verb,to,affects).
%word_corresponds_to_semnet_relation_GEN(contribution,noun,of-to,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(contribution,noun,to,affects). % NOM untouched
% word_corresponds_to_semnet_relation_GEN(decrease,   verb, _,     affects). % GR
word_corresponds_to_semnet_relation_GEN(delay,verb,_,affects). % GR
word_corresponds_to_semnet_relation_GEN(dependent,adj,on,affects). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(dependent,adj,upon,affects). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(dependence,noun,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(depolarization,noun,_,affects). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(depolarize,verb,_,affects). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(depress,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(deregulate,verb,_,affects). % GR 05/23/2017 --Negation project Chromosomal translocations that deregulate the expression of the BCL6 gene.
word_corresponds_to_semnet_relation_GEN(deregulation,noun,_,affects). % GR 05/23/2017 --from Negation project
%%word_corresponds_to_semnet_relation_GEN(effect,noun,of-in,affects).  % GR  01/11/08 Provisionally blocked Feb 09
%word_corresponds_to_semnet_relation_GEN(effect,noun,of-on,affects). % NOM remove
%word_corresponds_to_semnet_relation_GEN(effect,noun,on,affects). % NOM remove
%word_corresponds_to_semnet_relation_GEN(effect,noun,of,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(effect,noun,in,affects). % NOM add % Halil on 4/2/10 on Tom's request
word_corresponds_to_semnet_relation_GEN(effect,noun,_,affects). % NOM add
word_corresponds_to_semnet_relation_GEN(engagement,noun,_,affected_by). % 06/01/2017 source: GENIA -9144472_S5 TCR engagement stimulates the activation of the protein kinase Raf-1.
word_corresponds_to_semnet_relation_GEN(exacerbate,verb,_,affects). % GR
word_corresponds_to_semnet_relation_GEN(exacerbation,noun,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(experience,verb,_,affected_by). % GR  01/11/08
word_corresponds_to_semnet_relation_GEN(factor,noun,in,affects). % NOM untouched
word_corresponds_to_semnet_relation_GEN(immobilization,noun,_,affects). % GR 05/23/2017 --from Negation project
word_corresponds_to_semnet_relation_GEN(immobilize,verb,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(impact,verb,_,affects).
%word_corresponds_to_semnet_relation_GEN(impact,noun,of-on,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(impact,noun,on,affects). % NOM add
word_corresponds_to_semnet_relation_GEN(impact,noun,_,affects). % NOM untouched
word_corresponds_to_semnet_relation_GEN(implicate,verb,in,affects). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(important,adj,for,affects).
word_corresponds_to_semnet_relation_GEN(improve,verb,_,affects). % GR 01/15/08
word_corresponds_to_semnet_relation_GEN(influence,verb,_,affects).
%word_corresponds_to_semnet_relation_GEN(influence,noun,of-on,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(influence,noun,_,affects). % NOM add
word_corresponds_to_semnet_relation_GEN(influencing,adj,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(involve,verb,_,affects). % GR 01/15/08
word_corresponds_to_semnet_relation_GEN(involved,pastpart,in,affects).
%word_corresponds_to_semnet_relation_GEN(involvement,noun,of-with,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(involvement,noun,_,affects). % NOM add GR changed this to the passive on 06/28/2017 as it was giving reversed results
word_corresponds_to_semnet_relation_GEN(involve,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(involving,adj,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(lead,verb,to,affects).
word_corresponds_to_semnet_relation_GEN(lessen,verb,_,affects). %  GR
word_corresponds_to_semnet_relation_GEN(lower,verb,_,affects). %  GR
word_corresponds_to_semnet_relation_GEN(mediate,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(mediated,adj,_,affects).
word_corresponds_to_semnet_relation_GEN(mediated,pastpart,_,affects).
word_corresponds_to_semnet_relation_GEN(mediating,adj,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(modify,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(modulate,verb,_,affects).
%word_corresponds_to_semnet_relation_GEN(modulation,noun,of,affects). % NOM remove
%word_corresponds_to_semnet_relation_GEN(modulation,noun,of-by,affects_r). % NOM remove
%word_corresponds_to_semnet_relation_GEN(modulation,noun,of-with,affects_r). % NOM remove
word_corresponds_to_semnet_relation_GEN(modulation,noun,_,affects). % NOM add
word_corresponds_to_semnet_relation_GEN(modulator,noun,of,affects). % NOM untouched
word_corresponds_to_semnet_relation_GEN(normalize,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(normalization,noun,_,affects). % 06/14/2017 
%word_corresponds_to_semnet_relation_GEN(prevent,verb,_,affects). %MF took this out prevent  more than affects it means PREVENT
word_corresponds_to_semnet_relation_GEN(regulate,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(relate,verb,with,affects).
word_corresponds_to_semnet_relation_GEN(relate,verb,to,affects).
word_corresponds_to_semnet_relation_GEN(related,pastpart,with,affects).
word_corresponds_to_semnet_relation_GEN(related,pastpart,to,affects).
word_corresponds_to_semnet_relation_GEN(relation,noun,to,affects). % NOM unotuched
word_corresponds_to_semnet_relation_GEN(relation,noun,with,affects). % NOM untouched % with seems to mark object, that is why this is not simplified
%word_corresponds_to_semnet_relation_GEN(regulation,noun,of-by,affects). % NOM remove
%word_corresponds_to_semnet_relation_GEN(regulation,noun,of,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(regulation,noun,_,affects). % NOM add but GR changed it to the passive form on 5/31/2017
word_corresponds_to_semnet_relation_GEN(regulator,noun,of,affects). % NOM untouched
word_corresponds_to_semnet_relation_GEN(role,noun,in,affects). % GR 01/11/08 % NOM untouched
word_corresponds_to_semnet_relation_GEN(sensitive,adj,to,affects). % GR? MF and TCR decided to keep in 09/04/07
%word_corresponds_to_semnet_relation_GEN(sensitivity,noun,to,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(prolong,verb,_,affects). % tcr-provisional
%word_corresponds_to_semnet_relation_GEN(response,noun,to,affected_by). % GR  ?MF took out. Response is complex viz a viz Tom's Analysis
word_corresponds_to_semnet_relation_GEN(transduce,verb,_,affects). % GR 06/05/2017 Source: GENIA 7958618_S6 These redox-regulated enzymes trigger signal cascades for NF kappa B activation and transduce signals from the T cell antigen receptor, from CD4 and CD8 molecules, and from the IL-2 receptor beta-chain. 
word_corresponds_to_semnet_relation_GEN(transduction,noun,_,affects). % 06/14/2017 GR Source: GENIA: 8158122_S3 In B cells and macrophages it is constitutively present in cell nuclei, whereas in many other cell types, NF-kappa B translocates from cytosol to nucleus as a result of transduction by tumor necrosis factor alpha (TNF alpha)
word_corresponds_to_semnet_relation_GEN(suffer,verb,from,affected_by). % GR
word_corresponds_to_semnet_relation_GEN(suffering,adj,from,affected_by). % 06/14/2017 
%word_corresponds_to_semnet_relation_GEN(undergo,verb,_,affected_by). % tcr-spat  % GR Commented it out
% AFFECTS/AFFECTED_BY - NEW ONES
%word_corresponds_to_semnet_relation_GEN(alteration,noun,of,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(alteration,noun,_,affects). % NOM add
word_corresponds_to_semnet_relation_GEN(associate,verb,with,affects).
%word_corresponds_to_semnet_relation_GEN(association,noun,of-with,affects). % NOM remove
%word_corresponds_to_semnet_relation_GEN(association,noun,with,affects). % NOM remove
%word_corresponds_to_semnet_relation_GEN(associations,noun,with,affects). % NOM remove
word_corresponds_to_semnet_relation_GEN(association,noun,_,affects). % NOM add
% word_corresponds_to_semnet_relation_GEN(associate,verb,with,affects).
%%word_corresponds_to_semnet_relation_GEN(effect ,noun,of,affects). % Provisionally blocked
word_corresponds_to_semnet_relation_GEN(denature,verb,_,affects). % GR 06/2017 Negation project & GENIA corpus
word_corresponds_to_semnet_relation_GEN(factor,noun,in,affects). % NOM untouched
% word_corresponds_to_semnet_relation_GEN(induced,adj,_,affects).
%word_corresponds_to_semnet_relation_GEN(response,noun,of-to,affects).
%word_corresponds_to_semnet_relation_GEN(response,noun,to,affects).
word_corresponds_to_semnet_relation_GEN(responsible,adj,for,affects).
word_corresponds_to_semnet_relation_GEN(responsible,noun,for,affects). % wacky SemRep thing % NOM untouched
% word_corresponds_to_semnet_relation_GEN(role,noun,in,affects). % 
word_corresponds_to_semnet_relation_GEN(responsive,adj,to,affected_by). % GR 06/02/2017 PMID 28405903 The brain's immune response is highly responsive to environmental stimuli.
word_corresponds_to_semnet_relation_GEN(responsive,adj,_,affects). % GR 06/22/2017 PMID 28447195 The potential of estrogen responsive miRNAs.
word_corresponds_to_semnet_relation_GEN(sensitivity,noun,to,affected_by). % NOM untouched % Not sure whether to or _, due to "about"
word_corresponds_to_semnet_relation_GEN(sensitizer,noun,to,affected_by). % NOM untouched
word_corresponds_to_semnet_relation_GEN(stabilization,noun,_,affected_by). % GR 06/01/2017 source: GENIA 9872676_S6  We hypothesized that IL-10 prevents human monocyte NF-kappaB activation and resultant TNF-alpha production by stabilization of IkappaB-alpha.
word_corresponds_to_semnet_relation_GEN(stabilize,verb,_,affects). 
word_corresponds_to_semnet_relation_GEN(susceptibility,noun,of-to,affected_by). % 05/24/2017 source: GS - PMID 15499431
word_corresponds_to_semnet_relation_GEN(susceptible,adj,to,affected_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(sustain,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(sustaining,adj,_,affects).
word_corresponds_to_semnet_relation_GEN(synergize,verb,_,affects). % GR 06/05/2017 PMID 28077991 Therapeutically targeting ncRNAs may synergize androgen deprivation therapy (ADT) to have a better effect to fight against prostate cancer.
word_corresponds_to_semnet_relation_GEN(synergism,noun,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(synergy,noun,_,affects). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(transactivate,verb,_,affects). % GR 06/01/2017 source: GENIA A truncated Tat protein (Tat1-72), known to transactivate the HIV-1 long terminal repeat (LTR), no longer affected Mn-SOD expression, the cellular redox state or TNF-mediated cytotoxicity.
word_corresponds_to_semnet_relation_GEN(transactivation,noun,_,affects). % GR 06/01/2017 source: GENIA A truncated Tat protein (Tat1-72), known to transactivate the HIV-1 long terminal repeat (LTR), no longer affected Mn-SOD expression, the cellular redox state or TNF-mediated cytotoxicity.
word_corresponds_to_semnet_relation_GEN(weaken,verb,_,affects). % GR 06/13/2017 Negation triggers 
word_corresponds_to_semnet_relation_GEN(decrease,verb,_,affects). % 06/23/2017
word_corresponds_to_semnet_relation_GEN(encroach, verb,on,affects). % GR 04/13/2018 Negation project
word_corresponds_to_semnet_relation_GEN(encroach, verb,upon,affects). % GR 04/13/2018 Negation project
%word_corresponds_to_semnet_relation_GEN(encroaching, verb,on,affects). % GR 04/13/2018 Negation project
%word_corresponds_to_semnet_relation_GEN(encroaching,verb,upon,affects). % GR 04/13/2018 Negation project
word_corresponds_to_semnet_relation_GEN(encroaching,adj,_,affects). % GR 04/13/2018 Negation project
word_corresponds_to_semnet_relation_GEN(encroaching,noun,_,affects). % GR 04/13/2018 Negation project
word_corresponds_to_semnet_relation_GEN(abate,verb,_,affects). % Negation project
%word_corresponds_to_semnet_relation_GEN(abating,verb,_,affects). % Negation project
word_corresponds_to_semnet_relation_GEN(abating,adj,_,affects). % GR 04/13/2018 Negation project
word_corresponds_to_semnet_relation_GEN(abating,noun,_,affects). % GR 04/13/2018 Negation project
word_corresponds_to_semnet_relation_GEN(abate,verb,_,disrupts). % Negation project
%word_corresponds_to_semnet_relation_GEN(abating,verb,_,disrupts). % Negation project
word_corresponds_to_semnet_relation_GEN(abating,adj,_,disrupts). % GR 04/13/2018 Negation project
word_corresponds_to_semnet_relation_GEN(abating,noun,_,disrupts). % GR 04/13/2018 Negation project
word_corresponds_to_semnet_relation_GEN(harmful,adj,to,affects). % Negation project
word_corresponds_to_semnet_relation_GEN(action,noun,in,affects). % Negation project
word_corresponds_to_semnet_relation_GEN(concomitant,adj,_, coexists_with). % Negation project
word_corresponds_to_semnet_relation_GEN(attach,verb,to,has_location). % GR Negation project
%word_corresponds_to_semnet_relation_GEN(attached,verb,to,has_location). % GR Negation project
word_corresponds_to_semnet_relation_GEN(attached,adj,to,has_location). % GR Negation project
% ----------- TREATS
%word_corresponds_to_semnet_relation_GEN(respond,verb,to,treated_by). % Means CAUSE
% ----- DISRUPTS - OLD ONES
word_corresponds_to_semnet_relation_GEN(kill,verb,_,disrupts). % GR
word_corresponds_to_semnet_relation_GEN(inhibit,verb,_,disrupts). % GR 
% word_corresponds_to_semnet_relation_GEN(inhibition,noun,_,disrupts).
word_corresponds_to_semnet_relation_GEN(interfere,verb,with,disrupts). % GR
word_corresponds_to_semnet_relation_GEN(interference,noun,_,disrupts). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(stop,verb,_,disrupts). % GR
word_corresponds_to_semnet_relation_GEN(suppress,verb,_,disrupts). % GR 
word_corresponds_to_semnet_relation_GEN(disrupt,verb,_,disrupts). % tcr
% DISRUPTS - NEW ONES
word_corresponds_to_semnet_relation_GEN(abolish,verb,_,disrupts).
word_corresponds_to_semnet_relation_GEN(abrogate,verb,_,disrupts). % GR 06/01/2017 PMID 17352756  Keratinocyte conditioned medium abrogates the modulatory effects of IGF-1 and TGF-beta1 on collagenase expression in dermal fibroblasts.
word_corresponds_to_semnet_relation_GEN(abrogation,noun,_,disrupts). % GR 06/01/2017 GENIA 8898960_S3  Class II molecules are not expressed in plasma cells because of an active suppression resulting in the abrogation of class II gene transcription.
word_corresponds_to_semnet_relation_GEN(antagonize,verb,_,disrupts).
word_corresponds_to_semnet_relation_GEN(antagonist,noun,of,disrupts).
word_corresponds_to_semnet_relation_GEN(antagonist,noun,_,disrupted_by). % NOM untouched
word_corresponds_to_semnet_relation_GEN(arrest,verb,_,disrupts). % GR 05/23/2017 --from Negation project
%word_corresponds_to_semnet_relation_GEN(attenuator,noun,of,disrupts). % GR commented it out
word_corresponds_to_semnet_relation_GEN(attenuate,verb,_,disrupts).
word_corresponds_to_semnet_relation_GEN(attenuation,noun,_,disrupts). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(biodegradation,noun,_,disrupted_by). % 06/21/2017 
word_corresponds_to_semnet_relation_GEN(biodegrade,verb,_,disrupts). % 06/21/2017 
word_corresponds_to_semnet_relation_GEN(block,verb,_,disrupts).
%word_corresponds_to_semnet_relation_GEN(block,noun,of-by,disrupts). % NOM remove
word_corresponds_to_semnet_relation_GEN(block,noun,_,disrupts). % NOM add
word_corresponds_to_semnet_relation_GEN(blocker,noun,of,disrupts). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(blunt,verb,_,disrupts). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(convert,verb,_,interacts_with). % GR commented it out
word_corresponds_to_semnet_relation_GEN(counteract,verb,_,disrupts).
word_corresponds_to_semnet_relation_GEN(counteracted,adj,_,disrupts).
word_corresponds_to_semnet_relation_GEN(cytotoxic,adj,to,disrupts).
%word_corresponds_to_semnet_relation_GEN(decrease,noun,in,disrupts). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(decreased,adj,_,disrupts). % CA - adj  % GR commented it out
%word_corresponds_to_semnet_relation_GEN(decrease,verb,_,disrupts). % GR commented it out
word_corresponds_to_semnet_relation_GEN(degradation,noun,_,disrupted_by). % 06/21/2017 
word_corresponds_to_semnet_relation_GEN(degrade,verb,_,disrupts). % 06/21/2017 
word_corresponds_to_semnet_relation_GEN(destruct,verb,_,disrupts). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(destruction,noun,_,disrupts). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(diminish,verb,_,disrupts). % 06/21/2017 
word_corresponds_to_semnet_relation_GEN(diminishing,adj,_,disrupts). % 06/21/2017 
word_corresponds_to_semnet_relation_GEN(downregulate,verb,_,disrupts).
%word_corresponds_to_semnet_relation_GEN(downregulate,noun,_,disrupts). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(dull,verb,_,disrupts). % GR commented it out
%word_corresponds_to_semnet_relation_GEN('decreased activity',noun,_,disrupts). % GR commented it out
word_corresponds_to_semnet_relation_GEN(downmodulate,verb,_,disrupts). % 06/22/2017 
word_corresponds_to_semnet_relation_GEN(downmodulation,noun,_,disrupts). % 06/22/2017 
%word_corresponds_to_semnet_relation_GEN(down,-,regulate,verb,_,disrupts). % did not work, Halil
%word_corresponds_to_semnet_relation_GEN(downregulation,noun,of,disrupts). % NOM remove
word_corresponds_to_semnet_relation_GEN(downregulation,noun,_,disrupts). % NOM add
%word_corresponds_to_semnet_relation_GEN(down-regulation,noun,of,disrupts). % NOM remove
word_corresponds_to_semnet_relation_GEN([down,-,regulation],noun,_,disrupts). % NOM add
%word_corresponds_to_semnet_relation_GEN(downregulation,noun,in,disrupts). % GR commented it out
%word_corresponds_to_semnet_relation_GEN(down-regulation,noun,in,disrupts). % GR commented it out
word_corresponds_to_semnet_relation_GEN(eradicate,verb,_,disrupts). % GR 05/31/2017 source: Negative terms list  PMID: 28440853
word_corresponds_to_semnet_relation_GEN(eradication,noun,_,disrupts). % GR 05/31/2017 source: Negative terms list
%word_corresponds_to_semnet_relation_GEN(fall,noun,in,disrupts). % GR commented it out
word_corresponds_to_semnet_relation_GEN(impede,verb,_,disrupts).
word_corresponds_to_semnet_relation_GEN(inactivation,verb,_,disrupts). % NOM untouched
word_corresponds_to_semnet_relation_GEN(inactivate,verb,_,disrupts).
word_corresponds_to_semnet_relation_GEN(inactivator,noun,of,disrupts).
word_corresponds_to_semnet_relation_GEN(inhibit,verb,_,disrupts).
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,of-by,disrupted_by). % NOM remove 
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,of-with,disrupted_by). % NOM remove
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,of,disrupts). % NOM remove
word_corresponds_to_semnet_relation_GEN(inhibition,noun,during,disrupted_by). % NOM unotuched % disrupts?
%word_corresponds_to_semnet_relation_GEN(inhibition,noun,with,disrupted_by). % NOM remove
word_corresponds_to_semnet_relation_GEN(inhibition,noun,_,disrupts). % NOM untouched
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,of-include,disrupted_by).% too many nominalization errors
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,of,disrupts). % too many nominalization errors
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,for,disrupts). % too many nominalization errors
% word_corresponds_to_semnet_relation_GEN(inhibitor,noun,to,disrupts). % too many nominalization errors
%word_corresponds_to_semnet_relation_GEN(lower,verb,_,disrupts). % GR 05/23/2017 --from Negation project
%word_corresponds_to_semnet_relation_GEN(lowering,adj,_,disrupts).
word_corresponds_to_semnet_relation_GEN(negate,verb,_,disrupts).
word_corresponds_to_semnet_relation_GEN(oppose,verb,_,disrupts).
%word_corresponds_to_semnet_relation_GEN(prevent,verb,_,disrupts). %MF prevents means prevents and not disrupts 09/06/07
%word_corresponds_to_semnet_relation_GEN(prevention,noun,of-by,disrupts). %MF prevents means prevents and not disrupts 09/06/07
%word_corresponds_to_semnet_relation_GEN(prevention,noun,of-with,disrupts). %MF prevents means prevents and not disrupts 09/06/07
%word_corresponds_to_semnet_relation_GEN(protect,verb,against,disrupts). %MF protect means prevents and not disrupts 09/06/07
word_corresponds_to_semnet_relation_GEN(reduce,verb,_,disrupts).
word_corresponds_to_semnet_relation_GEN(reduced,adj,_,disrupts).
word_corresponds_to_semnet_relation_GEN(reduced,noun,_,disrupts). % NOM untouched
word_corresponds_to_semnet_relation_GEN(reduction,noun,in,disrupts). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(reduction,noun,of,disrupts). % NOM remove
word_corresponds_to_semnet_relation_GEN(reduction,noun,_,disrupts). % NOM untouched
word_corresponds_to_semnet_relation_GEN(repress,verb,_,disrupts).
%word_corresponds_to_semnet_relation_GEN(repression,noun,of,disrupts). % NOM remove
word_corresponds_to_semnet_relation_GEN(repression,noun,_,disrupts). % NOM add
word_corresponds_to_semnet_relation_GEN(repressor,noun,of,disrupts). % NOM untouched
word_corresponds_to_semnet_relation_GEN(reverse,verb,_,disrupts). % 05/31/2017 source: GENIA - 8887687_S8 E-selectin plays an important role in mediating neutrophil adhesion. Platelet activating factor (PAF) contributed to cell adhesion.
word_corresponds_to_semnet_relation_GEN(shrink,verb,_,disrupts). % GR
word_corresponds_to_semnet_relation_GEN(silencing,adj,_,disrupts). % GR 05/23/2017 --from Negation project
word_corresponds_to_semnet_relation_GEN(slow,verb,_,disrupts). % GR
word_corresponds_to_semnet_relation_GEN(slowed,adj,_,disrupts).
word_corresponds_to_semnet_relation_GEN(suppress,verb,_,disrupts).
%word_corresponds_to_semnet_relation_GEN(suppression,noun,of,disrupts). % NOM remove
word_corresponds_to_semnet_relation_GEN(suppression,noun,_,disrupts). % NOM add
word_corresponds_to_semnet_relation_GEN(suppressing,adj,_,disrupts). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(uncoupling,adj,_,disrupts). % GR 05/23/2017 --from Negation project
% AUGMENTS/HAS_AUGMENTATION
word_corresponds_to_semnet_relation_GEN(accelerate,verb,_,augments). % GR 06/01/2017 source: GENIA 1851861_S2 Human herpesvirus 6 (HHV-6) can activate the human immunodeficiency virus (HIV) promoter and accelerate cytopathic effects in HIV-infected human T cells.
word_corresponds_to_semnet_relation_GEN(activate,verb,_,augments).
%word_corresponds_to_semnet_relation_GEN(activation,noun,of_by,has_augmentation). % NOM remove
word_corresponds_to_semnet_relation_GEN(activation,noun,_,augments). % NOM add?
word_corresponds_to_semnet_relation_GEN(activated,adj,_,augments). % GR 06/15/2017
word_corresponds_to_semnet_relation_GEN(activated,pastpart,_,augments). %GR 03/21/08
word_corresponds_to_semnet_relation_GEN(enhance,verb,_,augments).
%word_corresponds_to_semnet_relation_GEN(enhancement,noun,of-by,has_augmentation). % NOM remove
word_corresponds_to_semnet_relation_GEN(enhancement,noun,_,augments). % NOM add
word_corresponds_to_semnet_relation_GEN(upregulation,noun,_,augments). % NOM untouched
word_corresponds_to_semnet_relation_GEN(activator,noun,of,augments). % NOM untouched
word_corresponds_to_semnet_relation_GEN(augment,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(catalyse,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(contribute,verb,to,augments).
%word_corresponds_to_semnet_relation_GEN(contribution,noun,of-to,augments). % NOM remove
word_corresponds_to_semnet_relation_GEN(contribution,noun,to,augments). % NOM add (lexicon bug?)
word_corresponds_to_semnet_relation_GEN(costimulate,verb,_,augments). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(costimulation,noun,of-with,augments). % GR 06/01/2017 source: GENIA 7543515_S1 Costimulation of human CD4+ T cells with LFA-3 and B7 induce distinct effects on AP-1 and NF-kappa B transcription factors.
%word_corresponds_to_semnet_relation_GEN(dependence,noun,of-on,augments). % NOM remove
%word_corresponds_to_semnet_relation_GEN(dependence,noun,of-by,augments). % NOM remove
word_corresponds_to_semnet_relation_GEN(dependence,noun,_,has_augmentation). % NOM add % was augments?
word_corresponds_to_semnet_relation_GEN(depend,verb,upon,augments).
word_corresponds_to_semnet_relation_GEN(depend,verb,on,augments).
%word_corresponds_to_semnet_relation_GEN(dependant,adj,upon,enhanced_by).
%word_corresponds_to_semnet_relation_GEN(dependant,adj,on,enhanced_by).
word_corresponds_to_semnet_relation_GEN(dependant,adj,_,augments).
%word_corresponds_to_semnet_relation_GEN(dependent,adj,upon,enhanced_by).
%word_corresponds_to_semnet_relation_GEN(dependent,adj,on,enhanced_by). % NOM untouched
word_corresponds_to_semnet_relation_GEN(dependent,adj,_,augments).
word_corresponds_to_semnet_relation_GEN(driven,adj,_,augments). % GR 06/02/2017 source: GENIA 9764907_S9 Cepharanthine was found to suppress HIV-1 LTR-driven gene expression through the inhibition of NF-kappaB activation.
word_corresponds_to_semnet_relation_GEN(driven,pastpart,_,augments).
word_corresponds_to_semnet_relation_GEN(elevation,noun,in,augments).
word_corresponds_to_semnet_relation_GEN(elicit,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(enrich,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(exacerbate,verb,_,augments). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(exacerbation,noun,_,augments). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(facilitate,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(facilitating,adj,_,augments). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(improve,verb,_,augments). % NOM remove
word_corresponds_to_semnet_relation_GEN(increase,verb,_,augments). % NOM untouched
%word_corresponds_to_semnet_relation_GEN(increase,noun,in,augments).
word_corresponds_to_semnet_relation_GEN(increase,noun,_,augments).
% word_corresponds_to_semnet_relation_GEN(increased,adj,_,augments).
word_corresponds_to_semnet_relation_GEN(increased,pastpart,_,augments).
word_corresponds_to_semnet_relation_GEN(increasing,adj,_,augments).
%word_corresponds_to_semnet_relation_GEN(induction,noun,of-by,has_augmentation). % NOM remove
%word_corresponds_to_semnet_relation_GEN(induction,noun,by,has_augmentation). % NOM remove
word_corresponds_to_semnet_relation_GEN(induction,noun,_,augments). % NOM add
word_corresponds_to_semnet_relation_GEN(induced,adj,_,augments).
word_corresponds_to_semnet_relation_GEN(induce,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(inducible,noun,by,has_augmentation). % called head - ergo noun % NOM untouched
word_corresponds_to_semnet_relation_GEN(intensify,verb,_,augments). % GR 01/11/08
%word_corresponds_to_semnet_relation_GEN(intensification,noun,of,augments). % GR 10/28/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(intensification,noun,of-by,enhanced_by). % GR 10/28/08 % NOM remove
word_corresponds_to_semnet_relation_GEN(intensification,noun,_,augments). % NOM add
%word_corresponds_to_semnet_relation_GEN(intensification,noun,_,enhances). % NOM add
word_corresponds_to_semnet_relation_GEN(intensifying,adj,_,augments). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(optimize,verb,_,augments). % GR 10/28/08
word_corresponds_to_semnet_relation_GEN(optimization,noun,_,augments). % GR 10/28/08 % NOM add
%word_corresponds_to_semnet_relation_GEN(optimization,noun,of-by,enhanced_by). % GR 10/28/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(optimization,noun,of-with,enhanced_by). % GR 10/28/08 % NOM remove
%word_corresponds_to_semnet_relation_GEN(optimization,noun,_,enhances). % GR 10/28/08 % NOM add
word_corresponds_to_semnet_relation_GEN(potentiate,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(potentiated,adj,_,augments). % GR 06/08/09
word_corresponds_to_semnet_relation_GEN(prolong,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(promote,verb,_,augments). % GR 06/27/2017 source: GS 9374467_S7 JNK activation in calcineurin-stimulated cells caused nuclear exclusion of NFAT4.
% word_corresponds_to_semnet_relation_GEN(protect,verb,_,augments). % GR commented it out
word_corresponds_to_semnet_relation_GEN(sensitize,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(stimulate,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(stimulate,adj,_,augments).
word_corresponds_to_semnet_relation_GEN(stimulate,noun,_,augments). % NOM untouched
word_corresponds_to_semnet_relation_GEN(stimulated,adj,_,augments). % GR 06/02/2017 source: GENIA 9374467_S7 JNK activation in calcineurin-stimulated cells caused nuclear exclusion of NFAT4.
word_corresponds_to_semnet_relation_GEN(stimulating,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(stimulating,noun,_,augments). % CA - tagger issues % NOM untouched
word_corresponds_to_semnet_relation_GEN(stimulating,adj,_,augments). % CA - tagger issues
%word_corresponds_to_semnet_relation_GEN(stimulation,noun,of,augments). % NOM remove
word_corresponds_to_semnet_relation_GEN(stimulation,noun,_,augments). % NOM add
word_corresponds_to_semnet_relation_GEN(synergize,verb,_,augments). % GR 06/05/2017 From GENIA 28315341 Low doses of tizanidine synergize the anti-nociceptive and anti-inflammatory effects of ketorolac or naproxen while reducing of side effects. 
word_corresponds_to_semnet_relation_GEN(synergy,noun,_,augments). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(transactivate,verb,_,augments). % GR 06/01/2017 source: GENIA A truncated Tat protein (Tat1-72), known to transactivate the HIV-1 long terminal repeat (LTR), no longer affected Mn-SOD expression, the cellular redox state or TNF-mediated cytotoxicity.
word_corresponds_to_semnet_relation_GEN(transactivation,noun,_,augments). % GR 06/01/2017 source: GENIA A truncated Tat protein (Tat1-72), known to transactivate the HIV-1 long terminal repeat (LTR), no longer affected Mn-SOD expression, the cellular redox state or TNF-mediated cytotoxicity.
%word_corresponds_to_semnet_relation_GEN(up-regulate,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(upregulate,verb,_,augments).
%word_corresponds_to_semnet_relation_GEN(up-regulates,verb,_,augments).
%word_corresponds_to_semnet_relation_GEN(upregulation,noun,by,has_augmentation). % NOM remove
word_corresponds_to_semnet_relation_GEN(upregulation,noun,in,augments). % NOM untouched
word_corresponds_to_semnet_relation_GEN(upregulation,noun,_,augments). % NOM add
word_corresponds_to_semnet_relation_GEN([up,-,regulation],noun,in,augments). % NOM untouched
word_corresponds_to_semnet_relation_GEN([up,-,regulation],noun,_,augments). % NOM add
% ----- CONVERTS_TO/HAS_CONVERSION % CA added
%word_corresponds_to_semnet_relation_GEN(biotransformation,noun,of-to,converts_to). % NOM remove
word_corresponds_to_semnet_relation_GEN(biotransformation,noun,to,converts_to). % NOM untouched
word_corresponds_to_semnet_relation_GEN(convert,verb,to,converts_to). % 05/23/2017 This rule does not fire
word_corresponds_to_semnet_relation_GEN(converts,verb,to,converts_to). % 11/16/2017 as per Halil fix of Bug 61
word_corresponds_to_semnet_relation_GEN(convert,verb,into,converts_to). % 05/23/2017 - source GS
word_corresponds_to_semnet_relation_GEN(converts,verb,into,converts_to). % 11/16/2017 as per Halil fix of Bug 61
%word_corresponds_to_semnet_relation_GEN(conversion,noun,of-to,converts_to). % NOM remove
word_corresponds_to_semnet_relation_GEN(conversion,noun,to,converts_to). % NOM untouched
word_corresponds_to_semnet_relation_GEN(conversion,noun,into,converts_to). % 05/23/2017 - source GS
word_corresponds_to_semnet_relation_GEN(derive,verb,from,has_conversion).
word_corresponds_to_semnet_relation_GEN(derived,adj,_,converts_to).
word_corresponds_to_semnet_relation_GEN(dimerization,noun,_,converts_to). % 06/22/2017 
%word_corresponds_to_semnet_relation_GEN(form,verb,_,converts_to).
%word_corresponds_to_semnet_relation_GEN(formed,adj,_,converts_to).
%word_corresponds_to_semnet_relation_GEN(formation,noun,of-from,has_conversion).
word_corresponds_to_semnet_relation_GEN(oxidize,verb,from-to,converts_to). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(oxidizing,adj,_,converts_to). % 06/14/2017 
word_corresponds_to_semnet_relation_GEN(transduction,noun,_,converts_to). % GR 06/05/2017 source: GENIA 7506531_S2 Rapid tyrosine phosphorylation of key cellular proteins is a crucial event in the transduction of activation signals to T-lymphocytes. NF-kappa B translocates from cytosol to nucleus as a result of transduction by tumor necrosis factor alpha (TNF alpha), phorbol ester, and other polyclonal signals.
%word_corresponds_to_semnet_relation_GEN(from,prep,_,part_of). % dangerous rule
word_corresponds_to_semnet_relation_GEN(in,prep,_,treats). % tcr-provisional
word_corresponds_to_semnet_relation_GEN(occur,verb,in,coexists_with). % GR 12/12/2017  [Our results indicate that lipid peroxidation [moft] occurs in X-ALD [dsyn] Note: I also added the corresponding ontological predication and occur_with is already an indicator
word_corresponds_to_semnet_relation_GEN(occurring,adj,in,coexists_with). % GR 12/12/2017
% ----- LOCATION_OF/HAS_LOCATION % 
word_corresponds_to_semnet_relation_GEN(at,prep,_,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(exist,verb,_,has_location). % NOM untouched
word_corresponds_to_semnet_relation_GEN(egress,verb,from,has_location). % GR 12/20/2017
word_corresponds_to_semnet_relation_GEN(find,verb,_,has_location). % tcr-spat-provisional 
word_corresponds_to_semnet_relation_GEN(formation,noun,in,has_location).
word_corresponds_to_semnet_relation_GEN(from,prep,_,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(have,verb,_,location_of). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(harbor,verb,_,location_of). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(harbour,verb,_,location_of). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(harboring,verb,_,location_of). % 05/25/2017 source: GS - PMID 15561572
word_corresponds_to_semnet_relation_GEN(harboring,adj,_,location_of). % 05/25/2017 source: GS - PMID 15561572
word_corresponds_to_semnet_relation_GEN(harbouring,adj,_,location_of). % 05/25/2017 source: GS - PMID 15561572
word_corresponds_to_semnet_relation_GEN(implant,verb,into,has_location). % 05/25/2017 source: GS - PMID 15561572
word_corresponds_to_semnet_relation_GEN(implant,verb,in,has_location).
word_corresponds_to_semnet_relation_GEN(implanted,adj,in,has_location).
word_corresponds_to_semnet_relation_GEN(locate,verb,at,location_of).
%word_corresponds_to_semnet_relation_GEN(location,noun,of,location_of). % NOM remove
word_corresponds_to_semnet_relation_GEN(location,noun,_,location_of). % NOM add
word_corresponds_to_semnet_relation_GEN(in,prep,_,has_location). % tcr-spat
word_corresponds_to_semnet_relation_GEN(inside,prep,_,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(mod_head,_,_,location_of). % tcr-spat 
word_corresponds_to_semnet_relation_GEN(of,prep,_,has_location). % tcr-spat
word_corresponds_to_semnet_relation_GEN(on,prep,_,has_location). % tcr-provisional
word_corresponds_to_semnet_relation_GEN(originate,verb,in,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(originate,verb,from,has_location). % GR 12/18/2017
word_corresponds_to_semnet_relation_GEN(originating,adj,in,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(originating,adj,from,has_location). % GR 12/18/2017
word_corresponds_to_semnet_relation_GEN(originating,noun,in,has_location).
word_corresponds_to_semnet_relation_GEN(originating,noun,from,has_location).
word_corresponds_to_semnet_relation_GEN(place,verb,into,has_location). % GR 01/11/08
word_corresponds_to_semnet_relation_GEN(site,noun,for,location_of). % GR 08/06/08 % NOM untouched
word_corresponds_to_semnet_relation_GEN(site,noun,of,location_of). % tcr-spat % NOM untouched
word_corresponds_to_semnet_relation_GEN(terminating,adj,in,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(terminate,verb,in,has_location). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(translocate,verb,to,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(translocation,noun,to,has_location). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(traverse,verb,_,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(traversing,adj,_,has_location). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(traversing,noun,_,has_location). % 11/28/2017 
% word_corresponds_to_semnet_relation_GEN(with,     prep, _,  location_of). % tcr-spat-provisional % CA chg to A-W
word_corresponds_to_semnet_relation_GEN(within,prep,_,has_location). % GR 04/29/09
word_corresponds_to_semnet_relation_GEN(phosphorylation,noun,_,interacts_with). % Halil, for argnom project
word_corresponds_to_semnet_relation_GEN(phosphorylate,verb,_,interacts_with). % Halil, for argnom project
word_corresponds_to_semnet_relation_GEN(production,noun,_,produces). % Halil, for argnom project
word_corresponds_to_semnet_relation_GEN(inhibitor,noun,of,inhibits). % Halil, for argnom project
word_corresponds_to_semnet_relation_GEN(inhibitor,noun,for,inhibits). % Halil, for argnom project
word_corresponds_to_semnet_relation_GEN(inhibitor,noun,of,disrupts). % Halil, for argnom project
word_corresponds_to_semnet_relation_GEN(inhibitor,noun,for,disrupts). % Halil, for argnom project
% From Graciela, 5/7/2012, generalized from domain migration projects
word_corresponds_to_semnet_relation_GEN(impinge,verb,_,affects).
word_corresponds_to_semnet_relation_GEN(impingement,noun,_,affected_by). % GR 06/14/2015
word_corresponds_to_semnet_relation_GEN(implication,noun,for,affects).
word_corresponds_to_semnet_relation_GEN(across,prep,_,has_location). % Seemed suspicious, but Graciela sent some examples 5/8/2012.
word_corresponds_to_semnet_relation_GEN(using,adj,for,method_of). % 06/26/2017 Source: GS
word_corresponds_to_semnet_relation_GEN(using,adj,_,has_method).
word_corresponds_to_semnet_relation_GEN(through,prep,_,method_of).
word_corresponds_to_semnet_relation_GEN(via,prep,_,has_method).
word_corresponds_to_semnet_relation_GEN(exploit,verb,_,uses).
% From SemGen, curated by Graciela and Marcelo 5/23/2014
word_corresponds_to_semnet_relation_GEN(deplete,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(depletion,noun,_,inhibits). % 05/31/2017 source: GENIA -10202937_S6 Fludarabine caused a specific depletion of STAT1 protein (and mRNA) but not of other STATs.
%word_corresponds_to_semnet_relation_GEN(deplete,verb,_,disrupts). % GR commented this out on 05/25/2017 as per email communication with Tom on 3/17/2016
word_corresponds_to_semnet_relation_GEN(destabilize,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(impair,verb,_,inhibits).
word_corresponds_to_semnet_relation_GEN(impair,verb,_,disrupts).
%word_corresponds_to_semnet_relation_GEN(coexpress,verb,_,produces). % commented out by GR on 06/23/2017 as per conversation with Halil
%word_corresponds_to_semnet_relation_GEN([co,-,express],verb,_,produces). % commented out by GR on 06/23/2017 as per conversation with Halil
% because base processing does not work well.
word_corresponds_to_semnet_relation_GEN([co,-,expresses],verb,_,produces).
word_corresponds_to_semnet_relation_GEN([co,-,expressed],verb,_,produces).
word_corresponds_to_semnet_relation_GEN(recruit,verb,_,interacts_with).
word_corresponds_to_semnet_relation_GEN(recruitment,noun,_,interacts_with). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(amplification,verb,_,stimulates). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(amplify,verb,_,augments).
word_corresponds_to_semnet_relation_GEN(amplification,verb,_,stimulates). % GR 06/14/2017
word_corresponds_to_semnet_relation_GEN(elevate,verb,_,stimulates).
word_corresponds_to_semnet_relation_GEN(synergize,verb,_,stimulates). % GR 06/05/2017 source: GENIA 25933584 Host plant chemicals chemicals synergize or enhance the emission of sex pheromones or the response from the receiver. 
word_corresponds_to_semnet_relation_GEN(induct,verb,_,causes).
word_corresponds_to_semnet_relation_GEN(raise,verb,_,stimulates). % Halil, from Rui Zhang sentences, 5/27/14
word_corresponds_to_semnet_relation_GEN(raise,verb,_,augments). % Halil 
word_corresponds_to_semnet_relation_GEN(link,verb,to,coexists_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(linked,verb,to,coexists_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(linked,adj,to,coexists_with). % GR 07/2015 On 11/20/2017 GR changed POS from pastpart to adj
word_corresponds_to_semnet_relation_GEN(unrelated,adj,to,neg_coexists_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(via,prep,_,interacts_with). % GR 07/2015
word_corresponds_to_semnet_relation_GEN(accumulate,verb,in,has_location). % GR 06/05/2017 source: GENIA 28457857 Senescent cells accumulate in tissues with age. 
word_corresponds_to_semnet_relation_GEN(accumulate,verb,_,location_of). % GR 06/05/2017 source: GENIA 9374467_S7 Upon activation, T lymphocytes accumulate high levels of the neuropeptide enkephalin
word_corresponds_to_semnet_relation_GEN(activate,verb,_,affects). % 06/15/2017  this should be the default in case everything else fails
word_corresponds_to_semnet_relation_GEN(choice,noun,for,treats). %12/18/2017  
word_corresponds_to_semnet_relation_GEN(drug,noun,for,used_by). %12/18/2017 
word_corresponds_to_semnet_relation_GEN(free,adj,from,neg_process_of). % GR 01/17/2018 To keep under observation
word_corresponds_to_semnet_relation_GEN(used,adj,in,used_by). % GR 01/24/2018 To keep under observation
%word_corresponds_to_semnet_relation_GEN([-,based],adj,_,used_by). % GR 01/26/2018 Negation corpus
%word_corresponds_to_semnet_relation_GEN([-,based],pastpart,_,used_by). % GR 01/26/2018 Negation corpus
%word_corresponds_to_semnet_relation_GEN([-,based],adj,_,location_of). % GR 01/26/2018 Negation corpus
%word_corresponds_to_semnet_relation_GEN([-,based],pastpart,_,location_of). % GR 01/26/2018 Negation corpus
word_corresponds_to_semnet_relation_GEN(based,adj,_,used_by). % GR 01/26/2018 Negation corpus
word_corresponds_to_semnet_relation_GEN(based,pastpart,_,used_by). % GR 01/26/2018 Negation corpus
word_corresponds_to_semnet_relation_GEN(based,adj,_,location_of). % GR 01/26/2018 Negation corpus
word_corresponds_to_semnet_relation_GEN(based,pastpart,_,location_of). % GR 01/26/2018 Negation corpus
% ----------- New NEG_ predicates from Negation file
%word_corresponds_to_semnet_relation_GEN(absence,noun,_,neg_coexists_with). % GR 06/13/2017 - commented out on 08/2018
%word_corresponds_to_semnet_relation_GEN(absence,noun,_,neg_process_of). % GR 06/13/2017 - commented out on 08/2018
word_corresponds_to_semnet_relation_GEN(derepress,verb,_,neg_disrupts). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(derepress,verb,_,neg_inhibits). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(derepression,noun,_,neg_disrupts). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(derepression,noun,_,neg_inhibits). % GR 06/13/2017 Example: To assess the involvement of dUTP binding and dUTPase activity in derepression by DutNM1. Only the apo form of DutNM1 is active in Stl derepression. A novel role for Taf14 in repression and derepression of stress induced genes.
word_corresponds_to_semnet_relation_GEN(devoid,adj,_,neg_part_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(devoid,adj,_,neg_location_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(devoid,adj,_,neg_process_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(incompatible,adj,with,neg_coexists_with). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(incompatible,adj,with,neg_process_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(insensitive,adj,_,neg_affects). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(insensitive,adj,to,neg_affected_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lack,verb,_,neg_has_part). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lack,verb,_,neg_location_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lack,verb,_,neg_has_process). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lack,noun,_,neg_has_part). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lack,noun,_,neg_location_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lack,noun,_,neg_has_process). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lacking,adj,_,neg_has_part). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lacking,adj,_,neg_location_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lacking,adj,_,neg_has_process). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lacking,adj,in,neg_has_part). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lacking,adj,in,neg_location_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(lacking,adj,in,neg_has_process). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(miss,verb,_,neg_diagnoses). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(miss,verb,_,neg_has_administration). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(missing,adj,_,neg_process_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(negative,adj,for,neg_has_process). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(negative,adj,for,neg_diagnoses). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(negative,adj,_,neg_process_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(negative,adj,_,neg_part_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(negative,adj,_,affects). % GR 06/13/2017 for lack of 'negatively affects'
word_corresponds_to_semnet_relation_GEN(neither,adj,_,neg_process_of). % GR 06/13/2017 
%word_corresponds_to_semnet_relation_GEN(nonexpressing,adj,_,neg_produced_by). % GR 06/13/2017  commented out by GR on 06/23/2017 as per conversation with Halil
word_corresponds_to_semnet_relation_GEN(nonproducing,adj,_,neg_produced_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(nonsecreting,adj,_,neg_produced_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(omit,verb,_,neg_administered_to). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(omit,verb,_,neg_uses). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(omitted,adj,from,neg_used_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(omitted,adj,from,neg_administered_to). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(omitted,adj,_,neg_used_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(omitted,adj,_,neg_administered_to). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(refractory,adj,to,neg_affected_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(refractory,adj,to,neg_treated_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(refractory,adj,_,neg_affected_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(refractory,adj,_,neg_treated_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(resistance,noun,to,neg_affected_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(resistance,noun,to,neg_treated_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(resistant,adj,to,neg_treated_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(resistant,adj,to,neg_process_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(resistant,adj,to,neg_affected_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(resistant,adj,_,neg_treated_by). % GR 06/13/2017 
%word_corresponds_to_semnet_relation_GEN(spare,verb,_,neg_affected_by). % GR 06/13/2017 
%word_corresponds_to_semnet_relation_GEN(spare,verb,_,neg_process_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(sparing,adj,_,neg_used_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(sparing,adj,_,neg_affected_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(sparing,adj,_,neg_disrupted_by). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(uninfected,adj,_,neg_process_of). % GR 06/13/2017 
%word_corresponds_to_semnet_relation_GEN(uninfected,adj,_,neg_part_of). % GR 06/13/2017 
word_corresponds_to_semnet_relation_GEN(unresponsive,adj,to,neg_treated_by). % GR 06/13/2017 
%word_corresponds_to_semnet_relation_GEN(rule,[out],verb,[adv]). for
%word_corresponds_to_semnet_relation_GEN(rule,[out],verb,[adv]). _

word_corresponds_to_semnet_relation_GEN(treating,noun,_,treats).
word_corresponds_to_semnet_relation_GEN(detecting,adj,_,diagnoses).
word_corresponds_to_semnet_relation_GEN(use,verb,for,treats).
word_corresponds_to_semnet_relation_GEN(present,adj,in,has_location).
word_corresponds_to_semnet_relation_GEN(adopt,verb,for,treats).
word_corresponds_to_semnet_relation_GEN(use,verb,for,used_by).

word_corresponds_to_semnet_relation_GEN(target,verb,_,inhibits). % GR 04/12/2018 --from Contradictions project => medication that targets a substance that causes a disease - 
word_corresponds_to_semnet_relation_GEN(targeting,adj,_,inhibits). % GR 04/12/2018 --from Contradictions project => medication that targets a substance that causes a disease - 
word_corresponds_to_semnet_relation_GEN(targeting,noun,_,inhibits). % GR 04/12/2018 --from Contradictions project => medication that targets a substance that causes a disease -
word_corresponds_to_semnet_relation_GEN(target,verb,_,treats).
word_corresponds_to_semnet_relation_GEN(targeting,adj,_,treats).
word_corresponds_to_semnet_relation_GEN(targeting,noun,_,treats).

word_corresponds_to_semnet_relation_GEN(comorbidity,noun,_,coexists_with).
