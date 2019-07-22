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
*  For full details, please see the MetaMap Terms & Conditions, available at
*  https://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% negex triggers called by negex.pl

:- module(negex_triggers,  [
	nega_phrase_tokens/2,
	negb_phrase_tokens/2,
	pnega_phrase_tokens/2,
	pnegb_phrase_tokens/2,
	pseudoneg_phrase_tokens/2,
	conj_phrase_tokens/2
   ]).

%
% List of negation triggers.
%

% In https://code.google.com/p/negex/wiki/NegExTerms,
% these are "Pre-condition negation terms (used to mark an indexed term as negated)".

nega_phrase_tokens(absence, [of]).
nega_phrase_tokens('aren''t', []). % GR
nega_phrase_tokens(barring, []). % GR
nega_phrase_tokens(cannot, []).
nega_phrase_tokens('can''t', []). % GR
%nega_phrase_tokens(cannot, [see]). % commented out by GR
%nega_phrase_tokens(checked, [for]). % commented out by GR
nega_phrase_tokens('couldn''t', []). % GR
nega_phrase_tokens(declined, []).
nega_phrase_tokens(declines, []).
nega_phrase_tokens(deny, []).
nega_phrase_tokens(denied, []).
nega_phrase_tokens(denies, []).
nega_phrase_tokens(denying, []).
nega_phrase_tokens('didn''t', []). % GR
nega_phrase_tokens(disappearance, [of]). % GR
nega_phrase_tokens('doesn''t', []). % GR
nega_phrase_tokens('don''t', []). % GR
%nega_phrase_tokens(evaluate, [for]).
nega_phrase_tokens(except, []). % GR
nega_phrase_tokens(except, [for]). % GR
nega_phrase_tokens(exclude, []). % GR
nega_phrase_tokens(excluding, []). % GR
nega_phrase_tokens(fail, [to]). % GR
nega_phrase_tokens(fails, [to,reveal]). % candidate for deletion
nega_phrase_tokens(failed, [to]). % GR
nega_phrase_tokens(failure, [to]). % GR
nega_phrase_tokens(free, [of]).
nega_phrase_tokens('hadn''t', []). % GR
nega_phrase_tokens('hasn''t', []). % GR
nega_phrase_tokens('haven''t', []). % GR
nega_phrase_tokens(inability, [to]). % GR
nega_phrase_tokens(incapable, [of]). % GR
nega_phrase_tokens(instead, [of]). % GR
nega_phrase_tokens(insufficient, [to]). % GR
nega_phrase_tokens('isn''t', []). % GR
nega_phrase_tokens(lost, [the,ability,to]). % GR
%nega_phrase_tokens(negative, [for]).
nega_phrase_tokens(neither, []). % GR
nega_phrase_tokens(never, []). %HK
nega_phrase_tokens(never, [developed]). % candidate for deletion
nega_phrase_tokens(never, [had]). % candidate for deletion
nega_phrase_tokens(no, []).
nega_phrase_tokens(no, [abnormal]). % candidate for deletion
nega_phrase_tokens(no, [cause,of]).
nega_phrase_tokens(no, [complaints,of]). 
nega_phrase_tokens(no, [evidence]).
nega_phrase_tokens(no, [new,evidence]).
nega_phrase_tokens(no, [other,evidence]).
nega_phrase_tokens(no, [evidence,to,suggest]).
nega_phrase_tokens(no, [findings,of]).
nega_phrase_tokens(no, [findings,to,indicate]).
nega_phrase_tokens(no, [history,of]). % GR
nega_phrase_tokens(no, [indication,of]). % GR
nega_phrase_tokens(no, [longer]). % GR
nega_phrase_tokens(no, [mammographic,evidence,of]).
nega_phrase_tokens(no, [new]).
nega_phrase_tokens(no, [other]). % GR
nega_phrase_tokens(no, [radiographic,evidence,of]).
nega_phrase_tokens(no, [sign,of]).
nega_phrase_tokens(no, [significant]). % GR even when sth is no/not significant, sth is there
nega_phrase_tokens(no, [signs,of]).
nega_phrase_tokens(no, [suggestion,of]).
nega_phrase_tokens(no, [suspicious]).
nega_phrase_tokens(no, [one]). % GR
nega_phrase_tokens(nobody, []). % GR
nega_phrase_tokens(non, []).
nega_phrase_tokens(none, []).	% GR
nega_phrase_tokens(nor, []). % GR
nega_phrase_tokens(not, []).
nega_phrase_tokens(not, [appear]). % candidate for deletion?
nega_phrase_tokens(not, [appreciate]). % candidate for deletion?
%nega_phrase_tokens(not, [associated,with]). % candidate for deletion?
nega_phrase_tokens(not, [complain,of]).
%nega_phrase_tokens(not, [demonstrate]). % candidate for deletion?
%nega_phrase_tokens(not, [exhibit]). % candidate for deletion?
nega_phrase_tokens(not, [feel]). % candidate for deletion?
%nega_phrase_tokens(not, [had]). % candidate for deletion?
%nega_phrase_tokens(not, [have]). % candidate for deletion?
nega_phrase_tokens(not, [know,of]).
nega_phrase_tokens(not, [known,to,have]).
%nega_phrase_tokens(not, [reveal]). % candidate for deletion?
nega_phrase_tokens(not, [see]). % candidate for deletion?
nega_phrase_tokens(not, [responsive,to]).
nega_phrase_tokens(not, [sufficiently]).
nega_phrase_tokens(not, [to,be]).
nega_phrase_tokens(nothing, []). % GR
nega_phrase_tokens('n''t', []). % GR candidate for deletion?
nega_phrase_tokens(obvious, [exception,to]). % GR
%nega_phrase_tokens(patient, [was,not]). % not needed if we enter 'was/were not' as negative indicators
nega_phrase_tokens(rather, [than]).
%nega_phrase_tokens(resolved, []). % candidate for deletion?
%nega_phrase_tokens(test, [for]). % candidate for deletion?
nega_phrase_tokens(to, [exclude]).
nega_phrase_tokens(unable, [to]). % GR
nega_phrase_tokens(unactivated, []). % GR
nega_phrase_tokens(unaffect, []). % GR
nega_phrase_tokens(unchanged, [for]). % GR
nega_phrase_tokens(unremarkable, [for]). % GR
%nega_phrase_tokens(unstimulated, []). % GR
nega_phrase_tokens(was, [no]). % GR
nega_phrase_tokens('wasn''t', []). % GR
nega_phrase_tokens(were, [no]). % GR
nega_phrase_tokens('weren''t', []). % GR
nega_phrase_tokens(with, [no]). % GR
%nega_phrase_tokens(with, [or,without]). % GR
nega_phrase_tokens(with, [the,exception,of]). % GR
nega_phrase_tokens(with, [the,notable,exception,of]). % GR
nega_phrase_tokens(without, []).
nega_phrase_tokens(without, [any,evidence,of]).
nega_phrase_tokens(without, [evidence]).
nega_phrase_tokens(without, [indication,of]).
nega_phrase_tokens(without, [sign,of]).
nega_phrase_tokens(rules, [out]).
nega_phrase_tokens(rules, [him,out]). % candidate for deletion?
nega_phrase_tokens(rules, [her,out]). % candidate for deletion?
nega_phrase_tokens(rules, [the,patient,out]). % candidate for deletion?
nega_phrase_tokens(rules, [out,for]).
nega_phrase_tokens(rules, [him,out,for]). % candidate for deletion
nega_phrase_tokens(rules, [her,out,for]). % candidate for deletion
nega_phrase_tokens(rules, [the,patient,out,for]).
nega_phrase_tokens(ruled, [out]).
nega_phrase_tokens(ruled, [him,out]). % candidate for deletion
nega_phrase_tokens(ruled, [her,out]). % candidate for deletion
nega_phrase_tokens(ruled, [the,patient,out]). % candidate for deletion
nega_phrase_tokens(ruled, [out,for]).
nega_phrase_tokens(ruled, [him,out,for]).
nega_phrase_tokens(ruled, [her,out,for]). % candidate for deletion
nega_phrase_tokens(ruled, [the,patient,out,for]). % candidate for deletion
nega_phrase_tokens(ruled, [out,against]).
nega_phrase_tokens(ruled, [him,out,against]). % candidate for deletion
nega_phrase_tokens(ruled, [her,out,against]). % candidate for deletion
nega_phrase_tokens(ruled, [the,patient,out,against]). % candidate for deletion
nega_phrase_tokens(did, [rule,out]).
nega_phrase_tokens(did, [rule,out,for]).
nega_phrase_tokens(did, [rule,out,against]).
nega_phrase_tokens(did, [rule,him,out]). % candidate for deletion
nega_phrase_tokens(did, [rule,her,out]). % candidate for deletion
nega_phrase_tokens(did, [rule,the,patient,out]). % candidate for deletion
nega_phrase_tokens(did, [rule,him,out,for]). % candidate for deletion
nega_phrase_tokens(did, [rule,her,out,for]). % candidate for deletion
nega_phrase_tokens(did, [rule,him,out,against]). % candidate for deletion
nega_phrase_tokens(did, [rule,her,out,against]). % candidate for deletion
nega_phrase_tokens(did, [rule,the,patient,out,for]). % candidate for deletion
nega_phrase_tokens(did, [rule,the,patient,out,against]). % candidate for deletion
nega_phrase_tokens(can, [rule,out]). % candidate for deletion
nega_phrase_tokens(can, [rule,out,for]). % candidate for deletion
nega_phrase_tokens(can, [rule,out,against]). % candidate for deletion
nega_phrase_tokens(can, [rule,him,out]). % candidate for deletion
nega_phrase_tokens(can, [rule,her,out]). % candidate for deletion
nega_phrase_tokens(can, [rule,the,patient,out]). % candidate for deletion
nega_phrase_tokens(can, [rule,him,out,for]). % candidate for deletion
nega_phrase_tokens(can, [rule,her,out,for]). % candidate for deletion
nega_phrase_tokens(can, [rule,the,patient,out,for]). % candidate for deletion
nega_phrase_tokens(can, [rule,him,out,against]). % candidate for deletion
nega_phrase_tokens(can, [rule,her,out,against]). % candidate for deletion
nega_phrase_tokens(can, [rule,the,patient,out,against]). % candidate for deletion
nega_phrase_tokens(adequate, [to,rule,out]).
nega_phrase_tokens(adequate, [to,rule,him,out]). % candidate for deletion
nega_phrase_tokens(adequate, [to,rule,her,out]). % candidate for deletion
nega_phrase_tokens(adequate, [to,rule,the,patient,out]). % candidate for deletion
nega_phrase_tokens(adequate, [to,rule,out,for]).
nega_phrase_tokens(adequate, [to,rule,him,out,for]). % candidate for deletion
nega_phrase_tokens(adequate, [to,rule,her,out,for]). % candidate for deletion
nega_phrase_tokens(adequate, [to,rule,the,patient,out,for]). % candidate for deletion
nega_phrase_tokens(adequate, [to,rule,the,patient,out,against]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,out]).
nega_phrase_tokens(sufficient, [to,rule,him,out]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,her,out]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,the,patient,out]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,out,for]).
nega_phrase_tokens(sufficient, [to,rule,him,out,for]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,her,out,for]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,the,patient,out,for]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,out,against]).
nega_phrase_tokens(sufficient, [to,rule,him,out,against]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,her,out,against]). % candidate for deletion
nega_phrase_tokens(sufficient, [to,rule,the,patient,out,against]). % candidate for deletion
% The following nega_phrase_token was added by NLM
nega_phrase_tokens(with, [no,evidence,of]).
nega_phrase_tokens(unlikely, [to]). % GR

% In https://code.google.com/p/negex/wiki/NegExTerms,
% these are "Post-condition negation terms".

negb_phrase_tokens(absent, []). % GR
negb_phrase_tokens(unlikely, []).
%negb_phrase_tokens(free, []). % GR candidate for deletion
negb_phrase_tokens(-, [free]). % GR 
negb_phrase_tokens(exception, []). % GR
negb_phrase_tokens(obvious, [exception]). % GR
negb_phrase_tokens(was, [ruled,out]).
negb_phrase_tokens(is, [ruled,out]).
negb_phrase_tokens(are, [ruled,out]).
negb_phrase_tokens(have, [been,ruled,out]).
negb_phrase_tokens(has, [been,ruled,out]).
negb_phrase_tokens(is,[negative]).
negb_phrase_tokens(are,[negative]).
negb_phrase_tokens(was,[negative]).
negb_phrase_tokens(were,[negative]).

% In https://code.google.com/p/negex/wiki/NegExTerms,
% these are "Pre-condition possibility phrase (used to mark an indexed term as possible)".

%%% pnega_phrase_tokens(rule, [out]).
%%% pnega_phrase_tokens('r/o', []).
%%% pnega_phrase_tokens(ro, []).
%%% pnega_phrase_tokens(rule, [him,out]).
%%% pnega_phrase_tokens(rule, [her,out]).
%%% pnega_phrase_tokens(rule, [the,patient,out]).
%%% pnega_phrase_tokens(rule, [out,for]).
%%% pnega_phrase_tokens(rule, [him,out,for]).
%%% pnega_phrase_tokens(rule, [her,out,for]).
%%% pnega_phrase_tokens(rule, [the,patient,out,for]).
%%% pnega_phrase_tokens(be, [ruled,out,for]).
%%% pnega_phrase_tokens(should, [be,ruled,out,for]).
%%% pnega_phrase_tokens(ought, [to,be,ruled,out,for]).
%%% pnega_phrase_tokens(may, [be,ruled,out,for]).
%%% pnega_phrase_tokens(might, [be,ruled,out,for]).
%%% pnega_phrase_tokens(could, [be,ruled,out,for]).
%%% pnega_phrase_tokens(will, [be,ruled,out,for]).
%%% pnega_phrase_tokens(can, [be,ruled,out,for]).
%%% pnega_phrase_tokens(must, [be,ruled,out,for]).
%%% pnega_phrase_tokens(is, [to,be,ruled,out,for]).
%%% pnega_phrase_tokens(what, [must,be,ruled,out,is]).
pnega_phrase_tokens(_,_) :- !, fail.
%%% % In https://code.google.com/p/negex/wiki/NegExTerms,
%%% % these are "Post-condition possibility terms (used to mark an indexed term as possible)".

%%% pnegb_phrase_tokens(did, [not,rule,out]).
%%% pnegb_phrase_tokens(not, [ruled,out]).
%%% pnegb_phrase_tokens(not, [been,ruled,out]).
%%% pnegb_phrase_tokens(being, [ruled,out]).
%%% pnegb_phrase_tokens(be, [ruled,out]).
%%% pnegb_phrase_tokens(should, [be,ruled,out]).
%%% pnegb_phrase_tokens(ought, [to,be,ruled,out]).
%%% pnegb_phrase_tokens(may, [be,ruled,out]).
%%% pnegb_phrase_tokens(might, [be,ruled,out]).
%%% pnegb_phrase_tokens(could, [be,ruled,out]).
%%% pnegb_phrase_tokens(will, [be,ruled,out]).
%%% pnegb_phrase_tokens(can, [be,ruled,out]).
%%% pnegb_phrase_tokens(must, [be,ruled,out]).
%%% pnegb_phrase_tokens(is, [to,be,ruled,out]).
%%% pnegb_phrase_tokens(unlikely, []).  % GR
pnegb_phrase_tokens(_,_) :- !, fail.

% In https://code.google.com/p/negex/wiki/NegExTerms,
% these are "Pseudo negation terms".

pseudoneg_phrase_tokens(no,	 [increase]).
pseudoneg_phrase_tokens(no,      [suspicious,change]).
pseudoneg_phrase_tokens(no,      [significant,change]).
pseudoneg_phrase_tokens(no,      [change]).
pseudoneg_phrase_tokens(no,      [interval,change]).
pseudoneg_phrase_tokens(no,      [definite,change]).
pseudoneg_phrase_tokens(no,      [significant,interval,change]).
pseudoneg_phrase_tokens(no,      [further]). %GR
pseudoneg_phrase_tokens(no,      [missing,teeth]). %GR
pseudoneg_phrase_tokens(no,      [missing]). %GR
pseudoneg_phrase_tokens(not,     [extend]).
pseudoneg_phrase_tokens(not,     [cause]).
pseudoneg_phrase_tokens(not,     [drain]).
pseudoneg_phrase_tokens(not,     [certain,if]).
pseudoneg_phrase_tokens(not,     [certain,whether]).
pseudoneg_phrase_tokens(not,     [necessarily]).
pseudoneg_phrase_tokens(not,     [only]).
pseudoneg_phrase_tokens(not,     [unlike]).  %GR
pseudoneg_phrase_tokens(not,     [exclusively]). %GR
pseudoneg_phrase_tokens(not,     [significantly]). %GR
pseudoneg_phrase_tokens(not,     [solely]).	   %GR
pseudoneg_phrase_tokens(not,     [a,common]).	   %GR
pseudoneg_phrase_tokens(not,     [common]).	   %GR
pseudoneg_phrase_tokens(not,     [an,uncommon]).   %GR
pseudoneg_phrase_tokens(not,     [uncommon]).	   %GR
pseudoneg_phrase_tokens(not,     [an,important]).  %GR
pseudoneg_phrase_tokens(not,     [important]).	   %GR
pseudoneg_phrase_tokens(not,     [an,unimportant]).  %GR
pseudoneg_phrase_tokens(not,     [unimportant]).	   %GR
pseudoneg_phrase_tokens(not,     [a,frequent]).  %GR
pseudoneg_phrase_tokens(not,     [frequent]).	 %GR
pseudoneg_phrase_tokens(not,     [an,infrequent]).  %GR
pseudoneg_phrase_tokens(not,     [infrequent]).	 %GR
pseudoneg_phrase_tokens(not,     [a,rare]).  %GR
pseudoneg_phrase_tokens(not,     [rare]). %GR
pseudoneg_phrase_tokens(gram,    [negative]).
pseudoneg_phrase_tokens(without, [difficulty]).
pseudoneg_phrase_tokens(without, [delay]). %GR
pseudoneg_phrase_tokens(without, [question]). %GR
pseudoneg_phrase_tokens(without, [a,doubt]). %GR
pseudoneg_phrase_tokens(without, [further]). %GR
pseudoneg_phrase_tokens(without, [any,further]). %GR
%pseudoneg_phrase_tokens(without, [delay]). %GR
pseudoneg_phrase_tokens(no,      [guarantee,that]). %GR
pseudoneg_phrase_tokens(with,    [and,without]). %HK
pseudoneg_phrase_tokens(with,    [or,without]). %HK

% In https://code.google.com/p/negex/wiki/NegExTerms,
% these are "Termination terms".

%%% conj_phrase_tokens(although, []).
%%% conj_phrase_tokens(apart, [from]).
%%% conj_phrase_tokens(as, [a,cause,for]).
%%% conj_phrase_tokens(as, [a,cause,of]).
%%% conj_phrase_tokens(as, [a,etiology,for]).
%%% conj_phrase_tokens(as, [a,etiology,of]).
%%% conj_phrase_tokens(as, [a,reason,for]).
%%% conj_phrase_tokens(as, [a,reason,of]).
%%% conj_phrase_tokens(as, [a,secondary,cause,for]).
%%% conj_phrase_tokens(as, [a,secondary,cause,of]).
%%% conj_phrase_tokens(as, [a,secondary,etiology,for]).
%%% conj_phrase_tokens(as, [a,secondary,etiology,of]).
%%% conj_phrase_tokens(as, [a,secondary,origin,for]).
%%% conj_phrase_tokens(as, [a,secondary,origin,of]).
%%% conj_phrase_tokens(as, [a,secondary,reason,for]).
%%% conj_phrase_tokens(as, [a,secondary,reason,of]).
%%% conj_phrase_tokens(as, [a,secondary,source,for]).
%%% conj_phrase_tokens(as, [a,secondary,source,of]).
%%% conj_phrase_tokens(as, [a,source,for]).
%%% conj_phrase_tokens(as, [a,source,of]).
%%% conj_phrase_tokens(as, [a,cause,for]).
%%% conj_phrase_tokens(as, [a,cause,of]).
%%% conj_phrase_tokens(as, [an,etiology,for]).
%%% conj_phrase_tokens(as, [an,etiology,of]).
%%% conj_phrase_tokens(as, [an,origin,for]).
%%% conj_phrase_tokens(as, [an,origin,of]).
%%% conj_phrase_tokens(as, [a,reason,for]).
%%% conj_phrase_tokens(as, [a,reason,of]).
%%% conj_phrase_tokens(as, [a,secondary,cause,for]).
%%% conj_phrase_tokens(as, [a,secondary,cause,of]).
%%% conj_phrase_tokens(as, [a,secondary,etiology,for]).
%%% conj_phrase_tokens(as, [a,secondary,etiology,of]).
%%% conj_phrase_tokens(as, [a,secondary,origin,for]).
%%% conj_phrase_tokens(as, [a,secondary,origin,of]).
%%% conj_phrase_tokens(as, [a,secondary,reason,for]).
%%% conj_phrase_tokens(as, [a,secondary,reason,of]).
%%% conj_phrase_tokens(as, [a,secondary,source,for]).
%%% conj_phrase_tokens(as, [a,secondary,source,of]).
%%% conj_phrase_tokens(as, [a,source,for]).
%%% conj_phrase_tokens(as, [a,source,of]).
%%% conj_phrase_tokens(as, [the,cause,for]).
%%% conj_phrase_tokens(as, [the,cause,of]).
%%% conj_phrase_tokens(as, [the,etiology,for]).
%%% conj_phrase_tokens(as, [the,etiology,of]).
%%% conj_phrase_tokens(as, [the,origin,for]).
%%% conj_phrase_tokens(as, [the,origin,of]).
%%% conj_phrase_tokens(as, [the,reason,for]).
%%% conj_phrase_tokens(as, [the,reason,of]).
%%% conj_phrase_tokens(as, [the,secondary,cause,for]).
%%% conj_phrase_tokens(as, [the,secondary,cause,of]).
%%% conj_phrase_tokens(as, [the,secondary,etiology,for]).
%%% conj_phrase_tokens(as, [the,secondary,etiology,of]).
%%% conj_phrase_tokens(as, [the,secondary,origin,for]).
%%% conj_phrase_tokens(as, [the,secondary,origin,of]).
%%% conj_phrase_tokens(as, [the,secondary,reason,for]).
%%% conj_phrase_tokens(as, [the,secondary,reason,of]).
%%% conj_phrase_tokens(as, [the,secondary,source,for]).
%%% conj_phrase_tokens(as, [the,secondary,source,of]).
%%% conj_phrase_tokens(as, [the,source,for]).
%%% conj_phrase_tokens(as, [the,source,of]).
%%% conj_phrase_tokens(aside, [from]).
%%% conj_phrase_tokens(but, []).
%%% conj_phrase_tokens(cause, [for]).
%%% conj_phrase_tokens(cause, [of]).
%%% conj_phrase_tokens(causes, [for]).
%%% conj_phrase_tokens(causes, [of]).
%%% conj_phrase_tokens(etiology, [for]).
%%% conj_phrase_tokens(etiology, [of]).
%%% conj_phrase_tokens(except, []).
%%% conj_phrase_tokens(however, []).
%%% conj_phrase_tokens(nevertheless, []).
%%% conj_phrase_tokens(origin, [for]).
%%% conj_phrase_tokens(origin, [of]).
%%% conj_phrase_tokens(origins, [for]).
%%% conj_phrase_tokens(origins, [of]).
%%% conj_phrase_tokens(other, [possibilities,of]).
%%% conj_phrase_tokens(reason, [for]).
%%% conj_phrase_tokens(reason, [of]).
%%% conj_phrase_tokens(reasons, [for]).
%%% conj_phrase_tokens(reasons, [of]).
%%% conj_phrase_tokens(secondary, [to]).
%%% conj_phrase_tokens(source, [for]).
%%% conj_phrase_tokens(source, [of]).
%%% conj_phrase_tokens(sources, [for]).
%%% conj_phrase_tokens(sources, [of]).
%%% conj_phrase_tokens(still, []).
%%% conj_phrase_tokens(though, []).
%%% conj_phrase_tokens(trigger, [event,for]).
%%% conj_phrase_tokens(yet, []).

%%% % The following conj_phrase_tokens were added by NLM
%%% conj_phrase_tokens(other,     [than]).
%%% conj_phrase_tokens(otherwise, []).
%%% %conj_phrase_tokens(then,      []).   %?
%%% conj_phrase_tokens(to,        [account,for]).
%%% conj_phrase_tokens(to,        [explain]).
%%% conj_phrase_tokens(non,       [traumatic]).
conj_phrase_tokens(_A, _B) :- !, fail.

% Negated UMLS concepts must below to one of the following semantic types
% this is essentially a specially defined semantic group that is a super set of "Disorders"
% negex_semtype_list([acab,anab,biof,cgab,comd,dsyn,emod,fndg,
%		      inpo,lbtr,menp,mobd,neop,patf,phsf,sosy]).

% fin

% :- use_module(library(addportray)).
% portray_mm_output(mm_output(_ExpandedUtterance,_CitationTextAtom,_ModifiedText,_Tagging,_AAs,
% 			    _Syntax,_DisambiguatedMMOPhrases,_ExtractedPhrases)) :- write('MMO').
% :- add_portray(portray_mm_output).
