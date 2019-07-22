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
:- module(semrules_DOM,	[
	word_corresponds_to_semnet_relation_DOM/4,	multiphrase_corresponds_to_semnet_relation_DOM/6,	phrase_corresponds_to_semnet_relation_DOM/6
]).

multiphrase_corresponds_to_semnet_relation_DOM(_, _, _, _, _, _) :- !, fail.

phrase_corresponds_to_semnet_relation_DOM(_, _, _, _, _, _) :- !, fail.

% ----- Source Code Control System
%
%  Set of current semantic types, many not in UMLS Semantic Network
%  Needs to be manually updated
%  word_corresponds_to_semnet_relation_DOM( ?Word, ?POS, ?Cue, ?Relation )
%

%word_corresponds_to_semnet_relation_DOM(aim,[to,address],verb). % check with Halil if OK
word_corresponds_to_semnet_relation_DOM(address,verb,_,addresses).
word_corresponds_to_semnet_relation_DOM(combat,verb,_,addresses).
word_corresponds_to_semnet_relation_DOM(designed,adj,for,addresses).
word_corresponds_to_semnet_relation_DOM(direct,verb,at,addresses).
word_corresponds_to_semnet_relation_DOM(direct,verb,to,addresses).
word_corresponds_to_semnet_relation_DOM(directed,adj,at,addresses).
word_corresponds_to_semnet_relation_DOM(directed,adj,to,addresses).
word_corresponds_to_semnet_relation_DOM(focus,verb,on,addresses).
word_corresponds_to_semnet_relation_DOM(focused,adj,on,addresses).
word_corresponds_to_semnet_relation_DOM(eliminate,verb,_,addresses). % June 2011
word_corresponds_to_semnet_relation_DOM(erradicate,verb,_,addresses). % June 2011
%word_corresponds_to_semnet_relation_DOM(for,prep,_,addresses).
word_corresponds_to_semnet_relation_DOM(mod_head,_,_,addressed_by).
word_corresponds_to_semnet_relation_DOM(impinge,verb,_,affects). % June 2011
word_corresponds_to_semnet_relation_DOM(implication,noun,for,affects).
word_corresponds_to_semnet_relation_DOM(analyze,verb,_,analyzes). % June 2011
word_corresponds_to_semnet_relation_DOM(analysis,noun,_,analyzes). % June 2011
word_corresponds_to_semnet_relation_DOM(insight,noun,_,analyzes). % June 2011
word_corresponds_to_semnet_relation_DOM(effects,noun,_,assesses_effect_of).
word_corresponds_to_semnet_relation_DOM(impact,noun,_,assesses_effect_of).
word_corresponds_to_semnet_relation_DOM(results,noun,_,assesses_effect_of).
word_corresponds_to_semnet_relation_DOM(among,prep,_,attribute_of).
word_corresponds_to_semnet_relation_DOM(become,verb,_,attribute_of).
word_corresponds_to_semnet_relation_DOM(mod_head,_,_,characteristic_of).
word_corresponds_to_semnet_relation_DOM(collaborate,verb,with,coordinates_with).
word_corresponds_to_semnet_relation_DOM(collaboration,noun,with,coordinates_with).
word_corresponds_to_semnet_relation_DOM(conjunction,noun,with,coordinates_with).
word_corresponds_to_semnet_relation_DOM(coordinate,verb,with,coordinates_with).
word_corresponds_to_semnet_relation_DOM(coordination,noun,with,coordinates_with).
word_corresponds_to_semnet_relation_DOM(partner,verb,with,coordinates_with).
word_corresponds_to_semnet_relation_DOM(partnership,noun,with,coordinates_with).
word_corresponds_to_semnet_relation_DOM(work,verb,with,coordinates_with).
word_corresponds_to_semnet_relation_DOM(assess,verb,_,evaluates).
word_corresponds_to_semnet_relation_DOM(assess,verb,through,evaluated_by).
word_corresponds_to_semnet_relation_DOM(assessment,noun,of,evaluates).
word_corresponds_to_semnet_relation_DOM(determine,verb,_,evaluates).
word_corresponds_to_semnet_relation_DOM(evaluate,verb,_,evaluates).
word_corresponds_to_semnet_relation_DOM(evaluation,noun,of,evaluates).
word_corresponds_to_semnet_relation_DOM(measure,verb,_,evaluates).
word_corresponds_to_semnet_relation_DOM(measured,verb,_,evaluates).
word_corresponds_to_semnet_relation_DOM(measurement,noun,_,evaluates).
word_corresponds_to_semnet_relation_DOM(access,verb,_,has_access_to).
word_corresponds_to_semnet_relation_DOM(access,noun,to,has_access_to).
word_corresponds_to_semnet_relation_DOM(obtain,verb,_,has_access_to).
word_corresponds_to_semnet_relation_DOM(associated,pastpart,with,impediment_to).
word_corresponds_to_semnet_relation_DOM(barrier,noun,_,impediment_to).
word_corresponds_to_semnet_relation_DOM(hinder,verb,_,impediment_to).
word_corresponds_to_semnet_relation_DOM(impediment,noun,to,impediment_to).
word_corresponds_to_semnet_relation_DOM(measure,verb,_,measures).
word_corresponds_to_semnet_relation_DOM(measured,verb,_,measures).
word_corresponds_to_semnet_relation_DOM(measurement,noun,_,measures).
word_corresponds_to_semnet_relation_DOM(through,prep,_,has_method). % June 2011
word_corresponds_to_semnet_relation_DOM(monitor,verb,_,monitors).
word_corresponds_to_semnet_relation_DOM(monitoring,noun,of-by,monitored_by).
word_corresponds_to_semnet_relation_DOM(oversee,verb,_,monitors).
word_corresponds_to_semnet_relation_DOM(survey,verb,_,monitors).
word_corresponds_to_semnet_relation_DOM(surveillance,noun,for,monitors).
word_corresponds_to_semnet_relation_DOM(surveillance,noun,of,monitors).
word_corresponds_to_semnet_relation_DOM(encourage,verb,_,promotes).
word_corresponds_to_semnet_relation_DOM(enhance,verb,_,promotes). % June 2011
word_corresponds_to_semnet_relation_DOM(facilitate,verb,_,promotes).
word_corresponds_to_semnet_relation_DOM(foster,verb,_,promotes).
word_corresponds_to_semnet_relation_DOM(promote,verb,_,promotes).
word_corresponds_to_semnet_relation_DOM(promotion,noun,_,promotes).
word_corresponds_to_semnet_relation_DOM(reinforce,verb,_,promotes). % June 2011
word_corresponds_to_semnet_relation_DOM(support,verb,_,promotes).
word_corresponds_to_semnet_relation_DOM(vehicle,noun,for,promotes).
word_corresponds_to_semnet_relation_DOM(deliver,verb,_,provides).
word_corresponds_to_semnet_relation_DOM(give,verb,_,provides).
word_corresponds_to_semnet_relation_DOM(implement,verb,_,provides).
word_corresponds_to_semnet_relation_DOM(launch,verb,_,provides).
word_corresponds_to_semnet_relation_DOM(offer,verb,_,provides).
word_corresponds_to_semnet_relation_DOM(provide,verb,_,provides).
word_corresponds_to_semnet_relation_DOM([risk,factor],noun,for, predisposes). % June 2011
word_corresponds_to_semnet_relation_DOM(assist,verb,_,serves).
word_corresponds_to_semnet_relation_DOM(care,verb,for,serves). % June 2011
word_corresponds_to_semnet_relation_DOM(serve,verb,_,serves).
word_corresponds_to_semnet_relation_DOM(tend,verb,to,serves).
word_corresponds_to_semnet_relation_DOM(based,adj,_,setting_for).
word_corresponds_to_semnet_relation_DOM(mod_head,_,_,setting_for).
word_corresponds_to_semnet_relation_DOM(address,verb,_,targets).
word_corresponds_to_semnet_relation_DOM(aim,verb,at,targets).
word_corresponds_to_semnet_relation_DOM(aim,verb,to,targets).
word_corresponds_to_semnet_relation_DOM(designed,adj,for,targets).
word_corresponds_to_semnet_relation_DOM(direct,verb,at,targets).
word_corresponds_to_semnet_relation_DOM(direct,verb,to,targets).
word_corresponds_to_semnet_relation_DOM(directed,adj,at,targets).
word_corresponds_to_semnet_relation_DOM(directed,adj,to,targets).
word_corresponds_to_semnet_relation_DOM(emphasis,noun,on,targets).
word_corresponds_to_semnet_relation_DOM(emphasize,verb,_,targets).
word_corresponds_to_semnet_relation_DOM(focus,verb,on,targets).
word_corresponds_to_semnet_relation_DOM(for,prep,_,targets).
word_corresponds_to_semnet_relation_DOM(in,prep,_,targets). % June 2011
word_corresponds_to_semnet_relation_DOM(reach,verb,_,targets).
word_corresponds_to_semnet_relation_DOM(on,prep,_,targets). % June 2011
word_corresponds_to_semnet_relation_DOM(tailor,verb,to,targets).
word_corresponds_to_semnet_relation_DOM(target,verb,_,targets).
word_corresponds_to_semnet_relation_DOM(mod_head,_,_,targeted_by).

% stubs

multiphrase_corresponds_to_semnet_relation_GEN(_, _, _, _, _, _) :- fail.

phrase_corresponds_to_semnet_relation_GEN(_, _, _, _, _, _) :- fail.

