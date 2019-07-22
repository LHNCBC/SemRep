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

% File:	    domain_processing.pl
% Module:   domain
% Author:   tcr, NLS, Usemrep  modified by FML to isolate domain-specific processing

% ----- Module declaration and exported predicates

:- module(domain_processing, [
	choose_domain/2,
	domain_specific_initialize_metamap/1,
	domain_specific_semgroup_member/3,
	domain_specific_unset_control_options/1,
	perform_domain_processing/8
   ]).

:- use_module( skr_lib(semgroup_member), [
	semgen_semgroup_member/2,
	semrep_semgroup_member/2
   ]).

%:- use_module( usemrep_domain(domain_semgroup_member), [
%	domain_semgroup_member/2
%   ]).

:- use_module( usemrep_lib(get_semtypes), [
        abbreviate_umls_semtypes/2,
%	get_semtypes/3,
	get_umls_semtypes/3,
	look_up_semantic_groups/3	
   ]).

:- use_module( usemrep_lib(ssuppserv), [
        remove_referential_features/2,
        remove_referential_features_from_element/2
   ]).

%:- use_module( usemrep_vocabulary(domain_vocabulary), [
%	domain_vocabulary_term/5
%   ]).

%:- use_module( usemrep_domain(choose_domains), [
%	anatomy_domain/1,
%	clinical_domain/1
%  ]).

:- use_module( skr( skr ), [
        initialize_skr/1
%	metamap_phrases/5
   ]).


:- use_module( skr_lib( nls_system ), [
%	control_option/1,
        subtract_from_control_options/1
%	add_to_control_options/1
   ]).

%:- use_module( library( lists ), [
%	rev/2
%   ]).

:- use_module( library( sets ), [
	intersect/2
   ]).


choose_domain(genetics, genetics).
choose_domain(_ProcessingMode,generic).

%perform_domain_processing(InputLabel, InputText, CitationTextAtom,
%	                  Domain, _AnatomyDomain, _SyntacticAnalysis,
% 			  minimal_syntax(MetaConcAnalysis1), 
%			  minimal_syntax(MetaConcAnalysis2)) :-
%        clinical_domain(Domain),
%	perform_clinical_domain_processing(InputLabel, InputText, CitationTextAtom,
%	                                   Domain, '', 	                                    
%					   MetaConcAnalysis1, MetaConcAnalysis2).
perform_domain_processing(_InputLabel, _InputText, _CitationTextAtom,
	                  genetics, _AnatomyDomain, _SyntacticAnalysis,
			  minimal_syntax(MetaConcAnalysis1), minimal_syntax(MetaConcAnalysis2)) :-
	filter_domain(genetics, '', MetaConcAnalysis1, MetaConcAnalysis2).
perform_domain_processing(_InputLabel, _InputText, _CitationTextAtom, 
	                  _Domain, _AnatomyDomain, _SyntacticAnalysis, 
			  minimal_syntax(MetaConcAnalysis1), minimal_syntax(MetaConcAnalysis1)).


%perform_clinical_domain_processing(InputLabel, InputText, CitationTextAtom, 
%	                           PrincipalDomain, AnatomyDomain,
%	                           MetaConcAnalysis1, MetaConcAnalysis3) :-
%	( PrincipalDomain == [] ->  % no secondary anatomy domain
%	  MetaConcAnalysis1 = MetaConcAnalysis3
%	; re_metamap_anatomy_concept(InputLabel, InputText, CitationTextAtom,
%	                             MetaConcAnalysis1, MetaConcAnalysis2),
%	  filter_domain(PrincipalDomain, AnatomyDomain, MetaConcAnalysis2, MetaConcAnalysis3)
%	).

%re_metamap_anatomy_concept(InputLabel, InputText, CitationTextAtom,
%	                   minimal_syntax(MetaMapList), 
%	                   minimal_syntax(MetaMapListWithAnat)) :-
%	re_metamap_anatomy_concept(InputLabel, InputText, CitationTextAtom,
%	                          MetaMapList, MetaMapListWithAnat).

%re_metamap_anatomy_concept(_InputLabel, _InputText, _CitationTextAtom,
%	                   [], []).
%re_metamap_anatomy_concept(InputLabel, InputText, CitationTextAtom,
%	                   [MSUAmbiguityList|More],
 %                          [MSUAmbiguityListWithSemTypesOut|Gap]) :-
%	check_for_anat(MSUAmbiguityList),
%	!,
%	metamap_again(InputLabel, InputText, CitationTextAtom,
%	              MSUAmbiguityList, MSUAmbiguityListWithAnat),
%	get_semtypes([MSUAmbiguityListWithAnat], MSUAmbiguityListWithSemTypes, _Domain),
%	MSUAmbiguityListWithSemTypes = [MSUAmbiguityListWithSemTypesOut],
%	re_metamap_anatomy_concept(InputLabel, InputText, CitationTextAtom, More, Gap).
%re_metamap_anatomy_concept(InputLabel, InputText, CitationTextAtom, 
%	                   [NoAnatomy|More], [NoAnatomy|Gap]) :-
%	re_metamap_anatomy_concept(InputLabel, InputText, CitationTextAtom, More, Gap).

%check_for_anat(MSUInstance) :-
%	rev(MSUInstance, RevMSUInstance),
%	remove_punc_and_confid(RevMSUInstance, RevMSUInstanceRemove),
%	( RevMSUInstanceRemove = [head(ItemList)|_]
%        ; RevMSUInstanceRemove = [mod(ItemList)|_]
%	),
%	check_metaconc(ItemList),
%	!.
	
%remove_punc_and_confid([], []).
%remove_punc_and_confid([punc(_)|More], Gap) :-
%	!,
%	remove_punc_and_confid(More, Gap).
%remove_punc_and_confid([confid(_)|More],Gap) :-
%	!,
%	remove_punc_and_confid(More, Gap).
%remove_punc_and_confid([Other|More], [Other|Gap]) :-
%	!,
%	remove_punc_and_confid(More,Gap).

%check_metaconc([metaconc(MetaConcList)|_More]) :-
%	anat_semtype(MetaConcList),
%	!.
%check_metaconc([metaconc(['Bulb':'C0556573':[plnt]])|_More]) :- !.
%check_metaconc([_NoAnat|More]) :-
%	check_metaconc(More).

%anat_semtype([_Concept:_CUI:SemtypeList|_More]) :-
%	intersect(SemtypeList, [bpoc,blor,bsoj,bdsu,tisu,anst,cgab]),
%	!.
%anat_semtype([_NoAnat|More]) :-
%	anat_semtype(More).

%metamap_again(_InputLabel, _InputText, _CitationTextAtom,
%	     MSUAmbiguityList, MSUAmbiguityListWithAnat) :-
%	remove_referential_features_from_element(MSUAmbiguityList, _Phrase),
%	subtract_from_control_options([prefer_multiple_concepts]),
%	add_to_control_options([term_processing,allow_concept_gaps]),
%%	metamap_phrases(InputText, InputLabel, CitationTextAtom,
%%	                minimal_syntax([Phrase]), minimal_syntax([MSUAmbiguityListWithAnat])),
%	MSUAmbiguityListWithAnat = [],
%        subtract_from_control_options([term_processing,allow_concept_gaps]).

% -----

filter_domain(_PrincipalDomain, _AnatomyDomain, [], []):- !.
filter_domain(PrincipalDomain, AnatomyDomain, 
	      [MSUAmbiguityList|More], [MSUAmbiguityListReplaced|Gap]) :-
	filter_domain_aux(PrincipalDomain, AnatomyDomain, 
	                  MSUAmbiguityList, MSUAmbiguityListReplaced),
	!,
	filter_domain(PrincipalDomain, AnatomyDomain, More, Gap).
filter_domain(PrincipalDomain, AnatomyDomain, [NoReplacement|More], [NoReplacement|Gap]) :-
	filter_domain(PrincipalDomain, AnatomyDomain, More, Gap).

% -----

filter_domain_aux(_PrincipalDomain, _AnatomyDomain, [], []):- !.
filter_domain_aux(PrincipalDomain, AnatomyDomain,
	          [MSUInstance|More], [MSUInstanceReplaced|Gap]) :-
	functor(MSUInstance, Label, 1),
% verb should probably not be here. Verbs should not be metamapped, in general.
% This is temporary.
% Not sure this is even necessary. Why does it matter what the type is?
	memberchk(Label, [mod,head,prep,pastpart,shapes,verb]),
	!,
	arg(1, MSUInstance, ArgList),
	cleanup_MSU_instance(PrincipalDomain, AnatomyDomain, ArgList, NewArgList),
	functor(MSUInstanceReplaced, Label, 1),
	arg(1, MSUInstanceReplaced, NewArgList),
	filter_domain_aux(PrincipalDomain, AnatomyDomain, More, Gap).
filter_domain_aux(PrincipalDomain, AnatomyDomain,
	          [MSUInstance|More], [MSUInstance|Gap]) :-
	filter_domain_aux(PrincipalDomain, AnatomyDomain, More, Gap).

cleanup_MSU_instance(genetics, AnatomyDomain, ArgList, NewArgList) :-
	!,
	( memberchk(metaconc(_MetaConcList), ArgList) ->
	  memberchk(semgroup(SemGroupList), ArgList),
% chem brings stuff like phsu. I am not sure we want those -- Halil
%	  ( intersect(SemGroupList, [chem,diso,gene]),
	  ( intersect(SemGroupList, [diso,gene]),	  
	    cleanup(genetics, AnatomyDomain, ArgList, NewArgList)
	  ; remove_referential_features(ArgList, NewArgList)
          )
        ; ArgList = NewArgList
        ).	
%cleanup_MSU_instance(PrincipalDomain, AnatomyDomain, ArgList, NewArgList) :-
%	clinical_domain(PrincipalDomain),
%	!,
%	cleanup(PrincipalDomain, AnatomyDomain, ArgList, NewArgList).
cleanup_MSU_instance(_PrincipalDomain, _AnatomyDomain, ArgList, ArgList).

cleanup(PrincipalDomain, AnatomyDomain, ArgList, NewArgList) :-
	memberchk(metaconc(MetaConcList), ArgList),
	find_metaconcs_to_keep(PrincipalDomain, AnatomyDomain, MetaConcList, NewMetaConcList),
        ( NewMetaConcList == [] ->
	  remove_referential_features(ArgList, NewArgList)
        ; get_umls_semtypes(NewMetaConcList, AUSemtypeList, []),
	  abbreviate_umls_semtypes(AUSemtypeList, USemtypeList),
	  look_up_semantic_groups(AUSemtypeList, SemGroupSet, PrincipalDomain),
	  filter(ArgList, NewMetaConcList, AUSemtypeList, 
	         USemtypeList, SemGroupSet, NewArgList)
	),
	!.
cleanup(_PrincipalDomain, _AnatomyDomain, ArgList, ArgList).

find_metaconcs_to_keep(_PrincipalDomain, _AnatomyDomain, [], []).
find_metaconcs_to_keep(genetics, AnatomyDomain, [Concept:_CUI:STList|More], 
	               [Concept:_CUI:STList|Rest]) :-
	member(SemType, STList), 
	semgen_semgroup_member(SemType, SemGroup),
%	memberchk(SemGroup, [chem,diso,gene]),
	memberchk(SemGroup, [diso,gene]),
	!,
	find_metaconcs_to_keep(genetics, AnatomyDomain, More, Rest).
%find_metaconcs_to_keep(PrincipalDomain, AnatomyDomain, [Concept:_CUI:STList|More], 
%	               [Concept:_CUI:STList|Rest]) :-
%	concept_in_domain(PrincipalDomain, AnatomyDomain, Concept, STList),
%	!,
%	find_metaconcs_to_keep(PrincipalDomain, AnatomyDomain, More, Rest).
find_metaconcs_to_keep(PrincipalDomain, AnatomyDomain, [_NotInDomain|More], Rest) :-
	find_metaconcs_to_keep(PrincipalDomain, AnatomyDomain, More, Rest).

filter([], _MetaConcList, _AUSemtypeList, _USemtypeList, _SemGroupSet, []).
filter([metaconc(_)|More], MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet, 
       [metaconc(MetaConcList)|Gap]) :-
	!,
	filter(More, MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet, Gap).
filter([ausemtype(_)|More], MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet, 
       [ausemtype(AUSemtypeList)|Gap]) :-
	!,
	filter(More, MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet, Gap).
filter([usemtype(_)|More], MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet,
       [usemtype(USemtypeList)|Gap]) :-
	!,
	filter(More, MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet, Gap).
filter([semgroup(_)|More], MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet,
       [semgroup(SemGroupSet)|Gap]) :-
	!,
	filter(More, MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet, Gap).
filter([Other|More], MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet,
       [Other|Gap]) :-
	filter(More, MetaConcList, AUSemtypeList, USemtypeList, SemGroupSet, Gap).
	
	
%% Rule #1: A concept is in the domain if it is a generic concept.
%% This has not yet been implemented.
%concept_in_domain(_PrincipalDomain, _AnatomyDomain, Concept, STList) :-
%	generic_concept(Concept, STList),
%	!.

%% Rule #2: A concept is in the domain if the Principal Domain is an anatomy domain and
%% the concept is in the vocabulary of the Principal Domain.

%% Yuck...I hate putting *two* cuts in one small predicate, even if it's the right thing to do,
%% but the parents should not be cut for the sake of their wayward children.
%concept_in_domain(PrincipalDomain, AnatomyDomain, Concept, STList) :-
%	anatomy_domain(PrincipalDomain),
%	!,
%	member(SemType, STList),
%	domain_semgroup_member(SemType, SemGroup),
%	domain_vocabulary_term(SemGroup, PrincipalDomain, AnatomyDomain, Concept, STList),
%	!.
%% Rule #3: A concept is in the domain if the Principal Domain is NOT an anatomy domain and
%% the concept is in the vocabulary of EITHER the Principal Domain OR the secondary anatomy domain.
%concept_in_domain(PrincipalDomain, AnatomyDomain, Concept, STList) :-
%	member(SemType, STList),
%	domain_semgroup_member(SemType, SemGroup),
%	( domain_vocabulary_term(SemGroup, PrincipalDomain, AnatomyDomain, Concept, STList) ->
%	  true
%	; domain_vocabulary_term(SemGroup, AnatomyDomain, [], Concept, STList)
%        ),
%	!.

%% needs to be elaborated
%generic_concept(_Concept, _STList) :- fail.

domain_specific_unset_control_options(Domain) :-
	select_metamap_options(Domain, Options),
%	unset_control_options(Options).
	subtract_from_control_options(Options).

domain_specific_initialize_metamap(Domain) :-
	select_metamap_options(Domain, Options),
%	initialize_metamap(Options).
	initialize_skr(Options).

select_metamap_options(Domain, Options) :-
	( domain_specific_options(Domain, Options) ->
	  true
	; default_metamap_options(Options)
	).
%domain_specific_options(generic,           [strict_model, show_cuis]).
domain_specific_options(genetics, 	   [no_derivational_variants]).
%domain_specific_options(cardiology,        []).
%domain_specific_options(gastroenterology,  []).
%domain_specific_options(nephrology,        []).
%domain_specific_options(oncology,          []).
%domain_specific_options(pulmonary,         []).

% default_metamap_options([strict_model, show_cuis]).
default_metamap_options([]).

domain_specific_semgroup_member(Domain, AUSemtype, SemanticGroup) :-
        ( Domain == genetics ->
          semgen_semgroup_member(AUSemtype, SemanticGroup)
        ; semrep_semgroup_member(AUSemtype, SemanticGroup) % used for clinical domains as well
        ).


/* 
% ************************* Filter For Domain *************************

Uses Susanne Humphrey's JD Indexing to determine domain
Molecular genetics domain is determined by the following rule.
In the list of JD'S BASED ON WORD OCCURRENCES, the following occur
in the top 15 ranked JD's:
'Genetics, Medical' AND ('Genetics, Biochemical' OR 'Biochemistry'
                          OR 'Molecular Biology' OR 'Cytogenetics')

% filter_for_domain(WordOccurrenceList, Domain) :-
% 	get_jds(WordOccurrenceList, JDList),
% 	memberchk('Genetics, Medical', JDList),
% 	intersect(JDList,
%               ['Genetics, Biochemical', 'Biochemistry',
%                 'Molecular Biology','Cytogenetics']).
    
Output from jdi/2 is the following for both WordOccurrenceList and _CitationCountList:
[['Genetics, Medical',3.593366965651512E-02],['Cytogenetics',1.641530357301235E-02]|More]

Currently loosed filter to apply to molecular biology generally
*/



