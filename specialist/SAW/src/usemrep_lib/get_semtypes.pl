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

% File:	    get_semtypes.pl
% Module:   get_semtypes
% Author:   tcr
% Purpose:  Get UMLS semantic types and convert them to Semantic Groups


% ----- Module declaration and exported predicates

:- module( get_semtypes, [
	abbreviate_umls_semtypes/2,
	get_semtypes/3,
	get_umls_semtypes/3,
	look_up_semantic_groups/3
   ]).

:- load_files( usemrep_lib(module_version), [
	when(compile_time)
   ]).

% ----- Imported predicates

:- use_module( usemrep_domain(domain_processing), [
	domain_specific_semgroup_member/3
   ]).

:- use_module( usemrep_domain(domain_data), [
	domain_semtype/3
   ]).

%:- use_module( usemrep_lib(generic_domain), [
%	domain_semtype/3
%   ]).

:- use_module( usemrep_lib(exceptions_GEN), [
	concept_to_ignore_GEN/2
   ]).

:- use_module( usemrep_domain(exceptions_DOM), [
	concept_to_ignore_DOM/2
   ]).

:- use_module( library( lists ), [
	delete/3,
	is_list/1
   ]).

:- use_module( library( ordsets ), [
	list_to_ord_set/2
   ]).

:- use_module( skr_lib(nls_lists), [
		get_from_list/3
   ]).

:- use_module( skr_lib(nls_system), [
		control_option/1,
		control_value/2
   ]).

:- use_module(usemrep_lib(module_version), [
		global_module_version/1
	]).

%:- use_versioned_module( usemrep_lib( semtype_translation ),
%	get_semtypes, [
%	semtype_translation/2
%   ]).

:- use_module( usemrep_lib( semtype_translation ), [
	semtype_translation/3
   ]).

%:- use_module( usemrep_lib( semtype_translation_2012AA ), [
%	semtype_translation/2
%   ]).

%:- use_module( usemrep_lib( semtype_translation06 ), [
%	semtype_translation/2
%   ]).


% ******************************** GET SEMTYPES *****************************


get_semtypes(minimal_syntax(NPUnitsWithMetaConcs), 
             minimal_syntax(NPUnitsWithSemtypes),
	     Domain) :-
	get_semtypes(NPUnitsWithMetaConcs, NPUnitsWithSemtypes, Domain).

get_semtypes([], [], _Domain).
get_semtypes([NPU|MoreNPUs], 
             [NPUWithSemGroups|MoreNPUWithSemtype],
	     Domain) :-
	combine_multiple_meta_mappings(NPU, NPUWithSemGroups, Domain),
	get_semtypes(MoreNPUs, MoreNPUWithSemtype, Domain).


/*
 ----- COMBINE MULTIPLE META MAPPINGS

 In the event of multiple Meta mappings, the first mapping is the best.  
 Multiple mappings are due to ambiguous Meta concepts (such as "Surgery <1>", 
 and "surgery <2>"), or they arise from ambiguous matches between the input text 
 and Meta concepts, as with input *vena cava stent filter*.

 Multiple mappings due to ambiguous Meta concepts are contiguous in the list of
 mappings.

 For now the best mapping is considered to be the only mapping, except that the
 semantic types from ambiguous Meta concepts (of the "surgery" type) are added to
 the best, first mapping.

 For example, if an MSU looks like this

 [ [ mod(X),
     mod(Y),
     head([
           bases([pathology]),
           lexmatch([pathology]),
           inputmatch([pathology]),
           tag(noun),
           tokens([pathology]),
           metaconc(['Pathological aspects':[ftcn]])
   ],
   [ mod(X),
     mod(Y),
     head([
           bases([pathology]),
           lexmatch([pathology]),
           inputmatch([pathology]),
           tag(noun),
           tokens([pathology]),
           metaconc(['Pathology:[bmod]])
    ]
]

  combine_mappings/3 will merge the two metaconc terms as follows:

   [ mod(X),
     mod(Y),
     head([
           bases([pathology]),
           lexmatch([pathology]),
           inputmatch([pathology]),
           tag(noun),
           tokens([pathology]),
           metaconc(['Pathological aspects':[ftcn],'Pathology:[bmod]])
    ]
*/

combine_multiple_meta_mappings([FirstMapping|MoreMappings], NPUWithSemGroups, Domain) :-
	combine_mappings(MoreMappings, FirstMapping, FinalMapping),
	convert_to_semantic_groups(FinalMapping, NPUWithSemGroups, Domain).

% ----- COMBINE MAPPINGS

combine_mappings([], FirstMapping, FirstMapping).
combine_mappings([NextMapping|RestMappings], FirstMapping, FinalMapping) :-
	combine_npu_components(FirstMapping, NextMapping, AugmentedFirstMapping),
	!,
	combine_mappings(RestMappings, AugmentedFirstMapping, FinalMapping).
combine_mappings(_, FirstMapping, FirstMapping).

% --- COMBINE NPU COMPONENTS

% An NPU component is something like mod(_), or head(_)
% If components are identical, continue
% If components are not identical, but differ only in Meta concept, add the concept from 
%    AnotherComponent to FirstComponent
% Otherwise stop comparing mappings


combine_npu_components([], [], []).
combine_npu_components([FirstComponent|MoreFirst],
		       [FirstComponent|MoreAnother],
		       [FirstComponent|MoreComponents]) :-
	!,
	combine_npu_components(MoreFirst, MoreAnother, MoreComponents).
combine_npu_components([FirstComponent|MoreFirst],
		       [AnotherComponent|MoreAnother],
		       [NewComponent|MoreComponents]) :-
	functor(FirstComponent, Label, _),
	functor(AnotherComponent, Label, _),
	arg(1, FirstComponent, FirstElementList),
	arg(1, AnotherComponent, AnotherElementList),
	get_from_list(bases, FirstElementList,   BasesList),
	get_from_list(bases, AnotherElementList, BasesList),
	!,
	combine_component_elements(FirstElementList, AnotherElementList, BasesList, NewElementList),
	functor(NewComponent, Label, 1),
	arg(1, NewComponent, NewElementList),
	combine_npu_components(MoreFirst, MoreAnother, MoreComponents).


% --- COMBINE COMPONENT ELEMENTS


combine_component_elements([], [], _Token, []).
combine_component_elements([Element|MoreFirst],
			   [Element|MoreAnother],
			   BasesList,
			   [Element|MoreElements]) :-
	!,
	combine_component_elements(MoreFirst, MoreAnother, BasesList, MoreElements).
combine_component_elements([metaconc(FirstMetaConc)|MoreFirst],
			   [metaconc(AnotherMetaConc)|MoreAnother],
			   BasesList,
			   [metaconc(NewMetaConc)|MoreElements]) :-
	combine_metaconcs(FirstMetaConc, AnotherMetaConc, BasesList, NewMetaConc),
	combine_component_elements(MoreFirst, MoreAnother, BasesList, MoreElements).


% combine the two MetaConcs as desribed above,
% but throw away any metaconcs defined as concept_to_ignore (in exceptions.pl).
combine_metaconcs(MC1, MC2, BasesList, NewMetaConc) :-
	MC1 = [FirstMetaConc:_FirstCUI:_FirstSemTypeList|_Rest],
	MC2 = [AnotherMetaConc:_AnotherCUI:_AnotherSemTypeList],
	( concept_to_ignore_BOTH(BasesList, FirstMetaConc) ->
	  NewMetaConc = MC2
	; concept_to_ignore_BOTH(BasesList, AnotherMetaConc) ->
	  NewMetaConc = MC1
	; append(MC1, MC2, TempNewMetaConc),
	  list_to_ord_set(TempNewMetaConc, NewMetaConc)
	).

% ----- Convert to Semantic Groups

convert_to_semantic_groups([], [], _Domain).
convert_to_semantic_groups([NPUComponent|More],
			   [NewNPUComponent|NPUWithSemGroups],
			   Domain) :-
	functor(NPUComponent, Label, _Arity),
	% mod_head_pastpart_verb_label(Label),
	arg(1, NPUComponent, NPUComponentArg),
	get_from_list(metaconc, NPUComponentArg, MetaConcList),
	is_list(MetaConcList),
	!,
	convert_one_NPU_to_semantic_groups(MetaConcList, NPUComponentArg,
				       Domain, Label, NewNPUComponent),
	convert_to_semantic_groups(More, NPUWithSemGroups, Domain).


convert_to_semantic_groups([NoMetaConc|More],
			   [NoMetaConc|NPUWithSemGroups], Domain) :-
	convert_to_semantic_groups(More, NPUWithSemGroups, Domain).

convert_one_NPU_to_semantic_groups(MetaConcList, NPUComponentArg,
				   Domain, Label, NewNPUComponent) :-
	( get_from_list(bases, NPUComponentArg, BasesList),
	  MetaConcList = [MetaConc:_CUI:_SemTypeList],
	  concept_to_ignore_BOTH(BasesList, MetaConc) ->
	  delete(NPUComponentArg, metaconc(MetaConcList), NewNPUComponentArg)
	; % get full UMLS semtypes
	  get_umls_semtypes(MetaConcList, AUSemtypeList, []),
	  % get abbreviated versions
	  abbreviate_umls_semtypes(AUSemtypeList, UMLSSemtypeList),
	  look_up_semantic_groups(AUSemtypeList, SemGroupList, Domain),
	  NewNPUComponentArg = [usemtype(UMLSSemtypeList),
				ausemtype(AUSemtypeList),
				semgroup(SemGroupList)|NPUComponentArg]
	),
	functor(NewNPUComponent, Label, 1),
	arg(1, NewNPUComponent, NewNPUComponentArg).


% mod_head_pastpart_verb_label(mod).
% mod_head_pastpart_verb_label(head).
% mod_head_pastpart_verb_label(pastpart).
% mod_head_pastpart_verb_label(verb).

% ----- GET_UMLS_SEMTYPES -----

get_umls_semtypes([], AllUSemtypes, AllUSemtypes).
get_umls_semtypes([_CUI:_MetaConcAtom:USemtypeList|MoreMetaConcs], AllUSemtypes, Accum) :-
	append(Accum, USemtypeList, TempAccum),
	get_umls_semtypes(MoreMetaConcs, AllUSemtypes, TempAccum).

% ----- LOOK_UP_SEMANTIC_GROUPS

look_up_semantic_groups(AUSemtypeList, SemGroupSet, Domain) :-
	look_up_semantic_groups_1(AUSemtypeList, SemGroupList, Domain),
	list_to_ord_set(SemGroupList, SemGroupSet).

look_up_semantic_groups_1([], [], _Domain).
look_up_semantic_groups_1([AUSemtype|RestAUSemtypes],
			  [SemanticGroup|RestSemanticGroups],
			  Domain) :-
	domain_specific_semgroup_member(Domain, AUSemtype, SemanticGroup),
	!,
	look_up_semantic_groups_1(RestAUSemtypes, RestSemanticGroups, Domain).
% Not sure this is necessary
look_up_semantic_groups_1([AUSemtype|RestAUSemtypes],
                          [SemanticGroup|RestSemanticGroups],
                          Domain) :-
        ( control_option(use_generic_domain_extension)
        ; control_option(use_generic_domain_modification)
	),
    	domain_specific_semgroup_member(Domain, AUSemtype, SemanticGroup),
%	generic_domain:domain_semtype(AUSemtype,_,SemanticGroup),
        !,
        look_up_semantic_groups_1(RestAUSemtypes, RestSemanticGroups, Domain).
look_up_semantic_groups_1([AUSemtype|RestAUSemtypes],
                          [SemanticGroup|RestSemanticGroups],
                          Domain) :-
	control_value(domain, _),
        domain_data:domain_semtype(AUSemtype,_,SemanticGroup),
        !,
        look_up_semantic_groups_1(RestAUSemtypes, RestSemanticGroups, Domain).

look_up_semantic_groups_1([_Other|RestAUSemtypes], SemanticGroups, Domain) :-
	look_up_semantic_groups_1(RestAUSemtypes, SemanticGroups, Domain).

% ----- ABBREVIATE UMLS SEMTYPES -----

abbreviate_umls_semtypes([], []).
abbreviate_umls_semtypes([AUSemtype|MoreAUSemtypes], [UMLSSemtype|MoreUMLSSemtypes]) :-
	global_module_version(Version),
	semtype_translation(Version,UMLSSemtype, AUSemtype),
	!,
	abbreviate_umls_semtypes(MoreAUSemtypes, MoreUMLSSemtypes).
% Is this really necessary?
abbreviate_umls_semtypes([AUSemtype|MoreAUSemtypes], [UMLSSemtype|MoreUMLSSemtypes]) :-
        ( control_option(use_generic_domain_extension)
        ; control_option(use_generic_domain_modification)
	),
        global_module_version(Version),
	%        generic_domain:domain_semtype(AUSemtype, UMLSSemtype, _),
	semtype_translation(Version,UMLSSemtype, AUSemtype),
	!,
        abbreviate_umls_semtypes(MoreAUSemtypes,MoreUMLSSemtypes).
abbreviate_umls_semtypes([AUSemtype|MoreAUSemtypes], [UMLSSemtype|MoreUMLSSemtypes]) :-
	control_value(domain,_Domain),
        domain_data:domain_semtype(AUSemtype, UMLSSemtype, _),
        abbreviate_umls_semtypes(MoreAUSemtypes,MoreUMLSSemtypes).

%%% DOMAIN-CHOICE LOGIC begins here
% There are three files that exist in both GENeric and DOMain flavors:
% exceptions, locsemnet, and semrules.

% (1) The logic for locsemnet and semrules is:
%  * If running DOMain SemRep, call the DOM version first,
%    and if that fails, call the GEN version.
%  * If running GENeric SemRep, call the GEN version only.

% (2) The logic for exceptions is
%  * If running DOMain SemRep,  call the DOM version only.
%  * If running GENeric SemRep, call the GEN version only.

% concept_to_ignore is part of exceptions, so the second set of rules applies.
concept_to_ignore_BOTH(BasesList, MetaConc) :-
	( control_value(domain, _Domain) ->
	  concept_to_ignore_DOM(BasesList, MetaConc)
	; concept_to_ignore_GEN(BasesList, MetaConc)
	).
