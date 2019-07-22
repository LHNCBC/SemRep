
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

% File:	    skr_umls_info.pl
% Module:   SKR
% Author:   Lan
% Purpose:  This module defines year-specific UMLS information such as
%           source codes. See also semtype_translationXX for semantic types.


:- module(skr_umls_info, [
	convert_to_root_sources/2,
	verify_sources/1,
	verify_sts/1,
	sab_tables_exist/0			  
    ]).

:- use_module(skr_db(db_access), [
	db_get_root_source_name/2,
	db_get_versioned_source_name/2,
	get_data_model/1,
	get_data_version/1,
	get_data_release/2,
	model_location/5
    ]).

:- use_module(library(file_systems), [
	file_exists/1,
	file_exists/2
   ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2,
	send_message/2,
	set_message/5
    ]).

:-   absolute_file_name(skr_lib(semtype_translation_2017AA),
			AbsFileName,
			[extensions(['.pl'])]),
     file_exists(AbsFileName) ->
     use_module(skr_lib(semtype_translation_2017AA),
		[expand_semtypes/2,
		 is_abbrev_semtype/1]),
     format(user_error, 'File skr_umls_info.pl is loading ~w~n', [AbsFileName])
   ; format(user_error, 'File skr_umls_info.pl is NOT loading~n', []).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	upper/2
    ]).


/* verify_sources(+Sources)
   verify_sources_aux(+Sources, -UnknownSources, RemovedSources)

verify_sources/1 checks to make sure all elements in the Sources list
are valid sources. It displays any that are not and then fails.
*/

verify_sources(Sources) :-
	verify_sources_aux(Sources, RemovedSources, UnknownSources),
	announce_removed_and_unknown_sources(RemovedSources, UnknownSources).

announce_removed_and_unknown_sources(RemovedSources, UnknownSources) :-
	announce_removed_sources(RemovedSources),
	announce_unknown_sources(UnknownSources).

announce_removed_sources(RemovedSources) :-
	( RemovedSources == [] ->
	  true
	; get_data_version(Version),
	  get_data_model(Model),
	  get_data_release(Release, 0),
	  set_message(RemovedSources, PluralIndicator, Verb, _Pronoun, _Determiner),
	  send_message('~n### WARNING: The UMLS source~w ~p ', [PluralIndicator,RemovedSources]),
	  send_message('~w not represented in the ~w ~w ~w data.~n', [Verb,Model,Release,Version])
	).

announce_unknown_sources(UnknownSources) :-
	( UnknownSources == [] ->
	  true
	; set_message(UnknownSources, PluralIndicator, Verb, _Pronoun, _Determiner),
	  fatal_error('The UMLS source~w ~p ~w unknown. Aborting.~n',
		      [PluralIndicator,UnknownSources,Verb])
	).

verify_sources_aux([], [], []).
verify_sources_aux([FirstSource|RestSources], RemovedSources, UnknownSources) :-
	is_umls_source(FirstSource, ZeroOrOne),
	!,
	maybe_add_to_removed_sources(ZeroOrOne, FirstSource, RemovedSources, RestRemovedSources),
	verify_sources_aux(RestSources, RestRemovedSources, UnknownSources).
verify_sources_aux([FirstSource|RestSources], RemovedSources, [FirstSource|RestUnknownSources]) :-
	verify_sources_aux(RestSources, RemovedSources, RestUnknownSources).

maybe_add_to_removed_sources(0,  Source, [Source|RestRemovedSources], RestRemovedSources).
maybe_add_to_removed_sources(1, _Source, RestRemovedSources, RestRemovedSources).


/* The UMLS sources are now stored in BDB tables.
   umls_source(?Source)
   umls_root_source(?RootSource)
   umls_versioned_source(?VersionedSource)
   convert_to_root_sources(+Sources, -RootSources)

umls_versioned_source/1 define either the root or versioned source names,
and umls_source/1 defines either root or versioned source names.
convert_to_root_sources/2 converts a list of Sources (either versioned or
root) to a list of RootSources, dropping any elements of Sources that are
neither. */

is_umls_source(Source, ZeroOrOne) :-	
	( is_umls_root_source(Source, ZeroOrOne) ->
	  true
	; is_umls_versioned_source(Source, ZeroOrOne)
	).

sab_tables_exist :-
	get_data_model(Model),
	get_data_version(Version),
	get_data_release(Year, 0),
	model_location(Version, Year, Model, _BasePath, Location),
	concat_atom([Location, '/', sab_rv], SAB_RV_TABLE),
	concat_atom([Location, '/', sab_vr], SAB_VR_TABLE),
	file_exists(SAB_RV_TABLE, read),
	file_exists(SAB_VR_TABLE, read).	

is_umls_root_source(RootSource, ZeroOrOne) :-
	( sab_tables_exist ->
	  db_get_versioned_source_name_with_UC(RootSource, Results),
	  member([_VersionedSource,ZeroOrOne], Results)
	; ZeroOrOne is 1
	).

is_umls_versioned_source(VersionedSource, ZeroOrOne) :-
	( sab_tables_exist ->
	  db_get_root_source_name_with_UC(VersionedSource, [_RootSource,ZeroOrOne])
	; ZeroOrOne is 1
	).

% Versioned-to-root is one-to-one
convert_versioned_source_to_root(VersionedSource, RootSource, ZeroOrOne) :-
	db_get_root_source_name_with_UC(VersionedSource, [RootSource,ZeroOrOne]).


db_get_root_source_name_with_UC(VersionedSource, Results) :-
	( db_get_root_source_name(VersionedSource, Results),
	  Results \== [] ->
	  true
	; upper(VersionedSource, VersionedSourceUC),
	  db_get_root_source_name(VersionedSourceUC, Results)
	).

db_get_versioned_source_name_with_UC(RootSource, Results) :-
	( db_get_versioned_source_name(RootSource, Results),
	  Results \== [] ->
	  true
	; upper(RootSource, RootSourceUC),
	  db_get_versioned_source_name(RootSourceUC, Results)
	).

% Root-to-versioned is one-to-many, e.g.,
% CPT --> CPT2005
% CPT --> CPT2011
% LNC --> LNC215
% LNC --> LNC234
% ICD9CM --> ICD9CM_1998
% ICD9CM --> ICD9CM_2005
% ICD9CM --> ICD9CM_2011

convert_to_root_sources([], []).
convert_to_root_sources([RootSource|Rest], [RootSource|ConvertedRest]) :-
	is_umls_root_source(RootSource, _ZeroOrOne),
	!,
	convert_to_root_sources(Rest, ConvertedRest).
convert_to_root_sources([VersionedSource|Rest], [RootSource|ConvertedRest]) :-
	convert_versioned_source_to_root(VersionedSource, RootSource, _ZeroOrOne),
	!,
	convert_to_root_sources(Rest, ConvertedRest).
convert_to_root_sources([_NotASource|Rest], ConvertedRest) :-
	convert_to_root_sources(Rest, ConvertedRest).

/* verify_sts(+STs)
   find_illegal_sts(+STs, -IllegalSTs)

verify_sts/1 checks to make sure all elements in the STs list
are valid semantic types. It displays any that are not and then fails.
find_illegal_sts/2 uses the factual predicate is_abbrev_semtype/1 to find
the IllegalSTs in STs. */

verify_sts(STs) :-
	find_illegal_semtypes(STs, IllegalSTs),
	( IllegalSTs == [] ->
	  true
	; set_message(IllegalSTs, PluralIndicator, Verb, _Pronoun, _Determiner),
	  fatal_error('The semantic type~w ~p ~w unknown~n',
		      [PluralIndicator,IllegalSTs,Verb])
	).

find_illegal_semtypes([], []).
find_illegal_semtypes([FirstSemType|RestSemTypes], IllegalSemTypes) :-
	is_abbrev_semtype(FirstSemType),
	!,
	find_illegal_semtypes(RestSemTypes, IllegalSemTypes).
find_illegal_semtypes([FirstSemType|RestSemTypes], [FirstSemType|RestIllegalSemTypes]) :-
	find_illegal_semtypes(RestSemTypes, RestIllegalSemTypes).
