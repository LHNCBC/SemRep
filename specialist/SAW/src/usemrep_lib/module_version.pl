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

:- module( module_version, [
	add_year_to_filename/2,
	global_module_version/1,
	set_global_module_version/1,
	use_versioned_module/3
   ]).

:- use_module( skr_lib(sicstus_utils), [concat_atom/2]).

:- dynamic global_module_version/1.

% Instead of writing, e.g.,
% 
% :- use_module( skr_lib( tagger_access03 ), [stop_tagger_access/0]).
% 
% and having to modify all these lines every time we migrate to a new version,
% say, 03 to 04, we could use the code below and write
% 
% :- use_versioned_module( skr_lib( tagger_access ), usemrep, [stop_tagger_access/0]).

% The arity-2 version of use_versioned_module is intented to eventually replace
% the arity-3 version...if I ever manage to implement the logic to determine
% what file called module_version.pl (which would have to be done via the message facility).

% :- use_umls_version_module( skr_lib( tagger_access ), [stop_tagger_access/0]).

% use_umls_version_module/2 is intended for files requiring a version number,
% but it can be used for any file, e.g.,
% :- use_umls_version_module( library( basics ), [memberchk/2]).
% is fine, and this will simply fall through to the second clause
% which will call the built-in use_module.

% Example:
% :- use_versioned_module( skr_lib(db_access),      [ get_concept_cui/2 ]).

%% use_versioned_module(Module, ModulePredicates) :-
%% 	% If the file has no skr_lib() or usemrep_main() or other wrapper,
%% 	% just load that file, period.
%% 	atom(Module),
%% 	!,
%% 	use_module(Module, ModulePredicates).
%% use_versioned_module(Module, ModulePredicates) :-
%% 	% If the file does have a skr_lib() or usemrep_main() or other wrapper,
%% 	% first determine the filename that is the argument of the wrapper.
%% 	functor(Module, Library, 1),
%% 	arg(1, Module, FileName),
%% 	% Next determine if that file is one requiring a version number.
%% 	file_requiring_version(FileName),
%% 	!,
%% 	% Retrieve the version number, e.g., '04'.
%% 	global_module_version(Version),
%% 	% Assemble, e.g., 'db_access04' by concatenating 'db_access' and '04'
%% 	concat_atom([FileName, Version], FileNameWithYear),
%% 	% create the term with its wrapper, e.g., usemrep_main('db_access04')
%% 	functor(ModuleWithYear, Library, 1),
%% 	arg(1, ModuleWithYear, FileNameWithYear),
%% 	% Call use module on the module with its yearname.
%% 	use_module(ModuleWithYear, ModulePredicates).
%% 
%% % Default clause, in case use_versioned_module/2 is called on a module
%% % not requiring a version number.
%% use_versioned_module(Module, ModulePredicates) :-
%% 	use_module(Module, ModulePredicates).
%% 

%use_versioned_module(Module, _Importer, ModulePredicates) :-
%	% If the file has no skr_lib() or usemrep_main() or other wrapper,
%	% just load that file, period.
%	atom(Module),
%	!,
%	use_module(Module, ModulePredicates).

%use_versioned_module(Module, Importer, ModulePredicates) :-
%	format(user_output,'IN USER VERSIONED~n',[]),
%	flush_output(user_output),
%	% If the file does have a skr_lib() or usemrep_main() or other wrapper,
%	% first determine the filename that is the argument of the wrapper.
%	functor(Module, Library, 1),
%	format(user_output,'AFTER FUNCTOR.~n',[]),
%	flush_output(user_output),
%	arg(1, Module, FileName),
%	format(user_output,'AFTER ARG.',[]),
%		flush_output(user_output),
%	% Next determine if that file is one requiring a version number.
%	file_requiring_version(FileName),
%		format(user_output,'AFTER REQUIRE~n',[]),
%		flush_output(user_output),
%	!,
%	format(user_output,'File requiring version ~a ~a~n',[FileName,Importer]),
%	flush_output(user_output),
%	add_year_to_filename(FileName, FileNameWithYear),
%	format(user_output,'FileNameWithYear ~a~n',[FileNameWithYear]),
%		flush_output(user_output),
%	% create the term with its wrapper, e.g., usemrep_main('db_access04')
%	functor(ModuleWithYear, Library, 1),
%	arg(1, ModuleWithYear, FileNameWithYear),
%	format(user_output,'ModuleWithYear ~w imported by ~a~n',[ModuleWithYear,Importer]),
%		flush_output(user_output),
%	% Call use module on the module with its yearname.
%	Importer:use_module(ModuleWithYear, ModulePredicates).
%% Default clause, in case use_versioned_module/2 is called on a module
%% not requiring a version number.
%use_versioned_module(Module, Importer, ModulePredicates) :-
%	Importer:use_module(Module, ModulePredicates).

% These are the files requiring a version number, e.g., db_access04
%file_requiring_version(semnet_access).
%file_requiring_version(semtype_translation).

% This is the default version number
%global_module_version('06').
%global_module_version('_2012AA').

set_global_module_version(Version) :- 
	( global_module_version(_) ->
	  retract(global_module_version(_))
	; true
	)
	,
	assert(global_module_version(Version)).
%%	global_module_version(X),
%%	format('Global module versiona ~a ~n', [X]).

%add_year_to_filename(FileName, FileNameWithYear) :-
%	% Retrieve the version number, e.g., '05'.
%	format(user_output,'ADD YEAR ~n',[]),
%	flush_output(user_output),
%	global_module_version(Version),
%	format(user_output,'Global module version ~a~n',[Version]),
%	flush_output(user_output),	
%	% Assemble, e.g., 'db_access04' by concatenating 'db_access' and '04'
%	concat_atom([FileName, Version], FileNameWithYear),
%	format('FileName: ~a~n',FileNameWithYear).
