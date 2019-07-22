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

% File:	    db_access.pl
% Module:   DB Access
% Author:   Lan
% Purpose:  Provide access to the NLS DBs (Berkeley DB version)


:- module(db_access, [
	all_digits/1,
	db_get_all_acros_abbrs/2,
	db_get_concept_cui/2,
	% db_get_concept_sts/2,
	db_get_cui_sourceinfo/2,
        db_get_cui_sources_and_semtypes/3,
	% db_get_cui_sts/2,
	% db_get_cui_semtypes/2,
	db_get_mesh_mh/2,
	db_get_mesh_tc_relaxed/2,
	db_get_meta_mesh/2,
	db_get_mwi_word_count/3,
	db_get_mwi_word_data/4,
	% db_get_string_sources/2,
	db_get_synonyms/2,
	db_get_synonyms_with_cat/3,
	db_get_unique_acros_abbrs/2,
	db_get_root_source_name/2,
	db_get_variants/3,
	db_get_versioned_source_name/2,
	double_quotes/2,
	default_release/1,
	get_data_model/1,
	get_data_version/1,
	get_data_release/2,
	initialize_db_access/0,
	initialize_db_access/3,
	model_location/5,
	stop_db_access/0,
	%%%%%%%%%%% lexical access predicates
	db_get_lex_base_forms/2,
	db_get_lex_base_forms_with_cat/3,
	db_get_lex_cats/2,
	db_get_lex_prefix_EUIs/2,
	db_get_lex_record/2,
	db_get_lex_record_list/2,
	db_get_lex_dm_variants_no_cat/2,
	db_get_lex_dm_variants_with_cat/3,
	db_get_lex_im_varlist/2,
	exec_init_dbs/1,
	form_simple_query/5,
	run_query/3		      
   ]).

:- use_module(metamap(metamap_tokenization), [
	local_digit/1
    ]).

:- use_module(metamap(metamap_variants), [
	variant_score/2
    ]).

:- use_module(skr(skr_json), [
	json_output_format/1
    ]).

:- use_module(skr(skr_utilities),[
	debug_call/2,
	debug_message/3,
	ensure_atom/2,
	fatal_error/2,
	send_message/2
    ]).

:- use_module(skr(skr_xml), [
	xml_output_format/1
    ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

% :- use_module(skr_lib(ctypes), [
% 	is_digit/1
%     ]).

:- use_module(skr_lib(nls_strings),[
	atom_codes_list/2,
	concatenate_items_to_atom/2,
	concatenate_items_to_string/2,
	% eliminate_multiple_meaning_designator_string/2,
	is_print_string/1,
	split_atom_completely/3,
	split_string_completely/3,
	trim_whitespace/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atom/2,
	lower/2,
	ttyflush/0
    ]).

:- use_module(library(codesio), [
	read_from_codes/2
    ]).

:- use_module(library(file_systems), [
	directory_exists/1,
	directory_exists/2,
	directory_members_of_directory/3,
	file_exists/2
   ]).

:- use_module(library(sets),[
	del_element/3
    ]).

:- use_module(library(system),[
	environ/2
    ]).

:- use_module(library(lists),[
	append/2
    ]).

:- dynamic db_access_status/3.
:- dynamic db_access_var_table/1.

% foreign_resource(c_nls_db, [
foreign_resource(db_access, [
	c_nls_db_exec_2_list_jgm,
	exec_init_dbs,
	exec_destroy_dbs
   ]).

foreign(c_nls_db_exec_2_list_jgm, c, c_nls_db_exec_2_list_jgm(+string,-term,[-integer])).

foreign(exec_init_dbs, c, exec_init_dbs(+string)).

foreign(exec_destroy_dbs, c, exec_destroy_dbs).

% :- load_foreign_resource('../db_access').
:- environ('DYNAMIC_LIB_DIR',DynamicLibDir),
   atom_concat(DynamicLibDir,'/db_access',DbAccessSo),
   load_foreign_resource(DbAccessSo).

% :- abolish(foreign_resource/2, [force(true)]).

% :- abolish(foreign/3, [force(true)]).

verify_valid_dbs(Location, BasePath, ChosenRelease) :-
	( \+ directory_exists(Location) ->
	  announce_valid_models(BasePath, ChosenRelease),
	  fatal_error('Database directory ~q does not exist.~n', [Location])
	;  \+ directory_exists(Location, [read]) ->
	  fatal_error('Database directory ~q exists but is not readable.~n', [Location])
	;  \+ directory_exists(Location, [execute]) ->
	  fatal_error('Database directory ~q exists but is not executable.~n', [Location])
	; exec_init_dbs(Location)
	).

announce_valid_models(BasePath, ChosenRelease) :-
	concat_atom(['DB.*.', ChosenRelease, '.base'], Glob),
	% Suppose ChosenRelease is, e.g, 2010AA;
	% that's the value selected via the "-V" flag, or the default release.
	% Generate the list of all subdirectories of BasePath
	% (which is the 'MODEL_LOCATION_BASE_DIR' env var (e.g., /nfsvol/nls3aux18/DB))
	% that match the glob 'DB.*.base'.
	% For the 2011AA release, that will be ['Base', 'USAbase', and 'NLM'].
	directory_members_of_directory(BasePath, Glob, RelAbsList),
	% directory_members_of_directory/3 returns a list of
	% RelativePath-AbsolutePath terms, so remove the AbsolutePath terms.
	( foreach(Relative-_Absolute, RelAbsList),
	  foreach(Relative, RelList)
	do true
	),
	% RelativePath is of the form DB.Version.Release.Model,
	% so extract the Version component
	( foreach(Dir, RelList),
	  foreach(Release, Versions)
	do
	  atom_codes(Dir, DirCodes),
	  split_string_completely(DirCodes, ".", [_DB,ReleaseCodes,_Version,_Model]),
	  atom_codes(Release, ReleaseCodes)
	),
	announce_valid_models_1(Versions, ChosenRelease).

announce_valid_models_1([], ChosenRelease) :-
	fatal_error('There is no ~w release!~n', [ChosenRelease]).	
announce_valid_models_1([H|T], ChosenRelease) :-	
	fatal_error('For the ~w release, the valid models are: ~w~n', [ChosenRelease, [H|T]]).


/* initialize_db_access
   initialize_db_access(+Version, +Release, +Model)
   stop_db_access

initialize_db_access/0 calls verify_valid_dbs to validate the BDB directory
for the specified version and data model.

The default version is "normal".

Current models for each version are: relaxed or strict.
stop_db_access/0 calls exec_destroy_dbs to close them.  */

% default_version('USAbase').
default_version(DefaultVersion) :-
	get_data_release(Release, 0),
	atom_codes(Release, [A,B,C,D,_E,_F]),
	number_codes(FourDigitRelease, [A,B,C,D]),
	( FourDigitRelease >= 2011 ->
	  DefaultVersion = 'USAbase'
	; DefaultVersion = 'NLM'
	).

default_release('2017AA').

initialize_db_access :-
	get_data_release(Release, 1),
	get_data_version(Version),
	get_data_model(Model),
	initialize_db_access(Version, Release, Model).

initialize_db_access(Version, Release, Model) :-
	db_access_status(Version, Release, Model),
	!.
initialize_db_access(Version, Release, Model) :-
	set_var_table(VarTable),
	% Version is one of 'Base', 'USAbase', 'NLM', or 'Full'
	% Model   is one of strict (default), relaxed
	model_location(Version, Release, Model, BasePath, Location),
	verify_valid_dbs(Location, BasePath, Release),
	assert(db_access_status(Version, Release, Model)),
	conditionally_announce_database_info(Version, Release, Model, Location, VarTable),
	!.
initialize_db_access(Version, Release, Model) :-
	fatal_error('Cannot open Berkeley DB databases (~a ~a ~a model).~n',
		    [Version,Release,Model]).

conditionally_announce_database_info(Version, Release, Model, Location, VarTable) :-
	( \+ control_option(silent) ->
	   send_message('Berkeley DB databases (~a ~a ~a model) are open.~n',
		  [Version, Release, Model]),
	  send_message('Static variants will come from table ~s in ~w.~n',[VarTable,Location]),
	  announce_variant_info
	; true
	).

announce_variant_info :-
	variant_generation_mode(Mode),
	send_message('Derivational Variants: ~w.~n',[Mode]).

variant_generation_mode(Mode) :-
	( control_option(no_derivational_variants) ->
	  Mode = 'NONE'
	; control_option(all_derivational_variants) ->
	  Mode = 'ALL'
	; Mode = 'Adj/noun ONLY'
	).


set_var_table(VarTable) :-
	retractall(db_access_var_table(_)),
	( control_option(unique_acros_abbrs_only) ->
	    ( control_option(all_derivational_variants) ->
	      VarTable="varsu"
	    ; VarTable="varsanu"
	    )
        ;   ( control_option(all_derivational_variants) ->
	      VarTable="vars"
	    ; VarTable="varsan"
	    )
	),
	assert(db_access_var_table(VarTable)).

stop_db_access :-
	retractall(db_access_status(_,_,_)),
	exec_destroy_dbs.

model_location(Version, Release, ModelName, Path, Location) :-
	model_location_base_dir(Path),
	concat_atom([Path, '/DB.', Version, '.', Release, '.', ModelName],  Location).

model_location_base_dir(Path) :- environ('MODEL_LOCATION_BASE_DIR', Path).

run_query(Query, Results, Return) :-
	c_nls_db_exec_2_list_jgm(Query, Results, Return),
	debug_call(db, length(Results, Length)),
	debug_message(db, '~N### query "~w" returned ~d result(s)~n', [Query, Length]).

/* db_get_concept_cui(+Concept, -CUI)

db_get_concept_cui/2 gets the CUI for Input which is assumed to be a concept
name.  */

db_get_concept_cui(Concept, CUIAtom) :-
	( Concept == [] ->
	  CUIAtom = []
	; ensure_string(Concept, ConceptString),
	  db_get_concept_cui_aux(ConceptString, Results),
	  get_cui_from_results(Results, CUIAtom)
	),
	!.
db_get_concept_cui(Concept, []) :-
	fatal_error('db_get_concept_cui failed for ~w~n', [Concept]).

db_get_concept_cui_aux(ConceptAtom, CUI) :-
	form_simple_query("cui", "conceptcui", "concept", ConceptAtom, Query),
	run_query(Query, CUIs0, 1),
	append(CUIs0, CUI).

get_cui_from_results([],      'C0000000').
get_cui_from_results([CUI|_], CUI).

db_get_versioned_source_name(RootSourceName, VersionedSourceName) :-
	ensure_string(RootSourceName, RootSourceNameString),
	db_get_versioned_source_name_aux(RootSourceNameString, VersionedSourceName),
	!.
db_get_versioned_source_name(RootSourceName, []) :-
	fatal_error('db_get_versioned_source_name failed for ~w~n', [RootSourceName]).

db_get_versioned_source_name_aux(RootSourceName, VersionedSourceNames) :-
	form_simple_query("versioned, exists", "sab_rv", "root", RootSourceName, Query),
	run_query(Query, VersionedSourceNames, 1).

db_get_root_source_name(VersionedSourceName, RootSourceName) :-
	ensure_string(VersionedSourceName, VersionedSourceNameString),
	db_get_root_source_name_aux(VersionedSourceNameString, RootSourceName),
	!.
db_get_root_source_name(VersionedSourceName, []) :-
	fatal_error('db_get_root_source_name failed for ~w~n', [VersionedSourceName]).

db_get_root_source_name_aux(VersionedSourceName, RootSourceNames) :-
	form_simple_query("root, exists", "sab_vr", "versioned", VersionedSourceName, Query),
	run_query(Query, RootSourceNames0, 1),
	RootSourceNames0 = [RootSourceNames].

/* db_get_cui_sourceinfo(+CUI, -Sourceinfo)

db_get_cui_sourceinfo/2 gets Sourceinfo for CUI, where Sourceinfo is a list of
lists [I,STR,SAB,TTY] where I is the ith entry (the order being
determined by mrconso.eng, STR is the string for source SAB with term type
TTY). Warning, CUI can be a string, but returned elements are either atoms
or numbers, not strings. */

% There is no need to call this predicate unless one of
% -G --sources
% -R --restrict_to_sources <list>
% -e --exclude_sources <list>
% -q --machine_output
% --XMLf --XMLf1 --XMLn --XMLn1
% is set

db_get_cui_sourceinfo(CUI, SourceInfo) :-
	( CUI == [] ->
	  SourceInfo = []
	; ensure_string(CUI, CUIString),
	  db_get_cui_sourceinfo_aux(CUIString, SourceInfo)
	),
	!.
db_get_cui_sourceinfo(CUI, []) :-
	fatal_error('db_get_cui_sourceinfo failed for ~p~n', [CUI]).

db_get_cui_sourceinfo_aux(CUIAtom, SourceInfo) :-
	form_simple_query("i, str, src, tty", "cuisourceinfo", "cui", CUIAtom, Query),
	run_query(Query, SourceInfo0, 1),
 	sort(SourceInfo0, SourceInfo).

%%% /* db_get_concept_sts(+Concept, -SemTypes)
%%% 
%%% db_get_concept_sts/2 gets the (abbreviated) semantic types, SemTypes, for Input.  */
%%% 
%%% db_get_concept_sts(Concept, SemTypes) :-
%%% 	( Concept == [] ->
%%% 	  SemTypes = []
%%% 	; ensure_atom(Concept, ConceptAtom),
%%% 	  db_get_concept_sts_aux(ConceptAtom, SemTypes)
%%% 	),
%%% 	!.
%%% db_get_concept_sts(Concept, []) :-
%%% 	fatal_error('db_get_concept_sts failed for ~w~n', [Concept]),
%%% 	abort.
%%% 
%%% db_get_concept_sts_aux(ConceptAtom, SemTypes) :-
%%% 	form_simple_query("st", "conceptst", "concept", ConceptAtom, Query),
%%% 	run_query(Query, SemTypes0, 1),
%%% 	append(SemTypes0, SemTypes).

/* db_get_cui_sts(+CUI, -SemTypes)

%%% db_get_cui_sts/2 gets the (abbreviated) semantic types, SemTypes, for Input.  */
%%% 
%%% db_get_cui_sts(CUI, SemTypes) :-
%%% 	( CUI == [] ->
%%% 	  SemTypes = []
%%% 	; ensure_atom(CUI, CUIAtom),
%%% 	  db_get_cui_sts_aux(CUIAtom, SemTypes)
%%% 	),
%%% 	!.
%%% db_get_cui_sts(CUI, []) :-
%%% 	fatal_error('db_get_cui_sts failed for ~w~n', [CUI]).
%%% 
%%% db_get_cui_sts_aux(CUIAtom, SemTypes) :-
%%% 	form_simple_query("st", "cuist", "concept", CUIAtom, Query),
%%% 	run_query(Query, SemTypes0, 1),
%%% 	append(SemTypes0, SemTypes).

/* db_get_all_acros_abbrs(+Word, -AAPairs)

db_get_all_acros_abbrs/2 gets the list of acronym/abbreviation pairs
of Input using db_get_all_acros_abbrs_aux/2.  The elements of AAPairs are 
of the form <AA>:<type> where <AA> is an acronym, abbreviation or expansion
and <type> is either 'a' (acronym/abbreviation) or 'e' (expansion).  */

db_get_all_acros_abbrs(Word, AAPairs) :-
	( Word == [] ->
	  AAPairs = []
	; lower(Word, LCWord),
	  ensure_atom(LCWord, LCWordAtom),
	  db_get_all_acros_abbrs_aux(LCWordAtom, AAPairs)
	),
	!.
db_get_all_acros_abbrs(Word, []) :-
	fatal_error('db_get_all_acros_abbrs failed for ~p~n', [Word]).

db_get_all_acros_abbrs_aux(Word, AAPairs) :-
	form_simple_query("expansion, type", "nlsaa", "word", Word, Query),
	run_query(Query, AAListPairs, 1),
	list_pairs_to_pairs(AAListPairs, AAPairs).

list_pairs_to_pairs([], []).
list_pairs_to_pairs([[L,R]|Rest], [L:R|RestPairs]) :-
	list_pairs_to_pairs(Rest, RestPairs).

/* db_get_unique_acros_abbrs(+Word, -AAPairs)

db_get_unique_acros_abbrs/2 gets the list of unique acronym/abbreviation pairs
of Input using db_get_unique_acros_abbrs_aux/2.  The elements of AAPairs are 
of the form <AA>:<type> where <AA> is an acronym, abbreviation or expansion
and <type> is either 'a' (acronym/abbreviation) or 'e' (expansion).  */

db_get_unique_acros_abbrs(Word, AAPairs) :-
	( Word == [] ->
	  AAPairs = []
	; lower(Word, LCWord),
	  ensure_atom(LCWord, LCWordAtom),
	  db_get_unique_acros_abbrs_aux(LCWordAtom, AAPairs)
	),
	!.
db_get_unique_acros_abbrs(Word, []) :-
	fatal_error('get_unique_acros_abbrs failed for ~p~n', [Word]).

% Result must be in a list for subsequent processing
% db_get_unique_acros_abbrs_aux(Word, [Expansion:Type]) :-
db_get_unique_acros_abbrs_aux(Word, AAPairs) :-
	form_simple_query("expansion, type", "nlsaau", "word", Word, Query),
	run_query(Query, AAListPairs, 1),
        list_pairs_to_pairs(AAListPairs, AAPairs).

/* db_get_synonyms(+Word, -Synonyms)
   db_get_synonyms(+Word, +Category, -Synonyms)

db_get_synonyms/2 gets the list of Synonyms which are either
Dorland or NLS synonyms of Input.  The elements of Synonyms are either atoms
or strings depending on Input.
db_get_synonyms/3 restricts results by Category. */

db_get_synonyms(Word, Synonyms) :-
	( Word == [] ->
	  Synonyms = []
	; ensure_atom(Word, WordAtom),
	  db_get_synonyms_aux(WordAtom,Synonyms0),
	  remove_input(Synonyms0, WordAtom, Synonyms)
	),
	!.
db_get_synonyms(Word, []) :-
	fatal_error('db_get_synonyms/2 failed for ~p.~n', [Word]).

db_get_synonyms_aux(WordAtom, Synonyms) :-
	form_simple_query("syn, scat", "syns", "word", WordAtom, Query),
	run_query(Query, Synonyms0, 1),
	normalize_synonyms(Synonyms0, Synonyms1),
	sort(Synonyms1, Synonyms).

remove_input([], _, []).
remove_input([Input-_|Rest], Input, ModifiedRest) :-
	!,
	remove_input(Rest, Input, ModifiedRest).
remove_input([First|Rest],Input, [First|ModifiedRest]) :-
	remove_input(Rest, Input, ModifiedRest).

db_get_synonyms_with_cat(Word, WordCategory, Synonyms) :-
	( Word == [] ->
	  Synonyms = []
        ; ensure_atom(Word, WordAtom),
	  ensure_atom(WordCategory, WordCategoryAtom),
	  db_get_synonyms_with_cat_aux(WordAtom, WordCategoryAtom, Synonyms0),
	  del_element(WordAtom-WordCategoryAtom, Synonyms0, Synonyms)
	),
	!.
db_get_synonyms_with_cat(Word, WordCategory, []) :-
	fatal_error('db_get_synonyms/3 failed for ~w/~w.~n', [Word, WordCategory]).

db_get_synonyms_with_cat_aux(WordAtom, WordCategoryAtom, Synonyms) :-
	form_complex_query("syn, scat", "syns", "word", WordAtom, "wcat", WordCategoryAtom, Query),
	run_query(Query, Synonyms0, 1),
	normalize_synonyms(Synonyms0, Synonyms1),
	sort(Synonyms1, Synonyms).

/* normalize_synonyms(+Synonyms, -NormalizedSynonyms)
   normalize_synonym(+Synonym, -NormalizedSynonym)

normalize_synonyms/2 normalizes Synonyms using normalize_synonym/2
by lowercasing, stripping off expressions of the form <n> and stripping spaces.  */

normalize_synonyms([], []).
normalize_synonyms([[Syn,Cat]|Rest], [NormalizedSyn-Cat|NormalizedRest]) :-
	normalize_synonym(Syn, NormalizedSyn),
	normalize_synonyms(Rest, NormalizedRest).

normalize_synonym(Synonym, NormalizedSynonym) :-
	lower(Synonym, LCSynonym),
	atom_codes(LCSynonym, CSString0),
	% atom_codes(LCSynonym, LCSynonymString),
	% eliminate_multiple_meaning_designator_string(LCSynonymString, CSString0),
	trim_whitespace(CSString0, NormalizedSynonymString),
	atom_codes(NormalizedSynonym, NormalizedSynonymString).

/* db_get_mesh_tc_relaxed(+MeSH, -TreeCodes)

db_get_mesh_tc_relaxed/2 gets the list of MeSH tree codes for Input.
The elements of TreeCodes are either atoms or strings depending on Input.
Only MeSH terminology WITH treecodes will be found; thus subheadings and
check tags will produce nothing.  */

db_get_mesh_tc_relaxed(MeSH, TreeCodes) :-
	( MeSH == [] ->
	  TreeCodes = []
	; ensure_atom(MeSH, MeSHAtom),
	  db_get_mesh_tc_relaxed_aux(MeSHAtom, TreeCodes0),
	  atom_codes_list(TreeCodes0, TreeCodes)
	),
	!.
db_get_mesh_tc_relaxed(MeSH, []) :-
	fatal_error('db_get_mesh_tc_relaxed failed for ~p.~n', [MeSH]).

db_get_mesh_tc_relaxed_aux(MeSHAtom, TreeCodes) :-
	form_simple_query("tc", "meshtcrelaxed", "mesh", MeSHAtom, Query),
	run_query(Query, TreeCodes0, 1),
	append(TreeCodes0, TreeCodes).

/* db_get_mesh_mh(+MeSH, -MH)

db_get_mesh_mh/2 gets the MeSH heading for Input, if any. All MeSH terms with a
heading should succeed. Everything else fails. */

db_get_mesh_mh(MeSH, MH) :-
	( MeSH == [] ->
	  MH = []
	; ensure_atom(MeSH, MeSHAtom),
	  db_get_mesh_mh_aux(MeSHAtom, MH)
	),
	!.
% db_get_mesh_mh(MeSH, []) :-
% 	fatal_error('db_get_mesh_mh failed for ~p.~n', [MeSH]).

db_get_mesh_mh_aux(MeSHAtom, MH) :-
	form_simple_query("mh", "meshmh", "mesh", MeSHAtom, Query),
	run_query(Query, [[Result]], 1),
	( Result == 'X' ->
	  MH = MeSHAtom
	; MH = Result
	).

/* db_get_meta_mesh(+MeSH, -MHString)

db_get_meta_mesh/2 gets the MeSH heading for MeSH. It fails if there is no MeSH
for MeSH. Only MeSH terminology with (pseudo)treecodes will succeed. Thus
subheadings will fail. */

db_get_meta_mesh(MeSH, MHString) :-
	( MeSH == [] ->
	  MHString = []
	; ensure_atom(MeSH, MeSHAtom),
	  db_get_meta_mesh_aux(MeSHAtom, MHAtom),
	  atom_codes(MHAtom, MHString)
	),
	!.
% db_get_meta_mesh(MeSH, []) :-
% 	fatal_error('db_get_meta_mesh failed for ~p.~n', [MeSH]).

db_get_meta_mesh_aux(MeSHAtom, MH) :-
	form_simple_query("mesh", "metamesh", "meta", MeSHAtom, Query),
	run_query(Query, [[Result]], 1),
	( Result == 'X' ->
	  MH = MeSHAtom
	; MH = Result
	).

/* db_get_mwi_word_data(+Table, +Word, +DebugFlags, -Results)

db_get_mwi_word_data/4 gets word data (Results) from Table for Word.
Results is a list of terms of the form
     usc(NMStr,String,Concept)
where NMStr is the normalized form of String.
*/

db_get_mwi_word_data(Table, Word, DebugFlags, Results) :-
	( Word == [] ->
	  Results = []
	; ensure_atom(Word, WordAtom),
	  ensure_atom(Table, TableAtomTemp),
	  possibly_widen_table(TableAtomTemp, TableAtom, Widen),
	  debug_db_get_mwi_data_1(DebugFlags),
	  db_get_mwi_word_data_aux(Widen, TableAtom, WordAtom, DebugFlags, RawResults),
	  debug_db_get_mwi_data_2(DebugFlags),
	  form_uscs(RawResults, 0, Results),
	  debug_db_get_mwi_data_3(DebugFlags, Results)
	
	  ),
	!.
db_get_mwi_word_data(Table, Word, _DebugFlags, []) :-
	fatal_error('db_get_mwi_word_data failed for word ~p on table ~p.~n', [Word,Table]).

possibly_widen_table(NarrowTable, WideTable, Widen) :-
	db_access_status(Version, Release, Model),
	model_location(Version, Release, Model, _BasePath, Location),
	concat_atom([NarrowTable, '_WIDE'], MaybeWideTable),
	concat_atom([Location, '/', MaybeWideTable], WideTablePath),
	( file_exists(WideTablePath, read) ->
	  WideTable = MaybeWideTable,
	  Widen is 1
	; WideTable = NarrowTable,
	  Widen is 0
	).

% Suppose QueryString (the word being looked up) is "heart" and Table is first_wordsb.
% The monstrous code below creates the query
% 'select suistrings.nmstr, suistrings.str, cuiconcept.concept
%         from first_wordsb, suistrings, cuiconcept
%        where first_wordsb.word=''heart''
%          and first_wordsb.sui = suistrings.sui
%          and first_wordsb.cui = cuiconcept.cui'

db_get_mwi_word_data_aux(0, Table, Word, DebugFlags, RawResults) :-
	db_get_mwi_word_data_NARROW(Table, Word, DebugFlags, RawResults).
db_get_mwi_word_data_aux(1, Table, Word, DebugFlags, RawResults) :-
	db_get_mwi_word_data_WIDE(Table, Word, DebugFlags, RawResults).

% This is the wide version
db_get_mwi_word_data_WIDE(Table, Word, DebugFlags, RawResults) :-
	form_simple_query("nmstr, str, concept", Table, "word", Word, Query),
	% statistics(runtime, [T0|_]),
        run_query(Query, RawResults, 1),
	% statistics(runtime, [T1|_]),
	% Total is T1 - T0,
	% length(RawResults, Length),
	% format(user_output, 'TABLE|~w|~w|~d ms|~d results.~n', [Table,Word,Total,Length]),
	debug_db_get_mwi_data_aux_2(DebugFlags, RawResults),
	!.
db_get_mwi_word_data_WIDE(Table, Word, _DebugFlags, _RawResults) :-
	fatal_error('db_get_mwi_word_data_aux failed for ~p on table ~p.~n', [Word,Table]).

% This is the narrow version
db_get_mwi_word_data_NARROW(Table, Word, DebugFlags, RawResults) :-
	% Table is one of all_words, first_words, first_wordsb,
	% first_words_of_one, first_words_of_two
	concatenate_items_to_string(["suistrings.nmstr, suistrings.str, ",
                                    "cuiconcept.concept"],
                                   Fields),
        concatenate_items_to_string([Table,
                                     ", suistrings, cuiconcept"],
                                    Tables),
        concatenate_items_to_string([Table,".word"], Field),
        form_simple_query(Fields, Tables, Field, Word, Query0),
        concatenate_items_to_string([" and ",
                                     Table, ".sui = suistrings.sui and ",
                                     Table, ".cui = cuiconcept.cui"],
                                    QueryTail),
        concatenate_items_to_atom([Query0,QueryTail], Query),
        run_query(Query, RawResults, 1),
	debug_db_get_mwi_data_aux_2(DebugFlags, RawResults),
	!.

db_get_mwi_word_data_NARROW(Table, Word, _DebugFlags, _RawResults) :-
	fatal_error('db_get_mwi_word_data_aux failed for ~p on table ~p.~n', [Word,Table]).

form_uscs([], _N, []) :- !.
form_uscs([First|Rest], N, [usc(Nmstr,Str,Concept)|ModifiedRest]) :-
	First = [Nmstr,Str,Concept0],
	!,
	( Concept0 == 'X' ->
	  Concept = Str
	; Concept = Concept0
	),
	NewN is N + 1,
	form_uscs(Rest, NewN, ModifiedRest).
form_uscs([First|_], N, _) :-
	!,
	NewN is N + 1,
	fatal_error('form_uscs failed on item ~d = ~p.~n', [NewN,First]).
form_uscs(X, N, _) :-
	fatal_error('form_uscs failed after item ~d; the non-list is ~p.~n', [N,X]).

/* db_get_mwi_word_count(+Table, +Input, -Count)

db_get_mwi_word_count/3 gets the word Count from Table for Input.
It fails if Input is not in Table. Note that since counts of 1 are not stored
in the table, failure indicates a count of either 0 or 1. */

db_get_mwi_word_count(Table, Word, Count) :-
	Word \== [],
	ensure_atom(Table, TableAtom),
	ensure_atom(Word, WordAtom),
	db_get_mwi_word_count_aux(TableAtom, WordAtom, [[Count]]),
	!.
% This predicate must be allowed to fail gracefully
% db_get_mwi_word_count(Table, Word, 0) :-
%        fatal_error('db_get_mwi_word_count failed for ~p on table ~p.~n',
%              	      [Word,Table]).

db_get_mwi_word_count_aux(TableAtom, WordAtom, WordCount) :-
	form_simple_query("wcount", TableAtom, "word", WordAtom, Query),
	run_query(Query, WordCount, 1).

/* 
   db_get_variants(+Concept, +Category, -Variants)

db_get_variants/3 gets the list of Variants of Concept, which can be either
an atom or a string.  The result can depend on the Category, but if the
Category is [], no filtering is done on Category.
The table used ot obtain the variants is determined by db_access_var_table/1. */

db_get_variants(Concept, Category, Variants) :-
	ensure_string(Concept, ConceptString),
	db_get_variants_aux(ConceptString, Category, Variants).

% always treat adjectives as nouns, if possible
db_get_variants_aux(ConceptString, Category0, AllVariantsTerms) :-
	get_real_category(Category0, Category),
	Recurse = 1,
	( Category == adj ->
	  get_variants_from_db(ConceptString, Recurse, noun, NounVariantsLists),
	  maybe_get_adj_variants(ConceptString, Recurse, NounVariantsLists, AdjVariantsLists),
	  append(NounVariantsLists, AdjVariantsLists, AllVariantsLists)
	; get_variants_from_db(ConceptString, Recurse, Category, AdjVariantsLists),
	  AllVariantsLists = AdjVariantsLists
	),
	sort(AllVariantsLists, SortedAllVariantsLists),
	convert_to_variant_terms(SortedAllVariantsLists, AllVariantsTerms).

maybe_get_adj_variants(ConceptString, Recurse, NounVariants, AdjVariants) :-
	  ( atom_codes(ConceptAtom, ConceptString),
	    \+ memberchk([_Dist,ConceptAtom,_LexCat,_Hist,_Roots], NounVariants) ->
	    % If the noun variants don't include the word itself,
	    % e.g., for "pancreatic" with expvars == 2,
	    % look for adj variants and combine the results.
	    get_variants_from_db(ConceptString, Recurse, adj, AdjVariants)
	  ; AdjVariants = []
	  ).

get_real_category([Category], Category).
get_real_category([],         []).

get_variants_from_db(WordString, Recurse, Category, AllVariantsLists) :-
	db_access_var_table(VarTable),
	ensure_string(VarTable, VarTableString),
	( ( control_option(allcats)
	  ; Category == [] ) ->
	  % select var, vcat, dist, hist, roots from varsan where word=\'haemophilic\'
	  form_simple_query("dist, var, vcat, hist, roots",
			      VarTableString, "word", WordString, Query)
	; ensure_atom(Category, CategoryAtom),
	  % select var, vcat, dist, hist, roots from varsan
	  %     where word=\'haemophilic\' and wcat=\'noun\'
	  form_complex_query("dist, var, vcat, hist, roots",
			     VarTableString, "word", WordString, "wcat", CategoryAtom, Query)
	),
	run_query(Query, VariantsLists, 1),
	maybe_get_expanded_variants(Recurse, WordString, ExpandedVariantsLists),
	% maybe_get_subsyn_variants(Recurse, WordString, SubSynVariantsLists),
	% append([VariantsLists,ExpandedVariantsLists,SubSynVariantsLists], AllVariantsLists).
	append(VariantsLists, ExpandedVariantsLists, AllVariantsLists).


% maybe_get_subsyn_variants(Recurse1, WordString, SubSynVariants) :-
% 	( \+ control_option(subsyn) ->
% 	  SubSynVariants = []
% 	; Recurse1 =:= 0 ->
% 	  SubSynVariants = []
% 	; Category = [],
% 	  atom_codes(WordAtom, WordString),
% 	  setof(SSV, subsyn_variant(WordAtom, SSV), SubSynVariants0) ->
% 	  variant_score(synonym, SynonymScore),
% 	  atom_codes(WordAtom, WordString),
% 	  (  foreach(SubSynVar, SubSynVariants0),
% 	     foreach(SubSynVarList, SubSynVariants1),
% 	     param([Category,SynonymScore])
%  	  do SubSynVarList = [SynonymScore,SubSynVar,noun,s,[]]
% 	  ),
% 	  SubSynVariants = SubSynVariants1
% 	; SubSynVariants = []
% 	).

% 	; % First, look in the subsyn_vars table to get the subsynonymy variant,
% 	  % e.g., assessment --> evaluation
% 	  form_simple_query("subsynvar", "subsynvars", "word", WordString, Query),
% 	  run_query(Query, SubSynResult, 1),
% 	  sort(SubSynResult, SortedSubSynResult),
% 	  ( SortedSubSynResult == [] ->
% 	    SubSynVariants = []
% 	  ; % Then get the usual variants based on the SubSyn variant(s)
% 	    % Result = [[ExpVar]],
% 	    % Result looks like [[v1], [v2], ..., [v3]]
% 	    Category = [],
% 	    Recurse2 is 0,
% 	    variant_score(synonym, SynonymScore),
% 	    (  foreach([SubSynVar], SortedSubSynResult),
% 	       foreach(SubSynVarList, SubSynVariants0),
% 	       param([Category,Recurse2,SynonymScore])
% 	    do SubSynVarList = [SynonymScore,SubSynVar,noun,s,[]]
% 	    ),
% % 	    (  foreach([SubSynVar], SortedSubSynResult),
% % 	       foreach(SubSynVarList, SubSynVariants0),f% 
% % 	       param([Category,Recurse2,SynonymScore])
% % 	    do get_variants_from_db(SubSynVar, Recurse2, Category, SubSynVarList0),
% % 	       adjust_subsyn_scores(SubSynVarList0, SynonymScore, SubSynVarList1),
% % 	       SubSynVarList = [[SynonymScore,SubSynVar,noun,s,[]]|SubSynVarList1]
% % 	    ),
% 	    % append(SubSynVariants0, SubSynVariants)
% 	    SubSynVariants = SubSynVariants0
% 	  )	    
% 	).

% adjust_subsyn_scores(VarsIn, SynonymScore, VarsOut) :-
% 	(  foreach([DistanceIn, Variant,LexCat,HistoryIn, Roots], VarsIn),
% 	   foreach([DistanceOut,Variant,LexCat,HistoryOut,Roots], VarsOut),
% 	   param([SynonymScore])
%         do DistanceOut is DistanceIn + SynonymScore,
% 	   concat_atom([d,HistoryIn], HistoryOut)
% 	).
 
maybe_get_expanded_variants(Recurse1, WordString, ExpandedVariants) :-
	( \+ control_option(expvars) ->
	  ExpandedVariants = []
	; Recurse1 =:= 0 ->
	  ExpandedVariants = []
	; % IF  expvars control option is set
	  % AND Recurse is 1:
          % First, look in the expvar table to get the expanded variant,
	  % e.g., intrahepatically --> hepatic
	  control_value(expvars, ExpVarsLevelInteger),
%	  format(user_output, 'GEV: ~d:~s~n', [ExpVarsLevelInteger,WordString]),
	  ensure_atom(ExpVarsLevelInteger, ExpVarsLevelAtom),
	  form_complex_query("level, expvar, penalty", "expvars",
			     "word", WordString, "level", ExpVarsLevelAtom, Query),
	  run_query(Query, ExpVarsResult, 1),
	  sort(ExpVarsResult, SortedExpVarsResult),
	  ( SortedExpVarsResult == [] ->
	    ExpandedVariants = []
	  ; % Then get the usual variants based on the expanded variant(s)
	    % With expvars.2, multiple results are possible!
	    % Result = [[ExpVar]],
	    % Result looks like [[v1], [v2], ..., [v3]]
	    Recurse2 is 0,
	    Category = [],
	    variant_score(derivation, DerivationScore),
	    (  foreach([_Level,ExpVar,Penalty], SortedExpVarsResult),
	       foreach(ExpVarList, ExpandedVariants0),
	       param([Recurse2,Category,DerivationScore])
	    do get_variants_from_db(ExpVar, Recurse2, Category, ExpVarList0),
	       adjust_expvar_scores(ExpVarList0, Penalty, DerivationScore, ExpVarList)
	    ),
	    append(ExpandedVariants0, ExpandedVariants)	  
	  )	    
	).

adjust_expvar_scores(VarsIn, Penalty, DerivationScore, VarsOut) :-
	(  foreach([DistanceIn, Variant,LexCat,HistoryIn, Roots], VarsIn),
	   foreach([DistanceOut,Variant,LexCat,HistoryOut,Roots], VarsOut),
	   param([DerivationScore,Penalty])
        do DistanceOut is DistanceIn + DerivationScore + Penalty,
	   concat_atom([d,HistoryIn], HistoryOut)
	).


convert_to_variant_terms([], []).
% temp  to handle null values until they're removed
% convert_to_variant_terms([[]|Rest],ConvertedRest) :-
%    !,
%    convert_to_variant_terms(Rest,ConvertedRest).
convert_to_variant_terms([[Distance,Var,VCat0,Hist,Roots]|Rest],
                         [v(Var,DistInteger,VCat,HistCodes,Roots,_)|ConvertedRest]) :-
	ensure_atom(Distance, DistAtom),
	atom_codes(DistAtom, DistCodes),
	number_codes(DistInteger, DistCodes),
	convert_variant_category(VCat0, VCat),
	% format('~nconverted ~q to ~q~n', [VCat0, VCat]),
	atom_codes(Hist, HistCodes),
	convert_to_variant_terms(Rest, ConvertedRest).

convert_variant_category(VCat0, VCat) :-
	( VCat0 == none ->
	  VCat = []
	; VCat0 == [] ->
	  VCat = []
	; VCat = [VCat0]
	).

/* form_simple_query(+Fields, +Table, +Field, +Value, -Query)
   form_complex_query(+Fields, +Table, +Field1, +Value1, +Field2, +Value2, -Query)

form_simple_query/5 constructs the atom Query of the form
     select <Fields> from <Table> where <Field>="Value"
where Fields, Table, Field, and Value can be atoms or strings.
form_complex_query/7 constructs the atom Query of the form
     select <Fields> from <Table> where <Field1>="Value1" and <Field2>="Value2"
where Fields, Table, Field1, Value1, Field2, and Value2 can be atoms or strings.
*/

form_simple_query(Fields, Table, Field, Value0, Query) :-
	form_complex_query(Fields, Table, Field, Value0, [], [], Query).

form_complex_query(Fields, Table, Field1, Value1_0, Field2, Value2_0, Query) :-
        ensure_string(Value1_0, Value1_1),
        double_quotes(Value1_1, Value1),
        form_rest_query(Value2_0, Field2, RestQuery),
        concatenate_items_to_atom(["select ",Fields," from ",Table,
                                   " where ",Field1,"='",Value1 | RestQuery],
                                  Query).

form_rest_query(Value2_0, Field2, RestQuery) :-
	% in db_get_variants/3, Value2_0 is the Category;
	% if the Category is [], ignore it, and
	% don't instantiate the rest of the query,
	% other than closing the final quote!
	( Value2_0 == [] ->
	  RestQuery = ["'"]
	; ensure_string(Value2_0, Value2_1),
	  double_quotes(Value2_1, Value2),
	  RestQuery = [ "' and ",Field2,"='",Value2,"'"]
	).

/* double_quotes(+String, -ModifiedString)

double_quotes/2 computes ModifiedString by doubling each occurrence of a
single quotation mark.  */

double_quotes("", "").
double_quotes([39|Rest], [39,39|ModifiedRest]) :-
	!,
	double_quotes(Rest, ModifiedRest).
double_quotes([First|Rest], [First|ModifiedRest]) :-
	double_quotes(Rest, ModifiedRest).

get_data_version(Version) :-
	( control_value(mm_data_version, Version) ->
	  true
        ; default_version(Version)
        ).

% 2008AA, 2009AB, etc.
get_data_release(NormalizedRelease, Announce) :-
	default_release(DefaultRelease),
	normalize_db_access_year(DefaultRelease, NormalizedDefaultRelease),
	% Is the model year explicitly specified on the command line?
	( control_value(mm_data_year, SpecifiedRelease),
	  normalize_db_access_year(SpecifiedRelease, NormalizedSpecifiedRelease),
	  NormalizedDefaultRelease \== NormalizedSpecifiedRelease ->
	  ( Announce is 1 ->
	    send_message('### WARNING: Overriding default model ~w with ~w.~n',
			 [NormalizedDefaultRelease, SpecifiedRelease])
	  ; true
	  ),
	  NormalizedRelease = NormalizedSpecifiedRelease
	; NormalizedRelease = NormalizedDefaultRelease
	).

normalize_db_access_year(Release, NormalizedRelease) :-
	ensure_atom(Release, ReleaseAtom),
	( ReleaseAtom = '99' ->
	  NormalizedRelease = '1999AA'
	; ReleaseAtom = '1999' ->
	  NormalizedRelease = '1999AA'
	; atom_length(ReleaseAtom, ReleaseLength),
	  atom_codes(ReleaseAtom, Codes),
	  ( % e.g., 08, 09, 10, etc.
	    ReleaseLength =:= 2,
	    all_digits(Codes) ->
	    concat_atom(['20', ReleaseAtom, 'AA'], NormalizedRelease)
	  ; ReleaseLength =:= 4,
	    all_digits(Codes) ->
	    Codes = [FirstCode,SecondCode|_],
	      % e.g., 2008, 2009, 2010, etc.
	    ( FirstCode == 0'2 ->
	      concat_atom([ReleaseAtom, 'AA'], NormalizedRelease)
	      % e.g., 0809, 0910, etc.
	    ; FirstCode == 0'0 ->
	      atom_codes(FirstDigit, [FirstCode]),
	      atom_codes(SecondDigit, [SecondCode]),
	      concat_atom(['20', FirstDigit, SecondDigit, 'AB'], NormalizedRelease)
	      % e.g., 1011, 1112, etc.
	    ; FirstCode == 0'1 ->
	      atom_codes(FirstDigit, [FirstCode]),
	      atom_codes(SecondDigit, [SecondCode]),
	      concat_atom(['20', FirstDigit, SecondDigit, 'AB'], NormalizedRelease)
	    )
	  ; ReleaseLength =:= 4,
	    % Nonstandard, e.g., 08AA, 09AB, 10AA, etc.
	    Codes = [D1, D2, A, ABC],
	    local_digit(D1),
	    local_digit(D2),
	    A == 0'A,
	    is_ABC(ABC) ->
	    choose_century(D1, Century),
	    concat_atom([Century, Release], NormalizedRelease)
	  ; ReleaseLength =:= 6,
	    % e.g., 2008AA, 2009AB, 2010AA, etc.
	    Codes = [D1, D2, D3, D4, A, ABC],
	    all_digits([D1,D2,D3,D4]),
	    A == 0'A,
	    is_ABC(ABC) ->
	    NormalizedRelease = ReleaseAtom
	  )
	),
	!.
normalize_db_access_year(Release, _NormalizedRelease) :-
	fatal_error('There is no ~w Model Release.~n', [Release]).

choose_century(Digit, Century) :-
	( Digit =< 0'5 ->
	  Century = '20'
	; Century = '19'
	).

all_digits([]).
all_digits([H|T]) :- local_digit(H), all_digits(T).

is_ABC(0'A).
is_ABC(0'B).
is_ABC(0'C).


get_data_model(Model) :-
	% This is more complex than it need be.
	( control_option(strict_model) ->
	  Model = strict
	; control_option(relaxed_model) ->
	  Model = relaxed
	; Model = strict
	).

debug_db_get_mwi_data_1(DebugFlags) :-
	( memberchk(7, DebugFlags) ->
	  format('~ndb_get_mwi_word_data: Input is an atom.~n', []),
	  ttyflush
	; true
	).

debug_db_get_mwi_data_2(DebugFlags) :-
	( memberchk(7, DebugFlags) ->
	  format('db_get_mwi_word_data: back from db_get_mwi_word_data_aux calling form_uscs.~n',[]),
	  ttyflush
	; true
	).

debug_db_get_mwi_data_3(DebugFlags, Results) :-
	( memberchk(7, DebugFlags) ->
	  format('db_get_mwi_word_data: ',[]),
	  ttyflush,
	  length(Results,NR),
	  format('~d form_uscs results.~n',[NR]),
	  ttyflush
	; true
	).

debug_db_get_mwi_data_aux_2(DebugFlags, RawResults) :-
	( memberchk(7, DebugFlags) ->
	  format('db_get_mwi_word_data_aux: ',[]),
	  ttyflush,
	  length(RawResults,NRR),
	  format('~d RawResults.~n',[NRR]),
	  ttyflush,
	  format('db_get_mwi_word_data_aux: RawResults = ~p~n',[RawResults]),
	  ttyflush
	; true
	).

ensure_string(AtomOrString, String) :-
        ( is_print_string(AtomOrString) ->
          String = AtomOrString
        ; atom_codes(AtomOrString, String)
        ).

db_get_lex_base_forms(Word, BaseForms) :-
	form_simple_query("baseform", "lex_form", "word", Word, Query),
	run_query(Query, BaseForms0, 1),
	append(BaseForms0, BaseForms).

db_get_lex_base_forms_with_cat(Word, LexCat, BaseForms) :-
	( LexCat == [] ->
	  db_get_lex_base_forms(Word, BaseForms)
	; form_complex_query("baseform", "lex_form", "word", Word, "lexcat", LexCat, Query),
	  run_query(Query, BaseForms0, 1),
	  append(BaseForms0, BaseForms)
	).

db_get_lex_cats(Word, LexCats) :-
	form_simple_query("lexcat", "lex_form", "word", Word, Query),
	run_query(Query, LexCats0, 1),
	append(LexCats0, LexCats1),
	sort(LexCats1, LexCats).

db_get_lex_record_list(EUIList, LexicalEntryList) :-
	(  foreach(EUI, EUIList),
	   foreach(LexicalEntry, LexicalEntryList)
           % The BDB lexical entry is just an ordinary atom, so
	do db_get_lex_record(EUI, LexicalEntryAtom),
	   % transform it into a string, and
	   atom_codes(LexicalEntryAtom, LexicalEntryString),
	   % read the term from the string!
	   read_from_codes(LexicalEntryString, LexicalEntry)
	   ).

% This predicate will not used until the find_prefix logic is ported to Prolog
db_get_lex_record(EUI, LexRec) :-
	form_simple_query("lexrec", "lex_rec", "EUI", EUI, Query),
	run_query(Query, [[LexRec]], 1).

% This predicate is used only in dynamic variant generation.
db_get_lex_im_varlist(BaseForm, VarList) :-
	form_simple_query("infl_variant, infl_lexcat, infl_feature", "im_vars", "citation_form",
			   BaseForm, Query),
	run_query(Query, VarList, 1).

% db_get_lex_im_varlist(Word, VarList) :-
% 	form_simple_query("infl_variant, infl_lexcat, infl_feature", "im_vars", "citation_form",
% 			  Word, Query),
% 	run_query(Query, VarList, 1).

% This predicate is used only in dynamic variant generation.
db_get_lex_dm_variants_no_cat(Word, DMVariants) :-
	form_simple_query("dm_variant, dm_variant_lexcat", "dm_vars", "base_form", Word, Query),
	run_query(Query, DMVariants, 1).
db_get_lex_dm_variants_with_cat(Word, Category, DMVariants) :-
	form_complex_query("dm_variant, dm_variant_lexcat", "dm_vars", "base_form",
			   Word, "base_lexcat", Category, Query),
	run_query(Query, DMVariants, 1).


% Each element of EUIList is either the fake EUI '0' or an actual EUI, e.g., 'E0000001';
% if the first EUI of EUIList is '0',
% PrefixAtom is a (proper) prefix of a normalized lexical item.
db_get_lex_prefix_EUIs(PrefixAtom, EUIList) :-
	form_simple_query("EUI", "norm_prefix", "norm_prefix", PrefixAtom, Query),
	% If the call to run_query returns [], it means failure,
	% so force the result to be [H|T]!
	run_query(Query, [H|T], 1),
	append([H|T], EUIList).
	% format(user_output, 'Q1: ~w~n', [PrefixAtom]).

% No longer needed, now that cui_sourceinfo.txt table is created
% without first_of_each_source_only option.

% db_get_string_sources(String, SourcesList) :-
% 	% No need to get the sources unless they're necessary!
% 	( \+ control_option(sources),
% 	  \+ control_option(restrict_to_sources),
% 	  \+ control_option(exclude_sources),
% 	  \+ control_option(machine_output),
% 	  \+ xml_output_format(_) ->
% 	  SourcesList = []
% 	; form_simple_query("sources", "string_sources", "string", String, Query),
% 	  run_query(Query, [[SourcesAtom]], 1),
% 	  split_atom_completely(SourcesAtom,  ',', SourcesList0),
% 	  sort(SourcesList0, SourcesList)
% 	).

% db_get_cui_semtypes(CUI, SemTypesList) :-
% 	form_simple_query("semtypes", "cui_semtypes", "cui", CUI, Query),
% 	run_query(Query, [[SemTypesAtom]], 1),
% 	split_atom_completely(SemTypesAtom, ',', SemTypesList0),
% 	sort(SemTypesList0, SemTypesList).

db_get_cui_sources_and_semtypes(CUI, SourcesList, SemTypesList) :-
        % No need to get the sources unless they're necessary!
        ( \+ control_option(sources),
          \+ control_option(restrict_to_sources),
          \+ control_option(exclude_sources),
          \+ control_option(machine_output),
          \+ xml_output_format(_),
          \+ json_output_format(_) ->
          form_simple_query("semtypes", "cui_srcs_sts", "cui", CUI, Query),
          run_query(Query, [[SemTypesAtom]], 1),
          SourcesAtom = []
        ; form_simple_query("sources, semtypes", "cui_srcs_sts", "cui", CUI, Query),
          run_query(Query, [[SourcesAtom, SemTypesAtom]], 1)
        ),
        split_atom_completely(SourcesAtom,  ',', SourcesList0),
        split_atom_completely(SemTypesAtom, ',', SemTypesList0),
        sort(SourcesList0, SourcesList),
        sort(SemTypesList0, SemTypesList).
