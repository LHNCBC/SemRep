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

% File:	    usemrep.pl   % -*- prolog -*-
% Module:   usemrep
% Author:   tcr, NLS, Usemrep  modified by MF to include semspec;
%           modified by FML for code cleanup and modularization.
% Purpose:  semantic interpretation of medical text.
%           MF added a semantic_specification module.
% ----- Module declaration and exported predicates

:- module(usemrep, [
                errorFlag/1,
		fg/0,
		go/0,
		go_4/4,
		override_file/3,
		override_control_option/2,
		program_abbreviation/1,
		program_name/1,
		program_version/1,
		q/0,
		run/0,
		shutdown_usemrep/0,
		usage/0
   ]).

:- load_files( usemrep_lib(module_version), [
		when(compile_time)
   ]).

% ----- Imported predicates

% Print strings double-quoted
:- use_module(skr_lib(print_chars)).

:- use_module(skr_lib( addportray ), [
		add_portray/1,
		del_portray/1
   ]).

:- use_module( library( file_systems ), [
		close_all_streams/0
   ]).

:- use_module( library( lists ), [
		append/2,
		delete/3,		 
		last/2,
		rev/2
   ]).

:- use_module( library( sets ), [
		list_to_set/2
   ]).

:- use_module(library(avl),[
	avl_to_list/2,
	empty_avl/1
    ]).

% Never called
% :- use_module( lexicon( lexical ), [
% 	concatenate_strings/3
%    ]).

:- use_module(metamap(metamap_parsing), [
	generate_syntactic_analysis_plus/4
    ]).

:- use_module(tagger(tagger_access), [
	% get_tagger_server_hosts_and_port/3,
	tag_text/5
   ]).

%:- use_module(wsd(wsdmod), [
%	get_WSD_server_hosts_and_port/3
%   ]).

:- use_module( skr(skr_fe), [
	form_expanded_utterances/3,
        form_original_utterances/7
   ]).

:- use_module( skr(skr_utilities), [
	convert_non_text_fields_to_blanks/6,
	write_raw_token_lists/2,
	write_sentences/2
	]).

:- use_module( skr_lib(nls_lists), [
		get_from_list/3
   ]).

:- use_module(skr_lib(pos_info), [
	adjust_AAs_pos_info/4,
	create_EXP_raw_token_list/7,
	create_UNEXP_raw_token_list/6
    ]).

:- use_module( skr_lib(server_choice), [
	get_server_streams/1
   ]).

%:- use_module( skr_lib(sicstus_utils), [
%		concat_atom/2
%   ]).

:- use_module( usemrep_domain(domain_data), [
	domain_name/1
   ]).

:- use_module( usemrep_lib(conditionally), [
	conditionally_call_eot_format/1,
	conditionally_display_current_control_options/2,
	conditionally_issue_usage_message/0,
	conditionally_output_abstract_id/2,
	conditionally_output_utterance/3,
	conditionally_remove_aa_and_aadef_tokens/3
	% conditionally_write_special/1
   ]).

%:- use_module( usemrep_lib(jdimod), [
%		call_jdi_and_choose_domain/7
%   ]).

:- use_module( usemrep_lib(debug), [
	set_debug_bit_vector/2,
	debug_call/3
   ]).

:- use_module( usemrep_lib(abgenemod), [
	call_abgene/3,
	get_abgene_atom/2
   ]).

:- use_module( usemrep_lib(sem_xml), [
	conditionally_print_xml_header/2,
	conditionally_print_xml_footer/3,
	generate_document_header_xml/3
   ]).

:- use_module( usemrep_lib(wrappers), [
	anaphora_resolution_wrapper/12,
	% consult_tagged_text_wrapper/8,
	draw_inferences_wrapper/8,
	extract_sentences_wrapper/12,
	generate_variant_info_wrapper/5,
	% get_abstract_id_wrapper/5,
	get_semtypes_wrapper/6,
	get_all_bases_wrapper/6,
	% insert_msu_token_lists_wrapper/6,
	identify_genetic_concepts_wrapper/7,
	intermediate_analysis_wrapper/11,
	metamap_phrases_wrapper/17,
	% minimal_commitment_analysis_wrapper/8,
	perform_domain_processing_wrapper/9,
	semantic_interpretation_wrapper/16,
	semantic_specification_wrapper/11
	% tokenize_string_wrapper/4
   ]).

:- use_module( usemrep_domain(domain_processing), [
	choose_domain/2,
	domain_specific_initialize_metamap/1,
	domain_specific_unset_control_options/1
   ]).

:- use_module( usemrep_lib(anaphora), [
	identify_sortal_anaphora/3
	% identify_sortal_anaphora_baseline/3
		       
	]).

:- use_module( usemrep_lib(ssuppserv), [
	add_indexes_to_all_MSUs/3,
	add_msu_index/3,
	announce_initialize_db_access_for_entrezgene/0,
	announce_initialize_db_access_for_genspec/1,
	% announce_initialize_db_access_for_profile_call/0,
	announce_initialize_metamap/1,
	announce_stop_db_access_for_entrezgene/0,
	announce_stop_db_access_for_genspec/0,
	% announce_stop_db_access_for_profile_call/0,
	announce_stop_metamap/0,
	% close_open_stream/1,
	choose_utterance_list/4,
	% create_citation_text_atom/2,
	determine_utterance_types/2,
	get_abstract_id/2,
	get_last_indexes/3,
	get_text/2,
	maybe_insert_domain_concepts/4,
	% merge_sentences/2,
	override_control_options/0,
	override_file_args/2,
	processing_mode/1,
	remove_referential_analysis_if_necessary/2
	% retokenize_for_period/2,
	% trim_all_whitespace/2
   ]).

:- use_module( usemrep_lib(write_results), [
	mca_output_with_label/3,
	phrasex_output/3,
	write_sorted/11
	% write_special/3
   ]).

:- use_module( usemrep_lib(module_version), [
	set_global_module_version/1
   ]).

:- use_module( skr_lib(retokenize), [
	remove_null_atom_defns/2,
	retokenize/2
   ]).

:- use_module( skr_lib( efficiency ), [
	maybe_atom_gc/3
   ]).

:- use_module( skr_lib( nls_strings ), [
	atom_codes_list/2,
	form_one_string/3,
	portray_strings_double_quoted/1,
	replace_all_substrings/4,
	safe_number_codes/2,
	split_string_completely/3
   ]).

:- use_module( skr_lib( nls_system ), [
        % assert_control_option/1,
	% assert_control_value/2,
        control_option/1,
	control_value/2,
	display_control_options_for_modules/2,
	get_control_options_for_modules/2,
	get_from_iargs/4,
	interpret_options/4,
	interpret_args/4,
	option_requires_list_arg/1,
	parse_command_line/1,
	reset_control_options/1,
	set_control_values/2,
	toggle_control_options/1,
	verify_options_and_values/0
   ]).

:- use_module( usemrep_lib( portray_minimal_syntax ), [
		portray_minimal_syntax_structure/1
   ]).

:- dynamic override_file/3.
:- dynamic override_control_option/2.
:- dynamic errorFlag/1.             % Used to flag an error from deep inside

% ***************************************************************************
% **************************** USEMREP TOP LEVEL *****************************
% ***************************************************************************

fg :- debug, go.

% go/0 is the typical interactive call,
% whether in batchmode (processing an input file from inside prolog)
% or in user_input mode (user types in sentences).
go :- go_4(interactive, _Predications, _InputFile, _OutputFile).

run :- go_4(runtime, _Predications, _InputFile, _OutputFile).

% go/1 is called from runtime_entry(start) in loader.pl

% loader.pl calls go(runtime)
% +Mode, -Predications, -InputFile, -OutputFile
go_4(Mode, Predications, InputFile, OutputFile ) :-
	close_all_streams,
%	add_portray(portray_strings_double_quoted),
	% Comment next line out to disable pretty-printing
%	add_portray(portray_minimal_syntax_structure),
	% -Options, -Args:
	parse_command_line(command_line(Options,Args)),
	reset_control_options(usemrep),
	( initialize_usemrep(Options, Args, InterpretedArgs) ->
	  override_control_options,
	  set_debug_bit_vector(Mode, DebugBitVector),
	  batch_usemrep(InterpretedArgs, DebugBitVector, Predications, InputFile, OutputFile )
        ; usage
        ),
	stop_usemrep(Mode).

stop_usemrep(interactive).
stop_usemrep(runtime) :- shutdown_usemrep.

shutdown_usemrep :-
	announce_stop_metamap,
	% announce_stop_db_access_for_profile_call,
	announce_stop_db_access_for_entrezgene,
	announce_stop_db_access_for_genspec,
	close_all_streams.		              % from library(files)

q :- shutdown_usemrep.

/* initialize_usemrep(+Options, +Args, -InterpretedArgs)

initialize_usemrep/3 interprets command line options and arguments (opening
files as necessary) and then sets and displays the usemrep control options
discovered.  It returns InterpretedArgs for later use (e.g., the stream
associated with a file).
shutdown_usemrep/0 disconnects from servers, stops access to other modules,
and closes all streams.  
%%%%% is_control_option(usemrep,a,abstract_mode,no,none).
is_control_option(usemrep,b,debug_format,no,none).
is_control_option(usemrep,o,parse_output,yes,none).
is_control_option(usemrep,h,help,no,none).
is_control_option(usemrep,z,time,no,none).
is_control_option(usemrep,i,info,no,none).
is_control_option(usemrep,t,tag,yes,none).
is_control_option(usemrep,m,mmofile,no,
                  aspec(mmofile,mandatory,file,read,no_default,'MMO file')).
is_control_option(usemrep,w,warnings,no,none).

*/

initialize_usemrep(Options, Args, InterpretedArgs) :-
	get_control_options_for_modules([usemrep], AllOptions),
	interpret_options(Options, AllOptions, usemrep, IOptions),
	\+ member(iopt(help,_), IOptions),
	program_version(Version),
	program_abbreviation(Abbrev),
	ArgSpec=[ aspec(infile, mandatory, file, read, user_input,
                           'Input file containing labelled utterances.'),
                  aspec(outfile, mandatory, file, write,
                            or(['<infile>', '.', Abbrev, '.', Version], user_output),
                            'Output file.')
		 ],
	override_file_args(Args, OverrideArgs),
	% Among other things, opens files named on command line for reading or writing
	interpret_args(IOptions, ArgSpec, OverrideArgs, InterpretedArgs),
	toggle_control_options(IOptions),
	set_control_values(IOptions, InterpretedArgs),
	conditionally_display_current_control_options(IOptions, Version),
	% set default MetaMap options values
	set_metamap_option_with_value(lexicon_year,    '2006',    LexiconYear),
	set_metamap_option_with_value(mm_data_year,    '2006AA',  MMDataYear),
	set_metamap_option_with_value(mm_data_version, 'USAbase', MMDataVersion),
	get_global_module_version_and_lexicon_type(GlobalModuleVersion, LexiconType),
	set_global_module_version(GlobalModuleVersion),
% 	( control_value(mm_data_year,'2012AA') ->
% 	  set_global_module_version('12'),
%	  LexiconType = c
%	; control_value(mm_data_year,'2014AA') ->
%	  set_global_module_version('14'),
%	  LexiconType = db
%	; control_value(mm_data_year,'2015AA') ->
%	  set_global_module_version('15'),
%	  LexiconType = db
%	; set_global_module_version('06'),
%	  LexiconType = c
%	),
	MetaMapInitializationOptions0 = [ % as_module, % These commented-out options are obsolete!
					  % best_mappings_only,
					  % stop_large_n,
					  % no_acros_abbrs,
					  % mappings,
					  % an_derivational_variants,
					% word_sense_disambiguation,
					lexicon_year-LexiconYear,
					mm_data_year-MMDataYear,
					mm_data_version-MMDataVersion,
				        lexicon-LexiconType ],
					% show_cuis],
	( control_option(relaxed_model) ->
	  append(MetaMapInitializationOptions0,[relaxed_model],
		 MetaMapInitializationOptions1)
	; MetaMapInitializationOptions1 = MetaMapInitializationOptions0
	),
	verify_valid_domain,
	get_user_MM_options(AssertedTerms),
	append(MetaMapInitializationOptions1, AssertedTerms, ALLMetaMapInitializationOptions),
	announce_initialize_metamap(ALLMetaMapInitializationOptions),
	verify_options_and_values,
	verify_SemRep_output_options,
	announce_initialize_db_access_for_genspec(MMDataYear),
	announce_initialize_db_access_for_entrezgene,
%	announce_initialize_db_access_for_profile_call,
	conditionally_issue_usage_message.

get_user_MM_options(ListOfAssertions) :-
	% Atom = 'prune-40,restrict_to_sts-dsyn/neop,relaxed_model,mm_data_version-NLM'
	retract(control_option(mm_add)),
	retract(control_value(mm_add, Atom)),
	!,
	% Codes = "prune-40,restrict_to_sts-dsyn/neop,relaxed_model,mm_data_version-NLM"
	atom_codes(Atom, Codes),
	% ListOfMMOptions = ["prune-40","restrict_to_sts-dsyn/neop","relaxed_model","mm_data_version-NLM"]
	split_string_completely(Codes, ",", ListOfMMOptionsCodes),
	convert_all_user_MM_options(ListOfMMOptionsCodes, ListOfAssertions).
get_user_MM_options([]).

convert_all_user_MM_options([], []).
convert_all_user_MM_options([H|T], [AssertedH|AssertedT]) :-
	convert_one_user_MM_option(H, AssertedH),
	convert_all_user_MM_options(T, AssertedT).

convert_one_user_MM_option(OptionCodes, OptionTerm) :-
	  % e.g., "relaxed_model"
	( \+ memberchk(0':, OptionCodes) ->
	  atom_codes(OptionAtom, OptionCodes),
	  OptionTerm = OptionAtom
	; append([OptionNameCodes, "-", OptionValueCodes], OptionCodes) ->
	  format(user_error, '#### ERROR: Option "~s" is ill formed.~n',[OptionCodes]),
	  halt
	; append([OptionNameCodes, ":", OptionValueCodes], OptionCodes) ->
	  atom_codes(OptionNameAtom, OptionNameCodes),
	  form_option_value(OptionValueCodes, OptionNameAtom, OptionValue),
	  OptionTerm = OptionNameAtom-OptionValue
	).

% Convert atoms that look like numbers (e.g., '40') to numbers,
% but leave everything else unchanged
convert_to_number(OptionValue, OptionValueNumber) :-
	( is_a_number(OptionValue, OptionValueNumber) ->
	  true
	; OptionValueNumber = OptionValue
	).

is_a_number(Atom, Number) :-
	atom(Atom),
	atom_codes(Atom, Codes),
	safe_number_codes(Number, Codes).


form_option_value(OptionValueCodes, OptionNameAtom, OptionValue) :-
	( memberchk(0'/, OptionValueCodes) ->
	  split_string_completely(OptionValueCodes, "/", StringList),
	  atom_codes_list(AtomList, StringList),
	  OptionValue0 = AtomList
	; option_requires_list_arg(OptionNameAtom) ->
	  atom_codes(Atom, OptionValueCodes),
	  OptionValue0 = [Atom]
	; atom_codes(Atom, OptionValueCodes),
	  OptionValue0 = Atom
	),
	convert_to_number(OptionValue0, OptionValue).
	
get_global_module_version_and_lexicon_type(GlobalModuleVersion, LexiconType) :-
	( control_value(mm_data_year, MMDataYear),
	  check_known_values(MMDataYear, GlobalModuleVersion, LexiconType) ->
	  true
	; GlobalModuleVersion = '06',
	  LexiconType = db
	).

check_known_values('2012',   '12', db).
check_known_values('2012AA', '12', db).
check_known_values('2012AB', '12', db).

check_known_values('2014',   '14', db).
check_known_values('2014AA', '14', db).
check_known_values('2014AB', '14', db).

check_known_values('2015',   '15', db).
check_known_values('2015AA', '15', db).
% check_known_values('2015AB', '15', c).

check_known_values('2017',   '17', db).
check_known_values('2017AA', '17', db).

check_known_values('2018',   '18', db).
check_known_values('2018AA', '18', db).

verify_valid_domain :-
	control_value(domain, Domain),
	!,
	( domain_name(DomainName),
	  Domain = DomainName ->
	  true
	; format(user_error, '#### ERROR: Domain "~w" is unknown.~n',[Domain]),
	  abort
	).
verify_valid_domain.


program_name('usemrep').
program_abbreviation('sem').
program_version('v1.8').

/* MetaMap options

strict_model
moderate_model
optval(mm_data_version,name,uwda)
no_acros_abbr
unique_acros_abbrs_only
show_cuis
no_derivational_variants
prefer_multiple_concepts (This is a must for intra NP analysis)
*/
set_metamap_option_with_value(OptionName, DefaultValue, Value) :-
	( control_value(OptionName, OptionValue) ->
	  Value = OptionValue
	; Value = DefaultValue
	).

% ---------- USAGE ----------

usage :-
	format(user_output, '~nSemantic interpretation for biomedical text.~n',[]),
	format(user_output, '  Note: The MedPost part-of-speech tagger is called by default.~n~n',[]),
	display_control_options_for_modules(usemrep, []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  batch_usemrep  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

batch_usemrep gets input and output file names from the user and redirects I/O to 
these files.   Although output to  the user is suspended, progress is reported 
after each parse.

*/

% +InterpretedArgs, +DebugBitVector, -Predications, -InputFile, -OutputFile
batch_usemrep(InterpretedArgs, DebugBitVector, Predications, InputFile, OutputFile) :-
	get_filenames_and_streams(InterpretedArgs,
				  InputFile, InputStream,
				  OutputFile, OutputStream),
	set_input(InputStream),
	set_output(OutputStream),
	TaggingMode = '',
%	get_WSD_server_hosts_and_port(WSDServerHosts, WSDForced, WSDServerPort),
	format(user_output,'Beginning to process ~a ~asending output to ~a.~n',
		           [InputFile,TaggingMode,OutputFile]),
	processing_mode(ProcessingMode),
	maybe_atom_gc(_, _, _),

	( control_option(xml_output_format) ->
	  conditionally_print_xml_header(1,OutputStream),
	  flush_output(OutputStream)
	; true
	),
	get_server_streams(ServerStreams),
	batch_acquire_and_process_input(DebugBitVector, ProcessingMode,
					InputStream, OutputStream, ServerStreams,
%					WSDServerHosts, WSDForced, WSDServerPort,
					Predications),
	 ( control_option(xml_output_format) ->
	   conditionally_print_xml_footer(1,'XMLf1',OutputStream),
	   flush_output(OutputStream)
	 ; true
	 ),

	conditionally_call_eot_format(OutputStream),
	format(user_output, '~nOutput written to ~a.~n',[OutputFile]),
	close(InputStream),
	close(OutputStream).

% +InterpretedArgs, -everythingElse
get_filenames_and_streams(InterpretedArgs, InputFile, InputStream, OutputFile, OutputStream) :-
	get_from_iargs(infile,  name,   InterpretedArgs, InputFile),
	get_from_iargs(infile,  stream, InterpretedArgs, InputStream),
	get_from_iargs(outfile, name,   InterpretedArgs, OutputFile),
	get_from_iargs(outfile, stream, InterpretedArgs, OutputStream),
	!.

call_abgene_and_format(PrincipalDomain, ABGeneAtom, ABGenes) :-
	call_abgene(PrincipalDomain, ABGeneAtom, ABGenes),
	format(user_output, '~80c~n~n', [35]),
	!.

% If input text can be acquired, then read and process input,
% and then fail back into the call to repeat/0 in batch_usemrep/1 above.
% Otherwise, just succeed, and allow batch_usemrep/1 to terminate.

% +DebugBitVector, +ProcessingMode, +InputStream, +OutputStream,
%		-PredicationSet, -RestPredicationSets
batch_acquire_and_process_input(DebugBitVector, ProcessingMode,
				InputStream, OutputStream, ServerStreams,
%				WSDServerHosts, WSDForced, WSDServerPort,
				[PredicationSet|RestPredicationSets]) :-
	% If we can acquire text, then do so,
	batch_acquire_input_text('', InputStream, OutputStream, 
				 InputText, InputLabel,
				 OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
	                         UNExpandedUtterances, AAs, UDAs, Tokens, ExpandedUtterances),
 	!,

	% Process the text.
	% format('~nALL TOKENS:~n', []),
	batch_process_input(DebugBitVector, ProcessingMode, ServerStreams,
%			    WSDServerHosts, WSDForced, WSDServerPort,
			    OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
			    UNExpandedUtterances, AAs, UDAs,
			    Tokens, ExpandedUtterances,
			    OutputStream, InputText, InputLabel, PredicationSet),
	!,      % prevent a "halt" command from retrying batch_process_input
%	prompt_for_next,
	batch_acquire_and_process_input(DebugBitVector, ProcessingMode,
					InputStream, OutputStream, ServerStreams,
%					WSDServerHosts, WSDForced, WSDServerPort,
					RestPredicationSets).
% otherwise, just terminate and close the Predications list
batch_acquire_and_process_input(_DebugBitVector, _ProcessingMode,
				_InputStream, _OutputStream, _ServerStreams,
				[]).
%				_WSDServerHosts, _WSDForced, _WSDServerPort,[]).

% read to blank line or end-of-file
% +Prompt, +InputStream, +OutputStream, -InputText, -InputLabel,
%		-Utterances, -AAs, -Tokens, -ExpandedUtterances
batch_acquire_input_text(_Prompt, _InputStream, OutputStream, 
			 InputText, InputLabel,
			 OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
			 Utterances, AAs, UDAs,
			 Tokens, ExpandedUtterances) :-
        % Just get text here; get the domain and validate AFTER get_abstract_id
        get_text(InputText,TempInputLabel),
        InputText \== [],
	acquire_input_text(InputText, TempInputLabel, OutputStream,
			   OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
			   Utterances, ExpandedUtterances,
			   _Sentences, _CoordSentences, AAs, UDAs, Tokens),
	get_abstract_id(ExpandedUtterances, InputLabel),
        format(user_output,'Processing ----- Citation ~a -----~n', [InputLabel]),
	conditionally_output_abstract_id(OutputStream,InputLabel).

acquire_input_text(Lines0, InputLabel, OutputStream,
	           OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
		   Utterances, ExpandedUtterances,
		   Sentences, CoordSentences, AdjustedAAs, UDAs, RawTokenList) :-
%        trim_all_whitespace(Lines00,Lines0),
	extract_sentences_wrapper(Lines0, InputLabel, OutputStream,
	                          InputType, TextFields, NonTextFields,
				  Sentences, CoordSentences, UniqueID, AAs, UDAs, Lines),


	form_one_string(Lines0, " ", OrigInputStringWithBlanks),
	form_one_string(Lines, [10], InputStringWithCRs),
	form_one_string(Lines, " ",  InputStringWithBlanks),
	TokenState is 0,
	atom_codes(OrigCitationTextAtomWithBlanks, OrigInputStringWithBlanks),
	atom_codes(CitationTextAtomWithCRs, InputStringWithCRs),
	convert_non_text_fields_to_blanks(InputType, InputStringWithBlanks,
 					  TextFields, NonTextFields,
					  TrimmedTextFieldsOnlyString, NumBlanksTrimmed),
	CurrentPos is NumBlanksTrimmed,
	write_sentences(CoordSentences,Sentences),
	PrevToken = '',
	UDA_AVL = empty,
	create_EXP_raw_token_list(Sentences, UDA_AVL, PrevToken, CurrentPos, TokenState,
				  TrimmedTextFieldsOnlyString, TempExpRawTokenList),
		
				  % InputStringWithBlanks, TempExpRawTokenList),
	ExpRawTokenList = TempExpRawTokenList,
	create_UNEXP_raw_token_list(Sentences, UDA_AVL, CurrentPos, TokenState,
				    TrimmedTextFieldsOnlyString, UnExpRawTokenList),
				   % InputStringWithBlanks, UnExpRawTokenList),
	!,
	write_raw_token_lists(ExpRawTokenList,UnExpRawTokenList),
	form_original_utterances(Sentences, 1, 0, 0, CitationTextAtomWithCRs,
				UnExpRawTokenList, Utterances),
	form_expanded_utterances(CoordSentences,Utterances,ExpandedUtterances),
	( control_option(genetics_processing) ->
	  RawTokenList = UnExpRawTokenList
        ; RawTokenList = ExpRawTokenList
        ),
	avl_to_list(AAs, AAList),
	adjust_AAs_pos_info(AAList, UnExpRawTokenList, UniqueID, AdjustedAAs),

	!.

% ---------- batch_process_input ----------

% ask the user, while in batch mode, if more input should be processed
% handy for verifying incremental results in a batch file
%prompt_for_next :-
%	control_option(prompt_for_more),
%	!,
%	nl, nl,
%	prompt(_, 'Continue? '),
%	askmore(Askmore, '>> ', _, 'Continue? '),
%	( memberchk(Askmore, [65, 97]) -> % "Aa"; stop right here
%	  close_all_streams,
%	  abort
%	; Askmore == 33 ->  % "!"; don't prompt any more and do rest of file in batchmode
%	  retract(control_option(prompt_for_more))
%	; true
%	).
%prompt_for_next.

% +DebugBitVector, +ProcessingMode, +UNExpandedUtterances, +AAs, +Tokens,
%		+ExpandedUtterances, +MetaMapStream, +SMOStream,
%		+OutputStream, +InputText, +InputLabel, -Predications
batch_process_input(DebugBitVector, ProcessingMode, ServerStreams,
%		    WSDServerHosts, WSDForced, WSDServerPort,
		    OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
		    UNExpandedUtterances, AAs, UDAs,Tokens, ExpandedUtterances,
		    OutputStream, InputText, InputLabel, Predications) :-
	maybe_atom_gc(_, _, _),
	( control_option(expanded_utterances_only)
	; control_option(unexpanded_utterances_only)
	; control_option(write_syntax_only)
	; control_option(extract_phrases_only)
        ;
%	  call_jdi_and_choose_domain(ProcessingMode, InputText, ABGeneAtom,
%				     OutputStream, InputLabel, PrincipalDomain, AnatomyDomain),
	  choose_domain(ProcessingMode,PrincipalDomain),
	  get_abgene_atom(InputText,ABGeneAtom),  
	  maybe_atom_gc(_, _, _),
	  domain_specific_initialize_metamap(PrincipalDomain),
	  call_abgene_and_format(PrincipalDomain, ABGeneAtom, ABGenes),
	% UtteranceTypes are Section Headers: can be
	%		'', "RESULTS:", "METHODS:", "CONCLUSIONS:", "BACKGROUND:".
	  determine_utterance_types(UNExpandedUtterances, UtteranceTypes),
	  empty_avl(WordDataCacheIn),
	  empty_avl(USCCacheIn),
	  conditionally_remove_aa_and_aadef_tokens(PrincipalDomain, Tokens, ModifiedTokens)
        ),
	( control_option(xml_output_format) ->
	   generate_document_header_xml(InputLabel,OrigCitationTextAtomWithBlanks,DocumentHeaderXML),
	   format(OutputStream,'~s',[DocumentHeaderXML]),
	  flush_output(OutputStream)
	; true
	),
	(
	  PrecedingVarInfoList = [],
	  PrecedingAnalysis = [],
	  PrecedingConjList = [],
	  PredicationsInLen is 1,
	  process_input(UNExpandedUtterances, ExpandedUtterances, UtteranceTypes,
			ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
	                OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
			DebugBitVector, AAs, UDAs,ModifiedTokens, ABGenes,
			WordDataCacheIn, USCCacheIn, ModifiedTokens,
			_WordDataCacheOut, _USCCacheOut, _RawTokensOut,
			PrincipalDomain, _AnatomyDomain, OutputStream,
			PrecedingVarInfoList,
			PrecedingAnalysis,
			PrecedingConjList,
			PredicationsInLen,Predications)
	; format('ERROR: usemrep:process_input failed~n', [])
        ), 
	( control_option(xml_output_format) ->
	  format(OutputStream,'~n</Document>',[]),
	  flush_output(OutputStream)
	; true
	),
	domain_specific_unset_control_options(PrincipalDomain),
	maybe_atom_gc(_, _, _).
batch_process_input(_DebugBitVector, _ProcessingMode, _ServerStreams,
%		    _WSDServerHosts, _WSDForced, _WSDServerPort,
		    _OrigCitationTextAtomWithBlanks, _CitationTextAtomWithCRs,
		    _UNExpandedUtterances, _AAs, _UDAs,
		    _Tokens, _ExpandedUtterances,
		    _OutputStream, _InputText, _InputLabel, []).

process_input([], [], [], _ServerStreams,%_WSDServerHosts, _WSDForced, _WSDServerPort,
	      _OrigCitationTextAtomWithBlanks, _CitationTextAtomWithCRs,
	      _DebugBitVector, _AAs, _UDAs, _Tokens, _ABGenes, 
	      WordDataCache,USCCache,RawTokens,WordDataCache,USCCache,RawTokens,
	      _PrincipalDomain,_AnatomyDomain,_OutputStream,
	      _PrecedingVarInfoList,_PrecedingAnalysis,_PrecedingConjList,
	      _PredicationInLen, []) :-
        	format(user_output, '~n',[]).
% +InputLabel is an id of, say, a line from an abstract, e.g., '16432086.ti.1'
% +UNExpandedInputText is the text of that line
% +ExpandedInputText is UNExpandedInputText with acronyms expanded. 
% +         Identical if domain is genetics.
% +UtteranceType is a section like '', 'RESULTS:', 'METHODS:'
% +DebugBitVector
% +AAs is a list of acronyms.  Passed way down into full fielded output, but never used.
% +TokensIn
% +ABGenes
% +PrincipalDomain: eg genetics
% +OutputStream: Stream associated with OutputFile from go/4
process_input([utterance(InputLabel, UNExpandedInputText,Pos1,_Pos2)|RestUNExpandedUtterances],
	      [utterance(InputLabel, ExpandedInputText,_Pos3,_Pos4)|RestExpandedUtterances],
	      [UtteranceType|RestUtteranceTypes],
	      ServerStreams,
	      %WSDServerHosts, WSDForced, WSDServerPort,
	      OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
	      DebugBitVector, AAs, UDAs, _TokensIn,ABGenes,
	      WordDataCacheIn,USCCacheIn,RawTokensIn,
	      WordDataCacheOut,USCCacheOut,RawTokensOut,
	      PrincipalDomain, AnatomyDomain, OutputStream,
	      PrecedingVarInfoList, PrecedingAnalysis, PrecedingConjList,
	      PredicationInLen,[InputLabel:SemInterpSet|RestPredicationSets]) :-
	% ChosenUtt = (Domain==genetics) ? Utterances : ExpandedUtterances
	% Utterances are expanded in SKR:text/text_objects:find_and_expand_aas
	%		expansion = acryonym substitution
	% Print utterances only
	( control_option(expanded_utterances_only),
	  conditionally_output_utterance(OutputStream,InputLabel,ExpandedInputText)
	; control_option(unexpanded_utterances_only),
	  conditionally_output_utterance(OutputStream,InputLabel,UNExpandedInputText)
	; % Do further processing
	  choose_utterance_list(PrincipalDomain, UNExpandedInputText, ExpandedInputText,
				ChosenInputText),
	  format(user_output, '~nProcessing ~a ~s~n', [InputLabel,UNExpandedInputText]),
	  conditionally_output_utterance(OutputStream, InputLabel, UNExpandedInputText),
   	  ( % Choose which process_one to call based on which program is running
	      control_option(write_syntax_only),
	      process_one_utterance_syntax(DebugBitVector,ServerStreams,
					   ChosenInputText,PrincipalDomain, AnatomyDomain,
					   InputLabel, OutputStream, Syntax, Definitions),
	      Syntax = minimal_syntax(RealSyntax),
	      mca_output_with_label(InputLabel,RealSyntax, OutputStream)
	  ;   %control_value(other_program, phrasex),
	      control_option(extract_phrases_only),
	      process_one_utterance_syntax(DebugBitVector,ServerStreams,
					   ChosenInputText, PrincipalDomain, AnatomyDomain,
					   InputLabel, OutputStream, Syntax, Definitions),
	      phrasex_output([utterance(InputLabel, ChosenInputText)],
				[parse(InputLabel, Syntax, Definitions)],
				OutputStream)
	  ;
%	      format(user_output,'PrecedingAnalysis: ~q~n',[PrecedingAnalysis]),
%	      format(user_output,'PrecedingVarInfoList: ~q~n',[PrecedingVarInfoList]),
	      process_one_utterance(ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
				    DebugBitVector,ChosenInputText,
				    OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
				    PrincipalDomain, AnatomyDomain,
				    ABGenes, InputLabel, OutputStream,AAs,UDAs,
				    WordDataCacheIn,USCCacheIn,RawTokensIn,
				    WordDataCacheNext,USCCacheNext,RawTokensNext,
				    PrecedingVarInfoList,PrecedingAnalysis,PrecedingConjList,
				    VarInfoList,SyntacticAnalysis, ConjList,
				    AnalysisWithSemTypes,AnalysisWithSemTypes1,
				    SemInterpSet),
	      write_sorted(OutputStream, Pos1, SyntacticAnalysis, PrecedingAnalysis, AnalysisWithSemTypes, 
			   OrigCitationTextAtomWithBlanks, InputLabel, UNExpandedInputText,
			   UtteranceType, PredicationInLen, SemInterpSet),
%	      append([AnalysisWithSemTypes1],PrecedingAnalysis,PrecedingAnalysisOut),
%	      append([VarInfoList],PrecedingVarInfoList,PrecedingVarInfoListOut),
	      append(PrecedingAnalysis,[AnalysisWithSemTypes1],PrecedingAnalysisOut),
	      append(PrecedingVarInfoList,[VarInfoList],PrecedingVarInfoListOut),
	      append(PrecedingConjList,[ConjList],PrecedingConjListOut),
	      length(SemInterpSet,PredicationLen),
	      PredicationOutLen is PredicationLen + PredicationInLen
	  ),
	  flush_output(OutputStream)
        ),
	process_input(RestUNExpandedUtterances, RestExpandedUtterances, RestUtteranceTypes,
		      ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
		      OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
		      DebugBitVector, AAs, UDAs, _TokensOut, ABGenes,
		      WordDataCacheNext,USCCacheNext,RawTokensNext,
		      WordDataCacheOut,USCCacheOut,RawTokensOut,
		      PrincipalDomain, AnatomyDomain, OutputStream,
		      PrecedingVarInfoListOut, PrecedingAnalysisOut, PrecedingConjListOut,
		      PredicationOutLen, RestPredicationSets).
process_input([utterance(InputLabel, _UNExpandedInputText,_Pos1,_Pos2)|RestUNExpandedUtterances],
	      [utterance(InputLabel, _ExpandedInputText,_Pos3,_Pos4)|RestExpandedUtterances],
	      [_UtteranceType|RestUtteranceTypes],
	      ServerStreams, %WSDServerHosts, WSDForced, WSDServerPost,
	      OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
	      DebugBitVector, AAs, UDAs, _TokensIn,ABGenes,
	      WordDataCacheIn,USCCacheIn,RawTokensIn,
	      WordDataCacheOut,USCCacheOut,RawTokensOut,
	      PrincipalDomain, AnatomyDomain, OutputStream,
	      PrecedingVarInfoList, PrecedingAnalysis, PrecedingConjList,
	      PredicationLen, RestPredicationSets) :-
        format(OutputStream, '#### ERROR: process_one_utterance failed on ~a~n',[InputLabel]),
%	append([[]],PrecedingAnalysis,PrecedingAnalysisOut),
%	append([[]],PrecedingVarInfoList,PrecedingVarInfoList),
	append(PrecedingAnalysis,[[]],PrecedingAnalysisOut),
	append(PrecedingVarInfoList,[[]],PrecedingVarInfoListOut),
	append(PrecedingConjList,[[]],PrecedingConjListOut),
	process_input(RestUNExpandedUtterances, RestExpandedUtterances, RestUtteranceTypes,
		      ServerStreams,
%		      WSDServerHosts, WSDForced, WSDServerPost,
		      OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
		      DebugBitVector, AAs, UDAs, _TokensOut, ABGenes,
		      WordDataCacheIn, USCCacheIn, RawTokensIn,
		      WordDataCacheOut,USCCacheOut,RawTokensOut,
		      PrincipalDomain, AnatomyDomain, OutputStream,
		      PrecedingVarInfoListOut, PrecedingAnalysisOut, PrecedingConjListOut,
		      PredicationLen, RestPredicationSets).

/*

process_one_utterance/11 is the heart of the program.
This predicate takes as input a list of ASCII and produces as output
a type of semantic structure called here a Conceptual Structure.
The data structures used by each of the predicates called by process_one_utterance
are described with examples at the end of the file.
*/
%	+DebugBitVector, +ChosenInputText,
%		+PrincipalDomain, +AnatomyDomain, +ABGenes, +InputLabel,
%		+OutputStream, -SyntacticAnalysis, -AnalysisWithSemTypes,
%		-Predications
process_one_utterance(ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
		      DebugBitVector, ChosenInputText,
		      OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,
		      PrincipalDomain, AnatomyDomain, ABGenes, InputLabel,
		      OutputStream, AAList, UDAs, WordDataCacheIn,USCCacheIn,RawTokensIn,
		      WordDataCacheOut,USCCacheOut,RawTokensOut,
		      PrecedingVarInfoList,PrecedingAnalysis, PrecedingConjList,
		      VarInfoList, SyntacticAnalysis, ConjList,
		      AnalysisWithSemTypes,AnalysisWithSemTypes1,Predications) :-
	ServerStreams = TaggerServerStream-_WSDServerStream,
	perform_syntactic_analysis(DebugBitVector, TaggerServerStream,
				   InputLabel, ChosenInputText,
				   OutputStream, TaggedText,Definitions,
				   VarInfoList, SyntacticAnalysis),
	perform_referential_analysis(ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
	                             DebugBitVector, ChosenInputText,
				     OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs, TaggedText,
				     Definitions, VarInfoList, PrincipalDomain, AnatomyDomain,
				     ABGenes, InputLabel, OutputStream,AAList,UDAs,
				     WordDataCacheIn,USCCacheIn,RawTokensIn,
				     WordDataCacheOut,USCCacheOut,RawTokensOut,
				     SyntacticAnalysis,PrecedingAnalysis,
				     LastIndex,LastMSUIndex,AnalysisWithSemTypes),
	maybe_atom_gc(_, _, _),
	perform_relational_analysis(DebugBitVector, ChosenInputText, PrincipalDomain,
				    AnatomyDomain, Definitions, LastIndex,LastMSUIndex,
				    VarInfoList, InputLabel, OutputStream,
				    PrecedingVarInfoList,PrecedingAnalysis, PrecedingConjList,
				    AnalysisWithSemTypes, AnalysisWithSemTypes1, ConjList,
				    Predications),
%	format('~nfound ~q~n~n', [Predications]),
        maybe_atom_gc(_, _, _),
	flush_output(OutputStream),
	flush_output(user_output).

process_one_utterance_syntax(DebugBitVector, ServerStreams, ChosenInputText,
	                     _PrincipalDomain, _AnatomyDomain,
			     InputLabel, OutputStream,
			     Syntax, Definitions) :-
%	perform_pre_linguistic_analysis(DebugBitVector, ChosenInputText,
%	        InputLabel, OutputStream, TaggedText, Words),
%	perform_categorial_analysis(DebugBitVector, InputLabel, ChosenInputText,
%					   OutputStream, Words, TaggedText,
%					   Definitions, Syntax, _VarInfoList),
	ServerStreams = TaggerServerStream-_WSDServerStream,
	perform_syntactic_analysis(DebugBitVector, TaggerServerStream,
				   InputLabel, ChosenInputText,
				   OutputStream, _TaggedText, Definitions,
				   _VarInfoList, Syntax), 
	maybe_atom_gc(_, _, _),
	flush_output(OutputStream),
	flush_output(user_output).


perform_syntactic_analysis(_DebugBitVector, ServerStreams,
			   InputLabel, InputText,
			   OutputStream, TaggedText, Definitions, VarInfoList, AnalysisWithBases) :-
	tag_text(InputText, ServerStreams, _FullTagList, TagList, TaggedText),
	generate_syntactic_analysis_plus(InputText, TagList, SyntAnalysis0, Definitions),
	generate_variant_info_wrapper(Definitions, InputLabel, InputText, OutputStream, VarInfoList),
	get_all_bases_wrapper(InputText, InputLabel, OutputStream, SyntAnalysis0,
			      Definitions, AnalysisWithBases).

% +DebugBitVector, +InputText, +CitationTextAtom
%	_Definitions, +PrincipalDomain, +AnatomyDomain, +ABGenes, +InputLabel,
%	+OutputStream, minimal_syntax(+SyntacticAnalysis),
%	-AnalysisWithMetaConcs
perform_referential_analysis(ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
			     DebugBitVector, InputText,
			     OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs, TagList,
			     Definitions, VarInfoList, PrincipalDomain, AnatomyDomain,
			     ABGenes, InputLabel, OutputStream,AAs,UDAs,
			     WordDataCacheIn,USCCacheIn,RawTokensIn,
			     WordDataCacheOut,USCCacheOut,RawTokensOut,
			     minimal_syntax(SyntacticAnalysis),
			     PrecedingAnalysis,LastIndex,LastMSUIndex,AnalysisWithMetaConcs5) :-
	remove_all_bases_for_metamap(SyntacticAnalysis,SyntacticAnalysisWOBases),
	debug_call(DebugBitVector, 3,
		   format('~nMETAMAP in...~n', [])),
	get_metamap_analysis(ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
			     SyntacticAnalysisWOBases, InputText, InputLabel,
	                     OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs, TagList,
			     OutputStream, AAs,UDAs,
			     WordDataCacheIn,USCCacheIn,RawTokensIn,
			     WordDataCacheOut,USCCacheOut,RawTokensOut,
	                     AnalysisWithMetaConcs0),
	debug_call(DebugBitVector, 3,
		   format('~nMETAMAP out...~n', [])),
	get_all_bases_wrapper(InputText, InputLabel, OutputStream, AnalysisWithMetaConcs0,
			      Definitions, AnalysisWithBases),
        AnalysisWithBases = minimal_syntax(AnalysisWithMetaConcs1),
	get_semtypes_wrapper(AnalysisWithMetaConcs1, InputText, InputLabel, OutputStream,
			     AnalysisWithSemTypes2, PrincipalDomain),
	get_last_indexes(PrecedingAnalysis,LastIndex,LastMSUIndex),
	maybe_insert_domain_concepts(AnalysisWithSemTypes2, SyntacticAnalysis,
				     AnalysisWithSemTypes12, SyntacticAnalysis1),
	add_indexes_to_all_MSUs(AnalysisWithSemTypes12, LastIndex, AnalysisWithSemTypes),
        identify_genetic_concepts_wrapper(InputLabel, InputText, OutputStream, PrincipalDomain, 
					  AnalysisWithSemTypes, ABGenes, AnalysisWithGenetics),
	perform_domain_processing_wrapper(InputLabel, InputText, OrigCitationTextAtomWithBlanks,
					  OutputStream,
	                                  PrincipalDomain, AnatomyDomain, 
	                                  SyntacticAnalysis1, 
				          AnalysisWithGenetics, AnalysisWithMetaConcs3),
	remove_referential_analysis_if_necessary(AnalysisWithMetaConcs3, AnalysisWithMetaConcs4),
	( control_option(anaphora_resolution) ->
	  identify_sortal_anaphora(AnalysisWithMetaConcs4,VarInfoList,AnalysisWithMetaConcs5)
	; AnalysisWithMetaConcs5 = AnalysisWithMetaConcs4
	).

update_negation_status(AnalysisIn,_PredicationList,ConjList,AnalysisOut) :-
%	update_negation_status_from_predications(AnalysisIn,AnalysisIn,PredicationList,AnalysisOut0),
	update_negation_status_from_conj(AnalysisIn,ConjList,AnalysisOut).

%update_negation_status_from_predications([],_AnalysisIn,_PredicationList,[]) :- !.
%update_negation_status_from_predications([ThisMSU|RestMSUs],AnalysisIn,PredicationList,
%					 [UpdatedMSU|RestUpdatedMSUs]) :-
%	update_specific_MSU(ThisMSU,AnalysisIn,PredicationList,UpdatedMSU),
%	!,
%	update_negation_status_from_predications(RestMSUs,AnalysisIn,PredicationList,RestUpdatedMSUs).
%update_negation_status_from_predications([ThisMSU|RestMSUs],AnalysisIn,PredicationList,[ThisMSU|RestUpdatedMSUs]) :-
%	update_negation_status_predications(RestMSUs,AnalysisIn,PredicationList,RestUpdatedMSUs).

%update_specific_MSU(ThisMSU,_AnalysisIn,[],ThisMSU).
%update_specific_MSU(ThisMSU,AnalysisIn,[Predication|RestPredications],UpdatedMSU):-
%	specific_in_MSU(ThisMSU,Predication,Specific),
%	general_negated(AnalysisIn,Predication),
%	update_MSU_aux(ThisMSU,Specific,RestPredications,UpdatedMSU),
%	!.

update_negation_status_from_conj([],_ConjList,[]) :- !.
update_negation_status_from_conj([ThisMSU|RestMSUs],ConjList,[UpdatedMSU|RestUpdatedMSUs]) :-
	update_conj_MSU(ThisMSU,ConjList,UpdatedMSU),
	!,
	update_negation_status_from_conj(RestMSUs,ConjList,RestUpdatedMSUs).
update_negation_status_from_conj([ThisMSU|RestMSUs],ConjList,[ThisMSU|RestUpdatedMSUs]) :-
	update_negation_status_from_conj(RestMSUs,ConjList,RestUpdatedMSUs).

update_conj_MSU(ThisMSU,[],ThisMSU).
update_conj_MSU(ThisMSU,[Conj|_RestConjList],UpdatedMSU):-
	update_conj_MSU_aux(ThisMSU,Conj,UpdatedMSU),
	!.
update_conj_MSU(ThisMSU,[_Conj|RestConjList],UpdatedMSU):-
	update_conj_MSU(ThisMSU,RestConjList,UpdatedMSU).

update_conj_MSU_aux([],_Conj,[]).
update_conj_MSU_aux([MSUItem|RestMSU],Conj,[UpdatedMSUItem|RestUpdatedMSU]) :-
	Conj = coord(_Coord,_Type,_Index,LConjList,_RConj,_),
	neg_conj_index(LConjList, Index),
%	is_neg_conj(Conj),
	functor(MSUItem,Functor,1),
	arg(1,MSUItem,MSUItemList),
	coordinate_msu_item(MSUItemList,Index,Conj),
	!,
	update_negation(MSUItemList,UpdatedMSUItemList),
	functor(UpdatedMSUItem,Functor,1),
	arg(1,UpdatedMSUItem,UpdatedMSUItemList),
	update_conj_MSU_aux(RestMSU,Conj,RestUpdatedMSU).
update_conj_MSU_aux([MSUItem|RestMSU],Conj,[MSUItem|RestUpdatedMSU]) :-
	update_conj_MSU_aux(RestMSU,Conj,RestUpdatedMSU).

update_negation(ItemList,ItemListOut) :-		      
	( \+ memberchk(negated(_),ItemList) ->
	  append(ItemList,[negated(1)],ItemListOut)
	; memberchk(negated(0),ItemList) ->
	  delete(ItemList,negated(0),ItemList0),
	  append(ItemList0,[negated(1)],ItemListOut)
	),
	!.
update_negation(ItemList,ItemList).

neg_conj_index([ConjElement|_RestConjElements],Index) :-
	memberchk(negated(1),ConjElement),
	!,
	memberchk(index(Index),ConjElement).
neg_conj_index([_ConjElement|RestConjElements],Index) :-
	neg_conj_index(RestConjElements,Index).

coordinate_msu_item(MSUItemList,_FirstNegIndex,coord(_Coord,_Type,_Index,_LConjList,MSUItemList,_)).
coordinate_msu_item(MSUItemList,FirstNegIndex0,coord(_Coord,_Type,_Index,[MSUItemList|_RestLConjList],_RConj,_)) :-
	get_from_list(index,MSUItemList,MSUIndex0),
	( MSUIndex0 = MSUIndex:_Index0/MSUIndex ->
	   true
	; MSUIndex0 = MSUIndex
	),
	( FirstNegIndex0 = FirstNegIndex:_Index1/FirstNegIndex ->
	   true
	; FirstNegIndex0 = FirstNegIndex
	),
	FirstNegIndex =< MSUIndex.
coordinate_msu_item(MSUItemList,FirstNegIndex,coord(_Coord,_Type,_Index,[_LConjList|RestLConjList],RConj,_)) :-
	coordinate_msu_item(MSUItemList,FirstNegIndex,coord(_Coord,_Type,_Index,RestLConjList,RConj,_)).

update_conjlist_negation_status([],_Analysis,[]) :- !.
update_conjlist_negation_status([Conj|RestConjList],Analysis,[OutConj|RestOutConjList]):-
	Conj = coord(Coord,Type,Index,LConjList,RConj,A),
	update_conj_element(RConj,Analysis,RConjUpdated),
	update_conj_elements(LConjList,Analysis,LConjListUpdated),
	!,
	OutConj = coord(Coord,Type,Index,LConjListUpdated,RConjUpdated,A),
	update_conjlist_negation_status(RestConjList,Analysis,RestOutConjList).
update_conjlist_negation_status([_Conj|RestConjList],Analysis,RestOutConjList) :-
	update_conjlist_negation_status(RestConjList,Analysis,RestOutConjList).

update_conj_elements([],_Analysis,[]):- !.
update_conj_elements([Item|Rest],Analysis,[UpdatedItem|RestUpdated]) :-
	update_conj_element(Item,Analysis,UpdatedItem),
	!,
	update_conj_elements(Rest,Analysis,RestUpdated).

update_conj_element(Item,[MSU|_RestMSU],UpdatedItem) :-
	update_conj_element_aux(Item,MSU,UpdatedItem).
update_conj_element(Item,[_MSU|RestMSU],UpdatedItem) :-
	update_conj_element(Item,RestMSU,UpdatedItem).

update_conj_element_aux(Item,[MSUItem|_RestMSU],MSUItemList) :-
	functor(MSUItem,_Functor,1),
	arg(1,MSUItem,MSUItemList),
	get_from_list(index,MSUItemList, Index),
	get_from_list(index,Item,Index),
	!.
update_conj_element_aux(Item,[_MSUItem|RestMSU],UpdatedItem) :-
	update_conj_element_aux(Item,RestMSU,UpdatedItem).

	
% perform_relational_analysis creates several difference lists,
% which are all unified as follows:
% Predications/PredicationsTail
%              SemSpecAnalysis/SemSpecAnalysisTail
%                                    SemInterpHead/SemInterpTail
%                                                  InferenceHead/InferenceTail
%                                                                           []
% Thus the set of all predications is in Predications.
perform_relational_analysis(DebugBitVector, InputText, PrincipalDomain,
			    _AnatomyDomain, Definitions,_LastIndex,LastMSUIndex,
			    VarInfoList, InputLabel, OutputStream,
			    PrecedingVarInfoList, PrecedingAnalysis, PrecedingConjList,
			    AnalysisWithSemTypes, AnalysisWithSemTypes2,ConjList,
			    Predications) :-
	AnalysisWithSemTypes=minimal_syntax(AnalysisWithSemTypesCore),
	debug_call(DebugBitVector, 1,
		   format('~n### Before intermediate analysis:~n', [])),
	debug_call(DebugBitVector, 1,
		   portray_minimal_syntax_structure(AnalysisWithSemTypesCore)),
	% -ConjunctList
	intermediate_analysis_wrapper(AnalysisWithSemTypesCore, InputText, InputLabel,
				      OutputStream, Definitions, PrincipalDomain,
				      Predications, PredicationsTail,
				      AdjustedAnalysis, ConjList0, SubordinatorPredicateList),
	debug_call(DebugBitVector, 1,
		   format('~n~*c~n', [80,35])),
	debug_call(DebugBitVector, 1,
		   format('~n~*c~n', [80,35])),
	debug_call(DebugBitVector, 1,
		   format('~n### After intermediate analysis:~n', [])),
	debug_call(DebugBitVector, 1,
		   portray_minimal_syntax_structure(AdjustedAnalysis)),
	% format(user_output,'ConjList: ~q~n',[ConjList0]),
	% LastIndex and LastMSUIndex are computed earlier in perform_referential_analysis/25,
	% so there's no reason to re-compute them!
	% get_last_indexes(PrecedingAnalysis,_LastIndex,LastMSUIndex),
        add_msu_index(AdjustedAnalysis, LastMSUIndex, AnalysisWithSemTypes1),	


	% +ConjunctList
	semantic_specification_wrapper(InputText, InputLabel, OutputStream,
				       PrincipalDomain, minimal_syntax(AdjustedAnalysis),
				       Definitions, VarInfoList, ConjList0,
				       SemSpecAnalysis, SemSpecAnalysisTail, PrincipalDomain),
	% Predications holds the Comparative predications in the first difference list (DL1)
	% whose tail is ComparativePredicationsTail.
	% SemSpecAnalysis holds the hypernymic predications in the second difference list (DL2)
	% whose tail is SemSpecAnalysisTail.
	% Unify the tail of DL1 with the head of DL2.
	% ( control_option(anaphora_resolution) ->
	%   append(CoreferenceOut,SemSpecAnalysis,PredicationsTail)
	%  ; SemSpecAnalysis = PredicationsTail
	%  ),
	 ( control_option(anaphora_resolution) ->
	   anaphora_resolution_wrapper(InputText, InputLabel, OutputStream,
				       PrecedingAnalysis, PrecedingVarInfoList, PrecedingConjList,
				       AnalysisWithSemTypes1, VarInfoList, ConjList0,
				       SemSpecAnalysis, [], CoreferenceOut),
%	  format(user_output,'COREFERENCE: ~q~n', [CoreferenceOut]),
	  append(CoreferenceOut,SemSpecAnalysis,PredicationsTail)
	; SemSpecAnalysis = PredicationsTail
	 ),

%	format('~n~nBeforeNegUpdate: ~w~n~n', [AnalysisWithSemTypes1]),
	update_negation_status(AnalysisWithSemTypes1,PredicationsTail,ConjList0,AnalysisWithSemTypes2),
	update_conjlist_negation_status(ConjList0,AnalysisWithSemTypes2,ConjList),
%	format('~n~n AfterNegUpdate: ~w~n~n', [AnalysisWithSemTypes2]),
	
	% portray_minimal_syntax_structure(AnalysisWithSemTypes1),
	semantic_interpretation_wrapper(DebugBitVector,
					PrincipalDomain, Definitions,
					InputText, InputLabel,
					OutputStream,
					VarInfoList, AnalysisWithSemTypes2,
					PrecedingVarInfoList,PrecedingAnalysis, PrecedingConjList,
					ConjList,SubordinatorPredicateList,PredicationsTail,
					SemInterpHead, SemInterpTail), 
	% SemInterpHead holds the seminterp predications in the third difference list (DL3)
	% whose tail is SemInterpTail.
	% Unify the tail of DL2 with the head of DL3.
	SemSpecAnalysisTail = SemInterpHead,
	% Pass all the predications (in SemInterpHead) into draw_inferences_wrapper/6,
	% and return in InferenceHead all the predications generated via inference.
 	draw_inferences_wrapper(PrincipalDomain, DebugBitVector,
				InputLabel, InputText, OutputStream,
				Predications, InferenceHead, InferenceTail),



	
	SemInterpTail = InferenceHead,
	InferenceTail = [].

% MMOStream isn't needed; if we were reading from an .mmo,
%		it would already be in MMOUtterance
%	+SyntacticAnalysis = minimal_syntax(Analysis)
%	+InputText, +InputLabel
%	+OutputStream: used for printing error messages
% -AnalysisWithMetaConcs = minimal_syntax(AWMC)
% the predicate skr_phrase does most of the work.
get_metamap_analysis(ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
	             SyntacticAnalysis, InputText, InputLabel, 
	             OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,TagList,
		     OutputStream, AAs,UDAs,
		     WordDataCacheIn,USCCacheIn,RawTokensIn,
	             WordDataCacheOut,USCCacheOut,RawTokensOut,AnalysisWithMetaConcs) :-
	% Call MetaMap
	metamap_phrases_wrapper(ServerStreams, %WSDServerHosts, WSDForced, WSDServerPort,
				SyntacticAnalysis, InputText, InputLabel, 
	                        OrigCitationTextAtomWithBlanks, CitationTextAtomWithCRs,TagList,
				OutputStream, AAs, UDAs,
				WordDataCacheIn, USCCacheIn, RawTokensIn, RawTokensOut,
				WordDataCacheOut, USCCacheOut, AnalysisWithMetaConcs).
	

remove_all_bases_for_metamap([],[]) :- !.
remove_all_bases_for_metamap([ThisMSU|RestMSUs],[ThisModifiedMSU|RestModifiedMSUs]):-
	remove_all_bases_from_msu(ThisMSU,ThisModifiedMSU),
	remove_all_bases_for_metamap(RestMSUs,RestModifiedMSUs).

remove_all_bases_from_msu([],[]) :- !.
remove_all_bases_from_msu([ThisElement|RestElement],[ModifiedElement|RestModifiedElement]) :-
	functor(ThisElement,Functor,1),
	arg(1,ThisElement,ArgList),
	update_args(ArgList,NewArgList),
	!,
	functor(ModifiedElement,Functor,1),
	arg(1,ModifiedElement,NewArgList),
	remove_all_bases_from_msu(RestElement,RestModifiedElement).
remove_all_bases_from_msu([_ThisElement|RestElement],[_ModifiedElement|RestModifiedElement]) :-
	remove_all_bases_from_msu(RestElement,RestModifiedElement).


update_args([],[]) :- !.
update_args([bases(_)|Rest],RestOut) :-
	update_args(Rest,RestOut).
update_args([Element|Rest],[Element|RestOut]) :-
	update_args(Rest,RestOut).


verify_SemRep_output_options :-
	( control_option(full_fielded_output_format),
	  control_option(xml_output_format) ->
	  format(user_error, '\n\n###################################################################\n', []),
	  format(user_error, 'WARNING: full_fielded_output_format (-F) and xml_output_format (-X)\n', []),

	  format(user_error, '         cannot both be used; using xml_output_format ONLY.\n', []),
	  format(user_error, '###################################################################\n\n', []),
	  retract(nls_system:control_option(full_fielded_output_format))
	; true
	).

% The first call to conditionally_get_time is made with '?' as first arg,
% which will test control_option(time) to see if calls to statistics should be made.
% If so,
%   * NewTime will be instantiated to an integer representing the time, and
%   * TimeTest will be instantiated to 1.
% If not,
%   * NewTime will be left uninstantiated, and
%   * TimeTest will be instantiated to 0.
% This is simply an efficiency hack, because unifying on the first argument
% is faster than calling the dynamic prediate control_option/1.
% Subsequent calls to conditionally_get_time/3 are then made with the first
% argument of either 0 (i.e., do nothing) or 1 (i.e., call statistics/1),
% and no further calls to the dynamic control_option(time) are made.

/*

************************************************************************************
                Description of the data structures used in process_one_utterance/10
************************************************************************************


-----tokenize_string( ListOfAscii, Words ),
Takes a list of ASCII as input and produces a list of atoms.

	ListOfAscii = A Prolog string.

		"Use of thermogram in detection of meningitis."


	Words = A list of lists of atoms. Each list of atoms is either a potential lexical 
		 entry and thus does not contain any punctuation other than hyphen, or is
		 a list containing punctuation other than hyphen.

		[[use,of,thermogram,in,detection,of,meningitis],['.']]


-----assembledefns( Words, Definitions ),
Lexical look-up.

	Definitions = A list of colon-structures representing the entry from the Specialist
		       Lexicon for each lexical entry in Words.  Note that a lexical entry
		       may contain more than one text word. Each colon-structure is an atom 
                       followed by a colon followed by a list of either atoms or 
                       colon-structures.

		[lexicon:[lexmatch:[use],inputmatch:[use],
			  records:[lexrec:[base:[use],spelling_variants:[],
			  entries:[entry:[num:[1],
				          cat:[noun],
				   	  variants:[uncount],
				     	  complements:[pphr:[prep:[of],obj:[np]],
						       pphr:[prep:[by],obj:[np]]],
					  nominalizations_of:[use],proper:[],misc:[]],
				   entry:[num:[2],cat:[verb],...]]],
		 lexicon:[lexmatch:[of],...],
		 lexicon:[lexmatch:[thermogram],...],
		 lexicon:[lexmatch:[in],...],
		 lexicon:[lexmatch:[detection],...],
		 lexicon:[lexmatch:[of],...],
		 lexicon:[lexmatch:[meningitis],...],
		 punctuation:[lexmatch:['.'],...] 
		]


-----generate_variant_info( Definitions, VarInfoList ),
Retrieve category labels and morphological information.

	VarInfoList = A list of colon structures for each lexical entry and punctuation..

		[ use:[noun:[base],verb:[base]],
		  of:[prep:[base]],thermogram:[noun:[base]],
		  in:[adv:[base],prep:[base]],
		  detection:[noun:[base]],
		  of:[prep:[base]],
		  meningitis:[noun:[base]],
		  punctuation:[lexmatch:['.'],inputmatch:['.'],records:[punct:[period]]] 
		]

-----consult_tagged_text( Definitions, VarInfoList, TaggedTextList, LabeledText, 1 ),
Resolve category label ambiguity by consulting the output from the stochastic tagger

        TaggedTextList = [[use,noun],[of,prep],[thermogram,noun],[in,prep],
                          [detection,noun],[of,prep],[meningitis,noun]]


-----minimal_commitment_analysis( Definitions, VarInfoList, LabeledText,SyntAnalysis ),
Underspecified syntactic analysis.

        LabeledText = a list of input tokens labeled by the Stochastic Tagger

                [ noun(use), prep(of), noun(thermogram), prep(in), prep(detection),
                  prep(of), noun(meningitis), punk('.')
                ]

1	SyntAnalysis = A structure whose principal functor is the atom 'minimal_syntax'
		       and whose argument is a list of lists. Each inner list is a
		       minimal syntactic unit containing labelled components.

		minimal_syntax([[head(use)],
				[prep(of),head(thermogram)],
				[prep(in),head(detection)],
				[prep(of),head(meningitis)] ] )


-----metamap_phrases( SyntAnalysis, AnalysisWithMetaConcs ),
Lookup phrases in the UMLS Metathesaurus.

	AnalysisWithMetaConcs = SyntAnalysis augmented with Metathesaurus concepts and 
				a confidence value.

		minimal_syntax([
			[head([tokens([use])])],
			[prep([tokens([of])]),head([metaconc(["Thermography"]),
						    tokens([thermogram])]),
			 confid(92)],
			[prep([tokens([in])]),head([metaconc(["detection"]),
						    tokens([detection])]),
			 confid(100)],
			[prep([tokens([of])]),head([metaconc(["Meningitis"]),
						    tokens([meningitis])]),
			 confid(100)]])


-----get_semtypes( AnalysisWithMetaConcs, AnalysisWithSemTypes ),
Lookup UMLS semantic types for each Metathesaurus concept.

	AnalysisWithSemTypes = AnalysisWithMetaConcs augmented with semantic types. 
			       Note: usemtypes are UMLS semantic types; isemtypes
			       are generalized semantic types assigned by the program.

		minimal_syntax([
			[head([tokens([use]),usemtype(['None']),isemtype('None')])],
			[prep([tokens([of])]),
			 head([metaconc(["Thermography"]),
			       tokens([thermogram]),
			       usemtype(['Diagnostic Procedure']),
			       isemtype(diagnosis)]),
			 confid(92)],
		        [prep([tokens([in])]),
			 head([metaconc(["detection"]),
			       tokens([detection]),
			       usemtype(['Therapeutic or Preventive Procedure']),
			       isemtype(therapy)]),
			 confid(100)],
			[prep([tokens([of])]),
			 head([metaconc(["Meningitis"]),
			       tokens([meningitis]),
			       usemtype(['Disease or Syndrome','Finding']),
			       isemtype(disorder)]),
			 confid(100)]])


-----intermediate_analysis( AnalysisWithSemTypes, IntermedAnalysis, ConjunctList ),
Further linguistic analysis combining syntactic and semantic information.

	IntermedAnalysis = AnalysisWithSemTypes augmented with an index for each
			  lexical entry.  The index is used in processing 
			  coordinate structures.

		minimal_syntax([
			[head([index(1),tokens([use]),usemtype(['None']),isemtype('None')])],
			[prep([index(2),tokens([of])]),
			 head([index(3),metaconc(["Thermography"]),
					tokens([thermogram]),
					usemtype(['Diagnostic Procedure']),
					isemtype(diagnosis)]),
			 confid(92)],
			[prep([index(4),tokens([in])]),
			 head([index(5),metaconc(["detection"]),
					tokens([detection]),
					usemtype(['Therapeutic or Preventive Procedure']),
					isemtype(therapy)]),
			 confid(100)],
			[prep([index(6),tokens([of])]),
			 head([index(7),metaconc(["Meningitis"]),
					tokens([meningitis]),
					usemtype(['Disease or Syndrome','Finding']),
					isemtype(disorder)]),
			 confid(100)] ])
	
	ConjunctList = A list of coordinate structures.


-----semantic_interpretation( IntermedAnalysis, ConjunctList, ConceptualStructure, _ ),
Construct semantic predications.

	ConceptualStructure = A structure with principal functor 'np_interp' and one
			      argument, which is a list of structures whose principal
			      functor is 'concstruc' (for conceptual structure). Each
			      'construc' has a list argument containing a semantic 
			      predication encoded as a structure whose principal 
			      functor is the name of the predication ( 'detection' in
			      this example).  The argument of this structure is a list
			      containing the semantic roles of the predication; each is
			      encoded as a structure whose functor is the name of the role 
			      and whose argument is a list containing syntactic and semantic
			      information (from IntermedAnalysis).

		np_interp([
		    concstruc([
			detection([
			    theme([head([index(7),
					 metaconc(["Meningitis"]),
					 tokens([meningitis]),
					 usemtype(['Disease or Syndrome','Finding']),
					 isemtype(disorder)]),
 				   confid(100)]),
			    instr([head([index(3),
					 metaconc(["Thermography"]),
					 tokens([thermogram]),
					 usemtype(['Diagnostic Procedure']),
					 isemtype(diagnosis)]),
				   confid(92)]) ]) ]) ])


-----show_analysis( ConceptualStructure ).


*/
