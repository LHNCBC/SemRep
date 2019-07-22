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

% File:	    nls_system.pl
% Module:   NLS System
% Author:   Lan
% Purpose:  Provide various system-related predicates.


:- module(nls_system, [
	args/0,
	% called by MetaMap API -- do not change signature!
	add_to_control_options/1,
	add_to_current_control_options/3,
	% assert_control_option/1,
	assert_control_value/2,
	control_option/1,
	control_value/2,
	display_control_options_for_modules/2,
	display_current_control_options/2,
	display_mandatory_metamap_options/0,
	% called by MetaMap API -- do not change signature!
	get_control_options_for_modules/2,
	get_from_iargs/4,
	get_program_name/1,
	% called by MetaMap API -- do not change signature!
	interpret_args/4,
	option_requires_list_arg/1,
	% called by MetaMap API -- do not change signature!
	interpret_options/4,
	% called by MetaMap API -- do not change signature!
	parse_command_line/1,
	% called by MetaMap API -- do not change signature!
	parse_command_line/3,		      
	pa/0,
	pwd/0,
	pwd/1,
	% called by MetaMap API -- do not change signature!
	reset_control_options/1,
	% called by MetaMap API -- do not change signature!
	set_control_options/1,
	% called by MetaMap API -- do not change signature!
	set_control_values/2,
	% called by MetaMap API -- do not change signature!
	subtract_from_control_options/1,
	% called by MetaMap API -- do not change signature!
	toggle_control_options/1,
	update_command_line/5,
	verify_options_and_values/0
    ]).


:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	number_codes_list/2,
	concatenate_items_to_atom/2,
	safe_number_codes/2,
	split_atom_completely/3,
	split_string_completely/3
    ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2,
	send_message/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	can_open_file/2,
	midstring/4,
	midstring/5
    ]).

:- load_files([library(system),
	       skr_lib(sicstus_utils)], [
	when(always)
    ]).

:- use_module(library(system), [
	environ/2
   ]).


:- use_module(library(lists), [
	nth1/3,
	rev/2
    ]).

% called by MetaMap API -- do not change signature!
:- dynamic control_option/1.
:- dynamic control_value/2.
% :- dynamic control_value/3.

get_program_name(ProgramName) :-
	( current_module(usemrep) ->
	  ProgramName = usemrep
	; current_module(M),
	  is_control_option(M, _, _, _, _) ->
	  ProgramName = M
	; ProgramName = metamap
	).

% get_program_name(ProgramName) :-
% 	environ('SP_APP_PATH', AbsolutePathName),
% 	atom_codes(AbsolutePathName, AbsolutePathNameCodes),
% 	basename(AbsolutePathNameCodes, ProgramNameCodes),
% 	atom_codes(ProgramName, ProgramNameCodes).

option_requires_list_arg(OptionName) :-
	is_control_option(_, _, OptionName, _,
			  aspec(OptionName, _, list, _, _, _)),
	!.

/* is_control_option(?Module, ?ShortControlOption, ?ControlOption, ?IsDefault, ?ArgSpec)

is_control_option/5 is a factual predicate of legal ControlOptions (and their
single-character ShortControlOptions) for each Module.  It also indicates
which options are defaults (IsDefault) and which options take an argument
(ArgSpec, none if none).  */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% MetaMap %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_control_option(metamap, '+', bracketed_output,		no, none).
is_control_option(metamap, '8', dynamic_variant_generation, 	no, none).
is_control_option(metamap, '@', 'WSD_SERVER', no,
                  aspec('WSD_SERVER', mandatory, none, none, no_default,
                        'Which WSD server to use')).
is_control_option(metamap, 'A', strict_model, 			no, none).
% is_control_option(metamap, 'B', moderate_model, 		no, none).
is_control_option(metamap, 'C', relaxed_model, 			no, none).
is_control_option(metamap, 'D', all_derivational_variants,      no, none).
is_control_option(metamap, 'E', indicate_citation_end,   	no, none).
is_control_option(metamap, 'F', formal_tagger_output, 		no, none).
is_control_option(metamap, 'G', sources,                 	no, none).
% is_control_option(metamap, 'H', display_original_phrases, 	no, none).
is_control_option(metamap, 'I', show_cuis, 		 	no, none).
is_control_option(metamap, 'J', restrict_to_sts, no,
                  aspec(restrict_to_sts, mandatory, list, none, no_default,
                        'List of semantic types to use for output')).
is_control_option(metamap, 'K', ignore_stop_phrases, 		no, none).
is_control_option(metamap, 'L', lexicon_year, 	 		no,
                  aspec(lexicon_year, mandatory, none, none, no_default,
                       'Lexicon year')).
% is_control_option(metamap, 'M', mmi_output,              no, none).
is_control_option(metamap, 'N', fielded_mmi_output,      	no, none).
% is_control_option(metamap, 'O', show_preferred_names_only, 	no, none).
is_control_option(metamap, 'Q', composite_phrases, 		yes,
                  aspec(composite_phrases, mandatory, integer, yes, 4,
                        'Max number of prepositional phrases to glom on')).	% MetaMap default
is_control_option(metamap, 'R', restrict_to_sources, no,
                  aspec(restrict_to_sources, mandatory, list, none, no_default,
                        'List of sources to use for output')).

is_control_option(metamap, 'S', 'TAGGER_SERVER', no,
                  aspec('TAGGER_SERVER', mandatory, none, none, no_default,
                        'Which tagger server to use')).
is_control_option(metamap, 'T', tagger_output, 			no, none).
% is_control_option(metamap, 'U', allow_duplicate_concept_names,  no, none).
is_control_option(metamap, 'V', mm_data_version, no,
                  aspec(mm_data_version, mandatory, none, none, no_default,
                        'Version of MetaMap data to use')).
% is_control_option(metamap, 'W', preferred_name_sources,  	no, none).
% is_control_option(metamap, 'X', truncate_candidates_mappings, 	no, none).
is_control_option(metamap, 'Y', prefer_multiple_concepts, 	no, none).
is_control_option(metamap, 'Z', mm_data_year, no,
                  aspec(mm_data_year,mandatory, none, none, no_default,
                        'Release of MetaMap data to use')).

is_control_option(metamap,   a, all_acros_abbrs,	        no, none).
is_control_option(metamap,   b, compute_all_mappings,    	no, none).
is_control_option(metamap,   c, show_candidates,	        no, none).
is_control_option(metamap,   d, no_derivational_variants,	no, none).
is_control_option(metamap,   e, exclude_sources, 		no,
                  aspec(exclude_sources, mandatory, list, none, no_default,
                        'List of sources to exclude for output')).
is_control_option(metamap,   f, number_the_mappings, 		no, none).
is_control_option(metamap,   g, allow_concept_gaps, 		no, none).
% is_control_option(metamap,   f, fielded_output, 		no, none).
is_control_option(metamap,   i, ignore_word_order, 		no, none).
is_control_option(metamap,   j, dump_aas,                	no, none).
is_control_option(metamap,   k, exclude_sts, no,
                  aspec(exclude_sts, mandatory, list, none, no_default,
                        'List of semantic types to exclude for output')).
is_control_option(metamap,   l, allow_large_n,		        no, none).
is_control_option(metamap,   m, hide_mappings,	         	no, none).
is_control_option(metamap,   n, number_the_candidates, 	      	no, none).
is_control_option(metamap,   o, allow_overmatches, 		no, none).
is_control_option(metamap,   q, machine_output, 		no, none).
is_control_option(metamap,   p, hide_plain_syntax,	        no, none).
is_control_option(metamap,   r, threshold, no,
                  aspec(threshold, mandatory, integer, none, no_default,
                        'Threshold for displaying candidates')).
is_control_option(metamap,   s, short_semantic_types,	 	no, none).
is_control_option(metamap,   t, no_tagging,		        no, none).
is_control_option(metamap,   u, unique_acros_abbrs_only, 	no, none).
is_control_option(metamap,   v, variants, 			no, none).
is_control_option(metamap,   x, syntax, 			no, none).
is_control_option(metamap,   y, word_sense_disambiguation, 	no, none).
is_control_option(metamap,   z, term_processing, 		no, none).

is_control_option(metamap,  '', debug, 			 	no,
                  aspec(debug, mandatory, list, none, no_default, 'Debugging settings')).
is_control_option(metamap,  '', help, 		 	 	no, none).
is_control_option(metamap,  '', tokenize_only, 		 	no, none).
is_control_option(metamap,  '', negex,		 	 	no, none).
is_control_option(metamap,  '', negex_st_add, 		 	no,
                  aspec(negex_st_add, mandatory, list, none, no_default, 'SemTypes to add to NegEx')).
is_control_option(metamap,  '', negex_st_del, 		 	no,
                  aspec(negex_st_del, mandatory, list, none, no_default, 'SemTypes to delete from to NegEx')).
is_control_option(metamap,  '', negex_st_set, 		 	no,
                  aspec(negex_st_set, mandatory, list, none, no_default, 'SemTypes to set for NegEx')).

is_control_option(metamap,  '', blanklines, 		 	no,
                  aspec(blanklines, mandatory, integer, none, no_default,
			'Number of newlines to read to signal end of citation')).
% is_control_option(metamap, '', 'LEXICON_SERVER', no,
%                   aspec('LEXICON_SERVER', mandatory, none, none, no_default,
%                         'Which lexicon server to use')).
		  
is_control_option(metamap,  '', silent,		 	 	no, none).
is_control_option(metamap,  '', aas_only,                	no, none).
% is_control_option(metamap,  '', max_ambiguity,	 	 no,
%                   aspec(max_ambiguity, mandatory, integer, none, 0,
%                         'Maximum allowable degree of ambiguity')).
% is_control_option(metamap,  '', min_conc_length, 	 no,
%                   aspec(min_concn_length, mandatory, integer, none, no_default,
%                         'Must specify an integer value.')).
is_control_option(metamap,  '', min_length, 	 no,
                  aspec(min_length, mandatory, integer, none, no_default,
                        'Minimum length of concept name')).
is_control_option(metamap,  '', phrases_only, 	 	 	no, none).
% is_control_option(metamap,  '', apostrophe_s_contraction, 	no, none).
is_control_option(metamap,  '', warnings, 	 	 	no, none).
is_control_option(metamap,  '', mappings_limit, no,
                  aspec(mappings_limit, mandatory, integer, none, no_default,
                        'Max number of mappings to allow before backtracking into pruning')).
is_control_option(metamap,  '', no_prune,	 	 	no, none).
is_control_option(metamap,  '', prune, no,
                  aspec(prune, mandatory, integer, none, no_default,
                        'Max number of candidates to allow before pruning')).
% sldi == "Single-Line Delimited Input"
is_control_option(metamap,  '', sldi,	 	 		no, none).
% sldiID == "Single-Line Delimited Input with ID"
is_control_option(metamap,  '', sldiID,	 	 		no, none).
% expvars expands variant generation to e.g., intrahepatically --> hepatic
% is_control_option(metamap,  '', expvars,	 	 	no,
% 		  aspec(expvars, mandatory, integer, none, no_default, 'expvars setting: 0, 1, 2.')).
% noexp allows users to specify false positives expvars on the command line
% is_control_option(metamap,  '', noexp,	 	 		no,
% 		  aspec(noexp, mandatory, list, none, no_default, 'expvars false positives.')).
% is_control_option(metamap,  '', restore, 	 	 	no, none).
is_control_option(metamap,  '', 'UDA',   			no,
		  aspec('UDA', mandatory, file, read, no_default, 'File containing UDAs')).
is_control_option(metamap,  '', 'JSONf',   			no,
		  aspec('JSONf', mandatory, integer, none, no_default, 'integer specifying JSON indenting')).
is_control_option(metamap,  '', 'JSONn',	 	 	no, none).
is_control_option(metamap,  '', 'XMLf',		 	 	no, none).
is_control_option(metamap,  '', 'XMLf1',		 	no, none).
is_control_option(metamap,  '', 'XMLn',		 	 	no, none).
is_control_option(metamap,  '', 'XMLn1',		 	no, none).
is_control_option(metamap,  '', 'conj',		 	 	no, none).

% is_control_option(metamap,  '', 'allcats',		 	no, none).

is_control_option(metamap,  '', lexicon, yes,
                   aspec(lexicon, mandatory, none, yes, db,
                         'Specify "c" or "db" for lexicon version.')).

is_control_option(metamap,  '', map_thresh, no,
                   aspec(map_thresh, mandatory, none, none, no_default,
                         'Integer specifying what percentage of mappings to keep (for internal use only!)')).

is_control_option(metamap,  '', prompt, no,
                   aspec(prompt, mandatory, none, none, no_default,
                         'Specify the prompt for interactive use.')).

% show lexical definitions
is_control_option(metamap,  '', 'show_lex',		 	no, none).
is_control_option(metamap,  '', 'pipe_output',		 	no, none).
% recompile negex_triggers.pl
is_control_option(metamap,  '', 'negex_trigger_file', no,
		  aspec(negex_trigger_file, mandatory, file, read, no_default,
			'File containing NegEx trigger definitions')).
is_control_option(metamap,  '', 'nomap', no,
		  aspec(nomap, mandatory, file, read, no_default,
			'File containing String/CUI pairs to exclude.')).
is_control_option(metamap,  '', 'novar', no,
		  aspec(novar, mandatory, file, read, no_default,
			'File containing variant pairs to exclude.')).		  
is_control_option(metamap,  '', 'utterances_only',	 	no, none).
is_control_option(metamap,  '', 'cascade',	 		no, none).
is_control_option(metamap,  '', 'num_break',	 		no, none).
is_control_option(metamap,  '', 'no_nums',	 		no,
                  aspec(no_nums, mandatory, list, none, no_default,
                        'List of semantic types to exclude for numerical_concepts')).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% USemRep %%%%%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_control_option(usemrep,  '', blanklines, 		 	no,
                  aspec(blanklines, mandatory, integer, none, no_default,
			'Number of newlines to read to signal end of citation')).
is_control_option(usemrep,  '', 'cascade',	 		no, none).
is_control_option(usemrep,  '', debug, 			 	no,
                  aspec(debug, mandatory, list, none, no_default, 'Debugging settings')).
is_control_option(usemrep,  '', domain, no,
                   aspec(domain, mandatory, none, no, no_default,
                         'Specify a non-generic domain.')).
is_control_option(usemrep,  '', lexicon, yes,
                   aspec(lexicon, mandatory, none, yes, db,
                         'Specify "c" or "db" for lexicon version.')).
is_control_option(usemrep,  '', negex,		 	 	no, none).
is_control_option(usemrep,  '', negex_st_add, 		 	no,
                  aspec(negex_st_add, mandatory, list, none, no_default, 'SemTypes to add to NegEx')).
is_control_option(usemrep,  '', negex_st_del, 		 	no,
                  aspec(negex_st_del, mandatory, list, none, no_default, 'SemTypes to delete from to NegEx')).
is_control_option(usemrep,  '', negex_st_set, 		 	no,
                  aspec(negex_st_set, mandatory, list, none, no_default, 'SemTypes to set for NegEx')).
is_control_option(usemrep,  '', 'nomap', no,
		  aspec(nomap, mandatory, file, read, no_default,
			'File containing String/CUI pairs to exclude.')).
is_control_option(usemrep,  '', 'no_nums',	 		no,
                  aspec(no_nums, mandatory, list, none, no_default,
                        'List of semantic types to exclude for numerical_concepts')).
is_control_option(usemrep,  '', 'num_break',	 		no, none).
is_control_option(usemrep,  '', prune, no,
                  aspec(prune, mandatory, integer, none, no_default,
                        'Max number of candidates to allow before pruning')).
is_control_option(usemrep,  '', silent,		 	 	no, none).
% sldi == "Single-Line Delimited Input"
is_control_option(usemrep,  '', sldi,	 	 		no, none).
% sldiID == "Single-Line Delimited Input with ID"
is_control_option(usemrep,  '', sldiID,	 	 		no, none).
is_control_option(usemrep,  '', usemrep_processing,   yes, none).

is_control_option(usemrep, '@', 'WSD_SERVER', no,
                  aspec('WSD_SERVER', mandatory, none, none, no_default,
                        'Which WSD server to use')).
is_control_option(usemrep, 'A', anaphora_resolution,   no,  none).
is_control_option(usemrep, 'D', dysonym_processing,   no,  none).
is_control_option(usemrep,  d, no_derivational_variants,	no, none).
is_control_option(usemrep, 'E', indicate_citation_end,  no,  none).
is_control_option(usemrep,   e, exclude_sources, 		no,
                  aspec(exclude_sources, mandatory, list, none, no_default,
                        'List of sources to exclude for output')).
is_control_option(usemrep, 'F', full_fielded_output_format,   no,  none).
is_control_option(usemrep, 'G', genetics_processing,         no,  none).
is_control_option(usemrep,   g, allow_concept_gaps, 		no, none).
is_control_option(usemrep,   h, help,                        no,  none).
is_control_option(usemrep,   i, ignore_word_order, 		no, none).
is_control_option(usemrep, 'J', restrict_to_sts, no,
                  aspec(restrict_to_sts, mandatory, list, none, no_default,
                        'List of semantic types to use for output')).
is_control_option(usemrep,   k, exclude_sts, no,
                  aspec(exclude_sts, mandatory, list, none, no_default,
                        'List of semantic types to exclude for output')).
is_control_option(usemrep, 'L', lexicon_year, 	 		no,
                  aspec(lexicon_year, mandatory, none, none, no_default,
                        'Lexicon year [2006,2012,2014]')).
is_control_option(usemrep,   l, allow_large_n,		        no, none).
is_control_option(usemrep, 'M', relaxed_model,               no, none).
is_control_option(usemrep, 'P', extract_phrases_only,        no, none).
is_control_option(usemrep, 'Q', composite_phrases,           no,
                  aspec(composite_phrases, mandatory, integer, yes, 4,
                        'Max number of prepositional phrases to glom on')).	% MetaMap default
is_control_option(usemrep,   q, unique_acros_abbrs_only,     no, none).
is_control_option(usemrep, 'R', write_syntax,                no,  none).
is_control_option(usemrep, 'r', write_syntax_only,           no,  none).
is_control_option(usemrep,   s, restrict_to_sources, no,
                  aspec(restrict_to_sources, mandatory, list, none, no_default,
                        'List of sources to use for output')).

is_control_option(usemrep, 'S', generic_processing,          no,  none).
is_control_option(usemrep, 'N', use_generic_domain_extension, no, none).
is_control_option(usemrep, 'n', use_generic_domain_modification, no, none).
is_control_option(usemrep,   t, threshold, no,
                  aspec(threshold, mandatory, integer, none, no_default,
                        'Threshold for displaying candidates')).
is_control_option(usemrep, 'T', 'TAGGER_SERVER', no,
                  aspec('TAGGER_SERVER', mandatory, none, none, no_default,
                        'Which tagger server to use')).
is_control_option(usemrep, 'U', expanded_utterances_only,    no,  none).
is_control_option(usemrep,  u,  unexpanded_utterances_only,  no,  none).
is_control_option(usemrep, 'V', mm_data_version, no,
                  aspec(mm_data_version, mandatory, none, none, no_default,
                        'Version of MetaMap data to use [USAbase,NLM]')).
is_control_option(usemrep,   w, warnings,                    no,  none).
is_control_option(usemrep, 'X', xml_output_format, no, none).
is_control_option(usemrep,   x, debug_call,                  no,
                  aspec(debug_call,mandatory,integer,none,no_default,debug_call)).
is_control_option(usemrep, 'Y', prefer_multiple_concepts, 	no, none).
is_control_option(usemrep,   y, word_sense_disambiguation, 	no, none).
is_control_option(usemrep, 'Z', mm_data_year, no,
                  aspec(mm_data_year,mandatory, none, none, no_default,
                        'Release of MetaMap data to use (e.g., 2015AA)')).
is_control_option(usemrep,   z, term_processing, 		no, none).
is_control_option(usemrep,  '', mm_add,   no,
                   aspec(mm_add, mandatory, none, none, no_default,
                         'List of MetaMap options to add to SemRep MetaMap options')).
is_control_option(usemrep,  '', mm_sub,   no,
                   aspec(mm_sub, mandatory, none, none, no_default,
                         'List of MetaMap options to subtract from SemRep default MetaMap options')).
% is_control_option(usemrep, Short, Long, X, Y) :- is_control_option(metamap, Short, Long, X, Y).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Other programs %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

is_control_option(build_ambig_examples,h,help,no,none).
is_control_option(build_ambig_examples,w,warnings,no,none).

is_control_option(filter_mrconso,s,strict_filtering,no,none).
is_control_option(filter_mrconso,'E',end_of_processing,no,none).
% is_control_option(filter_mrconso,'S',filter_by_term_status,no,none).
is_control_option(filter_mrconso,x,dump_syntax_only,no,none).
is_control_option(filter_mrconso,h,help,no,none).
is_control_option(filter_mrconso,i,info,no,none).
% is_control_option(filter_mrconso, 'R', mrrank_file, no,
%                   aspec(mrrank_file,mandatory,file,read,no_default,
%                         'MRRANK file')).
is_control_option(filter_mrconso, 'V', mm_data_version, no,
                  aspec(mm_data_version, mandatory, none, none, no_default,
                        'Version of MetaMap data to use')).
is_control_option(filter_mrconso, 'Z', mm_data_year, no,
                  aspec(mm_data_year,mandatory, none, none, no_default,
                        'Release of MetaMap data to use')).

is_control_option(filter_mrconso, p, progress_bar_interval, no,
                  aspec(progress_bar_interval,mandatory,integer,none,no_default,
                        'Interval of progress bar')).
is_control_option(filter_mrconso,t,total_lines,no,
                  aspec(total_lines, mandatory, integer, none, no_default,
                        'Total number of lines to process')).
is_control_option(filter_mrconso,  '', lexicon, no,
                  aspec(lexicon, mandatory, none, none, no_default,
                        'Whether to use original C code or lexAccess version of lexicon other than lex_form_input')).
is_control_option(filter_mrconso,  '', clfi, no,
                  aspec(clfi, mandatory, none, none, no_default,
                        'Whether to use original C code or lexAccess version of lex_form_input')).
is_control_option(filter_mrconso,'N',silent,no,none).
is_control_option(filter_mrconso,w,warnings,no,none).

is_control_option(glean_mrconso, f, first_term_is_concept, yes, none).
is_control_option(glean_mrconso, c, generate_CUIs,         no,  none).
is_control_option(glean_mrconso, s, generate_strings,      no,  none).
is_control_option(glean_mrconso, w, generate_words,        no,  none).
is_control_option(glean_mrconso, h, help,                  no,  none).

% is_control_option(mm_print, 'A',alnum_filter,no,none).
% is_control_option(mm_print, z,stop_phrase_file,no,
%                   aspec(stop_phrase_file,mandatory,file,read,no_default,
%                         'File of stop phrases')).
% is_control_option(mm_print, 'T',truncate_output,no,none).
% is_control_option(mm_print, f,filter_out_01,no,none).
% is_control_option(mm_print, r,threshold,no,
%                   aspec(threshold,mandatory,integer,none,no_default,
%                         'Threshold for displaying candidates')).
% is_control_option(mm_print, n,null_only,no,none).
% is_control_option(mm_print, 'K',non_null_only,no,none).
% is_control_option(mm_print, x,syntax,no,none).
% is_control_option(mm_print, s,simple_syntax,yes,none).
is_control_option(mm_print, c,candidates,yes,none).
is_control_option(mm_print, t,semantic_types,yes,none).
is_control_option(mm_print, m,mappings,yes,none).
% is_control_option(mm_print, o,organize_semantic_types,no,none).
% is_control_option(mm_print, 'F',first_mappings_only,no,none).
is_control_option(mm_print, 'I',show_cuis,no,none).
% is_control_option(mm_print, 'O',show_preferred_names_only,no,none).
% is_control_option(mm_print, l,not_in_lex_dump,no,none).
% is_control_option(mm_print, d,label_text_field_dump,no,none).
% is_control_option(mm_print, a,syntax_dump,no,none).
% is_control_option(mm_print, y,syntactic_pattern_dump,no,none).
% is_control_option(mm_print, w,with_text,no,none).
% is_control_option(mm_print, b,candidate_count_dump,no,none).
% is_control_option(mm_print, 'C',candidate_text_dump,no,none).
% is_control_option(mm_print, 'M',mapping_text_dump,no,none).
% is_control_option(mm_print, 'S',mapping_summary_dump,no,none).
% is_control_option(mm_print, 'U',unique_mapping_text_dump,no,none).
% is_control_option(mm_print, 'N',non_monotonic_mapping_dump,no,none).
% is_control_option(mm_print, 'k',odd_mapping_dump,no,none).
% is_control_option(mm_print, j,mapping_count_dump,no,none).
is_control_option(mm_print, p,potential_stopphrase_dump,no,none).
% is_control_option(mm_print, 'P',phrase_dump,no,none).
% is_control_option(mm_print, 'Q',prepositional_phrase_dump,no,none).
% is_control_option(mm_print, i,inputmatch_lexmatch_dump,no,none).

is_control_option(mm_print, h,help,no,none).
is_control_option(mm_print,  '', 'JSONf',   		 no,
		  aspec('JSONf', mandatory, integer, none, no_default, 'integer specifying JSON indenting')).
is_control_option(mm_print,  '', 'JSONn',	   	 no, none).
is_control_option(mm_print,  '', 'XMLf',		 no, none).
is_control_option(mm_print,  '', 'XMLf1',	 	 no, none).
is_control_option(mm_print,  '', 'XMLn',		 no, none).
is_control_option(mm_print,  '', 'XMLn1',	 	 no, none).
is_control_option(mm_print,  '', negex, no, none).

is_control_option(mm_variants,u,unique_acros_abbrs_only,no,none).
is_control_option(mm_variants,'D',all_derivational_variants,no,none).
is_control_option(mm_variants,'E',end_of_processing,no,none).
is_control_option(mm_variants,h,help,no,none).
is_control_option(mm_variants,i,info,no,none).
is_control_option(mm_variants,w,warnings,no,none).
is_control_option(mm_variants,'0',debug0,no,none).
is_control_option(mm_variants, 'V', mm_data_version, no,
                  aspec(mm_data_version,mandatory, none, none, no_default,
                        'Version of MetaMap data to use')).
is_control_option(mm_variants, 'Z', mm_data_year, no,
                  aspec(mm_data_year,mandatory, none, none, no_default,
                        'Year of MetaMap data to use')).
is_control_option(mm_variants,  '', lexicon, no,
                   aspec(lexicon, mandatory, none, none, no_default,
                         'Whether to use original C code or lexAccess version of lexicon other than lex_form_input')).
is_control_option(mm_variants,  '', debug, no,
                  aspec(debug, mandatory, list, none, no_default, 'Debugging settings')).

is_control_option(prefilter_mrconso,b,brand_name_suppression,no,none).
is_control_option(prefilter_mrconso,d,dump_prefilter_cases,no,none).
is_control_option(prefilter_mrconso,v,filter_all_vocabularies,no,none).
is_control_option(prefilter_mrconso,h,help,no,none).
is_control_option(prefilter_mrconso,i,info,no,none).
is_control_option(prefilter_mrconso,w,warnings,no,none).

% Obsolete programs:
% is_control_option(check_ambiguity,c,ignore_case,no,none).
% is_control_option(check_ambiguity,p,report_positives,no,none).
% is_control_option(check_ambiguity,h,help,no,none).
% is_control_option(check_ambiguity,i,info,no,none).
% is_control_option(check_ambiguity,w,warnings,no,none).
% is_control_option(chunk_mrconso,h,help,no,none).
% is_control_option(chunk_mrconso,i,info,no,none).
% is_control_option(chunk_mrconso,w,warnings,no,none).
% is_control_option(compute_categories,h,help,no,none).
% is_control_option(compute_categories,i,info,no,none).
% is_control_option(compute_categories,w,warnings,no,none).
% is_control_option(compute_mrconso_types,h,help,no,none).
% is_control_option(compute_tree_depths,p,write_pairs,no,none).
% is_control_option(compute_tree_depths,t,write_table,no,none).
% is_control_option(compute_tree_depths,h,help,no,none).
% is_control_option(compute_tree_depths,i,info,no,none).
% is_control_option(compute_tree_depths,w,warnings,no,none).
% is_control_option(create_pvm_io,h,help,no,none).
% is_control_option(create_solexicon,h,help,no,none).
% is_control_option(create_solexicon,i,info,no,none).
% is_control_option(create_solexicon,w,warnings,no,none).
 
% is_control_option(db_test,h,help,no,none).
% is_control_option(document_joiner,h,help,no,none).
% is_control_option(document_joiner,w,warnings,no,none).
% is_control_option(document_planner,i,ignore_head,yes,none).
% is_control_option(document_planner,l,level,no,
%                   aspec(level,mandatory,integer,none,no_default,
%                         'Hierarchical level at which to construct the plan')).
% is_control_option(document_planner,h,help,no,none).
% is_control_option(document_planner,w,warnings,no,none).
% is_control_option(dummy,h,help,no,none).
 
% is_control_option(explore_intractables,h,help,no,none).
% is_control_option(exp_utt, 'U', unexpanded_utterances, no, none).
% is_control_option(exp_utt, 'N', numbered_utterances,   no, none).
% is_control_option(exp_utt, 'P', no_PMID,               no, none).
% is_control_option(extract_generif,h,help,no,none).
% is_control_option(extract_generif,i,info,no,none).
% is_control_option(extract_generif,w,warnings,no,none).

% is_control_option(extractm,x,halt_on_error,yes,none).
% is_control_option(extractm,i,indented,yes,none).
% is_control_option(extractm,c,compact_format,no,none).
% is_control_option(extractm,s,short_subheadings,no,none).
% is_control_option(extractm,h,help,no,none).
% is_control_option(extract_framerd_phrases,l,display_labels,yes,none).
% is_control_option(extract_framerd_phrases,h,help,no,none).
% is_control_option(extract_framerd_phrases,i,info,no,none).
% is_control_option(extract_framerd_phrases,w,warnings,no,none).

% is_control_option(extract_mrconso_sources,f,first_of_each_source_only,yes,none).
% is_control_option(extract_mrconso_sources,h,help,no,none).
% is_control_option(extract_mrconso_sources,i,info,no,none).
% is_control_option(extract_mrconso_sources,w,warnings,no,none).
% is_control_option(extract_mrconso_sources, p, progress_bar_interval, no,
%                   aspec(progress_bar_interval,mandatory,integer,none,no_default,
%                         'Interval of progress bar')).
% is_control_option(extract_mrconso_sources, t, total_lines, no,
%                   aspec(total_lines,mandatory,integer,none,no_default,
%                         'Total number of lines to process')).

% is_control_option(filter_cases,l,lower_bound,no,
%                   aspec(lower_bound,mandatory,integer,none,no_default,
%                         'Lower bound of occurrence count')).
% is_control_option(filter_cases,u,upper_bound,no,
%                   aspec(upper_bound,mandatory,integer,none,no_default,
%                         'Upper bound of occurrence count')).
% is_control_option(filter_cases,w,warnings,no,none).
% is_control_option(filter_cases,h,help,no,none).

% is_control_option(find_subwords,a,use_atoms,no,none).
% is_control_option(find_subwords,h,help,no,none).
% is_control_option(find_subwords,i,info,no,none).
% is_control_option(flip_variants,h,help,no,none).
% is_control_option(flip_variants,i,info,no,none).
% is_control_option(flip_variants,w,warnings,no,none).

% is_control_option(glean_ambig,h,help,no,none).
% is_control_option(glean_ambig,w,warnings,no,none).
% is_control_option(glean_ambig,'R',mrrank_file,no,
%                   aspec(mrrank_file,mandatory,file,read,no_default,
%                         'MRRANK file')).
% is_control_option(glean_ambig, p, progress_bar_interval, no,
%                   aspec(progress_bar_interval,mandatory,integer,none,no_default,
%                         'Interval of progress bar')).
% is_control_option(glean_ambig, t, total_lines, no,
%                   aspec(total_lines,mandatory,integer,none,no_default,
%                         'Total number of lines to process')).
		  
% is_control_option(glean_co_st,n,normalize,yes,none).
% is_control_option(glean_co_st,h,help,no,none).
% is_control_option(glean_from_mm,t,three_column_output,yes,none).
% is_control_option(glean_from_mm,r,remove_raw_output_file,yes,none).
% is_control_option(glean_from_mm,h,help,no,none).
% is_control_option(glean_synonyms,h,help,no,none).
% is_control_option(glean_synonyms,i,info,no,none).
% is_control_option(glean_synonyms,w,warnings,no,none).
% is_control_option(glean_unique_aa,h,help,no,none).
 
% is_control_option(inq_annotate_best,e,best_of_each,yes,none).
% is_control_option(inq_annotate_best,h,help,no,none).
% is_control_option(inq_annotate_best,i,info,no,none).
% is_control_option(inq_annotate_best,w,warnings,no,none).
% is_control_option(inq_extract_annotations,h,help,no,none).
% is_control_option(inq_extract_annotations,i,info,no,none).
% is_control_option(inq_extract_annotations,w,warnings,no,none).
% is_control_option(inq_form_c_cits,u,uninverted_concepts,no,none).
% is_control_option(inq_form_c_cits,x,ranking_factor,no,
%                   aspec(ranking_factor,mandatory,integer,none,no_default,
%                         'Weight for multiplying MMI concept factors')).
% is_control_option(inq_form_c_cits,s,single_concepts_only,no,none).
% is_control_option(inq_form_c_cits,r,relation_file,no,
%                   aspec(relation_file,mandatory,file,read,no_default,
%                         'Input file of ABS relations')).
% is_control_option(inq_form_c_cits,h,help,no,none).
% is_control_option(inq_form_c_cits,i,info,no,none).
% is_control_option(inq_form_c_cits,w,warnings,no,none).
% is_control_option(inq_form_wpc_queries,f,field_operator,yes,none).
% is_control_option(inq_form_wpc_queries,m,mesh_rather_than_mmi,no,none).
% is_control_option(inq_form_wpc_queries,g,cphrase_rather_than_csum,no,none).
% is_control_option(inq_form_wpc_queries,n,no_weighting,no,none).
% is_control_option(inq_form_wpc_queries,u,uninverted_concepts,no,none).
% is_control_option(inq_form_wpc_queries,b,feedback_parameter_file,no,
%                   aspec(feedback_parameter_file,mandatory,file,read,no_default,
%                         'File of feedback parameters')).
% is_control_option(inq_form_wpc_queries,x,restricted_word_feedback,no,none).
% is_control_option(inq_form_wpc_queries,q,restricted_phrase_feedback,no,none).
% is_control_option(inq_form_wpc_queries,d,concept_feedback_limit,no,
%                   aspec(concept_feedback_limit,mandatory,integer,none,
%                         no_default,
%                         'Maximum number of concepts per feedback citation')).
% is_control_option(inq_form_wpc_queries,t,title_weight,no,
%                   aspec(title_weight,mandatory,integer,none,no_default,
%                         'Weight for title evidence (abstract is 1)')).
% is_control_option(inq_form_wpc_queries,w,wweight,no,
%                   aspec(wweight,mandatory,integer,none,no_default,
%                         'Weight for bag-of-words evidence')).
% is_control_option(inq_form_wpc_queries,p,pweight,no,
%                   aspec(pweight,mandatory,integer,none,no_default,
%                         'Weight for phrase evidence')).
% is_control_option(inq_form_wpc_queries,c,cweight,no,
%                   aspec(cweight,mandatory,integer,none,no_default,
%                         'Weight for concept evidence')).
% is_control_option(inq_form_wpc_queries,h,help,no,none).
% is_control_option(inq_form_wpc_queries,i,info,no,none).
% is_control_option(inq_pp_ranked,h,help,no,none).
% is_control_option(inq_pp_ranked,w,warnings,no,none).
% is_control_option(inq_transform_results,p,percentages,no,none).
% is_control_option(inq_transform_results,h,help,no,none).
% is_control_option(inq_transform_results,i,info,no,none).
% is_control_option(inq_transform_results,w,warnings,no,none).
 
% is_control_option(ll_to_umls,d,dump_labeled_terms,no,none).
% is_control_option(ll_to_umls,f,ambiguity_file,no,
%                   aspec(ambiguity_file,mandatory,file,read,no_default,
%                         'File of ambiguous terms')).
% is_control_option(ll_to_umls,m,max_ambiguity,no,
%                   aspec(max_ambiguity,mandatory,integer,none,no_default,
%                         'The maximum degree of ambiguity allowed')).
% is_control_option(ll_to_umls,h,help,no,none).
% is_control_option(ll_to_umls,i,info,no,none).
% is_control_option(ll_to_umls,w,warnings,no,none).
 
% is_control_option(medline_to_sgml,p,pre_parsed_mesh,no,none).
% is_control_option(medline_to_sgml,s,standard_sgml_format,no,none).
% is_control_option(medline_to_sgml,m,multiple_field_occurrences,no,none).
% is_control_option(medline_to_sgml,n,no_mesh_subheadings,no,none).
% is_control_option(medline_to_sgml,'N',ngi_processing,no,none).
% is_control_option(medline_to_sgml,h,help,no,none).
% is_control_option(medline_to_sgml,i,info,no,none).
% is_control_option(medline_to_sgml,w,warnings,no,none).
% is_control_option(merge_iis_output,h,help,no,none).
% is_control_option(merge_iis_output,w,warnings,no,none).
% is_control_option(merge_lexicons,h,help,no,none).
% is_control_option(mesh_terminology,h,help,no,none).
% is_control_option(mesh_terminology,i,info,no,none).
% is_control_option(mesh_update,h,help,no,none).
% is_control_option(mesh_update,i,info,no,none).
% is_control_option(mesh_update,w,warnings,yes,none).
% is_control_option(mesh_update_prep,d,delete_items,no,none).
% is_control_option(mesh_update_prep,u,update_items,no,none).
% is_control_option(mesh_update_prep,h,help,no,none).
% is_control_option(mesh_update_prep,i,info,no,none).
% is_control_option(metastat,h,help,no,none).
% is_control_option(metastat,i,info,no,none).
% is_control_option(metastat,w,warnings,no,none).
% is_control_option(mm_convert_mo,o,other_filter,no,none).
% is_control_option(mm_convert_mo,h,help,no,none).
% is_control_option(mm_extract_fielded,h,help,no,none).
% is_control_option(mm_extract_fielded,w,warnings,no,none).
% is_control_option(mm_filter,c,duplicate_concepts,no,none).
% is_control_option(mm_filter,h,help,no,none).

% is_control_option(mm_select,h,help,no,none).

% is_control_option(mm_tally,m,metamap_mo_format,no,none).
% is_control_option(mm_tally,c,mrcon_format,no,none).
% is_control_option(mm_tally,p,phrase_lengths,no,none).
% is_control_option(mm_tally,w,phrase_words,no,none).
% is_control_option(mm_tally,h,help,no,none).
% is_control_option(mm_time,h,help,no,none).
% is_control_option(mm_tokenizer,s,whitespace_tokenization,no,none).
% is_control_option(mm_tokenizer,w,wordind_tokenization,no,none).
% is_control_option(mm_tokenizer,m,metamap_tokenization,no,none).
% is_control_option(mm_tokenizer,c,complete_tokenization,no,none).
% is_control_option(mm_tokenizer,l,lower_case,no,none).
% is_control_option(mm_tokenizer,u,unique_tokens,no,none).
% is_control_option(mm_tokenizer,q,prolog_output,no,none).
% is_control_option(mm_tokenizer,b,brief_output,no,none).
% is_control_option(mm_tokenizer,h,help,no,none).
% is_control_option(mm_update_mh,x,dump_mesh,no,none).
% is_control_option(mm_update_mh,g,glean_mesh,no,none).
% is_control_option(mm_update_mh,p,pre_parsed_mesh,no,none).
% is_control_option(mm_update_mh,m,create_multiple_mesh_fields,no,none).
% is_control_option(mm_update_mh,l,create_multi_line_mesh_field,no,none).
% is_control_option(mm_update_mh,h,help,no,none).
% is_control_option(mm_update_mh,i,info,no,none).
% is_control_option(mm_update_mh,d,debug,no,none).

% is_control_option(phrasex,t,tag_text,yes,none).
% is_control_option(phrasex,h,help,no,none).
% is_control_option(phrasex,w,warnings,no,none).
% is_control_option(phrasex,d,dump,no,none).

% is_control_option(qualcon,r,relaxed_mode,no,none).
% is_control_option(qualcon,h,help,no,none).
% is_control_option(qualcon,w,warnings,no,none).

% is_control_option(random_split,h,help,no,none).
% is_control_option(random_split,i,info,no,none).
% is_control_option(random_split,w,warnings,no,none).
% is_control_option(rebuild_ambig,h,help,no,none).
% is_control_option(rebuild_ambig,w,warnings,no,none).

% is_control_option(sp_to_umls,d,dump_labeled_terms,no,none).
% is_control_option(sp_to_umls,f,ambiguity_file,no,
%                   aspec(ambiguity_file,mandatory,file,read,no_default,
%                         'File of ambiguous terms')).
% is_control_option(sp_to_umls,m,max_ambiguity,no,
%                   aspec(max_ambiguity,mandatory,integer,none,no_default,
%                         'The maximum degree of ambiguity allowed')).
% is_control_option(sp_to_umls,h,help,no,none).
% is_control_option(sp_to_umls,i,info,no,none).
% is_control_option(sp_to_umls,w,warnings,no,none).
% is_control_option(spattern,x,only_non_semnet,no,none).
% is_control_option(spattern,y,no_semnet_filtering,no,none).
% is_control_option(spattern,l,left_limit,no,
%                   aspec(left_limit,mandatory,integer,none,no_default,
%                         'Limit to search to the left')).
% is_control_option(spattern,r,right_limit,no,
%                   aspec(right_limit,mandatory,integer,none,no_default,
%                         'Limit to search to the right')).
% is_control_option(spattern,f,filter_file,no,
%                   aspec(filter_file,mandatory,file,read,no_default,
%                         'File of semantic types to filter out')).
% is_control_option(spattern,s,statistics_file,no,
%                   aspec(statistics_file,mandatory,file,write,no_default,
%                         'File of statistics')).
% is_control_option(spattern,i,include_relations,yes,none).
% is_control_option(spattern,o,'relational_output',no,none).
% is_control_option(spattern,m,mod_head_tuples,no,none).
% is_control_option(spattern,p,preposition_triples,no,none).
% is_control_option(spattern,v,verb_triples,no,none).
% is_control_option(spattern,t,head_triples,no,none).
% is_control_option(spattern,n,nominalization_triples,no,none).
% is_control_option(spattern,d,phrase_dump,no,none).
% is_control_option(spattern,w,warnings,no,none).
% is_control_option(spattern,h,help,no,none).
% is_control_option(spattern_dump,f,full_pattern_headers,no,none).
% is_control_option(spattern_dump,r,phrase_headers,no,none).
% is_control_option(spattern_dump,p,pattern_headers,no,none).
% is_control_option(spattern_dump,h,help,no,none).
% is_control_option(spattern_print,f,filter_file,no,
%                   aspec(filter_file,mandatory,file,read,no_default,
%                         'File of filter patterns')).
% is_control_option(spattern_print,m,max_to_print,no,
%                   aspec(max_to_print,mandatory,integer,none,no_default,
%                         'Maximum number of examples per pattern to print')).
% is_control_option(spattern_print,h,help,no,none).
% is_control_option(synonym_baser,d,dump_details,no,none).
% is_control_option(synonym_baser,w,warnings,no,none).
% is_control_option(synonym_baser,h,help,no,none).

% is_control_option(text_object_explorer,i,ignore_comments,yes,none).
% is_control_option(text_object_explorer,r,respect_whitespace,yes,none).
% is_control_option(text_object_explorer,g,gate_output,no,none).
% is_control_option(text_object_explorer,a,aas,no,none).
% is_control_option(text_object_explorer,b,sentence_boundary,no,none).
% is_control_option(text_object_explorer,p,parenthetical,no,none).
% is_control_option(text_object_explorer,c,compound_token,no,none).
% is_control_option(text_object_explorer,m,chemicals,no,none).
% is_control_option(text_object_explorer,'A',tfidf_csb,no,none).
% is_control_option(text_object_explorer,'B',tfidf_h,no,none).
 
% is_control_option(text_object_explorer,l,lower_bound,no,
%                   aspec(lower_bound,mandatory,integer,none,no_default,
%                         'Lower bound for various options')).
% is_control_option(text_object_explorer,'1',debug1,no,none).
% is_control_option(text_object_explorer,'2',debug2,no,none).
% is_control_option(text_object_explorer,'3',debug3,no,none).
% is_control_option(text_object_explorer,'4',debug4,no,none).
% is_control_option(text_object_explorer,h,help,no,none).
% is_control_option(text_object_explorer,i,info,no,none).
% is_control_option(text_object_explorer,w,warnings,no,
%                   aspec(warnings_file,mandatory,file,write,no_default,
%                         'File of warnings')).
% is_control_option(text_to_medline,h,help,no,none).
% is_control_option(text_to_medline,i,info,no,none).
% is_control_option(text_to_medline,w,warnings,no,none).
% is_control_option(thesaurus_parser,v,validate,yes,none).
% is_control_option(thesaurus_parser,h,help,no,none).
% is_control_option(thesaurus_parser,w,warnings,no,none).
% is_control_option(thesaurus_to_umls,h,help,no,none).
% is_control_option(thesaurus_to_umls,w,warnings,no,none).
% is_control_option(tr_to_umls,d,dump_labeled_terms,no,none).
% is_control_option(tr_to_umls,f,ambiguity_file,no,
%                   aspec(ambiguity_file,mandatory,file,read,no_default,
%                         'File of ambiguous terms')).
% is_control_option(tr_to_umls,m,max_ambiguity,no,
%                   aspec(max_ambiguity,mandatory,integer,none,no_default,
%                         'The maximum degree of ambiguity allowed')).
% is_control_option(tr_to_umls,h,help,no,none).
% is_control_option(tr_to_umls,i,info,no,none).
% is_control_option(tr_to_umls,w,warnings,no,none).
% is_control_option(treecodes,h,help,no,none).
% 
% is_control_option(uttok_pp,h,help,no,none).
% is_control_option(uttok_pp,i,info,no,none).
% is_control_option(uwda_to_umls,h,help,no,none).
% is_control_option(uwda_to_umls,w,warnings,no,none).
 
% is_control_option(xfer_to_occs,h,help,no,none).

/* get_control_options_for_modules(+Module(s), -Options)

get_control_options_for_modules/2 computes the set of all legal control
Options for Module(s) (an atom or list of atoms).  */

get_control_options_for_modules([], []) :-
    !.
get_control_options_for_modules(Module,Options) :-
    atom(Module),
    !,
    get_control_options_for_modules([Module],Options).
get_control_options_for_modules(Modules,Options) :-
    findall(Option,
            (member(Module,Modules), is_control_option(Module,_,Option,_,_)),
            Options0),
    sort(Options0,Options).


/* reset_control_options(+Module(s))

set_control_options/0 retracts all control_option/1 clauses and sets the
defaults for Module(s).  */

reset_control_options(ModuleOrModules) :-
	retractall(control_option(_)),
	retractall(control_value(_,_)),
	% retractall(control_value(_,_,_)),
	( atom(ModuleOrModules) ->
	  reset_control_options_aux([ModuleOrModules])
	; reset_control_options_aux(ModuleOrModules)
	).

reset_control_options_aux([]).
reset_control_options_aux([Module|Rest]) :-
	toggle_default_control_options(Module),
	reset_control_options_aux(Rest).

/* toggle_default_control_options(+Module)

toggle_default_control_options/1 toggles those control options of the form
is_control_option(Module,_,ControlOption,yes,_), i.e., with IsDefault
set to yes.  */

toggle_default_control_options(Module) :-
	findall(Option:Default,
		option_and_default(Module, Option,Default),
		DefaultOptions),
	toggle_control_options(DefaultOptions).

option_and_default(Module, Option, Default) :-
	( Module == metamap ->
	  is_control_option(metamap, _, Option, yes, LastArg)
	; Module == usemrep ->
	  is_control_option(usemrep, _, Option, yes, LastArg),
	  \+ is_control_option(metamap, _, Option, yes, LastArg)
	; get_program_name(ProgramName) ->
	  is_control_option(ProgramName, _, Option, yes, LastArg)
	),
	( atom(LastArg) ->
	  Default = LastArg
	; LastArg = aspec(Option,_,_,yes,Default,_)
	).

/* toggle_control_options(+Options)
   set_control_options(+Options)
   add_to_control_options(+Options)
   subtract_from_control_options(+Options)

toggle_control_options/1 toggles each of Options.
set_control_options/1 sets Options.
Each of toggle_control_options/1, set_control_options/1 and
add_to_control_options/1 adds Options regardless of whether they are already
set or not.
subtract_from_control_options/1 deletes a single occurrence of Options.
add_to_control_options/1 and subtract_from_control_options/1 are used
together to perform some processing with modified options and then revert
to the previous option configuration. */


toggle_control_options([]).
toggle_control_options([Option0|Rest]) :-
	( Option0 = iopt(Option,_) ->
	  Default = none
        ; Option0 = Option:Default
        ),
	retractall(control_option(Option)),
        assert_control_option(Option), 
	( Default \== none ->
	  retractall(control_value(Option, _V)),
	  assert_control_value(Option, Default)
	; true
	),
	toggle_control_options(Rest).

set_control_options([]).
set_control_options([iopt(Option,_)|Rest]) :-
	!,
	assert_control_option(Option),
	set_control_options(Rest).
set_control_options([optval(Option,_Attribute,Value)|Rest]) :-
	!,
	% assert_control_value(Option,Attribute,Value),
	assert_control_value(Option,Value),
	set_control_options(Rest).
set_control_options([Option-Value|Rest]) :-
	!,
	assert_control_option(Option),
	assert_control_value(Option,Value),
	set_control_options(Rest).
set_control_options([Option|Rest]) :-
	assert_control_option(Option),
	set_control_options(Rest).

add_to_current_control_options(OptionsIn-Values, OptionsToAdd, OptionsOut-Values) :-
	append(OptionsIn, OptionsToAdd, OptionsOut0),
	sort(OptionsOut0, OptionsOut).

add_to_control_options([]).
add_to_control_options([Option0|Rest]) :-
	( Option0=iopt(Option,_) ->
	  true
        ; Option=Option0
        ),
	assert_control_option(Option),
	add_to_control_options(Rest).

subtract_from_control_options([]).
subtract_from_control_options([Option0|Rest]) :-
	( Option0=iopt(Option,_) ->
	  true
        ; Option=Option0
        ),
	( retractall(control_option(Option))
        ; true
        ),
	!,
	subtract_from_control_options(Rest).

/* set_control_values(+IOptions, +IArgs)

set_control_values/2 asserts control_value/2 clauses for each control option
in IOptions with a value attribute in IArgs.  */

set_control_values([] ,_) :- set_default_lexicon.
set_control_values([iopt(_,none)|Rest], IArgs) :-
	!,
	set_control_values(Rest, IArgs).
set_control_values([iopt(Option,_)|Rest],IArgs) :-
	set_all_control_values(Option, IArgs),
	% assert the most commonly used attribute value (this is obsolete)
	( get_from_iargs(Option,value,IArgs,Value) ->
	  true
	; get_from_iargs(Option,stream,IArgs,Value) ->
	  true
	; get_from_iargs(Option,_Any,IArgs,Value) ->
	  true
	; Value = '<none>'
	),
	assert_control_value(Option,Value),
	set_control_values(Rest,IArgs).

set_all_control_values(Option, IArgs) :-
	get_from_iargs(Option, _Attribute, IArgs, Value),
	% assert_control_value(Option, Attribute, Value),
	assert_control_value(Option, Value),
	fail.
set_all_control_values(_, _).

/* display_current_control_options(+ProgramName, +DefaultRelease)

display_current_control_options/2 displays program name and year and currently set options.  */

display_current_control_options(ProgramName, DefaultRelease) :-
	conditionally_announce_program_name(ProgramName, DefaultRelease),
	display_current_control_options_aux.

display_current_control_options_aux :-
	% If any control options are set, show them all,
	% unless --silent is set from the command line.
	\+ control_option(silent),
	!,
	send_message('Control options:', []),
	( \+ control_option(Option) ->
	  send_message(' NONE~n~n', [])
	; send_message('~n', []),
	  control_option(Option),
	  display_one_control_option(Option),
	  fail
	; true
	).
display_current_control_options_aux.

display_one_control_option(Option) :-
	% ( control_value(Option,_Any,Value) ->
	( control_value(Option,Value) ->
	  send_message('  ~a=~p~n', [Option,Value])
	; send_message('  ~a~n', [Option])
	).

display_mandatory_metamap_options :-
	format('Options with mandatory arguments:~n', []),
	findall(LongOptionName-LongOptionNameLength:Description,
		( is_control_option(metamap,_ShortOptionName,LongOptionName,_IsDefault,
				    aspec(LongOptionName,mandatory,
					  _Type,_SubType,_Default,Description)),
		  atom_length(LongOptionName, LongOptionNameLength)),
		  OptionDescriptionList),
	longest_option_name_length(OptionDescriptionList, 0, LongestLength),
	sort(OptionDescriptionList, SortedOptionDescriptionList), 
	print_mandatory_options(SortedOptionDescriptionList, LongestLength).

print_mandatory_options([], _LongestLength) :- nl.
print_mandatory_options([OptionName-Length:Description|Rest], LongestLength) :-
	Padding is LongestLength - Length,
	format('~*c~w: ~w~n', [Padding,32,OptionName,Description]),
	print_mandatory_options(Rest, LongestLength).

longest_option_name_length([], LongestLength, LongestLength).
longest_option_name_length([_OptionName-Length:_Desctiption|Rest], LengthSoFar, LongestLength) :-
	( Length > LengthSoFar ->
	  NextLength is Length
	; NextLength is LengthSoFar
	),
	longest_option_name_length(Rest, NextLength, LongestLength).

/* display_control_options_for_modules(+Module, +OtherModules)

display_control_options_for_modules/2 displays the control options of
Module (indicating defaults) and then displays the control options of
the OtherModules.  */

display_all_control_options([]).
display_all_control_options([ShortOption-Option-IsDefault-ArgSpec|RestOptions]) :-
	display_one_control_option_4(ShortOption, Option, IsDefault, ArgSpec),
	display_all_control_options(RestOptions).

display_one_control_option_4(ShortOption, Option, IsDefault, ArgSpec) :-
	( IsDefault == yes ->
	  DefaultIndicator='  [DEFAULT] '
	; DefaultIndicator='            '
	),
	( ArgSpec = aspec(_SpecName,_Option,Type,_SubType,_Default,_Description) ->
	  concatenate_items_to_atom([" <",Type,">"],ArgType)
	; ArgType = ''
	),
	( ShortOption == '' ->
	  format('~a   --~a~a~n',  [DefaultIndicator,Option,ArgType])
	; format('~a-~a --~a~a~n', [DefaultIndicator,ShortOption,Option,ArgType])
	).	

display_all_other_module_control_options([]).
display_all_other_module_control_options([Option-ArgSpec|RestOptions]) :-
	display_one_other_module_control_option(Option, ArgSpec),
	display_all_other_module_control_options(RestOptions).

display_one_other_module_control_option(Option, ArgSpec) :-
	( ArgSpec = aspec(_SpecName,_Option,Type,_SubType,_Default,_Description) ->
	  concatenate_items_to_atom([" <",Type,">"], ArgType)
	; ArgType = ''
	),
	format('               --~a~a~n', [Option,ArgType]).

display_control_options_for_modules(Module, _OtherModules) :-
	format('  ~a options ("<none>" means no default datatype):~n', [Module]),
	setof(ShortOption-Option-IsDefault-ArgSpec,
	      is_control_option(Module, ShortOption, Option, IsDefault, ArgSpec),
	      AllOptions),
	display_all_control_options(AllOptions),
	fail.
display_control_options_for_modules(Module, OtherModules) :-
	member(OtherModule, OtherModules),
	format('~n', []),
	format('  ~a options:~n', [OtherModule]),
	setof(Option-ArgSpec,
	      ShortOption^IsDefault^ShortOption1^IsDefault1^ArgSpec1^
	          ( is_control_option(OtherModule, ShortOption,  Option, IsDefault,  ArgSpec),
		    \+ is_control_option(Module,   ShortOption1, Option, IsDefault1, ArgSpec1)
		  ),
	      AllOptions),
	display_all_other_module_control_options(AllOptions),
	fail.

display_control_options_for_modules(_Module, _OtherModules) :-
	format('~n', []).


/* parse_command_line(-CommandLine)
   parse_command_line(+RawCL, -Options, -Args)

parse_command_line/1 constructs CommandLine=command_line(Options,Args)
where Options is a list of options and Args a list of non-options.  A sequence
of single-character options is signalled by an initial '-'; a verbose option is
signalled by '--'.  Thus, for
1             parser -vt --semantics infile outfile
parse_command_line would produce
             command_line([v,t,semantics], [infile,outfile]).
parse_command_line/3 is both an auxiliary predicate and an alternative used
to parse new options.  RawCL should be of the form ['-<options>'], is a
singleton list consisting of an atom beginning with a hyphen.  */

pa :- pwd, args.

pwd(PWD) :- environ('PWD', PWD).

pwd :-
	environ('PWD', PWD),
	format(user_output, '~w~n', [PWD]).

args :-
	prolog_flag(argv, RawCommandLine),
	get_extra_SICStus_args(ExtraArgsList),
	append(RawCommandLine, ExtraArgsList, AugmentedCommandLine),
	format(user_output, '~w~n', [AugmentedCommandLine]).

get_extra_SICStus_args(ExtraArgsList) :-
	possibly_environ('EXTRA_SICSTUS_ARGS', ExtraArgs),
	( ExtraArgs == '' ->
	  ExtraArgsList = []
	; split_atom_completely(ExtraArgs, ' ', ExtraArgsList)
	).	

parse_command_line(command_line(Options,Args)) :-
	prolog_flag(argv, RawCommandLine),
	get_extra_SICStus_args(ExtraArgsList),
	append(RawCommandLine, ExtraArgsList, AugmentedCommandLine),
	parse_command_line(AugmentedCommandLine, Options, Args).

parse_command_line([], [], []).
parse_command_line([RawArg|Rest], [Option|RestOptions], RestArgs) :-
	midstring(RawArg, '--', Option, 0),
	check_null_option(Option, '--'),
	!,
	parse_command_line(Rest, RestOptions, RestArgs).
parse_command_line([RawArg|Rest], Options, RestArgs):-
	midstring(RawArg, '-', RawOptions, 0),
	check_null_option(RawOptions, '-'),
	!,
	% This is the SICStus atom_chars/2,
	% which is different from atom_codes/2.
	% This next call is correct!!
	atom_chars(RawOptions, FirstOptions),
	append(FirstOptions, RestOptions, Options),
	parse_command_line(Rest, RestOptions, RestArgs).
parse_command_line([RawArg|Rest], RestOptions, [RawArg|RestArgs]):-
	parse_command_line(Rest, RestOptions, RestArgs).

possibly_environ(EnvironVar, Value) :-
	( environ(EnvironVar, Value) ->
	  true
	; Value = []
	).

check_null_option(Option, Hyphens) :-
	( Option == '' ->
	  fatal_error('Cannot specify "~a" with no option.~n', [Hyphens])
	; true
	).

% remove duplicate options and args, and warn user about duplicates; e.g.,
% update_command_line([mm_data_year,z], ['1011','10'], metamap, OptionsOut, ArgsOut)
% will results in the removal of "z" and "10", so that
% OptionsOut = [mm_data_year] and
% ArgsOut    = ['10'].

% Options is the list of command-line flags, e.g., ['Z', 'V', 'Z', 'V']
% Args    is the list of args to the flags,  e.g., ['2012AB', 'USAbase', '2012AA', 'NLM']

% If the option is repeated and takes an argument,
% unify ArgsNext and the tail of ArgsIn, and unify ArgsOut and ArgsOutRest.
% If the option is repeated and does not take an argument,
%??? unify ArgsNext and ArgsIn and unify ArgsOut with [FirstArg|ArgsOutRest].
% unify ArgsNext and ArgsIn and unify ArgsOut with ArgsOutRest.
set_next_args_1(Program, FirstOption, ArgsIn, ArgsNext, ArgsOut, ArgsOutRest) :-
	  ( option_requires_arg(Program, FirstOption) ->
	    % Must handle the case of ArgsIn being []!
	    ( ArgsIn = [_FirstArg|ArgsNext] ->
	      true
	    ; ArgsNext = ArgsIn
	    ),
	    ArgsOut = ArgsOutRest
	  ; ArgsNext = ArgsIn,
	    ArgsOut = ArgsOutRest
%	    ( ArgsIn = [FirstArg|_] ->
%	      ArgsOut = [FirstArg|ArgsOutRest]
%	    ; ArgsOut = ArgsIn
%	    )
	  ).

% If the option is not repeated and takes an argument,
% unify ArgsNext and the tail of ArgsIn, and unify ArgsOut [FirstArg|ArgsOutRest].
% If the option is not repeated and does not take an argument,
% unify ArgsNext and ArgsIn, and unify ArgsOut with ArgsOutRest.

set_next_args_2(Program, FirstOption, ArgsIn, ArgsNext, ArgsOut, ArgsOutRest) :-
	  ( option_requires_arg(Program, FirstOption) ->
	    ( ArgsIn = [FirstArg|ArgsNext] ->
	      ArgsOut = [FirstArg|ArgsOutRest]
	    ; ArgsNext = ArgsIn,
	      ArgsOut = ArgsIn
	    )
	  ; ArgsNext = ArgsIn,
	    ArgsOut = ArgsOutRest
	  ).

update_command_line([], Args, _Program, [], Args).
update_command_line([FirstOption|RestOptions], ArgsIn, Program, OptionsOut, ArgsOut) :-
	% If the first control option is repeated,
	( repeated_control_option(Program, FirstOption, ArgsIn, RestOptions) ->
	% discard it by simply recursing on the final output variable OptionsOut
	  OptionsNext = OptionsOut,
	  set_next_args_1(Program, FirstOption, ArgsIn, ArgsNext, ArgsOut, ArgsOutRest)	  
	% If the first option is not repeated, keep it.
	; OptionsOut = [FirstOption|OptionsNext],
	  set_next_args_2(Program, FirstOption, ArgsIn, ArgsNext, ArgsOut, ArgsOutRest)
	),
	update_command_line(RestOptions, ArgsNext, Program, OptionsNext, ArgsOutRest).

% possibly_remove_first_arg(ArgsIn, FirstOption, Program, ArgsNext, ArgsOut, ArgsOutRest)
% If the current option being examined takes a mandatory argument,
% and the first arg does not begin with a hyphen, remove the first arg.

%%% possibly_remove_first_arg([], _FirstOption, _Program, [], [], _).
%%% possibly_remove_first_arg([FirstArg|RestArgs], FirstOption, Program, ArgsNext, ArgsOut, ArgsOutRest) :-
%%% 	( option_requires_arg(Program, FirstOption),
%%% 	  atom_codes(FirstArg, FirstArgCodes),
%%% 	  FirstArgCodes = [FirstChar|_],
%%% 	  % FirstArg does not begin with a hyphen (ASCII 45)
%%% 	  FirstChar =\= 45 ->
%%% 	  ArgsOut = ArgsOutRest,
%%% 	  ArgsNext = RestArgs
%%% 	; ArgsOut = [FirstArg|ArgsOutRest],
%%% 	  ArgsNext = [FirstArg|RestArgs]
%%% 	).

member_pos(Element, List, Program, Position) :-
	member_pos_aux(Element, List, Program, 2, Position).

member_pos_aux(Element, [Element|_Rest], _Program, Position, Position).
member_pos_aux(Element, [Other|Rest], Program, PositionIn, Position) :-
	( option_requires_arg(Program, Other) ->
	  PositionNext is PositionIn + 1
	; PositionNext is PositionIn
	),
	member_pos_aux(Element, Rest, Program, PositionNext, Position).
	
repeated_control_option(Program, FirstOption, ArgsIn, RestOptions) :-
	member_pos(OtherOption, RestOptions, Program, Position),
	equivalent_control_options(Program, FirstOption, OtherOption),
	!,
	short_or_long_hyphens(Program, FirstOption, FirstOptionHyphens),
	short_or_long_hyphens(Program, OtherOption, OtherOptionHyphens),
	( option_requires_arg(Program, FirstOption) ->
	  ArgsIn = [Value1|_],
	  nth1(Position, ArgsIn, Value2),
	  send_message('### WARNING: Option "~w~w ~w" overridden by option "~w~w ~w".~n',
		       [FirstOptionHyphens,FirstOption,Value1,OtherOptionHyphens,OtherOption,Value2])
	; send_message('### WARNING: Option "~w~w" overridden by option "~w~w".~n',
		       [FirstOptionHyphens,FirstOption,OtherOptionHyphens,OtherOption])
	).	

equivalent_control_options(Program, Option1, Option2) :-
	( Option1 == Option2 ->
	  true
	  %                          Short    Long
	; is_control_option(Program, Option1, Option2, _, _) ->
	  true
	; is_control_option(Program, Option2, Option1, _, _) ->
	  true
	).

short_or_long_hyphens(Program, Option, OptionHyphens) :-
	( is_control_option(Program, Option, _, _, _) ->
	  OptionHyphens = '-'
	; is_control_option(Program, _, Option, _, _) ->
	  OptionHyphens = '--'
	).

option_requires_arg(Program, Option) :-
	( is_control_option(Program, Option, _, _, ArgSpec) ->
	  compound(ArgSpec)
	; is_control_option(Program, _, Option, _, ArgSpec) ->
	  compound(ArgSpec)
	).

% control_option_requires_argspec(Program, Option) :-
%         ( is_control_option(Program, Option, _LongName, _, aspec(_,_,_,_,_,_)) ->
%           true
%         ; is_control_option(Program, _ShortName, Option, _, aspec(_,_,_,_,_,_))
%         ).


/* interpret_options(+Options, +AllOptions, +Module, -InterpretedOptions)

interpret_options/4 determines InterpretedOptions, a subset of AllOptions
unambiguously found in Options.  Single-character options must be Module
options.  Ambiguous or unknown Options cause failure.  */

interpret_options(Options, AllOptions, Module, IOptions) :-
	% format('All options: ~p~n', [AllOptions]),
	% format('Options: ~p~n', [Options]),
	compute_completions(Options, AllOptions, Module, OptionCompletions),
	% format('Option completions: ~p~n', [OptionCompletions]),
	resolve_options(Options, OptionCompletions, [], IOptions0, ok, StatusOut),
	rev(IOptions0, IOptions),
	% format('Interpreted options: ~p~n', [IOptions]),
	!,
	StatusOut == ok.

compute_completions([],_AllOptions,_Module, []).
compute_completions([First|Rest],AllOptions,Module,
                    [FirstCompletion|RestCompletions]) :-
    compute_completion(First,AllOptions,Module,FirstCompletion),
    compute_completions(Rest,AllOptions,Module,RestCompletions).

compute_completion(_Item, [],_Module, []).
compute_completion(Item,_AllOptions,Module, [iopt(Option,ArgSpec)]) :-
    ( is_control_option(Module,Item,Option,_,ArgSpec) ->
      true
    ; is_control_option(Module,_,Item,_,ArgSpec) ->
      Option = Item
    ),
    !.
% compute_completion(Item, [Option|Rest],Module,
%                    [iopt(Option,ArgSpec)|ComputedRest]) :-
%     midstring(Option,Item,_,0),  % Item is a prefix of Option
%     is_control_option(Module,_,Option,_,ArgSpec),
%     !,
%     compute_completion(Item,Rest,Module,ComputedRest).
compute_completion(Item, [_Option|Rest],Module,ComputedRest) :-
    compute_completion(Item,Rest,Module,ComputedRest).

resolve_options(Options,OptionCompletions,IOptionsIn,IOptionsOut,
                StatusIn,StatusOut) :-
    resolve_items(Options,OptionCompletions,options,IOptionsIn,IOptionsOut,
                  [], [],StatusIn,StatusOut).

resolve_items([], [],_Type,IOptionsIn,IOptionsIn,IArgsIn,IArgsIn,
              StatusIn,StatusIn).
resolve_items([Item|RestItems], [ItemCompletion|RestCompletions],Type,
              IOptionsIn,IOptionsOut,IArgsIn,IArgsOut,StatusIn,StatusOut) :-
    length(ItemCompletion,NCompletions),
    (   NCompletions =:= 0 ->
        (Type==options ->
            fatal_error('Unknown option ~p~n', [Item]),
            IOptionsInOut=IOptionsIn,
            StatusInOut=error
        ;   IOptionsInOut=IOptionsIn,
            IArgsInOut=[Item|IArgsIn],
            StatusInOut=StatusIn
        )
    ;   NCompletions =:= 1 ->
        ItemCompletion=[SingletonCompletion],
        (member(SingletonCompletion,IOptionsIn) ->
            send_message('### WARNING: Duplicate option ~p ignored.~n', [Item]),
            IOptionsInOut=IOptionsIn
        ;   IOptionsInOut=[SingletonCompletion|IOptionsIn]
        ),
        IArgsInOut=IArgsIn,
        StatusInOut=StatusIn
    ;   fatal_error('Ambiguous option ~p (~p).~n', [Item,ItemCompletion]),
        IOptionsInOut=IOptionsIn,
        IArgsInOut=IArgsIn,
        StatusInOut=error
    ),
    resolve_items(RestItems,RestCompletions,Type,IOptionsInOut,IOptionsOut,
                  IArgsInOut,IArgsOut,StatusInOut,StatusOut).


/* interpret_args(+IOptions, +ArgSpecs, +Args, -InterpretedArgs)

interpret_args/4 interprets Args (a list of atoms) first with respect to
those IOptions taking an argument and second with respect to ArgSpecs
(defined below).  It produces a list of InterpretedArgs (defined
below).

    aspec(<spec name>,<option>,<type>,<subtype>,<default>,<description>)
    e.g.,
    aspec(infile,mandatory,file,read,no_default,'Input file')

    <option>  mandatory
              optional
    <type>/<subtype>  none/none
                      list/none
                      file/read
                      file/write
                      file/readable
                      file/writable
                      integer/none
                      number/none

    <default>  no_default
               <default_term>

    <default_term>  or(<default_term>,<default_term>)
                    user_input
                    user_output
                    <atom>
                    <integer>
                    <list> suitable for use with concatenate_items_to_atom/2
                           list items of the form '<<spec name>>' (e.g., 
                           '<infile>') are replaced with the corresponding
                           value before concatenation
                    '<null>'

    iarg(<spec name>,<aspec>,<attributes>)
    e.g.,
    iarg(infile,aspec(infile,mandatory,file,read,no_default,'Input file'),
                [name('my.txt'),stream(<stream>)])

    <attributes>  <list> of <attribute>

    <attribute>  <attribute name>(<value>)

    <attribute name>  name
                      stream
*/

interpret_args(IOptions,ArgSpecs,Args,IArgs) :-
    interpret_option_args(IOptions,Args,ArgsInOut, [],IArgsInOut,ok,StatusInOut),
    interpret_args(ArgSpecs,ArgsInOut,IArgsInOut,IArgsOut,
                   StatusInOut,StatusOut),
    rev(IArgsOut,IArgs),
    !,
    StatusOut==ok.

interpret_option_args([],ArgsIn,ArgsIn,IArgsIn,IArgsIn,StatusIn,StatusIn).
interpret_option_args([iopt(_,none)|Rest],ArgsIn,ArgsOut,IArgsIn,IArgsOut,
                      StatusIn,StatusOut) :-
    interpret_option_args(Rest,ArgsIn,ArgsOut,IArgsIn,IArgsOut,
                          StatusIn,StatusOut).
interpret_option_args([iopt(_Option,ArgSpec)|Rest], [Arg|ArgsInOut],ArgsOut,
                      IArgsIn,IArgsOut,StatusIn,StatusOut) :-
    interpret_arg(ArgSpec,Arg,IArgsIn,IArgsInOut,StatusIn,StatusInOut),
    interpret_option_args(Rest,ArgsInOut,ArgsOut,IArgsInOut,IArgsOut,
                          StatusInOut,StatusOut).
interpret_option_args([iopt(_Option,ArgSpec)|Rest], [], [],IArgsIn,IArgsOut,
                      StatusIn,StatusOut) :-
    interpret_arg(ArgSpec,'<null>',IArgsIn,IArgsInOut,StatusIn,StatusInOut),
    interpret_option_args(Rest, [], [],IArgsInOut,IArgsOut,StatusInOut,StatusOut).

interpret_args([], [],IArgsIn,IArgsIn,StatusIn,StatusIn) :-
    !.
interpret_args([ArgSpec|RestArgSpecs], [Arg|RestArgs],IArgsIn,IArgsOut,
               StatusIn,StatusOut) :-
    interpret_arg(ArgSpec,Arg,IArgsIn,IArgsInOut,StatusIn,StatusInOut),
    interpret_args(RestArgSpecs,RestArgs,IArgsInOut,IArgsOut,
                   StatusInOut,StatusOut).
interpret_args([],Args,IArgsIn,IArgsIn,StatusIn,StatusIn) :-
    !,
    send_message('### WARNING: Extra arguments ~p ignored.~n', [Args]).
interpret_args([ArgSpec|RestArgSpecs], [],IArgsIn,IArgsOut,StatusIn,StatusOut) :-
    interpret_arg(ArgSpec,'<null>',IArgsIn,IArgsInOut,StatusIn,StatusInOut),
    interpret_args(RestArgSpecs, [],IArgsInOut,IArgsOut,StatusInOut,StatusOut).

interpret_arg(ArgSpec,'<null>',IArgsIn,IArgsOut,StatusIn,StatusOut) :-
    !,
    ArgSpec=aspec(SpecName,Option,Type,SubType,Default,Description),
    (Default==no_default ->
        IArgsOut=IArgsIn,
        (Option==mandatory ->
            fatal_error('Mandatory argument~n            ~p (~p)~nhas no value.~n',
			[SpecName,Description])
        ;   StatusOut=StatusIn
        )
    ;   findall(DefaultVal,compute_default_value(Default,IArgsIn,DefaultVal),
                DefaultVals),
        (\+DefaultVals==[] ->
            ((member(DefaultValue,DefaultVals),
              interpret_arg(aspec(SpecName,Option,Type,SubType,
                                  no_default,Description),
                            DefaultValue,
                            IArgsIn,IArgsOut,StatusIn,StatusOut)) ->
                true
            ;   fatal_error('Cannot interpret~n       ~p (~p).~n', [SpecName,Description])
            )
        ;   fatal_error('Cannot compute default for~n       ~p (~p).~n', [SpecName,Description])
        )
    ).
% user_input
interpret_arg(ArgSpec,user_input,
              IArgsIn, [iarg(SpecName,ArgSpec, [name(user_input),
                                              stream(user_input)])|IArgsIn],
              StatusIn,StatusIn) :-
    !,
    ArgSpec=aspec(SpecName,_Option,file,_SubType,_Default,_Description).
% user_output
interpret_arg(ArgSpec,user_output,
              IArgsIn, [iarg(SpecName,ArgSpec, [name(user_output),
                                              stream(user_output)])|IArgsIn],
              StatusIn,StatusIn) :-
    !,
    ArgSpec=aspec(SpecName,_Option,file,_SubType,_Default,_Description).
% file argument
interpret_arg(ArgSpec,Arg,
              IArgsIn, [iarg(SpecName,ArgSpec, [Att|RestAtts])|IArgsIn],
              StatusIn,StatusOut) :-
    ArgSpec=aspec(SpecName,Option,file,SubType,_Default,_Description),
    !,
    Att=name(Arg),
    (   (SubType==read; SubType==write) ->
	( absolute_file_name(Arg, AbsoluteFileName),
	  current_stream(AbsoluteFileName, _, _) ->
	  fatal_error('Cannot use file "~w" for both input and output!~n', [Arg])
	; true
	),
	prolog_flag(fileerrors,_,off),
	(open(Arg, SubType, Stream, [encoding('UTF-8')]) ->
	    prolog_flag(fileerrors,_,on),
	    ( SpecName == 'UDA' ->
	      close(Stream),
	      RestAtts = []
	    ; SpecName == 'nomap' ->
	      close(Stream),
	      RestAtts = []
	    ; RestAtts=[stream(Stream)]
	    ),
	    StatusOut=StatusIn
	;   prolog_flag(fileerrors,_,on),
	    Option==mandatory,
	    fatal_error('Cannot open ~p for ~p operations.~n', [Arg,SubType])
	)
    ;   ((SubType==readable, Mode=read);
	 (SubType==writable, Mode=write)) ->
	RestAtts=[],
	(can_open_file(Arg,Mode) ->
	    StatusOut=StatusIn
	;   Option==mandatory,
	    fatal_error('~p is not ~p.~n', [Arg,SubType])
	)
    ;   fatal_error('Unknown subtype ~p for type file at argument ~p.~n', [SubType,Arg])
    ).
% symbolic argument
interpret_arg(ArgSpec,Arg,
              IArgsIn, [iarg(SpecName,ArgSpec, [Att])|IArgsIn],
              StatusIn,StatusOut) :-
    ArgSpec=aspec(SpecName,_Option,none,_SubType,_Default,_Description),
    !,
    Att=name(Arg),
    StatusOut=StatusIn.
% list argument
interpret_arg(ArgSpec, Arg, IArgsIn, [iarg(SpecName,ArgSpec, [value(Atts)])|IArgsIn],
              StatusIn, StatusOut) :-
	ArgSpec = aspec(SpecName,_Option,list,_SubType,_Default,_Description),
	!,
	( atom_codes(Arg,ArgString),
	  parse_list(ArgString, List) ->
	  % ( SpecName == noexp ->
	  %   reformat_noexp_list(List, Atts)
	  number_codes_list(Atts, List),
	  StatusOut = StatusIn
	; Atts = [],
	StatusOut = error
	).
% numeric argument
interpret_arg(ArgSpec,Arg,
              IArgsIn, [iarg(SpecName,ArgSpec, [Att])|IArgsIn],
              StatusIn,StatusOut) :-
    !,
    ArgSpec=aspec(SpecName,_Option,Type,_SubType,_Default,_Description),
    atom_codes(Arg,ArgString),  % argument is numeric
    ( safe_number_codes(ArgValue,ArgString) ->
      true
    ; fatal_error('Must specify an integer, and not "~w" to specify ~w.~n', [Arg,SpecName]),
      abort
    ),
    Att=value(ArgValue),
    (((Type==integer, integer(ArgValue));
      (Type==number, number(ArgValue))) ->
	StatusOut=StatusIn
    ;   fatal_error('~p is not of type ~p.~n', [Arg,Type]),
	StatusOut=error
    ).

parse_list(InputString,List) :-
    % phrase(l_item_list(List0),InputString),
    split_string_completely(InputString, ",",  List0),
    remove_null_strings(List0,List).

remove_null_strings([], []) :-
    !.
remove_null_strings([""|Rest],ModifiedRest) :-
    !,
    remove_null_strings(Rest,ModifiedRest).
remove_null_strings([First|Rest], [First|ModifiedRest]) :-
    remove_null_strings(Rest,ModifiedRest).


% The argument to --noexp is of the form a:b:c:d,e:f:g:h,
% which must be transformed into [a:[b,c,d], e:[f,g,h]]
% reformat_noexp_list(List, NoExpList) :-
% 	(  foreach(L0, List),
% 	   foreach(L1, NoExpList)
% 	do split_string_completely(L0, ":", StringList),
% 	   atom_codes_list(AtomList0, StringList),
% 	    AtomList0=[H|T],
% 	    L1 = H:T
% 	).

% ---------- GRAMMAR FOR ITEM LIST

% l_item_list(L) --> ([0' ], !, l_item_list(L)
%                ;    l_item(I), [0',], !, l_item_list(M), {L=[I|M]}
%                ;    l_item(I), {L=[I]}
%                    ), !.
% 
% l_item(I) --> ([Char], {\+Char==0',}, l_item(J), {I=[Char|J]}
%           ;    {I=[]}
%               ), !.

compute_default_value(or(Default1,_Default2),IArgs,DefaultValue) :-
    compute_default_value(Default1,IArgs,DefaultValue).
compute_default_value(or(_Default1,Default2),IArgs,DefaultValue) :-
    compute_default_value(Default2,IArgs,DefaultValue),
    !.
compute_default_value(Default,_IArgs,Default) :-
    atom(Default).
compute_default_value(Default,_IArgs,Default) :-
    integer(Default).
compute_default_value(Default,IArgs,DefaultValue) :-
    compute_default_components(Default,IArgs,DefaultComponents),
    concatenate_items_to_atom(DefaultComponents,DefaultValue).

compute_default_components([],_IArgs, []).
compute_default_components([DefaultItem|RestDefaultItems],IArgs,
                           [Component|RestComponents]) :-
    ((midstring(DefaultItem,'<',StrippedLeft,0),
      midstring(StrippedLeft,Stripped,'>',0)) ->
        get_from_iargs(Stripped,name,IArgs,Component),
        \+illegal_default_component(Component)
    ;   Component=DefaultItem
    ),
    compute_default_components(RestDefaultItems,IArgs,RestComponents).


/* get_from_iargs(+Arg, +Attribute, +IArgs, -Value)
   get_from_iarg(+Arg, +Attribute, +IArg, -Value)
   get_from_attributes(+Attribute, +Attributes, -Value)

get_from_iargs/4 returns the Value for Attribute of Arg found in IArgs
(interpreted arguments; cf. interpret_args/3).  get_from_iarg/4 and
get_from_attributes/3 are auxiliaries.
get_from_iargs/4 and get_from_attributes/3 are nondeterminate.  */

get_from_iargs(_Arg,_Attribute, [],_Value) :-
    !,
    fail.
get_from_iargs(Arg,Attribute, [IArg|_Rest],Value) :-
    get_from_iarg(Arg,Attribute,IArg,Value).
get_from_iargs(Arg,Attribute, [_IArg|Rest],Value) :-
    get_from_iargs(Arg,Attribute,Rest,Value).

get_from_iarg(Arg,Attribute,iarg(Arg,_ArgSpec,Attributes),Value) :-
    get_from_attributes(Attribute,Attributes,Value).

get_from_attributes(_Attribute, [],_Value) :-
    !,
    fail.
get_from_attributes(Attribute, [AttributeTerm|_Rest],Value) :-
    functor(AttributeTerm,Attribute,1),
    arg(1,AttributeTerm,Value).
get_from_attributes(Attribute, [_First|Rest],Value) :-
    get_from_attributes(Attribute,Rest,Value).

illegal_default_component(user_input).
illegal_default_component(user_output).

conditionally_announce_program_name(ProgramName, DefaultRelease) :-
	( \+ control_option(silent) ->
	  atom_codes(DefaultRelease,    [D1,D2,D3,D4|_]),
	  atom_codes(ReleaseYear, [D1,D2,D3,D4]),
	    send_message('~n~w (~a)~n~n', [ProgramName,ReleaseYear])
	; true
	).

% Assert if not already asserted!
assert_control_option(Option) :-
	get_program_name(ProgramName),
	( Option == '' ->
	  fatal_error('"~a" is not a valid option for program "~a"!~n', [Option, ProgramName])
	; control_option(Option) ->
	  true
	; is_control_option(ProgramName, _ShortOption, Option, _, _ArgSpec) ->
	  assert(control_option(Option))
	; is_control_option(ProgramName, Option, LongOption, _, _ArgSpec) ->
	  assert(control_option(LongOption))
	; fatal_error('Option "~a" is not a valid option for program "~a"!~n', [Option, ProgramName])
	).

assert_control_value(Option, Value) :-
	get_program_name(ProgramName),
	% don't assert streams, because for "--UDA UDAfile",
	% we want the control value to be "UDAfile" and not the stream.
	( current_stream(_, _, Value) ->
	  true
	; is_control_option(ProgramName, _ShortOption, Option, _, ArgSpec),
	  compound(ArgSpec)  ->
	  retractall(control_value(Option, _V)),
	  assert(control_value(Option, Value))
	; is_control_option(ProgramName, Option, LongOption, _, ArgSpec),
	  compound(ArgSpec)  ->
	  retractall(control_value(LongOption, _V)),
	  assert(control_value(LongOption, Value))
	; fatal_error('~a/~a is not a valid option/arg combination for program "~a"!',
		      [Option, Value, ProgramName])	
	).

verify_options_and_values :-
	( control_option(Option),
	  is_control_option(_ProgramName, _ShortOption, Option, _, ArgSpec),
	  compound(ArgSpec),
	  \+ control_value(Option, _Value),
	  fatal_error('The "~w" option requires an argument!\n', [Option])
	; true
	).

set_default_lexicon :-
	( \+ control_value(lexicon, _) ->
	   assert(control_value(lexicon, db))
         ; true
	 ).

% assert_control_value(Option, Attribute, Value) :-
% 	( control_value(Option, Attribute, Value) ->
% 	  true
% 	; assert(control_value(Option, Attribute, Value))
% 	).
