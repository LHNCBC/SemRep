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

% File:	    skr_utilities.pl
% Module:   SKR
% Author:   Lan
% Purpose:  Utilities

:- module(skr_utilities, [
	basename/2,
	check_valid_file_type/2,
	compare_utterance_lengths/2,
	compute_sum/3,
	conditionally_print_end_info/2,
	conditionally_skr_begin_write/2,
	conditionally_skr_end_write/2,
	conditionally_print_header_info/4,
	convert_non_text_fields_to_blanks/6,
	debug_call/2,
	debug_message/3,
	do_formal_tagger_output/0,
	do_sanity_checking_and_housekeeping/4,
	ensure_atom/2,
 	ensure_number/2,
 	expand_split_word_list/2,
	fatal_error/2,
	generate_AAs_MMO_term/2,
	generate_bracketed_output/2,
	generate_candidates_output/8,
	% generate_header_output/6,
	generate_mappings_output/6,
	generate_MMO_terms/9,
	generate_phrase_output/7,
	generate_utterance_output/6,
	generate_variants_output/2,
	generate_EOT_output/1,
	get_candidate_feature/3,
	get_all_candidate_features/3,
	% called by MetaMap API -- do not change signature!
	output_should_be_bracketed/1,
	% member_var/2,
	memberchk_var/2,
	output_tagging/3,
	replace_crs_with_blanks/4,
	replace_blanks_with_crs/4,
	send_message/2,
	set_message/5,
	skr_begin_write/1,
	skr_end_write/1,
	% must be exported for mm_print
	skr_write_phrase/1,
	skr_write_string/1,
	split_word/3,
	token_template/5,
	token_template/6,
	usage/0,
	write_raw_token_lists/2,
	write_sentences/2,
	write_token_list/3
    ]).

:- use_module(lexicon(qp_lexicon), [
	concat_atoms_intelligently/2
    ]).

:- use_module(metamap(metamap_variants), [
	write_all_variants/4
    ]).

:- use_module(metamap(metamap_utilities), [
	candidate_term/16,
	% dump_aphrase_mappings/3,
	dump_mappings/3,
	% dump_evaluations_indented/6,
	num_dump_evaluations_indented/8,
	write_list_indented/1
    ]).

:- use_module(skr(skr), [
	% aev_print_version/2 is not explicitly called here,
	% but it must still be imported because they're called via debug_call.
	aev_print_version/2,
	% print_all_aevs/1,
	% print_candidate_grid/6 and print_duplicate_info/4 are not explicitly called here,
	% but they must still be imported because they're called via debug_call.
	print_candidate_grid/6,
	print_duplicate_info/4,
	stop_and_halt/0
    ]).

:- use_module(skr(skr_json), [
	json_output_format/1
    ]).

:- use_module(skr(skr_text_processing), [
	medline_field_separator_char/1
    ]).

:- use_module(skr(skr_umls_info), [
	verify_sources/1,
	verify_sts/1
    ]).

:- use_module(skr(skr_xml), [
	xml_output_format/1
    ]).

:- use_module(skr_lib(negex), [
	default_negex_semtypes/1
    ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	form_one_string/3,
	is_print_string/1,
	trim_whitespace_left_count/3,
	trim_whitespace_left/2,
	trim_whitespace_right/2
    ]).

:- use_module(skr_lib(nls_system), [
	assert_control_value/2,
	control_option/1,
	control_value/2,
	display_control_options_for_modules/2,
	display_mandatory_metamap_options/0,
	display_current_control_options/2,
	get_program_name/1
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	concat_atom/3,
	concat_strings_with_separator/3,
	ttyflush/0
    ]).


:- use_module(text(text_object_util), [
	annotation_type/1,
	ex_lbracket_char/1,
	ex_rbracket_char/1,
	ex_lbracket_tok/1,
	gather_whitespace/3,
	higher_order_tok/1,
	higher_order_type/1,
	higher_order_or_annotation_type/1,
	pe_tok/1,
	ws_char/1,
	ws_tok/1
   ]).		     

:- use_module(text(text_objects), [
	extract_an_lc_atoms/2,
	extract_token_atoms/2
   ]).		     

:- use_module(library(avl), [
	avl_member/3,
	avl_to_list/2
    ]).

:- use_module(library(codesio), [
	with_output_to_codes/2
    ]).

:- use_module(library(file_systems), [
	file_exists/1,
	file_exists/2
   ]).

:- use_module(library(lists), [
	append/2,
	delete/3,
	last/2,
	prefix/2,
	selectchk/3
   ]).

:- use_module(library(sets), [
	intersection/3,
	subtract/3
    ]).

:- use_module(library(system), [
	environ/2
    ]).

skr_begin_write(Label) :-
	format('>>>>> ~p~n', [Label]).

skr_write_list(List) :-
	format('[~n', []),
	skr_write_each(List),
	format(']~n', []).

skr_write_each([]).
skr_write_each([First|Rest]) :-
	format(' ~q', [First]),
	( Rest == [] ->
	  format('~n', [])
	; format(',~n', [])
	),
	skr_write_each(Rest).

skr_write_phrase(Phrase) :-
    skr_write_terms(Phrase).

skr_write_string(String) :-
    format('~s~n',[String]).

skr_write_strings([]).
skr_write_strings([First|Rest]) :-
    format('~s~n',[First]),
    skr_write_strings(Rest).

skr_write_terms([]).
skr_write_terms([First|Rest]) :-
    format('  ~p~n',[First]),
    skr_write_terms(Rest).

skr_end_write(Label) :-
    format('<<<<< ~p~n',[Label]).

% Output routines for debugging

write_sentences(CoordSentences, Sentences) :-
	( control_value(debug, DebugFlags),
	  memberchk(sentences, DebugFlags) ->
	  format(user_error, '### Sentences:~n', []),
	  write_token_list(CoordSentences, 0, 1),
	  format(user_error, '### CoordSentences:~n', []),
	  write_token_list(Sentences, 0, 1)
	; true
	).

write_raw_token_lists(ExpRawTokenList, UnExpRawTokenList) :-
	( control_value(debug, DebugFlags),
	  memberchk(tokens, DebugFlags) ->
	  format(user_error, '~n~n### ExpRawTokenList:~n', []),
	  write_token_list(ExpRawTokenList, 0, 1),
	  format(user_error, '~n~n### UnExpRawTokenList:~n', []),
	  write_token_list(UnExpRawTokenList, 0, 1)
	; true
	).


write_token_nl(0).
write_token_nl(1) :- format(user_error, '~n', []).

write_token_list([], _Indent, NL) :- write_token_nl(NL), ttyflush.
write_token_list([Token|RestTokens], Indent, NL) :-
	write_token(Token, Indent, NL),
	% write(Token), nl,
	write_token_list(RestTokens, Indent, NL).

write_token(Token, Indent, NL) :-
	arg(1, Token, Type),
	( annotation_type(Type) ->
	  write_complex_token(Token, Indent, NL)
	; write_simple_token(Token, Indent, NL)
	).

write_simple_token_list([SimpleToken|RestSimpleTokens], Indent) :-
	IndentP1 is Indent + 1,
	format(user_error, '~*c[', [Indent, 32]),
	write_simple_token(SimpleToken, 0, 0),
	write_rest_simple_token_list(RestSimpleTokens, IndentP1).

write_rest_simple_token_list([], _Indent) :-
	format(user_error, '],~n', []).
write_rest_simple_token_list([H|T], Indent) :-
	format(user_error, ',~n', []),
	write_simple_token(H, Indent, 0),
	write_rest_simple_token_list(T, Indent).

write_pos_term_list([H|T], Indent, NL) :-
	format(user_error, '~*c~w', [Indent, 32, H]),
	write_rest_pos_term_list(T, Indent, NL).

write_rest_pos_term_list([], _Indent, NL) :-
	write_pos_term_list_nl(NL).
write_rest_pos_term_list([H|T], Indent, NL) :-
	format(user_error, ',~n~*c~w', [Indent, 32, H]),
	write_rest_pos_term_list(T, Indent, NL).

write_pos_term_list_nl(0) :- format(user_error, ')', []).
write_pos_term_list_nl(1) :- format(user_error, ')~n', []).


write_complex_token(Token, Indent, NL) :-
	Token =.. [tok, Type, TokenList1, TokenList2|PosTerms],
	write_complex_token_1(Type, Indent, TokenList1, TokenList2, PosTerms, NL).

write_complex_token_1(aadef, Indent, TokenList1, TokenList2, PosTerms, _NL) :-
	format(user_error, '~w~n', ['tok(aadef,']),
	IndentP4 is Indent + 4,
	write_simple_token_list(TokenList1, IndentP4),
	write_simple_token_list(TokenList2, IndentP4),
	write_pos_term_list(PosTerms, IndentP4, 1).
	       
write_complex_token_1(aa, Indent, AATokenList1, [AADefToken], AAPosTermList, _NL) :-
	format(user_error, '~w~n', ['tok(aa,']),
	IndentP4 is Indent + 4,
	write_simple_token_list(AATokenList1, IndentP4),
	format(user_error, '~*c~w~n', [IndentP4, 32, '[tok(aadef,']),
	AADefToken =.. [tok, aadef, AADefTokenList1, AADefTokenList2|AADefPosTermList],
	IndentP8 is Indent + 8,
	write_simple_token_list(AADefTokenList1, IndentP8),
	write_simple_token_list(AADefTokenList2, IndentP8),
	write_pos_term_list(AADefPosTermList, IndentP8, 0),
	format(user_error, '],~n', []),
	write_pos_term_list(AAPosTermList, IndentP4, 1).

write_simple_token(Token, Indent, NL) :-
	Token =.. [tok, Type, TokenString, LCTokenString|PosTerms],
	make_atom(TokenString,   TokenAtom),
	make_atom(LCTokenString, LCTokenAtom),
	assemble_format_string(PosTerms, FormatString, NL),
	format(user_error, FormatString, [Indent, 32, Type, TokenAtom, LCTokenAtom|PosTerms]).

assemble_format_string(PosTerms, FormatString, NL) :-
	PosTerms = [H|T],
	assemble_format_atoms(T, H, NL, FormatAtoms),
	concat_atom(['~*ctok(~q,~q,~q,'|FormatAtoms], FormatString).

assemble_format_atoms([], _Last, NL, [NLFormatAtom]) :-
	assemble_format_atoms_nl(NL, NLFormatAtom).
assemble_format_atoms([NextPosTerm|RestPosTerms], _ThisPosTerm, NL, ['~w,'|RestFormatAtoms]) :-
	assemble_format_atoms(RestPosTerms, NextPosTerm, NL, RestFormatAtoms).

assemble_format_atoms_nl(0, '~w)').
assemble_format_atoms_nl(1, '~w)~n').

make_atom(String, Atom) :-
	( atomic(String) ->
	  Atom = String
	; atom_codes(Atom, String)
	).


do_sanity_checking_and_housekeeping(ProgramName, DefaultRelease, InputStream, OutputStream) :-
	verify_model_settings(ModelSettings),
	verify_tagger_output_settings(TaggerOutputSettings),
	verify_single_xml_output_format(SingleXMLOutputFormat),
	verify_single_json_output_format(SingleJSONOutputFormat),
	verify_single_output_format(SingleOutputFormat),
        verify_xml_settings(XMLSettings),
        verify_json_settings(JSONSettings),
        verify_MMO_settings(MMOSettings),
        verify_mmi_settings(MMISettings),
	verify_sources_options(SourcesOptions),
	verify_sources_values(SourcesValues),
	verify_semantic_type_options(SemTypeOptions),
	verify_semantic_type_values(SemTypeValues),
	verify_no_nums_semantic_type_values(NoNumsSemTypeValues),
	% verify_gap_size_values(GapSizeValues),
	verify_acros_abbrs_settings(AcrosAbbrsSettings),
	verify_derivational_variants_settings(DerivationalVariantsSettings),
	verify_tagger_server_settings(TaggerServerSettings),
	verify_WSD_server_settings(WSDServerSettings),
	verify_pruning_settings(PruningSettings),
	verify_sldi_settings(SLDISettings),
	verify_negex_trigger_setting(NegExTriggerSetting),
	verify_negex_settings(NegExSettings),
	% verify_lexicon_setting(LexiconSetting),
	verify_composite_phrases_setting(CompositePhrasesSetting),
	verify_blanklines_setting(BlankLinesSetting),
	verify_number_the_candidates_setting(NumberTheCandidatesSetting),
	verify_number_the_mappings_setting(NumberTheMappingsSetting),
	verify_hide_mappings_setting_1(HideMappingsSetting1),
	verify_hide_mappings_setting_2(HideMappingsSetting2),
	verify_all_results([ModelSettings, TaggerOutputSettings,
			    SingleOutputFormat, SingleXMLOutputFormat, SingleJSONOutputFormat,
			    XMLSettings, JSONSettings, MMOSettings, MMISettings,
			    SourcesOptions, SourcesValues,
			    SemTypeOptions, SemTypeValues, NoNumsSemTypeValues,
			    AcrosAbbrsSettings, DerivationalVariantsSettings,
			    TaggerServerSettings, WSDServerSettings,
			    PruningSettings, SLDISettings, NegExTriggerSetting, NegExSettings,
			    % LexiconSetting, CompositePhrasesSetting, BlankLinesSetting,
			    CompositePhrasesSetting, BlankLinesSetting,
			    NumberTheCandidatesSetting, NumberTheMappingsSetting,
			    HideMappingsSetting1,HideMappingsSetting2],
			   InputStream, OutputStream),
	display_current_control_options(ProgramName, DefaultRelease).

verify_model_settings(Result) :-
	( control_option(strict_model),
	  control_option(relaxed_model) ->
	  send_message('~n### MetaMap ERROR: Both strict_model and relaxed_model have been specified.~n', []),
	  Result is 1
	; Result is 0
	).

% Error if both tagger_output and formal_tagger_output are set
verify_tagger_output_settings(Result) :-
	( control_option(tagger_output),
	  control_option(formal_tagger_output) ->
	  send_message('~n### MetaMap ERROR: Only one of --tagger_output and ', []),
	  send_message('--formal_tagger_output can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

% Error if more than one of MMI, XML MMI, and JSON is set
% verify_single_output_format(Result) :-
% 	( control_option(machine_output), 
% 	  xml_output_format(XMLFormat) ->
% 	  send_message('~n### MetaMap ERROR: Only one of --machine_output and ', []),
% 	  send_message('--~w can be specified.~n', [XMLFormat]),
% 	  Result is 1 
% 	; Result is 0
%	).


verify_single_xml_output_format(Result) :-
	( control_option('XMLf') ->
	  XMLf is 1
	; XMLf is 0
	),
	( control_option('XMLn') ->
	  XMLn is 1
	; XMLn is 0
	),
	( control_option('XMLf1') ->
	  XMLf1 is 1
	; XMLf1 is 0
	),
	( control_option('XMLn1') ->
	  XMLn1 is 1
	; XMLn1 is 0
	),
	Total is XMLf + XMLn + XMLf1 + XMLn1,
	( Total > 1 ->
	  send_message('~n### MetaMap ERROR: Only one XML output format can be selected!~n', []),
	  Result is 1
	; Result is 0
	).


verify_single_json_output_format(Result) :-
	( control_option('JSONf') ->
	  JSONf is 1
	; JSONf is 0
	),
	( control_option('JSONn') ->
	  JSONn is 1
	; JSONn is 0
	),
	Total is JSONf + JSONn,
	( Total > 1 ->
	  send_message('~n### MetaMap ERROR: Only one JSON output format can be selected!~n', []),
	  Result is 1
	; Result is 0
	).

verify_single_output_format(Result) :-
	( control_option(machine_output) ->
	  MMO is 1
	; MMO is 0
	),
	( xml_output_format(_XMLFormat) ->
	  XML is 1
	; XML is 0
	),
	( control_option(fielded_mmi_output) ->
	  MMI is 1
	; MMI is 0
	),
	( json_output_format(_JSONFormat) ->
	  JSON is 1
	; JSON is 0
	),
	Total is MMO + XML + MMI + JSON,
	( Total > 1 ->
	  send_message('~n### MetaMap ERROR: Only one of --machine_output, XML output, ', []),
	  send_message('JSON output, and fielded MMI output can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

verify_xml_settings(0) :-
	( xml_output_format(XMLFormat) ->
	    warning_check([syntax, short_semantic_types, show_cuis, sources, dump_aas,
			   hide_plain_syntax, number_the_candidates, hide_mappings], XMLFormat)
	; true
	).

verify_json_settings(0) :-
	( json_output_format(JSONFormat) ->
	  warning_check([syntax, short_semantic_types, show_cuis, sources, dump_aas,
			 hide_plain_syntax, number_the_candidates, hide_mappings], JSONFormat)
	; true
	).

verify_MMO_settings(0) :-
	( control_option(machine_output) ->
	  warning_check([syntax, show_cuis, sources, dump_aas, hide_plain_syntax,
		         number_the_candidates, short_semantic_types, hide_mappings], machine_output)
	; true
	).

verify_mmi_settings(0) :-
	( control_option(fielded_mmi_output) ->
	  warning_check([show_cuis, show_candidates, hide_mappings, negex, syntax,
		         tagger_output, hide_plain_syntax, number_the_candidates,
		         short_semantic_types, sources], fielded_mmi_output)
	; true
	).

% Error if both restrict_to_sources and exclude_sources are set
verify_sources_options(Result) :-
	( control_option(restrict_to_sources),
	  control_option(exclude_sources) ->
	  send_message('~n### MetaMap ERROR: Only one of --restrict_to_sources and ', []),
	  send_message('--exclude_sources can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

% Error if either restrict_to_sources and exclude_sources is set, and invalid source is specified
verify_sources_values(Result) :-
	( control_value(restrict_to_sources,Sources)
	; control_value(exclude_sources,Sources)
	),
	!,
	( verify_sources(Sources) ->
	  Result is 0
	; Result is 1
	).
verify_sources_values(0).

% Error if both restrict_to_sts and exclude_sts are set
verify_semantic_type_options(Result) :-
	( control_option(restrict_to_sts),
	  control_option(exclude_sts) ->
	  send_message('~n### MetaMap ERROR: Only one of --restrict_to_sts and ', []),
	  send_message('--exclude_sts can be specified.~n', []),
	  Result is 1
	; Result is 0
	).

% Error if either restrict_to_sts and exclude_sts is set and invalid ST is specified
verify_semantic_type_values(Result) :-
	( control_value(restrict_to_sts, STs)
	; control_value(exclude_sts, STs)
	),
	!,
	( verify_sts(STs) ->
	  Result is 0
	; Result is 1
	).
verify_semantic_type_values(0).

verify_no_nums_semantic_type_values(Result) :-
	control_value(no_nums, STs0),
	delete(STs0, all,   STs1),
	delete(STs1, 'ALL', STs2),
	( verify_sts(STs2) ->
	  Result is 0
	; Result is 1
	).
verify_no_nums_semantic_type_values(0).
	

%%% % Error if invalid gap size is specified (obsolete)
%%% verify_gap_size_values(Result) :-
%%% 	control_value(gap_size, GapSize),
%%% 	!,
%%% 	( verify_gap_size_values_aux(GapSize) ->
%%% 	  Result is 0
%%% 	; send_message('gap_size argument must be of form Int1,Int2 ', []),
%%% 	  send_message('with Int1 >= Int2+2~n', []),
%%% 	  Result is 1
%%% 	).
%%% verify_gap_size_values(0).
%%% 
%%% verify_gap_size_values_aux(GapSize) :-
%%% 	GapSize = [MinPhraseLength, MaxGapSize],
%%% 	integer(MinPhraseLength),
%%% 	integer(MaxGapSize),
%%% 	MinPhraseLength >= MaxGapSize + 2.

% Error if both all_acros_abbrs and unique_acros_abbrs_only are set
verify_acros_abbrs_settings(Result) :-
	( control_option(all_acros_abbrs),
	  control_option(unique_acros_abbrs_only) ->
	  send_message('~n### MetaMap ERROR: Only one of --all_acros_abbrs and ', []),
	  send_message('--unique_acros_abbrs_only can be specified.~n', []),
	  Result is 1
	; Result is 0
	).			    

% Error if both all_derivational_variants and no_derivational_variants are set
verify_derivational_variants_settings(Result) :-
	( control_option(all_derivational_variants),
	  control_option(no_derivational_variants) ->
	  send_message('~n### MetaMap ERROR: Only one of --all_derivational_variants and ', []),
	  send_message('--no_derivational_variants can be specified.~n', []),
	  Result is 1
	; Result is 0
	).			    
% Error if no_tagging is specified, but a specific tagger server is specified
verify_tagger_server_settings(Result) :-
	( control_option(no_tagging),
	  control_value(tagger, ChosenTaggerServer) ->
	  send_message('~n### MetaMap ERROR: If no_tagging is specified, ', []),
	  send_message('a specific tagger (~s) cannot be chosen.~n', [ChosenTaggerServer]),
	  Result is 1
	; Result is 0
	).

% Error if WSD is not specified, but a specific WSD server is specified
verify_WSD_server_settings(Result) :-
	( \+ control_option(word_sense_disambiguation),
	  control_value('WSD_SERVER', ChosenWSDServer) ->
	  send_message('~n### MetaMap ERROR: If WSD is not specified, ', []),
	  send_message('a specific WSD server (~s) cannot be chosen.~n', [ChosenWSDServer]),
	  Result is 1
	; Result is 0
	).			    

% Error if both --prune and --no_prune are specified.
verify_pruning_settings(Result) :-
	( control_value(prune, _),
	  control_option(no_prune) ->
	  send_message('~n### MetaMap ERROR: Cannot specify --prune and --no_prune.~n', []),
	  Result is 1
	; Result is 0
	).			    

% Error if both --sldi and --sldiID are specified.
verify_sldi_settings(Result) :-
	( control_option(sldi),
	  control_option(sldiID) ->
	  send_message('~n### MetaMap ERROR: Cannot specify --sldi and --sldiID.~n', []),
	  Result is 1
	; Result is 0
	).			    

% Error if any of negex_trigger_file is set, but negex is not!
verify_negex_trigger_setting(Result) :-
	( control_value(negex_trigger_file, _) ->
	  load_negex_trigger_file,
	  Result is 0
	; Result is 0
	).

load_negex_trigger_file :-
	( control_value(negex_trigger_file, NegExTriggerFile) ->
	  prolog_flag(redefine_warnings, CurrentFlag),
	  prolog_flag(redefine_warnings, CurrentFlag, off),
	  negex_load(NegExTriggerFile),
	  prolog_flag(redefine_warnings, off, CurrentFlag)
	; true
	).

negex_load(NegExTriggerFile) :-
	atom_length(NegExTriggerFile, FileNameLength),
	BannerLength is FileNameLength + 40,
	send_message('~n~*c~n###### Loading NegEx trigger file ~w #####~n~*c~n',
	       [BannerLength,35,NegExTriggerFile,BannerLength,35]),
	compile(NegExTriggerFile).


% Error if any of negex_st_add, negex_st_del, negex_st_set is set and invalid ST is specified.
% Each of those three options, if set, contributes 1 to Total.
% If Total > 1, more than one of the three options has been set,
% so declare a fatal error.
verify_negex_settings(Result) :-	
	% If the user has specified negex_st_add,
	( control_value(negex_st_add, SemTypesToAdd0) ->
	  sort(SemTypesToAdd0, SemTypesToAdd1),
	  % remove all and 'ALL' from the SemTypes
	  subtract(SemTypesToAdd1, [all,'ALL'], SemTypesToAdd),
	  % and verify that all remaining specified SemTypes are valid.
	  % verify_sts/1 will throw a fatal error if an invalid ST is found.
	  verify_sts(SemTypesToAdd),
	  verify_useless_sts(add, SemTypesToAdd),
	  ADD is 1
	; ADD is 0
	),
	( control_value(negex_st_del, SemTypesToDel0) ->
	  sort(SemTypesToDel0, SemTypesToDel),
	  verify_all_del(SemTypesToDel),
	  verify_sts(SemTypesToDel),
	  verify_useless_sts(del, SemTypesToDel),
	  DEL is 1
	; DEL is 0
	),
	( control_value(negex_st_set, SemTypesToSet0) ->
	  subtract(SemTypesToSet0, [all,'ALL'], SemTypesToSet),
	  verify_sts(SemTypesToSet),
	  SET is 1
	; SET is 0
	),
	% Do not allow SET along with either ADD or DEL
	( 1 is SET /\ ( ADD \/ DEL ) ->
	  send_message('~n### MetaMap ERROR: --negex_st_set cannot be combined ', []),
	  send_message('with either --negex_st_add or --negex_st_del.~n', []),
	  Result is 1
	; Result is 0
	).

% Called by verify_negex_settings/1
verify_useless_sts(add, SemTypesToAdd) :-
	default_negex_semtypes(DefaultNegExSemTypes),
	intersection(SemTypesToAdd, DefaultNegExSemTypes, Intersection),
	( Intersection \== [] ->
	  set_message(Intersection, PluralIndicator, Verb, Pronoun, Determiner),
	  send_message('~n### WARNING: SemType~w ~p ~w~w default NegEx SemType~w.~n',
		       [PluralIndicator,Intersection,Verb,Determiner,PluralIndicator]),
	  send_message('             Adding ~w has no effect.~n', [Pronoun])
	; true
	).
verify_useless_sts(del, SemTypesToAdd) :-
	default_negex_semtypes(DefaultNegExSemTypes),
	subtract(SemTypesToAdd, DefaultNegExSemTypes, LeftOvers),
	( LeftOvers \= [] ->
	  set_message(LeftOvers, PluralIndicator, Verb, Pronoun, Determiner),
	  send_message('~n### WARNING: SemType~w ~p ~w not~w default NegEx SemType~w.~n',
		       [PluralIndicator,LeftOvers,Verb,Determiner,PluralIndicator]),
	  send_message('             Deleting ~w has no effect.~n', [Pronoun])

	; true
	).

% Called by verify_negex_settings/1
verify_all_del(SemTypesToDel) :-
	intersection(SemTypesToDel, [all,'ALL'], Intersection),
	( Intersection = [_|_] ->
	  fatal_error('"all" and "ALL" cannot be used with negex_st_del~n.', [])
	; true
	).

% verify_lexicon_setting(LexiconSetting) :-
% 	( \+ control_value(lexicon, c),
% 	  \+ control_value(lexicon, db) ->
% 	  send_message('~n### MetaMap ERROR: --lexicon setting must be either c or db!~n', []),
% 	  LexiconSetting is 1
% 	; control_value(lexicon, c),
% 	  control_value(lexicon, db) ->
% 	  send_message('~n### MetaMap ERROR: --lexicon setting must be either c or db!~n', []),
% 	  LexiconSetting is 1
% 	; LexiconSetting is 0
% 	).

verify_composite_phrases_setting(CompositePhrasesSetting) :-
	( \+ control_value(composite_phrases, _) ->
	  CompositePhrasesSetting is 0
	; control_value(composite_phrases, Value),
	  ( Value > 12 ->
	  send_message('~n### WARNING: --composite_phrases (-Q) setting of ~d > maximum of 12; using 12 instead.~n',
		       [Value]),
	    assert_control_value(composite_phrases, 12)
	  ; true
	  ),
	  CompositePhrasesSetting is 0
	).

verify_blanklines_setting(BlankLinesSetting) :-
	control_value(blanklines, N),
	!,
	( N >= 0 ->
	  BlankLinesSetting is 0
	; send_message('~n### MetaMap ERROR: --blanklines setting must be a nonnegative integer.~n', []),
	  BlankLinesSetting is 1
	).
verify_blanklines_setting(0).

verify_number_the_candidates_setting(NumberTheCandidatesSetting) :-
	( control_option(number_the_candidates),
	  \+ control_option(show_candidates) ->
	   send_message('~n### MetaMap ERROR: --number_the_candidates cannot be used without --show_candidates!~n', []),
	   NumberTheCandidatesSetting is 1
	;  NumberTheCandidatesSetting is 0
	).

verify_number_the_mappings_setting(NumberTheMappingsSetting) :-
	( control_option(number_the_mappings),
	  control_option(hide_mappings) ->
	   send_message('~n### MetaMap ERROR: --number_the_mappings cannot be used with --hide_mappings!~n', []),
	   NumberTheMappingsSetting is 1
	;  NumberTheMappingsSetting is 0
	).

verify_hide_mappings_setting_1(HideMappingsSetting1) :-
	( \+ control_option(show_candidates),
	  control_option(hide_mappings) ->
	  send_message('~n### MetaMap ERROR: --hide_mappings cannot be used without --show_candidates!~n', []),
	   HideMappingsSetting1 is 1
	;  HideMappingsSetting1 is 0
	).

verify_hide_mappings_setting_2(HideMappingsSetting2) :-
	( control_option(hide_mappings),
	  control_option(compute_all_mappings) ->
	  send_message('~n### MetaMap ERROR: --hide_mappings cannot be used with --compute_all_mappings!~n', []),
	   HideMappingsSetting2 is 1
	;  HideMappingsSetting2 is 0
	).

verify_all_results(ValuesList, InputStream, OutputStream) :-
	compute_sum(ValuesList, 0, Result),
	( Result =:= 0 ->
	  true
	; generate_EOT_output(OutputStream),
	  close(InputStream),
	  close(OutputStream),
	  stop_and_halt
	).	

warning_check([], _OutputOption).
warning_check([WarningOption|RestOptions], OutputOption) :-
	( control_option(WarningOption) ->
	  send_message('~N### WARNING: The ~w option has no effect on ~w output.~n',
		       [WarningOption,OutputOption])
	; true
	),
	warning_check(RestOptions, OutputOption).

% fatal_error_check([], _OutputOption, Result, Result).
% fatal_error_check([FatalErrorOption|RestOptions], OutputOption, ResultIn, ResultOut) :-
% 	( control_option(FatalErrorOption) ->
% 	  send_message('~n### MetaMap ERROR: The --~w option cannot be used with ~w output.~n',
% 		       [FatalErrorOption, OutputOption]),
% 	  ResultNext is 1
% 	; ResultNext is ResultIn
% 	),
% 	fatal_error_check(RestOptions, OutputOption, ResultNext, ResultOut).

send_message(Message, Format):-
	% send message to stderr only, not to the output file, if there is one
	format(user_error, Message, Format).

/* usage

usage/0 displays skr/metamap usage.  */

usage :-
    format('~nUsage: metamap [<options>] [<infile> [<outfile>]]~n~n', []),
    format('  <infile> contains text in one of several forms (default is user_input), and~n', []),
    format('  <outfile> is a file for results (default is <infile>.out).~n~n', []),
    display_mandatory_metamap_options,
    display_control_options_for_modules(metamap, []).

% format_cmd_line_for_machine_output(IArgs, IOptions, MachineOutputArgs)

% IOptions looks like
% [iopt(exclude_sts,
%       aspec(exclude_sts,mandatory,list,none,no_default,'List of semantic types to exclude.')),
%  iopt(relaxed_model,none),
%  iopt(mm_data_year,
%       aspec(mm_data_year,mandatory,none,none,no_default,'Year of MetaMap data to use.'))]

% Iargs looks like
% [iarg(exclude_sts,
%       aspec(exclude_sts,mandatory,list,none,no_default,'List of semantic types to exclude.'),
%       [value([neop,gngm])]),
%  iarg(mm_data_year,
%       aspec(mm_data_year,mandatory,none,none,no_default,'Year of MetaMap data to use.'),
%       [name('08')]),
%  iarg(infile,
%        aspec(infile,mandatory,file,read,no_default,'Input file containing labelled utterances'),
%        [name(user_input),stream(user_input)]),
%  iarg(outfile,
%       aspec(outfile,mandatory,file,write,no_default,'Output file'),
%       [name(user_output),stream(user_output)])]

% We want to create a list of the form
% [ OptionName1-OptionValue1, OptionName2-OptionValue2, ... OptionNameN-OptionValueN ]
% where the OptionNames are
% the first arg of the iarg(_) terms in Iargs and
% the first arg of the iopt(_) terms in IOptions
% and the OptionValues are extracted from
% the third  arg of the iarg(_) terms, if there is a matching iarg term, or
% the second arg of the iopt(_) terms otherwise.

format_cmd_line_for_machine_output(Options, Args, args(CommandLine,ArgsMO)) :-
	get_program_name(ProgramName),
	prolog_flag(argv, Argv),
	concat_atom([ProgramName|Argv], ' ', CommandLine),
	format_cmd_line_for_machine_output_1(Options, Args, ArgsMO).


basename(File, Base) :-
        basename_aux(File, S, S, Base).

% This predicate is stolen from system3.pl in the SICStus Prolog 4.1.1 library directory,
% from where it is NOT exported. Rather than modify the SP library file to export basename/4,
% I have just copied it here.
basename_aux([], Base, [], Base).
basename_aux([0'/|File], _, _, Base) :- !,
        basename_aux(File, S, S, Base).
basename_aux([C|File], S0, [C|S], Base) :- !,
        basename_aux(File, S0, S, Base).

% format_cmd_line_for_machine_output_1(Options, IArgs, ArgsMO)
format_cmd_line_for_machine_output_1([], ArgsRest, ArgsMO) :-
	format_iargs_for_machine_output(ArgsRest, ArgsMO).

format_cmd_line_for_machine_output_1([FirstOption|RestOptions], ArgsIn, [FirstArgMO|RestArgsMO]) :-
	format_one_option(FirstOption, ArgsIn, ArgsNext, FirstArgMO),
	format_cmd_line_for_machine_output_1(RestOptions, ArgsNext, RestArgsMO).
	
% FirstArg looks like
% iarg(exclude_sts,
%      aspec(exclude_sts,mandatory,list,none,no_default,'List of semantic types to exclude.'),
%      [value([neop,gngm])])
% We want to grab the first arg (exclude_sts)
% and extract [neop,gngm] from the third arg
format_one_option(IOption, ArgsIn, ArgsNext, ArgMO) :-
	arg(1, IOption, OptionName),
	ArgMO = OptionName-OptionValue,
	( member(MatchingArg, ArgsIn),
	  arg(1, MatchingArg, OptionName) ->
	  % delete MatchingOption from IOptionsIn, leaving IOptionsNext
	  selectchk(MatchingArg, ArgsIn, ArgsNext),
	  arg(3, MatchingArg, [ArgValueTerm|_]),
	  arg(1, ArgValueTerm, OptionValue)
	; ArgsNext = ArgsIn,
	  OptionValue = []
	).
	  
format_iargs_for_machine_output([], []).
format_iargs_for_machine_output([FirstIarg|RestIargs],
				  [FirstIargName-FirstIargValue|RestIargsMO]) :-
	arg(1, FirstIarg, FirstIargName),
	arg(3, FirstIarg, [FirstIargValueTerm|_]),
	arg(1, FirstIargValueTerm, FirstIargValue),
	format_iargs_for_machine_output(RestIargs, RestIargsMO).


output_should_be_bracketed(BracketedOutput) :-
	( control_option(bracketed_output) ->
	  BracketedOutput is 1
        ; BracketedOutput is 0
	).


generate_EOT_output(OutputStream) :-
	( control_option(indicate_citation_end) ->
	  % do not use portrayed(true)
	  write_term(OutputStream, 'EOT',[quoted(true)]),
	  format(OutputStream, '.~n', [])
	; true
	).

do_formal_tagger_output :-
	( xml_output_format(_XMLFormat) ->
	  true
	; json_output_format(_XMLFormat) ->
	  true
	; control_option(machine_output) ->
	  true
	; control_option(formal_tagger_output) ->
	  skr_begin_write('EOC'),
	  skr_write_string(".E"),
	  skr_end_write('EOC')
	; true
	).

generate_phrase_output(PhraseTextAtom, Phrase, StartPos, Length,
		       ReplacementPos, BracketedOutput, PhraseMMO) :-
	% Do not generate this output if machine_output, XML, or fielded_mmi_output is on!
        ( check_generate_utterance_output_control_options_1 ->
	  % Genrerate the MMO for the Phrase term
	  % which is needed for both MMO and XML output!
	  PhraseMMO = phrase(PhraseTextAtom,Phrase,StartPos/Length,ReplacementPos)
	; ( \+ control_option(hide_plain_syntax) ->
	    atom_codes(PhraseTextAtom,PhraseText),
	    format('~nPhrase: ~s~n',[PhraseText])
	  ; true
	  ),
	  ( control_option(syntax) ->
	    ( BracketedOutput == 1 ->
	      skr_begin_write('Syntax'),
	      format('msu~n', []),
	      skr_write_phrase(Phrase),
	      skr_end_write('Syntax')
	    ; format('~nmsu~n', []),
		write_list_indented(Phrase)
	    )
	  ; true
	    )
	).

generate_bracketed_output(BracketedOutput, PhraseWordInfo) :-
	( control_option(fielded_mmi_output) ->
	  true
	; \+ control_option(hide_plain_syntax),
	  BracketedOutput == 1 ->
	  skr_begin_write('Phrase'),
	  PhraseWordInfo=_:pwi(wdl(_,LCWordL),_,_),
	  atom_codes_list(LCWordL,LCWordLStrings),
	  concat_strings_with_separator(LCWordLStrings, " ", LCPhraseString),
	  ( LCPhraseString == "" ->
	    true
	  ; skr_write_string(LCPhraseString)
	  ),
	  skr_end_write('Phrase')
	; true
	).


generate_variants_output([], _BracketedOutput).
generate_variants_output([GVC|RestGVCs], BracketedOutput) :-
	AllGVCs = [GVC|RestGVCs],
	( control_option(variants) ->
	  ( BracketedOutput == 1 ->
	    skr_begin_write('Variants')
	  ; format('~n', [])
	  ),
	  compute_varnum_length(AllGVCs, MaxNumDigits),
	  write_all_variants(AllGVCs, MaxNumDigits, 1, _FinalCount),
	  ( BracketedOutput == 1 ->
	    skr_end_write('Variants')
	  ; true
	  )
	; true
	).

% Count how many variants are in all the GVCs, and then
% compute the number of digits in that number
% (i.e., the length of the character representation of the integer).
compute_varnum_length(AllGVCs, NumDigits) :-
	  (  foreach(gvc(_G,Vs,_Cs), AllGVCs),
	     fromto(0, SumIn, SumNext, VariantTotal)
	  do length(Vs, Length),
	     SumNext is SumIn + Length
	  ),
	  number_codes(VariantTotal, VariantTotalCodes),
	  length(VariantTotalCodes, NumDigits).



generate_candidates_output(Evaluations3, TotalCandidateCount, UtteranceLabel,
			   ExcludedCandidateCount, PrunedCandidateCount,
			   RemainingCandidateCount,
			   BracketedOutput, CandidatesMMO) :-
	% Do not generate this output if machine_output, XML, or fielded_mmi_output is on!
	( test_generate_candidate_output_control_options ->
	  % Generate the MMO for the Candidates term
	  % which is needed for both MMO and XML output!
	  CandidatesMMO = candidates(TotalCandidateCount,
				     ExcludedCandidateCount,PrunedCandidateCount,
				     RemainingCandidateCount,Evaluations3)
	; control_option(show_candidates),
	  % Even if show_candidates is on, if there are no candidates, there's no output.
	  Evaluations3 \== [] ->
	  conditionally_skr_begin_write(BracketedOutput, 'Candidates'),
	  conditionally_dump_evals(Evaluations3, TotalCandidateCount, UtteranceLabel,
				   ExcludedCandidateCount, PrunedCandidateCount,
				   RemainingCandidateCount),
	  conditionally_skr_end_write(BracketedOutput, 'Candidates')
	; true
	).

test_generate_candidate_output_control_options :-
	( control_option(machine_output) ->
	  true
	; xml_output_format(_XMLFormat) ->
	  true
	; json_output_format(_JSONFormat) ->
	  true
	; control_option(fielded_mmi_output)
	).


conditionally_skr_begin_write(BracketedOutput, Message) :-
	( BracketedOutput =:= 1 ->
	  skr_begin_write(Message)
	; true
	).

conditionally_skr_end_write(BracketedOutput, Message) :-
	( BracketedOutput =:= 1 ->
	  skr_end_write(Message)
	; true
	).

conditionally_dump_evals(Evaluations3, TotalCandidateCount, UtteranceLabel,
			 ExcludedCandidateCount, PrunedCandidateCount,
			 RemainingCandidateCount) :-
	( control_option(number_the_candidates) ->
	  StartNum is 1
	; StartNum is 0
	),
	num_dump_evaluations_indented(Evaluations3, TotalCandidateCount, UtteranceLabel,
				      ExcludedCandidateCount, PrunedCandidateCount,
				      RemainingCandidateCount, StartNum, 'Candidates').
%	; dump_evaluations_indented(Evaluations3, TotalCandidateCount,
%				    ExcludedCandidateCount, PrunedCandidateCount,
%				    RemainingCandidateCount, 'Candidates')
%	).

generate_mappings_output(Mappings, _Evaluations, UtteranceLabel,
			 _APhrases, BracketedOutput, MappingsMMO) :-
	% Do not generate this output if machine_output, XML, or fielded_mmi_output is on!
	( ( control_option(machine_output)
	  ; xml_output_format(_XMLFormat)
	  ; json_output_format(_JSONFormat)
	  ; control_option(fielded_mmi_output)
	  ) ->
	  % Generate the MMO for the Mappings,
	  % which is needed for both MMO and XML output!
	  MappingsMMO = mappings(Mappings)
	; ( \+ control_option(hide_mappings),
	      Mappings \== [] ->
            ( BracketedOutput == 1 ->
	      skr_begin_write('Mappings')
	    ; true
	    ),
	    % dump_aphrase_mappings(APhrases, UtteranceLabel, 'Mapping'),
	    dump_mappings(Mappings, UtteranceLabel, 'Mapping'),
	    ( BracketedOutput == 1 ->
	      skr_end_write('Mappings')
	    ; true
	    )
	  ; true
	  )
	).

% Generate the MMO for Args, AAs, and NegEx.
generate_header_output(IArgs, IOptions, NegExList, DisambMMOutput,
		       HeaderMMO, HeaderMMORest) :-
	  HeaderMMO = [ArgsMMO,AAsMMO,NegExMMO|HeaderMMORest],
	  format_cmd_line_for_machine_output(IOptions, IArgs, ArgsMMO),
	  generate_AAs_MMO_term(DisambMMOutput, AAsMMO),
	  % NegExList is currently hardcoded.
	  NegExMMO = neg_list(NegExList).

generate_utterance_output(Label, Text0, UttStartPos, UttLength, ReplPos, UtteranceMMO) :-
	( check_generate_utterance_output_control_options_1 ->
	  % Generate the MMO for the Utterance term,
	  % which is needed for both MMO and XML output!
	  UtteranceMMO = utterance(Label,Text0,UttStartPos/UttLength,ReplPos)
	; check_generate_utterance_output_control_options_2 ->
          format('~NProcessing ~a: ~s~n',[Label,Text0])
	; true
	).

generate_AAs_MMO_term(DisambMMOutput, aas(SortedAAList)) :-
	% Exctact the AA term from the DisambMMOutput
	get_aa_term(DisambMMOutput, AAListTokens),
	% avl_to_list(AAs, AAListTokens),
	reformat_AA_list_for_MMO(AAListTokens, DisambMMOutput, AAList),
	sort(AAList, SortedAAList).

	
reformat_AA_list_for_MMO([], _DisambMMOutput, []).
reformat_AA_list_for_MMO([FirstAA|RestAAs], DisambMMOutput,
		 [AAString*ExpansionString*CountData*CUIList|RestReformattedAAs]) :-
	reformat_one_AA_for_MMO(FirstAA, DisambMMOutput,
				AAString, ExpansionString, CountData, TempCUIList),
	% 0 seeds the predicate with a NegScore
	choose_best_mappings_only(TempCUIList, 0, CUIList),
	reformat_AA_list_for_MMO(RestAAs, DisambMMOutput, RestReformattedAAs).

reformat_one_AA_for_MMO(AATokenList-ExpansionTokenList,
			DisambMMOutput, AAAtom, ExpansionAtom, CountData, CUIList) :-
	% Get the actual atoms from the AATokenList
	extract_token_atoms(AATokenList, AAAtomList),
	AATokenList = [FirstAAToken|_],
	token_template(FirstAAToken, _TokenType, _Atom, _LCAtom, pos(AAStartPos,_EndPos)),
	length(AATokenList, AATokenListLength),
	% Get the actual atoms from the ExpansionTokenList
	extract_token_atoms(ExpansionTokenList, ExpansionAtomList),
	length(ExpansionTokenList, ExpansionTokenListLength),
	% Get the lowercase alphanumeric atoms only from the ExpansionTokenList;
	% this will be used to match against the word lists in the ev terms
	extract_an_lc_atoms(ExpansionTokenList, ANLCExpansionAtomList),
	% AAAtoms is for display purposes in the aas(_) term
	concat_atom(AAAtomList, AAAtom),
	atom_length(AAAtom, AAAtomLength),
	concat_atom(ExpansionAtomList, ExpansionAtom),
	atom_length(ExpansionAtom, ExpansionAtomLength), 
	concat_atom(ANLCExpansionAtomList, ' ', ANLCExpansionAtom),
	CountData = (AATokenListLength,
		     AAAtomLength,
		     ExpansionTokenListLength,
		     ExpansionAtomLength,
		     AAStartPos:AAAtomLength),
	find_matching_CUIs(DisambMMOutput, ANLCExpansionAtom, TempCUIList, []),
	sort(TempCUIList, CUIList).

% Starting with a list of terms of the form NegScore-CUI,
% remove all those terms whose NegScore is not the highest (most negative).
choose_best_mappings_only([], _PrevScore, []).
choose_best_mappings_only([FirstScore-FirstCUI|RestCUIs], PrevScore, CUIList) :-
	% If FirstScore is =< PrevScore,
	% either PrevScore is the initial 0, or the scores are the same;
	% in either case, we keep FirstCUI.
	( FirstScore =< PrevScore ->
	  CUIList = [FirstCUI|RestCUIList],
	  choose_best_mappings_only(RestCUIs, FirstScore, RestCUIList)
	% If FirstScore > PrevScore, the CUI is not one of the best mappings,
	% so we're done.
	; CUIList = []
	).	

find_matching_CUIs([], _ExpansionAtom, CUIList, CUIList).
find_matching_CUIs([FirstMMOTerm|RestMMOTerms], ExpansionAtom, CUIListIn, CUIListOut) :-
	FirstMMOTerm = mm_output(_ExpSentence, _Citation, _ModifiedText, _Tagging,
				 _AAs, _Syntax, MMOPhrases, _ExtractedPhrases),
	find_matching_CUIs_in_phrases(MMOPhrases, ExpansionAtom, CUIListIn, CUIListNext),
	find_matching_CUIs(RestMMOTerms, ExpansionAtom, CUIListNext, CUIListOut).

find_matching_CUIs_in_phrases([], _ExpansionAtom, CUIList, CUIList).
find_matching_CUIs_in_phrases([FirstPhraseTerm|RestPhraseTerms], ExpansionAtom, CUIListIn, CUIListOut) :-
	FirstPhraseTerm = phrase(_Phrase, _Candidates, Mappings, _Pwi, _Gvcs, _Ev0, _Aphrases),
	Mappings = mappings(MappingsList),
	find_matching_CUIs_in_mappings(MappingsList, ExpansionAtom, CUIListIn, CUIListNext),
	find_matching_CUIs_in_phrases(RestPhraseTerms, ExpansionAtom, CUIListNext, CUIListOut).
	
find_matching_CUIs_in_mappings([], _ExpansionAtom, CUIList, CUIList).
find_matching_CUIs_in_mappings([FirstMapping|RestMappings], ExpansionAtom, CUIListIn, CUIListOut) :-
	FirstMapping = map(_NegScore, CandidateList),
	find_matching_CUIs_in_candidates(CandidateList, ExpansionAtom, CUIListIn, CUIListNext),
	find_matching_CUIs_in_mappings(RestMappings, ExpansionAtom, CUIListNext, CUIListOut).

find_matching_CUIs_in_candidates([], _ExpansionAtom, CUIList, CUIList).
find_matching_CUIs_in_candidates([FirstCandidate|RestCandidates],
				 ExpansionAtom, CUIListIn, CUIListOut) :-
	( matching_CUI(FirstCandidate, ExpansionAtom, MatchingCUI) ->
	  CUIListIn = [MatchingCUI|CUIListNext]
	; CUIListNext = CUIListIn
	),
	find_matching_CUIs_in_candidates(RestCandidates, ExpansionAtom, CUIListNext, CUIListOut).

matching_CUI(Candidate, ExpansionAtomLC, CurrScore-MatchingCUI) :-
	get_all_candidate_features([negvalue,cui,metawords],
				   Candidate,
				   [CurrScore,MatchingCUI,MatchingWordList]),
	concat_atoms_intelligently(MatchingWordList, MatchingWordAtom),
	ExpansionAtomLC == MatchingWordAtom.


get_aa_term(MMOutput, AAs) :-
	MMOutput = [FirstMMOutput|_],
	FirstMMOutput = mm_output(_ExpandedUtterance,_Citation,_ModifiedText,
				  _Tagging,AAs,_Syntax,_MMOPhrases,_ExtractedPhrases).

% generate_MMO_terms(OutputStream, MMOTerms) :-
generate_MMO_terms(IArgs, IOptions, NegExList, DisambMMOutput,
		   HeaderMMO, HeaderMMORest, OutputStream, PrintMMO, AllMMO) :-
	  conditionally_generate_header_output(IArgs, IOptions, NegExList,
					       DisambMMOutput, HeaderMMO, HeaderMMORest),
	  AllMMO = HeaderMMO,
	  conditionally_write_MMO_terms(PrintMMO, AllMMO, OutputStream).

conditionally_generate_header_output(IArgs, IOptions, NegExList,
				     DisambMMOutput, HeaderMMO, HeaderMMORest) :-
	( ( control_option(machine_output)
	   ; xml_output_format(_XMLFormat)
	   ; json_output_format(_JSONFormat) ) ->
	  generate_header_output(IArgs, IOptions, NegExList,
				 DisambMMOutput, HeaderMMO, HeaderMMORest)
	; true
	).

conditionally_write_MMO_terms(PrintMMO, [ArgsMMO,AAsMMO,NegExMMO|UtteranceMMO], OutputStream) :-
	( PrintMMO is 1,
	  control_option(machine_output) ->
	  write_args_MMO_term(ArgsMMO, OutputStream),
	  write_AAs_MMO_term(AAsMMO, OutputStream),
	  write_negex_MMO_term(NegExMMO, OutputStream),
	  write_all_utterance_MMO_terms(UtteranceMMO, OutputStream)
	; true
	).
       
write_args_MMO_term(ArgsMMO, OutputStream) :-
	write_term(OutputStream, ArgsMMO,
		   [quoted(true),portrayed(true)]),
	format('.~n', []).

write_AAs_MMO_term(AAsMMO, OutputStream) :-
        write_term(OutputStream, AAsMMO,
		   [quoted(true),portrayed(true)]),
	format('.~n', []).

write_negex_MMO_term(NegExMMO, OutputStream) :-
	write_term(OutputStream, NegExMMO,
		   [quoted(true),portrayed(true)]),
	format('.~n', []).

write_all_utterance_MMO_terms([], _OutputStream).
write_all_utterance_MMO_terms([FirstMMOTerm|RestMMOTerms], OutputStream) :-
	write_one_utterance_MMO_term(FirstMMOTerm, OutputStream, RestMMOTerms, RemainingMMOTerms),
	write_EOU_term(OutputStream),
	write_all_utterance_MMO_terms(RemainingMMOTerms, OutputStream).

write_one_utterance_MMO_term(UtteranceTerm, OutputStream, RestMMOTerms, RemainingMMOTerms) :-
        % this is the one place to use portrayed(true)
        UtteranceTerm = utterance(Label, UtteranceString, PosInfo, ReplPos),
        % We can no longer simply do
        % write_term(UtteranceTerm, [quoted(true),portrayed(true)])
        % because ReplPos is a list of small integers,
        % which would be interpreted as a print string by portray/1. Blarg.
        format(OutputStream, 'utterance(~q,', [Label]),
        write_term(OutputStream, UtteranceString, [quoted(true),portrayed(true)]),
        format(OutputStream, ',~w,~w).~n', [PosInfo,ReplPos]),
        write_all_phrase_MMO_terms(RestMMOTerms, OutputStream, RemainingMMOTerms).

write_all_phrase_MMO_terms([], _OutputStream, []).
write_all_phrase_MMO_terms([H|T], OutputStream, RemainingMMOTerms) :-
	( functor(H, utterance, _UtteranceArity) ->
	  RemainingMMOTerms = [H|T]
	; [H|T] = [PhraseMMOTerm,CandidateMMOTerm,MappingsTerm|RestMMOTerms],
	  write_one_phrase_MMO_term(PhraseMMOTerm, OutputStream, CandidateMMOTerm, MappingsTerm),
	  write_all_phrase_MMO_terms(RestMMOTerms, OutputStream, RemainingMMOTerms)
	).

write_one_phrase_MMO_term(PhraseMMOTerm, OutputStream, CandidatesMMOTerm, MappingsMMOTerm) :-
	write_phrase_MMO_component(PhraseMMOTerm, OutputStream),
	write_candidates_MMO_component(CandidatesMMOTerm, OutputStream),
	write_mappings_MMO_component(MappingsMMOTerm, OutputStream).

write_phrase_MMO_component(PhraseTerm, OutputStream) :-
	PhraseTerm = phrase(PhraseString,Syntax,PosInfo,ReplPos),
	% We can no longer simply do
	% write_term(PhraseTerm, [quoted(true), portrayed(true)]),
	% because ReplPos is a list of small integers,
	% which would be interpreted as a print string by portray/1. Blarg.
	format(OutputStream, 'phrase(', []),
	write_term(OutputStream, PhraseString, [quoted(true),portrayed(true)]),
	format(OutputStream, ',~q,~w,~w).~n', [Syntax,PosInfo,ReplPos]).

write_candidates_MMO_component(CandidatesTerm, OutputStream) :-
	CandidatesTerm = candidates(TotalCandidateCount,
				    ExcludedCandidateCount,PrunedCandidateCount,
				    RemainingCandidateCount,
				    CandidateList),
	format(OutputStream, 'candidates(~d,~d,~d,~d,[',
	       [TotalCandidateCount,ExcludedCandidateCount,
		PrunedCandidateCount,RemainingCandidateCount]),
	% If the candidate list is empty, do nothing!
	( CandidateList = [H|T],
	  control_option(show_candidates) ->
	  write_MMO_candidate_list(T, OutputStream, H)
	; true
	),
	% write_term(CandidatesTerm, [quoted(true)]),
	format(']).~n', []).

write_MMO_candidate_list([], OutputStream, LastCandidateTerm) :-
	write_MMO_candidate_term(LastCandidateTerm, OutputStream).
write_MMO_candidate_list([Next|Rest], OutputStream, ThisCandidateTerm) :-
	write_MMO_candidate_term(ThisCandidateTerm, OutputStream),
	write(OutputStream, ','),
	write_MMO_candidate_list(Rest, OutputStream, Next).	

% Simply hide the LSComponents and TargetLSComponent terms
write_MMO_candidate_term(CandidateTerm, OutputStream) :-
	candidate_term(NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
		       MatchMap,_LSComponents,_TargetLSComponent,InvolvesHead,
		       IsOvermatch,UniqueSources,PosInfo,Status,Negated,CandidateTerm),
	format(OutputStream, 'ev(~q,~q,~q,~q,~q,~q,~q,~q,~q,~q,~q,~q,~q)',
	       [NegValue,CUI,MetaTerm,MetaConcept,MetaWords,SemTypes,
		MatchMap,InvolvesHead,IsOvermatch,UniqueSources,PosInfo,Status,Negated]).

write_mappings_MMO_component(MappingsTerm, OutputStream) :-
	MappingsTerm = mappings(MappingsList),
	format(OutputStream, 'mappings([', []),
	% MappingsTerm = mappings([map(-1000,[ev('C0018787')]),map(-1000,[ev('C1281570')])]))
	% do not use portrayed(true)
	( MappingsList = [H|T] ->
	  write_MMO_mappings_list(T, OutputStream, H)
	; true
	),
	% write_term(MappingsTerm, [quoted(true)]),
	format(OutputStream, ']).~n', []).

write_MMO_mappings_list([], OutputStream, LastMappingTerm) :-
	write_MMO_mapping_term(LastMappingTerm, OutputStream).
write_MMO_mappings_list([Next|Rest], OutputStream, ThisMappingTerm) :-
	write_MMO_mapping_term(ThisMappingTerm, OutputStream),
	write(OutputStream, ','),
	write_MMO_mappings_list(Rest, OutputStream, Next).	

write_MMO_mapping_term(map(NegValue,CandidateList), OutputStream) :-
	format(OutputStream, 'map(~d,[', [NegValue]),
	CandidateList = [H|T],
	write_MMO_candidate_list(T, OutputStream, H),
	format(OutputStream, '])', []).

write_EOU_term(OutputStream) :-
	write_term(OutputStream, 'EOU',[quoted(true)]),
	format(OutputStream, '.~n', []).


output_tagging(BracketedOutput, HRTagStrings, FullTagList) :-
	% (1a) If either tagger_output or format_tagger_output is on
	( ( control_option(formal_tagger_output)
	  ; control_option(tagger_output)
	  ) ->
	  % (2) If bracketed_output is on
	  ( BracketedOutput == 1 ->
	    skr_begin_write('Tagging')
	  ; format('~n', [])
	  ),
	  ( control_option(tagger_output) ->
	    skr_write_strings(HRTagStrings)
	  ; skr_write_list(FullTagList)
	  ),
	  ( BracketedOutput == 1 ->
	    skr_end_write('Tagging')
	  ; true
	  )
	% (1b) Otherwise, do nothing
	; true
	).

token_template(tok(TokenType,TokenString,LCTokenString,PosInfo),
	       TokenType, TokenString, LCTokenString, PosInfo).
	% format(user_output, 'TT/5: ~q~n', [tok(TokenType,TokenString,LCTokenString,PosInfo)]).

token_template(tok(TokenType,TokenString,LCTokenString,PosInfo1,PosInfo2),
	       TokenType, TokenString, LCTokenString, PosInfo1, PosInfo2).

compare_utterance_lengths([], []).
compare_utterance_lengths([FirstOrigUtterance|RestOrigUtterances],
			  [FirstExpUtterance|RestExpUtterances]) :-
	compare_one_utterance(FirstOrigUtterance, FirstExpUtterance),
	compare_utterance_lengths(RestOrigUtterances, RestExpUtterances).

compare_one_utterance(FirstOrigUtterance, FirstExpUtterance) :-
	FirstOrigUtterance = utterance(OrigID, _OrigUtterance, OrigStartPos/OrigLength, _OrigReplPos),
	FirstExpUtterance  = utterance(ExpID,  _ExpUtterance,  ExpStartPos/ExpLength,  _ExpReplPos),
	compare_one_utterance_aux(OrigID, OrigStartPos, OrigLength,
				  ExpID, ExpStartPos, ExpLength).

compare_one_utterance_aux(OrigID, OrigStartPos, OrigLength,
		   ExpID,  ExpStartPos,  ExpLength) :-
	compare_fields(OrigID, ExpID, 'OrigID',       OrigID,       'ExpID',       ExpID),
	compare_fields(OrigID, ExpID, 'OrigStartPos', OrigStartPos, 'ExpStartPos', ExpStartPos),
	compare_fields(OrigID, ExpID, 'OrigLength',   OrigLength,   'ExpLength',   ExpLength).	

compare_fields(OrigID, ExpID,
	       OrigFieldName, OrigFieldValue,
	       ExpFieldName,  ExpFieldValue) :-
	( OrigFieldValue == ExpFieldValue ->
	  true
	; format(user_output, 'In ~w/~w: ~w ~w =\\= ~w ~w~n',
		 [OrigID, ExpID,
		  OrigFieldName, OrigFieldValue,
		  ExpFieldName,  ExpFieldValue]),
	  ttyflush
	).

% replace_crs_with_blanks(String, CurrPos, NewString, ReplacementIndexes)
% NewString is a copy of String, but with all <CR>s replaced by blanks.
% ReplacementIndexes is a list of the character positions
% where the replacements occurred.
% CurrPos is just the current character position in String.

replace_crs_with_blanks([], _Pos, [], []).
replace_crs_with_blanks([H|T], Pos, [NewH|NewT], ReplacementIndexes) :-
	( H =:= 10 ->
	  NewH is 32,
	  ReplacementIndexes = [Pos|RestReplacementIndexes]
	; NewH is H,
	  RestReplacementIndexes = ReplacementIndexes
	),
	NextPos is Pos + 1,
	replace_crs_with_blanks(T, NextPos, NewT, RestReplacementIndexes).
% No more blanks to replace with <CR>s
replace_blanks_with_crs([], String, _CurrPos, String).
replace_blanks_with_crs([BlankIndex|RestIndexes], [H|T], CurrPos, [NewH|NewT]) :-
	( BlankIndex =:= CurrPos,
	  H =:= 32 ->
	  NewH is 10,
	  RemainingIndexes = RestIndexes
	; NewH is H,
	  RemainingIndexes = [BlankIndex|RestIndexes]
	),
	NextPos is CurrPos + 1,
	replace_blanks_with_crs(RemainingIndexes, T, NextPos, NewT).

/* compute_sum(+Values, +SumIn, -SumOut)

compute_sum/3
xxx
*/

compute_sum([], SumIn, SumIn).
compute_sum([First|Rest], SumIn, SumOut) :-
	SumInOut is SumIn + First,
	compute_sum(Rest, SumInOut, SumOut).

debug_call(Flag, Goal) :-
	( control_value(debug, DebugFlags) ->
	  debug_call_1(Flag, DebugFlags, Goal)
	; true
	).

debug_call_1(Flag, DebugFlags, Goal) :-
	ensure_list(Flag, FlagList),
	( intersection(FlagList, DebugFlags, [_|_]) ->
	  call(Goal)
	; memberchk('ALL', DebugFlags) ->
	  call(Goal)
	; true
	).

debug_message(Flag, String, Arguments) :-
	( control_value(debug, DebugFlags) ->
	  debug_message_1(Flag, String, DebugFlags, Arguments)
	; true
	),
	ttyflush.

debug_message_1(Flag, String, DebugFlags, Arguments) :-
	ensure_list(Flag, FlagList),
	( intersection(FlagList, DebugFlags, [_|_]) ->
	  format(user_output, String, Arguments)
	; memberchk('ALL', DebugFlags) ->
	  format(user_error, String, Arguments)
	; true
	).

ensure_list(X, List) :-
	( X == [] ->
	  List = X
	; X = [_|_] ->
	  List = X
	; List = [X]
	).

ensure_atom(Input, InputAtom) :-
	( atom(Input) ->
	  InputAtom = Input
	; number(Input) ->
	  number_codes(Input, InputCodes),
	  atom_codes(InputAtom, InputCodes)
	; is_print_string(Input) ->
	  atom_codes(InputAtom, Input)
	).
	
ensure_number(Atom, Number) :-
	( number(Atom) ->
	  Number is Atom
	; atom_codes(Atom, AtomCodes),
	  number_codes(Number, AtomCodes)
	).

% Generate the MMO, which is needed for machine_output, fielded_mmi_output, and XML output.
check_generate_utterance_output_control_options_1 :-
	( control_option(machine_output)     -> true
	; control_option(fielded_mmi_output) -> true
	; control_option(pipe_output) -> true
	; xml_output_format(_XMLFormat)      -> true
	; json_output_format(_JSONFormat)
	).

% If this predicate succeeds it causes
%           format('~NProcessing ~a: ~s~n',[Label,Text0])
% e.g.,
% Processing 12087422.ti.1: Toxicology and carcinogenesis studies . . .
% to be called, which we want to happen
% UNLESS hide_plain_syntax and hide_mappings are on
% AND    show_candidates, syntax and variants are off.
check_generate_utterance_output_control_options_2 :-
	( \+ control_option(hide_plain_syntax) -> true
	; \+ control_option(hide_mappings)     -> true
	; control_option(show_candidates)      -> true
	; control_option(syntax)               -> true
	; control_option(variants)
	).

conditionally_print_header_info(InputFile, TagMode, OutputFile, TagOption) :-
	( \+ control_option(silent) ->
	  format('~n~nBeginning to process ~a ~asending output to ~a.~n',
		 [InputFile,TagMode,OutputFile]),
	  announce_tagging_mode(TagOption)
	; true
	).

announce_tagging_mode(TagOption) :-
	  ( TagOption == tag ->
	    format('Tagging will be done dynamically.~n', [])
	  ; format('No tagging will be done.~n', [])
	  ).
	
conditionally_print_end_info(InputFile, OutputFile) :-
	( \+ control_option(silent) ->
	  format(user_output,
		 '~nBatch processing is finished for ~w sending output to ~w.~n',
		 [InputFile, OutputFile])
	; true
	).

% Expand, e.g., [doctor,breastfeeding,patients]
%            to [doctor,breast,feeding,patients]
expand_split_word_list([], []).
expand_split_word_list([H|T], ExpandedList) :-
	( split_word(H, Word1, Word2) ->
	  ExpandedList = [Word1,Word2|RestExpanded]
	; ExpandedList = [H|RestExpanded]
	),
	expand_split_word_list(T, RestExpanded).

split_word('', '', '').
% split_word(breastfeeding, breast, feeding).
% split_word(heartrate,     heart, rate).

check_valid_file_type(File, Type) :-
	( \+ file_exists(File) ->
	  fatal_error('~n~*c~n~w file~n~w~ndoes not exist. Aborting.~n~*c~n~n',
		       [80,0'#,Type,File,80,0'#])
	; true
	),
	( \+ file_exists(File, read) ->
	  fatal_error('~n~*c~n~q file~n~w~nis not not readable. Aborting.~n~*c~n~n',
		      [80,0'#,Type,File,80,0'#])
	; true
	).

get_all_candidate_features(FeatureList, Candidate, FeatureValueList) :-
	(  foreach(ThisFeature, FeatureList),
	   foreach(ThisFeatureValue, FeatureValueList),
	   param(Candidate)
	do get_candidate_feature(ThisFeature, Candidate, ThisFeatureValue)
	).

get_candidate_feature(negvalue, Candidate, NegValue) :-
	!,
	candidate_term(NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(cui, Candidate, CUI) :-
	!,
	candidate_term(_NegValue,CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(metaterm, Candidate, MetaTerm) :-
	!,
	candidate_term(_NegValue,_CUI,MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(metaconcept, Candidate, MetaConcept) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(metawords, Candidate, MetaWords) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(semtypes, Candidate, SemTypes) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(matchmap, Candidate, MatchMap) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(lscomponents, Candidate, LSComponents) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(targetlscomponent, Candidate, TargetLSComponent) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(involveshead, Candidate, InvolvesHead) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(isovermatch, Candidate, IsOvermatch) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       IsOvermatch,_Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(sources, Candidate, Sources) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,Sources,_PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(posinfo, Candidate, PosInfo) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,PosInfo,_Status,_Negated, Candidate).
get_candidate_feature(status, Candidate, Status) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,Status,_Negated, Candidate).
get_candidate_feature(negated, Candidate, Negated) :-
	!,
	candidate_term(_NegValue,_CUI,_MetaTerm,_MetaConcept,_MetaWords,_SemTypes,
		       _MatchMap,_LSComponents,_TargetLSComponent,_InvolvesHead,
		       _IsOvermatch,_Sources,_PosInfo,_Status,Negated, Candidate).
get_candidate_feature(BogusFeature, Candidate, _) :-
	fatal_error('### Attempt to extract unknown feature ~w from candidate term ~q!!~n',
		    [BogusFeature, Candidate]).

fatal_error(Message, Args) :-
	send_message('~n### MetaMap ERROR: ', []),
	send_message(Message, Args),
	halt.

set_message(SourceList, PluralIndicator, Verb, Pronoun, Determiner) :-
	length(SourceList, Length),
	( Length is 1 ->
	  PluralIndicator = '',
	  Verb = 'is',
	  Pronoun = 'it',
	  Determiner = ' a'
	; PluralIndicator = 's',
	  Verb = 'are',
	  Pronoun = 'them',
	  Determiner = ''
	).

convert_non_text_fields_to_blanks(InputType, InputStringWithBlanks,
				  TextFields, NonTextFields,
				  TrimmedTextFieldsOnlyString, NumBlanksTrimmed) :-
	( InputType == citation ->
	  cntfb(InputStringWithBlanks, TextFields, NonTextFields, TextFieldsOnly)
	; TextFieldsOnly = [InputStringWithBlanks]
	),
	append(TextFieldsOnly, TextFieldsOnlyString0),
	trim_whitespace_right(TextFieldsOnlyString0, TextFieldsOnlyString),
	% TextFieldsOnlyString = TextFieldsOnlyString0,
	trim_whitespace_left_count(TextFieldsOnlyString, TrimmedTextFieldsOnlyString, NumBlanksTrimmed).


% "cntfb" == "Convert Non-Text Fields to Blanks"
cntfb([], [], [], []).
cntfb([FirstChar|RestChars], TextFieldsIn, NonTextFieldsIn, [ConvertedField|RestConvertedFields]) :-
	convert_one_field_to_blanks([FirstChar|RestChars], TextFieldsIn, NonTextFieldsIn,
				    TextFieldsNext, NonTextFieldsNext, ConvertedField, CharsNext),
	cntfb(CharsNext, TextFieldsNext, NonTextFieldsNext, RestConvertedFields).
	
convert_one_field_to_blanks(CharsIn,
			    TextFieldsIn, NonTextFieldsIn,
			    TextFieldsNext, NonTextFieldsNext,
			    ConvertedField, CharsNext) :-
	get_field_components(TextFieldsIn,
			     FirstTextField, FirstTextFieldLines, RestTextFields),
	get_field_components(NonTextFieldsIn,
			     FirstNonTextField, FirstNonTextFieldLines, RestNonTextFields),
	( FirstTextField \== [],
	  prefix(CharsIn, FirstTextField) ->
	  convert_text_field(FirstTextField, FirstTextFieldLines, CharsIn,
			     ConvertedField, CharsNext),
	  TextFieldsNext = RestTextFields,
	  NonTextFieldsNext = NonTextFieldsIn
	; FirstNonTextField \== [],
	  trim_whitespace_left(CharsIn, TrimmedCharsIn),
	  prefix(TrimmedCharsIn, FirstNonTextField) ->
	  convert_non_text_field(FirstNonTextField, FirstNonTextFieldLines, TrimmedCharsIn,
				 ConvertedField, CharsNext),
	  TextFieldsNext = TextFieldsIn,
	  NonTextFieldsNext = RestNonTextFields
	).

get_field_components([], [], [], []).
get_field_components([[FirstField|RestFirstField]|RestFields],
		     FirstField, FirstFieldLines, RestFields) :-
	( RestFirstField \== [] ->
	  RestFirstField = [FirstFieldLines]
	; FirstFieldLines = []
	).

convert_text_field(TextField, TextFieldLines, CharsIn, ConvertedField, CharsNext) :-
	trim_field_name_prefix(TextField, CharsIn, TextFieldLength, NumBlanksTrimmed, CharsNext3),
	form_one_string(TextFieldLines, " ", PaddedTextFieldLines0),
	append(PaddedTextFieldLines0, " ", PaddedTextFieldLines),
	% each line has a blank space after it corresponding to the original <CR>
	% NumBlanks is TextFieldLength + NumBlanksTrimmed + TextFieldLinesLength,
	NumBlanks is TextFieldLength + NumBlanksTrimmed,
	% create a string of blanks whose length is equal to the length of the
	% field name, blanks, etc. up to the actual text
	length(BlanksList, NumBlanks),
	% format(user_output, 'LINE ~d >~s:~p~n', [NumBlanks,TextField,TextFieldLines]),
	( foreach(X, BlanksList) do X is 32 ),
	% This is where I'm missing a character?
	append(BlanksList, PaddedTextFieldLines, ConvertedField),
	walk_off_field_lines(TextFieldLines, CharsNext3, CharsNext).
	% format(user_output, 'AFTER >~s<:>~s<~n', [TextField,CharsNext]).
  
% Calculate how many characters are taken up by
%  * the text field (e.g., "TI", "AB", "PG", "OWN", "STAT", etc.,
%  * any blanks immediately after the text field,
%  * the following hyphen, and
%  * any blanks immediately after the hyphen.
% Currently, this prefix always (?) contains exactly 6 chars,
% but who knows if this might someday change!
trim_field_name_prefix(Field, CharsIn, FieldLength, NumBlanksTrimmed, CharsNext3) :-
	% trim off the text field, e.g., "TI", "AB", etc.
	append(Field, CharsNext0, CharsIn),
	% determine the length of the text field
	length(Field, FieldLength),
	% trim off any whitespace immediately after the text field
	trim_whitespace_left_count(CharsNext0, CharsNext1, NumBlanksTrimmed1),
	% trim off the hyphen after the blanks
	CharsNext1 = [FirstChar|CharsNext2],
	medline_field_separator_char(FirstChar),
	% trim off any whitespace immediately after the hyphen
	trim_whitespace_left_count(CharsNext2, CharsNext3, NumBlanksTrimmed2),
	% The "+1" is for the hyphen
	NumBlanksTrimmed is NumBlanksTrimmed1 + NumBlanksTrimmed2 + 1.


convert_non_text_field(NonTextField, NonTextFieldLines, CharsIn, ConvertedField, CharsNext) :-
	trim_field_name_prefix(NonTextField, CharsIn, NonTextFieldLength, NumBlanksTrimmed, CharsNext3),
	% convert_strings_to_blanks(NonTextFieldLines, BlanksLines),
	% length(BlanksLines, BlanksLinesLength),
	% must add 1 blank for each line
	% length(NonTextFieldLines, NonTextFieldLinesLength),

	form_one_string(NonTextFieldLines, " ", PaddedNonTextFieldLines0),
	% This is an ad-hoc hack to handle empty "AU" lines such as
	% AU  - Niven CF Jr
	% AU  - 
	% LA  - eng
	% from
	% PMID- 16561135
	% as of 02/24/2014
	append_blank_if_nonnull(PaddedNonTextFieldLines0, PaddedNonTextFieldLines),
	length(PaddedNonTextFieldLines, PNTFLength),
	NumBlanks is NonTextFieldLength + NumBlanksTrimmed + PNTFLength,
	length(BlanksList, NumBlanks),
	% format(user_output, 'LINE ~d >~s:~p~n', [NumBlanks,NonTextField,NonTextFieldLines]),
	( foreach(X, BlanksList) do X is 32 ),
	ConvertedField = BlanksList,
	walk_off_field_lines(NonTextFieldLines, CharsNext3, CharsNext).
	% format(user_output, 'AFTER >~s<:>~s<~n', [NonTextField,CharsNext]).

% convert each string in NonTextFieldLines to a string of blanks of the same length
%%% convert_strings_to_blanks(NonTextFieldLines, BlanksLines) :-
%%% 	(  foreach(Line, NonTextFieldLines),
%%% 	   foreach(BlanksLine, BlanksLines)
%%% 	do length(Line, LineLength),
%%% 	   length(BlanksLine, LineLength),
%%% 	   (  foreach(Blank, BlanksLine)
%%% 	   do Blank is 32
%%% 	   )
%%% 	).

append_blank_if_nonnull([], []).
append_blank_if_nonnull([H|T], PaddedNonTextFieldLines) :-
	append([H|T], " ", PaddedNonTextFieldLines).

walk_off_field_lines([], Chars, Chars).
walk_off_field_lines([FieldLine|RestFieldLines], CharsIn, CharsOut) :-
	append(FieldLine, CharsNext0, CharsIn),
	( CharsNext0 = [32|CharsNext1] ->
	  true
	; CharsNext1 = []
	),
	walk_off_field_lines(RestFieldLines, CharsNext1, CharsOut).

% Test for membership of Element in VarList,
% where VarList may have an uninstantiated tail, e.g., [a,b,c|Rest].
memberchk_var(Element, VarList) :-
	nonvar(VarList),
	( VarList = [Element|_] ->
	  true
	; VarList = [_|Rest],
	  memberchk_var(Element, Rest)
	).

% Test for membership of Element in VarList,
% where VarList may have an uninstantiated tail, e.g., [a,b,c|Rest].
% member_var(Element, VarList) :-
% 	nonvar(VarList),
% 	( VarList = [Element|_]
% 	; VarList = [_|Rest],
% 	  member_var(Element, Rest)
% 	).

