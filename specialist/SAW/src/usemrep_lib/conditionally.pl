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

% File:	    conditionally.pl
% Module:   usemrep
% Author:   tcr, NLS, Usemrep  modified by MF to include semspec,
%           and by FML for code cleanup and modularization.
% Purpose:  semantic interpretation of medical text. MF added a semantic_specificaton module.

% ----- Module declaration and exported predicates

:- module(conditionally, [
%		conditionally_call_dbase_format/2,
		conditionally_call_eot_format/1,
%		conditionally_compute_timing/8,
%		conditionally_consult_tagged_text/6,          % used by SemGen ONLY!!
%		conditionally_consult_tagged_text_wrapper/9,  % used by SemRep and not SemGen
		conditionally_display_current_control_options/2,
%		conditionally_flag_interesting_semtypes/4,
		conditionally_get_time/3,
%		conditionally_initialize_tagger_access/0,
		conditionally_issue_usage_message/0,
		conditionally_output_abstract_id/2,
		conditionally_output_utterance/3,
		conditionally_remove_aa_and_aadef_tokens/3,
		conditionally_set_tagging_mode/1
%		conditionally_tag_text/2,
%		conditionally_write_special/1
			
   ]).

:- load_files( usemrep_lib(module_version), [
		when(compile_time)
   ]).

:- use_module( usemrep_lib(ssuppserv), [
%		announce_initialize_tagger_access/0,
%		flag_interesting_semtypes/4,
		split_input_label/4
%		statistics/1
   ]).

%:- use_module( usemrep_lib(wrappers), [
%		consult_tagged_text_wrapper/8
%   ]).

:- use_module( usemrep_lib(full_fielded_output), [
	        complex_token_type/1
   ]).

% :- use_module( usemrep_lib(write_results), [
% 		timing/7,
% 		write_special/3
%    ]).

:- use_module( usemrep_main(usemrep), [
		usage/0
   ]).

%:- use_module( tagger(tagger_access), [
%                tag_text/2 
%   ]).

:- use_module( skr_lib(nls_strings), [
                trim_whitespace_right/2
   ]).
								    
:- use_module( skr_lib(nls_system), [
		control_option/1,
		control_value/2,
		display_current_control_options/2,
		get_from_iargs/4

   ]).

%conditionally_call_dbase_format(OutputStream, AbstractID) :-
%	( control_option(dbase_format) ->
%	  true
%        ; AbstractID \== '' ->
%	  conditionally_output_abstract_id(OutputStream, AbstractID)
%	; true
%	).

conditionally_call_eot_format(OutputStream) :-
	( control_option(indicate_citation_end) ->
	  format(OutputStream,'<<< EOT >>>~n~n',[])
        ; true
        ).

% get_input_filename(InterpretedArgs, InputFile) :-
% 	memberchk(iarg(infile, _, [name(InputFile)|_]), InterpretedArgs).

%conditionally_compute_timing(1, Time0, Time1, Time2, Time3, Time4, Time5, Time6) :-
%        timing(Time0, Time1, Time2, Time3, Time4, Time5, Time6).
%conditionally_compute_timing(0, _, _, _, _, _, _, _).

%conditionally_consult_tagged_text_wrapper(InputText, InputLabel,
%					  OutputStream, Definitions, VarInfoList, 
%                                          TaggedText, TaggedTextFlag, LabeledText, 1) :-
%	( control_option(tag) ->
%          TaggedTextFlag = tag,
%          consult_tagged_text_wrapper(Definitions, VarInfoList,
%				      InputLabel, InputText, OutputStream,
%				      TaggedText, LabeledText, 1)
%        ; TaggedTextFlag = notag
%        ).

conditionally_display_current_control_options(IOptions,ProgramVersion) :- 
	( IOptions == [] ->
	  format(user_output, '~nNo control options set.~n',[])
        ; display_current_control_options(usemrep,ProgramVersion)
	).

%conditionally_flag_interesting_semtypes(AnalysisWithSemtypes2, OutputStream,
%					InputLabel, InterestingSemtypes) :-
%	( control_option(flag_interesting_semtypes) ->
%	  flag_interesting_semtypes(AnalysisWithSemtypes2, OutputStream,
%				    InputLabel, InterestingSemtypes)
%	; true
%	).

%conditionally_get_time('?', NewTime, TimeTest) :-
%	( control_option(time) ->
%	  statistics(NewTime),
%	  TimeTest = 1
%       ; TimeTest = 0
%        ).
%conditionally_get_time(1, NewTime, _TimeTest) :-
%	statistics(NewTime).
%conditionally_get_time(0, _NewTime, _TimeTest).


%conditionally_initialize_tagger_access :-
%	( control_option(tag) ->
%	  announce_initialize_tagger_access
%        ; true
%        ).

conditionally_issue_usage_message :-
	( control_option(help) ->
	  usage
	; true
	).


% Print the line
% ----- Citation <AbstractID> -----
% iff full_fielded_output_format is NOT on
conditionally_output_abstract_id(OutputStream, InputLabel) :-
	(	control_option(full_fielded_output_format)
        ;       control_value(other_program,_)
	;       control_option(xml_output_format)
	;       format(OutputStream, '----- Citation ~a -----~n', [InputLabel])
	).


conditionally_output_utterance(OutputStream, InputLabel, UNExpandedInputText) :-
	( control_option(full_fielded_output_format)
	; control_option(xml_output_format)
        ; control_value(other_program,_)
	; trim_whitespace_right(UNExpandedInputText,UNExpandedInputTextTrim),
	  format(OutputStream, '~N~a ~s~n~n', [InputLabel,UNExpandedInputTextTrim])
	),
	!.

% In SemGen (PrincipalDomain == genetics), utterances are not expanded,
% so we want to discard the aa and aadef tokens to allow Dimitar calculation of
% character offsets to work correctly.
conditionally_remove_aa_and_aadef_tokens(PrincipalDomain, Tokens, ModifiedTokens) :-
	( PrincipalDomain == genetics ->
	  remove_aa_and_aadef_tokens(Tokens, ModifiedTokens)
	; ModifiedTokens = Tokens
	).

remove_aa_and_aadef_tokens([], []).
remove_aa_and_aadef_tokens([FirstToken|RestTokens], ModifiedTokens) :-
	functor(FirstToken, tok, 5),
	arg(1, FirstToken, TokenType),
	( complex_token_type(TokenType) ->
	  ModifiedTokens = RestModifiedTokens
	; ModifiedTokens = [FirstToken|RestModifiedTokens]
	),
	remove_aa_and_aadef_tokens(RestTokens, RestModifiedTokens).

%conditionally_set_tagging_mode(TaggingMode) :-
%	( control_option(tag) ->
%	  TaggingMode = ''
%        ; TaggingMode = '(no tagging) '
%        ).
	
%conditionally_tag_text(InpText, TaggedText) :-
%	( control_option(tag) ->
%	  tag_text(InpText, TaggedText)
%        ; TaggedText = []
%        ).

% conditionally_write_special(EnhancedSyntax) :-
% 	( control_option(debug_format) ->
%           write_special(EnhancedSyntax, semtype,  user_output),
%           write_special(EnhancedSyntax, metaconc, user_output)
%         ; true
%        ).

%conditionally_consult_tagged_text(Definitions, VarInfoList, TaggedText,
%				  TaggedTextFlag, LabeledText, Index) :-
%	( control_option(tag) ->
%          TaggedTextFlag = tag,
%          consult_tagged_text(Definitions, VarInfoList,
%			      TaggedText, LabeledText, Index)
%        ; TaggedTextFlag = notag
%        ).
