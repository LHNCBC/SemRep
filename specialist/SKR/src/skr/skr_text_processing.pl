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

% File:     skr_text_processing.pl
% Module:   SKR
% Author:   Lan
% Purpose:  Provide (high-level) text processing


:- module(skr_text_processing, [
	extract_sentences/13,
	get_skr_text/2,
	get_skr_text_2/2,
	medline_field_separator_char/1,
	text_field/1
	% is_sgml_text/1,
	% moved from labeler % needed by sgml_extractor?
	% warn_remove_non_ascii_from_input_lines/2
    ]).

:- use_module(metamap(metamap_tokenization), [
	local_ws/1,
	tokenize_text_utterly/2
    ]).

:- use_module(skr(skr), [
	stop_and_halt/0
    ]).

:- use_module(skr(skr_utilities), [
	basename/2,
	ensure_atom/2,
	fatal_error/2,
        send_message/2,
	skr_begin_write/1,
	skr_end_write/1,
	skr_write_string/1
    ]).

% :- use_module(skr_lib(ctypes), [
% 	is_white/1
%     ]).

:- use_module(skr_lib(nls_io), [
	fget_line/2,
	fget_lines_until_skr_break/4,
	fget_non_ws_only_line/3
    ]).


:- use_module(skr_lib(nls_strings), [
	replace_nonprints_in_strings/2,
	replace_tabs_in_strings/2,
	split_string/4,
	split_string_completely/3,
	trim_whitespace/2,
	trim_whitespace_left/2,
	trim_whitespace_right/2
    ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
    ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	lower/2,
	lower_chars/2,
	ttyflush/0
    ]).

:- use_module(text(text_objects), [
	find_and_coordinate_sentences/8
    ]).

:- use_module(library(lists), [
	append/2,
	rev/2
    ]).

:- use_module(library(lists3), [
	substitute/4
    ]).


/* get_skr_text(-Lines)
   get_skr_text(+InputStream, -Lines)

get_skr_text/2 gets Lines from current input by first skipping "blank" lines
and then reading until a "natural" breaking point, which is an empty line
or one containing only whitespace.
*/

get_skr_text(Lines, TextID) :-
	maybe_set_prompt,
	current_input(InputStream),
	get_num_blank_lines(NumBlankLines),
	get_skr_text_3(InputStream, NumBlankLines, Lines0, TextID),
	( Lines0 == [[]] ->
	  Lines = []
	; Lines = Lines0
	).

% get_skr_text_2 is used ONLY for reading in the UDA file (and the NoMap file).
get_skr_text_2(InputStream, [First|Rest]) :-
	% skip all blank lines at the beginning of the input
	fget_non_ws_only_line(InputStream, 1, First),
	% fget_line(InputStream, First),
        !,
	NumBlankLines is 1,
        fget_lines_until_skr_break(InputStream, NumBlankLines, NumBlankLines, Rest).
get_skr_text_2(_, []).

get_skr_text_3(InputStream, NumBlankLines, [FirstText|Rest], TextID) :-
	% Skip all blank lines at the beginning of the input
	fget_non_ws_only_line(InputStream, NumBlankLines, First),
	!,
	% "sldi" == "single-line-delimited input"
	% The sldi option reads exactly one line of input and then stops reading.
	( control_option(sldi) ->
	  TextID = '',
	  FirstText = First,
	  Rest = []
	  % The sldiID option reads exactly one line of input and then stops reading.
	  % The input *must* be of the form ID|Text.
	; control_option(sldiID) ->
	  ( append([TempTextID, "|", TempFirstText], First) ->
	    Rest = [],
	    trim_whitespace(TempTextID, TextID),
	    trim_whitespace(TempFirstText, FirstText),
	      ( control_option(pipe_output) ->
		atom_codes(FirstTextAtom, FirstText),
		format('SLDI|~w~n', [FirstTextAtom])
	      ; true
	      )
	  ; fatal_error('The sldiID option requires input lines of the form ID|Text\n', []),
	    stop_and_halt
	  )
	 ; TextID = '',
	   FirstText = First,
	   fget_lines_until_skr_break(InputStream, NumBlankLines, NumBlankLines, Rest)
	).
get_skr_text_3(_InputStream, _NumBlankLines, [], '').


get_num_blank_lines(NumBlankLines) :-
	( control_value(blanklines, NumBlankLines) ->
	  true
	; NumBlankLines is 1
	).

maybe_set_prompt :-
	( control_value(prompt, UserSpecifiedPrompt0) ->
	  concat_atom([UserSpecifiedPrompt0, ' '], UserSpecifiedPrompt),
	  prompt(_Prompt, UserSpecifiedPrompt)
	; seeing(user) ->
	  prompt(Prompt, Prompt)
	; prompt(_Prompt, '')
	).

/*    extract_sentences(+Lines, -InputType, -Sentences, -CoordinateSentences, -AAs)

extract_sentences/4 creates Sentences and CoordinatedSentences from the strings
in Lines.
Lines can be in the following forms:
  a. MEDLINE citation
     the first tokens on the first line must be "UI" or "PMID" followed by "-"
%%%   b. loosely fielded
%%%      the first line must begin with "*"
%%%      any line beginning with "*" is a field id; field data begins a new line
%%%   d. SGML-tagged text
%%%      the first line must begin with "<"
%%%   e. Project description
%%%      the first line must begin with "--PROJECT NUMBER"
%%%   f. previously labeled utterance (for backward compatibility)
%%%      the first line must be of the form "[ label ] text"
  g. arbitrary text

The computed InputType is one of
  citation
%%%   loose
  smart
%%%   sgml
%%%   project
%%%   lutt
  simple

Sentences is a list of tokens of the form
  tok(<type>,<*>,<*>,<position>)
CoordinatedSentences is a list of terms of the form
  tok(<type>,<*>,<*>,<position>,<original-position>)
See text_object_tokens:form_field_tokens/2 for a complete description of the
tokens.

The expansion is accomplished by using text_object facilities such as
acronym/abbreviation discovery.

*/

% Append all whitespace-only fields to the next field.
% This is necessary because the field grammar doesn't like whitespace fields.
glom_whitespace_fields([], LastField, Fields) :-
	( whitespace_only_field(LastField) ->
	  Fields = []
	; Fields = [LastField]
	).
glom_whitespace_fields([NextField|RestFields], FirstField, GlommedFields) :-
	( whitespace_only_field(FirstField) ->
	  % the 32 is for the <CR>
	  append([32|FirstField], NextField, FirstGlommedField),
	  glom_whitespace_fields(RestFields, FirstGlommedField, GlommedFields)
	; GlommedFields = [FirstField|RestGlommedFields],
	  glom_whitespace_fields(RestFields, NextField, RestGlommedFields)
	).

whitespace_only_field([]).
whitespace_only_field([FirstChar|RestChars]) :-
	local_ws(FirstChar),
	whitespace_only_field(RestChars).

extract_sentences(Lines0, TextID, InputType, TextFields, NonTextFields,
		  Sentences, CoordinatedSentences,
		  UniqueIDAtom, AAs, UDAListIn, UDAListOut, UDA_AVL, Lines) :-
	Lines0 = [FirstLine|RestLines],
	% This is for Steven Bedrick's --blanklines idea
	glom_whitespace_fields(RestLines, FirstLine, Lines00),
	replace_tabs_in_strings(Lines00, Lines1),
	replace_nonprints_in_strings(Lines1, Lines2),
	( medline_citation(Lines2) ->
	  extract_coord_sents_from_citation(Lines2, TextFields, NonTextFields,
					    Sentences, CoordinatedSentences,
					    UniqueIDString, AAs, UDAListIn, UDAListOut, UDA_AVL),
	  InputType = citation
	; is_smart_fielded(Lines2) ->
	  extract_coord_sents_from_smart(Lines2, TextFields, NonTextFields,
					 Sentences, CoordinatedSentences,
					 UniqueIDString, AAs, UDAListIn, UDAListOut, UDA_AVL),
	  InputType = smart,
	  NonTextFields = []
	; form_dummy_citation(Lines2, TextID, CitationLines),
	  extract_coord_sents_from_citation(CitationLines, TextFields, NonTextFields,
					    Sentences, CoordinatedSentences,
					    UniqueIDString, AAs, UDAListIn, UDAListOut, UDA_AVL),
	  InputType = simple
	),
	atom_codes(UniqueIDAtom, UniqueIDString),
	update_strings_with_UDAs(UDAListOut, Lines2, Lines),
	!.

% To be recognized as a MEDLINE citation, a list of strings must be such that
% (1) The first string begins with
%     * optional whitespace followed by
%     * "PMID" or "UI" (case insensitive) followed by
%     * optional whitespace followed by
%     * one of "-", "|", ":", or ".", i.e.,
% <whitespace>(PMID|UI)<whitespace>[-|:.]
% (2) A subsequent string begins with the same sequence as above,
% but with "TI" instead of "PMID" or "UI".

medline_citation([FirstString,NextString|RestStrings]) :-
	medline_PMID_field_name(PMIDFieldNameAtom),
	atom_codes(PMIDFieldNameAtom, PMIDFieldNameString),
	medline_field_string(FirstString, PMIDFieldNameString),
	!,
	medline_title_field_name(TitleFieldNameAtom),
	atom_codes(TitleFieldNameAtom, TitleFieldNameString),
	member(OtherString, [NextString|RestStrings]),
	medline_field_string(OtherString, TitleFieldNameString),
	!.

medline_field_string(PMIDString, FieldNameString) :-
	trim_whitespace_left(PMIDString, TrimmedPMIDString),
	lower_chars(TrimmedPMIDString, LowerTrimmedPMIDString),
	append(FieldNameString, StringWithoutCitationIndicator, LowerTrimmedPMIDString),
	trim_whitespace_left(StringWithoutCitationIndicator, TrimmedStringWithoutCitationIndicator),
	TrimmedStringWithoutCitationIndicator = [FirstChar|_RestChars],
	medline_field_separator_char(FirstChar).

medline_field_separator_char(0'-).
medline_field_separator_char(0'|).
medline_field_separator_char(0':).
medline_field_separator_char(0'.).

medline_PMID_field_name(pmid).
medline_PMID_field_name(ui).

% See
% https://www.nlm.nih.gov/bsd/mms/medlineelements.html
% for the various "TI" fields.
medline_title_field_name(ti).     % Title
medline_title_field_name(tt).     % Transliterated Title
medline_title_field_name(bti).    % Book Title
medline_title_field_name(cti).    % Collection Title
medline_title_field_name(vti).    % Volume Title


is_smart_fielded([First,_|_]) :-
	append(".", _, First).

/* form_dummy_citation(+Lines, -CitationLines)

form_dummy_citation/1 turns Lines into a pseudo MEDLINE field, TX. */

form_dummy_citation(Lines, TempTextID, CitationLines) :-
	set_text_id_and_field_type(TempTextID, TextID, FieldType),
	form_dummy_citation_aux(Lines, TextID, FieldType, CitationLines).

form_dummy_citation_aux([], _TextID, _FieldType, []).
form_dummy_citation_aux([First|Rest], TextID, FieldType, [PseudoField,ModifiedFirst|PaddedRest]) :-
	ensure_atom(TextID, TextIDAtom),
	atom_codes(TextIDAtom, TextIDCodes),
	append("UI  - ", TextIDCodes, PseudoField),
	append([FieldType, "  - ", First], ModifiedFirst),
	padding_string(Padding),
	pad_lines(Rest, Padding, PaddedRest),
	!.

pad_lines([], _Padding, []).
pad_lines([First|Rest], Padding, [PaddedFirst|PaddedRest]) :-
	append(Padding, First, PaddedFirst),
	pad_lines(Rest, Padding, PaddedRest).

padding_string("      ").


set_text_id_and_field_type(TempTextID, TextID, FieldType) :-
	( TempTextID == '' ->
	    ( current_stream(AbsFileNameAtom, read, _Stream) ->
	      atom_codes(AbsFileNameAtom, AbsFileNameString), 
	      basename(AbsFileNameString, RelFileNameString),
	      atom_codes(TextID, RelFileNameString)
	    ; TextID = 'USER'
	    ),
	  % TextID = '00000000',
	  FieldType = "TX"
	% TempTextID is NOT '' in the case of --sldiID
	; TextID = TempTextID,
	  FieldType = "TX"
	).

/* 
   extract_coord_sents_from_citation(+CitationLines, -TextFields, -NonTextFields,
   				     -Sentences, -CoordinatedSentences, AAs)
*/

extract_coord_sents_from_citation(CitationLines, TextFields, NonTextFields,
				  Sentences, CoordinatedSentences,
				  UniqueID, AAs, UDAListIn, UDAListOut, UDA_AVL) :-
    extract_all_fields(CitationLines, CitationFields),
    (   member([FieldIDString,Field], CitationFields),
	lower_chars(FieldIDString, LowerFieldIDString),
	atom_codes(LowerFieldIDAtom, LowerFieldIDString), 
	medline_PMID_field_name(LowerFieldIDAtom) ->
        extract_document_unique_identifier(Field, UniqueID)
    ;   UniqueID = "00000000"
    ),
    extract_coord_sents_from_fields(UniqueID, CitationFields, TextFields, NonTextFields,
				    Sentences, CoordinatedSentences,
				    AAs, UDAListIn, UDAListOut, UDA_AVL),
    !.

%%% /* extract_utterances_from_sgml(+CitationLines, -Utterances)
%%% 
%%% extract_utterances_from_sgml/2 processes the strings CitationLines
%%% producing Utterances, a list of strings consisting of labeled utterances
%%% from the citation text fields.
%%% extract_utterances_from_sgml/3 also computes ExpandedUtterances with
%%% AAs expanded. */
%%% 
%%% extract_utterances_from_sgml(CitationLines,
%%% 			     Sentences, CoordinatedSentences, AAs, [RealText]) :-
%%%     extract_fields_from_sgml(CitationLines,CitationFields0),
%%%     standardize_field_names(CitationFields0,CitationFields),
%%% % temp
%%% %format('CitationFields:~n~p~n~n',[CitationFields]),
%%%     (select_field("DOC",CitationFields,UIField) ->
%%%         extract_document_unique_identifier(UIField,UniqueID)
%%%     ;   UniqueID="00000000"
%%%     ),
%%%     extract_real_text(CitationFields, RealText),
%%%     form_dummy_citation([RealText], RealCitationLines),
%%%     extract_coord_sents_from_citation(RealCitationLines, Sentences, CoordinatedSentences, AAs),
%%%     % extract_utterances_from_text_fields(UniqueID,CitationFields,Utterances),
%%%     !.
%%% 
%%% extract_real_text(CitationFields, RealText) :-
%%% 	extract_real_text_from_each_field(CitationFields, TextFields),
%%% 	append(TextFields, RealText).
%%% 
%%% extract_real_text_from_each_field([], []).
%%% extract_real_text_from_each_field([H|T], [TextH|TextT]) :-
%%%         H = [_FieldType, TextHStrings],
%%% 	atom_codes_list(TextHAtoms, TextHStrings),
%%% 	concat_atom(TextHAtoms, ' ', TextHAtomsWithBlanks),
%%% 	atom_codes(TextHAtomsWithBlanks, TextH),
%%% 	extract_real_text_from_each_field(T, TextT).


%%% standardize_field_names([],[]) :-
%%%     !.
%%% standardize_field_names([[Field,Lines]|Rest],
%%% 			[[StandardField,Lines]|ModifiedRest]) :-
%%%     standard_name(Field,StandardField),
%%%     standardize_field_names(Rest,ModifiedRest).
%%% 
%%% standard_name("DOCID","UI") :- !.
%%% standard_name("MedlineID","UI") :- !.
%%% standard_name("NOINDEX","NOINDEX") :- !.
%%% standard_name("TITLE","TI") :- !.
%%% standard_name("ArticleTitle","TI") :- !.
%%% standard_name("TEXT","AB") :- !.
%%% standard_name("AbstractText","AB") :- !.
%%% standard_name(Name,Name) :- !.


/* extract_all_fields(+CitationLines, -CitationFields)
   extract_all_fields(+FieldID, +FirstFieldLine, +CitationLines, -CitationFields)

extract_all_fields/2
extract_all_fields/4
*/

extract_all_fields([], []) :- !.
extract_all_fields([FirstLine|RestLines], CitationFields) :-
	phrase(f_begins_field([FieldID,FirstFieldLine]), FirstLine),
	% atom_codes(FieldIDAtom, FieldIDString),
	% atom_codes(FirstFieldLineAtom, FirstFieldLineString),
	!,
	extract_all_fields_4(FieldID, FirstFieldLine, RestLines, CitationFields).
extract_all_fields([FirstLine|RestLines], CitationFields) :-
	send_message('### WARNING: The following line should begin a field but does not:~n', []),
	send_message('~s~nIt is being ingored.~n~n', [FirstLine]),
	extract_all_fields(RestLines, CitationFields).

extract_all_fields_4(none, _FirstFieldLine, _RestLines, []) :- !.
extract_all_fields_4(FieldID, FirstFieldLine, RestLines,
                  [ExtractedFields|RestCitationFields]) :-
	extract_rest_of_field(RestLines, RestFieldLines, NewFieldID,
			      NewFirstFieldLine, NewRestLines),
	( FirstFieldLine == [] ->
	  ExtractedFields = [FieldID]
	; ExtractedFields = [FieldID,[FirstFieldLine|RestFieldLines]]
	),
	extract_all_fields_4(NewFieldID, NewFirstFieldLine, NewRestLines, RestCitationFields),
	!.

/* extract_rest_of_field(+CitationLines, -FieldLines, -NewFieldID,
                         -NewFirstFieldLine, -NewRestLines)

extract_rest_of_field/5
*/

extract_rest_of_field([], [], none, [], []) :- !.
extract_rest_of_field([First|Rest], [], NewFieldID, NewFirstFieldLine, Rest) :-
	phrase(f_begins_field([NewFieldID,NewFirstFieldLine]), First),
	!.
extract_rest_of_field([""|Rest],RestFieldLines, NewFieldID, NewFirstFieldLine, NewRestLines) :-
	extract_rest_of_field(Rest, RestFieldLines, NewFieldID, NewFirstFieldLine, NewRestLines),
	!.
extract_rest_of_field([First|Rest],[First|RestFieldLines], NewFieldID,
		      NewFirstFieldLine, NewRestLines) :-
	extract_rest_of_field(Rest, RestFieldLines, NewFieldID, NewFirstFieldLine, NewRestLines).

/*  BEGINS FIELD GRAMMAR  */

% Non-DCG version
% f_begins_field(FR,A,B) :-
% 	f_dense_token(Field,A,C),
% 	medline_field_data(Field,_D,_E),
% 	f_separator(_F,C,G),
% 	% format(user_output, 'BEFORE f_any~n', []),
% 	format(user_output, 'BEFORE f_any(~w,~w,~w)~n', [R,G,H]),
% 	f_any(R,G,H),
% 	% format(user_output, 'AFTER f_any~n', []),
% 	format(user_output, 'AFTER f_any(~w,~w,~w)~n', [R,G,H]),
% 	FR = [Field,R],
% 	B = H.

% Cleaned-up DCG version
f_begins_field(FR) -->
	f_dense_token(F),
	{ medline_field_data(F,_,_) },
	f_separator(_),
	f_any(R),
	{ FR = [F,R] }.

% Original DCG version
% f_begins_field(FR) --> f_dense_token(F), {medline_field_data(F,_,_)},
%                        f_separator(_), f_any(R),
%                         {FR=[F,R]}.

% Non-DCG version
% f_dense_token(T,A,B) :-
% 	A = [Char|C],
% 	\+ Char == 32,
% 	\+ Char == 45,
% 	( f_dense_token(U,C,D) ->
% 	  T = [Char|U],
% 	  B = D
% 	; T = [Char],
% 	  B = C
% 	).

% Cleaned-up DCG version
f_dense_token(T) -->
	[Char], { \+ Char == 0' }, { \+ medline_field_separator_char(Char) },
	( f_dense_token(U) ->
	  { T = [Char|U] }
	; { T = [Char] }
	).

% Original DCG version
% f_dense_token(T) --> [Char], {\+Char==0' }, {\+Char==0'-}, f_dense_token(U),
%                      {T = [Char|U]}
%                  |   [Char], {\+Char==0' }, {\+Char==0'-}, {T = [Char]}.

% Non-DCG version
% f_separatorS,A,B) :-
% 	( A = [32,45,32|C],
% 	  f_blanks(Blanks,C,D) ->
% 	  S = [32,45,32|Blanks],
% 	  B = D
% 	; A = [45,32|E],
% 	  f_blanks(Blanks,E,F) ->
% 	  S = [45,32|Blanks],
% 	  B = F
% 	; A = [32|G],
% 	  f_separator(V,G,H) ->
% 	  S = [32|V],
% 	  B = H
% 	).

% Cleaned-up DCG version
f_separator(S) -->
	f_blanks(_B1),
	[Char],
	{ medline_field_separator_char(Char) },
	f_blanks(_B2),
	{ S = Char }.

% f_separator(S) -->
% 	( " - ", f_blanks(B) ->
% 	  { S = [32,45,32|B] }
% 	; "- ",  f_blanks(B) ->
% 	  { S = [45,32|B] }
% 	; " ",   f_separator(V) ->
% 	  { S = [32|V] }
%	).% 

% Original DCG version
% f_separator(S) --> [0' ,0'-,0' ], f_blanks(B), !, {S = [0' ,0'-,0' |B]}
%                |   [0'-,0' ], f_blanks(B), !, {S = [0'-,0' |B]}
%                |   [0' ], !, f_separator(V), {S = [0' |V]}.
% 

% Non-DCG version
% f_blanks(Blanks,A,B) :-
% 	( A = [32|C] ->
% 	  f_blanks(C,C,D),
% 	  Blanks = [32|C],
% 	  B = D
% 	; Blanks = [] ->
% 	  B = A
% 	 ).

% Cleaned-up DCG version
f_blanks(B) -->
	( [Char], { local_ws(Char) } ->
	  f_blanks(C), { B = [32|C] }
	; { B = [] }
	).

% Original DCG version
% f_blanks(B) --> [0' ], !, f_blanks(C), {B = [0' |C]}
%             |   {B = []}.

% Non-DCG version
% f_any(T,A,B) :-
% 	( A = [Char|C] ->
% 	  f_any(U,C,D),
% 	  T = [Char|U],
% 	  B = D
% 	; T = [] ->
% 	  B = A
% 	).
	
% Cleaned-up DCG version
f_any(T) -->
	( [Char] ->
	   f_any(U), { T = [Char|U] }
	; { T = [] }
	).

% Original DCG version
% f_any(T) --> [Char], !, f_any(U), {T=[Char|U]}
%          |    {T=[]}.


/* medline_field(?Field, ?ShortDescription, ?LongDescription)

medline_field/3 is a factual predicate that defines Medline/PubMed fields.
Note that legal fields are either those defined by PubMed or additional
fields (UI, TX, QU and QT) that we use. */

medline_field_data(FieldString, ShortDescription, LongDescription) :-
	atom_codes(FieldAtom, FieldString),
	medline_field(FieldAtom, ShortDescription, LongDescription).

medline_field('UI',
	      'Unique Identifier',
	      'Unique Identifier').
medline_field('TX',
	      'Text',
	      'Text').
medline_field('QU',
	      'Query',
	      'Query').
medline_field('QT',
	      'Query Text',
	      'Query Text').
medline_field('AB',
	      'Abstract',
	      'Abstract').
medline_field('AD',
	      'Affiliation',
	      'Institutional affiliation and address of the first author, and grant numbers').
medline_field('AID',
	      'Article Identifier',
	      'Article ID values may include the pii (controlled publisher identifier) or doi (Digital Object Identifier)').
medline_field('AU',
	      'Author',
	      'Authors').
medline_field('CI',
	      'Copyright Information',
	      'Copyright statement').
medline_field('CIN',
	      'Comment In',
	      'Reference containing a comment about the article').
medline_field('CN',
	      'Corporate Author',
	      'Corporate author or group names with authorship responsibility').
medline_field('CON',
	      'Comment On',
	      'Reference upon which the article comments').
medline_field('DA',
	      'Date Created',
	      'Used for internal processing at NLM').
medline_field('DCOM',
	      'Date Completed',
	      'Used for internal processing at NLM').
medline_field('DEP',
	      'Date of Electronic Publication',
	      'Electronic publication date').
medline_field('DP',
	      'Publication Date',
	      'The date the article was published').
medline_field('EDAT',
	      'Entrez Date',
	      'The date the citation was added to PubMed').
medline_field('EFR',
	      'Erratum For',
	      'Cites the original article needing the correction').
medline_field('EIN',
	      'Erratum In',
	      'Reference containing a published erratum to the article').
medline_field('FAU',
	      'Full Author Name',
	      'Full Author Names').
medline_field('FIR',
	      'Full Investigator',
	      'Full investigator name').
medline_field('FPS',
	      'Full Personal Name as Subject',
	      'Full Personal Name of the subject of the article').
medline_field('GN',
	      'General Note',
	      'Supplemental or descriptive information related to the document').
medline_field('GR',
	      'Grant Number',
	      'Research grant numbers, contract numbers, or both that designate financial support by any agency of the US PHS (Public Health Service)').
medline_field('GS',
	      'Gene Symbol',
	      'Abbreviated gene names (used 1991 through 1996)').
medline_field('IP',
	      'Issue',
	      'The number of the issue, part, or supplement of the journal in which the article was published').
medline_field('IR',
	      'Investigator',
	      'NASA-funded principal investigator').
medline_field('IRAD',
	      'Investigator Affiliation',
	      'Affiliation of NASA-funded principal investigator').
medline_field('IS',
	      'ISSN',
	      'International Standard Serial Number of the journal').
medline_field('JID',
	      'NLM Unique ID',
	      'Unique journal ID in NLM''s catalog of books, journals, and audiovisuals').
medline_field('LA',
	      'Language',
	      'The language in which the article was published').
medline_field('LR',
	      'Last Revision Date',
	      'The date a change was made to the record during a maintenance procedure').
medline_field('MH',
	      'MeSH Terms',
	      'NLM''s controlled vocabulary').
medline_field('MHDA',
	      'MeSH Date',
	      'The date MeSH terms were added to the citation. The MeSH date is the same as the Entrez date until MeSH are added').
medline_field('OAB',
	      'Other Abstract',
	      'Abstract supplied by an NLM collaborating organization').
medline_field('OCI',
	      'Other Copyright Information',
	      'Copyright owner').
medline_field('OID',
	      'Other ID',
	      'Identification numbers provided by organizations supplying citation data').
medline_field('ORI',
	      'Original Report In',
	      'Displays on Patient Summary. Cites original article associated with the patient summary').
medline_field('OT',
	      'Other Term',
	      'Non-MeSH subject terms (keywords) assigned by an organization identified by the Other Term Owner').
medline_field('OTO',
	      'Other Term Owner',
	      'Organization that provided the Other Term data').
medline_field('OWN',
	      'Owner',
	      'Organization acronym that supplied citation data').
medline_field('PG',
	      'Pagination',
	      'The full pagination of the article').
medline_field('PHST',
	      'Publication History Status Date',
	      'History status date').
medline_field('PL',
	      'Place of Publication',
	      'Journal''s country of publication').
medline_field('PMID',
	      'PubMed Unique Identifier',
	      'Unique number assigned to each PubMed citation').
medline_field('PS',
	      'Personal Name as Subject',
	      'Individual is the subject of the article').
medline_field('PST',
	      'Publication Status',
	      'Publication status').
medline_field('PT',
	      'Publication Type',
	      'The type of material the article represents').
medline_field('PUBM',
	      'Publishing Model',
	      'Article''s model of print or electronic publishing').
medline_field('RF',
	      'Number of References',
	      'Number of bibliographic references for Review articles').
medline_field('RIN',
	      'Retraction In',
	      'Retraction of the article').
medline_field('RN',
	      'EC/RN Number',
	      'Number assigned by the Enzyme Commission to designate a particular enzyme or by the Chemical Abstracts Service for Registry Numbers').
medline_field('ROF',
	      'Retraction Of',
	      'Article being retracted').
medline_field('RPF',
	      'Republished From',
	      'Original article').
medline_field('RPI',
	      'Republished In',
	      'Corrected and republished article').
medline_field('SB',
	      'Subset',
	      'Journal/Citation Subset values representing various topic areas').
medline_field('SFM',
	      'Space Flight Mission',
	      'NASA-supplied data space flight/mission name and/or number').
medline_field('SI',
	      'Secondary Source Identifier',
	      'Identifies a secondary source that supplies information, e.g., other data sources, databanks and accession numbers of molecular sequences discussed in articles').
medline_field('SO',
	      'Source',
	      'Composite field containing bibliographic information').
medline_field('SPIN',
	      'Summary For Patients In',
	      'Cites a patient summary article').
medline_field('STAT',
	      'Status Tag',
	      'Used for internal processing at NLM').
medline_field('TA',
	      'Journal Title Abbreviation',
	      'Standard journal title abbreviation').
medline_field('TI',
	      'Title',
	      'The title of the article').
medline_field('TT',
	      'Transliterated / Vernacular Title',
	      'Non-Roman alphabet language titles are transliterated.').
medline_field('UIN',
	      'Update In',
	      'Update to the article').
medline_field('UOF',
	      'Update Of',
	      'The article being updated').
medline_field('VI',
	      'Volume', 'Journal volume').

extract_coord_sents_from_smart(SmartLines, TextFields, NonTextFields,
			       Sentences, CoordinatedSentences,
			       UniqueID, AAs, UDAListIn, UDAListOut, UDA_AVL) :-
	extract_all_smart_fields(SmartLines, CitationFields),
	( select_field("UI", CitationFields, UIField) ->
	  extract_document_unique_identifier(UIField, UniqueID)
	; UniqueID = "00000000"
	),
	extract_coord_sents_from_fields(UniqueID, CitationFields, TextFields,
					NonTextFields, Sentences, CoordinatedSentences,
					AAs, UDAListIn, UDAListOut, UDA_AVL),
	!.

/* extract_all_smart_fields(+SmartLines, -CitationFields)
   concatenate_broken_lines(+SmartLinesIn, -SmartLinesOut)
   extract_each_smart_field(+SmartLines, -CitationFields)

extract_all_smart_fields/2
concatenate_broken_lines/2
extract_each_smart_field/2
*/

extract_all_smart_fields(SmartLines0,CitationFields) :-
    concatenate_broken_lines(SmartLines0,SmartLines),
    extract_each_smart_field(SmartLines,CitationFields),
    !.

/* Fields can be broken across lines. When this is done, an exclam is added
   to the end of the broken line and the rest of the original line continues
   on the next line (presumably it doesn't begin with ".", the field
   designator. */
concatenate_broken_lines([First,Second|Rest],Result) :-
    \+append(".",_,Second),
    !,
    rev(First,RevFirst),
    (RevFirst = [0'!|RestRevFirst] ->
        rev(RestRevFirst,RestFirst),
	append(RestFirst,Second,NewFirst)
    ;   append(First,Second,NewFirst) % maybe should warn when ! is missing
    ),
    concatenate_broken_lines([NewFirst|Rest],Result).
concatenate_broken_lines([First|Rest],[First|ModifiedRest]) :-
    !,
    concatenate_broken_lines(Rest,ModifiedRest).
concatenate_broken_lines(X,X).


extract_each_smart_field([],[]) :-
    !.
extract_each_smart_field([FirstLine|RestLines],
			 [[CitID,[Field]]|RestCitationFields]) :-
    FirstLine = [0'.,FieldChar|Field],
    FieldID = [FieldChar],
    !,
    % temp; this is awful
    ( FieldID=="I" ->
      skr_begin_write('ID'),
      skr_write_string(FirstLine),
      skr_end_write('ID')
    ; true
    ),
    smartfield_to_citationfield(FieldID,CitID),
    extract_each_smart_field(RestLines,RestCitationFields).
extract_each_smart_field([FirstLine|RestLines],CitationFields) :-
    send_message('### WARNING: The following line should begin a field but does not:~n', []),
    send_message('~s~nIt is being ingored.~n~n',[FirstLine]),
    extract_each_smart_field(RestLines,CitationFields).

smartfield_to_citationfield("I","UI") :- !.
smartfield_to_citationfield("T","TI") :- !.
smartfield_to_citationfield("A","AB") :- !.
smartfield_to_citationfield(X,X) :- !.

/* select_field(+FieldID, +CitationFields, -Field)

select_field/3
*/

select_field(FieldID, [[FieldID,Field]|_Rest], Field) :-
	!.
select_field(FieldID, [_First|Rest], Field) :-
	select_field(FieldID, Rest, Field).


/* extract_document_unique_identifier(+UIField, -UniqueID)

extract_document_unique_identifier/2
*/

extract_document_unique_identifier(Field, UniqueID) :-
	( Field = [UniqueID] ->
	  true
	; UniqueID  = "00000000"
	).

extract_coord_sents_from_fields(UniqueID, Fields, TextFields, NonTextFields, Sentences,
				CoordinatedSentences, AAs, UDAListIn, UDAListOut, UDA_AVL) :-
	extract_text_fields(Fields, TextFields0, NonTextFields),
	% need to add an extra " ", because there's a <CR> to take into account
	% between the end of the TI and the beginning of the AB!
	add_whitespace_to_end_of_title(TextFields0, TextFields1), 
	padding_string(Padding),
	unpad_fields(TextFields1, Padding, TextFields2),
	right_trim_last_string(TextFields2, TextFields3),
	find_and_coordinate_sentences(UniqueID, TextFields3, Sentences, CoordinatedSentences,
				      AAs, UDAListIn, UDAListOut, UDA_AVL),
	update_text_fields_with_UDAs(UDAListOut, TextFields0, TextFields).

add_whitespace_to_end_of_title(TextFields0, TextFields1) :-
	( TextFields0 = [["TI",TitleStrings]|Rest] ->
	  TitleStrings = [FirstTitleString|RestTitleStrings],
	  add_ws_to_last_title_string(RestTitleStrings, FirstTitleString, NewTitleStrings),
	  TextFields1 = [["TI",NewTitleStrings]|Rest]
	; TextFields1 = TextFields0
	).

add_ws_to_last_title_string([], LastTitleString, [LastTitleStringWithFinalWS]) :-
	append(LastTitleString, " ", LastTitleStringWithFinalWS).
add_ws_to_last_title_string([NextTitleString|RestTitleStrings],
			    TitleString, [TitleString|RestNewTitleStrings]) :-
	add_ws_to_last_title_string(RestTitleStrings, NextTitleString, RestNewTitleStrings).


right_trim_last_string([], []).
right_trim_last_string([H|T], TextFieldsOut) :-
	TextFieldsIn = [H|T],
	% get the last [FieldName, Fieldtrings] in TextFieldsIn
	append(TextFieldsIn0, [[LastFieldName, LastTextFieldStrings]], TextFieldsIn),
	% get the last String in FieldStrings
	append(LastTextFieldStrings0, [LastTextFieldLastString], LastTextFieldStrings),
	!,
	trim_whitespace_right(LastTextFieldLastString, TrimmedLastTextFieldLastString),
	append(LastTextFieldStrings0, [TrimmedLastTextFieldLastString], LastTextFieldStrings1),
	append(TextFieldsIn0, [[LastFieldName, LastTextFieldStrings1]], TextFieldsOut).

% If there are no UDAs, simply return TextFields unchanged!
update_text_fields_with_UDAs([], TextFields, TextFields).
update_text_fields_with_UDAs([FirstUDA|RestUDAs], TextFieldsIn, TextFieldsOut) :-
	update_text_fields_with_one_UDA(TextFieldsIn, FirstUDA, TextFieldsNext),
	update_text_fields_with_UDAs(RestUDAs, TextFieldsNext, TextFieldsOut).


update_text_fields_with_one_UDA([], _UDA, []).
update_text_fields_with_one_UDA([[FieldName,Strings]|RestFields], UDA,
				[[FieldName,UpdatedStrings]|UpdatedRestFields]) :-
	update_strings_with_one_UDA(Strings, UDA, UpdatedStrings),
	update_text_fields_with_one_UDA(RestFields, UDA, UpdatedRestFields).

% Previous version:
% update_text_fields_with_UDAs([], _UDAList, []).
% update_text_fields_with_UDAs([[FieldName,Strings]|RestField], UDAList,
% 			     [[FieldName,UpdatedStrings]|UpdatedRestFields]) :-
% 	update_strings_with_UDAs(UDAList, Strings, UpdatedStrings),
% 	update_text_fields_with_UDAs(RestField, UDAList, UpdatedRestFields).

% If there are no UDAs, simply return Strings unchanged!
update_strings_with_UDAs([], Strings, Strings).
update_strings_with_UDAs([FirstUDA|RestUDAs], StringsIn, StringsOut) :-
	update_strings_with_one_UDA(StringsIn, FirstUDA, StringsNext),
	update_strings_with_UDAs(RestUDAs, StringsNext, StringsOut).

update_strings_with_one_UDA([], _UDA, []).
update_strings_with_one_UDA([FirstString|RestStrings], UDA:Expansion:_StartPos:_EndPos,
			    [UpdatedFirstString|UpdatedRestStrings]) :-
	tokenize_text_utterly(FirstString, TokenizedFirstString),
	substitute(UDA, TokenizedFirstString, Expansion, UpdatedTokenizedFirstString),
	append(UpdatedTokenizedFirstString, UpdatedFirstString),
	update_strings_with_one_UDA(RestStrings, UDA:Expansion:_StartPos:_EndPos, UpdatedRestStrings).

/* text_field(?TextField)
   text_field/1 is a factual predicate of the individual textual fields.
*/

text_field('DOC').
text_field('QU').
text_field('QT').
text_field('TI').
text_field('AB').
text_field('AS').
text_field('MP').
text_field('OP').
text_field('SP').
text_field('PE').
text_field('RX').
text_field('HX').
text_field('TX').


/* extract_text_fields(+FieldsIn, -TextFields, -NonTextFields)

extract_text_fields/3 computes
 * TextFields, those fields in FieldsIn which satisfy text_field/1, and
 * NonTextFields, those fields in FieldsIn which no not.
*/

extract_text_fields([], [], []).
extract_text_fields([[Field,Lines]|Rest], [[Field,Lines]|RestTextFields], NonTextFields) :-
	atom_codes(FieldAtom, Field),
	text_field(FieldAtom),
	!,
	extract_text_fields(Rest, RestTextFields, NonTextFields).
extract_text_fields([First|Rest], TextFields, [First|RestNonTextFields]) :-
	extract_text_fields(Rest, TextFields, RestNonTextFields).


unpad_fields([], _Padding, []).
unpad_fields([[Field,Lines]|Rest], Padding, [[Field,UnPaddedLines]|UnPaddedRest]) :-
	unpad_lines(Lines, Padding, UnPaddedLines),
	unpad_fields(Rest, Padding, UnPaddedRest).


unpad_lines([], _Padding, []).
unpad_lines([First|Rest], Padding, [UnPaddedFirst|UnPaddedRest]) :-
	( append(Padding, UnPaddedFirst, First) ->
	  true
	; UnPaddedFirst = First
	),
	unpad_lines(Rest, Padding, UnPaddedRest).
