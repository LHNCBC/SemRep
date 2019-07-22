
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

% File:	    tagger_access.pl
% Module:   Tagger Access
% Author:   Lan
% Purpose:  Provide access to the NLS Tagger Server

:- module(tagger_access, [
	% must be exported for filter_mrconso
	% not any more!
	% tag_text/2,
 	tag_text/5
   ]).

:- use_module(metamap(metamap_tokenization), [
	local_punct/1,
	no_combine_pronoun/1
   ]).

:- use_module(skr(skr_utilities), [
        fatal_error/2
   ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	atom_codes_list_list/2,
	split_string/4,
	split_string_completely/3,
	trim_and_compress_whitespace/2
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(server_choice), [
	get_server_stream/2
   ]).

:- use_module(skr(skr_utilities), [
	ensure_atom/2,
	ensure_number/2
   ]).

% :- use_module(skr_lib(ctypes), [
% 	is_punct/1
%     ]).

:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	lower/2,
	replist/3,
	ttyflush/0,
	with_input_from_chars/3
   ]).

:- use_module(library(between), [
	between/3
   ]).

:- use_module(library(codesio), [
	read_from_codes/2
   ]).

:- use_module(library(lists), [
	append/2,
	last/2,
	rev/2
   ]).

:- use_module(library(random), [
	random_member/2
   ]).

:- use_module(library(system), [
	environ/2
    ]).

/* tag_text(+Input, -TaggedTextList)
   tag_text(+Input, -FullTaggedTextList, -TaggedTextList, -TaggedTextStrings)
   tag_text_with_options(+Input, +ModeOption, +PrologOption, -TaggedTextList)
   tag_text_aux(+OptionsAtom, +QueryAtom, -TaggedTextList)

tag_text_with_options/4 tags Input with ModeOption (syn, sem, or semonly)
and PrologOption (prolog or prologfull).  tag_text/2 calls
tag_text_with_options/4 with ModeOption syn and PrologOption prolog.
tag_text/4 tags Input producing all forms of syn output: prolog, prologfull
and normal.
TaggedTextList is list of taggings each of which is a list consisting of a
token and its tag (both atoms, even if Input is a string).
TaggedTextStrings strings consisting of the human-readable output. */

% tag_text/2 is used by filter_mrconso -- do not remove!
% Not any more!
% tag_text(Input, TaggedTextList) :-
% 	get_server_stream('TAGGER', TaggerServerStream),
%         tag_text_with_options(Input, TaggerServerStream, syn, prologfull, TaggedTextList).

tag_text(Input, TaggerServerStream, FullTaggedTextList, TaggedTextList, TaggedTextStrings) :-
	tag_text_with_options(Input, TaggerServerStream, syn, prologfull, FullTaggedTextList),
	!,
	( atom_codes_list_list(FullTaggedTextList, FullTaggedTextListStrings) ->
	  form_prolog_output(FullTaggedTextListStrings, TaggedTextList),
	  form_human_readable_output(FullTaggedTextListStrings, TaggedTextStrings)
	; fatal_error('tag_text/4 failed for ~p~n returning ~p~n', [Input,FullTaggedTextList])
	),
	!.

tag_text_with_options([], _, _, _, []) :- !.
tag_text_with_options(Input, TaggerServerStream, ModeOption, PrologOption, TaggedTextList) :-
	concat_atom([ModeOption, '|', PrologOption], Options),
	ensure_atom(Input, QueryAtom),
	call_tagger(Options, TaggerServerStream, QueryAtom, TaggedTextList0),
	TaggedTextList0 \== '',
	TaggedTextList0 \== end_of_file,
	% TaggedTextList = TaggedTextList0,
	postprocess_tagger_apostrophe_s(TaggedTextList0, TaggedTextList),
	!.
tag_text_with_options(Input, _, _, _Tagger, []) :-
	format('tagger_access:tag_text_with_options/4 failed for ~p~n', [Input]).

call_tagger(Options, TaggerServerStream, QueryAtom, TaggedTextList) :-
	% format(user_output, 'Chose tagger ~w~n', [ChosenTaggerServerHost]),
	call_tagger_server(Options, TaggerServerStream, QueryAtom, TaggedTextAtom),
	!,
	atom_codes(TaggedTextAtom, TaggedTextString),
	escape_backslashes(TaggedTextString, EscapedString),
	% special case when result begins with ^J
	( TaggedTextString = [10|_] ->
	  TaggedTextList = []
	; with_input_from_chars(read(Stream,TaggedTextList),
				Stream,
				EscapedString)
	).

call_tagger(_Options, _TaggerServerStream, QueryAtom, _TaggedTextList) :-
	fatal_error('Tagger (call_tagger_aux) failed on "~w"~n.', [QueryAtom]).


% There's something I don't completely understand about SP's being compliant
% with ISO escape sequences, but QP's not being so. At any rate, that means that, e.g.,
% ['\', 'noun/4'] must be explicitly mangled to ['\\', 'noun/4'] for the string to be
% successfully read in.
escape_backslashes(TaggedTextString, EscapedString) :-
	( append([Prefix,[39,92,39],Suffix], TaggedTextString) ->
	  escape_backslashes(Suffix, EscapedSuffix),
	  append([Prefix,[39,92,92,39],EscapedSuffix], EscapedString)
	; EscapedString = TaggedTextString
	).

form_prolog_output([], []).
form_prolog_output([[WordString,TypeString]|Rest],
		   [[Word,ModifiedType]|ModifiedRest]) :-
	atom_codes(Word, WordString),
	( split_string(TypeString, "/", ModifiedTypeString, _) ->
	  atom_codes(ModifiedType, ModifiedTypeString)
	; atom_codes(ModifiedType,TypeString)
	),
	form_prolog_output(Rest,ModifiedRest).

form_human_readable_output([], [""]) :- !.
form_human_readable_output(FTTLS, Result) :-
	form_human_readable_output_aux(FTTLS, "", "", [], RevResult),
	rev(RevResult, Result).

form_human_readable_output_aux([], CurWords, CurTypes, RRIn, RR) :-
	( CurWords == "" ->
	  RR = RRIn
	; ( RRIn == [] ->
	    RR = [CurTypes,CurWords]
	  ; RR = [CurTypes,CurWords,""|RRIn]
	  )
	).
form_human_readable_output_aux([[Word,Type]|Rest],CurWords,CurTypes,RRIn,RROut) :-
    length(Word,LWord),
    length(Type,LType),
    (   LWord > LType ->
	LNew=LWord,
	LDiff is LWord - LType,
	NewWord=Word,
	replist(0' ,LDiff,Padding),  % ' ,
        append(Type,Padding,NewType)
    ;   LType > LWord ->
	LNew=LType,
        LDiff is LType - LWord,
	NewType=Type,
	replist(0' ,LDiff,Padding),  % ' ,
        append(Word,Padding,NewWord)
    ;   LNew=LType,
        NewWord=Word,
	NewType=Type
    ),
    length(CurWords,LinePos),
    NewLinePos is LinePos + LNew + 1,
    (((NewLinePos < 78;
       CurWords=="")) ->
        % append to current lines
        (CurWords=="" ->
	    NewCurWords=NewWord,
	    NewCurTypes=NewType
	;   append([CurWords," ",NewWord],NewCurWords),
	    append([CurTypes," ",NewType],NewCurTypes)
	),
	RRInOut=RRIn
    ;   % start new lines
	NewCurWords=NewWord,
	NewCurTypes=NewType,
	(RRIn==[] ->
	    RRInOut=[CurTypes,CurWords]
	;   RRInOut=[CurTypes,CurWords,""|RRIn]
	)
    ),
    form_human_readable_output_aux(Rest,NewCurWords,NewCurTypes,RRInOut,RROut).

call_tagger_server(Options, TaggerServerStream, QueryAtom, TaggedTextAtom) :-
	% GIVEN:
	%   options <- Options (is an atom?)
	%   text <- QueryAtom
	%   input <- Options + NewLine + QueryAtom + NewLine + NewLine
	% Original C code
	%   strcpy(input, options);
	%   strcat(input, "\n");
	%   strcat(input, text);
	%   strcat(input, "\n");
	%   strcat(input, "\n");
	% Try to connect 100 times; if it still fails, give up!
	atom_codes(NewLine, [10]), % is there a more elegant way to do this?
	concat_atom([Options,NewLine,QueryAtom,NewLine,NewLine], Request),
	test_post_tagger_query(TaggerServerStream, Request),
	test_get_tagger_result(TaggerServerStream, Request, Response),
	% At this point, we know the tagger is OK,
	% so we can cut out the choice point created by between/3 above
	atom_codes(TaggedTextAtom, Response).
	% close(SocketStream).

% post_Tagger_query/2
test_post_tagger_query(SocketStream, Request) :-
	( format(SocketStream, '~a~n~n^THE_END^~n', [Request]),
	  flush_output(SocketStream) ->
	  true
        ; fatal_error('Unable to post Tagger query~n~w~n', [Request])
        ).

% get_tagger_result/2
test_get_tagger_result(SocketStream, Request, StreamTerm) :-
	( get_chars_tagger(SocketStream, StreamTerm) ->
	  true
        ; fatal_error('Unable to get Tagger result for ~n~w~n', [Request])
        ).

% The tagger server sends back "%%" to signal EOF. No idea why.
get_chars_tagger(Stream, Input) :-
	get_code(Stream, Code),
	( Code =:= 37,
	  peek_code(Stream, NextCode),
	  NextCode =:= 37 ->
	  Input = []
	; otherwise ->
	  Input = [Code|Codes],
	  get_chars_tagger(Stream, Codes)
	).

% The idea here is to overcome a feature of the tagger that
% breaks XYX's into [XYZ, ', s]; this predicate gloms the "'s"
% onto the previous token.

% Transform, e.g.,
% [finkelstein,noun], ['\'',ap], [s,'noun/3'], [test,'verb/3'], [positive,'adj/2']
% into
% ['finkelstein\'s',noun], [test,'verb/3'], [positive,'adj/2']
% by glomming the "'s" onto the end of the preceeding token.
% However, do NOT do this glomming if the token immedidately preceeding the ['\'',ap]
% has LexCat == pron (the exceptions are "he", "she", and "it")
% or if that token ends in a punctuation char.

% In other words, by deMorgan's laws, do the glomming IF the preceeding token
% (1) is not a he/she/it pronoun AND
% (2) does not end in a punctuation char

postprocess_tagger_apostrophe_s([], []).
postprocess_tagger_apostrophe_s(TaggedTextList0, TaggedTextList) :-
	TaggedTextList0 = [[OrigWord,LexCat], ['\'',ap], [s,'noun/3']|RestTaggedTextList0],
	\+ no_combine_pronoun(OrigWord),
	atom_codes(OrigWord, OrigWordCodes),
	last(OrigWordCodes, LastCode),
	\+ local_punct(LastCode),
	!,
	concat_atom([OrigWord, '''', s], Noun),
	TaggedTextList = [[Noun,LexCat]|RestTaggedTextList],
	postprocess_tagger_apostrophe_s(RestTaggedTextList0, RestTaggedTextList).
postprocess_tagger_apostrophe_s([H|RestIn], [H|RestOut]) :-
	postprocess_tagger_apostrophe_s(RestIn, RestOut).

%%% postprocess_tagger_apostrophe_s([], _QueryAtom, []).
%%% postprocess_tagger_apostrophe_s(TaggedTextList0, QueryAtom, TaggedTextList) :-
%%% % 	TaggedTextList0 = [['\'',ap],[OrigWord,LexCat], ['\'',ap]|RestTaggedTextList0],
%%% % 	concat_atom(['''', OrigWord, ''''], OrigWordWithSingleQuotes),
%%% % 	sub_atom(QueryAtom, _Before, _Length, _After, OrigWordWithSingleQuotes),
%%% % 	!,
%%% % 	TaggedTextList = [[OrigWordWithSingleQuotes,LexCat]|RestTaggedTextList],
%%% % 	postprocess_tagger_apostrophe_s(RestTaggedTextList0, QueryAtom, RestTaggedTextList).
%%% postprocess_tagger_apostrophe_s(TaggedTextList0, QueryAtom, TaggedTextList) :-
%%% 	TaggedTextList0 = [[OrigWord,LexCat], ['\'',ap], [s,'noun/3']|RestTaggedTextList0],
%%% 	\+ no_combine_pronoun(OrigWord),
%%% 	atom_codes(OrigWord, OrigWordCodes),
%%% 	last(OrigWordCodes, LastCode),
%%% 	\+ local_punct(LastCode),
%%% 	!,
%%% 	concat_atom([OrigWord, '''', s], Noun),
%%% 	TaggedTextList = [[Noun,LexCat]|RestTaggedTextList],
%%% 	postprocess_tagger_apostrophe_s(RestTaggedTextList0, QueryAtom, RestTaggedTextList).
%%% postprocess_tagger_apostrophe_s(TaggedTextList0, QueryAtom, TaggedTextList) :-
%%% 	TaggedTextList0 = [[OrigWord,LexCat], ['\'',ap], [t,noun]|RestTaggedTextList0],
%%% 	combines_with_t(OrigWord),
%%% 	atom_codes(OrigWord, OrigWordCodes),
%%% 	last(OrigWordCodes, LastCode),
%%% 	\+ local_punct(LastCode),
%%% 	!,
%%% 	concat_atom([OrigWord, '''', t], Noun),
%%% 	TaggedTextList = [[Noun,LexCat]|RestTaggedTextList],
%%% 	postprocess_tagger_apostrophe_s(RestTaggedTextList0, QueryAtom, RestTaggedTextList).
%%% % postprocess_tagger_apostrophe_s(TaggedTextList0, QueryAtom, TaggedTextList) :-
%%% % 	TaggedTextList0 = [[OrigWord,LexCat], ['\'',ap]|RestTaggedTextList0],
%%% % 	concat_atom([OrigWord, ''''], OrigWordWithSingleQuotes),
%%% % 	sub_atom(QueryAtom, _Before, _Length, _After, OrigWordWithSingleQuotes),
%%% % 	!,
%%% % 	TaggedTextList = [[OrigWordWithSingleQuotes,LexCat]|RestTaggedTextList],
%%% % 	postprocess_tagger_apostrophe_s(RestTaggedTextList0, QueryAtom, RestTaggedTextList).
%%% postprocess_tagger_apostrophe_s([H|RestIn], QueryAtom, [H|RestOut]) :-
%%% 	postprocess_tagger_apostrophe_s(RestIn, QueryAtom, RestOut).
%%% 
%%% no_combine_pronoun(he).
%%% no_combine_pronoun(she).
%%% no_combine_pronoun(it).
%%% 
%%% 
%%% combines_with_t(Word) :-
%%% 	lower(Word, LowerWord),
%%% 	combines_with_t_1(LowerWord).
%%% 
%%% combines_with_t_1(don).
%%% combines_with_t_1(doesn).
%%% combines_with_t_1(isn).
%%% combines_with_t_1(shouldn).
%%% combines_with_t_1(won).
