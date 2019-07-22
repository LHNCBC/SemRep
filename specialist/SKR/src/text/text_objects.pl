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

% File:     text_objects.pl
% Module:   Text Objects
% Author:   Lan
% Purpose:  To provide support for text objects (sentences, parentheticals, AAs, ...)

:- module(text_objects, [
	create_nomap_pairs/2,
	dump_all_AAs/4,
	dump_all_UDAs/4,
	extract_token_atoms/2,
	extract_an_lc_atoms/2,
	find_and_coordinate_sentences/8,
        get_UDAs/1,
        maybe_dump_AAs/3,
	reformat_AA_list_for_output/3
    ]).

:- use_module(skr(skr_xml), [
	xml_output_format/1
    ]).

:- use_module(skr_db(db_access), [
	all_digits/1
    ]).

:- use_module(metamap(metamap_tokenization), [
	tokenize_fields_utterly/2,
	tokenize_text_utterly/2,
	tokenize_text/2
    ]).

:- use_module(skr(skr), [
	stop_and_halt/0
   ]).


:- use_module(skr(skr_text_processing), [
	get_skr_text_2/2
   ]).

:- use_module(skr(skr_utilities), [
	check_valid_file_type/2,
	fatal_error/2,
	token_template/5,
	send_message/2,
	write_token_list/3
    ]).

:- use_module(skr_lib(nls_avl), [
	add_to_avl_once/4
    ]).

:- use_module(skr_lib(nls_strings), [
	concatenate_items_to_atom/2,
	lex_stop_word/1,
	split_string_completely/3,
	trim_and_compress_whitespace/2,
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
	ttyflush/0
    ]).

:- use_module(text(sentence_initial_words), [
	sentence_initial_word/1
    ]).

:- use_module(text(text_object_io), [
	to_io_global/2,
	translate_newlines_to_backslash/2,
	write_warning/4
    ]).

:- use_module(text(text_object_tokens), [
	compute_token_position/2,
	extract_text_from_tokens/2,
	filter_out_field_comments/2,
	form_field_tokens/2,
	form_simple_tokens/4,
	get_token_position/2,
	position_contains/2,
	position_ge/2,
	remove_bracketing/2,
	split_scope/4,
	trim_ws_tokens/2
    ]).

:- use_module(text(text_object_util), [
	aa_tok/1,
	aadef_tok/1,
	alpha_type/1,
	an_tok/1,
	an_type/1,
	at_an_tok/1,
	at_ws_or_exlb_tok/1,
	at_ws_tok/1,
	at_ws_or_pn_tok/1,
	brackets/2,
	break_punc/1,
	contains_alpha_tok/1,
	ex_lbracket_tok/1,
	ex_rbracket_tok/1,
	gather_whitespace/3,
	higher_order_tok/1,
	higher_order_type/1,
	hyphen_punc/1,
	hyphen_or_slash_tok/1,
	ic_tok/1,
	lbracket_tok/1,
	lc_tok/1,
	multi_brackets/2,
	ne_lbracket_tok/1,
	ne_rbracket_tok/1,
	nu_tok/1,
	pe_tok/1,
	pn_tok/1,
	punc_tok/1,
	rbracket_tok/1,
	sentence_punc/1,
	sentence_punc_tok/1,
	token_sequence_length/4,
	uc_tok/1,
	utterance_punc/1,
	utterance_punc_tok/1,
	ws_tok/1
    ]).

:- use_module(library(avl), [
	empty_avl/1,
	avl_member/2,
	avl_member/3,
	avl_to_list/2,
	list_to_avl/2
    ]).

:- use_module(library(file_systems), [
	close_all_streams/0
    ]).

:- use_module(library(lists), [
	append/2,
	last/2,
	rev/2,
	select/3,
	sublist/4,
	suffix/2
    ]).

:- dynamic aa_cutoff_value/1.


/* is_at_sentence_boundary(+Tokens, +RevPre, -RBWSs, -NewRest)
   is_at_parenthetical_sentence_boundary(+Tokens, +RevPre, -RBWSs, -NewRest)

is_at_sentence_boundary/4 determines if Tokens is at a sentence boundary,
i.e., it begins with a sentence punctuation token followed by a right-bracket
whitespace sequence and satisfies further conditions (essentially to avoid
breaking at an abbreviation followed by something that doesn't look like
the beginning of a sentence, e.g., E. coli).
NOTE: This predicate assumes no knowledge about the interaction between
      parenthetical expressions and sentences. That is why it checks
      for a RIGHT-BRACKET whitespace sequence. If parenthetical expressions
      and sentences are being determined simultaneously, the
      predicate is_at_parenthetical_sentence_boundary/4 must be used after
      finding matching right bracketing at the top level because
      sentence boundary detection has been disabled.
*/

is_at_sentence_boundary([Token|Rest], RevPre, RBWSs, NewRest) :-
	sentence_punc_tok(Token),
%	\+ at_ws_tok(RevPre),	% must not be preceded by whitespace
	rb_ws_seq(Rest,RBs,WSs,RBWSs,NewRest),
	( NewRest == [] ->	% has to end, nothing left
	  true
	; WSs == [],            % no whitespace, but next token is
	  Rest = [NextToken|_], % a known sentence-initial word
	  % NextToken = tok(ic, TokenString, _LCTokenString, _Pos),
	  token_template(NextToken, ic, TokenString, _LCTokenString, _Pos),
	  atom_codes(TokenAtom, TokenString),
	  likely_sentence_initial_word(TokenAtom, TokenString) ->
	  true
	; WSs == [] ->	% otherwise, no whitespace, so fail.
	  fail
	; RBs \== [] ->	% non-null RBs signal break
	  true
	; non_ws_seq(RevPre,RevNWSs,_),
	  % Test if the current sentence punc token could signal an abbreviation,
          % i.e., if the previous token is alphabetic and not a multi-char uc.
          can_be_abbreviation(RevNWSs,CBAYN),
	  % NewRest is the tokens after the sentence punc token
          can_begin_sentence(NewRest, Token, CBSYN),
	  ( control_value(debug, DebugFlags),
	    memberchk(2, DebugFlags) ->
	    rev(RevNWSs,NWSs),
	    extract_text_from_tokens(NWSs,PreT0),
	    % Token = tok(_,T,_,_),
	    token_template(Token, _Type, T,_LCT, _PosInfo),
	    extract_text_from_tokens(RBWSs,RBWSsT0),
	    non_ws_seq(NewRest,NRNWSs,_),
	    extract_text_from_tokens(NRNWSs,PostT0),
	    append([PreT0,T,RBWSsT0,PostT0],AllT0),
	    translate_newlines_to_backslash(AllT0,AllT),
	      ( to_io_global(warnings_stream,Stream) ->
		true
	      ; Stream = user_output
	      ),
	    format(Stream,'iasb:~s|~a|~a|~n', [AllT,CBAYN,CBSYN])
	  ; true
	  ),
	   ( CBAYN == n
	   ; CBSYN == y)
	),
	!.

likely_sentence_initial_word(TokenAtom, TokenString) :-
	( sentence_initial_word(TokenAtom) ->
	  true
	; length(TokenString, TokenStringLength),
	  TokenStringLength > 6
	).

is_at_parenthetical_sentence_boundary(Tokens,RevPre,RBWSs,NewTokens) :-
    rb_seq(RevPre,_RBs,NewRevPre),
    NewRevPre=[Token|RestNewRevPre],
    sentence_punc_tok(Token),
    \+ at_ws_tok(RestNewRevPre),  % must not be preceded by whitespace
    rb_ws_seq(Tokens,RBs,WSs,RBWSs,NewTokens),
    (   NewTokens==[] ->  % has to end, nothing left
        true
    ;   WSs==[] ->      % must be whitespace
        fail
    ;   RBs\==[] ->     % non-null RBs signal break
        true
    ;   non_ws_seq(RestNewRevPre,RevNWSs,_),
        can_be_abbreviation(RevNWSs,CBAYN),
        can_begin_sentence(NewTokens, '', CBSYN),
	( control_value(debug, DebugFlags),
	  memberchk(2, DebugFlags) ->
          rev(RevNWSs,NWSs),
          extract_text_from_tokens(NWSs,PreT0),
	  Token = tok(_,T,_,_),
          token_template(Token, _Type, T, _LCT, _PosInfo),
          extract_text_from_tokens(RBWSs,RBWSsT0),
          non_ws_seq(NewTokens,NRNWSs,_),
          extract_text_from_tokens(NRNWSs,PostT0),
          append([PreT0,T,RBWSsT0,PostT0],AllT0),
          translate_newlines_to_backslash(AllT0,AllT),
          ( to_io_global(warnings_stream,Stream) ->
            true
          ; Stream=user_output
          ),
	  format(Stream,'iasb:~s|~a|~a|~n', [AllT,CBAYN,CBSYN])
        ; true
        ),
        ( CBAYN == n
	; CBSYN == y
	)
    ),
    !.
rb_ws_seq(TokensIn, RBs, WSs, RBWSs, TokensOut) :-
	rb_seq(TokensIn, RBs, TokensInOut),
	ws_seq(TokensInOut, WSs, TokensOut),
	append(RBs, WSs, RBWSs),
	!.

rb_seq([], [], []) :- !.
rb_seq([RBToken|RestIn], [RBToken|RBs], RestOut) :-
	rbracket_tok(RBToken),
	!,
	rb_seq( RestIn, RBs, RestOut).
rb_seq(TokensIn, [], TokensIn).

ws_seq([], [], []) :- !.
ws_seq([WSToken|RestIn], [WSToken|WSs], RestOut) :-
	ws_tok(WSToken),
	!,
	ws_seq(RestIn, WSs, RestOut).
ws_seq(TokensIn, [], TokensIn).

non_ws_seq([], [], []) :- !.
non_ws_seq([WSToken|RestIn], [], [WSToken|RestIn]) :-
	ws_tok(WSToken),
	!.
non_ws_seq([First|RestIn], [First|NWSs], TokensOut) :-
    non_ws_seq(RestIn, NWSs, TokensOut).

can_be_abbreviation([], y).
can_be_abbreviation([tok(Type,Text,_,_)|_], YN) :-
	% any alphabetic token (except for uc of length > 1) can be an abbreviation
	% note that tokens are in reverse order
	( Type == lc ->
	  YN = y
	; Type == ic ->
	  YN = y
	; Type == mc ->
	  YN = y
	; ( Type == uc,
	    length(Text,1)) ->
	  YN = y
	; YN = n
	).
%% recurse for complex tokens (?)
%can_be_abbreviation([tok(_Type,Scope,_)|_],YN) :-
%    rev(Scope,RevScope),
%    can_be_abbreviation(RevScope,YN),
%    !.
%% just in case
%can_be_abbreviation(_RevTokens,n).


% the only thing which cannot begin a sentence is an isolated lc, i.e.,
% followed by nothing, whitespace or sentence, utterance or break punctuation
can_begin_sentence(TokenList, PreviousToken, Result) :-
	% If the previous token was a semicolon, it doesn't matter what comes next;
	% declare a sentence break!
	( utterance_punc_tok(PreviousToken) ->
	  Result = y
	; can_begin_sentence_1(TokenList, Result)
	).

% can_begin_sentence_1([tok(lc,_,_,_)], n) :- !.
can_begin_sentence_1([Token], n) :-
	token_template(Token, lc, _String, _LCString, _PosInfo),
	!.
% can_begin_sentence_1([tok(lc,_,_,_), tok(ws,_,_,_)|_], n) :- !.
can_begin_sentence_1([Token1,Token2|_], n) :-
	token_template(Token1, lc, _Token1String, _Token1LCString, _Token1PosInfo_),
	token_template(Token2, ws, _Token2String, _Token2LCString, _Token2PosInfo_),
	!.
% can_begin_sentence_1([tok(lc,_,_,_), tok(pn,PN,_,_)|_], n) :-
can_begin_sentence_1([Token1, Token2|_], n) :-
	token_template(Token1, lc, _Token1String, _Token1LCString, _Token1PosInfo_),
	token_template(Token2, pn,  PN,           _Token2LCString, _Token2PosInfo_),
	( sentence_punc(PN) ->
	  true
	; utterance_punc(PN) ->
	  true
	; break_punc(PN) ->
	  true
	; hyphen_punc(PN) ->
	  true
	),
	!.
can_begin_sentence_1(_Tokens, y).
%can_begin_sentence([tok(Type,_,_,_)],n) :-
%    (Type==lc; Type==nu),
%    !.
%can_begin_sentence([tok(Type,_,_,_),tok(ws,_,_,_)|_],n) :-
%    (Type==lc; Type==nu),
%    !.
%can_begin_sentence([tok(Type,_,_,_),tok(pn,PN,_,_)|_],n) :-
%    (Type==lc; Type==nu),
%    (sentence_punc(PN); utterance_punc(PN); break_punc(PN)),
%    !.
%can_begin_sentence(_Tokens,y).

/* find_field_sentences(+FieldTokens, -FieldSentences)
   find_sentences(+Tokens, +Label, -Sentences)
   find_sentences(+Tokens, +RevPre, +Level, +Label, -Sentences)
   find_bracketing(+TokensIn, +RevPre, +Level, +LBList, +Label,
                   -LBToClear, -RevBExpr, -TokensOut)
*/

find_field_sentences([], []) :- !.
find_field_sentences([[Field,Tokens]|Rest],
		     [[Field,Sentences]|RestFieldSentences]) :-
	find_sentences(Tokens, [], 0, 'dummy.ab.1', Sentences),
	find_field_sentences(Rest, RestFieldSentences).

find_sentences([], [], _, _, []) :- !.
find_sentences([], RevPre, Level, Label, [SnTok|Sentence]) :-
	% out of input; missing sentence boundary
	!,
	construct_sentence_token(RevPre, Level, SnTok, Sentence),
	( control_option(warnings) ->
	  extract_text_from_tokens(Sentence, SentenceText),
	  write_warning(SentenceText, wsb, Label, 'Missing sentence boundary')
	; true
	).
find_sentences([Token|Rest], RevPre, Level, Label, Sentences) :-
	% look for bracketing expression
	lbracket_tok(Token),
	% always respect whitespace for now
	%    (control_option(respect_whitespace) ->
	%        at_ws_tok(RevPre)
	%    ;   true
	%    ),
	at_ws_tok(RevPre),
	% Token=tok(pn,LB,LB,_),
	token_template(Token, pn, LB, LB, _PosInfo),
	NewLevel is Level + 1,
	ConsumedTokenCount is 0,
	find_bracketing(Rest, ConsumedTokenCount, [Token], NewLevel, [LB], Label, _, RevPE, NewRest),
	% make sure the bracketing doesn't skip ahead more than 2000 characters;
	% analying > 90,000 citations revealed no gap > 1371 chars.
	test_bracketing_distance(Rest, NewRest),
	% format(user_output, '### Bracketing Diff ~w  ~d~n', [Label, Diff]),
	% skr_fe:write_token_list([FirstRest], 0, 1),
	% skr_fe:write_token_list([FirstNewRest], 0, 1),
	% nl(user_output),
	construct_parenthetical_token(RevPE,NewLevel,PETok,_PE),
	append([RevPE,[PETok],RevPre],NewRevPre),
	( (Level=:=0,
	   is_at_parenthetical_sentence_boundary(NewRest, NewRevPre, RBWSs, NewNewRest)) ->
	   % note that here RBWSs should only consist of WSs (except in the highly
	   % unlikely occurrence of RBs without a preceding LB)
	   rev(RBWSs, RevRBWSs),
	   append(RevRBWSs, NewRevPre, RevS),
	   construct_sentence_token(RevS, Level, SnTok, Sentence),
	   SentenceWithTok = [SnTok|Sentence],
	   append(SentenceWithTok, RestSentences, Sentences),
	   find_sentences(NewNewRest, [], Level, Label, RestSentences)
	;  find_sentences(NewRest, NewRevPre, Level, Label, Sentences)
	).
find_sentences([Token|Rest], RevPre, Level, Label, Sentences) :-
	% look for sentence break
	is_at_sentence_boundary([Token|Rest], RevPre, RBWSs, NewRest),
	% note that here RBWSs should only consist of WSs (except in the highly
	% unlikely occurrence of RBs without a preceding LB)
	!,
	rev(RBWSs, RevRBWSs),
	append([RevRBWSs,[Token],RevPre], RevS),
	construct_sentence_token(RevS, Level, SnTok, Sentence),
	SentenceWithTok = [SnTok|Sentence],
	append(SentenceWithTok, RestSentences, Sentences),
	find_sentences(NewRest, [], Level, Label, RestSentences).
find_sentences([First|Rest], RevPre, Level, Label, Sentences) :-
	find_sentences(Rest, [First|RevPre], Level, Label, Sentences).

test_bracketing_distance(Rest, NewRest) :-
	( Rest == [] ->
	  true
	; NewRest == [] ->
	  true
	; Rest = [FirstRest|_],
	  NewRest = [FirstNewRest|_],
	  % FirstRest = tok(_, _, _, pos(Start1,_End1)),
	  token_template(FirstRest, _FirstType, _FirstString, _FirstLCString, pos(Start1Pos,_End1)),
	  % FirstNewRest = tok(_, _, _, pos(Start2,_End2)),
	  token_template(FirstNewRest, _NewType, _NewString, _NewLCString, pos(Start2Pos,_End2)),
	  CharPosDiff is Start2Pos - Start1Pos,
	  CharPosDiff < 2000
	).

find_bracketing([Token|Rest], _ConsumedTokenCount, RevPre, _Level,
		[LB|_RestLB], _Label, LBToClear, [Token|RevPre], Rest) :-
	% look for closing bracket (BEFORE looking for another opening bracket)
	rbracket_tok(Token),
	!,
	% if it's non-exclusive, we must NOT be at whitespace
	( ne_rbracket_tok(Token) ->
	  \+ at_ws_tok(RevPre)
	;  true
	),
	% Token=tok(pn,RB,RB,_),
	token_template(Token, pn, RB, RB, _PosInfo),
	(  brackets(LB, RB) ->
	   LBToClear = ""
	; multi_brackets(LBToClear, RB) ->
	  true
	),
	% bracketed text must be non-null
	( RevPre = [_] ->
	  !,
	  fail
	; true
	),
	!.
find_bracketing([Token|RestIn], ConsumedTokenCount, RevPre, Level,
		LBList, Label, LBToClearOut, RevBExpr, RestOut) :-
	% look for nested bracketing expression (AFTER looking for closing bracket)
	lbracket_tok(Token),
	% if it's non-exclusive, we must be at whitespace or an exclusive lb
	( ne_lbracket_tok(Token) ->
	  at_ws_or_exlb_tok(RevPre)
	; true
	),
	% Token=tok(pn,LB,LB,_),
	token_template(Token, pn, LB, LB, _PosInfo),
	NewLevel is Level + 1,
	find_bracketing(RestIn, 0, [Token], NewLevel, [LB|LBList], Label,
			LBToClear, RevSubBExpr, RestInOut),
	RevSubBExpr = [ClosingSubExprBracket|_RestSubExpr],
	% Determine the Left (open) Bracket's char
	token_template(Token, pn, [LBracketTokenChar],
		       _LCLBracketTokenkStr, _LBracketTokenPosInfo),
	% Determine the Right (close) Bracket's char
	token_template(ClosingSubExprBracket, pn, [ClosingSubExprBracketChar],
		       _LCClosingSubExprBracketStr, _ClosingSubExprBracketPosInfo),
	% Require that the left/open and right/close bracket chars match,
	% i.e., "(" and ")" or "[" and "]", but not, e.g., "(" aned "]".
	% This refinementy is designed to handle ill-formed input in which parens/brackets
	% do not match, e.g., the following monstrosity from PMID 3097297:
	% polyriboinosinic acid-polyribocytidylic acid [poly(I.C] was compared as a ...".
	% In that text, we do NOT want "poly(I.C" recognized as a parenthetical expression.

	brackets([LBracketTokenChar], [ClosingSubExprBracketChar]),
	!,
	construct_parenthetical_token(RevSubBExpr, NewLevel, BETok, _SubBExpr),
	% do not allow this subbracketing to be "undone", i.e. fail rather than
	% allow LB to be considered to be non-bracketing
	% always preventing the undoing of the subbracketing seems to be better
	% than conditional undoing based on its "length" and that of what
	% precedes it
	% 1. token length
	%    length(RevPre,LPre),
	%    length(RevSubBExpr,LCur),
	% 2. character length
	%extract_text_from_tokens(RevPre,PT),
	%length(PT,LPre),
	%extract_text_from_tokens(RevSubBExpr,SubT),
	%length(SubT,LCur),
	% 1. and 2.
	%    (LCur < LPre ->
	%        !
	%    ;   true
	%    ),
	% 3. always prevent undoing of the subbracketing
	!,
	( LBToClear == LB ->
	  LBToClearOut = LBToClear,
	% RevBExpr=[SubBEToken|RevPre],
	  append([RevSubBExpr, [BETok],RevPre], RevBExpr),
	  RestOut = RestInOut
	; append([RevSubBExpr, [BETok],RevPre], NewRevPre),
	  length([Token|RestIn], OrigTokenListLength),
	  length(RestInOut, NewTokenListLength),
	  ConsumedTokenCountNext is ConsumedTokenCount + OrigTokenListLength - NewTokenListLength,
          find_bracketing(RestInOut, ConsumedTokenCountNext, NewRevPre, Level, LBList, Label,
			  LBToClearOut, RevBExpr, RestOut)
	).
find_bracketing([Token|RestIn], ConsumedTokenCount, RevPre, Level,
		LBList, Label, LBToClear, RevBExpr, RestOut) :-
	% ConsumedTokenCount < 4600,
	% format(user_output, 'NTC = ~d~n', [ConsumedTokenCount]),
	% format(user_output, '~d:~*c~n', [ConsumedTokenCount,ConsumedTokenCount,42]),
	ConsumedTokenCount1 is ConsumedTokenCount + 1,
 	find_bracketing(RestIn, ConsumedTokenCount1, [Token|RevPre], Level,
			LBList, Label, LBToClear, RevBExpr, RestOut).

construct_sentence_token(RevPre, Level, Token, Pre) :-
	token_template(Token, sn,"", Level, Pos),
	rev(RevPre, Pre),
	compute_token_position(Pre, Pos).

construct_parenthetical_token(RevPre, Level, Token, Pre) :-
	token_template(Token, pe, "", Level, Pos),
	rev(RevPre, Pre),
	compute_token_position(Pre, Pos).


/* 
   get_aa_cutoff_value(-Cutoff)
   default_aa_cutoff_value(?Cutoff)

get_aa_cutoff_value/1 gets the current AA cutoff value. If there is none
it uses default_aa_cutoff_value/1 to provide one.
default_aa_cutoff_value/1 is a factual predicate defining the default AA
cutoff value. */

get_aa_cutoff_value(Cutoff) :-
    aa_cutoff_value(Cutoff),
    !.
get_aa_cutoff_value(Cutoff) :-
    default_aa_cutoff_value(Cutoff),
    assert(aa_cutoff_value(Cutoff)),
    !.

default_aa_cutoff_value(0.30).



/* find_all_aas(+Sentences, +AAsIn, +OutputStream, -AAs)
   find_aas_1(+Tokens, +RevPre, +AAsIn, -AAsOut)

find_all_aas/4 finds the locally-defined Acronym/Abbreviations in Sentences
               for a given PMID
All these predicates create an AVL tree AAs.
find_all_aas/4 simply has an in/out AVL tree.
find_aas_1/5 is an auxiliary for processing a single sentence.  */

find_all_aas(Sentences, _UIString, _OutputStream, AAs) :-
	empty_avl(AAs0),
	% reversed order of args from QP library version!
	last(Sentences, LastToken),
	arg(4, LastToken, pos(_,LastPos)),
	find_aas(Sentences, LastPos, AAs0, AAs),
%	maybe_dump_aas(UIString, AAs, OutputStream),
	!.

maybe_dump_AAs(UIAtom, AAs, OutputStream) :-
	( ( control_option(dump_aas) ->
	    true
	  ; control_option(aas_only)
	  ),
	  \+ control_option(machine_output),
	  \+ xml_output_format(_XMLOutput) ->
	  % The "aa" argument specifies that "AA" should be the first field in he AA output
	  current_output(OutputStream),
	  dump_all_AAs(AAs, aa, UIAtom, OutputStream)
	; true
	).

dump_all_UDAs(UDAList, FirstField, UI, OutputStream) :-
	reformat_UDA_list_for_output(UDAList, UI, OutputUDAList),
	write_AA_data(OutputUDAList, FirstField, 'UA', OutputStream),
	flush_output(OutputStream).

reformat_UDA_list_for_output([], _UI, []).
reformat_UDA_list_for_output([UDA:Expansion:StartPos:EndPos|RestUDAs],
			      UI, [ReformattedUDA|RestReformattedUDAs]) :-
	reformat_one_UDA_for_output(UDA, Expansion, StartPos, EndPos, UI, ReformattedUDA),
	reformat_UDA_list_for_output(RestUDAs, UI, RestReformattedUDAs).

reformat_one_UDA_for_output(UDAString, ExpansionString, StartPos, _EndPos, UI, ReformattedUDAData) :-
	length(UDAString, UDATextLength),
	atom_codes(UDATextAtom, UDAString),
	tokenize_text_utterly(UDAString, TokenizedUDA),
	length(TokenizedUDA, UDATokenLength),
	length(ExpansionString, ExpansionTextLength),
	atom_codes(ExpansionTextAtom, ExpansionString),
	tokenize_text_utterly(ExpansionString, TokenizedExpansion),
	length(TokenizedExpansion, ExpansionTokenLength),
	ReformattedUDAData = UI-UDATextAtom-ExpansionTextAtom-
			     UDATokenLength-UDATextLength-
			     ExpansionTokenLength-ExpansionTextLength-StartPos.


% UDAList looks like
% ["CAT":"Computerized Axial Tomography",
%  "PET":"Positron Emission Tomography"]

% The FirstField argument should be either "aa" or "pmid",
% and determines whether the first two fields should be
% AA|PMID  -- for --dump_aas output, e.g.,
% AA|00000000|HA|heart attack|1|2|3|12
% or
% PMID|AA  -- for --fielded_mmi_output, e.g.,
% 00000000|AA|HA|heart attack|1|2|3|12

dump_all_AAs(AAListTokens, FirstField, UI, OutputStream) :-
	% Key is e.g., "HA"
	% Values is e.g., "Heart Attack"
	% avl_member(Key, AVL, Values),
	% dump_one_avl(Key, UI, Values, OutputStream),
	% fail.
	reformat_AA_list_for_output(AAListTokens, UI, AAList),
	write_AA_data(AAList, FirstField, 'AA', OutputStream),
	flush_output(OutputStream).
	

reformat_AA_list_for_output([], _UI, []).
reformat_AA_list_for_output([AA-Expansion|RestTokenListPairs], UI, [FirstAAData|RestAAData]) :-
	reformat_one_AA_for_output(AA, Expansion, UI, FirstAAData),
	reformat_AA_list_for_output(RestTokenListPairs, UI, RestAAData).

reformat_one_AA_for_output(AA, Expansion, UI, ReformattedAAData) :-
	extract_text_from_tokens(AA, AATextAtom),
	atom_length(AATextAtom, AATextLength),
	get_AA_startpos_and_endpos(AA, AATextLength, StartPos, _EndPos),
	length(AA, AATokenLength),
	extract_text_from_tokens(Expansion, ExpansionTextAtom),
	length(Expansion, ExpansionTokenLength),
	atom_length(ExpansionTextAtom, ExpansionTextLength),
	ReformattedAAData = UI-AATextAtom-ExpansionTextAtom-
			    AATokenLength-AATextLength-
			    ExpansionTokenLength-ExpansionTextLength-StartPos.

get_AA_startpos_and_endpos(AA, AATextLength, StartPos, EndPos) :-
	AA = [FirstAAToken|_],
	FirstAAToken = tok(_,_,_,pos(StartPos,_)),
	EndPos is StartPos + AATextLength.

write_AA_data([], _FirstField, _Type, _OutputStream).
write_AA_data([FirstAAData|RestAAs], FirstField, Type, OutputStream) :-
	FirstAAData = UI-AATextAtom-ExpansionTextAtom-
		      AATokenLength-AATextLength-
		      ExpansionTokenLength-ExpansionTextLength-StartPos,
	output_fields(FirstField, Type, OutputStream, UI, AATextAtom, ExpansionTextAtom,
		      AATokenLength, AATextLength, ExpansionTokenLength, ExpansionTextLength,
		      StartPos, AATextLength),
	write_AA_data(RestAAs, FirstField, Type, OutputStream).

output_fields(FirstField, Type, OutputStream, UI, AATextAtom, ExpansionTextAtom,
	      AATokenLength, AATextLength, ExpansionTokenLength,  ExpansionTextLength,
	      StartPos, EndPos) :-
	set_field_order(FirstField, Type, UI, Field1, Field2),
	update_UDA_pos_info(StartPos, EndPos, PosInfo),
	format(OutputStream, '~w|~w|~w|~w|~d|~d|~d|~d|~w~n',
	       [Field1,Field2,AATextAtom,ExpansionTextAtom,AATokenLength,
		AATextLength, ExpansionTokenLength, ExpansionTextLength, PosInfo]).


% Willie requested that empty posinfo be displayed for UDAs, rather than 0:Length
update_UDA_pos_info(StartPos, EndPos, PosInfo) :-
	( StartPos is 0 ->
	  PosInfo = ''
	; PosInfo = StartPos:EndPos
	).

set_field_order(aa,  _Type, UI, 'AA', UI).
set_field_order(pmid, Type, UI,   UI, Type).

find_aas([], _LastPos, AAsIn, AAsIn).
find_aas([tok(sn,_,_,Pos)|Rest], LastPos, AAsIn, AAsOut) :-
	!,
	split_scope(Rest, Pos, Scope, NewRest),
	find_aas_1(Scope, LastPos, [], AAsIn, AAsInOut),
	find_aas(NewRest, LastPos, AAsInOut, AAsOut).
find_aas([_Token|Rest], LastPos, AAsIn, AAsOut) :-
	find_aas(Rest, LastPos, AAsIn, AAsOut).


find_aas_1([], _LastPos, _, AAsIn, AAsIn).
find_aas_1([PE_Token|Rest], LastPos, RevPre, AAsIn, AAsOut) :-
	pe_tok(PE_Token),
	Rest = [NextToken|_],
	ex_lbracket_tok(NextToken),
	!,
	arg(4, PE_Token, PosTerm),
	% format('~nfind_aas (pe)~n',[]),
	% extract_text_from_tokens([PE_Token|Rest],Text),
	% format(OutputStream,'~s~n~p~n',[Text,[PE_Token|Rest]]),
	split_scope(Rest, PosTerm, Scope0, NewRest),
	% extract_text_from_tokens(Scope0,Scope0Text),
	% format(OutputStream,'Scope0:~n~s~n~p~n',[Scope0Text,Scope0]),
	remove_bracketing(Scope0, Scope),
	% extract_text_from_tokens(Scope,ScopeText),
	% format(OutputStream,'Scope:~n~s~n~p~n',[ScopeText,Scope]),
	% recurse within the parenthetical
	find_aas_1(Scope, LastPos, [], AAsIn, AAsNext1),
	% handle the parenthetical, itself
	% This next section is a first attempt at identifying AAs
	% whose expansion occurs *after* the AA itself, e.g.,
	% "AIDS (Acquired Immune Deficiency Syndrome)"
	find_aa(Scope, Scope0, PE_Token, LastPos, RevPre, AAsNext1, AAsNext2),
	% ( find_aa(Scope, Scope0, PE_Token, LastPos, RevPre, AAsNext1, AAsNext2),
	%   AAsNext2 \= AAsNext1 ->
	%   true
	% ; rev(RevPre, RevRevPre),
	%   rev(Scope, RevScope),
	%   find_aa(RevRevPre, Scope0, PE_Token, LastPos, RevScope, AAsNext1, AAsNext2),
	%   AAsNext2 \= AAsNext1 ->
	%   true
	% ; AAsNext2 = AAsNext1
	% ),
	% not skipping causes aas within a pe to be detected twice
	% skip over Scope
	find_aas_1(NewRest, LastPos, [PE_Token|RevPre], AAsNext2, AAsOut).
	% do NOT skip over Scope
	% find_aas(Rest,[PE_Token|RevPre],AAsNext2,AAsOut,OutputStream).
find_aas_1([Token|Rest], LastPos, RevPre, AAsIn, AAsOut) :-
	find_aas_1(Rest, LastPos, [Token|RevPre], AAsIn, AAsOut).


test_valid_aa(AATokens, LastCitationPos) :-
	test_valid_aa_01(AATokens),
	test_valid_aa_02(AATokens),
	test_valid_aa_03(AATokens),
	test_valid_aa_04(AATokens),
	test_valid_aa_05(AATokens),
	test_valid_aa_06(AATokens),
	test_valid_aa_07(AATokens),
	test_valid_aa_08(AATokens),
	test_valid_aa_09(AATokens),
	test_valid_aa_10(AATokens),
	test_valid_aa_11(AATokens),
	test_valid_aa_12(AATokens),
	test_valid_aa_13(AATokens),
	test_valid_aa_14(AATokens),
	test_valid_aa_15(AATokens),
	test_valid_aa_16(AATokens),
	test_valid_aa_17(AATokens, LastCitationPos),
	test_valid_aa_18(AATokens),
	test_valid_aa_19(AATokens),
	test_valid_aa_20(AATokens).


	
% Proposed AA must not contain > 20 chars; that rules out less than 0.04% of AAs
test_valid_aa_01(AATokens) :-
	AATokens = [AA1|AARest],
	arg(4, AA1, pos(AAStartPos,_)),
	token_sequence_length(AARest, AA1, AAStartPos, AATokensLength),
	AATokensLength > 20,
	!,
	announce_aa_failure('AA-01', AATokens),
	fail.
test_valid_aa_01(_).

% Proposed AA must not
% (1) contain > 3 tokens,
% (2) contain > 3 times as many chars as tokens or contain > 19 chars, and
% (3) contain a hyphen
test_valid_aa_02(AATokens) :-
	AATokens = [AA1|AARest],
	arg(4, AA1, pos(AAStartPos,_)),
	token_sequence_length(AARest, AA1, AAStartPos, AATokensLength),
	length(AATokens, NumAATokens),
	NumAATokens > 3,
	( AATokensLength > 3 * NumAATokens ->
	  true
	; AATokensLength > 19
	),
	\+ memberchk(tok(pe,"-","-",_), AATokens),
	!,
	announce_aa_failure('AA-02', AATokens),
	fail.
test_valid_aa_02(_).

% Proposed AA must not contain forbidden token coorcurrence
test_valid_aa_03(AATokens) :-
	contains_forbidden_token_coocurrence(aa, AATokens),
	!,
	announce_aa_failure('AA-03', AATokens),
	fail.
test_valid_aa_03(_).

% Proposed AA must not contain a forbidden AA token sequence
test_valid_aa_04(AATokens) :-
        contains_forbidden_token_sequence(aa, AATokens),
        !,
	announce_aa_failure('AA-04', AATokens),
        fail.
test_valid_aa_04(_).

% Proposed AA contains only 1 token,
% that token is at least 12 chars
% UPDATED: Proposed AA contains a token of > 10 chars
test_valid_aa_05(AATokens) :-
	member(Token, AATokens),
	arg(4, Token, pos(Start,End)),
	End - Start > 10,
	% \+ uc_tok(SingleToken),
	% \+ strict_an_tok(SingleToken),
	!,
	announce_aa_failure('AA-05', AATokens),
	fail.
test_valid_aa_05(_).

% Proposed AA must not contain four pe tokens
test_valid_aa_06(AATokens) :-
	count_tokens_of_type(AATokens, pe, 0, Count),
	Count > 3,
	!,
	announce_aa_failure('AA-06', AATokens),
	fail.
test_valid_aa_06(_).

% Proposed AA must not contain a token beginning "equal"
test_valid_aa_07(AATokens) :-
	count_prefix_match_tokens(AATokens, 'equal', 0, Count),
	Count > 0,
	!,
	announce_aa_failure('AA-07', AATokens),
	fail.
test_valid_aa_07(_).

% All an tokens in proposed token list are lc tokens
% AND there are at least 5 tokens
test_valid_aa_08(AATokens) :-
	length(AATokens, AATokenLength),
	AATokenLength > 4,
	all_an_tokens_are_lc(AATokens),
	!,
	announce_aa_failure('AA-08', AATokens),
	fail.
test_valid_aa_08(_).

% Proposed AA contains a pe token that spans at least 10 chars
test_valid_aa_09(AATokens) :-
	memberchk(tok(pe,_,_,pos(X,Y)), AATokens),
	Y - X >= 10,
	!,
	announce_aa_failure('AA-09', AATokens),
	fail.
test_valid_aa_09(_).

% Proposed AA contains at least two ", " (comma, space)
test_valid_aa_10(AATokens) :-
	count_comma_space_seq(AATokens, 0, 0, CommaSpaceCount),
	CommaSpaceCount > 1,
	!,
	announce_aa_failure('AA-10', AATokens),
	fail.
test_valid_aa_10(_).

% Proposed AA contains a non-uc an token > 4 chars long and one of ':', ';', '='.
test_valid_aa_11(AATokens) :-
	member(Token, AATokens),
	an_tok(Token),
	arg(4, Token, pos(StartPos,EndPos)),
	EndPos - StartPos > 4,
	member(tok(pn, [Char], [Char], _Pos2), AATokens),
	memberchk(Char, [58,59,61]), % :;=
	!,
	announce_aa_failure('AA-11', AATokens),
	fail.
test_valid_aa_11(_).

% Proposed AA contains a forbidden word
test_valid_aa_12(AATokens) :-
	member(tok(Type, _String, LCString, _Pos1), AATokens),
	Type \== nu,
	Type \== pe,
	atom_codes(LCAtom, LCString),
	forbidden_aa_word(LCAtom),
	!,
	announce_aa_failure('AA-12', AATokens),
	fail.
test_valid_aa_12(_).

% Proposed AA begins with 'such' or 'also', or 'including'.
test_valid_aa_13(AATokens) :-
	AATokens = [FirstAAToken|_],
	FirstAAToken = tok(Type, _String, LCString, _Pos),
	Type \== pe,
	atom_codes(LCAtom, LCString),
	forbidden_first_aa_word(LCAtom),
	!,
	announce_aa_failure('AA-13', AATokens),
	fail.
test_valid_aa_13(_).

% Proposed AA begins with 'i.e' or 'e.g'.
test_valid_aa_14(AATokens) :-
	AATokens = [T1,T2,T3|_],
	T1 = tok(_Type2, _String2, [Char2], _Pos2),
	T2 = tok(pn, ".", "." , _Pos1),
	T3 = tok(_Type3, _String3, [Char3], _Pos3),
	( Char2 == 105, % i
	  Char3 == 101  % e
	; Char2 == 101, % e
	  Char3 == 103  % g
	),
	!,
	announce_aa_failure('AA-14', AATokens),
	fail.
test_valid_aa_14(_).

% Proposed AA contains > 4 tokens
% AND over a third of tokens are multi-letter lc tokens
test_valid_aa_15(AATokens) :-
	length(AATokens, AATokensLength),
	AATokensLength > 4,
	count_multi_letter_lc_tokens(AATokens, 0, MultiLetterLCTokenCount),
	MultiLetterLCTokenCount * 3 > AATokensLength,
	!,
	announce_aa_failure('AA-15', AATokens),
	fail.
test_valid_aa_15(_).

% Proposed AA contains > 10 an chars
% AND over a third of tokens are multi-letter lc tokens
test_valid_aa_16(AATokens) :-
	count_an_token_chars(AATokens, 0, AATokensCharCount),
	AATokensCharCount > 10,
	length(AATokens, AATokensLength),
	count_multi_letter_lc_tokens(AATokens, 0, MultiLetterLCTokenCount),
	MultiLetterLCTokenCount * 3 > AATokensLength,
	!,
	announce_aa_failure('AA-16', AATokens),
	fail.
test_valid_aa_16(_).

% Proposed AA must not occur within 5 chars of the end of the citation.
test_valid_aa_17(AATokens, LastCitationPos) :-
	LastCitationPos > 250,
	% reversed order of args from QP library version!
	last(AATokens, LastAAToken),
	arg(4, LastAAToken, pos(_,LastAAPos)),
	LastAAPos + 5 > LastCitationPos,
	!,
	announce_aa_failure('AA-17', AATokens),
	fail.
test_valid_aa_17(_AATokens, _LastCitationPos).

% Proposed AA cannot be a single lc token of over 6 chars
test_valid_aa_18(AATokens) :-
	AATokens = [SingleToken],
	lc_tok(SingleToken),
	arg(2, SingleToken, TokenChars),
	length(TokenChars, TokenLength),
	TokenLength > 6,
	!,
	fail.
test_valid_aa_18(_AATokens).

% Proposed AA cannot begin or end with a punc token
test_valid_aa_19(AATokens) :-
	AATokens = [FirstAAToken|_RestAATokens],
	last(AATokens, LastAAToken),
	( pn_tok(FirstAAToken) ->
	  true
	; pn_tok(LastAAToken)
	),
	!,
	fail.
test_valid_aa_19(_AATokens).

% Proposed AA cannot consist of a single character
test_valid_aa_20(AATokens) :-
	AATokens = [SingleToken],
	token_template(SingleToken, _TokenType, TokenString, _LCTokenString, _PosInfo),
	TokenString = [_SingleChar],
	!,
	fail.
test_valid_aa_20(_AATokens).	

count_tokens_of_type([], _Type, Count ,Count).
count_tokens_of_type([FirstToken|RestTokens], Type, CountIn, CountOut) :-
	( arg(1, FirstToken, Type) ->
	  CountNext is CountIn + 1
	; CountNext is CountIn
	),
	count_tokens_of_type(RestTokens, Type, CountNext, CountOut).

all_an_tokens_are_lc([]).
all_an_tokens_are_lc([FirstToken|RestTokens]) :-
	( an_tok(FirstToken) ->
	  lc_tok(FirstToken)
	; true
	),
	all_an_tokens_are_lc(RestTokens).

count_comma_space_seq([], _CommaSeen, CommaSpaceCount, CommaSpaceCount).
count_comma_space_seq([ThisToken|RestTokens], CommaSeen, CommaSpaceCountIn, CommaSpaceCountOut) :-
	% If the previous token was a comma
	( CommaSeen =:= 1,
	% and the current token is a ws token
	  ws_tok(ThisToken) ->
	  CommaSpaceCountNext is CommaSpaceCountIn + 1,
	  count_comma_space_seq(RestTokens, 0, CommaSpaceCountNext, CommaSpaceCountOut)
	% If the current token iss a comma
	; punc_tok(ThisToken),
	  arg(3, ThisToken, [44]) ->
	% change the state to 1.
	  count_comma_space_seq(RestTokens, 1, CommaSpaceCountIn, CommaSpaceCountOut)
	; count_comma_space_seq(RestTokens, 0, CommaSpaceCountIn, CommaSpaceCountOut)
	).	
	
count_an_token_chars([], CharCount, CharCount).
count_an_token_chars([FirstToken|RestTokens], CharCountIn, CharCountOut) :-
	( an_tok(FirstToken) ->
	  arg(4, FirstToken, pos(StartPos,EndPos)),
	  StringLength is EndPos - StartPos,
	  CharCountNext is CharCountIn + StringLength
	; CharCountNext is CharCountIn
	),
	count_an_token_chars(RestTokens, CharCountNext, CharCountOut).

count_multi_letter_lc_tokens([], Count, Count).
count_multi_letter_lc_tokens([FirstToken|RestTokens], CountIn, CountOut) :-
	( lc_tok(FirstToken),
	  arg(2, FirstToken, [_,_|_]) ->
	  CountNext is CountIn + 1
	; CountNext is CountIn
	),
	count_multi_letter_lc_tokens(RestTokens, CountNext, CountOut).

% Case sensitive!
forbidden_first_scope_word('described').
forbidden_first_scope_word('due').
forbidden_first_scope_word('The').
forbidden_first_scope_word('There').
forbidden_first_scope_word('We').
forbidden_first_scope_word('METHODS').
forbidden_first_scope_word('MATERIAL').

forbidden_first_aa_word(also).
forbidden_first_aa_word(including).
forbidden_first_aa_word(such).
forbidden_first_aa_word(termed).

forbidden_aa_word(apropos).
forbidden_aa_word(daily).
forbidden_aa_word(edu).
forbidden_aa_word(https).
forbidden_aa_word(html).
forbidden_aa_word(preliminary).
forbidden_aa_word(report).
forbidden_aa_word(than).
forbidden_aa_word(www).

forbidden_scope_word(almost).
forbidden_scope_word(are).
forbidden_scope_word(could).
forbidden_scope_word(is).
forbidden_scope_word(namely).
forbidden_scope_word(respectively).
forbidden_scope_word(significantly).
forbidden_scope_word(that).
forbidden_scope_word(was).
forbidden_scope_word(we).
forbidden_scope_word(were).
forbidden_scope_word(which).
forbidden_scope_word(whereas).

forbidden_token_sequence(aa, Sequence) :-
	Sequence = [H|T],
	forbidden_aa_token_sequence(H, T).
forbidden_token_sequence(scope, Sequence) :-
	Sequence = [H|T],
	forbidden_scope_token_sequence(H, T).

forbidden_aa_token_sequence(' ',    [usa, ' ']).
forbidden_aa_token_sequence(ca,     ['.']).
forbidden_aa_token_sequence(higher, [' ', than]).
forbidden_aa_token_sequence(in,     [' ', all]).
forbidden_aa_token_sequence(or,     [' ', anti]).
forbidden_aa_token_sequence(study,  [' ', of]).

forbidden_scope_token_sequence('+',        ['/',  '-']).
forbidden_scope_token_sequence(':',        [' ', an]).
forbidden_scope_token_sequence(':',        [' ', a]).
forbidden_scope_token_sequence(':',        [' ', comparison, ' ',   of]).
forbidden_scope_token_sequence(':',        [' ', the]).
forbidden_scope_token_sequence(but,        [' ', the]).
forbidden_scope_token_sequence(equal,      [' ', to]).
forbidden_scope_token_sequence(in,         [' ', twelve]).
forbidden_scope_token_sequence(isolated,   [' ', from, ' ', the]).
forbidden_scope_token_sequence(of,         [' ', third]).
forbidden_scope_token_sequence(production, [' ', in,  ' ', a]).
forbidden_scope_token_sequence(to,         [' ', the, ' ', study]).
forbidden_scope_token_sequence(with,       [' ', only]).

forbidden_token_coocurrence(aa, Tokens) :-
	Tokens = [H|T],
	forbidden_aa_token_coocurrence(H, T).
forbidden_token_coocurrence(scope, Tokens) :-
	Tokens = [H|T],
	forbidden_scope_token_coocurrence(H, T).

forbidden_aa_token_coocurrence(from,  [the]).
forbidden_aa_token_coocurrence(from,  [to]).

forbidden_scope_token_coocurrence(a,          [an,  of,  the, with]).
forbidden_scope_token_coocurrence(a,          [an,  of,  for, with]).
forbidden_scope_token_coocurrence(after,      [in,  of,  the]).
forbidden_scope_token_coocurrence(an,         [and, in,  of,  the]).
forbidden_scope_token_coocurrence(and,        [and, the, to]).
forbidden_scope_token_coocurrence(and,        [seven]).
forbidden_scope_token_coocurrence(and,        [this]).
forbidden_scope_token_coocurrence(about,      [in,  of,  the]).
forbidden_scope_token_coocurrence(and,        [by,  the, with]).
forbidden_scope_token_coocurrence(and,        [by,  in,  of,  the]).
forbidden_scope_token_coocurrence(and,        [its, of,  with]).
forbidden_scope_token_coocurrence(and,        [one, to,  the]).
forbidden_scope_token_coocurrence(clinical,   [outcomes]).
forbidden_scope_token_coocurrence(even,       [when]).
forbidden_scope_token_coocurrence(for,        [from, of,  the]).
forbidden_scope_token_coocurrence(in,         [of,   the, to]).
forbidden_scope_token_coocurrence(in,         [the,  under]).
forbidden_scope_token_coocurrence(of,         [or,   over]).
forbidden_scope_token_coocurrence(properties, [of,   from]).
forbidden_scope_token_coocurrence(the,        [this]).
forbidden_scope_token_coocurrence(the,        [this]).
forbidden_scope_token_coocurrence(values,     [between]).

forbidden_post_comma_scope_word(we).
forbidden_post_comma_scope_word(the).
forbidden_post_comma_scope_word(they).

contains_forbidden_token_coocurrence(Type, TokenList) :-
	forbidden_token_coocurrence(Type, ForbiddenTokenCooccurrence),
	contains_all_specific_tokens(ForbiddenTokenCooccurrence, TokenList).

contains_all_specific_tokens([], _TokenList).
contains_all_specific_tokens([FirstToken|RestTokens], TokenList) :-
	contains_specific_token(TokenList, FirstToken, RestTokenList),
	contains_all_specific_tokens(RestTokens, RestTokenList).


contains_comma_and_forbidden_scope_word([T1,T2,T3|RestScopeTokens]) :-
	( punc_tok(T1),
	  arg(2, T1, [44]),
	  ws_tok(T2),
	  lc_tok(T3),
	  arg(3, T3, LCTokenString),
	  atom_codes(LCTokenAtom, LCTokenString),
	  forbidden_post_comma_scope_word(LCTokenAtom) ->
	  true
	; contains_comma_and_forbidden_scope_word([T2,T3|RestScopeTokens])
	).

test_for_sentence(Scope) :-
	Scope = [T1|Rest],
	Rest = [T2,T3,T4,T5|_],
	( test_token_seq(T1, T2, T3, T4, T5) ->
	  true
	; test_for_sentence(Rest)
	).

test_token_seq(T1, T2, T3, T4, T5) :-
	an_tok(T1),
	( ws_tok(T2) ->
	    punc_ws_ic_toks(T3, T4, T5)
	  ; punc_ws_ic_toks(T2, T3, T4)
	).

punc_ws_ic_toks(T2, T3, T4) :-
	  punc_tok(T2),
	  arg(2, T2, [46]), % period
	  ws_tok(T3),
	  ic_tok(T4).

count_exact_match_tokens([], _Atom, Count, Count).
count_exact_match_tokens([FirstToken|RestTokens], Atom, CountIn, CountOut) :-
	( token_matches_atom(FirstToken, Atom) ->
	  CountNext is CountIn + 1
	; CountNext is CountIn
	),
	count_exact_match_tokens(RestTokens, Atom, CountNext, CountOut).

token_matches_atom(Token, Atom) :-
	arg(3, Token, LCString),
	atom_codes(Atom, LCString).

count_prefix_match_tokens([], _Atom, Count, Count).
count_prefix_match_tokens([FirstToken|RestTokens], Atom, CountIn, CountOut) :-
	( atom_matches_token_prefix(FirstToken, Atom) ->
	  CountNext is CountIn + 1
	; CountNext is CountIn
	),
	count_prefix_match_tokens(RestTokens, Atom, CountNext, CountOut).

atom_matches_token_prefix(Token, Atom) :-
	atom_codes(Atom, String),
	arg(3, Token, TokenLCString),
	append(String, _, TokenLCString).

tokens_match_atoms([], []).
tokens_match_atoms([FirstToken|RestTokens], [FirstAtom|RestAtoms]) :-
	arg(3, FirstToken, FirstTokenLCString),
	atom_codes(FirstTokenLCAtom, FirstTokenLCString),
	FirstTokenLCAtom == FirstAtom,
	tokens_match_atoms(RestTokens, RestAtoms).

contains_forbidden_token_sequence(Type, TokenList) :-
	forbidden_token_sequence(Type, ForbiddenSequence),
	length(ForbiddenSequence, Length),
	sublist(TokenList, SubList, _, Length),
	contains_no_pe_token(SubList),
	tokens_match_atoms(SubList, ForbiddenSequence).

% Proposed scope must not contain the following sequence of tokens:
% (1) uc > 5 chars
% (2) pn ':'
% (3) ws
% (4) uc > 5 chars
contains_forbidden_token_sequence_1(Scope) :-
	Scope = [T1,T2,T3,T4|Rest],
	( uc_tok(T1),
	  arg(4, T1, pos(StartPos1,EndPos1)),
	  EndPos1 - StartPos1 > 5,
	  punc_tok(T2),
	  arg(2, T2, [58]), % ':'
	  ws_tok(T3),
	  uc_tok(T4),
	  arg(4, T4, pos(StartPos4,EndPos4)) ->
	  EndPos4 - StartPos4 > 5
	; contains_forbidden_token_sequence_1([T2,T3,T4|Rest])
	).

contains_no_pe_token([]).
contains_no_pe_token([FirstToken|RestTokens]) :-
	arg(1, FirstToken, Type),
	Type \== pe,
	contains_no_pe_token(RestTokens).

contains_DNA_sequence(Scope) :-
	member(Token, Scope),
	arg(1, Token, uc),
	arg(3, Token, LCString),
	arg(4, Token, pos(StartPos,EndPos)),
	EndPos - StartPos > 10,
	dna_sequence(LCString).

dna_sequence([]).
dna_sequence([H|T]) :-
	dna_letter(H),
	dna_sequence(T).

dna_letter(103). % g
dna_letter(97).  % a
dna_letter(116). % t
dna_letter(99).  % c
dna_letter(105). % i
dna_letter(117). % u

test_valid_scope(AATokens, ScopeTokens) :-
	test_valid_scope_01(AATokens, ScopeTokens),
	test_valid_scope_02(AATokens, ScopeTokens),
	test_valid_scope_03(AATokens, ScopeTokens),
	test_valid_scope_04(AATokens, ScopeTokens),
	test_valid_scope_05(AATokens, ScopeTokens),
	test_valid_scope_06(AATokens, ScopeTokens),
	test_valid_scope_07(AATokens, ScopeTokens),
	test_valid_scope_08(AATokens, ScopeTokens),
	test_valid_scope_09(AATokens, ScopeTokens),
	test_valid_scope_10(AATokens, ScopeTokens),
	test_valid_scope_11(AATokens, ScopeTokens),
	test_valid_scope_12(AATokens, ScopeTokens),
	test_valid_scope_13(AATokens, ScopeTokens),
	test_valid_scope_14(AATokens, ScopeTokens).



% Proposed scope must not contain the following sequence of tokens:
% (1) uc > 5 chars
% (2) pn ':'
% (3) ws
% (4) uc > 5 chars
test_valid_scope_01(AATokens, ScopeTokens) :-
	contains_forbidden_token_sequence_1(ScopeTokens),
	!,
	announce_scope_failure('SC-01', AATokens, ScopeTokens),
	fail.
test_valid_scope_01(_, _).

% Proposed scope must not begin with a five-digit number
test_valid_scope_02(AATokens, ScopeTokens) :-
	ScopeTokens = [FirstToken|_],	
	nu_tok(FirstToken),
	arg(2, FirstToken, NumberString),
	number_codes(Number, NumberString),
	Number > 9999,	
        !,
	announce_scope_failure('SC-02', AATokens, ScopeTokens),
        fail.
test_valid_scope_02(_, _).

% Proposed scope must not contain a forbidden scope token sequence
test_valid_scope_03(AATokens, ScopeTokens) :-
        contains_forbidden_token_sequence(scope, ScopeTokens),
        !,
	announce_scope_failure('SC-03', AATokens, ScopeTokens),
        fail.
test_valid_scope_03(_, _).

% Proposed scope must not contain a uc token that looks like a DNA sequence
test_valid_scope_04(AATokens, ScopeTokens) :-
        contains_DNA_sequence(ScopeTokens),
        !,
	announce_scope_failure('SC-04', AATokens, ScopeTokens),
        fail.
test_valid_scope_04(_, _).

% Proposed scope must not contain a pe token
test_valid_scope_05(AATokens, ScopeTokens) :-
        memberchk(tok(pe,_,_,_), ScopeTokens),
        !,
	announce_scope_failure('SC-05', AATokens, ScopeTokens),
        fail.
test_valid_scope_05(_, _).

% Proposed scope must not contain a comma followed by a forbidden word
test_valid_scope_06(AATokens, ScopeTokens) :-
	contains_comma_and_forbidden_scope_word(ScopeTokens),
	!,
	announce_scope_failure('SC-06', AATokens, ScopeTokens),
	fail.
test_valid_scope_06(_, _).

% Proposed scope must not contain a forbidden scope word
test_valid_scope_07(AATokens, ScopeTokens) :-
	contains_specific_token(ScopeTokens, ScopeTokenLCAtom, _RestScopeTokens),
	forbidden_scope_word(ScopeTokenLCAtom),
	!,
	announce_scope_failure('SC-07', AATokens, ScopeTokens),
	fail.
test_valid_scope_07(_, _).

% Proposed scope must not contain forbidden token coorcurrence
test_valid_scope_08(AATokens, ScopeTokens) :-
	contains_forbidden_token_coocurrence(scope, ScopeTokens),
	!,
	announce_scope_failure('SC-08', AATokens, ScopeTokens),
	fail.
test_valid_scope_08(_, _).

% Proposed scope must not contain a period token that is
% (1) immediately preceeded by an alphanumeric token of > 4 chars
%     followed by opotional whitespace, and
% (2) immediately followed by ws and a ic token.
% The above description suggests that we're trying to detect an acronym
% that spans a a sentence boundary.
test_valid_scope_09(AATokens, ScopeTokens) :-
	test_for_sentence(ScopeTokens),
	!,
	announce_scope_failure('SC-09', AATokens, ScopeTokens),
	fail.
test_valid_scope_09(_, _).

% Proposed scope must not contain three "the" tokens.
test_valid_scope_10(AATokens, ScopeTokens) :-
	count_exact_match_tokens(ScopeTokens, 'the', 0, Count),
	Count > 2,
	!,
	announce_scope_failure('SC-10', AATokens, ScopeTokens),
	fail.
test_valid_scope_10(_, _).

% Proposed scope must not contain two "in" and two "the" tokens.
test_valid_scope_11(AATokens, ScopeTokens) :-
	count_exact_match_tokens(ScopeTokens, 'the', 0, TheCount),
	TheCount >= 2,
	count_exact_match_tokens(ScopeTokens, 'in', 0, InCount),
	InCount >= 2,
	!,
	announce_scope_failure('SC-11', AATokens, ScopeTokens),
	fail.
test_valid_scope_11(_, _).

% Proposed scope must not begin with "There" or "The"
test_valid_scope_12(AATokens, ScopeTokens) :-
	ScopeTokens = [FirstScopeToken|_],
	% Case sensitive!
	arg(2, FirstScopeToken, FirstScopeString),
	atom_codes(FirstScopeAtom, FirstScopeString),
	forbidden_first_scope_word(FirstScopeAtom),
	!,
	announce_scope_failure('SC-12', AATokens, ScopeTokens),
	fail.
test_valid_scope_12(_, _).

% Proposed scope must not contain a comma token
% immediately followed by ws and then either "the" or "we"
test_valid_scope_13(AATokens, ScopeTokens) :-
	memberchk(tok(pe,_,_,_), ScopeTokens),
	!,
	announce_scope_failure('SC-13', AATokens, ScopeTokens),
	fail.
test_valid_scope_13(_, _).

% Proposed scope must not contain > 80 chars; that rules out less than 0.04% of AAs
test_valid_scope_14(AATokens, ScopeTokens) :-
	ScopeTokens = [S1|SRest],
	arg(4, S1, pos(ScopeStartPos,_)),
	token_sequence_length(SRest, S1, ScopeStartPos, ScopeTokensLength),
	ScopeTokensLength > 80,
	!,
	announce_scope_failure('SC-14', AATokens, ScopeTokens),
	fail.
test_valid_scope_14(_, _).

test_valid_aa_and_expansion(AATokens, ScopeTokens) :-
	test_valid_aa_and_expansion_01(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_02(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_03(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_04(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_05(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_06(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_07(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_08(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_09(AATokens, ScopeTokens),
	test_valid_aa_and_expansion_10(AATokens, ScopeTokens).

% Proposed AA contains more chars than proposed Scope
test_valid_aa_and_expansion_01(AATokens, ScopeTokens) :-
	trim_ws_tokens(ScopeTokens, TrimmedScopeTokens),
	AATokens = [AA1|AARest],
	TrimmedScopeTokens = [Scope1|ScopeRest],
	arg(4, AA1,    pos(AAStartPos,_)),
	arg(4, Scope1, pos(ScopeStartPos,_)),
	token_sequence_length(AARest,    AA1,    AAStartPos,    AATokensLength),
	token_sequence_length(ScopeRest, Scope1, ScopeStartPos, ScopeLength),
	AATokensLength > ScopeLength,
	!,
	announce_both_failure('2-00', AATokens, ScopeTokens),
	fail.
test_valid_aa_and_expansion_01(_, _).

% Proposed AA contains "and" but proposed Scope does not.
% Proposed AA contains "and" but proposed Scope does not.
test_valid_aa_and_expansion_02(AATokens, ScopeTokens) :-
	contains_specific_token(AATokens, 'and', _RestAATokens),
	\+ contains_specific_token(ScopeTokens, 'and', _RestScopeTokens),
	!,
	announce_both_failure('2-02', AATokens, ScopeTokens),
	fail.
test_valid_aa_and_expansion_02(_, _).

% Proposed AA contains "non" but proposed Scope does not.
test_valid_aa_and_expansion_03(AATokens, ScopeTokens) :-
	contains_specific_token(AATokens, 'non', _RestAATokens),
	\+ contains_specific_token(ScopeTokens, 'non', _RestScopeTokens),
	!,
	announce_both_failure('2-03', AATokens, ScopeTokens),
	fail.
test_valid_aa_and_expansion_03(_, _).

% Proposed AA has more tokens than proposed scope
% AND over a third of the AA tokens are multi-letter lc tokens
test_valid_aa_and_expansion_04(AATokens, ScopeTokens) :-
	length(AATokens, TokensLength),
	length(ScopeTokens, ScopeLength),
	TokensLength > ScopeLength,
	count_multi_letter_lc_tokens(AATokens, 0, MultiLetterLCTokenCount),
	MultiLetterLCTokenCount * 3 > ScopeLength,
	!,
	announce_both_failure('2-04', AATokens, ScopeTokens),
	fail.	
test_valid_aa_and_expansion_04(_, _).

% Proposed scope has more than 7 tokens
% AND proposed AA has more than 1 token
% AND over a third of the AA tokens are multi-letter lc tokens
test_valid_aa_and_expansion_05(AATokens, ScopeTokens) :-
	length(AATokens, TokensLength),
	length(ScopeTokens, ScopeLength),
	ScopeLength > 7,
	TokensLength > 1,
	count_multi_letter_lc_tokens(AATokens, 0, MultiLetterLCTokenCount),
	MultiLetterLCTokenCount * 3 > ScopeLength,
	!,
	announce_both_failure('2-05', AATokens, ScopeTokens),
	fail.	
test_valid_aa_and_expansion_05(_, _).

% Proposed scope has more twice as many an tokens
% as the proposed AA has an chars.
test_valid_aa_and_expansion_06(AATokens, ScopeTokens) :-
	count_an_token_chars(AATokens, 0, AATokensCharCount),
	count_an_tokens(ScopeTokens, 0, ScopeANTokens),
	ScopeANTokens > 2 * AATokensCharCount,
	!,
	announce_both_failure('2-06', AATokens, ScopeTokens),
	fail.
test_valid_aa_and_expansion_06(_, _).

% Proposed AA contains a non-uc alphanumeric token > 6 chars long
% that does not appear in the Scope
test_valid_aa_and_expansion_07(AATokens, ScopeTokens) :-
	length(AATokens, AATokensLength),
	AATokensLength > 2,	
	member(Token, AATokens),
	an_tok(Token),
	\+ uc_tok(Token),
	arg(4, Token, pos(StartPos,EndPos)),
	EndPos - StartPos > 6,
	% ScopeTokens doesn't contain a token with the same token type, text and lc text
	\+ contains_matching_token(Token, ScopeTokens),
	!,
	announce_both_failure('2-07', AATokens, ScopeTokens),
	fail.
test_valid_aa_and_expansion_07(_, _).

% Proposed Expansion begins with a canonical section header such as
% "RESULTS", "CONCLUSION", "CONCLUSIONS", "METHODS", "OBJECTIVE", etc.
test_valid_aa_and_expansion_08(AATokens, ScopeTokens) :-
	ScopeTokens = [FirstScopeToken|_Rest],
	uc_tok(FirstScopeToken),
	arg(2, FirstScopeToken, FirstScopeTokenString),
	atom_codes(FirstScopeTokenAtom, FirstScopeTokenString),
	section_header(FirstScopeTokenAtom),
	announce_both_failure('2-09', AATokens, ScopeTokens),	
	!,
	fail.
test_valid_aa_and_expansion_08(_, _).

% Proposed Expansion contains a "(" or ")" token.
test_valid_aa_and_expansion_09(AATokens, ScopeTokens) :-
	member(Token, ScopeTokens),
	( ex_rbracket_tok(Token)
	; ex_lbracket_tok(Token)
	),
	announce_both_failure('2-09', AATokens, ScopeTokens),	
	!,
	fail.
test_valid_aa_and_expansion_09(_, _).


% Proposed AA cannot contain > 2 punc tokens
test_valid_aa_and_expansion_10(_AATokens, ScopeTokens) :-
	count_tokens_of_type(ScopeTokens, pn, 0, Count),
	Count > 3,
	!,
	fail.
test_valid_aa_and_expansion_10(_AATokens, _ScopeTokens).

announce_aa_failure(Rule, Tokens) :- announce_failure(Rule, Tokens).

announce_scope_failure(Rule, AATokens, ScopeTokens) :-
	announce_both_failure(Rule, AATokens, ScopeTokens).

announce_failure(_Rule, _Tokens).

announce_both_failure(_Rule, _AATokens, _ScopeTokens).

% announce_failure(Rule, Tokens) :-
% 	assemble_tokens_atom(Tokens, TokensAtom),
% 	format(user_output, 'Rule ~w FAILED on "~w".~n', [Rule,TokensAtom]).

% announce_both_failure(Rule, AATokens, ScopeTokens) :-
% 	assemble_tokens_atom(AATokens, AATokensAtom),
% 	assemble_tokens_atom(ScopeTokens, ScopeTokensAtom),
% 	format(user_output, 'Rule ~w FAILED on "~w"/"~w".~n', [Rule,ScopeTokensAtom,AATokensAtom]).

assemble_tokens_atom(Tokens, Atom) :-
	extract_token_atoms(Tokens, TokenAtoms),
	concat_atom(TokenAtoms, Atom).

extract_token_atoms([], []).
extract_token_atoms([tok(_TokType,Atom,_LCAtom,_PosInfo)|RestTokens], [Atom|RestAtoms]) :-
	extract_token_atoms(RestTokens, RestAtoms).

% Keep only the LC atoms from an tokens
extract_an_lc_atoms([], []).
extract_an_lc_atoms([tok(TokType,_Atom,LCAtom,_PosInfo)|RestTokens], ExtractedAtoms) :-
	( an_type(TokType) ->
	  ExtractedAtoms = [LCAtom|RestExtractedAtoms]
	; ExtractedAtoms = RestExtractedAtoms
	),
	extract_an_lc_atoms(RestTokens, RestExtractedAtoms).


contains_specific_token(TokenList, LCTokenAtom, RestTokenList) :-
	select(Token, TokenList, RestTokenList),
	an_tok(Token),
	arg(3, Token, LCTokenString),
	atom_codes(LCTokenAtom, LCTokenString).

contains_matching_token(AAToken, Scope) :-
	arg(1, AAToken, AATokenType),
	arg(3, AAToken, AATokenLCText),
	member(ScopeToken, Scope),
	arg(1, ScopeToken, ScopeTokenType),
	arg(3, ScopeToken, ScopeTokenLCText),
	AATokenType == ScopeTokenType,
	AATokenLCText == ScopeTokenLCText.

%%% Citation 10388480 contains the following:
%%% lipoprotein(a) [Lp(a)]
%%% so "Lp(a)" is an AA for "lipoprotein(a)".
%%% The citation contains also
%%% Lp(a)-cholesterol [Lp(a)-C] 
%%% so "Lp(a)-C" is an AA for "lipoprotein(a)-cholesterol".
%%% Fine and dandy.
%%% 
%%% However, later in the citation, we find
%%% 
%%% [ApotekTM Lp(a); r = 0.832; ...]
%%% 
%%% and we need to block "a" as an AA for "ApotekTM Lp"
%%% because this would deconstruct the existing acronym "Lp(a)".
%%% (It turns out that this specific case is blocked anyway
%%% by the requirement of having a space before the open paren.)
%%% 
%%% When deconstructing_known_AA/4 is called,
%%%
%%% Tokens = [tok(mc,"ApotekTM","apotektm",pos(1605,1613)),
%%%           tok(ws," "," ",pos(1613,1614)),
%%%	      tok(ic,"Lp","lp",pos(1614,1616))]
%%%
%%% PE_Token = tok(pe,[],2,pos(1616,1619))
%%% 
%%% ScopeWithParens = [tok(pn,"(","(",pos(1616,1617)),
%%%                    tok(lc,"a","a",pos(1617,1618)),
%%%		       tok(pn,")",")",pos(1618,1619))]
%%% 
%%% and AAsIn contains this node:
%%% 
%%% node([tok(ic,"Lp","lp",pos(129,131)),
%%% 	  tok(pe,[],2,pos(131,134)),
%%% 	  tok(pn,"(","(",pos(131,132)),
%%% 	  tok(lc,"a","a",pos(132,133)),
%%% 	  tok(pn,")",")",pos(133,134))],
%%%     [[tok(lc,"lipoprotein","lipoprotein",pos(113,124)),
%%% 	  tok(pn,"(","(",pos(124,125)),
%%% 	  tok(lc,"a","a",pos(125,126)),
%%% 	  tok(pn,")",")",pos(126,127)),
%%% 	  tok(pn,")",")",pos(127,128))]],
%%% 	 0,
%%%  	 empty,
%%% 	 empty),
%%% 
%%% We then append Tokens and [PE_Tokens|ScopeWithParens] to get
%%% 
%%% [tok(mc,"ApotekTM","apotektm",pos(1605,1613)),
%%%  tok(ws," "," ",pos(1613,1614)),
%%%  tok(ic,"Lp","lp",pos(1614,1616)),
%%%  tok(pe,[],2,pos(1616,1619)),
%%%  tok(pn,"(","(",pos(1616,1617)),
%%%  tok(lc,"a","a",pos(1617,1618)),
%%%  tok(pn,")",")",pos(1618,1619))]
%%% 
%%% and find a suffix (the last 5 elements) of the resulting list is a known AA.
%%% That means that the proposed AA ("a") and the end of its proposed expansion ("Lp")
%%% is already a known AA, and we want to block that.
%%% 
%%% What a complex case.
 
deconstructing_known_AA(Tokens, PE_Token, ScopeWithParens, AAsIn) :-
	avl_member(KnownAcronym, AAsIn),
	append(Tokens, [PE_Token|ScopeWithParens], TokenList),
	suffix(TokenList, Suffix),
	matching_strings(KnownAcronym, Suffix).

%%% The above is intended to block the deconstruction of existing acronyms.
%%% There is a complementary case, in which we want to block the recognition
%%% as an AA of a string that consists of part of the expansion plus the AA
%%% of an existing acronym. Consider for example (from citation 10495948)
%%% "Drosophila melanogaster Su(mg)", in which "mg" is expanded to "melanogaster Su".
%%% (This AA is actually blocked because of the absence of a space before the open paren,
%%% but the example remains instructive.) Thus "Su(mg)" consists of
%%% (1) part of the expansion ("Su")
%%% followed by
%%% (2) the AA itself "(mg)".
%%% Fine and dandy.
%%% We then enounter "The Suppressor of modifier mdg4 (Su(mg))" and we want to block
%%% the recognition of "Su(mg)" as an AA for "Suppressor of modifier mdg4". Why?
%%% Su(mg) can't itself be an AA, because it's already part of the expansion + an AA.
%%% 
%%% At this point, ScopeWithParens is a list like
%%% 
%%% [tok(pn,"(","(",pos(245,246)),
%%%  tok(ic,"Su","su",pos(246,248)),
%%%  tok(pe,[],2,pos(248,252)),
%%%  tok(pn,"(","(",pos(248,249)),
%%%  tok(lc,"mg","mg",pos(249,251)),
%%%  tok(pn,")",")",pos(251,252)),
%%%  tok(pn,")",")",pos(252,253))]
%%% 
%%% and AAsIn contains this node:
%%% 
%%% node([tok(lc,"mg","mg",pos(76,78))],                      <--- Key
%%%     [[tok(lc,"melanogaster","melanogaster",pos(60,72)),   <--- Value
%%% 	  tok(ws," "," ",pos(72,73)),
%%% 	  tok(ic,"Su","su",pos(73,75))]],
%%% 	0,
%%% 	empty,
%%% 	empty)
%%% 
%%% We then create the following list:
%%% 
%%%     [tok(lc,"melanogaster","melanogaster",pos(60,72)),
%%% 	 tok(ws," "," ",pos(72,73)),
%%% 	 tok(ic,"Su","su",pos(73,75)),
%%%      tok(pe,_,_,_),
%%%      tok(pn,_,_,_),
%%%      tok(lc,"mg","mg",pos(76,78)),
%%%      tok(pn,_,_,_)]
%%% 
%%% and verify that a suffix of that list of length at least 5
%%% (in this case, the suffix beginning with the "Su" token)
%%% matches a sublist of the same length of ScopeWithParens.
%%% 
%%% Another really complex case.

proposed_AA_overlaps_prev_scope(ScopeWithParens, AAsIn) :-
	avl_member(Key, AAsIn, [Value]),
	append([Value,
		[tok(pe,_,_,_),
		 tok(pn,_,_,_)],
		Key,
		[tok(pn,_,_,_)]],
	       TokenList),
	suffix(TokenList, TokenListSuffix),
	length(TokenListSuffix, TokenListSuffixLength),
	TokenListSuffixLength >= 5,
	sublist(ScopeWithParens, ScopeSubList, _, TokenListSuffixLength),
	matching_strings(TokenListSuffix, ScopeSubList),
	!.

remove_trailing_whitespace_tokens(AATokens0, AATokens) :-
	rev(AATokens0, RevAATokens0),
	remove_leading_whitespace_tokens(RevAATokens0, NoWSRevAATokens0),
	rev(NoWSRevAATokens0, AATokens).

remove_leading_whitespace_tokens([H|T], TokensOut) :-
	( ws_tok(H) ->
	  remove_leading_whitespace_tokens(T, TokensOut)
	; TokensOut = [H|T]
	).

% Given, e.g., "heart attack (HA)":
% * AATokens is the list of tokens in the proposed AA, e.g., ['HA'].
%   By 'HA', I denote a term of the form tok(uc,"HA","ha",pos(14,16)).
% * AATokensWithParens is the list of tokens in the AA, along with the
%   preceeding opening bracket token and the following closing bracket token,
%   e.g., ['(', 'HA', ')'].
% * RevPre is the reverse of the list of tokens preceeding the proposed AA,
%   which is the proposed AA expansion, e.g., [' ', attack, ' ', heart].
find_aa(AATokensIn, AATokensWithParens, PE_Token, LastPos, RevPre, AAsIn, AAsOut) :-
	% If the text contains an abomination like
	% "intravesical prostatic protrusion (IPP )",
	% the AATokens0 coming into to find_aa/7 will end with a ws token,
	% which must be removed.
	remove_trailing_whitespace_tokens(AATokensIn, AATokens),
	% We require that the parenthesized AA be preceeded by white space.
	% This is to block generation of acronyms in cases like
	% Drosophila melanogaster Su(mg)
	% where we do NOT want "mg" to be expanded to "melanogaster Su"
 	RevPre = [FirstRevPre|_],
 	ws_tok(FirstRevPre),
	contains_alpha_tok(AATokens),
 	test_valid_aa(AATokens, LastPos),
	token_sequence_length(AATokens, 0, AATokensLength),
	% RevPre is the reverse of the list of the tokens
	% preceeding the proposed AA.
	% Go back into RevPre until we find a token beginning with
	% the same first letter as the first word in the proposed AA.

	find_initial_scope(AATokens, AATokensLength, RevPre,
			   TempRevScope0, AlphaNumericChar, TempRestTokens),
	% This next call prevents an AA expansion from beginning or ending with a stop word;
	% this blocks potential AAs like
	% "and acquired immune deficiency syndrome (AIDS)"
	% "combinations of [Ca]"
	block_stop_words(TempRevScope0),
	% We may have gobbled up some ws and punc tokens
	% that need to go back onto RestTokens;
	% also check to make sure scope doesn't begin with a lex_stop_word.
	% This last check has been disabled. No idea why it was there in the first place.
	rev(TempRevScope0, TempScope0),
	ensure_first_letter_match(TempScope0, AlphaNumericChar),
	LastUnwantedToken0 = '',
 	push_back_unwanted_tokens(TempRevScope0, TempRestTokens,
				  LastUnwantedToken0, TempRevScope1,
				  RestTokens, LastUnwantedToken),
	rev(TempRevScope1, Scope),
	% an AA expansion can't be immediately preceeded by a punc tok;
	% E.g., in "the effect of 20K-hGH on human PRL receptor (hPRLR).",
	% the expansion shouldn't begin with "hGH", but with "human".
	% ( RestTokens = [FirstRestToken|_] ->
	%   \+ punc_tok(FirstRestToken)
	% ; true
	% ),
	Scope \== [],

	\+ deconstructing_known_AA(Scope, PE_Token, AATokensWithParens, AAsIn),
	\+ proposed_AA_overlaps_prev_scope(AATokensWithParens, AAsIn),
	\+ shared_token(AAsIn, AATokensWithParens, Scope, _SharedToken),
	AAsNext = AAsIn,
	test_valid_scope(AATokens, Scope),
	test_valid_aa_and_expansion(AATokens, Scope),
	% test on 16249374 16463099
	% This test is necessary for downstream processing.
	% no_overlapping_aas(AAsIn, Scope, RestTokens),
	no_overlapping_aas(AAsNext, Scope, RestTokens),
	% announce_tokens_and_scope(AATokens, Scope),
	initialize_aa_matching(AATokens, Scope, AATokens0, AAScope0),
	perform_aa_matching(AATokens0, AAScope0, AATokens1, AAScope1, AAMatch1),
	% ( nl(user_output), member(M, AAMatch1), format(user_output, '~w~n', [M]), fail ; nl(user_output) ),
	% AATokens0 and AAScope0 are the word tokens for the AA and Scope, e.g.,
	% aatok(word,lc,"and","and",pos(177,180),5,0,none)

	% AATokens1 and AAScope1 are the remaining, unmatched exploded tokens
	% in the AA and Scope, e.g.,
	% aatok(char,ic,108,108,161,1,4,
	%	aatok(word,ic,"Asclepias","asclepias",pos(158,167),1,0,none)),

	% AAMatch1 are the matching characters, e.g.,
	% match(init,
	%	aatok(char,ic,65,97,193,1,1,
	%	      aatok(word,ic,"Asclepiadaceae","asclepiadaceae",pos(193,207),1,0,none)),
	%	aatok(char,ic,65,97,158,1,1,
	%	      aatok(word,ic,"Asclepias","asclepias",pos(158,167),1,0,none))),

	evaluate_aa_match(AATokens1, AAScope1, AAMatch1, AATokens0, AAScope0,
			  _NT, _NT0, _NS, _NS0, _T, _S, _V, YN, _OutputStream),
	YN == yes,
	% format(user_output, 'AAT: ', []),
	% skr_utilities:write_token_list(AATokens, 0, 1),
	% format(user_output, 'SCO: ', []),
	% skr_utilities:write_token_list(Scope, 0, 1),	
%	store_aa(AATokens, LastUnwantedToken, Scope, AAsIn, AAsOut),
	store_aa(AATokens, LastUnwantedToken, Scope, AAsNext, AAsOut),
	!.

%%    repeat find_initial_scope ..., if necessary
%%    don't forget reverse aas (i.e., when the defn, not the aa, is
%%    parenthesized and the aa precedes it but not necessarily immediately)
find_aa(_AATokens, _AATokensWithParens, _PE_Token,  _LastPos, _RevPre, AAsIn, AAsIn).

% This test is a stopgap designed to handle pathological cases such as
% "immunoliposomal doxorubicin (DXR) (ILD)" (PMID 14562030)
% We have already identified the AA "doxorubicin (DXR)",
% and we want to prefent "doxorubicin" from being re-used
% in the proposed AA "immunoliposomal doxorubicin (ILD)",
% even though it should be allowed. This is simply because
% the downstream code in create_EXP_raw_token_list can't
% properly handle such cases.
% That is the implementation based on \+ shared_token(AAsIn, Scope),
% which will block the longer AA ("immunoliposomal doxorubicin (ILD)").
% Instead, calling check_shared_token(AAsIn, Scope, AAsNext)
% will delete the already-created shorter AA ("doxorubicin (DXR)"),
% and allow the longer one to be created.

% check_shared_token(AAsIn, Scope, AAsNext) :-
%         ( avl_member(ShortForm, AAsIn, [ExpansionTokenList]),
%           member(SharedToken, Scope), 
%           memberchk(SharedToken, ExpansionTokenList) ->
%           avl:avl_delete(ShortForm, AAsIn, [ExpansionTokenList], AAsNext)
%         ; AAsNext = AAsIn
%         ).

shared_token(AAsIn, AATokensWithParens, Scope, _SharedToken) :-
	( shared_token_1(AAsIn, Scope, _SharedToken) ->
	  true
	; shared_token_1(AAsIn, AATokensWithParens, _SharedToken)
	).

shared_token_1(AAsIn, Scope, ScopeToken) :-	
	avl_member(ShortForm, AAsIn, [ExpansionTokenList]),
	member(ScopeToken, Scope),
 	( match_token_no_pos_info(ScopeToken, ExpansionTokenList) ->
	  true
	; match_token_no_pos_info(ScopeToken, ShortForm)
	).

match_token_no_pos_info(ScopeToken, AATokenList) :-
	token_template(ScopeToken, ScopeTokenType, ScopeTokenString,
		       _ScopeLCTokenString, _ScopePosInfo),
	member(AAToken, AATokenList),
	token_template(AAToken, AATokenType, AATokenString,
		       _AALCTokenString, _AAPosInfo),
	ScopeTokenType == 'uc',
	ScopeTokenType == AATokenType,
	ScopeTokenString == AATokenString.	

block_stop_words(TempRevScope0) :-
	trim_ws_tokens(TempRevScope0, TempRevScope),	
	TempRevScope = [First|_],
	\+ stop_word_token(First),
	last(TempRevScope, Last),
	\+ stop_word_token(Last).

stop_word_token(Token) :-
	token_template(Token, TokenType, TokenString, _LCTokenString, _PosInfo),
	an_type(TokenType),
	atom_codes(TokenAtom, TokenString),
	lex_stop_word(TokenAtom).


% push_back_unwanted_tokens(Scope0, RestTokens0, Scope, RestTokens),
push_back_unwanted_tokens([FirstToken|RestTokens], RestTokensIn,
			  LastUwantedTokenIn, RevScope,
			  RestTokensOut, LastUnwantedTokenOut) :-
	( skip_tok(FirstToken, Result),
	  Result =\= 4,
	  Result =\= 2 ->
	  RevScope = RestRevScope,
	  RestTokensNext = [FirstToken|RestTokensIn],
	  LastUnwantedTokenNext = FirstToken,
	  push_back_unwanted_tokens(RestTokens, RestTokensNext,
				    LastUnwantedTokenNext, RestRevScope,
				    RestTokensOut, LastUnwantedTokenOut)
	; RevScope = [FirstToken|RestTokens],
	  LastUnwantedTokenOut = LastUwantedTokenIn,
	  RestTokensOut = RestTokensIn
	).

ensure_first_letter_match(Scope0, AlphaNumericChar) :-
	Scope0 = [FirstToken|_RestTokens],
	FirstToken = tok(TokenType, _String, LCString, _PosInfo),
	token_matches_char(TokenType, LCString, AlphaNumericChar).

%	an_type(TokenType),
%	LCString = [AlphaNumericChar|_].

% push_back_unwanted_tokens([], RestTokens, [], RestTokens).
% push_back_unwanted_tokens([FirstScopeIn|RestScopeIn], RestTokensIn, ScopeOut, RestTokensOut) :-
% 	( skip_tok(FirstScopeIn) ->
% 	  RestTokensNext = [FirstScopeIn|RestTokensIn],
% 	  push_back_unwanted_tokens(RestScopeIn, RestTokensNext, ScopeOut, RestTokensOut)
% % 	; FirstScopeIn = tok(_TokenType, _String, LCString, _PosInfo),
% % 	  lex_stop_word(LCString) ->
% %	  push_back_unwanted_tokens(RestScopeIn, RestTokensIn, ScopeOut, RestTokensOut)
% 	; RestTokensOut = RestTokensIn,
% 	  ScopeOut = [FirstScopeIn|RestScopeIn]
% 	).	  

announce_tokens_and_scope(Tokens, Scope) :-
	format(user_output, 'Tokens are ~n', []),
	write_token_list(Tokens, 0, 1),
	ttyflush,
	format(user_output, 'Scope is ~n', []),
	write_token_list(Scope, 0, 1),
	ttyflush.

/*
We want to block overlapping acronyms. An example is from citation 16249374:

First, we encounter the legitimate acronym ER/SR here:

endoplasmic/sarcoplasmic reticulum (ER/SR)

Then we encounter this monster:

ER/SR Ca(2+)-ATPase (SERCA)

and we want to block the expansion of "SERCA" to "SR Ca(2+)-ATPase"
because it would create a mess. When we are at "SR", the AVL tree
containing the known acronyms looks like this:

node([tok(uc,"ER","er",pos(36,38)),
      tok(pn,"/","/",pos(38,39)),
      tok(uc,"SR","sr",pos(39,41))],
     [[tok(lc,"endoplasmic","endoplasmic",pos(0,11)),
       tok(pn,"/","/",pos(11,12)),
       tok(lc,"sarcoplasmic","sarcoplasmic",pos(12,24)),
       tok(ws," "," ",pos(24,25)),
       tok(lc,"reticulum","reticulum",pos(25,34))]],
     0,
     empty,
     empty)

Moreover, the token lists we are working with are

Scope = [
tok(uc,"SR","sr",pos(61,63))]
tok(ws," "," ",pos(63,64)),
tok(ic,"Ca","ca",pos(64,66)),
tok(pn,"(","(",pos(66,67)),
tok(nu,"2","2",pos(67,68)),
tok(pn,"+","+",pos(68,69)),
tok(pn,")",")",pos(69,70)),
tok(pn,"-","-",pos(70,71)),
tok(mc,"ATPase","atpase",pos(71,77)),
tok(ws," "," ",pos(77,78)),

RestTokens = [
tok(pn,"/","/",pos(60,61)),
tok(uc,"ER","er",pos(58,60)),
tok(ws," "," ",pos(57,58)),
tok(lc,"and","and",pos(54,57)),
tok(ws," "," ",pos(53,54)),
tok(pn,".",".",pos(52,53)),
tok(lc,"treatment","treatment",pos(43,52)),
tok(ws," "," ",pos(42,43)),
tok(pe,[],1,pos(35,42)),
ztok(ws," "," ",pos(34,35)),
tok(lc,"reticulum","reticulum",pos(25,34)),
tok(ws," "," ",pos(24,25)),
tok(lc,"sarcoplasmic","sarcoplasmic",pos(12,24)),
tok(pn,"/","/",pos(11,12)),
tok(lc,"endoplasmic","endoplasmic",pos(0,11))]

We then use avl_member/2 to examine each known acronym,
and assemble a list whose head is the first token in Scope ("SR"),
and whose tail is the first few tokens of RestTokens ("/", "ER").
Next, we reverse this list to get ["ER", "/", "SR"]
and check to see if this string matches any known acronym.

If it does, that means we're in the middle of a known acronym,
so we should prevent the current expansion. What a mess!

*/


no_overlapping_aas(AAsIn, Scope, RevRestScopeTokens) :-
	Scope = [FirstToken|_],
	avl_member(KnownAcronym, AAsIn),
	token_sequence_length(KnownAcronym, 0, AcronymLength),
	arg(2, FirstToken, FirstTokenString),
	length(FirstTokenString, FirstTokenStringLength),
	RestAcronymLength is AcronymLength - FirstTokenStringLength,
	get_remaining_tokens(RestAcronymLength, RevRestScopeTokens, RestRevCurrAcronym),
	rev([FirstToken|RestRevCurrAcronym], CurrAcronym),
	matching_strings(KnownAcronym, CurrAcronym),
	% This is too strict a test.
	% reversed order of args from QP library version!
	% last(KnownAcronym, LastAcronymComponent),
	% arg(2, FirstToken, FirstTokenString),
	% arg(2, LastAcronymComponent, LastAcronymComponentString),
	% FirstTokenString == LastAcronymComponentString,
	!,
	fail.
no_overlapping_aas(_AAsIn, _Scope, _RestTokens).

token_sequence_length([], Length, Length).
token_sequence_length([H|T], LengthIn, LengthOut) :-
	token_template(H, _TokenType, String, _LCString, _PosInfo),
	length(String, StringLength),
	LengthNext is LengthIn + StringLength,
	token_sequence_length(T, LengthNext, LengthOut).


get_remaining_tokens(LengthIn, [ScopeToken|RestRevScopeTokens], RemainingTokens) :-
	( LengthIn =:= 0 ->
	  RemainingTokens = []
	; arg(2, ScopeToken, TokenString),
	  length(TokenString, TokenStringLength),
	  TokenStringLength =< LengthIn,
	  RemainingTokens = [ScopeToken|RestRemainingTokens],
	  LengthNext is LengthIn - TokenStringLength,
	  get_remaining_tokens(LengthNext, RestRevScopeTokens, RestRemainingTokens)
	).

matching_string_prefixes([], LeftOverTokens, LeftOverTokens).
matching_string_prefixes([H1|T1], [H2|T2], LeftOverTokens) :-
	arg(2, H1, H1String),
	arg(2, H2, H2String),
	H1String = H2String,
	matching_string_prefixes(T1, T2, LeftOverTokens).

matching_strings(TokenList1, TokenList2) :-
	matching_string_prefixes(TokenList1, TokenList2, []).

find_initial_scope(Tokens, AATokensLength, RevPre, RevScope0, AlphaNumericChar, RestTokens) :-
	find_first_an(Tokens, AlphaNumericChar),
	% Initialize PreviousMatchingText to ''
	State is 0,
	ConsumedTokenCount is 0,
	match_initial_to_char(State, ConsumedTokenCount, AATokensLength, RevPre,
			      AlphaNumericChar, '', RevScope0, RestTokens).

find_first_an([tok(Type,_,[AN|_],_)|_], AN) :-
	an_type(Type),
	!.
find_first_an([_|Rest], AN) :-
	find_first_an(Rest, AN).

% match_initial_to_char(TokenList, Char, MatchTokens, RestTokens)
% TokenList is the list of tokens in the AA.
% We want to find a sequence of tokens in RevScope0 such that
% the first token in the sequence begins with the same letter
% aas the first token in TokenList.
% match_initial_to_char accumulates tokens in the TokenList
% until it finds a token whose first character is Char.
% Then it continues accumulating tokens as long as the tokens'
% first character is Char.
% All accumulated tokens are returned in MatchTokens;
% all remaining tokens from TokenList are returned in RestTokens.
% This is a refinement of the original algorithm, which stopped at
% the first matching token.

% State encodes whether a token matching the first letter
% of the first token in the AA TokenList has been found.

% State == 0 means that no matching token has yet been found.

match_initial_to_char(State, ConsumedTokenCount, AATokensLength, RevPre, InitialChar,
		      PreviousMatchingText, RevScope1, RestTokens) :-
	( ConsumedTokenCount > AATokensLength + 5 ->
	  RevScope1 = [],
	  RestTokens = RevPre
	; RevPre == [] ->
	  RevScope1 = [],
	  RestTokens = []
	; match_initial_to_char_1(State, ConsumedTokenCount, AATokensLength, RevPre, InitialChar,
				  PreviousMatchingText, RevScope1, RestTokens)
	).
	
match_initial_to_char_1(0, ConsumedTokenCount, AALength, [Token|RestTokens], InitialChar, '',
			MatchingTokens, LeftOverTokens) :-
	Token = tok(TokenType,_,LCText,_),
	  % Skip whitespace tokens
	( ws_tok(Token) ->
	  NextState is 0,
	  NextConsumedTokenCount is ConsumedTokenCount,
	  PreviousMatchingText = '',
	  MatchingTokens = [Token|RestMatchingTokens]
	  % If the first char of current token (LCText) is InitialChar,
	  % then we've found a matching token, so change the state to 1,
	  % and continue looking for more matching tokens.
	; token_matches_char(TokenType, LCText, InitialChar) ->
%	  ( ConsumedTokenCount =:= 0 ->
%	    NextState is 0
%	  ; NextState is 1
%	  ),
	  NextState is 1,
	  NextConsumedTokenCount is ConsumedTokenCount + 1,
	  PreviousMatchingText = '',
	  MatchingTokens = [Token|RestMatchingTokens]
	% Token does not match, so keep the state at 0, and continue
	; NextState is 0,
	  NextConsumedTokenCount is ConsumedTokenCount + 1,
	  PreviousMatchingText = '',
	  MatchingTokens = [Token|RestMatchingTokens]
	),
	match_initial_to_char(NextState, NextConsumedTokenCount, AALength, RestTokens, InitialChar,
			      PreviousMatchingText, RestMatchingTokens, LeftOverTokens).

% State == 1 means that a token matching InitialChar has already been found.
% Keep on going, looking for more matching tokens, skipping over skip tokens,
% as long as we don't consume more tokens than the length of the AA.
match_initial_to_char_1(1, ConsumedTokenCount, AALength, Tokens, InitialChar,
			PreviousMatchingText, MatchingTokens, LeftOverTokens) :-
	% Once a match has been found, it's OK to quit, as long as the previous matching token
	% was not a stop word; we don't want to allow, e.g., "AMI" to expand to
	% "and acute myocardial infarction".
	( Tokens = [Token|RestTokens],
	  skip_tok(Token, _Result),
	  % Result \== 4,
	  % If the token is skippable, don't count it against the length of the AA.
	  % maybe_increase_token_count(Result, ConsumedTokenCount, NextConsumedTokenCount),
	  NextConsumedTokenCount is ConsumedTokenCount,
	  MatchingTokens = [Token|RestMatchingTokens],
	  match_initial_to_char(1, NextConsumedTokenCount, AALength, RestTokens, InitialChar, " ",
				RestMatchingTokens, LeftOverTokens)
	  % Allow skipping over non-matching tokens to allow e.g., "Department of Defense (DoD)".
%	; ConsumedTokenCount < AALength,
%	  Tokens = [Token|RestTokens],
%	  Token = tok(_,_,LCText,_),
%	  an_tok(Token),
%	  NextConsumedTokenCount is ConsumedTokenCount + 1,
%	  MatchingTokens = [Token|RestMatchingTokens],
%	  match_initial_to_char(1, NextConsumedTokenCount, AALength, RestTokens, InitialChar,
%				LCText, RestMatchingTokens, LeftOverTokens)
	 % We've found a matching token, so stop.
	 % This must NOT be an if-then-else, because we must be able to backtrack
	 % over non-matching tokens so the algorithm doesn't give up prematurely,
	 % e.g., on "Department of Defense (DoD)".
	; Tokens = [Token|RestTokens],
	  Token = tok(_,_,LCText,_),
	  an_tok(Token),
%	  LCText = [InitialChar|_],
	  % format(user_output, 'PMT: ~w~n', [LCText]),
	  \+ lex_stop_word(LCText),
	  MatchingTokens = [Token|RestMatchingTokens],
	  MatchingTokens = [Token],
%	  RestMatchingTokens = [],
	  % LeftOverTokens = RestTokens,
	  NextConsumedTokenCount is ConsumedTokenCount + 1,
% 	  NextConsumedTokenCount =< AALength,
	  match_initial_to_char(1, NextConsumedTokenCount, AALength, RestTokens, InitialChar,
				LCText, RestMatchingTokens, LeftOverTokens)
	; \+ lex_stop_word(PreviousMatchingText),
	  MatchingTokens = [],
	  LeftOverTokens = Tokens
	).

%	; % Token does not match, so quit.
%	  % make sure that the last matching word is content-bearing
%	  Tokens = [Token|RestTokens],
%	  \+ lex_stop_word(PreviousMatchingText),
%	  MatchingTokens = [],
%	  LeftOverTokens = [Token|RestTokens]
%	).

% Increase the token count if the non-matching token is a stop word
% maybe_increase_token_count(Result, ConsumedTokenCount, NextConsumedTokenCount) :-
% 	( Result =:= 4 ->
% 	  NextConsumedTokenCount is ConsumedTokenCount
% 	; NextConsumedTokenCount is ConsumedTokenCount
% 	).

token_matches_char(TokenType, LCText, Char) :-
       ( TokenType \== nu ->
	 LCText = [Char|_]
       % TokenType == nu ->
       ; number_codes(Integer, LCText),
	 number_word(Integer, NumberString),
	 NumberString = [Char|_]
       ).

skip_tok(Token, Result) :-
	( ws_tok(Token) ->
	  Result is 1
	; hyphen_or_slash_tok(Token) ->
	  Result is 2
	; pe_tok(Token) ->
	  Result is 3
	; token_template(Token, _TokenType, _TokenString, LCTokenString, _PosInfo),
	  atom_codes(LCTokenAtom, LCTokenString),
	  lex_stop_word(LCTokenAtom) ->
	  Result is 4
	).

/* initialize_aa_matching(+Tokens, +Scope, -AATokens, -AAScope)

% From the lists of tokens in
% (1) the proposed AA (Tokens), and
% (2) the proposed scope (Scope),
% produce a modified list of aatok(_) terms, skipping the higher-order types.
% For example, the ic token

% tok(ic,"Symbols","symbols",pos(18,25)),

% leads to the creation of the aatok term

% aatok(word,ic,"Symbols","symbols",pos(18,25),1,0,none),

% 1 is the index of the aatok(_) term in the list of aatok(_) terms;
% 0 and 'none' are hardcoded. 

initialize_aa_matching/4 xxx  */

initialize_aa_matching(Tokens, Scope, AATokens, AAScope) :-
	initialize_aa_matching_1(Tokens, 1, AATokens),
	initialize_aa_matching_1(Scope,  1, AAScope).

initialize_aa_matching_1([], _N, []).
initialize_aa_matching_1([Token|Rest], N, InitializedTokens) :-
	N1 is N + 1,
	( higher_order_type(Token) ->
	  RestInitializedTokens = InitializedTokens
	; create_full_aa_token(Token, N, AAToken),
	  InitializedTokens = [AAToken|RestInitializedTokens]
	),
	initialize_aa_matching_1(Rest, N1, RestInitializedTokens).

create_full_aa_token(tok(Type,Text,LCText,Pos),
		     N,
                     aatok(word,Type,Text,LCText,Pos,N,0,none)).


/* perform_aa_matching(+AATokensIn, +AAScopeIn, -AATokensOut, -AAScopeOut, -AAMatch)

perform_aa_matching/5 matches the presumed AA represented by AATokensIn with
its presumed definition represented by AAScopeIn producing AAMatch and modified
AATokensOut and AAScopeOut.

AATokensIn is a list of word tokens, e.g.,
   [aatok(word,ic,"Asclepiadaceae","asclepiadaceae",pos(193,207),1,0,none)]

ScopeTokensIn is a list of word tokens, e.g.,
   [aatok(word,ic,"Asclepias","asclepias",pos(158,167),1,0,none),
    aatok(word,ws," "," ",pos(167,168),2,0,none),
    aatok(word,lc,"exaltata","exaltata",pos(168,176),3,0,none),
    aatok(word,ws," "," ",pos(176,177),4,0,none),
    aatok(word,lc,"and","and",pos(177,180),5,0,none),
    aatok(word,ws," "," ",pos(180,181),6,0,none),
    aatok(word,uc,"A","a",pos(181,182),7,0,none),
    aatok(word,pn,".",".",pos(182,183),8,0,none),
    aatok(word,ws," "," ",pos(183,184),9,0,none),
    aatok(word,lc,"syriaca","syriaca",pos(184,191),10,0,none),
    aatok(word,ws," "," ",pos(191,192),11,0,none)]

AATokensOut    are the remaining, unmatched exploded AA    tokens
ScopeTokensOut are the remaining, unmatched exploded Scope tokens
AllMatches     are all the matches

*/

perform_aa_matching(AATokensIn, ScopeTokensIn, AATokensOut, ScopeTokensOut, AllMatches) :-

	% dump_aa_match(before_matching,[],AA0,Scope0,OutputStream),
	%% match full tokens (alpha tokens only)
	%% AATokens1/ScopeTokens1 are the remaining tokens after full matching
	aa_match_full_tokens(AATokensIn, ScopeTokensIn,
			     AATokens1,  ScopeTokens1,
			     FullMatchesIn, FullMatchesOut),
	explode_aa_tokens(AATokens1,    ExplodedAATokens),
	explode_aa_tokens(ScopeTokens1, ExplodedScopeTokens),	
	% FullMatchesOut = [],
	%    (FullMatches == [] ->
	%        true
	%    ;   dump_aa_match(full,FullMatches,AA1,Scope1,OutputStream)
	%    ),
	%%  initial letters (alphanumeric tokens only)
	% I don't understand these next two calls,
	% which were breaking " Dspp-knock-out (Dspp-KO)", so I removed them.
	% get_relevant_exploded_aa_tokens(ExplodedAATokens,    AATokens1,    ExplodedAATokens1),
	% get_relevant_exploded_aa_tokens(ExplodedScopeTokens, ScopeTokens1, ExplodedScopeTokens1),
	% explode_aa_tokens(AATokens1,    ExplodedAATokens1),
	% explode_aa_tokens(ScopeTokens1, ExplodedScopeTokens1),

	% aa_match_initials(ExplodedAATokens1, ExplodedScopeTokens1,
	aa_match_initials(ExplodedAATokens, ExplodedScopeTokens,
			  AATokens2,        ScopeTokens2,
			  InitialMatchesIn, InitialMatchesOut),
	InitialMatchesOut = [],

	%    (Match3 == [] ->	%        true
	%    ;   append(FullMatches,Match3,MatchF3),
	%        dump_aa_match(initial_letters,MatchF3,AA3,Scope3,OutputStream)
	%    ),
	%   middle letters (alphanumeric tokens only)
	%   (cf word index)
	%   format(OutputStream,'~nmidT3: ~p~nmidS3: ~p~nmidM3: ~p~n',[AA3,Scope3,Match3]),

	% AATokens2 and ScopeTokens2 are the tokens remaining after aa_match_initials/5
	aa_match_middles(AATokens2,   ScopeTokens2,   InitialMatchesIn,
			 AATokensOut, ScopeTokensOut, MiddleMatches),

	% format(user_output, '~nMIDDLE MATCHES:~n', []),
	% ( member(X, MiddleMatches),
	%   print(X), nl,
	%   fail
	% ; true
	% ),	

	%    (Match4 = Match2 ->
	%        true
	%    ;   append(FullMatches,Match4,MatchF4),
	%        dump_aa_match(middle_letters,MatchF4,AA4,Scope4,OutputStream)
	%    ),
	% temp
	% format(user_output, '~w~n', [FullMatchesOut = InitialMatchesIn]),
	AllMatches = FullMatchesIn,
	FullMatchesOut = MiddleMatches.
	% format(user_output, '~w~n', [InitialMatchesOut = MiddleMatches]),
	% InitialMatchesOut = MiddleMatches,
	% format(user_output, '~w~n', [AllMatches = FullMatchesIn]),
	
	% append(FullMatchesIn, MiddleMatches, AllMatches).
	%    dump_aa_match(final,MatchF4,AA4,Scope4,OutputStream),
	%    dump_aa_match_summary(AA4,Scope4,MatchF4,AA0,Scope0,OutputStream),
	%    AAsOut=AAsIn.


/* explode_aa_tokens(+AATokens, -ExpAATokens)
   explode_aa_tokens/2 xxx
*/

%%% explode_aa_tokens/2 takes a list of tokens of the form

%%% aatok(word, lc, "heart",  "heart",  pos(0,5),   1, 0, none),
%%% aatok(word, ws, " ",      " ",      pos(5,6),   2, 0, none),
%%% aatok(word, lc, "attack", "attack", pos(6,12),  3, 0, none),
%%% aatok(word, ws, " ",      " ",      pos(12,13), 4, 0, none)

%%% and explodes them by transforming each alphanumeric aatok(word, ...) token
%%% into a list of aatok(char, ...) tokens as described below.
%%% Nnon-alphanumeric tokens are mapped to a 1-element list containing
%%% the original aatok(word, ...) token.

%%% E.g., the AAToken

%%% aatok(word,lc,"heart","heart",pos(18,5),1,0,none)

%%% is exploded into the list

%%% aatok(char, lc, 104, 104, 18, 1, 1,  aatok(word,lc,"heart","heart",pos(0,5),1,0,none)),
%%% aatok(char, lc, 101, 101, 19, 1, 2,  aatok(word,lc,"heart","heart",pos(0,5),1,0,none)),
%%% aatok(char, lc,  97,  97, 20, 1, 3,  aatok(word,lc,"heart","heart",pos(0,5),1,0,none)),
%%% aatok(char, lc, 114, 114, 21, 1, 4,  aatok(word,lc,"heart","heart",pos(0,5),1,0,none)),
%%% aatok(char, lc, 116, 116, 22, 1, 5,  aatok(word,lc,"heart","heart",pos(0,5),1,0,none))

%%% For the first aatok(char, ...) term above,
%%% Arg 1: (char) is hardcoded
%%% Arg 2: (lc)   is copied from the parent aatok(word, ...) token
%%% Arg 3: (104)  is the ASCII code for the char "h"
%%% Arg 4: (104)  is the ASCII code for the LC version of the char "h"
%%% Arg 5: (18)   is the position in the utterance of the char "h"
%%% Arg 6: (1)    is the position of the parent aatok(word, ...) token in the aatok list
%%% Arg 7: (1)    is the position in the token string of the char "h"
%%% Arg 8:        is the entire parent token

% explode_aa_tokens/2 is now implemented using difference lists,
% so there is no longer any need to append the partial results.
explode_aa_tokens(AATokens, ExpAATokens) :-
	explode_aa_tokens_aux(AATokens, ExpAATokens, []).
	% append(ExpAATokensList, ExpAATokens).

explode_aa_tokens_aux([], ExplodedOut, ExplodedOut).
explode_aa_tokens_aux([AAToken|RestAATokens], ExplodedIn, ExplodedOut) :-
	explode_one_aa_token(AAToken, ExplodedIn, ExplodedNext),
	explode_aa_tokens_aux(RestAATokens, ExplodedNext, ExplodedOut).

explode_one_aa_token(AAToken, ExplodedIn, ExplodedOut) :-
	% TokenPos is the position of this aatok within the list of all AATokens
	AAToken = aatok(word,Type,Text,LCText,Pos,TokenPos,0,none),
	an_type(Type),
	!,
	Pos = pos(UtteranceCharPos,_End),
	explode_one_aa_token_aux(Text, LCText, Type, UtteranceCharPos, TokenPos, 1, AAToken,
				 ExplodedIn, ExplodedOut).
explode_one_aa_token(AAToken, [AAToken|Rest], Rest).

explode_one_aa_token_aux([], [], _, _, _, _, _, ExplodedOut, ExplodedOut).
% [C1|RestCh]    is Text
% [LC1|RestLCCh] is LCText
% UtteranceCharPos is the position of the C1/LC1 char within the utterance
% TokenPos is the position of the aatok(word, ...) token within the list of such tokens
% TokenCharPos is the position of the C1/LC1 char within the word
explode_one_aa_token_aux([C1|RestChars], [LC1|RestLCChars],
			 Type, UtteranceCharPos, TokenPos, TokenCharPos, Parent,
			 [aatok(char,Type,C1,LC1,UtteranceCharPos,TokenPos,TokenCharPos,Parent)
			   |AATokenListNext], AATokenListOut) :-
	NewUtteranceCharPos is UtteranceCharPos + 1,
	NextTokenCharPos is TokenCharPos + 1,
	explode_one_aa_token_aux(RestChars, RestLCChars, Type, NewUtteranceCharPos, TokenPos,
				 NextTokenCharPos, Parent, AATokenListNext, AATokenListOut).

% get_relevant_exploded_aa_tokens([FirstExplodedToken|RestExplodedTokens],
% 				[FirstParentToken|RestParentTokens],
% 				RelevantExplodedTokens) :-
% 	( FirstExplodedToken = aatok(_Char,_Type,_C1,_LC1,_UtteranceCharPos,
% 				     _TokenPos,_TokenCharPos,FirstParentToken) ->
% 	  RelevantExplodedTokens = [FirstExplodedToken|RestExplodedTokens]
% 	; get_relevant_exploded_aa_tokens(RestExplodedTokens,
% 					  [FirstParentToken|RestParentTokens],
% 					  RelevantExplodedTokens)
% 	).
	

/* aa_match_full_tokens(+AAIn, +ScopeIn, -AAOut, -ScopeOut, -MatchOut)

aa_match_full_tokens/5 matches full tokens in AAIn with those in ScopeIn
(note that the AA and Scope variables have "in" and "out" versions)
When a matching token (lowercase compare) is found, it is removed from AATokens
and AAScope and put in AAMatch.
Then a match is attempted on the next AAToken using ALL AAScope tokens
except for the one just matched, i.e., full tokens can match in any order! */

% The text
% heart attack (HEART ATTACK)
% generates two full-token matches; aa_match_full_tokens/5 creates these two terms:

% match(full,
%       aatok(word, uc, "HEART", "heart", pos(14,19), 1, 0, none),  <--- the AA    token
%       aatok(word, lc, "heart", "heart", pos(0,5),   1, 0, none))  <--- the Scope token

% match(full,
%       aatok(word, uc, "ATTACK", "attack", pos(20,26), 3, 0, none),  <--- the AA    token
%       aatok(word, lc, "attack", "attack", pos(6,12),  3, 0, none))  <--- the Scope token

aa_match_full_tokens(AAIn, ScopeIn, AAOut, ScopeOut, MatchIn, MatchOut) :-
	aa_match_full_tokens_aux(AAIn, ScopeIn, AAOut, ScopeOut, MatchIn, MatchOut).
	% rev(RevMatchOut, MatchOut).

aa_match_full_tokens_aux([], ScopeOut, [], ScopeOut, MatchOut, MatchOut).
aa_match_full_tokens_aux([AATok|RestAAIn], ScopeIn, AAOut, ScopeOut, MatchIn, MatchOut) :-
	AATok = aatok(word,Type,_,LCText,_,_,_,_),
	% First AA token is uc, lc, ic, mc, or an (not nu),
	alpha_type(Type),
	% so try to find a full-token match for it in the scope tokens
	aa_match_full_single_token(ScopeIn, ScopeNext, AATok, LCText, MatchIn, MatchNext),
	!,
	aa_match_full_tokens_aux(RestAAIn, ScopeNext, AAOut, ScopeOut, MatchNext, MatchOut).
aa_match_full_tokens_aux([AATok|RestAAIn], ScopeIn, [AATok|RestAAOut], ScopeOut,
                         MatchIn, MatchOut) :-
	aa_match_full_tokens_aux(RestAAIn, ScopeIn, RestAAOut, ScopeOut, MatchIn, MatchOut).

% aa_match_full_single_token/6 has no terminating condition
% (i.e., a base case with [] instead of a non-empty list)
% because we either find a match for LCText, in which case we succeed,
% or we fail.

% LCText is a word in the AA list. This clause succeeds if
% there's a full-token match on LCText and the first Scope token.
% create a term of the form
% match(full, AAToken, ScopeToken)
aa_match_full_single_token([ScopeTok|RestScopeIn], RestScopeIn, AATok, LCText, 
			   [match(full,AATok,ScopeTok)|MatchIn], MatchIn) :-
	ScopeTok = aatok(word,_,_,LCText,_,_,_,_),
	!.
% There's no full match, so put the first Scope token in ScopeOut,
% and recurse on the remaining Scope tokens.
aa_match_full_single_token([ScopeTok|RestScopeIn], [ScopeTok|RestScopeOut],
			   AATok, LCText, MatchIn, MatchOut) :-
	aa_match_full_single_token(RestScopeIn, RestScopeOut, AATok, LCText, MatchIn, MatchOut).


/* aa_match_initials(+AAIn, +ScopeIn, -AAOut, -ScopeOut, -MatchOut)

aa_match_initials/6 matches exploded tokens (i.e., individual characters)
in AAIn with initial characters of tokens in ScopeIn (note that the
AA and Scope variables have "in" and "out" versions).  When a match
is found (lowercase compare), the characters involved are removed from
AATokens and AAScope and put in AAMatch.  Then a match is attempted on the
next AAToken character using only remaining AAScope tokens, i.e., initial
character matching respects order.  */

% The text
% heart attack (HA) HA.
% generates two initial matches; aa_match_initials/5 creates these two terms:

% match(init,
%       aatok(char, uc, 72,  104, 14, 1, 1,
%	      aatok(word, uc, "HA", "ha", pos(14, 16), 1, 0, none)), 
%       aatok(char, lc, 104, 104, 0,  1, 1,
%	      aatok(word, lc, "heart", "heart", pos(0, 5), 1, 0, none)))

% match(init, 
%       aatok(char, uc, 65, 97, 15, 1, 2,
%	      aatok(word, uc, "HA", "ha", pos(14, 16), 1, 0, none)), 
%       aatok(char, lc, 97, 97, 6,  3, 1,
%	      aatok(word, lc, "attack", "attack", pos(6, 12), 3, 0, none)))

aa_match_initials(AAIn, ScopeIn, AAOut, ScopeOut, MatchesIn, MatchesOut) :-
	aa_match_initials_aux(AAIn, ScopeIn, AAOut, ScopeOut, MatchesIn, MatchesOut).
	% rev(RevMatchOut, MatchOut).

aa_match_initials_aux([], ScopeIn, [], ScopeIn, MatchIn, MatchIn).
aa_match_initials_aux([AATok|RestAAIn], ScopeIn, AAOut, ScopeOut, MatchIn, MatchOut) :-
	AATok = aatok(char,_,_,LCCh,_,_,_,_),
	% !,
	aa_match_initial_single_token(ScopeIn, ScopeNext, AATok, LCCh, MatchIn, MatchNext),
	aa_match_initials_aux(RestAAIn, ScopeNext, AAOut, ScopeOut, MatchNext, MatchOut).
aa_match_initials_aux([AATok|RestAAIn], ScopeIn, [AATok|RestAAOut], ScopeOut, MatchIn, MatchOut) :-
	aa_match_initials_aux(RestAAIn, ScopeIn, RestAAOut, ScopeOut, MatchIn, MatchOut).

aa_match_initial_single_token([ScopeTok|RestScopeIn], RestScopeIn, AATok, LCCh,
			      [match(init,AATok,ScopeTok)|MatchIn], MatchIn) :-
	ScopeTok = aatok(char,_,_,LCCh,_,_,1,_).
	% must be nondeterminate
	% !.
aa_match_initial_single_token([ScopeTok|RestScopeIn], [ScopeTok|RestScopeOut],
			      AATok, LCCh, MatchIn, MatchOut) :-
    aa_match_initial_single_token(RestScopeIn, RestScopeOut, AATok, LCCh, MatchIn, MatchOut).

/* aa_match_middles(+AATokensIn,  +AAScopeIn,  +AAMatchIn,
   		    -AATokensOut, -AAScopeOut, -AAMatchOut)

aa_match_middles/6 matches exploded tokens (i.e., individual characters)
in AATokens with any characters of tokens in AAScope (note each of
AATokens, AAScope and AAMatch has an "in" and "out" variable).  When a match
is found (lowercase compare), the characters involved are removed from
AATokens and AAScope and put in AAMatch.  Then a match is attempted on the
next AAToken character using only remaining AAScope tokens, i.e., middle
character matching respects order.  */

%%% Given the input text
%%%
%%% heart attack (HT) HT
%%%
%%% aa_match_initials/6 matches "H" with "heart"; when aa_match_middles/6 is called:
%%%
%%% AAIn =
%%% aatok(char,uc,84,116,15,1,2,aatok(word,uc,"HT","ht",pos(14,16),1,0,none))  <-- "T"
%%%
%%% ScopeIn =
%%% aatok(char,lc,101,101,1,1,2,aatok(word,lc,"heart","heart",pos(0,5),1,0,none)),
%%% aatok(char,lc,97,97,2,1,3,aatok(word,lc,"heart","heart",pos(0,5),1,0,none)),
%%% aatok(char,lc,114,114,3,1,4,aatok(word,lc,"heart","heart",pos(0,5),1,0,none)),
%%% aatok(char,lc,116,116,4,1,5,aatok(word,lc,"heart","heart",pos(0,5),1,0,none)),
%%% aatok(word,ws," "," ",pos(5,6),2,0,none),
%%% aatok(char,lc,97,97,6,3,1,aatok(word,lc,"attack","attack",pos(6,12),3,0,none)),
%%% aatok(char,lc,116,116,7,3,2,aatok(word,lc,"attack","attack",pos(6,12),3,0,none)),
%%% aatok(char,lc,116,116,8,3,3,aatok(word,lc,"attack","attack",pos(6,12),3,0,none)),
%%% aatok(char,lc,97,97,9,3,4,aatok(word,lc,"attack","attack",pos(6,12),3,0,none)),
%%% aatok(char,lc,99,99,10,3,5,aatok(word,lc,"attack","attack",pos(6,12),3,0,none)),
%%% aatok(char,lc,107,107,11,3,6,aatok(word,lc,"attack","attack",pos(6,12),3,0,none)),
%%% aatok(word,ws," "," ",pos(12,13),4,0,none)
%%%
%%% MatchIn =
%%% match(init,
%%%       aatok(char,uc,72,104,14,1,1,aatok(word,uc,"HT","ht",pos(14,16),1,0,none)),
%%%       aatok(char,lc,104,104,0,1,1,aatok(word,lc,"heart","heart",pos(0,5),1,0,none)))

%%% In this case, the initial-match token is less than the AAIn token.

aa_match_middles(AAIn, ScopeIn, MatchIn, AAOut, ScopeOut, MatchOut) :-
	aa_match_middles_aux(AAIn, ScopeIn, MatchIn, AAOut, ScopeOut, MatchOut, 0, 1, 999, 1).

aa_match_middles_aux([], ScopeIn, MatchIn, [], ScopeIn, MatchIn, _, _, _, _).
aa_match_middles_aux([AATok|RestAAIn], ScopeIn, MatchIn,
		     [AATok|RestAAOut], ScopeOut, MatchOut,
		     LBN, LBI, UBN, UBI) :-
	% skip complete word token in AA (e.g., a ws token), but put it in the AAOut list
	AATok = aatok(word,_,_,_,_,_,_,_),
	!,
	aa_match_middles_aux(RestAAIn, ScopeIn, MatchIn,
			     RestAAOut, ScopeOut, MatchOut,
			     LBN, LBI, UBN, UBI).
aa_match_middles_aux(AAIn, [FirstScope|RestScopeIn], MatchIn,
		     AAOut, [FirstScope|RestScopeOut], MatchOut,
		     LBN, LBI, UBN, UBI) :-
	% skip complete word token in Scope (e.g., a ws token), but put it in the ScopeOut list
	FirstScope = aatok(word,_,_,_,_,_,_,_),
	!,
	aa_match_middles_aux(AAIn, RestScopeIn, MatchIn,
			     AAOut, RestScopeOut, MatchOut,
			     LBN, LBI, UBN, UBI).
aa_match_middles_aux([AATok|RestAAIn], ScopeIn, MatchIn,
		     AAOut, ScopeOut, [FirstMatch|RestMatchOut],
		     _LBN, _LBI, UBN, UBI) :-
	nonvar(MatchIn),
	MatchIn = [FirstMatch|RestMatchIn],
	% skip over match element to the left of the current token,
	% but put it in the MatchOut list
	FirstMatch = match(_MatchType, MatchAATok, MatchScopeTok),
	match_less_than_token(MatchAATok, AATok),
	!,
	MatchScopeTok = aatok(_,_,_,_,_,NewLBN,NewLBI,_),
	% update LBN and LBI to the matching Scope token's 
        % position of the parent aatok(word, ...) token in the Scope list, and the
	% position in the token string of the init match char ("h")
	aa_match_middles_aux([AATok|RestAAIn], ScopeIn, RestMatchIn,
			     AAOut, ScopeOut, RestMatchOut,
			     NewLBN, NewLBI, UBN, UBI).
aa_match_middles_aux([AATok|RestAAIn], ScopeIn, MatchIn,
		     AAOut, ScopeOut, MatchOut, LBN, LBI, _, _) :-
	% match the scope
	AATok = aatok(char,_,_,LCCh,_,_,_,_),
	compute_scope_upper_bound(MatchIn, UBN, UBI),
	aa_match_middle_single_token(ScopeIn, ScopeNext,
				     LBN, LBI, UBN, UBI, AATok, LCCh,
				     MatchIn, MatchNext),
	aa_match_middles_aux(RestAAIn, ScopeNext, MatchNext,
			     AAOut, ScopeOut, MatchOut,
			     LBN, LBI, UBN, UBI).
aa_match_middles_aux([AATok|RestAAIn], ScopeIn, MatchIn,
		     [AATok|RestAAOut], ScopeOut, MatchOut,
		     LBN, LBI, UBN, UBI) :-
	% no match
	aa_match_middles_aux(RestAAIn, ScopeIn, MatchIn,
			     RestAAOut, ScopeOut, MatchOut,
			     LBN, LBI, UBN, UBI).

compute_scope_upper_bound(Match, 999, 1) :- var(Match), !.
compute_scope_upper_bound([], 999, 1).
compute_scope_upper_bound([match(_,_,aatok(_,_,_,_,_,N,I,_))|_], N, I).

%%% N is the position of the parent aatok(word, ...) token in the Scope list
%%% I is the position in the token string of the char

aa_match_middle_single_token([FirstScope|_], _, _, _, UBN, UBI, _, _, _, _) :-
	FirstScope = aatok(char,_,_,_,_,N,I,_),
	( N > UBN ->
	  true
	; N =:= UBN ->
	  I > UBI
	),
	!,
	fail.
aa_match_middle_single_token([FirstScope|RestScopeIn], RestScopeIn,
			     LBN, LBI, _, _, AATok, LCCh, 
			     MatchOut, [match(mid,AATok,FirstScope)|MatchOut]) :-
	FirstScope = aatok(char,_,_,LCCh,_,N,I,_),
	!,
	I > 1,
	( N > LBN ->
	  true
	; N =:= LBN ->
	  I > LBI
	).
aa_match_middle_single_token([FirstScope|RestScopeIn], [FirstScope|RestScopeOut],
			     LBN, LBI, UBN, UBI, AATok, LCCh,
			     MatchIn, MatchOut) :-
	aa_match_middle_single_token(RestScopeIn, RestScopeOut,
				     LBN, LBI, UBN, UBI, AATok, LCCh,
				     MatchIn, MatchOut).

% Not used!
% token_less_than_match(aatok(_,_,_,_,_,TN,TI,_),
%                       match(_,aatok(_,_,_,_,_,MN,MI,_),_)) :-
% 	( TN < MN ->
% 	  true
% 	  TN =:= MN ->
% 	; TI < MI
% 	).

% In the initial match(_) term,
% the first  aatok(_) term represents the matching character from the AA, and
% the second aatok(_) term represents the matching character from the Scope:

% match(init,
%       aatok(char,
%             uc,72,104,14,1,1,
% 	      aatok(word,uc,"HT","ht",pos(14,16),1,0,none)),      <-- "H" from AA match (1)
%       aatok(char,
%	      lc,104,104,0,1,1,
%	      aatok(word,lc,"heart","heart",pos(0,5),1,0,none)))  <-- "h" from Scope Match (2)

% The first  argument of match_less_than_token/2 is the first aatok(_) term (from the AA match).
% The second argument of match_less_than_token/2 is one of the remaining chars in the AA:

% aatok(char,
%       uc,84,116,15,1,2,
%       aatok(word,uc,"HT","ht",pos(14,16),1,0,none))             <-- "T" from AA

% In this case, the "H" occurs before (i.e., is less than) the "T" token.

match_less_than_token(aatok(_,_,_,_,_,MatchTokenPos,MatchTokenCharPos,_),
                      aatok(_,_,_,_,_,TokenPos,     TokenCharPos,     _)) :-
	% The matching letter is from an earlier token.
	( MatchTokenPos < TokenPos ->
	  true
	% The matching letter is from the same token,
	% but occurrs earlier in the AA token character list.
	; MatchTokenPos =:= TokenPos ->
	  MatchTokenCharPos < TokenCharPos
	).


store_aa(AATokens, LastUnwantedToken, ExpansionTokens0, AAsIn, AAsOut) :-
	% We do *not* want to trim ws tokens from the AATokens,
	% because the text could be something like
	% "multiple endocrine neoplasia type 1 ( MEN-1 )".
	% This example (w/o the spaces) was flagged by Sugato Bagchi on 12/20/2011.
	% trim_ws_tokens(AATokens0, AATokens),
	% Trim only the definition; internal whitespace is needed later (I think).
	% Calling trim_ws_tokens results in the creation of bad tokens like "beta1".
	trim_ws_tokens(ExpansionTokens0, ExpansionTokens1),
        % ExpansionTokens1 = ExpansionTokens0,
	% This call handles AAs like "Transforming growth factor-beta (TGF-)"
	% which show up later as "TGF-2". We don't want the token "beta2" to be created!
	add_hyphen_to_expansion(AATokens, LastUnwantedToken, ExpansionTokens1, ExpansionTokens),
	% modify_scope_for_punctuation(Tokens, ExpansionTokens1, ExpansionTokens),
	% temp
	% format('~nAdding to AVL:~p~n~p~n',[Tokens,ExpansionTokens]),
	add_to_avl_once(AATokens, ExpansionTokens, AAsIn, AAsOut).

add_hyphen_to_expansion(AATokens, LastUnwantedToken, ExpansionTokensIn, ExpansionTokensOut) :-
	% Do the AA tokens end with a hyphen? E.g.,
	% "Hydrosulfide (HS-) coordination in iron porphyrinates." (PMID 20038134).
	( last(AATokens, LastAAToken),
	  token_template(LastUnwantedToken, pn, HyphenString1, HyphenString1, _PosInfo1),
	  token_template(LastAAToken,       pn, HyphenString1, HyphenString1, _PosInfo2),
	  hyphen_punc(HyphenString1) ->
	  last(ExpansionTokensIn, LastExpansionToken),
	  % deconstruct the last AA Expansion token
	  token_template(LastExpansionToken, _LastTokenType,
			 _LastExpansionTokenString, _LastExpansionTokenLCString,
			 pos(_LastStartPos,LastEndPos)),
	  LastEndPosPlus1 is LastEndPos + 1,
	  % create a new hyphen token
	  token_template(NewHyphenToken1, pn,
			 HyphenString1, HyphenString1,
			 pos(LastEndPos, LastEndPosPlus1)),
	  append(ExpansionTokensIn, [NewHyphenToken1], ExpansionTokensOut)
	  % Do the AA tokens begin with a hyphen? E.g.,
	  % "Delta-5 polyunsaturated fatty acids (-5 PUFA)" (PMID 20001760).
	; AATokens = [FirstAAToken|_],
	  token_template(FirstAAToken, pn, HyphenString2, HyphenString2, _PosInfo2),
	  hyphen_punc(HyphenString2) ->
	  ExpansionTokensIn = [FirstExpansionToken|_],
	  % deconstruct the first AA Expansion token
	  token_template(FirstExpansionToken, _FirstTokenType,
			 _FirstExpansionTokenString, _FirstExpansionTokenLCString,
			 pos(FirstStartPos,FirstEndPos)),
%	  FirstStartPosMinus1 is FirstStartPos - 1,
	  % create a new hyphen token
	  token_template(NewHyphenToken2, pn,
			 HyphenString2, HyphenString2,
			 pos(FirstStartPos, FirstEndPos)),
%			 pos(FirstStartPosMinus1, FirstStartPos)),
	  ExpansionTokensOut = [NewHyphenToken2|ExpansionTokensIn]
	; ExpansionTokensOut = ExpansionTokensIn
	).


% I have no idea why this is in here.
% modify_scope_for_punctuation(Tokens, ScopeIn, ScopeOut) :-
%	% reversed order of args from QP library version!
% 	last(Tokens, FinalToken),
% 	( punc_tok(FinalToken) ->
% 	  ScopeIn = [H|T],
% 	  add_punc_tok_to_scope(T, H, FinalToken, ScopeOut)
% 	; ScopeOut = ScopeIn
% 	).
% 
% add_punc_tok_to_scope([], ThisScopeToken, PuncToken, [ThisScopeToken,ModifiedPuncToken]) :-
% 	modify_final_token(ThisScopeToken, PuncToken, ModifiedPuncToken).
% 
% add_punc_tok_to_scope([NextScopeToken|RestScopeTokensIn], ThisScopeToken, PuncToken,
% 		      [ThisScopeToken|RestScopeTokensOut]) :-
% 	add_punc_tok_to_scope(RestScopeTokensIn, NextScopeToken, PuncToken, RestScopeTokensOut).
% 
% modify_final_token(ThisScopeToken, PuncToken, ModifiedPuncToken) :-
% 	ThisScopeToken = tok(_ThisType, _ThisString, _ThisLCString, ThisPos),
% 	ThisPos = pos(_StartPos,EndPos),
% 	EndPosPlusOne is EndPos + 1,
% 	PuncToken = tok(pn, PuncString, PuncLCString, _PuncPos),
% 	ModifiedPuncToken = tok(pn, PuncString, PuncLCString, pos(EndPos,EndPosPlusOne)).

% dump_aa_match(_Label,AAMatch,_AATokens,_AAScope,_OutputStream) :-
%    format(OutputStream,'Match (~a)~n',[Label]),
%    dump_aa_match(AAMatch,_OutputStream).
%    format(OutputStream,'  AATokens:~n',[]),
%    dump_aa_tokens(AATokens,OutputStream),
%    format(OutputStream,'  AAScope:~n',[]),
%    dump_aa_tokens(AAScope,OutputStream).

% dump_aa_match([],_).
% dump_aa_match([match(Type,aatok(word,_,T1,LCT1,Pos1,N1,_,_),
%                          aatok(word,_,T2,LCT2,Pos2,N2,_,_))|Rest],_OutputStream) :-
%    !,
%    format(OutputStream,'~t~d. ~5|~s  ~s  ~p   ~d. ~s  ~s  ~p   (~a)~n',
%              [N1,T1,LCT1,Pos1,N2,T2,LCT2,Pos2,Type]),
%    dump_aa_match(Rest,_OutputStream).
% dump_aa_match([match(Type,aatok(char,_,Ch1,LCCh1,Begin1,N1,I1,_),
%                          aatok(char,_,Ch2,LCCh2,Begin2,N2,I2,_))|Rest],_OutputStream) :-
%    !,
%    format(OutputStream,'~t~d.~4|~d ~c  ~c  ~d   ~d.~d ~c  ~c  ~d   (~a)~n',
%              [N1,I1,Ch1,LCCh1,Begin1,N2,I2,Ch2,LCCh2,Begin2,Type]),
%    dump_aa_match(Rest,_OutputStream).

% dump_aa_tokens([],_).
% dump_aa_tokens([aatok(word,_,Text0,LCText0,Pos,N,_,_)|Rest],_OutputStream) :-
%    !,
%    translate_newlines_to_backslash(Text0,Text),
%    translate_newlines_to_backslash(LCText0,LCText),
%    format(OutputStream,'~t~d. ~7|~s  ~s  ~p~n',[N,Text,LCText,Pos]),
%    dump_aa_tokens(Rest,_OutputStream).
% dump_aa_tokens([aatok(char,_,Ch,LCCh,Begin,N,I,_)|Rest],_OutputStream) :-
%    !,
%    format(OutputStream,'~t~d.~6|~d ~c  ~c  ~d~n',[N,I,Ch,LCCh,Begin]),
%    dump_aa_tokens(Rest,_OutputStream).


% dump_aa_match_summary(AATokens,AAScope,AAMatch,AATokens0,AAScope0,OutputStream) :-
%%    format(OutputStream,'T0: ~p~nS0: ~p~nT: ~p~nS: ~p~n',
%%           [AATokens0,AAScope0,AATokens,AAScope]),
%    count_an_chars_aa(AATokens0,NT0),
%    count_an_chars_aa(AAScope0,NS0),
%    count_an_chars_aa(AATokens,NT),
%    filter_out_interior(AAScope,FAAScope),
%    count_an_chars_aa(FAAScope,NS),
%    T is NT/NT0,
%    S is NS/NS0,
%    R is T + S,
%    format(OutputStream,'|',[]),
%    dump_the_match(AAMatch,OutputStream),
%    format(OutputStream,'|~d,~d;~d,~d:~f+~f=~f',[NT0,NT,NS0,NS,T,S,R]),
%    (R > 0.5 ->
%        format(OutputStream,'|no~n',[])
%    ;   format(OutputStream,'|yes~n',[])
%    ).

% AATokens are the unmatched exploded tokens in the AA
% AAScope  are the unmatched exploded tokens in the Scope
% AATokens0 are the original word tokens in the AA
% AAScope0  are the original word tokens in the Scope
evaluate_aa_match(AATokens, AAScope, _AAMatch, AATokens0, AAScope0,
		  NT, NT0, NS, NS0, T, S, V, YN,_OutputStream) :-
	% NT is the number of *unmatched* chars in the AA
	count_an_chars_aa(AATokens, 0, NT),
	% NT0 is the number of chars in the an (alphanumeric) tokens in the AA
	count_an_chars_aa(AATokens0, 0, NT0),
	filter_out_interior(AAScope, FAAScope),
	count_an_chars_aa(FAAScope, 0, NS),
	% NS0 is the number of alphanumeric chars in the expansion of the acronym
	count_an_chars_aa(AAScope0, 0,NS0),
	% We require that the expansion of the acronym (NS0) contain more chars
	% than the acronym itself (NT0). This is for handling cases such as
	% "Immigration and Nationality Act (Act).",
	% in which "(Act)" is *not* an acronym. I.e., we require NS0 > NT0,
	% but the sense of the test must be reversed for YN = no,
	% so the test for YN = no must be NS0 =< NT0.
	T is NT/NT0,
	S is NS/NS0,
	V is T + S,
	get_aa_cutoff_value(Cutoff),
	% default cutoff was 0.5, and now is 0.30
	( ( V > Cutoff
	  ; NS0 =< NT0 ) ->
	    YN = no
	;   YN = yes
	).

filter_out_interior([],[]).
filter_out_interior([aatok(_,_,_,_,_,N,I,_)|Rest],Result) :-
    I > 1,
    !,
    filter_the_interior(Rest,N,NewRest),
    filter_out_interior(NewRest,Result).
filter_out_interior([First|Rest],[First|RestResult]) :-
    First=aatok(_,_,_,_,_,N,1,_),
    !,
    keep_the_interior(Rest,N,RestResult).
filter_out_interior([First|Rest],[First|FilteredRest]) :-
    filter_out_interior(Rest,FilteredRest).


filter_the_interior([],_,[]) :-
    !.
filter_the_interior([aatok(_,_,_,_,_,N,_,_)|Rest],N,Result) :-
    !,
    filter_the_interior(Rest,N,Result).
filter_the_interior(AAToks,_,AAToks).


keep_the_interior([],_,[]).
keep_the_interior([First|Rest],N,[First|RestResult]) :-
    First=aatok(_,_,_,_,_,N,_,_),
    !,
    keep_the_interior(Rest,N,RestResult).
keep_the_interior(AAToks,_,Result) :-
    filter_out_interior(AAToks,Result).


count_an_chars_aa([], Count, Count).
count_an_chars_aa([aatok(char,_,_,_,_,_,_,_)|Rest], CountIn, CountOut) :-
	!,
	CountNext is CountIn + 1,
	count_an_chars_aa(Rest, CountNext, CountOut).
count_an_chars_aa([aatok(word,Type,Text,_,_,_,_,_)|Rest], CountIn, CountOut) :-
	( an_type(Type) ->
	  length(Text, Count),
	  CountNext is CountIn + Count
	; CountNext = CountIn
	),
	count_an_chars_aa(Rest, CountNext, CountOut).


%count_an_tokens_aa(AAToks,N) :-
%    count_an_tokens_aa(AAToks,0,N).
%
%count_an_tokens_aa([],N,N).
%count_an_tokens_aa([aatok(char,_,_,_,_,_,_,_)|Rest],NIn,NOut) :-
%    !,
%    count_an_tokens_aa(Rest,NIn,NOut).
%count_an_tokens_aa([aatok(word,Type,_,_,_,_,_,_)|Rest],NIn,NOut) :-
%    (an_type(Type) ->
%        NInOut is NIn + 1
%    ;   NInOut=NIn
%    ),
%    count_an_tokens_aa(Rest,NInOut,NOut).

count_an_tokens([], N, N).
count_an_tokens([tok(Type,_,_,_)|Rest], NIn, NOut) :-
	( an_type(Type) ->
	  NInOut is NIn + 1
	; NInOut=NIn
	),
	count_an_tokens(Rest, NInOut, NOut).

/* 
   find_and_coordinate_sentences(+UI, +Fields, -Sentences,
                                 -CoordinatedSentences, +AAs)
find_and_coordinate_sentences/5 computes Sentences and CoordinatedSentences
from the Fields of UI by using find_sentences/3 and find_aas/3 to find
sentences and higher-order tokens in them (e.g., acronyms/abbreviations) and
then constructing unexpanded and coordinated, expanded versions of the
sentences contained in Fields. CoordinatedSentences is a list of tok/5 terms
representing the expanded sentences in which the fifth argument is the position
of the corresponding token(s) in the original Sentences (generally, a
many-to-many relationship). */

%%% EXPLANATION of Sentences/CoordSentences:
%%% Sentences captures the character positions in the original literal text,
%%% WITHOUT taking into account the 6 blank spaces at the beginning
%%% of most TI/AB MEDLINE lines, but otherwise respecting all whitespace in the text.
%%% I.e., multiple blank spaces are converted to just one.
%%% There is also no blank space between the TI and the AB.
%%% This positional information is obviously not useful for indexing into
%%% the character positions of the original input text of MEDLINE citations.

%%% CoordSentences tokens' 4th argument modifies the PosInfo by omitting AAdefs.
%%% However, it also INCORRECTLY expands UDAs. This needs to be fixed.

find_and_coordinate_sentences(UI, Fields, Sentences, CoordinatedSentences,
			      AAs, UDAListIn, UDAListOut, UDA_AVL) :-
	tokenize_fields_utterly(Fields, TokenizedFields),
	% format(user_output, 'TokenizedFields: ~p~n', [TokenizedFields]),
	form_field_tokens(TokenizedFields, FieldTokens0),
	% format(user_output, '~nFieldTokens0: ~p~n', [FieldTokens0]),
	filter_out_field_comments(FieldTokens0, FieldTokens),
	% format(user_output, '~nFieldTokens: ~p~n', [FieldTokens]),
	find_field_sentences(FieldTokens, FieldSentences),
	% format(user_output, '~nFieldSentences: ~p~n', [FieldSentences]),
	flatten_field_sentences(FieldSentences, Sentences0),
	% format(user_output, '~nSentences0:~n', []),
	current_output(OutputStream),
	find_all_aas(Sentences0, UI, OutputStream, AAs0),
	avl_to_list(AAs0, AAList0),
	remove_aa_redundancy(AAList0, AAList1),
	remove_aa_expansion_overlap(AAList1, AAList1, AAList2),
	list_to_avl(AAList2, AAs),
	order_aas(AAList2, AAList),
	resolve_AA_conflicts(AAList, UDAListIn, UDAListNext),
	% format(user_output, '~nAAs:~p~n', [AAs]),
	% FakeAAs = node([tok(uc,"ha","ha",pos(16,18))],[[tok(lc,"heart","heart",pos(16,18)),tok(ws," "," ",pos(16,18)),tok(lc,"attack","attack",pos(16,18))]],0,empty,empty),
	% annotate_with_aas(FakeAAs, Sentences0, Sentences1),
	annotate_with_aas(AAList, Sentences0, Sentences1),
	empty_avl(UDA_AVL0),
	% UDA_AVL contains those UDAs that were actually found in the text
	% annotate_with_UDAs(Sentences1, UDA_AVL0, UDAListOut, UDA_AVL, Sentences2),
	UDAListUsed = [],
	annotate_with_UDAs(UDAListNext, Sentences1, UDA_AVL0, UDA_AVL,
			   UDAListUsed, UDAListOut, Sentences2),
	% format(user_output, '~nSentences1:~n', []),
	add_sentence_labels(Sentences2, UI, "TX", 0, Sentences),
	% format(user_output, '~nSentences:~n', []),
	% extract_UDA_tokens(UDATermList, UDATokens),
	coordinate_sentences(Sentences, UI, UDA_AVL, TempCoordinatedSentences),
	CoordinatedSentences  = TempCoordinatedSentences,
	% format(user_output, user_output, 'TempCoordinatedSentences:~n', []),
	% skr_fe:write_token_list(TempCoordinatedSentences, 0, 1),
	% format(user_output, user_output, 'CoordinatedSentences:~n', []),
	% skr_fe:write_token_list(CoordinatedSentences, 0, 1),
	!.


% UDATermList is a list of terms of the form UDA-Tokens, e.g.,
%   [tok(uc,"CAT","cat",pos(59,62))]-
%     [[tok(ic,"Computerized","computerized",pos(59,62)),
%       tok(ws," "," ",pos(59,62)),
%       tok(ic,"Axial","axial",pos(59,62)),
%       tok(ws," "," ",pos(59,62)),
%       tok(ic,"Tomography","tomography",pos(59,62))]

%% extract_UDA_tokens(UDATermList, UDATokens) :-
%% 	(  foreach(UDATerm, UDATermList),
%% 	   foreach(UDATermTokens, UDATokenList)
%% 	do UDATerm = _UDA-[UDATermTokens]
%% 	),
%% 	append(UDATokenList, UDATokens).


skip_over_position([],_,[]) :-
    !.
skip_over_position([tok(_,_,_,TokPos)|Rest],Pos,ModifiedRest) :-
    position_contains(Pos,TokPos),
    !,
    skip_over_position(Rest,Pos,ModifiedRest).
skip_over_position(Tokens,_,Tokens).

skip_over_ws([tok(ws,_,_,_)|Rest],ModifiedRest) :-
    !,
    skip_over_ws(Rest,ModifiedRest).
skip_over_ws(Tokens,Tokens).

skip_over_parenthetical([tok(pe,_,_,Pos)|Rest],ModifiedRest) :-
    !,
    skip_over_position(Rest,Pos,ModifiedRest).
% Do nothing if not at a parenthetical
skip_over_parenthetical(Tokens,Tokens).

% Skip over a parenthetical, skipping preceeding whitespace only if
% the parenthetical is not followed by an alphanumeric token, i.e.,
% by whitespace or punctuation. This prevents words from being
% concatentated when there is no whitespace after the parenthetical.
skip_over_ws_parenthetical([],[]) :- !.
skip_over_ws_parenthetical(TokensIn,TokensOut) :-
    skip_over_ws(TokensIn,TokensInOut),
    skip_over_parenthetical(TokensInOut,TokensOut),
    %%% change to \+at_ws_or_pn_tok(TokensOut),
    % \+at_an_tok(TokensOut),
    at_ws_or_pn_tok(TokensOut),
    !.
skip_over_ws_parenthetical(TokensIn,TokensOut) :-
    gather_whitespace(TokensIn,WSTokens,Tokens1),
    skip_over_parenthetical(Tokens1,Tokens2),
    append(WSTokens,Tokens2,TokensOut).


/* flatten_field_sentences(+FieldSentences, -Sentences)
   flatten_field_sentences_aux(+FieldSentences, +Offset, -Sentences)

flatten_field_sentences/2 concatenates the sentences in FieldSentences,
adjusting the pos/2 terms and inserting field tokens of the form
  tok(field,<field>,<lcfield>,<position>)
along the way.

flatten_field_sentences_aux/3 is an auxiliary predicate that keeps track of the
Offset from the beginning of all fields for the current field. */

flatten_field_sentences(FieldSentences,Sentences) :-
    flatten_field_sentences_aux(FieldSentences,0,Sentences0),
    append(Sentences0,Sentences).

flatten_field_sentences_aux([],_,[]) :-
    !.
flatten_field_sentences_aux([[Field,STokens]|Rest],Offset,
			[FirstSentences|RestSentences]) :-
    compute_field_length(STokens,0,FieldLength),
    adjust_tokens(STokens,Offset,AdjustedSTokens),
    NewOffset is Offset + FieldLength,
    lower(Field,LCField),
    FirstSentences=[tok(field,Field,LCField,pos(Offset,NewOffset))
		   |AdjustedSTokens],
    flatten_field_sentences_aux(Rest,NewOffset,RestSentences).

compute_field_length([],LengthIn,LengthIn) :-
    !.
compute_field_length([tok(_,_,_,pos(_,End))|Rest],LengthIn,LengthOut) :-
    (End > LengthIn ->
	LengthInOut=End
    ;   LengthInOut=LengthIn
    ),
    compute_field_length(Rest,LengthInOut,LengthOut).

adjust_tokens([],_Offset,[]) :-
    !.
adjust_tokens([tok(Type,Text,LCText,pos(Start,End))|Rest],Offset,
              [tok(Type,Text,LCText,pos(NewStart,NewEnd))|AdjustedRest]) :-
    NewStart is Start + Offset,
    NewEnd is End + Offset,
    adjust_tokens(Rest,Offset,AdjustedRest).


/* add_sentence_labels(+Sentences0, +UI, -Sentences)
   add_sentence_labels(+Sentences0, +UI, Field, +N, -Sentences)

add_sentence_labels/3 adds a label token at the start of each sentence
using add_sentence_labels/5 to keep track of the current Fieeld and N
(sentence number within the field). */

% add_sentence_labels(Sentences0,UI,Sentences) :-
%     add_sentence_labels(Sentences0,UI,"TX",0,Sentences).

add_sentence_labels([],_UI,_Field,_N,[]) :- !.
add_sentence_labels([tok(field,Field,LCField,Pos)|Rest],UI,_OldField,_N,
		    [tok(field,Field,LCField,Pos)
		     |ModifiedRest]) :-
    !,
    add_sentence_labels(Rest,UI,Field,0,ModifiedRest).
add_sentence_labels([tok(sn,Arg2,Arg3,Pos)|Rest],UI,Field,N,
		    [tok(label,Label,LCLabel,Pos),
		     tok(sn,Arg2,Arg3,Pos)
		     |ModifiedRest]) :-
    !,
    NewN is N + 1,
    form_label(UI,Field,NewN,Label,LCLabel),
    add_sentence_labels(Rest,UI,Field,NewN,ModifiedRest).
add_sentence_labels([First|Rest],UI,Field,N,[First|ModifiedRest]) :-
    add_sentence_labels(Rest,UI,Field,N,ModifiedRest).


/* coordinate_sentences(+Sentences, +UI, -CoordinatedSentences)
   coordinate_sentences(+Sentences, +UI, +Field, +N, +OffsetIn, -OffsetOut,
                        -CoordinatedSentences)

coordinate_sentences/3 converts Sentences (having tok/4 terms) into
CoordinatedSentences (having tok/5 terms, the last argument of which is a
position/2 term into Sentences) using the auxiliary coordinate_sentences/7
to form intermediate results which are appended together; it uses UI and
keeps track of the current Field, N (sentence number within the field),
and current offset within a given field.
Besides changing the focus from unexpanded to expanded form,
coordinate_sentences/3 also adds a label token at the start of each sentence. */

coordinate_sentences(Sentences, UI, UDA_AVL, CoordinatedSentences) :-
	coordinate_sentences(Sentences, UI, UDA_AVL, "TX", 0, 0,
			     _OffsetOut, CoordinatedSentences).

coordinate_sentences([], _UI, _UDA_AVL, _Field, _N, OffsetIn, OffsetIn, []).

coordinate_sentences([tok(field,Field,LCField,Pos)|Rest], UI, UDA_AVL, _OldField, N,
		     OffsetIn, OffsetOut,
		     [tok(field,Field,LCField,FieldPos,Pos)|CoordinatedRest]) :-
    !,
    gather_tokens_for_position(Rest,Pos,FieldTokens,NewRest),
    coordinate_sentences(FieldTokens,UI,UDA_AVL,Field,0,OffsetIn,OffsetInOut,
			 CoordinatedFieldSentences),
    FieldPos=pos(OffsetIn,OffsetInOut),
    append(CoordinatedFieldSentences,CoordinatedNewRest,CoordinatedRest),
    coordinate_sentences(NewRest, UI, UDA_AVL, Field, N, OffsetInOut, OffsetOut,
			 CoordinatedNewRest).
coordinate_sentences([tok(label,_Label,_LCLabel,_Pos)|Rest],UI,UDA_AVL,Field,N,
		     OffsetIn,OffsetOut,CoordinatedRest) :-
    % ignore an existing label; a new one will be created from an sn token
    !,
    coordinate_sentences(Rest, UI, UDA_AVL, Field, N, OffsetIn, OffsetOut, CoordinatedRest).
coordinate_sentences([tok(sn,Arg2,Arg3,Pos)|Rest], UI, UDA_AVL, Field, N,
		     OffsetIn,OffsetOut,
		     [tok(label,Label,LCLabel,SentencePos,Pos),
		      tok(sn,Arg2,Arg3,SentencePos,Pos)
		      |CoordinatedRest]) :-
    !,	
    gather_tokens_for_position(Rest,Pos,SentenceTokens,NewRest),
    NewN is N + 1,
    coordinate_sentences(SentenceTokens,UI, UDA_AVL, Field, NewN, OffsetIn, OffsetInOut,
			 CoordinatedSentenceSentences),
    form_label(UI,Field,NewN,Label,LCLabel),
    SentencePos=pos(OffsetIn,OffsetInOut),
    append(CoordinatedSentenceSentences,CoordinatedNewRest,CoordinatedRest),
    coordinate_sentences(NewRest, UI, UDA_AVL, Field, NewN, OffsetInOut, OffsetOut,
			 CoordinatedNewRest).
coordinate_sentences([tok(aadef,_Def,_AA,Pos)|Rest], UI, UDA_AVL, Field, N,
		     OffsetIn, OffsetOut, CoordinatedRest) :-
	!,
	gather_tokens_for_position(Rest, Pos, AADefTokens, NewRest0),
	coordinate_sentences(AADefTokens, UI, UDA_AVL, Field, N,
			     OffsetIn, OffsetInOut, CoordinatedAADefTokens),
	skip_over_ws_parenthetical(NewRest0, NewRest),
	append(CoordinatedAADefTokens, CoordinatedNewRest, CoordinatedRest),
	coordinate_sentences(NewRest, UI, UDA_AVL, Field, N,
			     OffsetInOut, OffsetOut, CoordinatedNewRest).
coordinate_sentences([tok(aa,_AAToks,RelatedAAToks,Pos)|Rest], UI, UDA_AVL, Field, N,
		     OffsetIn, OffsetOut, CoordinatedRest) :-
    !,
    gather_tokens_for_position(Rest,Pos,AATokens,NewRest),
    % use ExpandedTokens, not AATokens (unless there is a problem)
    (RelatedAAToks=[tok(aadef,ExpandedTokens,_,_)|_] ->
	true
    ;   ExpandedTokens=AATokens
    ),
    % temp
%    format('~ncoordinate_sentences (aa):~n',[]),
%    format('RelatedAAToks: ~p~n',[RelatedAAToks]),
%    format('AATokens: ~p~n',[AATokens]),
%    format('ExpandedTokens: ~p~n',[ExpandedTokens]),
    coordinate_sentences(ExpandedTokens, UI, UDA_AVL, Field, N, OffsetIn, OffsetInOut,
			 CoordinatedExpandedTokens0),
%    format('CoordinatedExpandedTokens0: ~p~n',[CoordinatedExpandedTokens0]),
    compute_token_position(AATokens,AATokensPos),
    set_original_positions(CoordinatedExpandedTokens0,AATokensPos,
			   CoordinatedExpandedTokens),
%    format('CoordinatedExpandedTokens: ~p~n',[CoordinatedExpandedTokens]),
    append(CoordinatedExpandedTokens,CoordinatedNewRest,CoordinatedRest),
    coordinate_sentences(NewRest, UI, UDA_AVL, Field, N, OffsetInOut, OffsetOut,
			 CoordinatedNewRest).
coordinate_sentences([tok(Type,Arg2,Arg3,Pos)|Rest], UI, UDA_AVL, Field, N,
		     OffsetIn, OffsetOut,
		     [tok(Type,Arg2,Arg3,TypePos,Pos)|CoordinatedRest]) :-
    % field and label are handled above, so this is for sn and pe
    higher_order_type(Type),
    !,
    gather_tokens_for_position(Rest,Pos,TypeTokens,NewRest0),
    % This handles the pathological case where a parenthetical expression
    % in quotation marks is an AA expansion. Something like "heart attack" (HA)/
    % GROSS.
    conditionally_skip_over_ws_parenthetical(TypeTokens, NewRest0, NewRest),
    coordinate_sentences(TypeTokens, UI, UDA_AVL, Field, N, OffsetIn, OffsetInOut,
			 CoordinatedTypeTokens),
    TypePos=pos(OffsetIn,OffsetInOut),
    append(CoordinatedTypeTokens,CoordinatedNewRest,CoordinatedRest),
    coordinate_sentences(NewRest, UI, UDA_AVL,  Field, N, OffsetInOut, OffsetOut,
			 CoordinatedNewRest).
coordinate_sentences([tok(Type,Text,LCText,Pos)|Rest], UI, UDA_AVL, Field, N,
		     OffsetIn,OffsetOut,
		     [tok(Type,Text,LCText,TypePos,Pos)|CoordinatedRest]) :-
    % Non-higher-order types
    % length(Text,TokenLength),
    % OffsetInOut is OffsetIn + TokenLength,
    set_type_pos(tok(Type,Text,LCText,Pos),
		 UDA_AVL, OffsetIn, OffsetInOut, TypePos),
    %%% Must take into account UDAs here; need to pass in UDA_AVL
    % TypePos=pos(OffsetIn,OffsetInOut),

    % format(user_error, '~p~n', [tok(Type,Text,LCText,Pos)]),
    % format(user_error, '~p~n~n', [tok(Type,Text,LCText,TypePos,Pos)]),
    coordinate_sentences(Rest, UI, UDA_AVL, Field, N, OffsetInOut, OffsetOut, CoordinatedRest).

set_type_pos(Token, UDA_AVL, OffsetIn, OffsetInOut, TypePos) :-
	Token = tok(_Type,Text,_LCText,Pos),
	( avl_member(_UDAShortFormTokenList, UDA_AVL, [UDALongFormTokenList]),	  
	  memberchk(Token, UDALongFormTokenList) ->
	  Token = tok(_Type,Text,_LCText,Pos),
	  TypePos = Pos,
	  Pos = pos(_, OffsetInOut)
	; length(Text, TokenLength),
	  OffsetInOut is OffsetIn + TokenLength,
	  TypePos = pos(OffsetIn, OffsetInOut)
	).

conditionally_skip_over_ws_parenthetical(TypeTokens, NewRest0, NewRest) :-
	( member(Token, TypeTokens),
	  aadef_tok(Token) ->
	  skip_over_ws_parenthetical(NewRest0, NewRest)
	; NewRest = NewRest0
	).

gather_tokens_for_position([First|Rest], Pos, [First|GatheredRest], NewRest) :-
	get_token_position(First, FirstPos),
	position_contains(Pos, FirstPos),
	!,
	gather_tokens_for_position(Rest, Pos, GatheredRest, NewRest).
gather_tokens_for_position(Tokens, _Pos, [], Tokens).

form_label(UI, Field, N, Label, LCLabel) :-
	number_codes(N, NString),
	lower(Field, LCField),
	% use lowercased field for historical reasons
	append([UI,".",LCField,".",NString], Label),
	lower(Label, LCLabel).

set_original_positions([], _, []).
set_original_positions([tok(Type,Arg2,Arg3,Arg4,_Arg5)|Rest], Pos,
		       [tok(Type,Arg2,Arg3,Arg4,Pos)|ModifiedRest]) :-
	set_original_positions(Rest, Pos, ModifiedRest).

% We want to discard an AA/Expansion pair if the Expansion is itself an AA:
% This rule blocks "(E)" as an AA for "EAE" in text like Kate Wendelsdorf's
% PMID- GSE2446
% TI  - Transcriptomic alterations induced by AT-EAE in mouse spinal cord
% AB  - We compared RNA samples extracted from spinal cords of control (C) and
% AT-EAE (E) mice using the "Multiple Yellow" strategy. 
% Both multiple sclerosis (MS) and experimental autoimmune encephalomyelitis (EAE),
% its animal model, ...

% because "EAE", the expansion of "E", is itself an AA.
% The token strings must match other than the posinfo terms.

remove_aa_expansion_overlap([], _AAListIn, []).
remove_aa_expansion_overlap([_AA1-[Expansion1]|Rest], AAListIn, AAListOut) :-
	member(AA2-_Expansion2, AAListIn),
	match_tokens_ignore_posinfo(Expansion1, AA2),
	!,
	remove_aa_expansion_overlap(Rest, AAListIn, AAListOut).
remove_aa_expansion_overlap([AA1-[Expansion1]|Rest], AAListIn, [AA1-[Expansion1]|AAListOut]) :-
	remove_aa_expansion_overlap(Rest, AAListIn, AAListOut).


% Remove any subsequent AAs that share either the AA or the expansion with the first AA

remove_aa_redundancy([], []).
remove_aa_redundancy([Singleton], [Singleton]) :- !.
remove_aa_redundancy([AA-[Def]|Rest], [AA-[Def]|ModifiedRest]) :-
	extract_text_from_tokens(AA, AAText0),
	lower(AAText0, AAText),
	extract_text_from_tokens(Def, DefText0),
	lower(DefText0, DefText),
	remove_each_aa_redundancy(Rest, AAText, DefText, NewRest),
	remove_aa_redundancy(NewRest, ModifiedRest).

remove_each_aa_redundancy([], _, _, []).
remove_each_aa_redundancy([FirstAA-[FirstDef]|Rest], AAText, DefText, ModifiedRest) :-
	extract_text_from_tokens(FirstAA, FirstAAText0),
	lower(FirstAAText0, FirstAAText),
	( FirstAAText == AAText ->
	  true
	; extract_text_from_tokens(FirstDef, FirstDefText0),
	  lower(FirstDefText0, FirstDefText),
	  FirstDefText == DefText
	),	
	!,
	remove_each_aa_redundancy(Rest, AAText, DefText, ModifiedRest).
remove_each_aa_redundancy([First|Rest], AAText, DefText, [First|ModifiedRest]) :-
	remove_each_aa_redundancy(Rest, AAText, DefText, ModifiedRest).

% order_aas/2 orders the aas alphabetically but preferring longer AAs.
% Implement the preference for longer strings by extracting the AA text
% and appending 127 (DEL) to each AA text string, and then performing a normal sort.
% Appending 127 will ensure that "ABCD" will appear before "ABC".
% If we go to UTF8, the 127 should change to 9999 or something else.
order_aas(AAListIn, AAListOut) :-
	augment_aas(AAListIn, AugAAList0),
	sort(AugAAList0, AugAAList),
	deaugment_aas(AugAAList, AAListOut).

augment_aas([], []).
augment_aas([AA-Def|Rest], [AAText-AA-Def|AugmentedRest]) :-
	extract_text_from_tokens(AA, AAText0),
	atom_codes(Atom127, [127]),
	concat_atom([AAText0,Atom127], AAText),
	augment_aas(Rest, AugmentedRest).

deaugment_aas([], []).
deaugment_aas([_AAText-AA-Def|Rest], [AA-Def|DeaugmentedRest]) :-
	deaugment_aas(Rest, DeaugmentedRest).

annotate_with_UDAs([], Sentences, UDA_AVL, UDA_AVL, UDAsUsed, UDAsUsed, Sentences).
annotate_with_UDAs([H|T], Sentences1, UDA_AVL0, UDA_AVL, UDAsUsedIn, UDAsUsedOut, Sentences2) :-
	UDAList = [H|T],
	annotate_with_UDAs_1(Sentences1, UDA_AVL0, UDAList, UDA_AVL,
			     UDAsUsedIn, UDAsUsedOut, Sentences2).

annotate_with_UDAs_1([], AVL, _UDAs, AVL, UDAsUsed, UDAsUsed, []).
annotate_with_UDAs_1([FirstToken|RestTokens], AVLIn, UDAs, AVLOut,
		     UDAsUsedIn, UDAsUsedOut, AnnotatedTokens) :-
	% Don't try to expand UDAs from non-alphanumeric tokens
	( \+ an_tok(FirstToken)	->
	  AVLNext = AVLIn,
	  AnnotatedTokens = [FirstToken|RestAnnotatedTokens],
	  UDAsUsedNext = UDAsUsedIn
	% The first token is alphanumeric, so get its String
	; FirstToken = tok(_TokenType,String,_LCString,pos(StartPos,EndPos)),
	  % Is String one of the UDAs?
	  member(String:ThisExpansion, UDAs) ->
	  UDAsUsedNext = [String:ThisExpansion:StartPos:EndPos|UDAsUsedIn],
	  % UDAShortForm = ThisUDA ->
	  lower(ThisExpansion, LCThisExpansion),
	  tokenize_text_utterly(ThisExpansion, TokenizedThisExpansion),
	  tokenize_text_utterly(LCThisExpansion, LCTokenizedThisExpansion),
	  form_simple_tokens(TokenizedThisExpansion, LCTokenizedThisExpansion,
			     StartPos/EndPos, ExpansionTokens),
	  LastUnwantedToken = '',
	  store_aa([FirstToken], LastUnwantedToken, ExpansionTokens, AVLIn, AVLNext),
	  append(ExpansionTokens, RestAnnotatedTokens, AnnotatedTokens)
	; AVLNext = AVLIn,
	  AnnotatedTokens = [FirstToken|RestAnnotatedTokens],
	  UDAsUsedNext = UDAsUsedIn
	),
	annotate_with_UDAs_1(RestTokens, AVLNext, UDAs, AVLOut,
			     UDAsUsedNext, UDAsUsedOut, RestAnnotatedTokens).


/* annotate_with_aas(+AAs, +SentencesIn, +AAList, -SentencesOut)

annotate_with_aas/3 produces SentencesOut by applying the Acronym/
Abbreviation definitions stored in the AVL tree AAs to SentencesIn.  */

annotate_with_aas([], SentencesIn, SentencesIn).
annotate_with_aas([AA|Rest], SentencesIn, SentencesOut) :-
	annotate_with_one_aa(AA, SentencesIn, SentencesInOut),
	annotate_with_aas(Rest, SentencesInOut, SentencesOut).

annotate_with_one_aa(AA-[Def], SentencesIn, SentencesOut) :-
	compute_token_position(Def, DefPos),
	insert_aa_def(SentencesIn, DefPos, Def, AA, DefToken, SentencesInOut),
	PreviousAATokens = [],
	insert_all_aa(SentencesInOut, PreviousAATokens, AA, DefToken, SentencesOut).

insert_aa_def([], DefPos, Def, AA, NewToken, [NewToken]) :-
	NewToken = tok(aadef,Def,AA,DefPos).
insert_aa_def([First|Rest], DefPos, Def, AA, NewToken, SentencesOut) :-
	First = tok(_,_,_,FirstPos),
	position_ge(FirstPos, DefPos),
	!,
	NewToken = tok(aadef,Def,AA,DefPos),
	SentencesOut = [NewToken,First|Rest].
insert_aa_def([First|Rest], DefPos, Def, AA, NewToken, [First|ModifiedRest]) :-
	insert_aa_def(Rest, DefPos, Def, AA, NewToken, ModifiedRest).

% We need to accumulate aa tokens as they're encountered,
% in order to ensure that no matching token Match has already participated
% in the expansion of an AA. This strategy handles cases such as PMID 22114065, which includes
% (1) liquid chromatography-mass spectrometry (LC-MS),
% (2) mass spectrometry (MS/MS), and
% (3) LC-MS/MS.
% After "LC-MS" is expanded using AA (1),
% we don't want "MS" to be re-used in the expansion of AA (2).

insert_all_aa([], _PreviousAATokens, _AA, _DefToken, []).
% If we see an aa token, add it to PreviousAATokens, and add it to SentencesOut as well.
insert_all_aa([FirstToken|RestTokens], PreviousAATokens, AA, DefToken, [FirstToken|SentencesOut]) :-
	aa_tok(FirstToken),
	!,
	insert_all_aa(RestTokens, [FirstToken|PreviousAATokens], AA, DefToken, SentencesOut).
insert_all_aa([FirstToken|RestTokens], PreviousAATokens, AA, DefToken, SentencesOut) :-
	\+ already_expanded_token(FirstToken, PreviousAATokens),
	match_tokens_ignore_ws_ho(AA, [FirstToken|RestTokens], Match, _),
	compute_token_position(Match, MatchPos),
	!,
	NewToken = tok(aa, Match, [DefToken], MatchPos),
	SentencesOut = [NewToken,FirstToken|ModifiedRestTokens],
	insert_all_aa(RestTokens, PreviousAATokens, AA, DefToken, ModifiedRestTokens).
insert_all_aa([FirstToken|RestTokens], PreviousAATokens, AA, DefToken, [FirstToken|ModifiedRestTokens]) :-
	insert_all_aa(RestTokens, PreviousAATokens, AA, DefToken, ModifiedRestTokens).

already_expanded_token(TestToken, PreviousAATokens) :-
	member(AA, PreviousAATokens),
	AA = tok(aa, AADefnTokens),
	memberchk(TestToken, AADefnTokens),
	!.

% Test if the two token lists are identical other than the PosInfo fields.
% This predicate is only for 4-argument tokens, e.g.,
% tok(uc,"E","e",pos(8,9))
% tok(uc,"EAE","eae",pos(3,6))

match_tokens_ignore_posinfo([], []).
match_tokens_ignore_posinfo([H1|T1], [H2|T2]) :-
	token_template(H1, TokenType1, TokenString1, LCTokenString1, _PosInfo1),
	token_template(H2, TokenType2, TokenString2, LCTokenString2, _PosInfo2),
	TokenType1     == TokenType2,
	TokenString1   == TokenString2,
	LCTokenString1 == LCTokenString2,
	match_tokens_ignore_posinfo(T1, T2).

/* match_tokens_ignore_ws_ho(+Pattern, +Tokens, -Match, -RestTokens)

match_tokens_ignore_ws_ho/4 matches the tokens in Pattern with those in Tokens
producing Match and RestTokens. Matching is done respecting case. The
match must begin with the first token in Tokens which MUST NOT be wsho
(whitespace or higher-order); otherwise, wsho tokens are ignored.  */

match_tokens_ignore_ws_ho(AATokens, [FirstSentenceToken|RestSentenceTokens],
			  Match, RestTokens) :-
	\+ ws_tok(FirstSentenceToken),
	\+ higher_order_tok(FirstSentenceToken),
	match_tokens_ignore_ws_ho_aux(AATokens, [FirstSentenceToken|RestSentenceTokens],
				      Match, RestTokens).

%    First=tok(Type,_,_,_),
%    ((Type==ws; higher_order_type(Type)) ->
%        !,
%        fail
%    ;   match_tokens_ignore_ws_ho_aux(Pattern,[First|Rest],Match,RestTokens)
%    ).

match_tokens_ignore_ws_ho_aux([], SentenceTokens, [], SentenceTokens).
match_tokens_ignore_ws_ho_aux([tok(_,Text,_,_)|RestAATokens],
			      [tok(Type,Text,LCText,Pos)|RestSentences],
			      [tok(Type,Text,LCText,Pos)|RestMatch],
			      SentencesOut) :-
	!,
	match_tokens_ignore_ws_ho_aux(RestAATokens, RestSentences, RestMatch, SentencesOut).
% First Sentence token is ws, so ignore it
match_tokens_ignore_ws_ho_aux([FirstAAToken|RestAATokens],
			      [FirstSentenceToken|RestSentenceTokens],
			      Match, SentencesOut) :-
	ws_tok(FirstSentenceToken),
	!,
	match_tokens_ignore_ws_ho_aux([FirstAAToken|RestAATokens], RestSentenceTokens,
				      Match, SentencesOut).
% % First AA token is ws, so ignore it
% match_tokens_ignore_ws_ho_aux([FirstAAToken|RestAATokens],
% 			      [FirstSentenceToken|RestSentenceTokens],
% 			      Match, SentencesOut) :-
% 	ws_tok(FirstAAToken),
% 	!,
% 	match_tokens_ignore_ws_ho_aux(RestAATokens, [FirstSentenceToken|RestSentenceTokens],
%				      Match, SentencesOut).

% Must comment out this clause, because it allows
% splitting an acronym across sentences!!
% match_tokens_ignore_ws_ho_aux(Pattern,[tok(Type,_,_,_)|Rest],Match,SentencesOut) :-
%     higher_order_type(Type),
%     !,
%     match_tokens_ignore_ws_ho_aux(Pattern,Rest,Match,SentencesOut).


get_UDAs(UDAs) :-
	( control_value('UDA', FileName),
	  check_valid_file_type(FileName, 'User-defined AA') ->
	  send_message('~NReading default AAs from file ~w~n', [FileName]),
	  open(FileName, read, InputStream),
	  create_UDAs(InputStream, 'UDA', UDAs),
	  close(InputStream)
	; UDAs = []
	).

% Type is either 'NoMap' or 'UDA'
create_UDAs(InputStream, Type, UDAs) :-
	% read in the contents of the UDA file
	get_skr_text_2(InputStream, UDALines),
	sort(UDALines, SortedUDALines),
	(  foreach(Line, SortedUDALines),
	   fromto(Strings, S0, S, [])
	   % remove all leading and trailing blanks from the entire line
	do trim_whitespace(Line, TrimmedLine),
	   TrimmedLine = [FirstChar|_Rest],
	     % skip comment lines, whose first printing char is '#'
	   ( FirstChar == 0'# ->
	     S0 = S
	   ; S0 = [TrimmedLine|S]
	   )
	),
	split_UDA_and_NoMap_pairs(Type, Strings, UDAs).

split_UDA_and_NoMap_pairs('UDA', Strings, UDAs) :-
	(  foreach(String, Strings),
	   foreach(UDA,    UDAs)
	do get_UDA_short_and_long_forms(String, AACodes, ExpansionCodes),	   
	   UDA = AACodes:ExpansionCodes
	).

split_UDA_and_NoMap_pairs('NoMap', Strings, NoMapPairs) :-
	(  foreach(String,    Strings),
	   foreach(NoMapPair, NoMapPairs)
	do get_NoMap_string_and_CUI(String, StringCodes, CUICodes),
	   NoMapPair = StringCodes:CUICodes
	).

get_NoMap_string_and_CUI(String, StringCodes, CUICodes) :-
	% The NoMapString and the CUI can appear in either order,
	% i.e., NoMapString|CUI or CUI|NoMapString;
	% take the one that looks like a CUI as the CUI!
	( split_string_completely(String, "|", [Before,After]) ->
	  true
	; fatal_error('Each data line in NoMap file must contain exactly one "|" char!~n', []),
	  stop_and_halt
	),
	( looks_like_CUI(Before) ->
	  CUICodes = Before,
	  StringCodes = After
	; CUICodes = After,
	  StringCodes = Before
	).	

looks_like_CUI(String) :-
	append("C", Digits, String),
	all_digits(Digits).

get_UDA_short_and_long_forms(String, AACodes, ExpansionCodes) :-
	% The UDA and the expansion can appear in either order,
	% i.e., UDA|Expansion or Expansion|UDA;
	% take the shorter one to be the UDA, and the longer to be the expansion
	trim_whitespace(String, TrimmedString),
	( TrimmedString = "|" ->
	  Before = [],
	  After = []
	; split_string_completely(TrimmedString, "|", [Before,After]) ->
	  true
	; fatal_error('Each data line in UDA file must contain exactly one "|" char!~n', []),
	  stop_and_halt
	),
	trim_and_compress_whitespace(Before, TrimmedBefore),
	trim_and_compress_whitespace(After, TrimmedAfter),
	determine_UDA_and_expansion(TrimmedBefore, TrimmedAfter, AACodes, ExpansionCodes).

% Use the same code to create NoMap pairs as for creating UDAs!
create_nomap_pairs(InputStream, StringNoMapPairs) :-
	create_UDAs(InputStream, 'NoMap', StringNoMapPairs).

% The shorter string is deemed to be the AA, and the longer, the expansion,
% unless either is zero length, in which case declare an error.

determine_UDA_and_expansion(BeforeString, AfterString, Shorter, Longer) :-
	( BeforeString == [] ->
	  fatal_error('UDA file cannot contain empty UDA or Expansion!~n', [])
	; AfterString == [] ->
	  fatal_error('UDA file cannot contain empty UDA or Expansion!~n', [])
	; length(BeforeString, BeforeLength),
	  length(AfterString, AfterLength),
	  ( BeforeLength < AfterLength ->
	    Shorter = BeforeString,
	    Longer  = AfterString
	  ; Shorter = AfterString,
	    Longer  = BeforeString
	  )
	).

% We need to remove from the UDA any UDA that is
% (1) itself an author-defined AA, or
% (2) part of an author-defined AA expansion
resolve_AA_conflicts([], UDAs, UDAs).
resolve_AA_conflicts([FirstAA|RestAAs], UDAsIn, UDAsOut) :-
	resolve_one_AA_conflict(UDAsIn, FirstAA, UDAsNext),
	resolve_AA_conflicts(RestAAs, UDAsNext, UDAsOut).

resolve_one_AA_conflict([], _AuthorDefinedAA, []).
resolve_one_AA_conflict([FirstUDA|RestUDAs], AuthorDefinedAA, UDAsOut) :-
	( aa_conflict_exists(FirstUDA, AuthorDefinedAA) ->
	  UDAsOut = RestUDAsOut
	; UDAsOut = [FirstUDA|RestUDAsOut]
	),
	resolve_one_AA_conflict(RestUDAs, AuthorDefinedAA, RestUDAsOut).

% A UDA conflicts with an author-defined AA if the UDA
% * is itself an author-defined AA, or
% * a word in the expansion of an author-defined AA
aa_conflict_exists(UDA:_UDAExpansion, AATokens-[AAExpansion]) :-
	lower(UDA, LCUDA),
	( member(tok(TokenType,_Text,LCText,_Pos), AATokens)
	; member(tok(TokenType,_Text,LCText,_Pos), AAExpansion)
	),
	% must not succeed on whitespace tokens!
	an_type(TokenType),
	LCText = LCUDA,
	!.

section_header('ANIMALS').
section_header('AVAILABILITY').
section_header('BACKGROUND').
section_header('CASE').
section_header('CLINICAL').
section_header('CONCLUSION').
section_header('CONCLUSIONS').
section_header('CONTEXT').
section_header('DATA').
section_header('DESIGN').
section_header('DEVELOPMENT').
section_header('DISCUSSION').
section_header('EXPERIMENTAL').
section_header('FINDINGS').
section_header('HYPOTHESIS').
section_header('IMPLICATIONS').
section_header('INTERPRETATION').
section_header('INTERVENTION').
section_header('INTERVENTIONS').
section_header('INTRODUCTION').
section_header('LIMITATIONS').
section_header('MAIN').
section_header('MATERIALS').
section_header('MEASUREMENTS').
section_header('MEASURES').
section_header('METHOD').
section_header('METHODOLOGY').
section_header('METHODS').
section_header('MOTIVATION').
section_header('OBJECT').
section_header('OBJECTIVE').
section_header('OBJECTIVES').
section_header('OUTCOME').
section_header('PARTICIPANTS').
section_header('PATIENTS').
section_header('POPULATION').
section_header('PROBLEM').
section_header('PROCEDURE').
section_header('PURPOSE').
section_header('RATIONALE').
section_header('RATIONALE').
section_header('RECENT').
section_header('RELEVANCE').
section_header('RESEARCH').
section_header('RESULT').
section_header('RESULTS').
section_header('SAMPLE').
section_header('SEARCH').
section_header('SELECTION').
section_header('SETTING').
section_header('SIGNIFICANCE').
section_header('STATEMENT').
section_header('STUDY').
section_header('SUBJECTS').
section_header('SUMMARY').


number_word(1, "one").
number_word(2, "two").
number_word(3, "three").
number_word(4, "four").
number_word(5, "five").
number_word(6, "six").
number_word(7, "seven").
number_word(8, "eight").
number_word(9, "nine").
number_word(10, "ten").
number_word(11, "eleven").
number_word(12, "twelve").
number_word(13, "thirteen").
number_word(14, "fourteen").
number_word(15, "fifteen").
number_word(16, "sixteen").
number_word(17, "seventeen").
number_word(18, "eighteen").
number_word(19, "nineteen").
number_word(20, "twenty").
% number_word(21, "twenty-one").
% number_word(22, "twenty-two").
% number_word(23, "twenty-three").
% number_word(24, "twenty-four").
% number_word(25, "twenty-five").
% number_word(26, "twenty-six").
% number_word(27, "twenty-seven").
% number_word(28, "twenty-eight").
% number_word(29, "twenty-nine").
% number_word(30, "thirty").
% number_word(31, "thirty-one").
% number_word(32, "thirty-two").
% number_word(33, "thirty-three").
% number_word(34, "thirty-four").
% number_word(35, "thirty-five").
% number_word(36, "thirty-six").
% number_word(37, "thirty-seven").
% number_word(38, "thirty-eight").
% number_word(39, "thirty-nine").
% number_word(40, "forty").
% number_word(41, "forty-one").
% number_word(42, "forty-two").
% number_word(43, "forty-three").
% number_word(44, "forty-four").
% number_word(45, "forty-five").
% number_word(46, "forty-six").
% number_word(47, "forty-seven").
% number_word(48, "forty-eight").
% number_word(49, "forty-nine").
% number_word(50, "fifty").
% number_word(51, "fifty-one").
% number_word(52, "fifty-two").
% number_word(53, "fifty-three").
% number_word(54, "fifty-four").
% number_word(55, "fifty-five").
% number_word(56, "fifty-six").
% number_word(57, "fifty-seven").
% number_word(58, "fifty-eight").
% number_word(59, "fifty-nine").
% number_word(60, "sixty").
% number_word(61, "sixty-one").
% number_word(62, "sixty-two").
% number_word(63, "sixty-three").
% number_word(64, "sixty-four").
% number_word(65, "sixty-five").
% number_word(66, "sixty-six").
% number_word(67, "sixty-seven").
% number_word(68, "sixty-eight").
% number_word(69, "sixty-nine").
% number_word(70, "seventy").
% number_word(71, "seventy-one").
% number_word(72, "seventy-two").
% number_word(73, "seventy-three").
% number_word(74, "seventy-four").
% number_word(75, "seventy-five").
% number_word(76, "seventy-six").
% number_word(77, "seventy-seven").
% number_word(78, "seventy-eight").
% number_word(79, "seventy-nine").
% number_word(80, "eighty").
% number_word(81, "eighty-one").
% number_word(82, "eighty-two").
% number_word(83, "eighty-three").
% number_word(84, "eighty-four").
% number_word(85, "eighty-five").
% number_word(86, "eighty-six").
% number_word(87, "eighty-seven").
% number_word(88, "eighty-eight").
% number_word(89, "eighty-nine").
% number_word(90, "ninety").
% number_word(91, "ninety-one").
% number_word(92, "ninety-two").
% number_word(93, "ninety-three").
% number_word(94, "ninety-four").
% number_word(95, "ninety-five").
% number_word(96, "ninety-six").
% number_word(97, "ninety-seven").
% number_word(98, "ninety-eight").
% number_word(99, "ninety-nine").
