
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

% File:     text_object_util.pl
% Module:   Text Object Utilities
% Author:   Lan
% Purpose:  To provide low-level, definitional support for text objects


:- module(text_object_util,[
	aa_tok/1,
	aadef_tok/1,
	alpha_type/1,
	an_tok/1,
	an_pn_tok/1,
	an_pn_xx_tok/1,
	an_type/1,
	annotation_type/1,
	at_an_tok/1,
	at_ws_or_exlb_tok/1,
	at_ws_tok/1,
	at_ws_or_pn_tok/1,
	brackets/2,
	break_punc/1,
	contains_alpha_tok/1,
	ex_lbracket/1,
	ex_lbracket_char/1,
	ex_lbracket_tok/1,
	ex_rbracket_char/1,
	ex_rbracket_tok/1,
	field_or_label_type/1,
	gather_whitespace/3,
	higher_order_or_annotation_type/1,
	higher_order_or_annotation_tok/1,
	higher_order_tok/1,
	higher_order_type/1,
	hyphen_or_slash_tok/1,
	hyphen_punc/1,
	hyphen_punc_char/1,
	ic_tok/1,
	lbracket/1,
	lbracket_tok/1,
	label_tok/1,
	lc_tok/1,
	multi_brackets/2,
	ne_lbracket_tok/1,
	ne_rbracket_tok/1,
	nu_tok/1,
	pe_tok/1,
	pn_tok/1,
	punc_tok/1,
	rbracket/1,
	rbracket_tok/1,
	sentence_punc/1,
	sentence_punc_tok/1,
	utterance_punc/1,
	utterance_punc_tok/1,
	token_sequence_length/4,
	uc_tok/1,
	ws_or_pn_tok/1,
	ws_tok/1,
	ws_char/1
    ]).

alpha_type(lc).
alpha_type(uc).
alpha_type(ic).
alpha_type(mc).
alpha_type(an).

an_type(lc).
an_type(uc).
an_type(ic).
an_type(mc).
an_type(an).
an_type(nu).
an_type(xx).

pn_type(pn).

xx_type(xx).

field_or_label_type(field).
field_or_label_type(label).

higher_order_type(field).
higher_order_type(label).
higher_order_type(sn).
higher_order_type(pe).

annotation_type(aa).
annotation_type(aadef).

higher_order_or_annotation_type(Type) :-
	( higher_order_type(Type) ->
	  true
	; annotation_type(Type)
	).

at_ws_or_pn_tok([Token|_]) :-
	( ws_tok(Token) ->
	  true
	; pn_tok(Token)
	).


at_ws_tok([]).
% at_ws_tok([tok(ws,_,_,_)|_]).
at_ws_tok([Token|_]) :- ws_tok(Token).

at_ws_or_exlb_tok([]).
at_ws_or_exlb_tok([tok(ws,_,_,_)|_]) :- !.
at_ws_or_exlb_tok([tok(pn,LB,LB,_)|_]) :- ex_lbracket(LB),
    !.

at_an_tok([]).
at_an_tok([Token|_]) :- an_tok(Token).

gather_whitespace([],[],[]) :- !.
gather_whitespace([First|Rest],[First|RestWS],NewRest) :-
    ws_tok(First),
    !,
    gather_whitespace(Rest,RestWS,NewRest).
gather_whitespace(Tokens,[],Tokens).

ws_tok(tok(ws,_,_,_)).
ws_tok(tok(ws,_,_,_,_)).

pn_tok(tok(pn,_,_,_)).
pn_tok(tok(pn,_,_,_,_)).


aa_tok(tok(aa,_,_,_)).

aadef_tok(tok(aadef,_,_,_)).

lc_tok(tok(lc,_,_,_)).

label_tok(tok(label,_,_,_)).
label_tok(tok(label,_,_,_,_)).


ic_tok(tok(ic,_,_,_)).

nu_tok(tok(nu,_,_,_)).

pe_tok(tok(pe,_,_,_)).
pe_tok(tok(pe,_,_,_,_)).

uc_tok(tok(uc,_,_,_)).

% 'an' is overloaded: It can stand for
% (1) broadly alphanumeric, meaning the union of all tokens
% containing alphabetic and numeric strings
% (i.e., lc, uc, ic, mc, an, and nu), and
% (2) narrowly alphanumeric, meaning a specific an token type,
% in which the individual string itself is alphanumeric.
% 
% an_tok/1 tests for a token that is broadly alphanumeric
% (i.e., any one of lc, uc, ic, mc, an, and nu),

an_tok(tok(Type,_,_,_)) :- an_type(Type).
% The arity-5 tok is needed here for merge_sentences_aux/5 in text_objects.pl
an_tok(tok(Type,_,_,_,_)) :- an_type(Type).


an_pn_xx_tok(tok(Type,_,_,_)) :-
	( an_type(Type) ->
	  true
	; pn_type(Type) ->
	  true
	; xx_type(Type)
	).	



an_pn_tok(tok(Type,_,_,_)) :-
	( an_type(Type) ->
	  true
	; pn_type(Type)
	).
% The arity-5 tok is needed here for merge_sentences_aux/5 in text_objects.pl
an_pn_tok(tok(Type,_,_,_,_)) :-
	( an_type(Type) ->
	  true
	; pn_type(Type)
	).


% contains_alpha_tok([]) :-
%     !,
%     fail.
contains_alpha_tok([tok(Type,_,_,_)|_]) :-
	alpha_type(Type),
	!.
contains_alpha_tok([_|Rest]) :- contains_alpha_tok(Rest).

annotation_tok(tok(Type,_,_,_)) :- annotation_type(Type).
annotation_tok(tok(Type,_,_,_,_)) :- annotation_type(Type).

higher_order_tok(tok(Type,_,_,_)) :- higher_order_type(Type).
higher_order_tok(tok(Type,_,_,_,_)) :- higher_order_type(Type).

higher_order_or_annotation_tok(Tok) :-
	( higher_order_tok(Tok) ->
	  true
	; annotation_tok(Tok)
	).

punc_tok(Tok) :- pn_tok(Tok).

hyphen_or_slash_tok(tok(pn,_String, LCString,_PosInfo)) :-
	( hyphen_punc(LCString) ->
	  true
	; slash_punc(LCString)
	).

hyphen_punc(String) :-
	String = [Char],
	hyphen_punc_char(Char).

slash_punc(String) :-
	String = [Char],
	slash_punc_char(Char).

% '-'
% hyphen_punc_char(45).
hyphen_punc_char(0'-).

slash_punc_char(0'/).

sentence_punc_tok(tok(pn,SP,SP,_)) :- sentence_punc(SP).

sentence_punc(String) :-
	String = [Char],
	sentence_punc_char(Char).

% '!'
% sentence_punc_char(33).
sentence_punc_char(0'!).
% '.'
% sentence_punc_char(46).
sentence_punc_char(0'.).
% ';'
% sentence_punc_char(59).
% Disabled for SemRep 01/12/2016
% sentence_punc_char(0';).
% '?'
% sentence_punc_char(63).
sentence_punc_char(0'?).

utterance_punc_tok(tok(pn,SP,SP,_)) :- utterance_punc(SP).

utterance_punc(String) :-
	String = [Char],
	utterance_punc_char(Char).

% ';'
% utterance_punc_char(59).
utterance_punc_char(0';).

% utterance_punc(";").

% tab
ws_char(9).
% newline
ws_char(10).
% blank space
ws_char(32).

break_punc(String) :-
	String = [Char],
	break_punc_char(Char).

% ','
% break_punc_char(44).
break_punc_char(0',).
% ':'
% break_punc_char(58).
break_punc_char(0':).

% break_punc(",").
% break_punc(":").

% any
lbracket_tok(tok(pn,LB,LB,_Pos)) :- lbracket(LB).

% exclusive
ex_lbracket_tok(tok(pn,LB,LB,_Pos)) :- ex_lbracket(LB).
ex_lbracket_tok(tok(pn,LB,LB,_Pos1,_Pos2)) :- ex_lbracket(LB).

% non-exclusive
ne_lbracket_tok(tok(pn,LB,LB,_)) :- ne_lbracket(LB).

% any
rbracket_tok(tok(pn,RB,RB,_)) :- rbracket(RB).

% exclusive
ex_rbracket_tok(tok(pn,RB,RB,_Pos)) :- ex_rbracket(RB).
ex_rbracket_tok(tok(pn,RB,RB,_Pos1,_Pos2)) :- ex_rbracket(RB).

% non-exclusive
ne_rbracket_tok(tok(pn,RB,RB,_)) :- ne_rbracket(RB).

ws_or_pn_tok(Tok) :-
        ( ws_tok(Tok) ->
          true
        ; pn_tok(Tok)
        ).

brackets(LB, RB) :-
	LB = [LChar],
	RB = [RChar],
	brackets_chars(LChar, RChar).

% '(', ')'
% brackets_chars(40, 41).
brackets_chars(0'(, 0')).
% '[', ']'
% brackets_chars(91, 93).
brackets_chars(0'[, 0']).
% '''', ''''
% brackets_chars(39, 39).
brackets_chars(0''', 0''').
% '"', '"'
% brackets_chars(34, 34).
brackets_chars(0'", 0'").
% '"', ':'; for Elhill abominations
% brackets_chars(34, 58).
brackets_chars(0'", 0':). %"

% brackets("(",")").
% brackets("[","]").
% brackets("'","'").
% brackets("""","""").
% brackets("""",":"). % for Elhill abominations

multi_brackets(LB, RB) :-
	LB = [LChar],
	RB = [RChar],
	multi_brackets_chars(LChar, RChar).

% '(', ']'
% multi_brackets_chars(40, 93).
multi_brackets_chars(0'(, 0']).
multi_brackets_chars(0'[, 0')).

% multi_brackets("(","]").

lbracket(LB) :-
	brackets(LB, _),
	!.

rbracket(RB) :-
	brackets(_, RB),
	!.

% exclusive bracket (i.e., it's used only for bracketing)

ex_lbracket(LB) :-
	LB = [LChar],
	ex_lbracket_char(LChar).

% '('
% ex_lbracket_char(40).
ex_lbracket_char(0'().
% '['
% ex_lbracket_char(91).
ex_lbracket_char(0'[).

% ex_lbracket("(").
% ex_lbracket("[").

ex_rbracket(RB) :-
	RB = [RChar],
	ex_rbracket_char(RChar).

% ')'
% ex_rbracket_char(41).
ex_rbracket_char(0')).
% ']'
% ex_rbracket_char(93).
ex_rbracket_char(0']).

% ex_rbracket(")").
% ex_rbracket("]").

% non-exclusive left bracket (i.e., it's used in non-bracketing ways)

ne_lbracket(NELB) :-
	NELB = [NELBChar],
	ne_lbracket_char(NELBChar).

% ''''
% ne_lbracket_char(39).
ne_lbracket_char(0''').
% '"'
% ne_lbracket_char(34).
ne_lbracket_char(0'"). %"

% ne_lbracket("'").
% ne_lbracket("""").

ne_rbracket(NERB) :-
	NERB = [NERBChar],
	ne_rbracket_char(NERBChar).

% ''''
% ne_rbracket_char(39).
ne_rbracket_char(0''').
% '"'
% ne_rbracket_char(34).
ne_rbracket_char(0'"). %"
% ':'
% ne_rbracket_char(58).
ne_rbracket_char(0':).

% ne_rbracket("'").
% ne_rbracket("""").
% ne_rbracket(":").

token_sequence_length([], LastToken, StartPos, TokensLength) :-
	arg(4, LastToken, pos(_, EndPos)),
	TokensLength is EndPos - StartPos.
token_sequence_length([NextToken|RestTokens], _ThisToken, StartPos, TokensLength) :-
	token_sequence_length(RestTokens, NextToken, StartPos, TokensLength).

