%----------------------------------------------------------------------
% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  The Zinc Modelling Tools for ECLiPSe
% The Initial Developer of the Original Code is  Joachim Schimpf
% with support from Cisco Systems and NICTA Victoria.
% Portions created by the Initial Developer are
% Copyright (C) 2007 Cisco Systems, Inc.  All Rights Reserved.
% 
% Contributor(s): Joachim Schimpf (initial development)
%                 Magnus Ågren (SICStus Prolog adaptation)
% END LICENSE BLOCK
%----------------------------------------------------------------------

% TODO:
% - Handle all integer and float expressions in tokenizer.
% - Check and possibly improve error handling (better position information?).

% Parser for FlatZinc 1.0

:- module(flatzinc_parser, [read_item/2, read_items/2]).

:- use_module(library(lists)).
:- use_module(library('zinc/zinc_utils')).

read_items(Stream, Terms) :-
	(   read_item(Stream, T)
	->  read_items(Stream, TS),
	    Terms = [T|TS]
	;   Terms = []
	).

%----------------------------------------------------------------------
% Parser - creates terms that are very similar to the source
%----------------------------------------------------------------------

read_item(Stream, Term) :-
	tokenize_item(Stream, Tokens, EndOfFile),
	(   EndOfFile==true
	->  (   Tokens==[] -> fail
	    ;   illarg_syntax('end of file'-[], read_item(Stream, Term), 1)
	    )
	;   Tokens==[]
	->  read_item(Stream, Term)
	;   item(Term, Tokens, [;]) -> true
	;   illarg_syntax(''-[], read_item(Stream, Term), 1)
	).


% Items --------------------------------

item(decl(var, Type, Ident, Anns, Value)) -->
	[var], !,
	non_array_ti_expr_tail(Type),
	expect(:),
	ident_anns(Ident, Anns),
	(   [=]
	->  non_array_flat_expr(Value0),
	    {Value = [Value0]}
	;   {Value = []}
	).
item(decl(par, Type, Ident, Anns, Value)) -->
	non_array_ti_expr_tail(Type),
	[:], !,
	ident_anns(Ident, Anns),
	expect(=),
	non_array_flat_expr(Value).
item(decl(vararray, range(1, Max), ElemType, Ident, Anns, Value)) -->
	[array],
	expect_list(['[',i(1),..]),
	int_literal(Max),
	expect_list([']',of]),
	[var], !,
	non_array_ti_expr_tail(ElemType),
	expect(:),
	ident_anns(Ident, Anns),
	(   [=]
	->  array_literal(Value0),
	    {Value = [Value0]}
	;   {Value = []}
	).
item(decl(pararray, range(1, Max), ElemType, Ident, Anns, Value)) -->
	[array], !,
	expect_list(['[',i(1),..]),
	int_literal(Max),
	expect_list([']',of]),
	non_array_ti_expr_tail(ElemType),
	expect(:),
	ident_anns(Ident, Anns),
	expect(=),
	array_literal(Value).


item(constraint(Elem, Anns)) -->
	[constraint], !,
	constraint_elem(Elem),
	annotations(Anns).

item(solve(Anns, Kind)) -->
	[solve], !,
	annotations(Anns), %!!! ordinary annotations should suffice
	solve_kind(Kind).

item(predicate(Elem)) -->
	[predicate], !,
	predicate_elem(Elem).

constraint_elem(Elem) -->
	[ident(Ident),'('], !,
	nonempty_flat_expr_list(Params),
	{Elem =.. [Ident|Params]}.
constraint_elem(Elem) -->
	variable_expr(Elem).

predicate_elem(Elem) -->
	[ident(Ident)],
	expect('('),
	nonempty_pred_decl_arg_list(Params),
	{Elem =.. [Ident|Params]},
	expect(')').

nonempty_pred_decl_arg_list([E|Es]) -->
	pred_decl_arg(E),
	(   [',']
	->  nonempty_pred_decl_arg_list(Es)
	;   {Es = []}
	).

pred_decl_arg(pred_arg(Ident, Type)) -->
	non_array_ti_expr_tail(Type), !,
	expect(:),
	[ident(Ident)].
pred_decl_arg(pred_arg(var(Ident), Type)) -->
	[var], !,
	non_array_ti_expr_tail(Type),
	expect(:),
	[ident(Ident)].
pred_decl_arg(pred_arg(var(Ident), array(Index, Type))) -->
	[array],
	expect('['),
	pred_arg_array_index(Index),
	expect_list([']', of]),
	[var], !,
	non_array_ti_expr_tail(Type),
	expect(:),
	[ident(Ident)].
pred_decl_arg(pred_arg(Ident, array(Index, Type))) -->
	[array],
	expect('['),
	pred_arg_array_index(Index),
	expect_list([']', of]),
	non_array_ti_expr_tail(Type),
	expect(:),
	[ident(Ident)].

pred_arg_array_index(sup) -->
	[int], !.
pred_arg_array_index(Max) -->
	expect_list([i(1),..]),
	int_literal(Max).

nonempty_flat_expr_list([E|Es]) -->
	flat_expr(E),
	(   [',']
	->  flat_expr_list(Es)
	;   expect(')'),
	    {Es = []}
	).

flat_expr_list(Es) -->
	(   [')']
	->  {Es = []}
	;   nonempty_flat_expr_list(Es)
	).

solve_kind(satisfy) -->
	[satisfy], !.
solve_kind(minimize(Expr)) -->
	[minimize], !,
	constraint_elem(Expr).
solve_kind(maximize(Expr)) -->
	[maximize], !,
	constraint_elem(Expr).


% Type-Inst --------------------------------

non_array_ti_expr_tail(set(Type)) -->
	[set], !,
	expect('of'),
	scalar_ti_expr_tail(Type).
non_array_ti_expr_tail(Type) -->
	scalar_ti_expr_tail(Type).

scalar_ti_expr_tail(bool) -->
	[bool], !.
scalar_ti_expr_tail(int) -->
	[int], !.
scalar_ti_expr_tail(float) -->
	[float], !.
scalar_ti_expr_tail({}(Ints)) -->
	['{'], !,
	nonempty_int_list(Ints).
scalar_ti_expr_tail(range(Min, Max)) -->
	int_literal(Min), !,
	expect(..),
	int_literal(Max).
scalar_ti_expr_tail(range(Min, Max)) -->
	float_literal(Min), !,
	expect(..),
	float_literal(Max).

nonempty_int_list([E|Es]) -->
	int_literal(E),
	(   [',']
	->  int_list(Es)
	;   expect('}'), {Es = []}
	).

int_list(Es) -->
	(   ['}']
	->  {Es = []}
	;   nonempty_int_list(Es)
	).



% Expressions --------------------------------
% Rules have been reordered such that cuts do not cut valid alternatives
% (i.e. rules that match a prefix of another rule must come later).

flat_expr(Expr) -->
	non_array_flat_expr(Expr), !.
flat_expr(Expr) -->
	array_literal(Expr).


non_array_flat_expr(Expr) -->
	set_literal(Expr), !.
non_array_flat_expr(Expr) -->
	scalar_flat_expr(Expr).


scalar_flat_expr(Expr) -->
	bool_literal(Expr), !.
scalar_flat_expr(Expr) -->
	float_literal(Expr), !.
scalar_flat_expr(Expr) -->
	int_literal(Expr), !.
scalar_flat_expr(Expr) -->
	[str(Expr)], !.
scalar_flat_expr(Expr) -->
	array_access_expr(Expr), !.
scalar_flat_expr(Expr) -->
	[ident(Expr)].


int_flat_expr(Expr) -->
	int_literal(Expr), !.
int_flat_expr(Expr) -->
	array_access_expr(Expr), !.
int_flat_expr(Expr) -->
	[ident(Expr)].


variable_expr(Expr) -->
	array_access_expr(Expr), !.
variable_expr(Expr) -->
	[ident(Expr)].


array_access_expr('_subscript'(Ident, Index)) -->
	[ident(Ident),'['],
	int_index_expr(Index),
	expect(']').

int_index_expr(Expr) -->
	[ident(Expr)], !.
int_index_expr(Expr) -->
	int_literal(Expr).


bool_literal(false) --> [false], !.
bool_literal(true) --> [true].


int_literal(SignedInt) -->
	(   [-]
	->  [i(Int)],
	    {SignedInt is -Int}
	;   [i(Int)],
	    {SignedInt = Int}
	).


float_literal(SignedFloat) -->
	(   [-]
	->  [f(Float)],
	    {SignedFloat is -Float}
	;   [f(Float)],
	    {SignedFloat = Float}
	).


set_literal({}(List)) -->
	['{'], !,
	sfe_list(List).
set_literal(range(Min, Max)) -->
	int_flat_expr(Min),
	[..], !,
	int_flat_expr(Max).

nonempty_sfe_list([E|Es]) -->
	scalar_flat_expr(E),
	(   [',']
	->  sfe_list(Es)
	;   expect('}'),
	    {Es = []}
	).

sfe_list(Es) -->
	(   ['}']
	->  {Es = []}
	;   nonempty_sfe_list(Es)
	).


array_literal([Array]) -->
	['['], !,
	nafe_list(Array).

nonempty_nafe_list([E|Es]) -->
	non_array_flat_expr(E),
	(   [',']
	->  nafe_list(Es)
	;   expect(']'),
	    {Es = []}
	).

nafe_list(Es) -->
	(   [']']
	->  {Es = []}
	;   nonempty_nafe_list(Es)
	).

% Annotations --------------------------------

ident_anns(Ident, Anns) -->
	[ident(Ident)],
	annotations(Anns).

annotations(Anns) -->
	(   [::]
	->  annotations_tail(Anns)
	;   {Anns = []}).

annotations_tail(Anns) -->
	annotation(Ann),
	(   [::]
	->  { Anns = [Ann|Anns1] },
	    annotations_tail(Anns1)
	;   { Anns = [Ann] }
	).

%!!! the first two clauses are hacks due to bugs in the flatzinc bnf
annotation(Ann) -->
	[ident(seq_search)], !,
	expect_list(['(', '[']),
	annotation_array(Anns),
	{Ann = seq_search(Anns)},
	expect(')').
annotation(Ann) -->
	[ident(viz)], !,
	expect_list(['(', '[']),
	annotation_array(Anns),
	{Ann = viz(Anns)},
	expect(')').
annotation(Ann) -->
	[ident(Ident)],
	(   ['(']
	->  nonempty_ann_expr_list(Params),
	    {Ann =.. [Ident|Params]}
	;   {Ann = Ident}
	).


%!!! this predicate is a hack due to a bug in the flatzinc bnf
annotation_array(Anns) -->
	(   [']']
	->  {Anns = []}
	;   annotation(Ann),
	    (   [',']
	    ->  annotation_array(Anns1)
	    ;   expect(']'),
		{Anns1 = []}
	    ),
	    {Anns = [Ann|Anns1]}
	).

nonempty_ann_expr_list([E|Es]) -->
	ann_expr(E),
	(   [',']
	->  nonempty_ann_expr_list(Es)
	;   expect(')'),
	    {Es = []}
	).

ann_expr(Ann) -->
	(   [ident(Ident), '(']
	->  nonempty_ann_expr_list(Params),
	    {Ann =.. [Ident|Params]}
	;   flat_expr(Ann)
	).

% Auxiliaries ------------------------------

expect_list([]) --> [].
expect_list([Token|Tokens]) -->
	expect(Token),
	expect_list(Tokens).

expect(E) -->
	[T],
	(   {T == E}
	->  []
	;   {illarg_syntax('expected \`~q\' but found \`~q\''-[E, T],
			   expect(E), 1)
	    }
	).

%----------------------------------------------------------------------
% Tokenizer
% Tokenize the input stream up until the next semicolon (or eof).
% Return a list of tokens, not including the terminating semicolon.
% On end-of-file, fail.
%----------------------------------------------------------------------

tokenize_item(Stream, Tokens, EndOfFile) :-
	read_item_codes(Stream, Codes, EndOfFile),
	(   tokenize_codes(Codes, Tokens) -> true
	;   illarg_syntax(''-[], tokenize_item(Stream, Tokens), 1)
	).


read_item_codes(Stream, Codes, EndOfFile) :-
	read_line(Stream, Raw),
	(   Raw = end_of_file
	->  Codes = [], EndOfFile = true
	;   Raw = [0'% | _]
	->  read_item_codes(Stream, Codes, EndOfFile)
	;   Codes = Raw, EndOfFile = false
	).

tokenize_codes([], []) :- !.
tokenize_codes([C|Codes], Tokens) :-
	token(C, T1, Codes, Rest), !,
	append(T1, Ts, Tokens),
	tokenize_codes(Rest, Ts).

token(0'\", [str(S)]) -->
	in_string(S),
	[0'\"].
token(0'{, ['{']) --> [].
token(0'}, ['}']) --> [].
token(0'(, ['(']) --> [].
token(0'), [')']) --> [].
token(0'[, ['[']) --> [].
token(0'], [']']) --> [].
token(0',, [',']) --> [].
token(0'., ['..']) --> ".".
token(0':, ['::']) --> ":".
token(0':, [':']) --> [].
token(0';, [';']) --> [].
token(0'=, ['=']) --> [].
token(0'-, ['-']) --> [].
token(C, []) -->
	{C =< 0' }.
token(D0, [f(F)]) -->
	{digit(D0)},
	in_number(DS0),
	[0'.], [D1],
	{digit(D1)},
	in_number(DS1),
	{append([D0|DS0], [0'.,D1|DS1], F0)},
	(   [0'e,0'-,D2], {digit(D2)}, in_number(DS2)
	->  {append(F0, [0'e,0'-,D2|DS2], F1)}
	;   [0'e,0'+,D2], {digit(D2)}, in_number(DS2)
	->  {append(F0, [0'e,0'+,D2|DS2], F1)}
	;   {F0 = F1}
	),
	{number_codes(F, F1)}.
token(D, [i(I)]) -->
	{digit(D)},
	in_number(DS),
	{number_codes(I, [D|DS])}.
token(S, [Token]) -->
	{letter(S)},
	in_id(SS),
	{atom_codes(Id,[S|SS])},
	{keyword(Id) -> Token = Id ; Token = ident(Id)}.

in_string([C|Cs]) --> [C],
	{C \== 0'\", C \== 0'\n}, !,
	in_string(Cs).
in_string([]) --> [].

in_id([S|SS]) --> [S],
	{alphanum(S)}, !,
	in_id(SS).
in_id([]) --> [].

in_number([D|DS]) --> [D],
	{digit(D)}, !,
	in_number(DS).
in_number([]) --> [].

digit(0'0).
digit(0'1).
digit(0'2).
digit(0'3).
digit(0'4).
digit(0'5).
digit(0'6).
digit(0'7).
digit(0'8).
digit(0'9).

letter(0'A).
letter(0'B).
letter(0'C).
letter(0'D).
letter(0'E).
letter(0'F).
letter(0'G).
letter(0'H).
letter(0'I).
letter(0'J).
letter(0'K).
letter(0'L).
letter(0'M).
letter(0'N).
letter(0'O).
letter(0'P).
letter(0'Q).
letter(0'R).
letter(0'S).
letter(0'T).
letter(0'U).
letter(0'V).
letter(0'W).
letter(0'X).
letter(0'Y).
letter(0'Z).
letter(0'a).
letter(0'b).
letter(0'c).
letter(0'd).
letter(0'e).
letter(0'f).
letter(0'g).
letter(0'h).
letter(0'i).
letter(0'j).
letter(0'k).
letter(0'l).
letter(0'm).
letter(0'n).
letter(0'o).
letter(0'p).
letter(0'q).
letter(0'r).
letter(0's).
letter(0't).
letter(0'u).
letter(0'v).
letter(0'w).
letter(0'x).
letter(0'y).
letter(0'z).

alphanum(0'0).
alphanum(0'1).
alphanum(0'2).
alphanum(0'3).
alphanum(0'4).
alphanum(0'5).
alphanum(0'6).
alphanum(0'7).
alphanum(0'8).
alphanum(0'9).
alphanum(0'A).
alphanum(0'B).
alphanum(0'C).
alphanum(0'D).
alphanum(0'E).
alphanum(0'F).
alphanum(0'G).
alphanum(0'H).
alphanum(0'I).
alphanum(0'J).
alphanum(0'K).
alphanum(0'L).
alphanum(0'M).
alphanum(0'N).
alphanum(0'O).
alphanum(0'P).
alphanum(0'Q).
alphanum(0'R).
alphanum(0'S).
alphanum(0'T).
alphanum(0'U).
alphanum(0'V).
alphanum(0'W).
alphanum(0'X).
alphanum(0'Y).
alphanum(0'Z).
alphanum(0'a).
alphanum(0'b).
alphanum(0'c).
alphanum(0'd).
alphanum(0'e).
alphanum(0'f).
alphanum(0'g).
alphanum(0'h).
alphanum(0'i).
alphanum(0'j).
alphanum(0'k).
alphanum(0'l).
alphanum(0'm).
alphanum(0'n).
alphanum(0'o).
alphanum(0'p).
alphanum(0'q).
alphanum(0'r).
alphanum(0's).
alphanum(0't).
alphanum(0'u).
alphanum(0'v).
alphanum(0'w).
alphanum(0'x).
alphanum(0'y).
alphanum(0'z).
alphanum(0'_).

keyword(annotation).
keyword(any).
keyword(array).
keyword(bool).
keyword(case).
keyword(constraint).
keyword(diff).
keyword(div).
keyword(elseif).
keyword(else).
keyword(endif).
keyword(enum).
keyword(false).
keyword(float).
keyword(function).
keyword(if).
keyword(include).
keyword(intersect).
keyword(int).
keyword(in).
keyword(let).
keyword(list).
keyword(maximize).
keyword(minimize).
keyword(mod).
keyword(not).
keyword(of).
keyword(satisfy).
keyword(subset).
keyword(superset).
keyword(output).
keyword(par).
keyword(predicate).
keyword(record).
keyword(set).
keyword(solve).
keyword(string).
keyword(symdiff).
keyword(test).
keyword(then).
keyword(true).
keyword(tuple).
keyword(union).
keyword(type).
keyword(var).
keyword(where).
keyword(xor).

