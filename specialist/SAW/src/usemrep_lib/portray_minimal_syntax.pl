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

% File:	    portray_minimal_syntax.pl
% Module:   semrep
% Author:   FML
% Purpose:  Pretty-printing output of perform_categorial_analysis/9
%           and perform_referential_analysis/3

% ----- Module declaration and exported predicates

:- module(portray_minimal_syntax, [
		portray_minimal_syntax_structure/1
   ]).

:- use_module( library(lists), [
		is_list/1
   ]).

:- use_module( skr_lib(sicstus_utils), [
		concat_atom/2
   ]).

% DEFINITIONS:
% (1) A syntax element is a term of the form head/1, verb/1, mod/1, conj/1, punc/1, aux/1, etc.
%     E.g., the following three terms are syntax elements:
%      * punc([inputmatch([,]), tokens([])])
%      * aux([lexmatch([are]), inputmatch([are]), tag(aux), tokens([are])])
%      * head([lexmatch([asthma]), inputmatch([asthma]), tag(noun),
%              tokens([asthma]), bases([]), metaconc(['Asthma':[dsyn]])])])
% (2) An MSU is a list of syntax elements.
% (3) An MSU list is a list of MSUs (duhhh).
% (4) A  bracketed MSU list is a list of MSU lists.
% (4) A minimal_syntax_structure, the top-level structure, is either
%       * a bracketed MSU list, or
%       * an MSU list

% portray_minimal_syntax_structure(_) :- !, fail.

portray_minimal_syntax_structure(Var) :-
	var(Var),
	!,
	write(_).
portray_minimal_syntax_structure(minimal_syntax(List)) :-
	!,
	format('~n~w~n', ['minimal_syntax(']),
	portray_minimal_syntax_structure(List),
	format('~n~w~n', [')']).
portray_minimal_syntax_structure([]) :-
	write('[]').
portray_minimal_syntax_structure([H|T]) :-
	( var(H) ->
	  write(_)
	; valid_minimal_syntax_structure([H|T], Type) ->
	  portray_minimal_syntax_structure_wrapper([H|T], Type)
	; write([H|T])
	).

% Ensure, before passing a list to the pretty-printer,
% that the list is indeed a Semrep syntax structure.
valid_minimal_syntax_structure(Structure, Type) :-
	( valid_coord_list(Structure) ->
	  Type = 'Coord'
	; valid_MSU(Structure) ->
	  Type = 'MSU'
	; valid_MSU_list(Structure) ->
	  Type = 'MSU_list'
	; valid_bracketed_MSU_list(Structure) ->
	  Type = bracketed_MSU_list
	).

valid_bracketed_MSU_list(List) :-
	var(List),
	!.
valid_bracketed_MSU_list([]).
valid_bracketed_MSU_list([H|T]) :-
	valid_MSU_list(H),
	valid_bracketed_MSU_list(T).

valid_MSU_list(List) :-
	var(List),
	!.
valid_MSU_list([]).
valid_MSU_list([H|T]) :-
	valid_MSU(H),
	valid_MSU_list(T).

valid_MSU(MSU) :-
	var(MSU),
	!.
valid_MSU([]).
valid_MSU([H|T]) :-
	valid_MSU_element(H),
	valid_MSU(T).

valid_MSU_element(Element) :-
	functor(Element, Functor, _Arity),
	valid_MSU_element_functor(Functor).

valid_coord_list(L) :-
	var(L),
	!.
valid_coord_list([]).
valid_coord_list([H|T]) :-
	valid_coord_structure(H),
	valid_coord_list(T).

% all_nil_list([]).
% all_nil_list([[]|T]) :-
% 	all_nil_list(T).

valid_coord_structure(coord(_Conj,_Type,_Index,_LConjList,_RConj, [])).
valid_coord_structure([]).

% in alphabetical order
valid_MSU_element_functor(disorder).
valid_MSU_element_functor(genphenom).

valid_MSU_element_functor(adv).
valid_MSU_element_functor(aux).
valid_MSU_element_functor(compl).
valid_MSU_element_functor(confid).
valid_MSU_element_functor(conj).
valid_MSU_element_functor(det).
valid_MSU_element_functor(empty_head).
valid_MSU_element_functor(empty_mod).
valid_MSU_element_functor(empty_prep).
valid_MSU_element_functor(head).
valid_MSU_element_functor(index).
valid_MSU_element_functor(ing).
valid_MSU_element_functor(mod).
valid_MSU_element_functor(modal).
valid_MSU_element_functor(msu_index).
valid_MSU_element_functor(msu_token_list).
valid_MSU_element_functor(prep).
valid_MSU_element_functor(pastpart).
valid_MSU_element_functor(princpart).
valid_MSU_element_functor(pron).
valid_MSU_element_functor(punc).
valid_MSU_element_functor(role).
valid_MSU_element_functor(shapes).
valid_MSU_element_functor(verb).

% in typical order in which these appear in actual terms
valid_MSU_element_functor(usemtype).
valid_MSU_element_functor(ausemtype).
valid_MSU_element_functor(semgroup).
valid_MSU_element_functor(lexmatch).
valid_MSU_element_functor(inputmatch).
valid_MSU_element_functor(tag).
valid_MSU_element_functor(tokens).
valid_MSU_element_functor(bases).
valid_MSU_element_functor(metaconc).

portray_minimal_syntax_structure_wrapper(List, Type) :-
	( nl,
	  portray_syntax_structure_type(List, Type) ->
	  true
	; format('~n~n### portray_minimal_syntax_structure FAILED!!~n~n', [])
	).

% This predicate is called on the very top-level list,
% whose elements are sub-lists. Those sub-lists contain syntax elements,
% perhaps after removing one layer of list brackets.
portray_syntax_structure_type([H|T], Type) :-
	format('~a~n', ['[']),
	portray_syntax_structure_type_1(Type, [H|T]),
	format('~a~n', [']']).

portray_syntax_structure_type_1(bracketed_MSU_list, L) :-
	portray_bracketed_MSU_list(L, 1).
portray_syntax_structure_type_1('MSU_list', L) :-
	portray_MSU_list(L, 1, 1).
portray_syntax_structure_type_1('MSU', L) :-
	portray_MSU(L, '1').
portray_syntax_structure_type_1('Coord', L) :-
	portray_coord_list(L, 1, 1).
portray_syntax_structure_type_1('AllNil', [H|T]) :-
	write_nil_list(T, H).

write_nil_list([], []) :-
	format('~q~n', [[]]).
write_nil_list([H|T], []) :-
	format('~q, ', [[]]),
	write_nil_list(T, H).

portray_coord_list([], _Position, _SubPosition).
portray_coord_list([H|T], Position, SubPosition) :-
	portray_coord_structure(H, Position, SubPosition),
	nl,
	NextPosition is Position + 1,
	portray_coord_list(T, NextPosition, SubPosition).

% portray_coord_list([FirstCoord, SecondCoord], Position, SubPosition) :-
% 	portray_coord_structure(FirstCoord, Position, SubPosition),
% 	nl,
% 	NextPosition is Position + 1,
% 	portray_coord_structure(SecondCoord, NextPosition, SubPosition).

portray_coord_structure([], Position, SubPosition) :-
	assemble_position_indicator(Position, SubPosition, PositionIndicator),
	display_indented_position_indicator(PositionIndicator, 0, _IndentedPositionLength),
	PositionM1 is Position - 1,
	format('~*c~w~n', [PositionM1, 32, []]).

portray_coord_structure(coord(Conj, Type, Index, LeftConjunct, RightConjunct, []),
			Position, SubPosition) :-
	assemble_position_indicator(Position, SubPosition, PositionIndicator),
	display_indented_position_indicator(PositionIndicator, 1, IndentedPositionLength),
	atom_codes(Conj, ConjName),
	length(ConjName, ConjNameLength),
	Indent is IndentedPositionLength + ConjNameLength + 9,
	IndentM3 is Indent - 3,
	format('~*ccoord(~w,~q,~q,~n', [Position, 32, Conj, Type, Index]),
	format('~*c~w', [IndentM3, 32, '[']),
	portray_one_conjunct(LeftConjunct,  Indent, 1),
	format('~*c~w~n', [IndentM3, 32, ']']),
	format('~*c~w', [IndentM3, 32, '[']),
	portray_one_conjunct_element(RightConjunct, Indent),
	% format('~n~*c~w', [Indent, 32, []]),
	format('~n~*c~w~n', [IndentM3, 32, ']']),
	format('~n~*c ~a <~a~n', [IndentedPositionLength, 32, ']', PositionIndicator]).
	
portray_one_conjunct([], _PositionIndicator, _Index).
portray_one_conjunct([H|T], PositionIndicator, Index) :-
	IndentM2 is PositionIndicator - 2,
	choose_indent(Index, IndentM2, Indent),
	format('~*c~w', [Indent, 32, '[']),
	portray_one_conjunct_element(H, PositionIndicator),
	format('~n~*c~w~n', [IndentM2, 32, ']']),
	NextIndex is Index + 1,
	portray_one_conjunct(T, PositionIndicator, NextIndex).

choose_indent(Index, IndentM3, Indent) :-
	( Index =:= 1 ->
	  Indent is 0
	; Indent is IndentM3
	).

portray_one_conjunct_element([H|T], PositionIndicator) :-
	!,
	format_one_MSU_element_argument_list_member(H, PositionIndicator),
	portray_one_conjunct_element(T, PositionIndicator).
portray_one_conjunct_element(Element, PositionIndicator) :-
	format_one_MSU_element_argument_list_member(Element, PositionIndicator).

portray_bracketed_MSU_list([], _Position).
portray_bracketed_MSU_list([H|T], Position) :-
	portray_MSU_list(H, Position, 1),
	NextPosition is Position + 1,
	portray_bracketed_MSU_list(T, NextPosition).

portray_MSU_list(Var, Position, SubPosition) :-
	var(Var),
	!,
	assemble_position_indicator(Position, SubPosition, PositionIndicator),
	atom_length(PositionIndicator, AtomLength),
	default_indent(Default),
	Indent is AtomLength + Default + 1,
	format('~n~*c | __VAR!!__~n~n', [Indent, 32]).
portray_MSU_list([], _Position, _SubPosition).
portray_MSU_list([H|T], Position, SubPosition) :-
	assemble_position_indicator(Position, SubPosition, PositionIndicator),
	portray_MSU(H, PositionIndicator),
	NextSubPosition is SubPosition + 1,
	portray_MSU_list(T, Position, NextSubPosition).

%atom_length(Atom, AtomLength) :-
%	atom_codes(Atom, String),
%	length(String, AtomLength).

assemble_position_indicator(Position, SubPosition, PositionIndicator) :-
	concat_atom([Position, '.', SubPosition], PositionIndicator).

% This predicate is called on the MSUs,
% i.e., the lowest level lists, whose members are syntax elements.

portray_MSU(Var, PositionIndicator) :-
	var(Var),
	!,
	atom_length(PositionIndicator, AtomLength),
	default_indent(Default),
	Indent is AtomLength + Default + 1,
	format('~n~*c | __VAR!!__~n~n', [Indent, 32]).
portray_MSU([], _PositionIndicator) :- !.
portray_MSU([H|T], PositionIndicator) :-
	!,
	display_indented_position_indicator(PositionIndicator, 1, IndentedPositionLength),
	portray_MSU_aux([H|T], 1, IndentedPositionLength),
	format('~n~*c ~a <~a~n~n', [IndentedPositionLength, 32, ']', PositionIndicator]).
portray_MSU(OtherStruct, PositionIndicator) :-
	display_indented_position_indicator(PositionIndicator, 1, IndentedPositionLength),
	portray_MSU_element(OtherStruct, 1, IndentedPositionLength),
	format('~n~*c ~a <~a~n~n', [IndentedPositionLength, 32, ']', PositionIndicator]).

display_indented_position_indicator(PositionIndicator, PrintBracket, IndentedPositionLength) :-
	determine_bracket_to_print(PrintBracket, BracketToPrint),
	default_indent(Default),
	format('~*c~a> ~a', [Default, 32, PositionIndicator, BracketToPrint]),
	atom_codes(PositionIndicator, PositionChars),
	length(PositionChars, PositionLength),
	IndentedPositionLength is PositionLength + 2.

determine_bracket_to_print(0, '').
determine_bracket_to_print(1, '[').

% Now do the actual formatting, now that we know the desired indent postion.
portray_MSU_aux([], _Position, _PosLength).
portray_MSU_aux([H|T], Position, PosLength) :-
	portray_MSU_element(H, Position, PosLength),
	NextPosition is Position + 1,
	portray_MSU_aux(T, NextPosition, PosLength).

% This predicate called on individual terms that are syntax elements.
% PosLength is the length of the marker "1", "2", etc. that
% indicates the position of the sublist in the top-level list

% Terms are printed as follows:
%
% 2> [ head([
%            lexmatch([benefits])
%            inputmatch([benefits])
%            tag(noun)
%            tokens([benefits])
%            bases([])
%            metaconc([benefits:[qnco]])
%          ])
%

%%% List is [NormGeneID,NormGeneName,GeneName,GenPhenomAtom,GeneFunctionList],
portray_MSU_element(genphenom(MinIndex, MaxIndex, List, MetaConc, SemTypeList),
		    Position, PosLength) :-

	!,
	portray_MSU_element(genphenom([MinIndex, MaxIndex, List, MetaConc, SemTypeList]),
			    Position, PosLength).


portray_MSU_element(disorder(MinIndex, MaxIndex, DisorderAtom, MetaConc, SemTypeList),
		    Position, PosLength) :-
	!,
	portray_MSU_element(disorder([MinIndex, MaxIndex, DisorderAtom, MetaConc, SemTypeList]),
	       Position, PosLength).

portray_MSU_element(msu_token_list(MinIndex, MaxIndex,ModifiedTokenList),
		    Position, PosLength) :-
	portray_MSU_element(msu_token_list([MinIndex,MaxIndex,ModifiedTokenList]),
			    Position, PosLength).

portray_MSU_element(Term, Position, PosLength) :-
	functor(Term, Functor, _Arity),
	arg(1, Term, Arg),
	atom_codes(Functor, FunctorChars),
	length(FunctorChars, FunctorLength),
	default_indent(Default),
	set_functor_indent(Position, Default, PosLength, FunctorIndent),
	Indent2 is Default + FunctorLength + PosLength + 4,
	Indent2M2 is Indent2 - 2,
	format_functor(Position, FunctorIndent, Functor),
	( is_list(Arg),
	  Arg = [H|T] ->
	  format_one_MSU_element_argument_list_member(H, Indent2),
	  format_rest_MSU_element_argument_list_members(T, Indent2),
 	  format('~n~*c~a', [Indent2M2, 32, '])'])
	; format('~w)', [Arg])
	).

% If the syntax element (e.g., head(_)) is the first in its list,
% it will be preceeded on the line by something like "2> ["
% as in the example above; in that case, indent only one space
% after the "[".

% If the syntax element is NOT the first in its list,
% it will NOT be preceeded by, e.g., "2> [",
% and so should be indented by
%   Default indentation
% + PosLength (the length of the index, e.g., "2", which has length 1)
% + 2 for the "[(" appearing after the functor ("head" or whatever)

set_functor_indent(Position, Default, PosLength, FunctorIndent) :-
	( Position =:= 1 ->
	  FunctorIndent is 1
	; FunctorIndent is Default + PosLength + 2
	).

% Format the functor of a syntax element;
% always indent FunctorIndent spaces,
% but print a <CR> if the syntax element's position is not 1.
format_functor(Position, FunctorIndent, Functor) :-
	( Position =:= 1 ->
	  FormatString =   '~*c~a(['
	; FormatString = '~n~*c~a(['
	),				  
	format(FormatString, [FunctorIndent, 32, Functor]).	

% This predicate formats the second through last elements
% of the list that is the argument of syntax elements,
% i.e., arity-1 terms whose functor is head, verb, mod, etc.
% E.g., for the syntax element
%       aux([lexmatch([are]), inputmatch([are]), tag(aux), tokens([are])])
% this predicate would format inputmatch([are]), tag(aux), and tokens([are])
format_rest_MSU_element_argument_list_members([], _Indent).
format_rest_MSU_element_argument_list_members([H|T], Indent) :-
	format_one_MSU_element_argument_list_member(H, Indent),
	format_rest_MSU_element_argument_list_members(T, Indent).

% This predicate formats a member of the list that is the argument
% of syntax elements, i.e., arity-1 terms whose functor is head, verb, mod, etc.
% It is called on terms like ausemtype/1, usemtype/1, metaconc/1, semgroup/1,
% lexmatch/1, inputmatch/1, tag/1, tokens/1, bases/1, etc.
format_one_MSU_element_argument_list_member(ausemtype([H|T]), Indent) :-
	!,
	format('~n~*causemtype([~q', [Indent,32,H]),
	ListIndent is Indent + 11,
	format_rest_type_or_metaconc_list_members(T, ListIndent),
	format('~a', ['])']).
format_one_MSU_element_argument_list_member(metaconc([H|T]), Indent) :-
	!,
	format('~n~*cmetaconc([~q', [Indent,32,H]),
	ListIndent is Indent + 10,
	format_rest_type_or_metaconc_list_members(T, ListIndent),
	format('~a', ['])']).
format_one_MSU_element_argument_list_member(usemtype([H|T]), Indent) :-
	!,
	format('~n~*cusemtype([~q', [Indent,32,H]),
	ListIndent is Indent + 10,
	format_rest_type_or_metaconc_list_members(T, ListIndent),
	format('~a', ['])']).
format_one_MSU_element_argument_list_member(Arg, Indent) :-
	format('~n~*c~w', [Indent,32,Arg]).

% This predicate is used to format the second through last members
% of the list that is the argument of ausemtype/1, usemtype/1, or metaconc/1.
% These terms are formatted as follows:
%
%            usemtype(['Health Care Activity'
%                      'Functional Concept'])
% or
%            metaconc(['Assessment procedure (procedure)':[hlca]
%                      'Evaluation':[ftcn]]) ])
%

format_rest_type_or_metaconc_list_members([], _Indent).
format_rest_type_or_metaconc_list_members([H|T], Indent) :-
	format('~n~*c~q', [Indent, 32, H]),
	format_rest_type_or_metaconc_list_members(T, Indent).

% The default indentation for pretty-printing
default_indent(1).
