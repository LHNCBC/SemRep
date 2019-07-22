
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

% File:	    nls_text.pl
% Module:   NLS Text
% Author:   Lan
% Purpose:  Provide simple text (atom) processing

:- module(nls_text,[
	concatenate_text/3,
	% OBSOLETE
	% eliminate_multiple_meaning_designator/2,
	is_all_graphic_text/1,
	string_uninvert/2
   ]).


:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	is_integer_string/1,
	split_string_backtrack/4,
	split_string_completely/3,
	trim_whitespace/2
    ]).


:- use_module(skr_lib(sicstus_utils), [
	concat_atom/2,
	concat_strings_with_separator/3
   ]).


:- use_module(library(lists), [
	reverse/2
   ]).

:- use_module(library(lists),[
    append/2
    ]).

/* append_text(+TextList, ?Text)

append_text/2 is analogous to append/2 for text (atoms) except that at least
one of its arguments MUST be instantiated.*/

append_text([], '') :- !.
append_text([Text], Text) :- !.
append_text(TextList, Text) :-
	concat_atom(TextList, Text).

/* concatenate_text(+TextList, +InsertText, -Text)

concatenate_text/3 forms Text, the concatenation of text in TextList
interposed with InsertText (often ' ').  */

concatenate_text([], _, '') :- !.
concatenate_text([Text], _, Text) :- !.
concatenate_text([First|Rest], InsertText, Text) :-
	concatenate_text_aux(Rest, InsertText, First, Text).

concatenate_text_aux([], _, TextIn, TextIn).
concatenate_text_aux([First|Rest], InsertText, TextIn, TextOut) :-
	append_text([TextIn,InsertText,First], TextInOut),
	concatenate_text_aux(Rest, InsertText, TextInOut, TextOut).

/* eliminate_multiple_meaning_designator(+Word, -ModifiedWord)

eliminate_multiple_meaning_designator/2 removes an expression of the form
<n> where n is an integer from Word (an atom) producing ModifiedWord.  */


% OBSOLETE
% eliminate_multiple_meaning_designator(Word, ModifiedWord) :-
% 	( atom_codes(Word, WordString),
% 	  split_string_backtrack(WordString, "<", Base, A1),
% 	  split_string_backtrack(A1, ">", Integer, Tail),
% 	  is_integer_string(Integer),
% 	  trim_whitespace(Tail, "") ->
% 	  trim_whitespace(Base, ModifiedWordString),
% 	  atom_codes(ModifiedWord, ModifiedWordString)
% 	; ModifiedWord = Word
% 	).

/* is_all_graphic_text(+Text)
   is_graphic_text(+Text)

is_all_graphic_text/1 succeeds if Text is an atom (INCLUDING '') consisting
entirely of graphic characters. */

is_all_graphic_text('') :- !.
is_all_graphic_text(Text) :-
	atom_codes(Text, S),
	is_all_graphic(S).

is_all_graphic([]).
is_all_graphic([Char|Rest]) :-
	is_graphic(Char),
	is_all_graphic(Rest).

is_graphic(0'!).  % see ctypes:is_graph/1 for non-alphanumeric graphics
is_graphic(0'").  % " This comment is just to un-confuse Emacs
is_graphic(0'#).
is_graphic(0'$).
is_graphic(0'%).
is_graphic(0'&).
is_graphic(0'\').
is_graphic(0'().
is_graphic(0')).
is_graphic(0'*).
is_graphic(0';).
is_graphic(0'<).
is_graphic(0'=).
is_graphic(0'>).
is_graphic(0'?).
is_graphic(0'@).
is_graphic(0'[).
is_graphic(0'\\).
is_graphic(0']).
is_graphic(0'^).
is_graphic(0'_).
is_graphic(0'`).
is_graphic(0'{).
is_graphic(0'|).
is_graphic(0'}).
is_graphic(0'~).
is_graphic(0'+).
is_graphic(0',).
is_graphic(0'-).
is_graphic(0'.).
is_graphic(0'/).
is_graphic(0':).

% Recursively uninvert a string, i.e., injury, abdominal ==> abdominal injury.
% More generally, "S1, S2, ..., Sn" ==> Sn, ..., S2, S1".
% This is now implemented in pure prolog, thereby allowing us to retire lexical.c!

string_uninvert(String, UninvertedString) :-
	split_string_completely(String, ", ", SplitString),
	reverse(SplitString, ReverseSplitString),
	concat_strings_with_separator(ReverseSplitString, " ", UninvertedString).
