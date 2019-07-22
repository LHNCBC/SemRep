
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

% File:	    sicstus_utils.pl
% Module:   SICStus Utils
% Author:   FML
% Purpose:  Provide compatibility with Quintus Prolog library predicates

% Quintus and SICStus Prolog provide different libraries.
% This file contains code for various Quintus Prolog
% library predicates that can be implemented in pure SICStus Prolog.

:- module(sicstus_utils, [
	% must be exported for SemRep
	atoms_to_strings/2,
	can_open_file/2,
	concat_atom/2,
	concat_atom/3,
	concat_strings_with_separator/3,
  	interleave_string/3,
	lower/2,
	lower_chars/2,
	lowercase_list/2,
	midstring/4,
	midstring/5,
	midstring/6,
	replist/3,
	string_char/3,
	string_size/2,
	subchars/4,
	sublist/2,
	substring/4,
	ttyflush/0,
	upper/2,
	upper_list/2,
	with_input_from_chars/3
   ]).


:- use_module(metamap(metamap_tokenization), [
	local_to_lower/2,
	local_to_upper/2
    ]).

% :- use_module(skr_lib(ctypes), [
% 	to_lower/2,
%	to_upper/2
%    ]).

:- use_module(library(codesio), [
	open_codes_stream/2
    ]).


:- use_module(library(lists), [
	append/2,
	sublist/3
    ]).


can_open_file(RelativeFileName, Mode) :-
	absolute_file_name(RelativeFileName, _AbsoluteFileName, [access(Mode)]).

concat_atom([], '').
concat_atom([H|T], Atom) :-
	concat_atoms_1(T, H, Atom).

concat_atoms_1([], H, H).
concat_atoms_1([Next|Rest], First, Atom) :-
	atom_concat(First, Next, Head),
	concat_atoms_1(Rest, Head, Atom).

concat_strings_with_separator([], _Separator, []).
concat_strings_with_separator([H|T], Separator, ConcatString) :-
	interleave_string([H|T], Separator, Interleaved),
	append(Interleaved, ConcatString).

interleave_string([], _InterleaveString, []).
interleave_string([H|T], InterleaveString, InterleavedList) :-
        interleave_string_1(T, H, InterleaveString, InterleavedList).

interleave_string_1([], H, _InterleaveString, [H]).
interleave_string_1([Next|T], H, InterleaveString, [H,InterleaveString|InterleavedT]) :-
        interleave_string_1(T, Next, InterleaveString, InterleavedT).

concat_atom(AtomList, SeparatorAtom, ConcatAtom) :-
	atoms_to_strings(AtomList, StringList),
	atom_codes(SeparatorAtom, SeparatorString),
	concat_strings_with_separator(StringList, SeparatorString, ConcatString),
	atom_codes(ConcatAtom, ConcatString).

atoms_to_strings([], []).
atoms_to_strings([FirstAtom|RestAtoms], [FirstString|RestStrings]) :-
	atom_codes(FirstAtom, FirstString),
	atoms_to_strings(RestAtoms, RestStrings).		


%   lower(+Text, ?Lower)
%   converts an atom, [XQP] string, or non-empty list of character codes
%   Text to lower case.  Lower and Text are the same type of term.

lower(Text, Lower) :-
        (   atom(Text) ->
                atom_codes(Text, TextChars),
                lower_chars(TextChars, LowerChars),
                atom_codes(Lower, LowerChars)
        ;   /* maybe a list of character codes */
                lower_chars(Text, Lower)
        ).

lower_chars([], []).
lower_chars([T|Ts], [U|Us]) :-
        local_to_lower(T, U),
        lower_chars(Ts, Us).

/* lowercase_list(+TextList, -LowercaseTextList)
lowercase_list/2 creates LowercaseTextList, the list of lowercase
strings/atoms corresponding to those in TextList. */

lowercase_list([], []).
lowercase_list([First|Rest], [LCFirst|LCRest]) :-
	lower(First, LCFirst),
	lowercase_list(Rest, LCRest).


%   upper(+Text, ?Upper)
%   converts an atom, [XQP] string, or non-empty list of character codes
%   Text to upper case.  Upper and Text are the same type of term.

upper_list([], []).
upper_list([H|T], [UPPERH|UPPERT]) :-
	upper(H, UPPERH),
	upper_list(T, UPPERT).

upper(Text, Upper) :-
        (   atom(Text) ->
                atom_codes(Text, TextChars),
                upper_chars(TextChars, UpperChars),
                atom_codes(Upper, UpperChars)
        ;   /* maybe a list of character codes */
                upper_chars(Text, Upper)
        ).

upper_chars(-, _) :- !, fail.
upper_chars([], []).
upper_chars([T|Ts], [U|Us]) :-
        local_to_upper(T, U),
        upper_chars(Ts, Us).


% This comment is lifted verbatim from the Quintus Prolog strings.pl library file:

%   midstring(Whole, Part, Fringes, Before, Length, After)
%   is true when Whole, Part, and Fringes are all three atoms or are
%   all three strings,
%       Whole = Alpha || Part || Omega,
%       Fringes = Alpha || Omega,
%       string_size(Alpha, Before),
%       string_size(Part,  Length),
%       string_size(Omega, After)
%   midstring/[5,4,3] leave the trailing arguments unspecified.
%   This family has many uses.

midstring(Whole, Part, Fringes, Before, Length, After) :-
	sub_atom(Whole, Before, Length, After, Part),
	atom_codes(Whole, WholeChars),
	atom_codes(Part, PartChars),
	append([Prefix,PartChars,Suffix], WholeChars),
	append(Prefix, Suffix, FringesChars),
	atom_codes(Fringes, FringesChars).

midstring(Whole, Part, Fringes) :-
        midstring(Whole, Part, Fringes, _, _, _).

midstring(Whole, Part, Fringes, Before) :-
        midstring(Whole, Part, Fringes, Before, _, _).

midstring(Whole, Part, Fringes, Before, Length) :-
        midstring(Whole, Part, Fringes, Before, Length, _).

%   replist(?Datum, ?Length, ?List)
%   is true when replist(List, Datum) & length(List, Length).
%   Note that the parameters are the wrong way around for this to
%   be part of the library(length) family.

replist(Datum, Length, List) :-
        (   var(Length) ->
            replist_find(List, Datum, 0, Length)
        ;   integer(Length) ->
            Length >= 0,
            replist_make(Length, Datum, List)
        ).

replist_find([], _, N, N).
replist_find([X|Xs], X, N0, N) :-
        N1 is N0+1,
        replist_find(Xs, X, N1, N).

replist_make(0, _, Xs) :- !,
        Xs = [].
replist_make(N0, X, [X|Xs]) :-
        N1 is N0-1,
        replist_make(N1, X, Xs).


%   string_char(?Index, +StringOrAtom, ?Char)
%   unifies Char with the ISO8859 code of the Indexth character of StringOrAtom.
string_char(Index, Atom, CharCode) :-
	IndexMinus1 is Index - 1,
	sub_atom(Atom, IndexMinus1, 1, _After, Char),
	atom_codes(Char, [CharCode]).


string_size(StringOrAtom, Size) :-
	( atom(StringOrAtom) ->
	  atom_codes(StringOrAtom, String)
	; String = StringOrAtom
        ),
	length(String, Size).

subchars(Atom, SubString, Offset, Length) :-
	sub_atom(Atom, Offset, Length, _After, SubAtom),
	atom_codes(SubAtom, SubString).

sublist(List, SubList) :-
        sublist(List, SubList, _Before).

% This comment is lifted verbatim from the Quintus Prolog strings.pl library file:

%   substring(StringOrAtom, SubString, Offset, Length)
%   unifies SubString with the substring of StringOrAtom starting Offset
%   characters from the beginning and continuing for Length characters.
%   For example, substring(lamentation, mentat, 2, 6).

substring(StringOrAtom, SubString, Offset, Length) :-
	midstring(StringOrAtom, SubString, _Fringes, Offset, Length, _After).

ttyflush :- flush_output(user_output).

%   with_input_from_chars(+Goal, -Stream, +Chars)
%   executes Goal with Stream bound to an input stream consisting of
%   the characters in Chars, which must be a ground list of characters.
%   Stream is made the current input stream while Goal is running.
%   Note the peculiar use of Stream: it is NOT for communicating a
%   value to with_input_from_chars's caller nor for receiving one,
%   but for passing a value to Goal (Jensen's device, or nearly).
%   This routine is determinate, but will fail if Goal fails.

with_input_from_chars(Goal, Stream, Chars) :-
	open_codes_stream(Chars, Stream),
	current_input(OldStream),
	set_input(Stream),
	( call(Goal) ->
	  Flag = 1
	; Flag = 0
	),
	set_input(OldStream),
	close(Stream),
	Flag =:= 1.
