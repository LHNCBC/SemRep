
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
*  http://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% File:     lexical.pl
% Module:   Lexical
% Author:   Lan
% Purpose:  Provide lexical routines.

/* Lexical Utilities Module
*/

:- module(lexical,[
    concatenate/3,
    concatenate_strings/3,
    is_integer_string/1,
    lowercase_list/2   % NOTE: Now handles atoms and strings
    ]).

:- use_module(skr_lib(sicstus_utils),[
	lower/2
    ]).

/* character(?Type, +CharacterAsciiValue)
Succeeds if a the character corresponding to a given value
is of a given type. Types include :

letter   digit  end_of_line (the newline character, ascii 10)
end_of_file (the end of file marker, ascii -1 )
upper_case_letter  lower_case_letter period
alphanumeric (letter or digit)  space  nonspace (anything but space) 
delimiter ( \",(,),[, or ] )
any ascii value (matches that value only)
a list of ascii values or a string (matches any value in the list/string)
not(any of the above)  (matches anything except the specified type)

Note that if type is uninstantiated, only the following Types will be matched
with any given character :
end_of_line, end_of_file, space, period, letter, digit, 
the character value itself.
*/

character(end_of_line,10) :- !.
character(end_of_file,-1) :- !.
character(space,32) :-!.
character(period,46) :- !.
% LRA  added alphanumeric to avoid having p0001 parsed as p,num(1)...
character(alphanumeric,Value) :-
    (character(letter,Value);
     character(digit,Value)
    ),
    !.
character(letter,Value) :-
  (character(lower_case_letter,Value);
   character(upper_case_letter,Value)
  ),
  !.
character(digit,Value) :- 
  Value>=0'0,
  Value=<0'9,
  !.
character(Char,Char) :- !.

% from here on down the type should be instantiated when calling.
character(X,_) :-
  var(X),
  !,
  fail.

character([H|T],Value) :-
  nonvar(H),
  member(Value,[H|T]),
  !.
character(upper_case_letter,Value) :-
  Value>=0'A,
  Value=<0'Z,
  !.
character(lower_case_letter,Value) :-
  Value>=0'a,
  Value=<0'z,
  !.
character(delimiter,Value) :-
  character([34|"()[]"],Value),
  !.
character(not(Char_type),Value) :-
   \+ character(Char_type,Value),
  !.
character(nonspace,Value) :-
  Value \== 32,
  !.


/* concatenate(+List, +StringToIntercalate, ?Atom)
Takes a list and a string to insert and returns an atom
whose name consists of the names of all of the 
atoms in the list smashed together, with the supplied string inserted 
between each atom and the next. 

example: concatenate([one,two,three],"/",Atom]).
         Atom = 'one/two/three' */

concatenate(List,Insert_string,Concatenated) :-
  get_strings(List,Insert_string,Concat_string),
  name(Concatenated,Concat_string),
  !.


/* get_strings(+AtomList, +SeparatorString, -ConcatenatedString)
Takes a list of prolog atoms and converts them into one long string,
with the atoms separated by the string provided. */

get_strings([num(X)], _, Xstring) :-    % suresh 8/30/88
	!,
	atom_codes(X, Xstring).
get_strings([X], _, Xstring) :-
	!,
	name(X, Xstring).
get_strings([num(Atom)|List], Insert_string, Concat_list) :-    % suresh 8/30/88
	!,
	atom_codes(Atom, Concat),
	append(Concat, Insert_string, Concat_string),
	append(Concat_string, Sub_concat, Concat_list),
	get_strings(List, Insert_string, Sub_concat).
get_strings([Atom|List], Insert_string, Concat_list) :-
	atom_codes(Atom, Concat),
	append(Concat, Insert_string, Concat_string),
	append(Concat_string, Sub_concat, Concat_list),
	get_strings(List, Insert_string, Sub_concat).
get_strings([], _, []).


/* concatenate_strings(+List, +InsertString, -NewList)
takes a list of strings and an insert string and generates a new
string separated by the insert string, placing the results in NewList.

Example:
| ?- concatenate_strings(["this","was"],".",R), name(X,R).

R = [116,104,105,115,46,119,97,115],
X = 'this.was' 
*/
concatenate_strings([],_,[]) :-
    !.
concatenate_strings([String],_,String) :-
    !.
concatenate_strings([First|Rest],InsertString,Result) :-
    append(First,InsertString,S),
    append(S,ConcatenatedRest,Result),
    concatenate_strings(Rest,InsertString,ConcatenatedRest),
    !.


/* is_integer_string(+Term)
is_integer_string/1 succeeds if Term is a string of digits.  The null string
succeeds by default. */

is_integer_string([]).
is_integer_string([First|Rest]) :-
    character(digit,First),
    is_integer_string(Rest).


/* is_upper_string(+Term)
is_upper_string/1 succeeds if Term is a string of non-lowercase characters.
The null string succeeds by default. */

%is_upper_string([]).
%is_upper_string([First|_Rest]) :-
%    character(lower_case_letter,First),
%    !,
%    fail.
%is_upper_string([_First|Rest]) :-
%    is_upper_string(Rest).


/* lowercase_atom(+Atom, -NewAtom)
Make all letters in Atom lowercase.

| ?- lowercase_atom('BIRD_SEED',R).
R = bird_seed */

%lowercase_atom(Atom,NewAtom) :-
%   atom_codes(Atom,String),
%   lowercase(String,LowString),
%   atom_codes(NewAtom,LowString).


/* lowercase_list(+TextList, -LowercaseTextList)
lowercase_list/2 creates LowercaseTextList, the list of lowercase
strings/atoms corresponding to those in TextList. */

lowercase_list([],[]).
lowercase_list([First|Rest],[LCFirst|LCRest]) :-
	lower(First,LCFirst),
	lowercase_list(Rest,LCRest).
