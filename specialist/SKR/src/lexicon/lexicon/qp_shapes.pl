
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

/* qp_shapes.pl - shapes grammar for recognizing numbers, etc
*/

:- module(qp_shapes, [
	shapes/3
    ]).

:- use_module(skr_lib(ctypes), [
       	to_lower/2
   ]).

:- use_module(skr_lib(nls_strings), [
    is_integer_string/1
    ]).

:- use_module(skr_lib(sicstus_utils), [
       	lower/2
   ]).

:- use_module(library(lists), [
	rev/2,
	append/2
   ]).

%%% Using shapes we want to recognize the following structures:
%%%
%%%	125, 125,000, 125.32, 12/27, 15%, 15.32%, $53, $43.20
%%%	12th, 3rd, 85th, 125,000th, 1/21st
%%%	fifty two, thirty-three, sixteenth

%%% shapes(-Record)
%%%
shapes(R) -->
    percentage(R), !.
shapes(R) -->
    dollar(R), !.
shapes(R) -->
    numeric_ordinal(R), !.
shapes(R) -->
    numeric(R), !.
shapes(R) -->
    wordord(R).
shapes(R) -->
    wordnum(R).

%%% 45% 45.54% 23,000% 12/34%
percentage(R) -->
    numeric(R1),
    ['%'],
    {
	R1 = shapes:[inputmatch:M1, features:Z],
	R  = shapes:[inputmatch:M2, features:[percentage|Z]],
	append(M1, ['%'], M2)
    },
    !.

%%% $45 $45.00 $45,000 $12/45
dollar(R) -->
    ['$'],
    numeric(R1),
    {
	R1 = shapes:[inputmatch:M1, features:Z],
	R  = shapes:[inputmatch:['$'|M1], features:[dollar|Z]]
    },
    !.

%%% 125,345,000th
numeric_ordinal(R) -->
    [A, ','],
    { atom_codes(A, NL), length(NL, L), L =< 3, is_integer_string(NL)},
    numeric_ordinal_comma(Rest),
    { R = shapes:[inputmatch:[A, ','|Rest], features:[ordinal_number]] }.
%%% 1/10th, 1/112th
numeric_ordinal(R) -->
    [A1, '/', A2],
    { atom_codes(A1, NL1), is_integer_string(NL1), numeric_ordinal_pred(A2) },
    { R = shapes:[inputmatch:[A1, '/', A2], features:[numeric_ordinal]] }.
%%% 45th or 23rd
numeric_ordinal(R) -->
    [A],
    { numeric_ordinal_pred(A), R = shapes:[inputmatch:[A], features:[numeric_ordinal]] }.

%%% handles comma separated integers
numeric_ordinal_comma(X) -->
    [Number],
    { atom_codes(Number, NL), length(NL, 3), is_integer_string(NL)},
    [','],
    !,
    numeric_ordinal_comma(R),
    { X = [Number, ','|R] }.
numeric_ordinal_comma([Ord]) -->
    [Ord],
    { numeric_ordinal_pred(Ord) }.

%%% succeeds if atom A has a ordinal suffix.
numeric_ordinal_pred(A) :-
    atom_codes(A, List),
    append(X, [Y, Z], List),
    is_integer_string(X),	    %%% is a number?
    rev(X, [D|XR]),
    check_ordinal_suffix(D, XR, Y, Z),
    !.

check_ordinal_suffix(0'1, X, S, L) :-	%%% _1st except 11th
    X \== [0'1|_],
    (	S == 0's
    ;	S == 0'S
    ),
    (	L = 0't
    ;	L = 0'T
    ),
    !.
check_ordinal_suffix(0'2, X, S, L) :-	%%% _2nd except 12th
    X \== [0'1|_],
    (	S == 0'n
    ;	S == 0'N
    ),
    (	L == 0'd
    ;	L == 0'D
    ),
    !.
check_ordinal_suffix(0'3, X, S, L) :-	%%% _3rd except 13th
    X \== [0'1|_],
    (	S == 0'r
    ;	S == 0'R
    ),
    (	L == 0'd
    ;	L == 0'D
    ),
    !.
check_ordinal_suffix(_, _, S, L) :-
    (	S == 0't
    ;	S == 0'T
    ),
    (	L == 0'h
    ;	L == 0'H
    ),
    !.    

%%% 125,000
numeric(R) -->
    [A, ','],
    { atom_codes(A, NL), length(NL, L), L =< 3, is_integer_string(NL)},
    numeric_comma(Rest),
    { R = shapes:[inputmatch:[A, ','|Rest], features:[comma_integer]] }.
%%% 125.32
numeric(R) -->
    [A1, '.', A2],
    { atom_codes(A1, NL1), is_integer_string(NL1), atom_codes(A2, NL2), is_integer_string(NL2) },
    { R = shapes:[inputmatch:[A1, '.', A2], features:[real_number]] }.
%%% 12/27
numeric(R) -->
    [A1, '/', A2],
    { atom_codes(A1, NL1), is_integer_string(NL1), atom_codes(A2, NL2), is_integer_string(NL2) },
    { R = shapes:[inputmatch:[A1, '/', A2], features:[fraction]] }.
%%% e.g., 125
numeric(R) -->
    [A],
    { atom_codes(A, NL), is_integer_string(NL), R = shapes:[inputmatch:[A], features:[integer]] }.

%%% handles comma separated integers
numeric_comma(X) -->
    [Number],
    { atom_codes(Number, NL), length(NL, 3), is_integer_string(NL)},
    (	[','] ->
	    { X = [Number, ','|R] }, numeric_comma(R)
	;   { X = [Number] }
    ).

%%% NUMBERS WRITTEN AS WORDS

wordnum(R) -->
    (	thousands(X1)
    ;	hundreds(X2)
    ;	lessthan100(X3)
    ),
    {
	append([X1, X2, X3], X),
	R = shapes:[inputmatch:X, features:[word_numeral]]
    },
    !.
wordnum(R) -->
    lessthan10(X1),
    lmatch([million], X2),
    { append(X1, X2, X), R = shapes:[inputmatch:X, features:[word_numeral]] },
    !.

%%% Simple grammar for numbers < 999,999
%%% X is a list of the matching input tokens
%%% 654,234
thousands(X) --> hundreds(X1), lmatch([thousand], X2), hundreds(X3),
    { append([X1, X2, X3], X) }, !.
%%% 623,012
thousands(X) --> hundreds(X1), lmatch([thousand], X2),
    (	lmatch([and], X3)
    ;	[], { X3 = []}
    ),
    lessthan100(X4),
    { append([X1, X2, X3, X4], X) },
    !.
%%% 13,645
thousands(X) --> lessthan100(X1), lmatch([thousand], X2), hundreds(X3),
    { append([X1, X2, X3], X) }, !.
%%% 13,014
thousands(X) --> lessthan100(X1), lmatch([thousand], X2),
    (	lmatch([and], X3)
    ;	[], { X3 = []}
    ),
    lessthan100(X4),
    { append([X1, X2, X3, X4], X) },
    !.
%%% 413,000
thousands(X) --> hundreds(X1), lmatch([thousand], X2), { append(X1, X2, X) }, !.
%%% 13,000
thousands(X) --> lessthan100(X1), lmatch([thousand], X2), { append(X1, X2, X) }, !.

hundreds(X) --> lessthan10(X1), lmatch([hundred, and], X2), lessthan100(X3),
    { append([X1, X2, X3], X) }.    %%% in library(lists)
hundreds(X) --> lessthan10(X1), lmatch([hundred], X2), { append(X1, X2, X) }.

%%% clauses are structured for longest match first
%%% In X are the tokens that matched
lessthan100(X) --> nty(X1), ['-'], lessthan10(X2), { append([X1, ['-'], X2], X) }, !.
lessthan100(X) --> nty(X1), lessthan10(X2), { append(X1, X2, X) }, !.
lessthan100(X) --> tens(X), !.
lessthan100(X) --> teens(X), !.
lessthan100(X) --> lessthan10(X), !.

%%% In X is the matching input token
tens(X) -->
    (	lmatch([ten], X)
    ;	lmatch([twenty], X)
    ;	lmatch([thirty], X)
    ;	lmatch([forty], X)
    ;	lmatch([fifty], X)
    ;	lmatch([sixty], X)
    ;	lmatch([seventy], X)
    ;	lmatch([eighty], X)
    ;	lmatch([ninety], X)
    ).

%%% In X is the matching input token
nty(X) -->
    (	lmatch([twenty], X)
    ;	lmatch([thirty], X)
    ;	lmatch([forty], X)
    ;	lmatch([fifty], X)
    ;	lmatch([sixty], X)
    ;	lmatch([seventy], X)
    ;	lmatch([eighty], X)
    ;	lmatch([ninety], X)
    ).

%%% in X is the matching input token
teens(X) -->
    (	lmatch([eleven], X)
    ;	lmatch([twelve], X)
    ;	lmatch([thirteen], X)
    ;	lmatch([fourteen], X)
    ;	lmatch([fifteen], X)
    ;	lmatch([sixteen], X)
    ;	lmatch([seventeen], X)
    ;	lmatch([eighteen], X)
    ;	lmatch([nineteen], X)
    ).

%%% In X is the matching input token
lessthan10(X) -->
    (	lmatch([one], X)
    ;	lmatch([two], X)
    ;	lmatch([three], X)
    ;	lmatch([four], X)
    ;	lmatch([five], X)
    ;	lmatch([six], X)
    ;	lmatch([seven], X)
    ;	lmatch([eight], X)
    ;	lmatch([nine], X)
    ).

%%% ordinals as words
wordord(R) -->
    [X],
    { lower(X, L), wordord_lower(L), R = shapes:[inputmatch:[X], features:[word_ordinal]] }.

wordord_lower(first).
wordord_lower(second).
wordord_lower(third).
wordord_lower(fourth).
wordord_lower(fifth).
wordord_lower(sixth).
wordord_lower(seventh).
wordord_lower(eighth).
wordord_lower(ninth).
wordord_lower(tenth).
wordord_lower(eleventh).
wordord_lower(twelfth).
wordord_lower(thirteenth).
wordord_lower(fourteenth).
wordord_lower(fifteenth).
wordord_lower(sixteenth).
wordord_lower(seventeenth).
wordord_lower(eighteenth).
wordord_lower(nineteenth).
wordord_lower(twentieth).

%%% lowercased matcher
lmatch([], []) --> [].
lmatch([F|R], [A|X]) -->
    [A],
    { lower(A, L), L == F },
    lmatch(R, X).

