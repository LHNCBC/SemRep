
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

%   Module : ctypes
%   Author : Richard A. O'Keefe
%   Conversion of base 8 to base 10 done by FML using QP
%   Updated: 27 Feb 1992
%   Purpose: Character classification

%   Copyright (C) 1988, Quintus Computer Systems, Inc.  All rights reserved.

/*  The predicates to_lower, to_upper, is_alnum, is_alpha, is_cntrl,
    is_digit, is_graph, is_lower, is_upper, is_print, is_punct, and
    is_space are taken directly from the April 84 draft of the C
    standard.  The remaining ones are of my own invention, but are
    reasonably useful.  If you want to make your programs portable
    between different operating systems, use is_endline and is_endfile
    instead of the literal constants 31 and 26 or 10 and -1.

    Release 2.0 of Quintus Prolog handles 8-bit characters in the
    base system, classifying them according to the new international
    standard ISO 8859/1.  (VAX/VMS users note: the main difference
    between this and DEC's Multinational Character Set is that the
    latter has OE and oe ligatures, which ISO 8859/1 uses for a
    multiplication and division sign.  The VMS version of Quintus
    Prolog uses the VMS interpretation of those codes.)  In July 88
    library(ctypes) was upgraded to ISO 8859/1 as well.  As part of
    this, the is_ascii(C) predicate has been retained with the
    meaning "C is an ASCII character"; it is false of the others.
    A new is_char(C) predicate has been introduced, meaning "C is the
    code of a character in the supported character set".

    There is a problem:  in the ASCII character set, every letter is
    either an upper case letter having a unique corresponding lower
    case letter, or a lower case letter having a unique corresponding
    upper case letter.  This is not true in ISO 8859/1.  There is a
    lower case y with an umlaut, but no upper case Y with an umlaut.
    The German "sz" ligature has SS as its upper case equivalent, but
    that's two characters.  To cope with this problem, I have chosen
    to classify these characters as Extra letters, neither Lower nor
    Upper.  The predicate is_alpha/1 recognises them.

    Another question concerns the non-breaking space (code 160).
    Currently, is_cntrl(160), is_space(160), but \+ is_white(160).
    That may not be exactly right.  Suggestions?
*/

:- module(ctypes, [
	is_alnum/1,
	is_alpha/1,
	is_ascii/1,
	is_char/1,
	is_cntrl/1,
	is_csym/1,
	is_csymf/1,
	is_digit/1,
	is_digit/2,
	is_digit/3,
	is_endfile/1,
	is_endline/1,
	is_graph/1,
	is_layout/1,
	is_lower/1,
	is_newline/1,
	is_newpage/1,
	is_paren/2,
	is_period/1,
	is_print/1,
	is_punct/1,
	is_quote/1,
	is_space/1,
	is_upper/1,
	is_white/1,
	to_lower/2,
	to_upper/2,
	ctypes_bits/2
   ]).


/*  Character classification is now done with the aid of a table.
    Each entry in this table has the form
	bits(Ch, 2'ELUDSCVVVVVV)
    where
	Extra (8'4000) is on if Ch is a letter, but not upper or lower case.
	Lower (8'2000) is on if Ch is a lower case letter
	Upper (8'1000) is on if Ch is an upper case letter
	Digit (8'0400) is on if Ch is a decimal digit
	Space (8'0200) is on if is_space(Ch)
	Cntrl (8'0100) is on if is_cntrl(Ch)
	Value (8'0077) is the value of Ch as a digit, 63 for non-digits.
*/

%   is_alnum(?Char)
%   is true when Char is the character code of a letter or digit.

is_alnum(C) :-
	% bits(C, M), M /\ 8'7400 =\= 0.
 	bits(C, M), M /\ 3840 =\= 0.


%   is_alpha(?Char)
%   is true when Char is the character code of a letter, in either
%   case or none.  (It is NOT the union of is_lower/1 and is_upper/1.)

is_alpha(C) :-
% 	% bits(C, M), M /\ 8'7000 =\= 0.
 	bits(C, M), M /\ 3584 =\= 0.

%   is_ascii(?Char)
%   is true when Char is a character code in the ASCII range.

is_ascii(C) :-
	range(C, 0, 127).


%   is_char(?Char)
%   is true when Char is a character code in whatever the supported
%   range happens to be.  (In this version:  ISO 8859/1.)

is_char(C) :-
	range(C, 0, 255).


%   is_cntrl(?Char)
%   is true when Char is the character code of a control character.
%   The ASCII control characters are 0..31; ISO 8859/1 adds 128..160.

is_cntrl(C) :-
	% bits(C, M), M /\ 8'0100 =\= 0.
 	bits(C, M), M /\ 64 =\= 0.


%   is_csym(?Char)
%   is true when Char is the code of a character which could occur in
%   the body of an identifier (it is a letter, digit, or underscore).

is_csym(C) :-
	% bits(C, M), M /\ 8'17400 =\= 0.
	bits(C, M), M /\ 7936 =\= 0.


%   is_csymf(?Char)
%   is true when Char is the code of a character which could occur as
%   the first character of an identfier (it is a letter or underscore).

is_csymf(C) :-
	% bits(C, M), M /\ 8'17000 =\= 0.
 	bits(C, M), M /\ 7680 =\= 0.


%   is_digit(?Char)
%   is true when Char is the character code of a decimal digit.

is_digit(C) :-
	% bits(C, M), M /\ 8'0400 =\= 0.
	bits(C, M), M /\ 256 =\= 0.


%   is_digit(?Char, ?Weight)
%   is true when Char is the character code of a decimal digit,
%   and Weight is its decimal value.

is_digit(Char, Weight) :-
	(   integer(Char)   -> Char >= "0", Char =< "9", Weight is Char - "0"
	;   integer(Weight) -> Weight >= 0, Weight =< 9, Char is Weight + "0"
	;   var(Char), var(Weight) ->
	    range(Weight, 0, 9), Char is Weight + "0"
	).



%   is_digit(?Char, ?Base, ?Weight)
%   is true when Base is an integer between 2 and 36,
%   Weight is an non-negative integer less than Base,
%   and Char is the character code of a digit in that base
%   whose numerical value is Weight.  "Digits" greater than
%   9 may be in either case.

is_digit(Char, Base, Weight) :-
	range(Base, 2, 36),
	bits(Char, Mask),
	% Weight is Mask /\ 8'77,
	Weight is Mask /\ 63,
	Weight < Base.


%   is_endfile(?Char)
%   is true when Char is the end of file code (note that this may
%   or may not be the code of some character; it isn't in Quintus
%   Prolog, but it was the code of ^Z in DEC-10 Prolog and two others).

is_endfile(-1).			% was 26 in Dec-10 and C Prolog


%   is_endline(+Char)
%   succeeds when Char is the code of some character which terminates
%   a line.  Char must already be instantiated.

is_endline(C) :-		% line terminator, NOT just newline
	C < 32,			% covers EOF, ^G, ^J, ^K, ^L, ^M, ^Z, ESC, ^_
	C =\= 9 /* ^I */.	% not ^I (TAB) though.


%   is_graph(?Char)
%   is true if Char is the character code of some character which
%   should make a visible mark when printed.

is_graph(C) :-
	% bits(C, M), M /\ 8'0300 =:= 0.
	bits(C, M), M /\ 192 =:= 0.


%   is_layout(?Char)
%   is true when Char is a character code which does NOT correspond to
%   a visible mark; it is the opposite of is_graph/1.

is_layout(C) :-
	% bits(C, M), M /\ 8'0300 =\= 0.
	bits(C, M), M /\ 192 =\= 0.


%   is_lower(?Char)
%   is true when Char is the character code of a lower case letter.

is_lower(C) :-
	% bits(C, M), M /\ 8'2000 =\= 0.
	bits(C, M), M /\ 1024 =\= 0.


%   is_newline(?Char)
%   is true when Char is the character code of the normal new-line
%   character.  This is 10 on UNIX, 13 in systems where CR is the
%   line terminator, and is 31 in systems where CR+LF is the terminator.

is_newline(10 /* ^J */).


%   is_newpage(?Char)
%   is true when Char is the character code of the control character
%   which starts a new page.  There need not be any such character.

is_newpage(12 /* ^L */).	% may fail (O/S-dependent)


%   is_paren(?Left, ?Right)
%   is true when Left and Right are character codes corresponding to
%   "()", "[]", "{}", or "<< >>".

is_paren(0'(, 0')).
is_paren(0'[, 0']).
is_paren(0'{, 0'}).
is_paren(171, 187).	% Left << and Right >> quote characters.


%   is_period(?Char)
%   is true when Char is the character code of any of the three stops
%   which may terminate a period (also known as a sentence):  the full
%   stop, the question mark, or the exclamation mark.

is_period(0'.).		% Full stop
is_period(0'?).		% Question mark
is_period(0'!).		% Exclamation mark


%   is_print(?Char)
%   is true when Char is a character code other than the code of a
%   control character.  That is, a graphic character, or a blank.

is_print(C) :-
	% bits(C, M), M /\ 8'0100 =:= 0.
	bits(C, M), M /\ 64 =:= 0.


%   is_punct(?Char)
%   is true when Char is the character code of some graphic character
%   other than a letter or digit.

is_punct(C) :-
	% bits(C, M), M /\ 8'3700 =:= 0.
	bits(C, M), M /\ 1984 =:= 0.


%   is_quote(?Char)
%   is true when Char is the character code of a quotation mark.
%   ', ", and ` are accepted as quotation marks.  This doesn't handle
%   << and >> because it fails to draw the left/right distinction.
%   (Instead of squandering code space on trash like 1/4,1/2,3/4 and
%   so on, why didn't ISO give us ENGLISH 6--9 66--99 quotation marks?)

is_quote(0'\').
is_quote(0'").  %" Comment is there just to appease Emacs
is_quote(0'`). 


%   is_space(?Char)
%   is true when Char is the character code of a "format effector",
%   space, tab, line-feed, carriage return, form feed, vertical tab.
%   For reasons of backwards compatibility this includes DEC-10 Prolog's
%   line terminator (31).

is_space(C) :-
	% bits(C, M), M /\ 8'0200 =\= 0.
	bits(C, M), M /\ 128 =\= 0.


%   is_upper(?Char)
%   is true when Char is the character code of an upper case letter.

is_upper(C) :-
	% bits(C, M), M /\ 8'1000 =\= 0.
	bits(C, M), M /\ 512 =\= 0.


%   is_white(?Char)
%   is true when Char is the character code of a space or tab.
%   The ISO 8859/1 non-breaking space (160) is _not_ currently
%   accepted, but that may change.

is_white(32).			% space
is_white( 9).			% tab


%   to_lower(?UpperCase, ?LowerCase)
%   converts upper case letters to lower case, and leaves other
%   characters alone.

to_lower(U, L) :-
	bits(U, M),
	% L is U + (M /\ 8'1000) >> 4.
	L is U + (M /\ 512) >> 4.


%   to_upper(?LowerCase, ?UpperCase)
%   converts lower case letters to upper case, and leaves other
%   characters (including "sz" and y-umlaut) alone.

to_upper(L, U) :-
	bits(L, M),
	% U is L - (M /\ 8'2000) >> 5.
	U is L - (M /\ 1024) >> 5.


%.  range(?X, +L, +U)
%   is a specialised version of between(L, U, X) where it is known that
%   L and U are integers with L < U, and we don't want an error message
%   if X is neither a variable nor an integer.  In fact L, U, and X are
%   all supposed to be character codes.

range(X, L, U) :-
	(   integer(X) -> L =< X, X =< U
	;   var(X) ->     range1(L, U, X)
	).

range1(X, _, X).
range1(L, U, X) :-
	M is L+1,
	M =< U,
	range1(M, U, X).


/*  The ASCII part of the table below was bootstrapped from an earlier
    version of library(ctypes) using the following code:

	tell(bits),
	is_ascii(C),
	(   is_digit(C) -> M is C - 0'0 +      8'0400
	;   is_upper(C) -> M is C - 0'A + 10 + 8'1000
	;   is_lower(C) -> M is C - 0'a + 10 + 8'2000
	;   is_csymf(C) -> M is 8'4077
	;   C =:= " "   -> M is 8'0277
	;   is_space(C) -> M is 8'0377
	;   is_cntrl(C) -> M is 8'0177
	;		   M is 8'0077
	),
	format('bits(~|~t~d~3+, 2''~|~`0t~2r~12+).~n', [C,M]),
	fail
    ;	told.
*/

bits(  0, 127).
bits(  1, 127).
bits(  2, 127).
bits(  3, 127).
bits(  4, 127).
bits(  5, 127).
bits(  6, 127).
bits(  7, 127).
bits(  8, 127).
bits(  9, 255).
bits( 10, 255).
bits( 11, 255).
bits( 12, 255).
bits( 13, 255).
bits( 14, 127).
bits( 15, 127).
bits( 16, 127).
bits( 17, 127).
bits( 18, 127).
bits( 19, 127).
bits( 20, 127).
bits( 21, 127).
bits( 22, 127).
bits( 23, 127).
bits( 24, 127).
bits( 25, 127).
bits( 26, 127).
bits( 27, 127).
bits( 28, 127).
bits( 29, 127).
bits( 30, 127).
bits( 31, 255).
bits( 32, 191).
bits( 33, 63).
bits( 34, 63).
bits( 35, 63).
bits( 36, 63).
bits( 37, 63).
bits( 38, 63).
bits( 39, 63).
bits( 40, 63).
bits( 41, 63).
bits( 42, 63).
bits( 43, 63).
bits( 44, 63).
bits( 45, 63).
bits( 46, 63).
bits( 47, 63).
bits( 48, 256).
bits( 49, 257).
bits( 50, 258).
bits( 51, 259).
bits( 52, 260).
bits( 53, 261).
bits( 54, 262).
bits( 55, 263).
bits( 56, 264).
bits( 57, 265).
bits( 58, 63).
bits( 59, 63).
bits( 60, 63).
bits( 61, 63).
bits( 62, 63).
bits( 63, 63).
bits( 64, 63).
bits( 65, 522).
bits( 66, 523).
bits( 67, 524).
bits( 68, 525).
bits( 69, 526).
bits( 70, 527).
bits( 71, 528).
bits( 72, 529).
bits( 73, 530).
bits( 74, 531).
bits( 75, 532).
bits( 76, 533).
bits( 77, 534).
bits( 78, 535).
bits( 79, 536).
bits( 80, 537).
bits( 81, 538).
bits( 82, 539).
bits( 83, 540).
bits( 84, 541).
bits( 85, 542).
bits( 86, 543).
bits( 87, 544).
bits( 88, 545).
bits( 89, 546).
bits( 90, 547).
bits( 91, 63).
bits( 92, 63).
bits( 93, 63).
bits( 94, 63).
bits( 95, 4159).
bits( 96, 63).
bits( 97, 1034).
bits( 98, 1035).
bits( 99, 1036).
bits(100, 1037).
bits(101, 1038).
bits(102, 1039).
bits(103, 1040).
bits(104, 1041).
bits(105, 1042).
bits(106, 1043).
bits(107, 1044).
bits(108, 1045).
bits(109, 1046).
bits(110, 1047).
bits(111, 1048).
bits(112, 1049).
bits(113, 1050).
bits(114, 1051).
bits(115, 1052).
bits(116, 1053).
bits(117, 1054).
bits(118, 1055).
bits(119, 1056).
bits(120, 1057).
bits(121, 1058).
bits(122, 1059).
bits(123, 63).
bits(124, 63).
bits(125, 63).
bits(126, 63).
bits(127, 127).
bits(128, 127).
bits(129, 127).
bits(130, 127).
bits(131, 127).
bits(132, 127).
bits(133, 127).
bits(134, 127).
bits(135, 127).
bits(136, 127).
bits(137, 127).
bits(138, 127).
bits(139, 127).
bits(140, 127).
bits(141, 127).
bits(142, 127).
bits(143, 127).
bits(144, 127).
bits(145, 127).
bits(146, 127).
bits(147, 127).
bits(148, 127).
bits(149, 127).
bits(150, 127).
bits(151, 127).
bits(152, 127).
bits(153, 127).
bits(154, 127).
bits(155, 127).
bits(156, 127).
bits(157, 127).
bits(158, 127).
bits(159, 127).
bits(160, 127).
bits(161, 63).
bits(162, 63).
bits(163, 63).
bits(164, 63).
bits(165, 63).
bits(166, 63).
bits(167, 63).
bits(168, 63).
bits(169, 63).
bits(170, 63).
bits(171, 63).
bits(172, 63).
bits(173, 63).
bits(174, 63).
bits(175, 63).
bits(176, 63).
bits(177, 63).
bits(178, 63).
bits(179, 63).
bits(180, 63).
bits(181, 63).
bits(182, 63).
bits(183, 63).
bits(184, 63).
bits(185, 63).
bits(186, 63).
bits(187, 63).
bits(188, 63).
bits(189, 63).
bits(190, 63).
bits(191, 63).
bits(192, 575).
bits(193, 575).
bits(194, 575).
bits(195, 575).
bits(196, 575).
bits(197, 575).
bits(198, 575).
bits(199, 575).
bits(200, 575).
bits(201, 575).
bits(202, 575).
bits(203, 575).
bits(204, 575).
bits(205, 575).
bits(206, 575).
bits(207, 575).
bits(208, 575).
bits(209, 575).
bits(210, 575).
bits(211, 575).
bits(212, 575).
bits(213, 575).
bits(214, 575).
bits(215, 575).
bits(216, 575).
bits(217, 575).
bits(218, 575).
bits(219, 575).
bits(220, 575).
bits(221, 575).
bits(222, 575).
bits(223, 2111).
bits(224, 1087).
bits(225, 1087).
bits(226, 1087).
bits(227, 1087).
bits(228, 1087).
bits(229, 1087).
bits(230, 1087).
bits(231, 1087).
bits(232, 1087).
bits(233, 1087).
bits(234, 1087).
bits(235, 1087).
bits(236, 1087).
bits(237, 1087).
bits(238, 1087).
bits(239, 1087).
bits(240, 1087).
bits(241, 1087).
bits(242, 1087).
bits(243, 1087).
bits(244, 1087).
bits(245, 1087).
bits(246, 1087).
bits(247, 1087).
bits(248, 1087).
bits(249, 1087).
bits(250, 1087).
bits(251, 1087).
bits(252, 1087).
bits(253, 1087).
bits(254, 1087).
bits(255, 2111).

ctypes_bits(C, B) :- bits(C, B).

/*
   The conversion from Quintus Prolog to SICStus prolog was tested
   by comparing the output of the following goal in QP and SP:

test_bits :-
	  tell(bits),
	  bits(C,D),       write(bits:C-D), nl, fail
	; is_alnum(C),     write(alnum:C), nl, fail
	; is_alnum(C),     write(alnum:C), nl, fail
	; is_alpha(C),     write(alpha:C), nl, fail
	; is_ascii(C),     write(ascii:C), nl, fail
	; is_char(C),      write(char:C),  nl, fail
	; is_cntrl(C),     write(cntrl:C), nl, fail
	; is_csym(C),      write(csym:C),  nl, fail
	; is_csymf(C),     write(csymf:C), nl, fail
	; is_digit(C),     write(digit:C), nl, fail
	; is_digit(C,D),   write(digit:C-D), nl, fail
	; is_digit(C,D,W), write(digit:C-D-W), nl, fail
	; is_endfile(C),   write(endfile:C), nl, fail
	; is_endline(C),   write(endline:C), nl, fail
	; is_graph(C),     write(graph:C), nl, fail
	; is_lower(C),     write(lower:C), nl, fail
	; is_newline(C),   write(newline:C), nl, fail
	; is_newpage(C),   write(newpage:C), nl, fail
	; is_paren(C,D),   write(paren:C-D), nl, fail
	; is_period(C),    write(period:C), nl, fail
	; is_print(C),     write(print:C), nl, fail
	; is_punct(C),     write(punct:C), nl, fail
	; is_quote(C),     write(quote:C), nl, fail
	; is_space(C),     write(space:C), nl, fail
	; is_upper(C),     write(upper:C), nl, fail
	; is_white(C),     write(white:C), nl, fail
	; to_lower(C,D),   write(to_lower:C-D), nl, fail
	; to_upper(C,D),   write(to_upper:C-D), nl, fail
	; told.
*/
