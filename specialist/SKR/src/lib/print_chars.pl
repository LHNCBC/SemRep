
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

%   Module : print_chars
%   Author : Richard A. O'Keefe
%   Updated: 10 Sep 1991
%   Purpose: Portray lists of characters as strings.

%   Adapted from shared code written by the same author; all changes
%   Copyright (C) 1988, Quintus Computer Systems, Inc.  All rights reserved.

:- module(print_chars, [
% 	plausible_chars/1,
	portray_chars/1
   ]).

:- use_module(metamap(metamap_tokenization), [
	local_ascii/1,
	local_print/1
   ]).

:- use_module(skr_lib(addportray), [
	add_portray/1
   ]).

:- use_module(skr_lib(ctypes), [
	ctypes_bits/2
   ]).

%   plausible_chars(Chars)
%   is true when Chars is a list of "plausible" characters, possibly
%   ending with a variable.  To be plausible, either is_graph(Char) or
%   is_space(Char) must be true for each element Char of Chars.  As
%   well as allowing Chars to end with a variable, it may end with
%   a $VAR(_) term bound by numbervars/3.

plausible_chars(Var) :-
	var(Var), !.
plausible_chars('$VAR'(_)).
plausible_chars([]).
plausible_chars([Char|Chars]) :-
	integer(Char),
	ctypes_bits(Char, Mask),
	% Mask /\ 8'0300 =\= 8'0100,	% is_graph or is_space
	Mask /\ 192 =\= 64,	% is_graph or is_space
	plausible_chars(Chars).


%   portray_chars(Chars)
%   checks whether Chars is a non-empty plausible list of character codes.
%   If it is, it prints the characters out between double quotes.
%   THIS IS A DEBUGGING AID.  Control characters are written out in ^X
%   form, rather than using \x.  If the list ends with a variable or
%   $VAR(_), that is written out as |_X, which will not, of course, be
%   read back.  That's ok, it's just for looking at.

portray_chars(Chars) :-
	Chars = [_|_],			% a non-empty list
	plausible_chars(Chars),		% of plausible characters
	put_code(0'"), 'portray chars'(Chars), put_code(0'").


'portray chars'(Var) :-
	var(Var),
	!,
	put_code(0'|), write(Var).
'portray chars'([]).
'portray chars'('$VAR'(N)) :-
	put_code(0'|), write('$VAR'(N)).
'portray chars'([Char|Chars]) :-
	% FML 03/24/2013: The only chars that will catch the first branch are
	% 9, 10, 11, 12, 13, and 31 (ctypes:(is_graph(C); is_space(C)), C < " ").
	(   Char < " " ->		% we should have to_cntrl/2
	    Caps is Char+64,		% in library(ctypes); as yet
	    put_code(0'^), put_code(Caps)	% we don't, so not EBCDICy.
	;   Char =:= 0'" ->		% " is written doubled
	    put_code(0'"), put_code(0'")
	    % FML 03/20/2014: double backslashes!
	;   Char =:= 0'\\ ->		% " is written doubled
	    put_code(0'\\), put_code(0'\\)
	;   put_code(Char)
	),
	'portray chars'(Chars).

/*  When you ensure_loaded(library(print_chars)), portray_chars/1
    is automagically hooked into portray/1.  If you don't want that,
    do del_portray(print_chars:portray_chars).
*/

:- initialization add_portray(portray_chars).
