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
                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  https://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

% File:	    nls_io.pl
% Module:   NLS I/O
% Author:   Lan
% Purpose:  Provide miscellaneous I/O routines.


:- module(nls_io,[
	fget_line/2,
  	fget_non_ws_only_line/3,
	fget_lines_until_skr_break/4,
	% needed by tools/lib/reader.pl
  	get_line/1,
    	get_line/2
    ]).

:- use_module(metamap(metamap_tokenization), [
	local_ascii/1,
	local_ws/1
    ]).

% :- use_module(skr_lib(ctypes),[
% 	is_ascii/1
%     ]).

:- use_module(skr(skr_utilities),[
	fatal_error/2
    ]).

:- use_module(skr_lib(sicstus_utils),[
	concat_atom/2
    ]).



/* fget_non_ws_only_line(+Stream, -Line)

fget_non_ws_only_line/2 reads lines from Stream until it encounters a non
"blank" Line, i.e., a line with only whitespace characters (if any).
It fails at end-of-file.  */

fget_non_ws_only_line(Stream, NumBlankLines, Line) :-
	% At end of stream, peek_code returns -1
	peek_code(Stream, Code),
	Code =\= -1,
	!,
	fget_line(Stream, Line0),
	( is_ws_only(Line0),
          % If --blanklines 0 is specified,
	  % whitespace before the first printing char is NOT ignored.
	  NumBlankLines =\= 0 ->
	  fget_non_ws_only_line(Stream, NumBlankLines, Line)
	; Line = Line0
	).

is_ws_only([]).
is_ws_only([Code|Rest]) :-
	% tab, lf, vt, ff, cr and space
	% is_space(Code),
	local_ws(Code),
	is_ws_only(Rest).

%%% /* fget_all_non_null_lines(+Stream, -Lines)
%%% 
%%% fget_all_non_null_lines/2 reads lines from Stream through EOF ignoring null
%%% lines. */
%%% 

/* fget_lines_until_skr_break(+Stream, -Lines)

fget_lines_until_skr_break/2 reads Lines from Stream until it encounters
a "blank" line consisting of whitespace characters only (if any) */

fget_lines_until_skr_break(Stream, _NumBlankLinesOrig, _NumBlankLines, Lines) :-
	% At end of stream, peek_code returns -1
	peek_code(Stream, Code),
	Code is -1,
	!,
	Lines = [].
fget_lines_until_skr_break(Stream, NumBlankLinesOrig, NumBlankLinesIn, Lines) :-
	fget_line(Stream, Line, Terminator),
	( Terminator =:= -1 ->
	  Lines = [Line]
	  % If the input line consists only of whitespace...
	; is_ws_only(Line) ->
	  % Steven Bedrick special handling
	  ( NumBlankLinesIn is 1 ->
	    Lines = []
	  ; NumBlankLinesNext is NumBlankLinesIn - 1,
	    Lines = [Line|RestLines],
	    fget_lines_until_skr_break(Stream, NumBlankLinesOrig, NumBlankLinesNext, RestLines)
	  )
	  % The line just read in is not all whitespace,
	; Lines = [Line|RestLines],
	  % so reset the NumBlankLines counters to their original value.
          fget_lines_until_skr_break(Stream, NumBlankLinesOrig, NumBlankLinesOrig, RestLines)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The following predicates are slightly-modified versions of code
% taken from the QP3.5 library file lineio.pl;
% The only change (other than renaming "Chars" variables to "Codes"
% is to fget_line_1/2 and get_line_1/2, in which the line
%	( terminator_code(Code) ->
% replaces the line
%	(   Char < " ", Char =\= 9, Char =\= 5 ->
% However, since get_line_1/2 is the lowest-level predicate,
% all the higher level predicates formerly imported from lineio.pl
% are included here as well.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fget_line(+Stream, ?Codes)
% reads a line from the given input Stream, and returns the characters in
% the list Codes.  It does NOT return the line terminating character, so it
% is useful for portable programming.  If the terminator was the end of the
% file, it simply fails and later calls will abort.

fget_line(Stream, Codes) :-
	fget_line(Stream, Line, _Terminator),
	Codes = Line.
	% format(user_output, 'Read in line "~s"~n', [Codes]),
	% true.


% fget_line(+Stream, ?Codes, ?Terminator)
% reads a line from the given input Stream, and returns the characters in
% the list Codes, and the line terminating character in Terminator.  If the
% terminator was end of file, it just returns it like always.  When you use
% this routine, the last line will often be ignored if not properly ended.

fget_line(Stream, Codes, Term) :-
	fget_line_1(Stream, Line, Terminator),
	Codes = Line,
	Term = Terminator.

fget_line_1(Stream, Line, Terminator) :-
	get_code(Stream, Code),
	( terminator_code(Code) ->
	  Line = [],
	  Terminator = Code
	; Line = [Code|Codes],
	  fget_line_1(Stream, Codes, Terminator)
	).

% NL
terminator_code(10).
% CR
terminator_code(13).
% eof in case user does not have a <CR> at the end of the file!!
terminator_code(-1).

