
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

/* lcheck.pl - checks validity of lexical records
*/

:- use_module(lexicon(qp_fm_lexrec), [
	fm_lexical_record/4
   ]).


:- use_module(lexicon(qp_recio), [
	read_lex_record/3
   ]).

:- use_module(library(ctypes), [
	is_newline/1,
	is_endfile/1
   ]).

runtime_entry(start) :- go.
runtime_entry(abort) :- nl, halt.

:- initialization go.
:- dynamic ofs/2.

go :-
    current_input(Stream),

    repeat,
	character_count(Stream, CharCount),
	line_count(Stream, LineCount),
	retractall(ofs(_, _)),
	assertz(ofs(CharCount, LineCount)),

	(   read_lex_record(Stream, Record) ->
		(   fm_lexical_record(_, _EUI, Record, []) ->
			fail
		    ;   report_fm_failure(Record), fail
		)
	    ;	(   is_endfile(_)
		;   report_read_failure
		)
	),
    halt.

%%% reports a failure to convert the record
report_fm_failure(Chars) :-
    look_for_base(Chars, Base),
    !,
    ofs(CharCount, LineCount),
    format('ERROR: Cannot convert record for: ~a [~d|~d]~n', [Base, CharCount, LineCount]).
report_fm_failure(_Chars) :-
    ofs(CharCount, LineCount),
    format('ERROR: Cannot convert record for: <> [~d|~d]~n', [CharCount, LineCount]).

%%% reports a failure to read a record
report_read_failure :-
    ofs(CharCount, LineCount),
    format('ERROR: Cannot read record: [~d|~d]~n', [CharCount, LineCount]).

%%% looks for base
look_for_base(Chars, Base) :-
    append("{base=", List, Chars),
    chars_to_newline(BL, List, _),
    !,
    atom_codes(Base, BL).

%%% chars_to_newline - returns all characters till a newline
chars_to_newline([C|R]) -->
    [C],
    { \+ is_newline(C) },
    chars_to_newline(R).
chars_to_newline([]) --> [].
