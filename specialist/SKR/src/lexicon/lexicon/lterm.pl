
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

/* lterm.pl - writes a lexical record as a Prolog term.
*/

:- use_module(lexicon(qp_fm_lexrec), [
	fm_lexical_record/4
   ]).

:- use_module(lexicon(qp_recio), [
	read_lex_record/3
   ]).

:- use_module(library(ctypes), [
	is_endfile/1
   ]).

runtime_entry(start) :- go.
runtime_entry(abort) :- nl, halt.

:- initialization go.

go :-
    current_input(Stream),

    repeat,
	(   read_lex_record(Stream, Record) ->
		(   fm_lexical_record(Memory, _EUI, Record, []) ->
			writeq(Memory), write('.'), nl,
			fail
		    ;   format(user_error, 'ERROR: Cannot convert record.~n', []),
			fail
		)
	    ;	(   is_endfile(_)
		;   format(user_error, 'ERROR: Cannot read record.~n', [])
		)
	),
    halt.
