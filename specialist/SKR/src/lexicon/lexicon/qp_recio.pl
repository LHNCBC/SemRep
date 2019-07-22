
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

/* qp_recio.pl - Reads in lexical records
*/

:- module(qp_recio, [
	read_lex_record/3
    ]).

:- use_module(skr_lib(ctypes), [
	is_newline/1,
	is_endfile/1
   ]).

%%% Reads in record at given offset from a lexicon file.
read_lex_record(Lexicon, Ofs, Rec) :-
	open(Lexicon, read, Stream, [reposition(true)]),
	seek(Stream, Ofs, bof, _New),
	read_lex_record_aux(Stream, Rec, []),
	close(Stream).

read_lex_record_aux(S) -->
    { get_code(S,C) },
    (	{ is_endfile(C) } ->
	    { !, fail }
	;   { true }
    ),
    ( { is_newline(C) } ->
	[C],
	{ get_code(S,C1) },
	(   { is_endfile(C) } ->
		{ !, fail }
	    ;	{ true }
	),
	( { is_eor(C1) } ->
	    [C1],
	    { get_code(S,C2) },
	    (	{ is_endfile(C) } ->
		    { !, fail }
		;   { true }
	    ),
	    ( { is_newline(C2) } ->
		[C2], !
	    ; [C2], !, read_lex_record_aux(S)
	    )
	; [C1], !, read_lex_record_aux(S)
	)
    ; [C], !, read_lex_record_aux(S)
    ).

is_eor(0'}).

