
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

/* qp_fm_utils.pl - some utility predicates.
*/

:- module(qp_fm_util, [
	fm_to_newline/3,
	fm_to_comma/3,
	fm_to_pipe/3,
	fm_to_char/4,
	fm_to_rparen/3,
	fm_newline/2,
	fm_spaces/2
   ]).

:- use_module(skr_lib(ctypes), [
	is_newline/1
   ]).

%%% fm_to_newline - returns all characters till a newline
fm_to_newline([C|R]) -->
	[C],
	{ \+ is_newline(C) },
	!,
	fm_to_newline(R).
fm_to_newline([]) --> [].

%%% all characters upto ',' or newline
fm_to_comma(S) -->
	    fm_to_char(0',, S).

%%% all characters upto '|' or newline   *** due to change in lexicon syntax in 2000
fm_to_pipe(S) -->
	    fm_to_char(0'|, S).


%%% all characters upto ')' or newline
fm_to_rparen(S) -->
	    fm_to_char(0'), S).

%%% fm_to_char - All characters upto (not including) 'S' or newline
fm_to_char(S, [C|R]) -->
	[C],
	{ \+ is_newline(C), C \== S },
	!,
	fm_to_char(S, R).
fm_to_char(_, []) --> [].

%%% absorbs zero or more consecutive spaces or tabs upto a new line.
fm_spaces --> [32], !, fm_spaces.
fm_spaces --> [9], !, fm_spaces.
fm_spaces --> [], !.

%%% absorbs a newline in the input or generates one on output.
fm_newline --> [10].
