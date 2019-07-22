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
***************************************************************************/

/*
% File:	    postlex.pl
% Module:   postlex
% Author:   psg
% Purpose:  Code for post-lexical substitutions

POST-LEXICAL SUBSTITUTIONS operates on the output of mincoman.
It changes certain known problematic patterns into other patterns,
using context to avoid undesirable changes.

% The actual substitutions are defined in regexp, because it
%		has the 'transform' predicate definitions.
% The code to match and replace has itself been replaced
%		by code in regexp.
% This once-glorious file is now left with nothing but a kludge
%		around an even uglier kludge.
*/


% ----- Module declaration and exported predicates

:- module( postlex,   [
	revert/2
   ]).

% ----- Imported predicates

:- use_module( library(basics),     [
	member/2,
	memberchk/2
   ]).

%:- library_directory( '/home/goetzp/specialist//SKR/src/lib').
% :- file_search_path(skr_lib,'/home/goetzp/specialist/SKR/src/lib').


% Revert a semantic analysis to a syntactic analysis.
% This is a hack done to kludge around a kludge in
%		perform_referential_analysis, in which SemanticAnalysis is
%		derived from SyntacticAnalysis, but SyntacticAnalysis is
%		inexplicably saved and later used, and the program crashes
%		if it no longer has the same # of MSUs as SemanticAnalysis,
%		which it won't after running postlex transforms.
% Remove confid([X]) from MSUs
% Remove usemtype, ausemtype, semgroup, metaconc from words.
revert([], []).
revert([A | ATail], [B | BTail]) :-
	revertMSU(A, B),
	revert(ATail, BTail).

revertMSU([], []).
% Strip out confid(_).
revertMSU([confid(_) | ATail], BTail) :-
	revertMSU(ATail, BTail).
revertMSU([A | ATail], [B | BTail]) :-
	revertWord(A, B),
	revertMSU(ATail, BTail).

revertWord(Word0, Word1) :-
	Word0 =.. [Type, Args0],
	Word1 =.. [Type, Args1],
	revertArgs(Args0, Args1).

revertArgs([], []).
revertArgs([A | ATail], BTail) :-
	A =.. [Feature, _],
	member(Feature, [index, usemtype, ausemtype, semgroup, metaconc]),
	!,
	revertArgs(ATail, BTail).
revertArgs([A | ATail], [A | BTail]) :-
	revertArgs(ATail, BTail).
