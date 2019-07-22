
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

% File:     loader.pl
% Module:   MetaMap
% Author:   Lan
% Purpose:  Loads SKR for MetaMap
%
% Note that MetaMap is now subsumed by SKR.
% In particular, metamap_fe and metamap have been replaced by skr_fe and skr.

:- use_module(skr(skr_fe), [
	fg/0,
	go/0,
	go/1,
	go/2,
	gt/0
    ]).

:- use_module(skr(skr), [
	stop_skr/0
    ]).

:- use_module(skr_lib(nls_signal), [
	establish_signal_handling/0
    ]).


:- use_module(skr_lib(sicstus_utils), [
	ttyflush/0
    ]).

:- use_module(library(file_systems), [
	file_exists/1,
	file_property/3
    ]).

:- use_module(library(random), [
	random/1,
	setrand/1
    ]).

:- use_module(library(system), [
	datime/1
    ]).

:- use_module(skr_lib(print_chars)).

:- prolog_flag(agc_margin, Current), New is 5*Current, prolog_flag(agc_margin, Current, New).

:- prolog_flag(gc_margin,  Current), New is 5*Current, prolog_flag(gc_margin,  Current, New).

%%% Code provided by Mats Carlsson of SICS to FML via e-mail 03/27/2007:
%%% 
%%% There are two issues:
%%% 
%%% 1. The initial seed of the random number generator is always the
%%%    same. This is by design, so that you can reproduce the run with the
%%%    same result, which is sometimes desirable. To get different
%%%    sequences of random numbers each time, the seed has to be set
%%%    explicitly.
%%% 
%%% 2. There's a bug in maybe/[0,1] that makes it always fail the first
%%%    time, no matter what the seed is.
%%% 
%%% The piece of code below addresses both issues: it computes a random
%%% seed, and it calls maybe/0 once to compensate for the bug.
%%% --Mats
%%%

%%% SICStus version updated by Per Mildner.

:- initialization
	datime(Date),
	Date = datime(A,B,C,D,E,F),
	X is 1 + ((A*D) mod 30000),
	Y is 1 + ((B*E) mod 30000),
	Z is 1 + ((C*F) mod 30000),
	%% high bits matters so make W big
	random(R),
	W is 1 + integer(R*(1<<30)),
	setrand(random(X,Y,Z,W)).

runtime_entry(start) :-
	establish_signal_handling,
	go.
    
runtime_entry(abort) :-
	format(user_error,'~nDisconnecting servers and closing files...',[]),
	ttyflush,
	stop_skr,
	format(user_error,'Done.~n',[]).

pl_to_po :-
	source_file(FilePL),
	compile_to_PO_if_necessary(FilePL),
	fail
      ; true.

compile_to_PO_if_necessary(FilePL) :-
	( sub_atom(FilePL, _, _, _, 'SKR') ->
	  true
	; sub_atom(FilePL, _, _, _, public_mm)
	),
	atom_concat(Prefix, '.pl', FilePL),
	atom_concat(Prefix, '.po', FilePO),
	% UNLESS the PO file exists and is more recent than the PL file,
	% compile the PL file to PO
	( file_exists(FilePO),
	  file_property(FilePL, modify_timestamp, TimeStampPL),
	  file_property(FilePO, modify_timestamp, TimeStampPO),
	  TimeStampPO > TimeStampPL ->
	  true
	; format(user_error, 'Saving ~w to ~w~n', [FilePL,FilePO]),
	  save_files(FilePL, FilePO)
	).
	   
:- pl_to_po.
