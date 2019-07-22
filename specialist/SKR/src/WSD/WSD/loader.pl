
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

% File:	    loader.pl
% Module:   abs
% Author:   NLS
% Purpose:  Loads abs


:- dynamic program/2.


:- use_module(disamb,[
	go/0
    ]).

:- use_module(skr_lib(sicstus_utils),[
	ttyflush/0
    ]).

:- use_module(library(file_systems),[
	close_all_streams/0
    ]).

:- use_module(skr_lib(nls_signal),[
	establish_signal_handling/0
    ]).

%:- initialization go.

:- assert(program(metamap,'MetaMap')).

runtime_entry(start) :-
    establish_signal_handling,
% Specify MetaMap processing
    retractall(program(_,_)),
    assert(program(metamap,'MetaMap')),
    go.
	
runtime_entry(abort) :-
%    format(user_output,'~nDisconnecting servers and closing files...',[]),
    ttyflush,
%    disconnect_from_all_host_servers,
    close_all_streams,
%    stop_abs,
    format(user_output,'Done~n.',[]).

