
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

% File:	    nls_signal.pl
% Module:   NLS Signal
% Author:   Lan
% Purpose:  Provide signal handling


:- module(nls_signal,[
	% called by MetaMap API -- do not change signature!
	establish_signal_handling/0
   ]).

:- use_module(library(system), [
	environ/2
   ]).

foreign_resource(nls_signal, ['C_establish_signal_handling']).

foreign('C_establish_signal_handling', c,
        'C_establish_signal_handling'([-integer])).

% :- load_foreign_resource('../nls_signal').
:- environ('DYNAMIC_LIB_DIR',DynamicLibDir),
   atom_concat(DynamicLibDir,'/nls_signal',NlsSignalSo),
   load_foreign_resource(NlsSignalSo).

% :- abolish(foreign_resource/2, [force(true)]).

% :- abolish(foreign/3, [force(true)]).

/* establish_signal_handling

establish_signal_handling/0 calls C_establish_signal_handling which calls
signal() to establish signal handlers.  */

establish_signal_handling :-
    'C_establish_signal_handling'(1).
