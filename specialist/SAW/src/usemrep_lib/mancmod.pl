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

% File:	    mancmod.pl
% Module:   mancmod
% Author:   Halil
% Purpose:  access Olivier's database of Metathesaurus ancestors


% ----- Module declaration and exported predicates

:- module( mancmod,   [ meta_ancestors/3,
			initialize_db_access_for_genspec/1,
			stop_db_access_for_genspec/0
	 ]).

:- use_module( library(system), [
	environ/2
   ]).

:- use_module( skr_lib(sicstus_utils), [
	substring/4
   ]).

:- dynamic db_access_status_for_genspec/1.

foreign_resource( mancmod,[
        c_genspec,
        c_init_dbs_genspec,
        c_destroy_dbs_genspec
      ]).

foreign(c_genspec,             c, c_genspec(+string, +string, -string)).
foreign(c_init_dbs_genspec,    c, c_init_dbs_genspec(+string, [-integer])).
foreign(c_destroy_dbs_genspec, c, c_destroy_dbs_genspec).


% genspec now lives locally
%:- load_foreign_resource('../mancmod').
:- environ('DYNAMIC_LIB_DIR',DynamicLibDir),
   atom_concat(DynamicLibDir,'/mancmod',MancmodSo),
   load_foreign_resource(MancmodSo).

%:- abolish(foreign_file,2).
%:- abolish(foreign,3).

/*

CUI1AtomIn represents the first CUI atom
CUI2AtomIn represents the second CUI atom
GenSpecAtomOut is the answer. It either has the format:
'CUI CUI2', where  CUI is the specific concept and CUI2 is the general concept
or
'None.', if there is no parent-child relationship between the two concepts.

*/

% ----- Meta Ancestors

meta_ancestors(CUI1AtomIn,CUI2AtomIn,GenSpecAtomOut) :-
	db_access_status_for_genspec(initialized),
	!,
	substring(CUI1AtomIn,'C',0,1),
	substring(CUI2AtomIn,'C',0,1),
	c_genspec(CUI1AtomIn,CUI2AtomIn,GenSpecAtomOut).
meta_ancestors(_CUI1AtomIn,_CUI2AtomIn,'None.').

initialize_db_access_for_genspec(DataYear) :-
	( db_access_status_for_genspec(initialized) ->
	  true
	; c_init_dbs_genspec(DataYear,DBCount),
	  DBCount > 0, 
	  assert(db_access_status_for_genspec(initialized))
	).

stop_db_access_for_genspec :-
	( db_access_status_for_genspec(initialized) ->
	  retractall(db_access_status_for_genspec(_)),
	  c_destroy_dbs_genspec
	; true
	).
