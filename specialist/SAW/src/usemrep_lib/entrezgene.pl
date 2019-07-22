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

% File:	    entrezgene.pl
% Module:   entrezgene
% Author:   Halil
% Purpose:  access EntrezGene database


% ----- Module  declaration and exported predicates

:- module( entrezgene,   [ retrieve_gene_symbol/2,
			   initialize_db_access_for_entrezgene/0,
			   stop_db_access_for_entrezgene/0
	 ]).

:- use_module(library(system), [
	environ/2
   ]).

:- dynamic db_access_status_for_entrezgene/1.

%foreign_resource(retrieve_entrezgene,[
%        c_retrieve_gene,
%        c_init_dbs,
%        c_close_dbs
%      ]).

foreign(c_retrieve_gene, c, c_retrieve_gene(+string, -string)).
foreign(c_init_dbs,      c, c_init_dbs).
foreign(c_close_dbs,     c, c_close_dbs).

foreign_resource(entrezgene,[ 
        c_retrieve_gene, 
        c_init_dbs, 
        c_close_dbs 
      ]).


%:- load_foreign_resource('../entrezgene').
:- environ('DYNAMIC_LIB_DIR',DynamicLibDir),
   atom_concat(DynamicLibDir,'/entrezgene',EntrezGeneSo),
   load_foreign_resource(EntrezGeneSo).

%:- abolish(foreign_file,2).
%:- abolish(foreign,3).

/*

GeneSymbolAtomIn represents the gene symbol isolaed by SemGen.
EntrezGeneAtomOut represents the gene symbols and ids found in
EntrezGene database for this gene symbol.
The format of EntrezGeneAtomOut is 
ids separated by commas|symbols separated by commas
If no symbol is found, the symbol is "None".
*/

% ----- Retrieve EntrezGene symbols

retrieve_gene_symbol(GeneSymbolAtomIn,EntrezGeneAtomOut) :-
    c_retrieve_gene(GeneSymbolAtomIn,EntrezGeneAtomOut).

initialize_db_access_for_entrezgene :-
	( db_access_status_for_entrezgene(initialized) ->
	  true
	; c_init_dbs,
	  assert(db_access_status_for_entrezgene(initialized))
	).

stop_db_access_for_entrezgene :-
	( db_access_status_for_entrezgene(initialized) ->
	  retractall(db_access_status_for_entrezgene(_)),
	  c_close_dbs
	; true
	).
