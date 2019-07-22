
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

% File:	    nls_avl.pl
% Module:   NLS AVL
% Author:   Lan
% Purpose:  Provide additional functionality to the AVL module

:- module(nls_avl,[
    add_to_avl/4,
    add_to_avl_once/4
    ]).


:- use_module(library(avl),[
    avl_change/5,
    avl_fetch/3,
    avl_store/4
    ]).

/* add_to_avl(+Key, +Value, +AVLIn, -AVLOut)

add_to_avl/4 adds Value to the list of values for Key in AVLIn producing
AVLOut.  */

add_to_avl(Key, Value, AVLIn, AVLOut) :-
	% First check if Key is an existing key in the AVL tree.
	( avl_fetch(Key, AVLIn, Values) ->
	  % Next, if Key is an existing key, check if Value is an existing value stored under Key.
	  ( memberchk(Value, Values) ->
	    % If Value is an existing value stored under Key, don't change anything.
	    AVLOut = AVLIn
	  % If Value is NOT an existing value stored under Key, add it.
	  ; avl_change(Key, AVLIn, Values, AVLOut, [Value|Values])
	  )
	% If Key is NOT an exising key in the AVL tree, add it.
	; avl_store(Key, AVLIn, [Value], AVLOut)
	).

/* add_to_avl_once(+Key, +Value, +AVLIn, -AVLOut)

add_to_avl_once/4 is similar to add_to_avl/4 but only allows a single Value
for a given Key. */

add_to_avl_once(Key, Value, AVLIn, AVLOut) :-
	( avl_fetch(Key, AVLIn, _Values) ->
	  AVLOut = AVLIn
	; avl_store(Key, AVLIn, [Value], AVLOut)
	).
