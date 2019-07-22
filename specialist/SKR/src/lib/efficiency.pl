
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

% File:	    efficiency.pl
% Module:   Efficiency
% Author:   Lan
% Purpose:  Provide predicates for enhancing efficiency


:- module(efficiency,[
	% maybe_atom_gc/2,
     	maybe_atom_gc/3
   ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2,
	send_message/2
    ]).

:- dynamic atom_gc_threshold/1.

/* maybe_atom_gc(-DidGC, -SpaceCollected)
   maybe_atom_gc(+Notify, -DidGC, -SpaceCollected)

   maybe_atom_gc/3 performs garbage_collect_atoms/0 if the amount of atom
   space in use exceeds a threshold gotten from atom_gc_threshold/1.
   It returns whether garbage collection was performed and the amount of
   space collected, if any.  Also, if Notify has value 'yes', details
   of the collection are displayed.
   maybe_atom_gc/2 calls maybe_atom_gc/3 with Notify set to 'no'.  */

% maybe_atom_gc(DidGC, SpaceCollected) :-
% 	maybe_atom_gc(no, other, DidGC, SpaceCollected).

maybe_atom_gc(Call, DidGC, SpaceCollected) :-
		maybe_atom_gc(no, Call, DidGC, SpaceCollected).

maybe_atom_gc(Notify, Call, DidGC, SpaceCollected) :-
	atom_gc_threshold(Threshold),
	statistics(atoms, [NAtoms0,SpaceInUse0,SpaceFree0|_]),
	( SpaceInUse0 > Threshold ->
	  maybe_notify_1(Notify, Call, Threshold, NAtoms0, SpaceInUse0, SpaceFree0),
	  garbage_collect_atoms,
	  DidGC = yes,
	  statistics(atoms, [NAtoms,SpaceInUse,SpaceFree|_]),
	  SpaceCollected is SpaceInUse0 - SpaceInUse,
	  maybe_notify_2(Notify, Call, NAtoms, SpaceInUse, SpaceFree, SpaceCollected),
	  NewThreshold is SpaceInUse + (SpaceFree // 2),
	  maybe_update_threshold(Notify, Call, NewThreshold)
	; DidGC = no,
	  SpaceCollected = 0
	),
	!.
maybe_atom_gc(_Notify, Call, _DidGC, _SpaceCollected) :-
    fatal_error('maybe_atom_gc/3 call ~w did not finish normally.~n', [Call]).


maybe_notify_1(Notify, Call, Threshold, NAtoms0, SpaceInUse0, SpaceFree0) :-
	( Notify == yes ->
          send_message('~Nmaybe_atom_gc call ~w threshold = ~d~n',[Call,Threshold]),
          TotalSpace0 is SpaceInUse0 + SpaceFree0,
          send_message('~Nmaybe_atom_gc before call ~w = ~d  ~d + ~d = ~d~n',
		       [Call,NAtoms0,SpaceInUse0,SpaceFree0,TotalSpace0])
        ; true
        ).

maybe_notify_2(Notify, Call, NAtoms, SpaceInUse, SpaceFree, SpaceCollected) :-
        ( Notify == yes ->
          TotalSpace is SpaceInUse + SpaceFree,
          send_message('~Nmaybe_atom_gc  after call ~w = ~d  ~d + ~d = ~d (~d)~n',
		       [Call,NAtoms,SpaceInUse,SpaceFree,TotalSpace,SpaceCollected])
        ; true
        ).

maybe_notify_3(Notify, Call, NewThreshold) :-
	( Notify == yes ->
	  send_message('~Nmaybe_atom_gc new threshold call ~w = ~d~n~n',
		       [Call,NewThreshold])
	; true
	).

maybe_update_threshold(Notify, Call, NewThreshold) :-
	( atom_gc_threshold(NewThreshold) ->
	  true
	; retractall(atom_gc_threshold(_)),
	  assert(atom_gc_threshold(NewThreshold)),
	  maybe_notify_3(Notify, Call, NewThreshold)
	).


/* atom_gc_threshold(?Threshold)

atom_gc_threshold/1 is a factual predicate with one clause indicating the
threshold to be used for atom garbage collection.  It is initially 0 but
is recomputed after each garbage collection.  It is set to
                     <base> + 1/2*<free>
where <base> is the amount of atom space in use and <free> the amount free
after the garbage collection.  */

atom_gc_threshold(0).
