%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   geler.pl                                               %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module( geler_q, 
	[
	    geler/2,
	    project_nonlin/3,
	    collect_nonlin/3
	]).

:- use_module( library(atts)).

:- attribute goals/1, all_nonlin/1.

attribute_goal( X, Goals) :-
  get_atts( X, goals(Gs)),
  nonexhausted( Gs, Goals, []),
  Goals = [_|_].
attribute_goal( X, Conj) :-
  get_atts( X, all_nonlin(Goals)),
  l2conj( Goals, Conj).

l2conj( [X|Xs], Conj) :-
  ( Xs = [], Conj = X
  ; Xs = [_|_], Conj = (X,Xc), l2conj( Xs, Xc)
  ).

nonexhausted( run(Mutex,G)) -->
  ( {
      var(Mutex)
    } ->
      [ G ]
  ;
      []
  ).
nonexhausted( (A,B)) -->
  nonexhausted( A),
  nonexhausted( B).

verify_attributes( X, Y, Later) :-
  get_atts( X, goals(Gx)),
  !,
  ( var(Y),
      ( get_atts( Y, goals(Gy)) ->
	  Later = [Gx,Gy],
	  put_atts( Y, -goals(_))
      ;
	  Later = [],
	  put_atts( Y, goals(Gx))
      )
  ; nonvar( Y),
      Later = [Gx]
  ).
verify_attributes( _, _, []).

/*
project_attributes( _, Cvas) :-
  collect_nonlin( Cvas, L, []),
  sort( L, Ls),
  put_atts( _, all_nonlin(Ls)).
*/

%
% called from project.pl
%
project_nonlin( _, Cvas, Reachable) :-
  collect_nonlin( Cvas, L, []),
  sort( L, Ls),
  prolog:term_variables_set( Ls, Reachable),
  put_atts( _, all_nonlin(Ls)).

collect_nonlin( []) --> [].
collect_nonlin( [X|Xs]) -->
  ( {get_atts( X, goals(Gx))} ->
      trans( Gx),
      collect_nonlin( Xs)
  ;
      collect_nonlin( Xs)
  ).

trans( (A,B)) -->
  trans( A),
  trans( B).
trans( run(Mutex,Gs)) -->
  ( {var(Mutex)} ->
       {Mutex = done},
       transg( Gs)
  ;
       []
  ).

transg( (A,B)) --> !,
  transg( A),
  transg( B).
transg( M:G) --> !,
  M:transg( G).
transg( G) --> [ G ].

run( Mutex, _) :- nonvar(Mutex).
run( Mutex, G) :- var(Mutex), Mutex=done, call( G).

:- meta_predicate geler(+,0).
%
geler( Vars, Goal) :-
  attach( Vars, run(_Mutex,Goal)).

attach( [], _).
attach( [V|Vs], Goal) :-
  ( var(V), get_atts( V, goals(Gv)) ->
      put_atts( V, goals((Goal,Gv)))
  ;
      put_atts( V, goals(Goal))
  ),
  attach( Vs, Goal).
