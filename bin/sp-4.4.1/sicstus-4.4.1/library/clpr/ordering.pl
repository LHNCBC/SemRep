%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   ordering.pl                                            %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Collect ordering constraints
% Produce an arrangement via toplogical sorting
%
%

:- use_module( library(types), [illarg/3]).

:- use_module( library(ugraphs), 
	[
	    top_sort/2,
	    add_edges/3,
	    add_vertices/3
	]).

ordering( X) :- var(X), !, fail.
ordering( A>B) :- !, ordering( B<A).
ordering( A<B) :-
  join_class( [A,B], Class),
  class_get_prio( Class, Ga),
  !,
  add_edges( [], [A-B], Gb),			% [] = empty graph
  combine( Ga, Gb, Gc),
  class_put_prio( Class, Gc).
ordering( Pb) :- Pb = [_|Xs],
  join_class( Pb, Class),
  class_get_prio( Class, Ga),
  !,
  ( Xs=[],
     add_vertices( [], Pb, Gb)
  ; Xs=[_|_],
     gen_edges( Pb, Es, []),
     add_edges( [], Es, Gb)
  ),
  combine( Ga, Gb, Gc),
  class_put_prio( Class, Gc).
ordering( _).

arrangement( Class, Arr) :-
  class_get_prio( Class, G),
  normalize( G, Gn),
  top_sort( Gn, Arr),
  !.
arrangement( _, _) :-
	illarg(system(unsatisfiable_ordering), 0, 0).

join_class( [],     _).
join_class( [X|Xs], Class) :-
  ( var(X) ->
      var_intern( X, Class)
  ;
      true
  ),
  join_class( Xs, Class).

combine( Ga, Gb, Gc) :-
  normalize( Ga, Gan),
  normalize( Gb, Gbn),
  ugraphs:graph_union( Gan, Gbn, Gc).

%
% both Ga and Gb might have their internal ordering invalidated
% because of bindings and aliasings
%
normalize( [], []).
normalize( G,  Gsgn) :-
  G=[_|_],
  keysort( G, Gs),
  group( Gs, Gsg),
  normalize_vertices( Gsg, Gsgn).

normalize_vertices( [], 	[]).
normalize_vertices( [X-Xnb|Xs], Res) :-
  ( normalize_vertex( X, Xnb, Xnorm) ->
      Res = [Xnorm|Xsn],
      normalize_vertices( Xs, Xsn)
  ;
      normalize_vertices( Xs, Res)
  ).

%
% get rid of nonvar vertices/edges, and turn V-[V] into V-[]
%
normalize_vertex( X, Nbs, X-Nbsss) :-
  var(X),
  sort( Nbs, Nbss),
  strip_nonvar( Nbss, X, Nbsss).

strip_nonvar( [],     _, []).
strip_nonvar( [X|Xs], Y, Res) :-
  ( X==Y -> strip_nonvar( Xs, Y, Res)
  ; var(X) ->
      Res=[X|Stripped],
      strip_nonvar( Xs, Y, Stripped)
  ; nonvar(X),
      Res=[]					% because Vars<anything
  ).

gen_edges( []) --> [].
gen_edges( [X|Xs]) -->
  gen_edges( Xs, X),
  gen_edges( Xs).

gen_edges( [],	   _) --> [].
gen_edges( [Y|Ys], X) -->
  [ X-Y ],
  gen_edges( Ys, X).

%
% map k-La,k-Lb.... into k-LaLb
%
group( [],	  []).
group( [K-Kl|Ks], Res) :-
  group( Ks, K, Kl, Res).

group( [],	  K, Kl, [K-Kl]).
group( [L-Ll|Ls], K, Kl, Res) :-
  ( K==L ->
      append( Kl, Ll, KLl),
      group( Ls, K, KLl, Res)
  ;
      Res = [K-Kl|Tail],
      group( Ls, L, Ll, Tail)
  ).

