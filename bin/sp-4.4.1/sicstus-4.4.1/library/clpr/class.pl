%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   class.pl                                               %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% the class theory
%

:- module( classr, 
	[
	    class_allvars/2,
	    class_new/4,
	    class_drop/2,
	    class_basis/2,
	    class_basis_add/3,
	    class_basis_drop/2,
	    class_basis_pivot/3,

	    ordering/1,
	    arrangement/2
	]).

:- use_module( clpr, '../clpr', [get_or_add_class/2,var_intern/2]).

:- ensure_loaded( ordering).

:- use_module( library(atts)).

:- attribute class_atts/4.


verify_attributes( X, Y, []) :-
  get_atts( X, class_atts(La,Lat,ABasis,PrioA)),
  !,
  var( Y),					% required
  get_atts( Y, class_atts(Lb,Lbt,BBasis,PrioB)),
  Lat = Lb,					% append
  append( ABasis, BBasis, CBasis),
  combine( PrioA, PrioB, PrioC),
  put_atts( Y, class_atts(La,Lbt,CBasis,PrioC)).
verify_attributes( _, _, []).

%
% for the debugger
%
% attribute_goal( V, V:A) :- get_atts( V, A), A = [_|_].


% ----------------------------------------------------------------------------------

class_new( Class, All,AllT, Basis) :-
  put_atts( Su, class_atts(All,AllT,Basis,[])),
  Su = Class.

class_get_prio( Class, Priority) :-
  get_atts( Class, class_atts(_,_,_,Priority)).

class_put_prio( Class, Priority) :-
  get_atts( Class, class_atts(All,AllT,Basis,_)),
  put_atts( Class, class_atts(All,AllT,Basis,Priority)).

class_drop( Class, X) :-
  get_atts( Class, class_atts(Allvars,Tail,Basis,Priority)),
  delete_first( Allvars, X, NewAllvars),
  delete_first( Basis, X, NewBasis),
  put_atts( Class, class_atts(NewAllvars,Tail,NewBasis,Priority)).

class_allvars( Class, All) :- get_atts( Class, class_atts(All,_,_,_)).

class_basis( Class, Basis) :- get_atts( Class, class_atts(_,_,Basis,_)).

class_basis_add( Class, X, NewBasis) :-
  NewBasis = [X|Basis],
  get_atts( Class, class_atts(All,AllT,Basis,Priority)),
  put_atts( Class, class_atts(All,AllT,NewBasis,Priority)).

class_basis_drop( Class, X) :-
  get_atts( Class, class_atts(All,AllT,Basis0,Priority)),
  delete_first( Basis0, X, Basis),
  Basis0 \== Basis,			% anything deleted ?
  !,
  put_atts( Class, class_atts(All,AllT,Basis,Priority)).
class_basis_drop( _, _).

class_basis_pivot( Class, Enter, Leave) :-
  get_atts( Class, class_atts(All,AllT,Basis0,Priority)),
  delete_first( Basis0, Leave, Basis1),
  put_atts( Class, class_atts(All,AllT,[Enter|Basis1],Priority)).

%
% remove the first occurence
%
delete_first( L,      _, Res) :- var(L), !, Res = L.
delete_first( [],     _, []).
delete_first( [Y|Ys], X, Res) :-
  ( X==Y ->
      Res = Ys
  ;
      Res = [Y|Tail],
      delete_first( Ys, X, Tail)
  ).
