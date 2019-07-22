%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   itf3.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% interface to attribute encoding and hooks
%

:- use_module( library(types), [illarg/3]).

:- use_module( library(atts)).

:- attribute	class/1, order/1, lin/1, forward/1,
		type/1, strictness/1, nonzero/0,
		target/0, keep_indep/0, keep/0. % project.pl

/* Moved here from store.pl to break cyclic dependencies. --Mats C. */
%
% critical impact on the backsubstitution effort
% AND precision in clp(r)
%
% nf_ordering( A, B, Rel) :-
%   get_atts( A, order( Oa)),
%   get_atts( B, order( Ob)),
%   compare( Rel, Oa, Ob).

%
goal_expansion( nf_ordering(A,B,Rel), _Lay, Module, Exp, []) :-
        clpqr( Module),
	Exp = (
		  get_atts( A, order(Oa)),
		  get_atts( B, order(Ob)),
		  compare( Rel, Oa, Ob)
	      ).
goal_expansion( decompose(Lin,H,R,I), _Lay, Module, Lin=[I,R|H], []) :-
	clpqr( Module).

clpqr( clpq).
clpqr( clpr).
/* End of code from store.pl */

%
% Parametrize the answer presentation mechanism
% (toplevel,compiler/debugger ...)
%
:- dynamic presentation_context/1.

presentation_context( Old, New) :-
  clause( presentation_context(Current), _),
  !,
  Current = Old,
  retractall( presentation_context(_)),
  assert( presentation_context( New)).
presentation_context( toplevel, New) :- 	% default
  assert( presentation_context( New)).

%
% attribute_goal( V, V:Atts) :- get_atts( V, Atts).
%
attribute_goal( V, Goal) :-
  presentation_context( Cont, Cont),
  dump_linear( V, Cont, Goals, Gtail),
  dump_nonzero( V, Cont, Gtail, []),
  l2wrapped( Goals, Goal).

l2wrapped( [],	   true).
l2wrapped( [X|Xs], Conj) :-
  ( Xs = [],	wrap( X, Conj)
  ; Xs = [_|_], wrap( X, Xw),
		Conj = (Xw,Xc),
		l2wrapped( Xs, Xc)
  ).

%
% Tests should be pulled out of the loop ...
%
wrap( C, W) :-
  prolog_flag(typein_module, Module),
  this_linear_solver( Solver),
  ( Module == Solver ->
      W = {C}
  ; predicate_property( Module:{_}, imported_from(Solver)) ->
      W = {C}
  ;
      W = Solver:{C}
  ).

dump_linear( V, Context) -->
  {
    get_atts( V, [lin(Lin),type(Type)]),
    !,
    decompose( Lin, H, _, I)
  },
  %
  % This happens if not all target variables can be made independend
  % Example: examples/option.pl:
  % | ?- go2(S,W).
  %
  % W = 21/4,
  % S>=0,
  % S<50 ? ;
  %
  % W>5,
  % S=221/4-W,		  this line would be missing !!!
  % W=<21/4
  %
  ( { Type=t_none ; get_atts( V, -target) } -> [] ; dump_v( Context, t_none, V, I, H) ),
  %
  ( {Type=t_none, get_atts( V, -target) } ->	% nonzero produces such
       []
  ;
       dump_v( Context, Type, V, I, H)
  ).
dump_linear( _, _) --> [].

dump_v( toplevel, Type, V, I, H) --> dump_var( Type, V, I, H).
dump_v( compiler, Type, V, I, H) --> compiler_dump_var( Type, V, I, H).

dump_nonzero( V, Cont) -->
  {
    get_atts( V, [nonzero,lin(Lin)]),
    !,
    decompose( Lin, H, _, I)
  },
  dump_nz( Cont, V, H, I).
dump_nonzero( _, _) --> [].

dump_nz( toplevel, V, H, I) --> dump_nz( V, H, I).
dump_nz( compiler, V, H, I) --> compiler_dump_nz( V, H, I).

numbers_only( Y, _) :- var(Y), !.
numbers_only( Y, _) :- arith_normalize( Y, Y), !.
numbers_only( Y, X) :-
	this_linear_solver( Solver),
	solver_type(Solver, Type),
	illarg(type(Type), X=Y, 2).

solver_type(clpq, rational).
solver_type(clpr, float).	     

verify_attributes( X, _, []) :-
  get_atts(X, [-class(_),-order(_),-lin(_),-forward(_),-type(_),-strictness(_),
	       -nonzero]),
  !.
verify_attributes( X, Y, []) :-
  get_atts( X, forward(F)),
  !,
  fwd_deref( F, Y).
verify_attributes( X, Y, Later) :-
  numbers_only( Y, X),
  put_atts( X, forward(Y)),
  verify_nonzero( X, Y),
  verify_type( X, Y, Later, []),
  verify_lin( X, Y).

fwd_deref( X, Y) :- nonvar(X), X=Y.
fwd_deref( X, Y) :- var(X),
	( get_atts( X, forward(F)) ->
	    fwd_deref( F, Y)
	;
	    X = Y
	).

verify_nonzero( X, Y) :-
  get_atts( X, nonzero),
  !,
  ( var(Y) ->
      put_atts( Y, nonzero)
  ;
      arith_eval( Y =\= 0)
  ).
verify_nonzero( _, _).

verify_type( X, Y) -->
  {
    get_atts( X, [type(Type),strictness(Strict)])
  },
  !,
  verify_type( Y, Type, Strict).
verify_type( _, _) --> [].

verify_type( Y, TypeX, StrictX) --> {var(Y)}, !,
  verify_type_var( TypeX, Y, StrictX).
verify_type( Y, TypeX, StrictX) -->
  {
    verify_type_nonvar( TypeX, Y, StrictX)
  }.

  verify_type_nonvar( t_none,	 _,	_).
  verify_type_nonvar( t_l(L),	 Value, S) :- lb( S, L, Value).
  verify_type_nonvar( t_u(U),	 Value, S) :- ub( S, U, Value).
  verify_type_nonvar( t_lu(L,U), Value, S) :- lb( S, L, Value), ub( S, U, Value).
  verify_type_nonvar( t_L(L),	 Value, S) :- lb( S, L, Value).
  verify_type_nonvar( t_U(U),	 Value, S) :- ub( S, U, Value).
  verify_type_nonvar( t_Lu(L,U), Value, S) :- lb( S, L, Value), ub( S, U, Value).
  verify_type_nonvar( t_lU(L,U), Value, S) :- lb( S, L, Value), ub( S, U, Value).

  lb( S, L, V) :- S /\ 0b10 =:= 0, !, arith_eval( L =< V).
  lb( _, L, V) :-		      arith_eval( L  < V).

  ub( S, U, V) :- S /\ 0b01 =:= 0, !, arith_eval( V =< U).
  ub( _, U, V) :-		      arith_eval( V  < U).


%
% Running some goals after X=Y simplifies the coding. It should be possible
% to run the goals here and taking care not to put_atts/2 on X ...
%
  verify_type_var( t_none,    _, _) --> [].
  verify_type_var( t_l(L),    Y, S) --> llb( S, L, Y).
  verify_type_var( t_u(U),    Y, S) --> lub( S, U, Y).
  verify_type_var( t_lu(L,U), Y, S) --> llb( S, L, Y), lub( S, U, Y).
  verify_type_var( t_L(L),    Y, S) --> llb( S, L, Y).
  verify_type_var( t_U(U),    Y, S) --> lub( S, U, Y).
  verify_type_var( t_Lu(L,U), Y, S) --> llb( S, L, Y), lub( S, U, Y).
  verify_type_var( t_lU(L,U), Y, S) --> llb( S, L, Y), lub( S, U, Y).

  llb( S, L, V) --> {S /\ 0b10 =:= 0}, !, [ {L =< V} ].
  llb( _, L, V) -->			  [ {L	< V} ].

  lub( S, U, V) --> {S /\ 0b01 =:= 0}, !, [ {V =< U} ].
  lub( _, U, V) -->			  [ {V	< U} ].


%
% We used to drop X from the class/basis to avoid trouble with subsequent
% put_atts/2 on X. Now we could let these dead but harmless updates happen.
% In R however, exported bindings might conflict, e.g. 0 \== 0.0
%
% If X is indep and we do _not_ solve for it, we are in deep shit
% because the ordering is violated.
%
verify_lin( X, Y) :-
  get_atts( X, [class(Class),lin(LinX)]),
  !,
  ( indep( LinX, X) ->
      detach_bounds( X),		% if there were bounds, they are requeued already
      class_drop( Class, X),
      nf( X-Y, Lin),
      deref( Lin, Lind),
      ( nf_coeff_of( Lind, X, _) ->
	  solve_x( Lind, X)
      ;
	  solve( Lind)
      )
  ;
      class_drop( Class, X),
      nf( X-Y, Lin),
      deref( Lin, Lind),
      solve( Lind)
  ).
verify_lin( _, _).







