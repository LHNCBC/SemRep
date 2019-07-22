%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   bb.pl                                                  %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module( library(types), [illarg/3]).

bb_inf( Is, Term, Inf) :-
	bb_inf( Is, Term, Inf, _, 0.001).

bb_inf( Is, Term, Inf, Vertex, Eps) :-
	nf( Eps, ENf),
	nf_constant( ENf, EpsN),
	wait_linear( Term, Nf, bb_inf_internal(Is,Nf,EpsN,Inf,Vertex)).

% ---------------------------------------------------------------------

bb_inf_internal( Is, Lin, Eps, _, _) :-
	bb_intern( Is, IsNf, Eps),
	( bb_delete( incumbent, _) -> true ; true ),
	repair( Lin, LinR),			% bb_narrow ...
	deref( LinR, Lind),
	var_with_def_assign( Dep, Lind),
	determine_active_dec( Lind),
	bb_loop( Dep, IsNf, Eps),
	fail.
bb_inf_internal( _, _, _, Inf, Vertex) :-
	bb_delete( incumbent, InfVal-Vertex),	% GC
	{ Inf =:= InfVal }.

bb_loop( Opt, Is, Eps) :-
	bb_reoptimize( Opt, Inf),
	bb_better_bound( Inf),
	vertex_value( Is, Ivs),
	( bb_first_nonint( Is, Ivs, Eps, Viol, Floor, Ceiling) ->
	    bb_branch( Viol, Floor, Ceiling),
	    bb_loop( Opt, Is, Eps)
	;	
	    round_values( Ivs, RoundVertex),
	    % print( incumbent( Inf-RoundVertex)), nl,
	    bb_put( incumbent, Inf-RoundVertex)
	).

%
% added ineqs may have led to binding
%
bb_reoptimize( Obj, Inf) :- var( Obj), iterate_dec( Obj, Inf).
bb_reoptimize( Obj, Inf) :- nonvar( Obj), Inf = Obj.

bb_better_bound( Inf) :-
	bb_get( incumbent, Inc-_),
	!,
	arith_eval( Inf < Inc).
bb_better_bound( _).

bb_branch( V, U, _) :- { V =< U }.
bb_branch( V, _, L) :- { V >= L }.

vertex_value( [],     []).
vertex_value( [X|Xs], [V|Vs]) :-
	rhs_value( X, V),
	vertex_value( Xs, Vs).

rhs_value( Xn, Value) :- nonvar(Xn), Value=Xn.
rhs_value( Xn, Value) :- var(Xn),
	deref_var( Xn, Xd),
	decompose( Xd, _, R, I),
	arith_eval( R+I, Value).

%
% Need only one as we branch on the first anyway ...
%
bb_first_nonint( [I|Is], [Rhs|Rhss], Eps, Viol, F, C) :-
	( arith_eval( floor(Rhs), Floor),
	  arith_eval( ceiling(Rhs), Ceiling),
	  arith_eval(min(Rhs-Floor,Ceiling-Rhs) > Eps) ->
	    Viol = I,
	    F = Floor,
	    C = Ceiling
	;
	    bb_first_nonint( Is, Rhss, Eps, Viol, F, C)
	).

round_values( [],     []).
round_values( [X|Xs], [Y|Ys]) :-
	arith_eval( round(X), Y),
	round_values( Xs, Ys).

bb_intern( [],	   [],       _).
bb_intern( [X|Xs], [Xi|Xis], Eps) :-
	nf( X, Xnf),
	bb_intern( Xnf, Xi, X, Eps),
	bb_intern( Xs, Xis, Eps).

bb_intern( [],             X, _, _) :- !,
	arith_eval( 0, X).
bb_intern( [v(I,[])],      X, _, Eps) :- !,
	X=I,
	arith_eval(min(I-floor(I),ceiling(I)-I) =< Eps).
bb_intern( [v(One,[V^1])], X, _, _) :-
	arith_eval(One=:=1),
	!,
	V=X,
	bb_narrow_lower( X),
	bb_narrow_upper( X).
bb_intern( _, _, Term, _) :-
	illarg(var, bb_inf(Term,_,_), 1).

bb_narrow_lower( X) :-
	( inf( X, Inf) ->
	    arith_eval( ceiling(Inf), Bound),
	    ( entailed(X>Bound) ->
		{ X >= Bound+1 }
	    ;
		{ X >= Bound }
	    )
	;
	    true
	).

bb_narrow_upper( X) :-
	( sup( X, Sup) ->
	    arith_eval( floor(Sup), Bound),
	    ( entailed(X<Bound) ->
		{ X =< Bound-1 }
	    ;
		{ X =< Bound }
	    )
	;
	    true
	).
