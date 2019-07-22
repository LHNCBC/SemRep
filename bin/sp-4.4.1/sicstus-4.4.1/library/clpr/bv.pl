%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   bv.pl                                                  %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
%  simplex with bounded variables, ch, 93/12
%

%
% TODO: +) var/bound/state classification and maintainance
%	+) inc/dec_step: take the best?, at least find unconstrained var first
%	+) trivially implied values
%	+) avoid eval_rhs through an extra column (Coeff=Rhs)
%	+) if an optimum is encountered, record the value as bound !!!
%	+) generalized (transparent) attribute handling
%	+) coordinate reconsideration cascades
%	+) =\=
%	+) strict inequalities via =\=
%	-) decompose via nonvar test -> no symbolic constants any more ?
%	   constants complicate the nonlin solver anyway ...
%	+) join t_l,l(L), .... into t_l(L), ...
%	+) shortcuts for strict ineqs
%	-) extra types for vars with l/u bound zero
%	-) occurrence lists for indep vars (with coeffs) ???
%		each solve produces one dep var -> push
%		only complication: pivots
%	-) *incremental* REVISED simplex ?!!
%
% sicstus2.1.9.clp conversion:
%
%	-) stable ordering through extra attribute ...
%	   interpreted vs compiled yields different var order
%	   -> nasty in R (need different eps)
%
%	-) check determinism again
%
%

:- public {}/1, maximize/1, minimize/1, sup/2, inf/2, imin/2. % xref.pl

:- use_module( library(ordsets), [ord_add_element/3]).

% :- use_module( library(deterministic)).

%
% For the rhs maint. the following events are important:
%
%	-) introduction of an indep var at active bound B
%	-) narrowing of active bound
%	-) swap active bound
%	-) pivot
%

%
% a variables bound (L/U) can have the states:
%
%	-) t_none
%	-) t_l			has a lower bound (not active yet)
%	-) t_u
%	-) t_L			has an active lower bound
%	-) t_U
%	-) t_lu
%	-) t_Lu
%	-) t_lU
%

% ----------------------------------- deref ------------------------------------ %

:- mode deref( +, -).
%
deref( Lin, Lind) :-
  split( Lin, H, I),
  normalize_scalar( I, Nonvar),
  length( H, Len),
  log_deref( Len, H, [], Restd),
  add_linear_11( Nonvar, Restd, Lind).

:- mode log_deref( +, +, -, -).
%
log_deref( 0, Vs,	      Vs, Lin) :- !,
  arith_eval( 0, Z),
  Lin = [Z,Z].
log_deref( 1, [v(K,[X^1])|Vs],	     Vs, Lin) :- !,
  deref_var( X, Lx),
  mult_linear_factor( Lx, K, Lin).
log_deref( 2, [v(Kx,[X^1]),v(Ky,[Y^1])|Vs], Vs, Lin) :- !,
  deref_var( X, Lx),
  deref_var( Y, Ly),
  add_linear_ff( Lx, Kx, Ly, Ky, Lin).
log_deref( N, V0,	      V2, Lin) :-
  P is N >> 1,
  Q is N - P,
  log_deref( P, V0,V1, Lp),
  log_deref( Q, V1,V2, Lq),
  add_linear_11( Lp, Lq, Lin).

/*
%
% tail recursive version
%
deref( Lin, Lind) :-
  split( Lin, H, I),
  normalize_scalar( I, Nonvar),
  lin_deref( H, Nonvar, Lind).

log_deref( _, Lin, [], Res) :-			% called from nf.pl
  arith_eval( 0, Z),
  lin_deref( Lin, [Z,Z], Res).

lin_deref( [],		    Ld, Ld).
lin_deref( [v(K,[X^1])|Vs], Li, Lo) :-
  deref_var( X, Lx),
  add_linear_f1( Lx, K, Li, Lii),
  lin_deref( Vs, Lii, Lo).
*/

%
% If we see a nonvar here, this is a fault
%
deref_var( X, Lin) :-
  get_atts( X, lin(Lin)), !.
deref_var( X, Lin) :-				% create a linear var
  arith_eval( 0, Z),
  arith_eval( 1, One),
  Lin = [Z,Z,X*One],
  put_atts( X, [order(_),lin(Lin),type(t_none),strictness(0b00)]).

var_with_def_assign( Var, Lin) :-
  decompose( Lin, Hom, _, I),
  ( Hom = [],					% X=k
      Var = I
  ; Hom = [V*K|Cs],
      ( Cs = [],
	arith_eval(K=:=1),
	arith_eval(I=:=0) ->			% X=Y
	  Var = V
      ; 					% general case
	  var_with_def_intern( t_none, Var, Lin, 0b00)
      )
  ).

var_with_def_intern( Type, Var, Lin, Strict) :-
  put_atts( Var, [order(_),lin(Lin),type(Type),strictness(Strict)]),
  decompose( Lin, Hom, _, _),
  get_or_add_class( Var, Class),
  same_class( Hom, Class).

var_intern( Type, Var, Strict) :-
  arith_eval( 0, Z),
  arith_eval( 1, One),
  Lin = [Z,Z,Var*One],
  put_atts( Var, [order(_),lin(Lin),type(Type),strictness(Strict)]),
  get_or_add_class( Var, _Class).

var_intern( Var, Class) :-			  % for ordered/1 but otherwise free vars
  get_atts( Var, [lin(_),type(_)]),
  !,
  get_or_add_class( Var, Class).	
var_intern( Var, Class) :-
  arith_eval( 0, Z),
  arith_eval( 1, One),
  put_atts( Var, [order(_),lin([Z,Z,Var*One]),type(t_none),strictness(0b00)]),
  get_or_add_class( Var, Class).

% ------------------------------------------------------------------------------

%
% [V-Binding]*
% Only place where the linear solver binds variables
%
export_binding( []).
export_binding( [X-Y|Gs]) :-
  export_binding( Y, X),
  export_binding( Gs).

%
% numerical stabilizer, clp(r) only
%
export_binding( Y, X) :- var(Y), Y=X.
export_binding( Y, X) :- nonvar(Y),
  ( arith_eval( Y=:=0) ->
      arith_eval( 0, X)
  ;
      Y = X
  ).

'solve_='( Nf) :-
  deref( Nf, Nfd),
  solve( Nfd).

'solve_=\\='( Nf) :-
  deref( Nf, Lind),
  decompose( Lind, Hom, _, Inhom),
  ( Hom = [],	 arith_eval( Inhom =\= 0)
  ; Hom = [_|_], var_with_def_intern( t_none, Nz, Lind, 0b00),
		 put_atts( Nz, nonzero)
  ).

'solve_<'( Nf) :-
  split( Nf, H, I),
  ineq( H, I, Nf, strict).

'solve_=<'( Nf) :-
  split( Nf, H, I),
  ineq( H, I, Nf, nonstrict).

maximize( Term) :-
	minimize( -Term).

%
% This is NOT coded as minimize(Expr) :- inf(Expr,Expr).
%
% because the new version of inf/2 only visits
% the vertex where the infimum is assumed and returns
% to the 'current' vertex via backtracking.
% The rationale behind this construction is to eliminate
% all garbage in the solver data structures produced by
% the pivots on the way to the extremal point caused by
% {inf,sup}/{2,4}.
%
% If we are after the infimum/supremum for minimizing/maximizing,
% this strategy may have adverse effects on performance because
% the simplex algorithm is forced to re-discover the
% extremal vertex through the equation {Inf =:= Expr}.
%
% Thus the extra code for {minimize,maximize}/1.
%
% In case someone comes up with an example where
%
%   inf(Expr,Expr)
%
% outperforms the provided formulation for minimize - so be it.
% Both forms are available to the user.
%
minimize( Term) :-
	wait_linear( Term, Nf, minimize_lin(Nf)).

minimize_lin( Lin) :-
	deref( Lin, Lind),
	var_with_def_intern( t_none, Dep, Lind, 0b00),
	determine_active_dec( Lind),
	iterate_dec( Dep, Inf),
	{ Dep =:= Inf }.

sup( Expression, Sup) :-
	sup( Expression, Sup, [], []).

sup( Expression, Sup, Vector, Vertex) :-
	inf( -Expression, -Sup, Vector, Vertex).

inf( Expression, Inf) :-
	inf( Expression, Inf, [], []).

inf( Expression, Inf, Vector, Vertex) :-
	wait_linear( Expression, Nf, inf_lin(Nf,Inf,Vector,Vertex)).

inf_lin( Lin, _, Vector, _) :-
	deref( Lin, Lind),
	var_with_def_intern( t_none, Dep, Lind, 0b00),
	determine_active_dec( Lind),
	iterate_dec( Dep, Inf),
	vertex_value( Vector, Values),
	bb_put( inf, [Inf|Values]),
	fail.
inf_lin( _, Infimum, _, Vertex) :-
	bb_delete( inf, L),
	assign( [Infimum|Vertex], L).

assign( [],     []).
assign( [X|Xs], [Y|Ys]) :-
	{X =:= Y},				  % more defensive/expressive than X=Y
	assign( Xs, Ys).

% --------------------------------- optimization ------------------------------- %
%
% The _sn(S) =< 0 row might be temporarily infeasible.
% We use reconsider/1 to fix this.
%
%   s(S) e [_,0] = d +xi ... -xj, Rhs > 0 so we want to decrease s(S)
%
%   positive xi would have to be moved towards their lower bound,
%   negative xj would have to be moved towards their upper bound,
%
%   the row s(S) does not limit the lower bound of xi
%   the row s(S) does not limit the upper bound of xj
%
%   a) if some other row R is limiting xk, we pivot(R,xk),
%      s(S) will decrease and get more feasible until (b)
%   b) if there is no limiting row for some xi: we pivot(s(S),xi)
%					    xj: we pivot(s(S),xj)
%      which cures the infeasibility in one step
%


%
% fails if Status = unlimited/2
%
iterate_dec( OptVar, Opt) :-
  get_atts( OptVar, lin(Lin)),
  decompose( Lin, H, R, I),

  % arith_eval( R+I, Now), print(min(Now)), nl,

  % dec_step_best( H, Status),
  dec_step( H, Status),
  ( Status = applied, iterate_dec( OptVar, Opt)
  ; Status = optimum, arith_eval( R+I, Opt)
  ).

iterate_inc( OptVar, Opt) :-
  get_atts( OptVar, lin(Lin)),
  decompose( Lin, H, R, I),
  inc_step( H, Status),
  ( Status = applied, iterate_inc( OptVar, Opt)
  ; Status = optimum, arith_eval( R+I, Opt)
  ).

%
% Status = {optimum,unlimited(Indep,DepT),applied}
% If Status = optimum, the tables have not been changed at all.
% Searches left to right, does not try to find the 'best' pivot
% Therefore we might discover unboundedness only after a few pivots
%
dec_step( [],	    optimum).
dec_step( [V*K|Vs], Status) :-
  get_atts( V, type(W)),
  ( W = t_U(U),
      ( arith_eval( K > 0) ->
	 ( lb( V, Vub-Vb-_) ->
	     Status = applied,
	     pivot_a(Vub,V,Vb,t_u(U))
	 ;
	     Status = unlimited(V,t_u(U))
	 )
      ;
	 dec_step( Vs, Status)
      )
  ; W = t_lU(L,U),
      ( arith_eval( K > 0) ->
	 Status = applied,
	 arith_eval( L-U, Init),
	 basis( V, Deps),
	 lb( Deps, V, V-t_Lu(L,U)-Init, Vub-Vb-_),
	 pivot_b(Vub,V,Vb,t_lu(L,U))
      ;
	 dec_step( Vs, Status)
      )
  ; W = t_L(L),
      ( arith_eval( K < 0) ->
	 ( ub( V, Vub-Vb-_) ->
	     Status = applied,
	     pivot_a(Vub,V,Vb,t_l(L))
	 ;
	     Status = unlimited(V,t_l(L))
	 )
      ;
	 dec_step( Vs, Status)
      )
  ; W = t_Lu(L,U),
      ( arith_eval( K < 0) ->
	 Status = applied,
	 arith_eval( U-L, Init),
	 basis( V, Deps),
	 ub( Deps, V, V-t_lU(L,U)-Init, Vub-Vb-_),
	 pivot_b(Vub,V,Vb,t_lu(L,U))
      ;
	 dec_step( Vs, Status)
      )
  ; W = t_none,
      Status = unlimited(V,t_none)
  ).

inc_step( [],	    optimum).
inc_step( [V*K|Vs], Status) :-
  get_atts( V, type(W)),
  ( W = t_U(U),
      ( arith_eval( K < 0) ->
	 ( lb( V, Vub-Vb-_) ->
	     Status = applied,
	     pivot_a(Vub,V,Vb,t_u(U))
	 ;
	     Status = unlimited(V,t_u(U))
	 )
      ;
	 inc_step( Vs, Status)
      )
  ; W = t_lU(L,U),
      ( arith_eval( K < 0) ->
	 Status = applied,
	 arith_eval( L-U, Init),
	 basis( V, Deps),
	 lb( Deps, V, V-t_Lu(L,U)-Init, Vub-Vb-_),
	 pivot_b(Vub,V,Vb,t_lu(L,U))
      ;
	 inc_step( Vs, Status)
      )
  ; W = t_L(L),
      ( arith_eval( K > 0) ->
	 ( ub( V, Vub-Vb-_) ->
	     Status = applied,
	     pivot_a(Vub,V,Vb,t_l(L))
	 ;
	     Status = unlimited(V,t_l(L))
	 )
      ;
	 inc_step( Vs, Status)
      )
  ; W = t_Lu(L,U),
      ( arith_eval( K > 0) ->
	 Status = applied,
	 arith_eval( U-L, Init),
	 basis( V, Deps),
	 ub( Deps, V, V-t_lU(L,U)-Init, Vub-Vb-_),
	 pivot_b(Vub,V,Vb,t_lu(L,U))
      ;
	 inc_step( Vs, Status)
      )
  ; W = t_none,
      Status = unlimited(V,t_none)
  ).

% ------------------------------ best first heuristic -------------------------- %
%
% A replacement for dec_step/2 that uses a local best first heuristic.
%
%

dec_step_best( H, Status) :-
  dec_eval( H, E),
  ( E = unlimited(_,_),
     Status = E
  ; E = [],
     Status = optimum
  ; E = [_|_],
     Status = applied,
     keysort( E, [_-Best|_]),
     ( Best = pivot_a(Vub,V,Vb,Wd), pivot_a(Vub,V,Vb,Wd)
     ; Best = pivot_b(Vub,V,Vb,Wd), pivot_b(Vub,V,Vb,Wd)
     )
  ).

dec_eval( [],	    []).
dec_eval( [V*K|Vs], Res) :-
  get_atts( V, type(W)),
  ( W = t_U(U),
      ( arith_eval( K > 0) ->
	 ( lb( V, Vub-Vb-Limit) ->
	     arith_eval( float(Limit*K), Delta),
	     Res = [Delta-pivot_a(Vub,V,Vb,t_u(U)) | Tail],
	     dec_eval( Vs, Tail)
	 ;
	     Res = unlimited(V,t_u(U))
	 )
      ;
	 dec_eval( Vs, Res)
      )
  ; W = t_lU(L,U),
      ( arith_eval( K > 0) ->
	 arith_eval( L-U, Init),
	 basis( V, Deps),
	 lb( Deps, V, V-t_Lu(L,U)-Init, Vub-Vb-Limit),
	 arith_eval( float(Limit*K), Delta),
	 Res = [Delta-pivot_b(Vub,V,Vb,t_lu(L,U)) | Tail],
	 dec_eval( Vs, Tail)
      ;
	 dec_eval( Vs, Res)
      )
  ; W = t_L(L),
      ( arith_eval( K < 0) ->
	 ( ub( V, Vub-Vb-Limit) ->
	     arith_eval( float(Limit*K), Delta),
	     Res = [Delta-pivot_a(Vub,V,Vb,t_l(L)) | Tail],
	     dec_eval( Vs, Tail)
	 ;
	     Res = unlimited(V,t_l(L))
	 )
      ;
	 dec_eval( Vs, Res)
      )
  ; W = t_Lu(L,U),
      ( arith_eval( K < 0) ->
	 arith_eval( U-L, Init),
	 basis( V, Deps),
	 ub( Deps, V, V-t_lU(L,U)-Init, Vub-Vb-Limit),
	 arith_eval( float(Limit*K), Delta),
	 Res = [Delta-pivot_b(Vub,V,Vb,t_lu(L,U)) | Tail],
	 dec_eval( Vs, Tail)
      ;
	 dec_eval( Vs, Res)
      )
  ; W = t_none,
      Res = unlimited(V,t_none)
  ).

% ------------------------- find the most constraining row --------------------- %
%
% The code for the lower and the upper bound are dual versions of each other.
% The only difference is in the orientation of the comparisons.
% Indeps are ruled out by their types.
% If there is no bound, this fails.
%
% *** The actual lb and ub on an indep variable X are [lu]b + b(X), where b(X)
% is the value of the active bound.
%
% Nota bene: We must NOT consider infeasible rows as candidates to
%	     leave the basis!
%

ub( X, Ub) :-
  basis( X, Deps),
  ub_first( Deps, X, Ub).

:- mode ub_first( +, ?, -).
%
ub_first( [Dep|Deps], X, Tightest) :-
  ( get_atts( Dep, [lin(Lin),type(Type)]),
    ub_inner( Type, X, Lin, W, Ub),
    arith_eval( Ub >= 0) ->
      ub( Deps, X, Dep-W-Ub, Tightest)
  ;
      ub_first( Deps, X, Tightest)
  ).

%
% Invariant: Ub >= 0 and decreasing
%
:- mode ub( +, ?, +, -).
%
ub( [], 	_, T0,T0).
ub( [Dep|Deps], X, T0,T1) :-
  ( get_atts( Dep, [lin(Lin),type(Type)]),
    ub_inner( Type, X, Lin, W, Ub),
    T0 = _-Ubb,
    arith_eval( Ub < Ubb),
    arith_eval( Ub >= 0) ->			% rare failure
      ub( Deps, X, Dep-W-Ub,T1)
  ;
      ub( Deps, X, T0,T1)
  ).

lb( X, Lb) :-
  basis( X, Deps),
  lb_first( Deps, X, Lb).

:- mode lb_first( +, ?, -).
%
lb_first( [Dep|Deps], X, Tightest) :-
  ( get_atts( Dep, [lin(Lin),type(Type)]),
    lb_inner( Type, X, Lin, W, Lb),
    arith_eval( Lb =< 0) ->
      lb( Deps, X, Dep-W-Lb, Tightest)
  ;
      lb_first( Deps, X, Tightest)
  ).

%
% Invariant: Lb =< 0 and increasing
%
:- mode lb( +, ?, +, -).
%
lb( [], 	_, T0,T0).
lb( [Dep|Deps], X, T0,T1) :-
  ( get_atts( Dep, [lin(Lin),type(Type)]),
    lb_inner( Type, X, Lin, W, Lb),
    T0 = _-Lbb,
    arith_eval( Lb > Lbb),
    arith_eval( Lb =< 0) ->			% rare failure
      lb( Deps, X, Dep-W-Lb,T1)
  ;
      lb( Deps, X, T0,T1)
  ).

%
% Lb =< 0 for feasible rows
%
:- mode lb_inner( +, ?, +, -, -).
%
lb_inner( t_l(L), X, Lin, t_L(L), Lb) :-
  nf_rhs_x( Lin, X, Rhs, K),
  arith_eval( K > 0),
  arith_eval( (L-Rhs)/K, Lb).
lb_inner( t_u(U), X, Lin, t_U(U), Lb) :-
  nf_rhs_x( Lin, X, Rhs, K),
  arith_eval( K < 0),
  arith_eval( (U-Rhs)/K, Lb).
lb_inner( t_lu(L,U), X, Lin, W, Lb) :-
  nf_rhs_x( Lin, X, Rhs, K),
  case_signum( K,
   (
    W = t_lU(L,U),
    arith_eval( (U-Rhs)/K, Lb)
   ),
    fail,
   (
    W = t_Lu(L,U),
    arith_eval( (L-Rhs)/K, Lb)
   )).

%
% Ub >= 0 for feasible rows
%
:- mode ub_inner( +, ?, +, -, -).
%
ub_inner( t_l(L), X, Lin, t_L(L), Ub) :-
  nf_rhs_x( Lin, X, Rhs, K),
  arith_eval( K < 0),
  arith_eval( (L-Rhs)/K, Ub).
ub_inner( t_u(U), X, Lin, t_U(U), Ub) :-
  nf_rhs_x( Lin, X, Rhs, K),
  arith_eval( K > 0),
  arith_eval( (U-Rhs)/K, Ub).
ub_inner( t_lu(L,U), X, Lin, W, Ub) :-
  nf_rhs_x( Lin, X, Rhs, K),
  case_signum( K,
   (
    W = t_Lu(L,U),
    arith_eval( (L-Rhs)/K, Ub)
   ),
    fail,
   (
    W = t_lU(L,U),
    arith_eval( (U-Rhs)/K, Ub)
   )).

% ---------------------------------- equations --------------------------------- %
%
% backsubstitution will not make the system infeasible, if the bounds on the indep
% vars are obeyed, but some implied values might pop up in rows where X occurs
%	-) special case X=Y during bs -> get rid of dependend var(s), alias
%

solve( Lin) :-
  decompose( Lin, H, _, I),
  solve( H, Lin, I, Bindings, []),
  export_binding( Bindings).

solve( [], _,  I, Bind0,Bind0) :-
  arith_eval( I=:=0).					% redundant or trivially unsat
solve( H, Lin, _, Bind0,BindT) :-
  H = [_|_],						% indexing
  %
  % [] is an empty ord_set, anything will be preferred
  % over 9-9
  %
  sd( H, [],ClassesUniq, 9-9-0,Category-Selected-_, NV,NVT),

  isolate( Selected, Lin, Lin1),

  ( Category = 1,
      put_atts( Selected, lin(Lin1)),
      decompose( Lin1, Hom, _, Inhom),
      bs_collect_binding( Hom, Selected, Inhom, Bind0,BindT),
      eq_classes( NV, NVT, ClassesUniq)
  ; Category = 2,
      get_atts( Selected, class(NewC)),
      class_allvars( NewC, Deps),
      ( ClassesUniq = [_] ->				% rank increasing
	  bs_collect_bindings( Deps, Selected, Lin1, Bind0,BindT)
      ;
	  Bind0 = BindT,
	  bs( Deps, Selected, Lin1)
      ),
      eq_classes( NV, NVT, ClassesUniq)
  ; Category = 3,
      put_atts( Selected, lin(Lin1)),
      get_atts( Selected, type(Type)),
      deactivate_bound( Type, Selected),
      eq_classes( NV, NVT, ClassesUniq),
      basis_add( Selected, Basis),
      undet_active( Lin1),
      decompose( Lin1, Hom, _, Inhom),
      bs_collect_binding( Hom, Selected, Inhom, Bind0,Bind1),
      rcbl( Basis, Bind1,BindT)
  ; Category = 4,
      get_atts( Selected, [type(Type),class(NewC)]),
      class_allvars( NewC, Deps),
      ( ClassesUniq = [_] ->				% rank increasing
	  bs_collect_bindings( Deps, Selected, Lin1, Bind0,Bind1)
      ;
	  Bind0 = Bind1,
	  bs( Deps, Selected, Lin1)
      ),
      deactivate_bound( Type, Selected),
      basis_add( Selected, Basis),
      % eq_classes( NV, NVT, ClassesUniq),		%  4 -> var(NV)
      equate( ClassesUniq, _),
      undet_active( Lin1),
      rcbl( Basis, Bind1,BindT)
  ).

%
% Much like solve, but we solve for a particular variable of type
% t_none
%
solve_x( Lin, X) :-
  decompose( Lin, H, _, I),
  solve_x( H, Lin, I, X, Bindings, []),
  export_binding( Bindings).

solve_x( [], _,  I, _,	      Bind0,Bind0) :-
  arith_eval( I=:=0).					% redundant or trivially unsat
solve_x( H, Lin, _, Selected, Bind0,BindT) :-
  H = [_|_],						% indexing
  sd( H, [],ClassesUniq, 9-9-0,_, NV,NVT),

  isolate( Selected, Lin, Lin1),

  ( get_atts( Selected, class(NewC)) ->
      class_allvars( NewC, Deps),
      ( ClassesUniq = [_] ->				% rank increasing
	  bs_collect_bindings( Deps, Selected, Lin1, Bind0,BindT)
      ;
	  Bind0 = BindT,
	  bs( Deps, Selected, Lin1)
      ),
      eq_classes( NV, NVT, ClassesUniq)
  ;
      put_atts( Selected, lin(Lin1)),
      decompose( Lin1, Hom, _, Inhom),
      bs_collect_binding( Hom, Selected, Inhom, Bind0,BindT),
      eq_classes( NV, NVT, ClassesUniq)
  ).



sd( [],       Class0,Class0, Preference0,Preference0, NV0,NV0).
sd( [X*K|Xs], Class0,ClassN, Preference0,PreferenceN, NV0,NVt) :-
  ( get_atts( X, class(Xc)) ->						% old
	NV0 = NV1,
	ord_add_element( Class0, Xc, Class1),
	( get_atts( X, type(t_none)) ->
	    preference( Preference0, 2-X-K, Preference1)
	;
	    preference( Preference0, 4-X-K, Preference1)
	)
  ;									% new
	Class1 = Class0,
	NV0 = [X|NV1], % 'C'( NV0, X, NV1),
	( get_atts( X, type(t_none)) ->
	    preference( Preference0, 1-X-K, Preference1)
	;
	    preference( Preference0, 3-X-K, Preference1)
	)
  ),
  sd( Xs, Class1,ClassN, Preference1,PreferenceN, NV1,NVt).

%
% A is best sofar, B is current
%
preference( A, B, Pref) :-
  A = Px-_-_,
  B = Py-_-_,
  compare( Rel, Px, Py),
  ( Rel = (=), Pref = B
	     % ( arith_eval(abs(Ka)=<abs(Kb)) -> Pref=A ; Pref=B )
  ; Rel = (<), Pref = A
  ; Rel = (>), Pref = B
  ).

%
% equate after attach_class because other classes may contribute
% nonvars and will bind the tail of NV
%
eq_classes( NV, _,   Cs) :- var( NV), !,
  equate( Cs, _).
eq_classes( NV, NVT, Cs) :-
  class_new( Su, NV,NVT, []),
  attach_class( NV, Su),
  equate( Cs, Su).

equate( [],	_).
equate( [X|Xs], X) :- equate( Xs, X).

%
% assert: none of the Vars has a class attribute yet
%
attach_class( Xs,     _) :- var( Xs), !.
attach_class( [X|Xs], Class) :-
  put_atts( X, class(Class)),
  attach_class( Xs, Class).

/**
unconstrained( [X*K|Xs], Uc,Kuc, Rest) :-
  ( get_atts( X, type(t_none)) ->
      Uc = X,
      Kuc = K,
      Rest = Xs
  ;
      Rest = [X*K|Tail],
      unconstrained( Xs, Uc,Kuc, Tail)
  ).
**/
/**/
unconstrained( Lin, Uc,Kuc, Rest) :-
  decompose( Lin, H, _, _),
  sd( H, [],_, 9-9-0,Category-Uc-_, _,_),
  Category =< 2,
  delete_factor( Uc, Lin, Rest, Kuc).
/**/

%
% point the vars in Lin into the same equivalence class
% maybe join some global data
%
same_class( [],       _).
same_class( [X*_|Xs], Class) :-
  get_or_add_class( X, Class),
  same_class( Xs, Class).

get_or_add_class( X, Class) :-
  get_atts( X, class(ClassX)),
  !,
  ClassX = Class.				% explicit =/2 because of cut
get_or_add_class( X, Class) :-
  put_atts( X, class(Class)),
  class_new( Class, [X|Tail],Tail, []). 	% initial class atts

allvars( X, Allvars) :-
  get_atts( X, class(C)),
  class_allvars( C, Allvars).

deactivate_bound( t_l(_),    _).
deactivate_bound( t_u(_),    _).
deactivate_bound( t_lu(_,_), _).
deactivate_bound( t_L(L),    X) :- put_atts( X, type(t_l(L))).
deactivate_bound( t_Lu(L,U), X) :- put_atts( X, type(t_lu(L,U))).
deactivate_bound( t_U(U),    X) :- put_atts( X, type(t_u(U))).
deactivate_bound( t_lU(L,U), X) :- put_atts( X, type(t_lu(L,U))).

intro_at( X, Value, Type) :-
  put_atts( X, type(Type)),
  ( arith_eval( Value =:= 0) ->
      true
  ;
      backsubst_delta( X, Value)
  ).


%
% The choice t_lu -> t_Lu is arbitrary
%
undet_active( Lin) :-
  decompose( Lin, Lin1, _, _),
  undet_active_h( Lin1).

undet_active_h( []).
undet_active_h( [X*_|Xs]) :-
  get_atts( X, type(Type)),
  undet_active( Type, X),
  undet_active_h( Xs).

undet_active( t_none,	 _).			% type_activity
undet_active( t_L(_),	 _).
undet_active( t_Lu(_,_), _).
undet_active( t_U(_),	 _).
undet_active( t_lU(_,_), _).
undet_active( t_l(L),	 X) :- intro_at( X, L, t_L(L)).
undet_active( t_u(U),	 X) :- intro_at( X, U, t_U(U)).
undet_active( t_lu(L,U), X) :- intro_at( X, L, t_Lu(L,U)).

determine_active_dec( Lin) :-
  decompose( Lin, Lin1, _, _),
  arith_eval( -1, Mone),
  determine_active( Lin1, Mone).

determine_active_inc( Lin) :-
  decompose( Lin, Lin1, _, _),
  arith_eval( 1, One),
  determine_active( Lin1, One).

determine_active( [],	    _).
determine_active( [X*K|Xs], S) :-
  get_atts( X, type(Type)),
  determine_active( Type, X, K, S),
  determine_active( Xs, S).

determine_active( t_L(_),    _, _, _).
determine_active( t_Lu(_,_), _, _, _).
determine_active( t_U(_),    _, _, _).
determine_active( t_lU(_,_), _, _, _).
determine_active( t_l(L),    X, _, _) :- intro_at( X, L, t_L(L)).
determine_active( t_u(U),    X, _, _) :- intro_at( X, U, t_U(U)).
determine_active( t_lu(L,U), X, K, S) :-
  case_signum( K*S,
    intro_at( X, L, t_Lu(L,U)),
    fail,
    intro_at( X, U, t_lU(L,U))).

%
% Careful when an indep turns into t_none !!!
%
detach_bounds( V) :-
  get_atts( V, lin(Lin)),
  put_atts( V, [type(t_none),strictness(0b00)]),
  ( indep( Lin, V) ->
      ( ub( V, Vub-Vb-_) ->			% exchange against thightest
	  basis_drop( Vub),
	  pivot( Vub, V, Vb)
      ; lb( V, Vlb-Vb-_) ->
	  basis_drop( Vlb),
	  pivot( Vlb, V, Vb)
      ;
	  true
      )
  ;
      basis_drop( V)
  ).

% ----------------------------- manipulate the basis --------------------------- %

basis_drop( X) :-
  get_atts( X, class(Cv)),
  class_basis_drop( Cv, X).

basis( X, Basis) :-
  get_atts( X, class(Cv)),
  class_basis( Cv, Basis).

basis_add( X, NewBasis) :-
  get_atts( X, class(Cv)),
  class_basis_add( Cv, X, NewBasis).

basis_pivot( Leave, Enter) :-
  get_atts( Leave, class(Cv)),
  class_basis_pivot( Cv, Enter, Leave).

% ----------------------------------- pivot ------------------------------------ %

%
% Pivot ignoring rhs and active states
%
pivot( Dep, Indep) :-
  get_atts( Dep, lin(H)),
  delete_factor( Indep, H, H0, Coeff),
  arith_eval( -1/Coeff, K),
  arith_eval( -1, Mone),
  arith_eval(  0, Z),
  add_linear_ff( H0, K, [Z,Z,Dep*Mone], K, Lin),
  backsubst( Indep, Lin).


pivot_a( Dep, Indep, Vb,Wd) :-
  basis_pivot( Dep, Indep),
  pivot( Dep, Indep, Vb),
  put_atts( Indep, type(Wd)).

pivot_b( Vub, V, Vb, Wd) :-
  ( Vub == V ->
      put_atts( V, type(Vb)),
      pivot_b_delta( Vb, Delta),		% nonzero(Delta)
      backsubst_delta( V, Delta)
  ;
      pivot_a( Vub, V, Vb,Wd)
  ).

pivot_b_delta( t_Lu(L,U), Delta) :- arith_eval( L-U, Delta).
pivot_b_delta( t_lU(L,U), Delta) :- arith_eval( U-L, Delta).

select_active_bound( t_L(L),	L).
select_active_bound( t_Lu(L,_), L).
select_active_bound( t_U(U),	U).
select_active_bound( t_lU(_,U), U).
select_active_bound( t_none,	Z) :- arith_eval( 0, Z).
%
% for project.pl
%
select_active_bound( t_l(_),	Z) :- arith_eval( 0, Z).
select_active_bound( t_u(_),	Z) :- arith_eval( 0, Z).
select_active_bound( t_lu(_,_), Z) :- arith_eval( 0, Z).


%
% Pivot taking care of rhs and active states
%
pivot( Dep, Indep, IndAct) :-
  get_atts( Dep, lin(H)),
  put_atts( Dep, type(IndAct)),
  select_active_bound( IndAct, Abv),			% Dep or Indep
  delete_factor( Indep, H, H0, Coeff),
  arith_eval( -1/Coeff, K),
  arith_eval( 0, Z),
  arith_eval( -1, Mone),
  arith_eval( -Abv, Abvm),
  add_linear_ff( H0, K, [Z,Abvm,Dep*Mone], K, Lin),
  backsubst( Indep, Lin).

backsubst_delta( X, Delta) :-
  arith_eval( 1, One),
  arith_eval( 0, Z),
  backsubst( X, [Z,Delta,X*One]).

backsubst( X, Lin) :-
  allvars( X, Allvars),
  bs( Allvars, X, Lin).
%
% valid if nothing will go ground
%
bs( Xs,     _, _) :- var( Xs), !.
bs( [X|Xs], V, Lin) :-
  ( get_atts( X, lin(LinX)),
    nf_substitute( V, Lin, LinX, LinX1) ->
      put_atts( X, lin(LinX1)),
      bs( Xs, V, Lin)
  ;
      bs( Xs, V, Lin)
  ).


%
% rank increasing backsubstitution
%
bs_collect_bindings( Xs,     _, _,   Bind0,BindT) :- var( Xs), !, Bind0=BindT.
bs_collect_bindings( [X|Xs], V, Lin, Bind0,BindT) :-
  ( get_atts( X, lin(LinX)),
    nf_substitute( V, Lin, LinX, LinX1) ->
      put_atts( X, lin(LinX1)),
      decompose( LinX1, Hom, _, Inhom),
      bs_collect_binding( Hom, X, Inhom, Bind0,Bind1),
      bs_collect_bindings( Xs, V, Lin, Bind1,BindT)
  ;
      bs_collect_bindings( Xs, V, Lin, Bind0,BindT)
  ).

%
% The first clause exports bindings,
% the second (no longer) aliasings
%
bs_collect_binding( [],       X, Inhom) --> [ X-Inhom ].
bs_collect_binding( [_|_],    _, _)     --> [].
/*
bs_collect_binding( [Y*K|Ys], X, Inhom) -->
  ( { Ys = [],
      Y \== X,
      arith_eval( K=:=1),
      arith_eval( Inhom=:=0)
    } ->
      [ X-Y ]
  ;
      []
  ).
*/

%
% reconsider the basis
%
rcbl( [],		Bind0,Bind0).
rcbl( [X|Continuation], Bind0,BindT) :-
  ( rcb( X, Status, Violated) ->			% have a culprit
      rcbl_status( Status, X, Continuation, Bind0,BindT, Violated)
  ;
      rcbl( Continuation, Bind0,BindT)
  ).

%
% reconsider one element of the basis
% later: lift the binds
%
reconsider( X) :-
  rcb( X, Status, Violated),
  !,
  rcbl_status( Status, X, [], Binds,[], Violated),
  export_binding( Binds).
reconsider( _).

%
% Find a basis variable out of its bound or at its bound
% Try to move it into whithin its bound
%   a) impossible -> fail
%   b) optimum at the bound -> implied value
%   c) else look at the remaining basis variables
%
rcb( X, Status, Violated) :-
  get_atts( X, [lin(Lin),type(Type)]),
  decompose( Lin, H, R, I),
  ( Type = t_l(L),
		 arith_eval( R+I =< L),
		 Violated = l(L),
		 inc_step( H, Status)

  ; Type = t_u(U),
		 arith_eval( R+I >= U),
		 Violated = u(U),
		 dec_step( H, Status)

  ; Type = t_lu(L,U),
		 arith_eval( R+I, At),
		 (
		   arith_eval( At =< L),
		   Violated = l(L),
		   inc_step( H, Status)
		 ;
		   arith_eval( At >= U),
		   Violated = u(U),
		   dec_step( H, Status)
		 )
  %
  % don't care for other types
  %
  ).

rcbl_status( optimum,		    X, Cont, B0,Bt, Violated) :- rcbl_opt( Violated, X, Cont, B0,Bt).
rcbl_status( applied,		    X, Cont, B0,Bt, Violated) :- rcbl_app( Violated, X, Cont, B0,Bt).
rcbl_status( unlimited(Indep,DepT), X, Cont, B0,Bt, Violated) :- rcbl_unl( Violated, X, Cont, B0,Bt, Indep, DepT).

%
% Might reach optimum immediately without changing the basis,
% but in general we must assume that there were pivots.
% If the optimum meets the bound, we backsubstitute the implied
% value, solve will call us again to check for further implied
% values or unsatisfiability in the rank increased system.
%
rcbl_opt( l(L), X, Continuation, B0,B1) :-
  get_atts( X, [lin(Lin),strictness(Strict),type(Type)]),
  decompose( Lin, _, R, I),
  arith_eval( R+I, Opt),
  case_signum( L-Opt,
   (
    narrow_u( Type, X, Opt),			% { X =< Opt }
    rcbl( Continuation, B0,B1)
   ),
   (
    Strict /\ 0b10 =:= 0,			% meets lower
    arith_eval( -Opt, Mop),
    normalize_scalar( Mop, MopN),
    add_linear_11( MopN, Lin, Lin1),
    decompose( Lin1, Hom, _, Inhom),
    ( Hom = [],    rcbl( Continuation, B0,B1)	% would not callback
    ; Hom = [_|_], solve( Hom, Lin1, Inhom, B0,B1)
    )
   ),
    fail
   ).
rcbl_opt( u(U), X, Continuation, B0,B1) :-
  get_atts( X, [lin(Lin),strictness(Strict),type(Type)]),
  decompose( Lin, _, R, I),
  arith_eval( R+I, Opt),
  case_signum( U-Opt,
    fail,
   (
    Strict /\ 0b01 =:= 0,			% meets upper
    arith_eval( -Opt, Mop),
    normalize_scalar( Mop, MopN),
    add_linear_11( MopN, Lin, Lin1),
    decompose( Lin1, Hom, _, Inhom),
    ( Hom = [],    rcbl( Continuation, B0,B1)	% would not callback
    ; Hom = [_|_], solve( Hom, Lin1, Inhom, B0,B1)
    )
   ),
   (
    narrow_l( Type, X, Opt),			% { X >= Opt }
    rcbl( Continuation, B0,B1)
   )).

%
% Basis has already changed when this is called
%
rcbl_app( l(L), X, Continuation, B0,B1) :-
  get_atts( X, lin(Lin)),
  decompose( Lin, H, R, I),
  ( arith_eval( R+I > L) ->			% within bound now
      rcbl( Continuation, B0,B1)
  ;
      % arith_eval( R+I, Val), print( rcbl_app(X:L:Val)), nl,
      inc_step( H, Status),
      rcbl_status( Status, X, Continuation, B0,B1, l(L))
  ).
rcbl_app( u(U), X, Continuation, B0,B1) :-
  get_atts( X, lin(Lin)),
  decompose( Lin, H, R, I),
  ( arith_eval( R+I < U) ->			% within bound now
      rcbl( Continuation, B0,B1)
  ;
      dec_step( H, Status),
      rcbl_status( Status, X, Continuation, B0,B1, u(U))
  ).

%
% This is never called for a t_lu culprit
%
rcbl_unl( l(L), X, Continuation, B0,B1, Indep, DepT) :-
  pivot_a( X, Indep, t_L(L), DepT),		% changes the basis
  rcbl( Continuation, B0,B1).
rcbl_unl( u(U), X, Continuation, B0,B1, Indep, DepT) :-
  pivot_a( X, Indep, t_U(U), DepT),		% changes the basis
  rcbl( Continuation, B0,B1).

narrow_u( t_u(_),    X, U) :- put_atts( X, type(t_u(U))).
narrow_u( t_lu(L,_), X, U) :- put_atts( X, type(t_lu(L,U))).

narrow_l( t_l(_),    X, L) :- put_atts( X, type(t_l(L))).
narrow_l( t_lu(_,U), X, L) :- put_atts( X, type(t_lu(L,U))).

% ----------------------------------- dump -------------------------------------

dump_var( t_none, V, I,H) --> !,
  ( { H=[W*K],V==W,arith_eval(I=:=0),arith_eval(K=:=1) } ->	% indep var
      []
  ;
      { nf2sum( H, I, Sum) },
      [ V = Sum ]
  ).
dump_var( t_L(L), V, I,H) --> !, dump_var( t_l(L),    V, I,H).
dump_var( t_l(L), V, I,H) --> !,
  {
    H= [_*K|_], 			% avoid 1 >= 0
    get_atts( V, strictness(Strict)),
    Sm is Strict /\ 0b10,
    arith_eval( 1/K, Kr),
    arith_eval( Kr*(L-I), Li),
    mult_hom( H, Kr, H1),
    arith_eval( 0, Z), nf2sum( H1, Z, Sum),
    ( arith_eval( K > 0) ->
	dump_strict( Sm, Sum >= Li, Sum > Li, Result)
    ;
	dump_strict( Sm, Sum =< Li, Sum < Li, Result)
    )
  },
  [ Result ].
dump_var( t_U(U), V, I,H) --> !, dump_var( t_u(U),    V, I,H).
dump_var( t_u(U), V, I,H) --> !,
  {
    H= [_*K|_], 			% avoid 0 =< 1
    get_atts( V, strictness(Strict)),
    Sm is Strict /\ 0b01,
    arith_eval( 1/K, Kr),
    arith_eval( Kr*(U-I), Ui),
    mult_hom( H, Kr, H1),
    arith_eval( 0, Z), nf2sum( H1, Z, Sum),
    ( arith_eval( K > 0) ->
	dump_strict( Sm, Sum =< Ui, Sum < Ui, Result)
    ;
	dump_strict( Sm, Sum >= Ui, Sum > Ui, Result)
    )
  },
  [ Result ].
dump_var( t_Lu(L,U), V, I,H) --> !, dump_var( t_l(L), V,I,H),
				    dump_var( t_U(U), V,I,H).
dump_var( t_lU(L,U), V, I,H) --> !, dump_var( t_l(L), V,I,H),
				    dump_var( t_U(U), V,I,H).
dump_var( t_lu(L,U), V, I,H) --> !, dump_var( t_l(L), V,I,H),
				    dump_var( t_U(U), V,I,H).
dump_var( T,	  V, I,H) -->
  [ V:T:I+H ].

dump_strict( 0, Result, _, Result).
dump_strict( 1, _, Result, Result).
dump_strict( 2, _, Result, Result).

dump_nz( _, H, I) -->
  {
    H = [_*K|_],
    arith_eval( 1/K, Kr),
    arith_eval( -Kr*I, I1),
    mult_hom( H, Kr, H1),
    arith_eval( 0, Z), nf2sum( H1, Z, Sum)
  },
  [ Sum =\= I1 ].
