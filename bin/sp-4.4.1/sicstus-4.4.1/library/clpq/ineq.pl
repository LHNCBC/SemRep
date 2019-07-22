%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   ineq.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Lin (=)< 0
%
ineq( [],		 I, _,	 Strictness) :- ineq_ground( Strictness, I).
ineq( [v(K,[X^1])|Tail], I, Lin, Strictness) :-
  ineq_cases( Tail, I, Lin, Strictness, X, K).

ineq_cases( [], I, _, Strictness, X, K) :-
  ineq_one( Strictness, X, K, I).
ineq_cases( [_|_], _, Lin, Strictness, _, _) :-
  deref( Lin, Lind),				% Id+Hd =< 0
  decompose( Lind, Hom, _, Inhom),
  ineq_more( Hom, Inhom, Lind, Strictness).

ineq_ground( strict,	I) :- arith_eval( I  < 0).
ineq_ground( nonstrict, I) :- arith_eval( I =< 0).

%
% Special cases: k={+-}1,i=0
%
ineq_one( strict,    X, K, I) :-
  ( arith_eval(K>0) ->
      ( arith_eval(I=:=0) ->
	  ineq_one_s_p_0( X)
      ;
	  arith_eval( I/K, Inhom),
	  ineq_one_s_p_i( X, Inhom)
      )
  ;
      ( arith_eval(I=:=0) ->
	  ineq_one_s_n_0( X)
      ;
	  arith_eval( -I/K, Inhom),
	  ineq_one_s_n_i( X, Inhom)
      )
  ).
ineq_one( nonstrict, X, K, I) :-
  ( arith_eval(K>0) ->
      ( arith_eval(I=:=0) ->
	  ineq_one_n_p_0( X)
      ;
	  arith_eval( I/K, Inhom),
	  ineq_one_n_p_i( X, Inhom)
      )
  ;
      ( arith_eval(I=:=0) ->
	  ineq_one_n_n_0( X)
      ;
	  arith_eval( -I/K, Inhom),
	  ineq_one_n_n_i( X, Inhom)
      )
  ).

/*
ineq_one( Strictness, X, K, I) :-
  get_atts( X, lin(LinX)),
  !,						% old variable, this is deref
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old( OrdX, K, I, Strictness, X, Ix).
ineq_one( Strictness, X, K, I) :-		% new variable, nothing depends on it
  arith_eval( -I/K, Bound),
  ineq_one_new( Strictness, X, K, Bound).

ineq_one_new( strict, X, K, Bound) :-
  arith_eval( 1, One),
  ( arith_eval( K < 0) ->
      var_intern( t_l(Bound), X, 0b10)
  ;
      var_intern( t_u(Bound), X, 0b01)
  ).
ineq_one_new( nonstrict, X, K, Bound) :-
  arith_eval( 1, One),
  ( arith_eval( K < 0) ->
      var_intern( t_l(Bound), X, 0b00)
  ;
      var_intern( t_u(Bound), X, 0b00)
  ).


ineq_one_old( [], K, I, Strictness, _X, Ix) :-
  arith_eval( K*Ix+I, Inhom),
  ineq_ground( Strictness, Inhom).
%
% here we would have the choice to bound X or Y
%
ineq_one_old( [Y*Ky|Tail], K, I, Strictness, X, Ix) :-
  ( Tail = [],
      arith_eval( K*Ky, Coeff),
      arith_eval( -(K*Ix+I)/Coeff, Bound),
      update_indep( Strictness, Y, Coeff, Bound)
  ; Tail = [_|_],
      arith_eval( -I/K, Bound),
      update_dep( Strictness, X, K, Bound)
  ).

update_dep( strict, X, K, Bound) :-
  get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
  ( arith_eval( K < 0) ->
      udls( Type, X, Lin, Bound, Old)
  ;
      udus( Type, X, Lin, Bound, Old)
  ).
update_dep( nonstrict, X, K, Bound) :-
  get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
  ( arith_eval( K < 0) ->
      udl( Type, X, Lin, Bound, Old)
  ;
      udu( Type, X, Lin, Bound, Old)
  ).
*/

% --------------------------- strict ----------------------------

ineq_one_s_p_0( X) :-
  get_atts( X, lin(LinX)),
  !,						% old variable, this is deref
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old_s_p_0( OrdX, X, Ix).
ineq_one_s_p_0( X) :-				% new variable, nothing depends on it
  arith_eval( 0, Zero),
  var_intern( t_u(Zero), X, 0b01).

ineq_one_s_n_0( X) :-
  get_atts( X, lin(LinX)),
  !,
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old_s_n_0( OrdX, X, Ix).
ineq_one_s_n_0( X) :-
  arith_eval( 0, Zero),
  var_intern( t_l(Zero), X, 0b10).

ineq_one_s_p_i( X, I) :-
  get_atts( X, lin(LinX)),
  !,
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old_s_p_i( OrdX, I, X, Ix).
ineq_one_s_p_i( X, I) :-
  arith_eval( -I, Bound),
  var_intern( t_u(Bound), X, 0b01).

ineq_one_s_n_i( X, I) :-
  get_atts( X, lin(LinX)),
  !,
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old_s_n_i( OrdX, I, X, Ix).
ineq_one_s_n_i( X, I) :-
  var_intern( t_l(I), X, 0b10).

ineq_one_old_s_p_0( [], 	 _, Ix) :-
  arith_eval( Ix < 0).
ineq_one_old_s_p_0( [Y*Ky|Tail], X, Ix) :-
  ( Tail = [],
      arith_eval( -Ix/Ky, Bound),
      update_indep( strict, Y, Ky, Bound)
  ; Tail = [_|_],
      arith_eval( 0, Zero),
      get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
      udus( Type, X, Lin, Zero, Old)
  ).

ineq_one_old_s_n_0( [], _, Ix) :-
  arith_eval( Ix > 0).
ineq_one_old_s_n_0( [Y*Ky|Tail], X, Ix) :-
  ( Tail = [],
      arith_eval( -Ky, Coeff),
      arith_eval( Ix/Coeff, Bound),
      update_indep( strict, Y, Coeff, Bound)
  ; Tail = [_|_],
      arith_eval( 0, Zero),
      get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
      udls( Type, X, Lin, Zero, Old)
  ).

ineq_one_old_s_p_i( [], I, _, Ix) :-
  arith_eval( Ix+I < 0).
ineq_one_old_s_p_i( [Y*Ky|Tail], I, X, Ix) :-
  ( Tail = [],
      arith_eval( -(Ix+I)/Ky, Bound),
      update_indep( strict, Y, Ky, Bound)
  ; Tail = [_|_],
      arith_eval( -I, Bound),
      get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
      udus( Type, X, Lin, Bound, Old)
  ).

ineq_one_old_s_n_i( [], 	 I, _, Ix) :-
  arith_eval( -Ix+I < 0).
ineq_one_old_s_n_i( [Y*Ky|Tail], I, X, Ix) :-
  ( Tail = [],
      arith_eval( -Ky, Coeff),
      arith_eval( (Ix-I)/Coeff, Bound),
      update_indep( strict, Y, Coeff, Bound)
  ; Tail = [_|_],
      get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
      udls( Type, X, Lin, I, Old)
  ).

% -------------------------- nonstrict --------------------------

ineq_one_n_p_0( X) :-
  get_atts( X, lin(LinX)),
  !,						% old variable, this is deref
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old_n_p_0( OrdX, X, Ix).
ineq_one_n_p_0( X) :-				% new variable, nothing depends on it
  arith_eval( 0, Zero),
  var_intern( t_u(Zero), X, 0b00).

ineq_one_n_n_0( X) :-
  get_atts( X, lin(LinX)),
  !,
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old_n_n_0( OrdX, X, Ix).
ineq_one_n_n_0( X) :-
  arith_eval( 0, Zero),
  var_intern( t_l(Zero), X, 0b00).

ineq_one_n_p_i( X, I) :-
  get_atts( X, lin(LinX)),
  !,
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old_n_p_i( OrdX, I, X, Ix).
ineq_one_n_p_i( X, I) :-
  arith_eval( -I, Bound),
  var_intern( t_u(Bound), X, 0b00).

ineq_one_n_n_i( X, I) :-
  get_atts( X, lin(LinX)),
  !,
  decompose( LinX, OrdX, _, Ix),
  ineq_one_old_n_n_i( OrdX, I, X, Ix).
ineq_one_n_n_i( X, I) :-
  var_intern( t_l(I), X, 0b00).

ineq_one_old_n_p_0( [], 	 _, Ix) :-
  arith_eval( Ix =< 0).
ineq_one_old_n_p_0( [Y*Ky|Tail], X, Ix) :-
  ( Tail = [],
      arith_eval( -Ix/Ky, Bound),
      update_indep( nonstrict, Y, Ky, Bound)
  ; Tail = [_|_],
      arith_eval( 0, Zero),
      get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
      udu( Type, X, Lin, Zero, Old)
  ).

ineq_one_old_n_n_0( [], _, Ix) :-
  arith_eval( Ix >= 0).
ineq_one_old_n_n_0( [Y*Ky|Tail], X, Ix) :-
  ( Tail = [],
      arith_eval( -Ky, Coeff),
      arith_eval( Ix/Coeff, Bound),
      update_indep( nonstrict, Y, Coeff, Bound)
  ; Tail = [_|_],
      arith_eval( 0, Zero),
      get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
      udl( Type, X, Lin, Zero, Old)
  ).

ineq_one_old_n_p_i( [], I, _, Ix) :-
  arith_eval( Ix+I =< 0).
ineq_one_old_n_p_i( [Y*Ky|Tail], I, X, Ix) :-
  ( Tail = [],
      arith_eval( -(Ix+I)/Ky, Bound),
      update_indep( nonstrict, Y, Ky, Bound)
  ; Tail = [_|_],
      arith_eval( -I, Bound),
      get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
      udu( Type, X, Lin, Bound, Old)
  ).

ineq_one_old_n_n_i( [], 	 I, _, Ix) :-
  arith_eval( -Ix+I =< 0).
ineq_one_old_n_n_i( [Y*Ky|Tail], I, X, Ix) :-
  ( Tail = [],
      arith_eval( -Ky, Coeff),
      arith_eval( (Ix-I)/Coeff, Bound),
      update_indep( nonstrict, Y, Coeff, Bound)
  ; Tail = [_|_],
      get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
      udl( Type, X, Lin, I, Old)
  ).

% ---------------------------------------------------------------


ineq_more( [],	       I,  _,	 Strictness) :- ineq_ground( Strictness, I).
ineq_more( [X*K|Tail], Id, Lind, Strictness) :-
   ( Tail = [], 					% one var: update bound instead of slack introduction
       get_or_add_class( X, _),
       arith_eval( -Id/K, Bound),
       update_indep( Strictness, X, K, Bound)
   ; Tail = [_|_],
       ineq_more( Strictness, Lind)
   ).

ineq_more( strict, Lind) :-
   ( unconstrained( Lind, U,K, Rest) -> 		% never fails, no implied value
       arith_eval( 0, Z),
       arith_eval( 1, One),
       var_intern( t_l(Z), S, 0b10),
       arith_eval( -1/K, Ki),
       add_linear_ff( Rest, Ki, [Z,Z,S*One], Ki, LinU),
       decompose( LinU, Hu, _, _),
       get_or_add_class( U, Class),
       same_class( Hu, Class),
       backsubst( U, LinU)
   ;
       arith_eval( 0, Z),
       var_with_def_intern( t_u(Z), S, Lind, 0b01),
       basis_add( S, _),
       determine_active_dec( Lind),
       reconsider( S)
   ).
ineq_more( nonstrict, Lind) :-
   ( unconstrained( Lind, U,K, Rest) -> 		% never fails, no implied value
       arith_eval( 0, Z),
       arith_eval( 1, One),
       var_intern( t_l(Z), S, 0b00),
       arith_eval( -1/K, Ki),
       add_linear_ff( Rest, Ki, [Z,Z,S*One], Ki, LinU),
       decompose( LinU, Hu, _, _),
       get_or_add_class( U, Class),
       same_class( Hu, Class),
       backsubst( U, LinU)
   ;
       arith_eval( 0, Z),
       var_with_def_intern( t_u(Z), S, Lind, 0b00),
       basis_add( S, _),
       determine_active_dec( Lind),
       reconsider( S)
   ).

update_indep( strict, X, K, Bound) :-
  get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
  ( arith_eval( K < 0) ->
      uils( Type, X, Lin, Bound, Old)
  ;
      uius( Type, X, Lin, Bound, Old)
  ).
update_indep( nonstrict, X, K, Bound) :-
  get_atts( X, [lin(Lin),type(Type),strictness(Old)]),
  ( arith_eval( K < 0) ->
      uil( Type, X, Lin, Bound, Old)
  ;
      uiu( Type, X, Lin, Bound, Old)
  ).


% ---------------------------------------------------------------------------------------

%
% Update a bound on a var xi
%
%   a) independent variable
%
%	a1) update inactive bound: done
%
%	a2) update active bound:
%	    Determine [lu]b including most constraining row R
%	      If we are within: done
%	    else pivot(R,xi) and introduce bound via (b)
%
%	a3) introduce a bound on an unconstrained var:
%	    All vars that depend on xi are unconstrained (invariant) ->
%	      the bound cannot invalidate any Lhs
%
%   b) dependent variable
%
%	repair upper or lower (maybe just swap with an unconstrained var from Rhs)
%

%
% Sign = 1,0,-1 means inside,at,outside
%

udl( t_none, X, Lin, Bound, _Sold) :-
  put_atts( X, [type(t_l(Bound)),strictness(0b00)]),
  ( unconstrained( Lin, Uc,Kuc, Rest) ->
      arith_eval( -1/Kuc, Ki),
      arith_eval( 0, Z),
      arith_eval( -1, Mone),
      add_linear_ff( Rest, Ki, [Z,Z,X*Mone], Ki, LinU),
      backsubst( Uc, LinU)
  ;
	basis_add( X, _),
	determine_active_inc( Lin),
	reconsider( X)
  ).
udl( t_l(L), X, Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
    true,
  (
    Strict is Sold /\ 0b01,
    put_atts( X, [type(t_l(Bound)),strictness(Strict)]),
    reconsider_lower( X, Lin, Bound)
  )).
udl( t_u(U), X, Lin, Bound, _Sold) :-
  case_signum( U-Bound,
    fail,
    solve_bound( Lin, Bound),
  (
    put_atts( X, type(t_lu(Bound,U))),
    reconsider_lower( X, Lin, Bound)
  )).
udl( t_lu(L,U), X, Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
    true,
  (
    case_signum( U-Bound,
      fail,
    (
      Sold /\ 0b01 =:= 0,
      solve_bound( Lin, Bound)
    ),
    (
      Strict is Sold /\ 0b01,
      put_atts( X, [type(t_lu(Bound,U)),strictness(Strict)]),
      reconsider_lower( X, Lin, Bound)
    ))
  )).

udls( t_none, X, Lin, Bound, _Sold) :-
  put_atts( X, [type(t_l(Bound)),strictness(0b10)]),
  ( unconstrained( Lin, Uc,Kuc, Rest) ->
      arith_eval( -1/Kuc, Ki),
      arith_eval( -1, Mone),
      arith_eval( 0, Z),
      add_linear_ff( Rest, Ki, [Z,Z,X*Mone], Ki, LinU),
      backsubst( Uc, LinU)
  ;
	basis_add( X, _),
	determine_active_inc( Lin),
	reconsider( X)
  ).
udls( t_l(L), X, Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
   (
    Strict is Sold \/ 0b10,
    put_atts( X, strictness(Strict))
   ),
   (
    Strict is Sold \/ 0b10,
    put_atts( X, [type(t_l(Bound)),strictness(Strict)]),
    reconsider_lower( X, Lin, Bound)
   )).
udls( t_u(U), X, Lin, Bound, Sold) :-
  arith_eval( U>Bound),
  Strict is Sold \/ 0b10,
  put_atts( X, [type(t_lu(Bound,U)),strictness(Strict)]),
  reconsider_lower( X, Lin, Bound).
udls( t_lu(L,U), X, Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
   (
    Strict is Sold \/ 0b10,
    put_atts( X, strictness(Strict))
   ),
   (
    arith_eval( U>Bound),
    Strict is Sold \/ 0b10,
    put_atts( X, [type(t_lu(Bound,U)),strictness(Strict)]),
    reconsider_lower( X, Lin, Bound)
   )).


udu( t_none, X, Lin, Bound, _Sold) :-
  put_atts( X, [type(t_u(Bound)),strictness(0b00)]),
  ( unconstrained( Lin, Uc,Kuc, Rest) ->
      arith_eval( -1/Kuc, Ki),
      arith_eval( -1, Mone),
      arith_eval( 0, Z),
      add_linear_ff( Rest, Ki, [Z,Z,X*Mone], Ki, LinU),
      backsubst( Uc, LinU)
  ;
	basis_add( X, _),
	determine_active_dec( Lin),
	reconsider( X)
  ).
udu( t_u(U), X, Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
    true,
   (
    Strict is Sold /\ 0b10,
    put_atts( X, [type(t_u(Bound)),strictness(Strict)]),
    reconsider_upper( X, Lin, Bound)
   )).
udu( t_l(L), X, Lin, Bound, _Sold) :-
  case_signum( Bound-L,
    fail,
    solve_bound( Lin, Bound),
   (
    put_atts( X, type(t_lu(L,Bound))),
    reconsider_upper( X, Lin, Bound)
   )).
udu( t_lu(L,U), X, Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
    true,
   (
    case_signum( Bound-L,
      fail,
     (
      Sold /\ 0b10 =:= 0,
      solve_bound( Lin, Bound)
     ),
     (
      Strict is Sold /\ 0b10,
      put_atts( X, [type(t_lu(L,Bound)),strictness(Strict)]),
      reconsider_upper( X, Lin, Bound)
     ))
   )).

udus( t_none, X, Lin, Bound, _Sold) :-
  put_atts( X, [type(t_u(Bound)),strictness(0b01)]),
  ( unconstrained( Lin, Uc,Kuc, Rest) ->
      arith_eval( -1/Kuc, Ki),
      arith_eval( -1, Mone),
      arith_eval( 0, Z),
      add_linear_ff( Rest, Ki, [Z,Z,X*Mone], Ki, LinU),
      backsubst( Uc, LinU)
  ;
	basis_add( X, _),
	determine_active_dec( Lin),
	reconsider( X)
  ).
udus( t_u(U), X, Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
   (
    Strict is Sold \/ 0b01,
    put_atts( X, strictness(Strict))
   ),
   (
    Strict is Sold \/ 0b01,
    put_atts( X, [type(t_u(Bound)),strictness(Strict)]),
    reconsider_upper( X, Lin, Bound)
   )).
udus( t_l(L), X, Lin, Bound, Sold) :-
  arith_eval( Bound>L),
  Strict is Sold \/ 0b01,
  put_atts( X, [type(t_lu(L,Bound)),strictness(Strict)]),
  reconsider_upper( X, Lin, Bound).
udus( t_lu(L,U), X, Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
   (
    Strict is Sold \/ 0b01,
    put_atts( X, strictness(Strict))
   ),
   (
    arith_eval( Bound>L),
    Strict is Sold \/ 0b01,
    put_atts( X, [type(t_lu(L,Bound)),strictness(Strict)]),
    reconsider_upper( X, Lin, Bound)
   )).

uiu( t_none,	X, _Lin, Bound, _) :-
  put_atts( X, [type(t_u(Bound)),strictness(0b00)]).
uiu( t_u(U),	X, _Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
    true,
   (
    Strict is Sold /\ 0b10,
    put_atts( X, [type(t_u(Bound)),strictness(Strict)])
   )).
uiu( t_l(L),	X, Lin, Bound, _Sold) :-
  case_signum( Bound-L,
    fail,
    solve_bound( Lin, Bound),
    put_atts( X, type(t_lu(L,Bound)))).
uiu( t_L(L),	X, Lin, Bound, _Sold) :-
  case_signum( Bound-L,
    fail,
    solve_bound( Lin, Bound),
    put_atts( X, type(t_Lu(L,Bound)))).
uiu( t_lu(L,U), X, Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
    true,
   (
    case_signum( Bound-L,
      fail,
     (
      Sold /\ 0b10 =:= 0,
      solve_bound( Lin, Bound)
     ),
     (
      Strict is Sold /\ 0b10,
      put_atts( X, [type(t_lu(L,Bound)),strictness(Strict)])
     ))
   )).
uiu( t_Lu(L,U), X, Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
    true,
   (
    case_signum( Bound-L,
      fail,
     (
      Sold /\ 0b10 =:= 0,
      solve_bound( Lin, Bound)
     ),
     (
      Strict is Sold /\ 0b10,
      put_atts( X, [type(t_Lu(L,Bound)),strictness(Strict)])
     ))
   )).
%
% update active:
%
uiu( t_U(U),	X, _Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
    true,
   (
    Strict is Sold /\ 0b10,
    ( lb( X, Vlb-Vb-Lb),
      arith_eval( Bound =< Lb+U) ->
	put_atts( X, [type(t_U(Bound)),strictness(Strict)]),
	pivot_a( Vlb, X, Vb, t_u(Bound)),
	reconsider( X)
    ;
	put_atts( X, [type(t_U(Bound)),strictness(Strict)]),
	arith_eval( Bound-U, Delta),
	backsubst_delta( X, Delta)
    )
   )).
uiu( t_lU(L,U), X, Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
    true,
   (
    case_signum( Bound-L,
      fail,
     (
      Sold /\ 0b10 =:= 0,
      solve_bound( Lin, Bound)
     ),
     (
      Strict is Sold /\ 0b10,
      ( lb( X, Vlb-Vb-Lb),
	arith_eval( Bound =< Lb+U) ->
	  put_atts( X, [type(t_lU(L,Bound)),strictness(Strict)]),
	  pivot_a( Vlb, X, Vb, t_lu(L,Bound)),
	  reconsider( X)
      ;
	  put_atts( X, [type(t_lU(L,Bound)),strictness(Strict)]),
	  arith_eval( Bound-U, Delta),
	  backsubst_delta( X, Delta)
      )
     ))
   )).


uius( t_none,	 X, _Lin, Bound, _Sold) :-
  put_atts( X, [type(t_u(Bound)),strictness(0b01)]).
uius( t_u(U),	 X, _Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
   (
    Strict is Sold \/ 0b01,
    put_atts( X, strictness(Strict))
   ),
   (
    Strict is Sold \/ 0b01,
    put_atts( X, [type(t_u(Bound)),strictness(Strict)])
   )).
uius( t_l(L),	 X, _Lin, Bound, Sold) :-
  arith_eval( Bound>L),
  Strict is Sold \/ 0b01,
  put_atts( X, [type(t_lu(L,Bound)),strictness(Strict)]).
uius( t_L(L),	 X, _Lin, Bound, Sold) :-
  arith_eval( Bound>L),
  Strict is Sold \/ 0b01,
  put_atts( X, [type(t_Lu(L,Bound)),strictness(Strict)]).
uius( t_lu(L,U), X, _Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
   (
    Strict is Sold \/ 0b01,
    put_atts( X, strictness(Strict))
   ),
   (
    arith_eval( Bound>L),
    Strict is Sold \/ 0b01,
    put_atts( X, [type(t_lu(L,Bound)),strictness(Strict)])
   )).
uius( t_Lu(L,U), X, _Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
   (
    Strict is Sold \/ 0b01,
    put_atts( X, strictness(Strict))
   ),
   (
    arith_eval( Bound>L),
    Strict is Sold \/ 0b01,
    put_atts( X, [type(t_Lu(L,Bound)),strictness(Strict)])
   )).
%
% update active:
%
uius( t_U(U),	 X, _Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
   (
    Strict is Sold \/ 0b01,
    put_atts( X, strictness(Strict))
   ),
   (
    Strict is Sold \/ 0b01,
    ( lb( X, Vlb-Vb-Lb),
      arith_eval( Bound =< Lb+U) ->
	put_atts( X, [type(t_U(Bound)),strictness(Strict)]),
	pivot_a( Vlb, X, Vb, t_u(Bound)),
	reconsider( X)
    ;
	put_atts( X, [type(t_U(Bound)),strictness(Strict)]),
	arith_eval( Bound-U, Delta),
	backsubst_delta( X, Delta)
    )
   )).
uius( t_lU(L,U), X, _Lin, Bound, Sold) :-
  case_signum( U-Bound,
    true,
   (
    Strict is Sold \/ 0b01,
    put_atts( X, strictness(Strict))
   ),
   (
    arith_eval( Bound>L),
    Strict is Sold \/ 0b01,
    ( lb( X, Vlb-Vb-Lb),
      arith_eval( Bound =< Lb+U) ->
	put_atts( X, [type(t_lU(L,Bound)),strictness(Strict)]),
	pivot_a( Vlb, X, Vb, t_lu(L,Bound)),
	reconsider( X)
    ;
	put_atts( X, [type(t_lU(L,Bound)),strictness(Strict)]),
	arith_eval( Bound-U, Delta),
	backsubst_delta( X, Delta)
    )
   )).


uil( t_none,	X, _Lin, Bound, _Sold) :-
  put_atts( X, [type(t_l(Bound)),strictness(0b00)]).
uil( t_l(L),	X, _Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
    true,
   (
    Strict is Sold /\ 0b01,
    put_atts( X, [type(t_l(Bound)),strictness(Strict)])
   )).
uil( t_u(U),	X, Lin, Bound, _Sold) :-
  case_signum( U-Bound,
    fail,
    solve_bound( Lin, Bound),
    put_atts( X, type(t_lu(Bound,U)))).
uil( t_U(U),	X, Lin, Bound, _Sold) :-
  case_signum( U-Bound,
    fail,
    solve_bound( Lin, Bound),
    put_atts( X, type(t_lU(Bound,U)))).
uil( t_lu(L,U), X, Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
    true,
   (
    case_signum( U-Bound,
      fail,
     (
      Sold /\ 0b01 =:= 0,
      solve_bound( Lin, Bound)
     ),
     (
      Strict is Sold /\ 0b01,
      put_atts( X, [type(t_lu(Bound,U)),strictness(Strict)])
     ))
   )).
uil( t_lU(L,U), X, Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
    true,
   (
    case_signum( U-Bound,
      fail,
     (
      Sold /\ 0b01 =:= 0,
      solve_bound( Lin, Bound)
     ),
     (
      Strict is Sold /\ 0b01,
      put_atts( X, [type(t_lU(Bound,U)),strictness(Strict)])
     ))
   )).
%
% update active bound: % { a>=100,d=<5000,c>=10,-2*a+d-c=10,a>=2490 }.
%
uil( t_L(L),	X, _Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
    true,
   (
    Strict is Sold /\ 0b01,
    ( ub( X, Vub-Vb-Ub),
      arith_eval( Bound >= Ub+L) ->
	put_atts( X, [type(t_L(Bound)),strictness(Strict)]),
	pivot_a( Vub, X, Vb, t_l(Bound)),
	reconsider( X)
    ;	%
	% max(X) >= Ub, no implied value missed
	%
	put_atts( X, [type(t_L(Bound)),strictness(Strict)]),
	arith_eval( Bound-L, Delta),
	backsubst_delta( X, Delta)
    )
   )).
uil( t_Lu(L,U), X, Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
    true,
   (
    case_signum( U-Bound,
      fail,
     (
      Sold /\ 0b01 =:= 0,
      solve_bound( Lin, Bound)
     ),
     (
      Strict is Sold /\ 0b01,
      ( ub( X, Vub-Vb-Ub),
	arith_eval( Bound >= Ub+L) ->
	  put_atts( X, [type(t_Lu(Bound,U)),strictness(Strict)]),
	  pivot_a( Vub, X, Vb, t_lu(Bound,U)),
	  reconsider( X)
      ;
	  put_atts( X, [type(t_Lu(Bound,U)),strictness(Strict)]),
	  arith_eval( Bound-L, Delta),
	  backsubst_delta( X, Delta)
      )
     )))).


uils( t_none,	 X, _Lin, Bound, _Sold) :-
  put_atts( X, [type(t_l(Bound)),strictness(0b10)]).
uils( t_l(L),	 X, _Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
   (
    Strict is Sold \/ 0b10,
    put_atts( X, strictness(Strict))
   ),
   (
    Strict is Sold \/ 0b10,
    put_atts( X, [type(t_l(Bound)),strictness(Strict)])
   )).
uils( t_u(U),	 X, _Lin, Bound, Sold) :-
  arith_eval( U>Bound),
  Strict is Sold \/ 0b10,
  put_atts( X, [type(t_lu(Bound,U)),strictness(Strict)]).
uils( t_U(U),	 X, _Lin, Bound, Sold) :-
  arith_eval( U>Bound),
  Strict is Sold \/ 0b10,
  put_atts( X, [type(t_lU(Bound,U)),strictness(Strict)]).
uils( t_lu(L,U), X, _Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
   (
    Strict is Sold \/ 0b10,
    put_atts( X, strictness(Strict))
   ),
   (
    arith_eval( U>Bound),
    Strict is Sold \/ 0b10,
    put_atts( X, [type(t_lu(Bound,U)),strictness(Strict)])
   )).
uils( t_lU(L,U), X, _Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
   (
    Strict is Sold \/ 0b10,
    put_atts( X, strictness(Strict))
   ),
   (
    arith_eval( U>Bound),
    Strict is Sold \/ 0b10,
    put_atts( X, [type(t_lU(Bound,U)),strictness(Strict)])
   )).
%
% update active bound:
%
uils( t_L(L),	 X, _Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
   (
    Strict is Sold \/ 0b10,
    put_atts( X, strictness(Strict))
   ),
   (
    Strict is Sold \/ 0b10,
    ( ub( X, Vub-Vb-Ub),
      arith_eval( Bound >= Ub+L) ->
	put_atts( X, [type(t_L(Bound)),strictness(Strict)]),
	pivot_a( Vub, X, Vb, t_l(Bound)),
	reconsider( X)
    ;	%
	% max(X) >= Ub, no implied value missed
	%
	put_atts( X, [type(t_L(Bound)),strictness(Strict)]),
	arith_eval( Bound-L, Delta),
	backsubst_delta( X, Delta)
    ))).
uils( t_Lu(L,U), X, _Lin, Bound, Sold) :-
  case_signum( Bound-L,
    true,
   (
    Strict is Sold \/ 0b10,
    put_atts( X, strictness(Strict))
   ),
   (
    arith_eval( U>Bound),
    Strict is Sold \/ 0b10,
    ( ub( X, Vub-Vb-Ub),
      arith_eval( Bound >= Ub+L) ->
	put_atts( X, [type(t_Lu(Bound,U)),strictness(Strict)]),
	pivot_a( Vub, X, Vb, t_lu(Bound,U)),
	reconsider( X)
    ;
	put_atts( X, [type(t_Lu(Bound,U)),strictness(Strict)]),
	arith_eval( Bound-L, Delta),
	backsubst_delta( X, Delta)
    ))).

reconsider_upper( X, Lin, U) :-
  decompose( Lin, H, R, I),
  arith_eval( R+I >= U),
  !,
  dec_step( H, Status),
  rcbl_status( Status, X, [], Binds,[], u(U)),
  export_binding( Binds).
reconsider_upper( _, _, _).

reconsider_lower( X, Lin, L) :-
  decompose( Lin, H, R, I),
  arith_eval( R+I =< L),
  !,
  inc_step( H, Status),
  rcbl_status( Status, X, [], Binds,[], l(L)),
  export_binding( Binds).
reconsider_lower( _, _, _).

%
% lin is dereferenced
%
solve_bound( Lin, Bound) :-
  arith_eval( Bound =:= 0),
  !,
  solve( Lin).
solve_bound( Lin, Bound) :-
  arith_eval( -Bound, Nb),
  normalize_scalar( Nb, Nbs),
  add_linear_11( Nbs, Lin, Eq),
  solve( Eq).
