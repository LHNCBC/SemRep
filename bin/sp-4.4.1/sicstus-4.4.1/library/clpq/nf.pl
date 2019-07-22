%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   nf.pl                                                  %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- use_module( library(types), [illarg/3]).
:- use_module( library(terms), [term_variables_set/2]).
:- use_module( geler).

% -------------------------------------------------------------------------

{   Rel   } :- var( Rel), !, illarg(var, {Rel}, 1).
{   R,Rs  } :- !, {R}, {Rs}.
{   R;Rs  } :- !, ({R} ; {Rs}).			% for entailment checking
{  L < R  } :- !, nf( L-R, Nf), submit_lt( Nf).
{  L > R  } :- !, nf( R-L, Nf), submit_lt( Nf).
{  L =< R } :- !, nf( L-R, Nf), submit_le( Nf).
{ <=(L,R) } :- !, nf( L-R, Nf), submit_le( Nf).
{  L >= R } :- !, nf( R-L, Nf), submit_le( Nf).
{ L =\= R } :- !, nf( L-R, Nf), submit_ne( Nf).
{ L =:= R } :- !, nf( L-R, Nf), submit_eq( Nf).
{ L  =	R } :- !, nf( L-R, Nf), submit_eq( Nf).
{   Rel   } :- illarg(type(constraint), {Rel}, 1).

%
% s -> c = ~s v c = ~(s /\ ~c) 
% where s is the store and c is the constraint for which
% we want to know whether it is entailed.
% 
entailed( C) :-
	negate( C, Cn),
	\+ { Cn }.

negate( Rel,   _) :- var( Rel), !, illarg(var, entailed(Rel), 1).
negate( (A,B), (Na;Nb)) :- !, negate( A, Na), negate( B, Nb).	
negate( (A;B), (Na,Nb)) :- !, negate( A, Na), negate( B, Nb).	
negate( A<B,   A>=B) :- !.
negate( A>B,   A=<B) :- !.
negate( A=<B,  A>B) :- !.
negate( A>=B,  A<B) :- !.
negate( A=:=B, A=\=B) :- !.
negate( A=B,   A=\=B) :- !.
negate( A=\=B, A=:=B) :- !.
negate( Rel,   _) :- illarg(type(constraint), entailed(Rel), 1).

/*
   Cases:  a) Nf=[]
	   b) Nf=[A]
	      b1) A=k
	      b2) invertible(A)
	      b3) linear -> A=0
	      b4) nonlinear -> geler
	   c) Nf=[A,B|Rest]
	      c1) A=k
		  c11) B=X^+-1, Rest=[] -> B=
		  c12) invertible(A,B)
		  c13) linear(B|Rest)
		  c14) geler
	      c2) linear(Nf)
	      c3) nonlinear -> geler
*/
submit_eq( []). 				% trivial success
submit_eq( [T|Ts]) :-
  submit_eq( Ts, T).

submit_eq( [],	   A) :- submit_eq_b( A).
submit_eq( [B|Bs], A) :- submit_eq_c( A, B, Bs).

submit_eq_b( v(_,[])) :- !, fail.		% b1: trivial failure
submit_eq_b( v(_,[X^P])) :-			% b2,b3: n*x^p=0 -> x=0
  var( X),
  P > 0,
  !,
  arith_eval( 0, Z),
  export_binding( X, Z).
submit_eq_b( v(_,[NL^1])) :-			% b2
  nonvar( NL),
  arith_eval( 0, Z),
  nl_invertible( NL, X, Z, Inv),
  !,
  nf( -Inv, S),
  nf_add( X, S, New),
  submit_eq( New).
submit_eq_b( Term) :-				% b4
  term_variables_set( Term, Vs),
  geler( Vs, resubmit_eq([Term])).

submit_eq_c( v(I,[]), B, Rest) :- !,
  submit_eq_c1( Rest, B, I).
submit_eq_c( A, B, Rest) :-			% c2
  A=v(_,[X^1]), var(X),
  B=v(_,[Y^1]), var(Y),
  linear( Rest),
  !,
  Hom = [A,B|Rest],
  % 'solve_='( Hom).
  nf_length( Hom, 0, Len),
  log_deref( Len, Hom, [], HomD),
  solve( HomD).
submit_eq_c( A, B, Rest) :-			% c3
  Norm = [A,B|Rest],
  term_variables_set( Norm, Vs),
  geler( Vs, resubmit_eq(Norm)).

submit_eq_c1( [], v(K,[X^P]), I) :-		% c11
  var( X),
  ( P =  1, !, arith_eval( -I/K, Val), export_binding( X, Val)
  ; P = -1, !, arith_eval( -K/I, Val), export_binding( X, Val)
  ).
submit_eq_c1( [], v(K,[NL^P]), I) :-		% c12
  nonvar( NL),
  ( P =  1, arith_eval( -I/K, Y)
  ; P = -1, arith_eval( -K/I, Y)
  ),
  nl_invertible( NL, X, Y, Inv),
  !,
  nf( -Inv, S),
  nf_add( X, S, New),
  submit_eq( New).
submit_eq_c1( Rest, B, I) :-			% c13
  B=v(_,[Y^1]), var(Y),
  linear( Rest),
  !,
  % 'solve_='( [v(I,[]),B|Rest]).
  Hom = [B|Rest],
  nf_length( Hom, 0, Len),
  normalize_scalar( I, Nonvar),
  log_deref( Len, Hom, [], HomD),
  add_linear_11( Nonvar, HomD, LinD),
  solve( LinD).
submit_eq_c1( Rest, B, I) :-			% c14
  Norm = [v(I,[]),B|Rest],
  term_variables_set( Norm, Vs),
  geler( Vs, resubmit_eq(Norm)).

% -----------------------------------------------------------------------

submit_lt( []) :- fail. 			% trivial failure
submit_lt( [A|As]) :-
  submit_lt( As, A).

submit_lt( [],	   v(K,P)) :- submit_lt_b( P, K).
submit_lt( [B|Bs], A) :- submit_lt_c( Bs, A, B).

submit_lt_b( [],    I) :- !, arith_eval( I<0).
submit_lt_b( [X^1], K) :-
  var(X),
  !,
  ( arith_eval( K>0) ->
      ineq_one_s_p_0( X)
  ;
      ineq_one_s_n_0( X)
  ).
submit_lt_b( P, K) :-
  term_variables_set( P, Vs),
  geler( Vs, resubmit_lt([v(K,P)])).

submit_lt_c( [],   A, B) :-
  A=v(I,[]),
  B=v(K,[Y^1]), var(Y),
  !,
  ineq_one( strict, Y, K, I).
submit_lt_c( Rest, A, B) :-
  Norm = [A,B|Rest],
  ( linear(Norm) ->
      'solve_<'( Norm)
  ;
      term_variables_set( Norm, Vs),
      geler( Vs, resubmit_lt(Norm))
  ).

submit_le( []). 				% trivial success
submit_le( [A|As]) :-
  submit_le( As, A).

submit_le( [],	   v(K,P)) :- submit_le_b( P, K).
submit_le( [B|Bs], A) :- submit_le_c( Bs, A, B).

submit_le_b( [],    I) :- !, arith_eval( I=<0).
submit_le_b( [X^1], K) :-
  var(X),
  !,
  ( arith_eval( K>0) ->
      ineq_one_n_p_0( X)
  ;
      ineq_one_n_n_0( X)
  ).
submit_le_b( P, K) :-
  term_variables_set( P, Vs),
  geler( Vs, resubmit_le([v(K,P)])).

submit_le_c( [],   A, B) :-
  A=v(I,[]),
  B=v(K,[Y^1]), var(Y),
  !,
  ineq_one( nonstrict, Y, K, I).
submit_le_c( Rest, A, B) :-
  Norm = [A,B|Rest],
  ( linear(Norm) ->
      'solve_=<'( Norm)
  ;
      term_variables_set( Norm, Vs),
      geler( Vs, resubmit_le(Norm))
  ).

submit_ne( Norm1) :-
  ( nf_constant( Norm1, K) ->
      arith_eval( K=\=0)
  ; linear( Norm1) ->
      'solve_=\\='( Norm1)
  ;
      term_variables_set( Norm1, Vs),
      geler( Vs, resubmit_ne(Norm1))
  ).


linear( []).
linear( v(_,Ps)) :- linear_ps( Ps).
linear( [A|As]) :-
  linear( A),
  linear( As).

linear_ps( []).
linear_ps( [V^1]) :- var( V).			% excludes sin(_), ...

%
% Goal delays until Term gets linear.
% At this time, Var will be bound to the normalform of Term.
%
:- meta_predicate wait_linear( ?, ?, 0).
%
wait_linear( Term, Var, Goal) :-
  nf( Term, Nf),
  ( linear( Nf) ->
      Var = Nf,
      call( Goal)
  ;
      term_variables_set( Nf, Vars),
      geler( Vars, wait_linear_retry(Nf,Var,Goal))
  ).

%
% geler clients
%
resubmit_eq( N) :-
  repair( N, Norm),
  submit_eq( Norm).

resubmit_lt( N) :-
  repair( N, Norm),
  submit_lt( Norm).

resubmit_le( N) :-
  repair( N, Norm),
  submit_le( Norm).

resubmit_ne( N) :-
  repair( N, Norm),
  submit_ne( Norm).

wait_linear_retry( Nf0, Var, Goal) :-
  repair( Nf0, Nf),
  ( linear( Nf) ->
      Var = Nf,
      call( Goal)
  ;
      term_variables_set( Nf, Vars),
      geler( Vars, wait_linear_retry(Nf,Var,Goal))
  ).

% -----------------------------------------------------------------------

/*
invertible( [v(Mone,[]),v(One,[X^Px,Y^Py])], Norm) :-
  Px+Py =:= 0,
  abs(Px) mod 2 =:= 1,				% odd powers only ...
  arith_eval( 1, One),
  arith_eval( -1, Mone),
  !,
  ( Px < 0 ->
     {X=\=0}
  ;
     {Y=\=0}
  ),
  nf( X-Y, Norm).				% x=y
*/

nl_invertible( sin(X),	 X, Y, Res) :- arith_eval( asin(Y), Res).
nl_invertible( cos(X),	 X, Y, Res) :- arith_eval( acos(Y), Res).
nl_invertible( tan(X),	 X, Y, Res) :- arith_eval( atan(Y), Res).
nl_invertible( exp(B,C), X, A, Res) :-
  ( nf_constant( B, Kb) ->
      arith_eval(A>0),
      arith_eval(Kb>0),
      arith_eval(Kb=\=1),
      X = C,
      arith_eval( log(A)/log(Kb), Res)
  ; nf_constant( C, Kc),
      \+ (arith_eval(A=:=0),arith_eval(Kc=<0)),
      X = B,
      arith_eval( exp(A,1/Kc), Res)
  ).

% -----------------------------------------------------------------------

nf( X, Norm) :- var(X), !,
  Norm = [v(One,[X^1])],
  arith_eval( 1, One).
nf( X, Norm) :- number(X), !,
  nf_number( X, Norm).
%
nf( rat(N,D), Norm) :- !,
  nf_number( rat(N,D), Norm).
%
nf( #(Const), Norm) :-
  monash_constant( Const, Value),
  !,
  ( arith_eval( 1, rat(1,1)) ->
	nf_number( Value, Norm) 		% swallows #(zero) ... ok in Q
  ;
	arith_normalize( Value, N),		% in R we want it
	Norm = [v(N,[])]
  ).
%
nf( -A, Norm) :- !,
  nf( A, An),
  arith_eval( -1, K),
  nf_mul_factor( v(K,[]), An, Norm).
nf( +A, Norm) :- !,
  nf( A, Norm).
%
nf( A+B, Norm) :- !,
  nf( A, An),
  nf( B, Bn),
  nf_add( An, Bn, Norm).
nf( A-B, Norm) :- !,
  nf(  A, An),
  nf( -B, Bn),
  nf_add( An, Bn, Norm).
%
nf( A*B, Norm) :- !,
  nf( A, An),
  nf( B, Bn),
  nf_mul( An, Bn, Norm).
nf( A/B, Norm) :- !,
  nf( A, An),
  nf( B, Bn),
  nf_div( Bn, An, Norm).
%
nf( Term, Norm) :-
  nonlin_1( Term, Arg, Skel, Sa1),
  !,
  nf( Arg, An),
  nf_nonlin_1( Skel, An, Sa1, Norm).
nf( Term, Norm) :-
  nonlin_2( Term, A1,A2, Skel, Sa1, Sa2),
  !,
  nf( A1, A1n),
  nf( A2, A2n),
  nf_nonlin_2( Skel, A1n, A2n, Sa1, Sa2, Norm).
%
nf( Term, Norm) :-
	illarg(type(expression), nf( Term, Norm), 1).

nf_number( N, Res) :-
  nf_number( N),
  arith_normalize( N, Normal),
  ( arith_eval( Normal=:=0) ->
      Res = []
  ;
      Res = [v(Normal,[])]
  ).

nf_number( N) :- number( N),
	!.	/* MC 980507 */
nf_number( N) :- compound( N), N=rat(_,_).	% sicstus

nonlin_1( abs(X), X, abs(Y), Y).
nonlin_1( sin(X), X, sin(Y), Y).
nonlin_1( cos(X), X, cos(Y), Y).
nonlin_1( tan(X), X, tan(Y), Y).

nonlin_2( min(A,B), A,B, min(X,Y), X, Y).
nonlin_2( max(A,B), A,B, max(X,Y), X, Y).
nonlin_2( exp(A,B), A,B, exp(X,Y), X, Y).
nonlin_2( pow(A,B), A,B, exp(X,Y), X, Y).	% pow->exp
nonlin_2( A^B,	    A,B, exp(X,Y), X, Y).

nf_nonlin_1( Skel, An, S1, Norm) :-
  ( nf_constant( An, S1) ->
     nl_eval( Skel, Res),
     nf_number( Res, Norm)
  ;
     S1 = An,
     arith_eval( 1, One),
     Norm = [v(One,[Skel^1])]
  ).

nf_nonlin_2( Skel, A1n, A2n, S1, S2, Norm) :-
  ( nf_constant( A1n, S1),
    nf_constant( A2n, S2) ->
      nl_eval( Skel, Res),
      nf_number( Res, Norm)
  ; Skel=exp(_,_),
    nf_constant( A2n, Exp),
    integerp( Exp, I) ->
      nf_power( I, A1n, Norm)
  ;
      S1 = A1n,
      S2 = A2n,
      arith_eval( 1, One),
      Norm = [v(One,[Skel^1])]
  ).


nl_eval( abs(X),   R) :- arith_eval( abs(X), R).
nl_eval( sin(X),   R) :- arith_eval( sin(X), R).
nl_eval( cos(X),   R) :- arith_eval( cos(X), R).
nl_eval( tan(X),   R) :- arith_eval( tan(X), R).
%
nl_eval( min(X,Y), R) :- arith_eval( min(X,Y), R).
nl_eval( max(X,Y), R) :- arith_eval( max(X,Y), R).
nl_eval( exp(X,Y), R) :- arith_eval( exp(X,Y), R).

monash_constant( X,	_) :- var(X), !, fail.
monash_constant( p,	3.141592653589793).
monash_constant( pi,	3.141592653589793).
monash_constant( e,	2.718281828459045).
monash_constant( zero,	Eps) :- arith_eps( Eps).

%
% check if a Nf consists of just a constant
%
nf_constant( [],	Z) :- arith_eval( 0, Z).
nf_constant( [v(K,[])], K).

%
% this depends on the polynf ordering, i.e. [] < [X^1] ...
%
split( [],	  [], Z) :- arith_eval( 0, Z).
split( [First|T], H,  I) :-
  ( First=v(I,[]) ->
      H=T
  ;
      arith_eval( 0, I),
      H = [First|T]
  ).

%
% runtime predicate
%
:- mode nf_add( +, +, ?).
%
nf_add( [],	Bs, Bs).
nf_add( [A|As], Bs, Cs) :-
  nf_add( Bs, A, As, Cs).

:- mode nf_add( +, +, +, ?).
%
nf_add( [],	A, As, Cs) :- Cs = [A|As].
nf_add( [B|Bs], A, As, Cs) :-
  A = v(Ka,Pa),
  B = v(Kb,Pb),
  compare( Rel, Pa, Pb),
  nf_add_case( Rel, A, As, Cs, B, Bs, Ka, Kb, Pa).

:- mode nf_add_case( +, +, +, -, +, +, +, +, +).
%
nf_add_case( <, A, As, Cs, B, Bs, _, _, _) :-
	Cs=[A|Rest],
	nf_add( As, B, Bs, Rest).
nf_add_case( >, A, As, Cs, B, Bs, _, _, _) :-
	Cs=[B|Rest],
	nf_add( Bs, A, As, Rest).
nf_add_case( =, _, As, Cs, _, Bs, Ka, Kb, Pa) :-
	arith_eval( Ka+Kb, Kc),
	( arith_eval( Kc=:=0 ) ->
	    nf_add( As, Bs, Cs)
	;
	    Cs=[v(Kc,Pa)|Rest],
	    nf_add( As, Bs, Rest)
	).

:- mode nf_mul( +, +, -).
%
nf_mul( A, B, Res) :-
  nf_length( A, 0, LenA),
  nf_length( B, 0, LenB),
  nf_mul_log( LenA, A, [], LenB, B, Res).

nf_mul_log( 0, As,     As, _,  _, []) :- !.
nf_mul_log( 1, [A|As], As, Lb, B, R) :- !,
  nf_mul_factor_log( Lb, B, [], A, R).
nf_mul_log( 2, [A1,A2|As], As, Lb, B, R) :- !,
  nf_mul_factor_log( Lb, B, [], A1, A1b),
  nf_mul_factor_log( Lb, B, [], A2, A2b),
  nf_add( A1b, A2b, R).
nf_mul_log( N, A0, A2, Lb, B, R) :-
  P is N>>1,
  Q is N-P,
  nf_mul_log( P, A0, A1, Lb, B, Rp),
  nf_mul_log( Q, A1, A2, Lb, B, Rq),
  nf_add( Rp, Rq, R).

:- mode nf_add_2( +, +, -).
%
nf_add_2( Af, Bf, Res) :-			%  unfold: nf_add( [Af], [Bf], Res).
  Af = v(Ka,Pa),
  Bf = v(Kb,Pb),
  compare( Rel, Pa, Pb),
  nf_add_2_case( Rel, Af, Bf, Res, Ka, Kb, Pa).

:- mode nf_add_2_case( +, +, +, -, +, +, +).
%
nf_add_2_case( <, Af, Bf, [Af,Bf], _, _, _).
nf_add_2_case( >, Af, Bf, [Bf,Af], _, _, _).
nf_add_2_case( =, _,  _,  Res, Ka, Kb, Pa) :-
	arith_eval( Ka+Kb, Kc),
	( arith_eval( Kc=:=0 ) ->
	    Res = []
	;
	    Res=[v(Kc,Pa)]
	).

%
% multiply with a scalar =\= 0
%
nf_mul_k( [],	       _, []).
nf_mul_k( [v(I,P)|Vs], K, [v(Ki,P)|Vks]) :-
  arith_eval( K*I, Ki),
  nf_mul_k( Vs, K, Vks).

nf_mul_factor( v(K,[]), Sum, Res) :- !, nf_mul_k( Sum, K, Res).
nf_mul_factor( F,	Sum, Res) :-
  nf_length( Sum, 0, Len),
  nf_mul_factor_log( Len, Sum, [], F, Res).

nf_mul_factor_log( 0, As,     As, _, []) :- !.
nf_mul_factor_log( 1, [A|As], As, F, [R]) :- !,
  mult( A, F, R).
nf_mul_factor_log( 2, [A,B|As], As, F, Res) :- !,
  mult( A, F, Af),
  mult( B, F, Bf),
  nf_add_2( Af, Bf, Res).
nf_mul_factor_log( N, A0,   A2, F, R) :-
  P is N>>1,
  Q is N-P,
  nf_mul_factor_log( P, A0, A1, F, Rp),
  nf_mul_factor_log( Q, A1, A2, F, Rq),
  nf_add( Rp, Rq, R).

mult( v(Ka,La), v(Kb,Lb), v(Kc,Lc)) :-
  arith_eval( Ka*Kb, Kc),
  pmerge( La, Lb, Lc).

pmerge( [],	Bs, Bs).
pmerge( [A|As], Bs, Cs) :-
  pmerge( Bs, A, As, Cs).

:- mode pmerge(+,+,+,-).
%
pmerge( [],	A, As, Res) :- Res = [A|As].
pmerge( [B|Bs], A, As, Res) :-
  A=Xa^Ka,
  B=Xb^Kb,
  compare( R, Xa, Xb),
  pmerge_case( R, A, As, Res, B, Bs, Ka, Kb, Xa).

:- mode pmerge_case( +, +, +, -, +, +, +, +, ?).
%
pmerge_case( <, A, As, Res, B, Bs, _, _, _) :-
	Res = [A|Tail],
	pmerge( As, B, Bs, Tail).
pmerge_case( >, A, As, Res, B, Bs, _, _, _) :-
	Res = [B|Tail],
	pmerge( Bs, A, As, Tail).
pmerge_case( =, _, As, Res, _, Bs, Ka, Kb, Xa) :-
	Kc is Ka+Kb,
	( Kc=:=0 ->
	    pmerge( As, Bs, Res)
	;
	    Res = [Xa^Kc|Tail],
	    pmerge( As, Bs, Tail)
	).

nf_div( [],	  _,   _) :- !, zero_division.
nf_div( [v(K,P)], Sum, Res) :- !,
  arith_eval( 1/K, Ki),
  mult_exp( P, -1, Pi),
  nf_mul_factor( v(Ki,Pi), Sum, Res).
nf_div( D, A, [v(One,[(A/D)^1])]) :-
  arith_eval( 1, One).

zero_division :- fail.				% raise_exception(_) ?

mult_exp( [],	    _, []).
mult_exp( [X^P|Xs], K, [X^I|Tail]) :-
  I is K*P,
  mult_exp( Xs, K, Tail).

%
% raise to integer powers
%
% | ?- time({(1+X+Y+Z)^15=0}).
% Timing 00:00:02.610	  2.610   iterative
% Timing 00:00:00.660	  0.660   binomial
nf_power( N, Sum, Norm) :-
  integer( N),
  compare( Rel, N, 0),
  ( Rel = (<) ->
      Pn is -N,
      % nf_power_pos( Pn, Sum, Inorm),
      binom( Sum, Pn, Inorm),
      arith_eval( 1, One),
      nf_div( Inorm, [v(One,[])], Norm)
  ; Rel = (>) ->
      % nf_power_pos( N, Sum, Norm)
      binom( Sum, N, Norm)
  ; Rel = (=) ->					% 0^0 is indeterminate but we say 1
      arith_eval( 1, One),
      Norm = [v(One,[])]
  ).


%
% N>0
%
nf_power_pos( 1, Sum, Norm) :- !, Sum = Norm.
nf_power_pos( N, Sum, Norm) :-
  N1 is N-1,
  nf_power_pos( N1, Sum, Pn1),
  nf_mul( Sum, Pn1, Norm).

%
% N>0
%
binom( Sum,    1, Power) :- !, Power = Sum.
binom( [],     _, []).
binom( [A|Bs], N, Power) :-
  ( Bs=[] ->
      nf_power_factor( A, N, Ap),
      Power = [Ap]
  ; Bs=[_|_] ->
      arith_eval( 1, One),
      factor_powers( N, A, v(One,[]), Pas),
      sum_powers( N, Bs, [v(One,[])], Pbs, []),
      combine_powers( Pas, Pbs, 0, N, 1, [], Power)
  ).

combine_powers( [],	[],	_, _, _, Pi, Pi).
combine_powers( [A|As], [B|Bs], L, R, C, Pi, Po) :-
  nf_mul( A, B, Ab),
  arith_normalize( C, Cn),
  nf_mul_k( Ab, Cn, Abc),
  nf_add( Abc, Pi, Pii),
  L1 is L+1,
  R1 is R-1,
  C1 is C*R//L1,
  combine_powers( As, Bs, L1, R1, C1, Pii, Po).


nf_power_factor( v(K,P), N, v(Kn,Pn)) :-
  arith_normalize( N, Nn),
  arith_eval( exp(K,Nn), Kn),
  mult_exp( P, N, Pn).

factor_powers( 0, _, Prev, [[Prev]]) :- !.
factor_powers( N, F, Prev, [[Prev]|Ps]) :-
  N1 is N-1,
  mult( Prev, F, Next),
  factor_powers( N1, F, Next, Ps).

sum_powers( 0, _, Prev, [Prev|Lt], Lt) :- !.
sum_powers( N, S, Prev, L0, Lt) :-
  N1 is N-1,
  nf_mul( S, Prev, Next),
  sum_powers( N1, S, Next, L0, [Prev|Lt]).

% ------------------------------------------------------------------------------

repair( Sum, Norm) :-
  nf_length( Sum, 0, Len),
  repair_log( Len, Sum, [], Norm).

repair_log( 0, As, As, []) :- !.
repair_log( 1, [v(Ka,Pa)|As], As, R) :- !,
  repair_term( Ka, Pa, R).
repair_log( 2, [v(Ka,Pa),v(Kb,Pb)|As], As, R) :- !,
  repair_term( Ka, Pa, Ar),
  repair_term( Kb, Pb, Br),
  nf_add( Ar, Br, R).
repair_log( N, A0, A2, R) :-
  P is N>>1,
  Q is N-P,
  repair_log( P, A0, A1, Rp),
  repair_log( Q, A1, A2, Rq),
  nf_add( Rp, Rq, R).


repair_term( K, P, Norm) :-
  length( P, Len),
  arith_eval( 1, One),
  repair_p_log( Len, P, [], Pr, [v(One,[])], Sum),
  nf_mul_factor( v(K,Pr), Sum, Norm).

repair_p_log( 0, Ps,	   Ps, [], L0, L0) :- !.
repair_p_log( 1, [X^P|Ps], Ps, R,  L0, L1) :- !,
  repair_p( X, P, R, L0, L1).
repair_p_log( 2, [X^Px,Y^Py|Ps], Ps, R, L0,L2) :- !,
  repair_p( X, Px, Rx, L0, L1),
  repair_p( Y, Py, Ry, L1, L2),
  pmerge( Rx, Ry, R).
repair_p_log( N, P0,	   P2, R,  L0, L2) :-
  P is N>>1,
  Q is N-P,
  repair_p_log( P, P0, P1, Rp, L0, L1),
  repair_p_log( Q, P1, P2, Rq, L1, L2),
  pmerge( Rp, Rq, R).


repair_p( Term, P, [Term^P], L0, L0) :- var( Term).
repair_p( Term, P, [],	     L0, L1) :- nonvar( Term),
  repair_p_one( Term, TermN),
  nf_power( P, TermN, TermNP),
  nf_mul( TermNP, L0, L1).

%
% An undigested term a/b is distinguished from an
% digested one by the fact that its arguments are
% digested -> cuts after repair of args!
%
repair_p_one( Term, TermN) :-
  nf_number( Term, TermN),			% freq. shortcut for nf/2 case below
  !.
repair_p_one( A1/A2, TermN) :-
  repair( A1, A1n),
  repair( A2, A2n),
  !,
  nf_div( A2n, A1n, TermN).
repair_p_one( Term, TermN) :-
  nonlin_1( Term, Arg, Skel, Sa),
  repair( Arg, An),
  !,
  nf_nonlin_1( Skel, An, Sa, TermN).
repair_p_one( Term, TermN) :-
  nonlin_2( Term, A1,A2, Skel, Sa1, Sa2),
  repair( A1, A1n),
  repair( A2, A2n),
  !,
  nf_nonlin_2( Skel, A1n, A2n, Sa1, Sa2, TermN).
repair_p_one( Term, TermN) :-
  nf( Term, TermN).

:- mode nf_length( +, +, -).
%
nf_length( [],	  Li, Li).
nf_length( [_|R], Li, Lo) :-
  Lii is Li+1,
  nf_length( R, Lii, Lo).

% ------------------------------------------------------------------------------

nf2term( [],     Z) :- arith_eval( 0, Z).
nf2term( [F|Fs], T) :- 
	f02t( F, T0),
	yfx( Fs, T0, T).

yfx( [],     T0, T0).
yfx( [F|Fs], T0, TN) :-
	fn2t( F, Ft, Op),
	T1 =.. [Op,T0,Ft],
	yfx( Fs, T1, TN).

f02t( v(K,P), T) :-
	( P = [] ->
	    T = K
	; arith_eval( K=:=1) ->
	    p2term( P, T)
	; arith_eval( K=:= -1) ->
	    T = -Pt,
	    p2term( P, Pt)
	;
	    T = K*Pt,
	    p2term( P, Pt)
	).

fn2t( v(K,P),  Term, Op) :-
  ( arith_eval( K=:=1) ->
      Term = Pt, Op = (+)
  ; arith_eval( K=:= -1) ->
      Term = Pt, Op = (-)
  ; arith_eval( K<0) ->
      arith_eval( -K, Kf),
      Term = Kf*Pt, Op = (-)
  ;
      Term = K*Pt, Op = (+)
  ),
  p2term( P, Pt).

p2term( [X^P|Xs], Term) :-
  ( Xs=[] ->
     pe2term( X, Xt),
     exp2term( P, Xt, Term)
  ; Xs=[_|_] ->
     Term = Xst*Xtp,
     pe2term( X, Xt),
     exp2term( P, Xt, Xtp),
     p2term( Xs, Xst)
  ).

exp2term( 1, X, X) :- !.
exp2term(-1, X, One/X) :- !, arith_eval( 1, One).
exp2term( P, X, Term) :-
  arith_normalize( P, Pn),
  % Term = exp(X,Pn).
  Term = X^Pn.

pe2term( X, Term) :- var(X), Term = X.
pe2term( X, Term) :- nonvar(X),
  X =.. [F|Args],
  pe2term_args( Args, Argst),
  Term =.. [F|Argst].

pe2term_args( [],     []).
pe2term_args( [A|As], [T|Ts]) :-
  nf2term( A, T),
  pe2term_args( As, Ts).

