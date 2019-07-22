%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   store.pl                                               %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% All constants to canonical rep.
%
normalize_scalar( S, [N,Z]) :-
  arith_normalize( S, N),
  arith_eval( 0, Z).

renormalize( List, Lin) :-
  decompose( List, Hom, R, I),
  length( Hom, Len),
  renormalize_log( Len, Hom, [], Lin0),
  add_linear_11( [I,R], Lin0, Lin).

renormalize_log( 1, [Term|Xs], Xs, Lin) :- !,
  Term = X*_,
  renormalize_log_one( X, Term, Lin).
renormalize_log( 2, [A,B|Xs], Xs, Lin) :- !,
  A = X*_,
  B = Y*_,
  renormalize_log_one( X, A, LinA),
  renormalize_log_one( Y, B, LinB),
  add_linear_11( LinA, LinB, Lin).
renormalize_log( N, L0, L2, Lin) :-
  P is N>>1,
  Q is N-P,
  renormalize_log( P, L0, L1, Lp),
  renormalize_log( Q, L1, L2, Lq),
  add_linear_11( Lp, Lq, Lin).

renormalize_log_one( X,        Term, Res) :- var(X),
  arith_eval( 0, Z),
  Res = [Z,Z,Term].
renormalize_log_one( X,        Term, Res) :- nonvar(X),
  Term = X*K,
  arith_eval( X*K, Xk),
  normalize_scalar( Xk, Res).

% ----------------------------- sparse vector stuff ---------------------------- %

add_linear_ff( LinA, Ka, LinB, Kb, LinC) :-
  decompose( LinA, Ha, Ra, Ia),
  decompose( LinB, Hb, Rb, Ib),
  decompose( LinC, Hc, Rc, Ic),
  arith_eval( Ia*Ka+Ib*Kb, Ic),
  arith_eval( Ra*Ka+Rb*Kb, Rc),
  add_linear_ffh( Ha, Ka, Hb, Kb, Hc).

add_linear_ffh( [],	  _,  Ys, Kb, Zs) :- mult_hom( Ys, Kb, Zs).
add_linear_ffh( [X*Kx|Xs], Ka, Ys, Kb, Zs) :-
  add_linear_ffh( Ys, X, Kx, Xs, Zs, Ka, Kb).

  add_linear_ffh( [],	    X, Kx, Xs, Zs, Ka, _) :- mult_hom( [X*Kx|Xs], Ka, Zs).
  add_linear_ffh( [Y*Ky|Ys], X, Kx, Xs, Zs, Ka, Kb) :-
     nf_ordering( X, Y, Rel),
     ( Rel = (=), arith_eval( Kx*Ka+Ky*Kb, Kz),
		( arith_eval(Kz=:=0) ->
		    add_linear_ffh( Xs, Ka, Ys, Kb, Zs)
		;
		    Zs = [X*Kz|Ztail],
		    add_linear_ffh( Xs, Ka, Ys, Kb, Ztail)
		)
     ; Rel = (<), Zs = [X*Kz|Ztail],
		arith_eval( Kx*Ka, Kz),
		add_linear_ffh( Xs, Y, Ky, Ys, Ztail, Kb, Ka)
     ; Rel = (>), Zs = [Y*Kz|Ztail],
		arith_eval( Ky*Kb, Kz),
		add_linear_ffh( Ys, X, Kx, Xs, Ztail, Ka, Kb)
     ).

add_linear_f1( LinA, Ka, LinB, LinC) :-
  decompose( LinA, Ha, Ra, Ia),
  decompose( LinB, Hb, Rb, Ib),
  decompose( LinC, Hc, Rc, Ic),
  arith_eval( Ia*Ka+Ib, Ic),
  arith_eval( Ra*Ka+Rb, Rc),
  add_linear_f1h( Ha, Ka, Hb, Hc).

add_linear_f1h( [],	  _,  Ys, Ys).
add_linear_f1h( [X*Kx|Xs], Ka, Ys, Zs) :-
  add_linear_f1h( Ys, X, Kx, Xs, Zs, Ka).

  add_linear_f1h( [],	    X, Kx, Xs, Zs, Ka) :- mult_hom( [X*Kx|Xs], Ka, Zs).
  add_linear_f1h( [Y*Ky|Ys], X, Kx, Xs, Zs, Ka) :-
     nf_ordering( X, Y, Rel),
     ( Rel = (=), arith_eval( Kx*Ka+Ky, Kz),
		( arith_eval(Kz=:=0) ->
		    add_linear_f1h( Xs, Ka, Ys, Zs)
		;
		    Zs = [X*Kz|Ztail],
		    add_linear_f1h( Xs, Ka, Ys, Ztail)
		)
     ; Rel = (<), Zs = [X*Kz|Ztail],
		arith_eval( Kx*Ka, Kz),
		add_linear_f1h( Xs, Ka, [Y*Ky|Ys], Ztail)
     ; Rel = (>), Zs = [Y*Ky|Ztail],
		add_linear_f1h( Ys, X, Kx, Xs, Ztail, Ka)
     ).

add_linear_11( LinA, LinB, LinC) :-
  decompose( LinA, Ha, Ra, Ia),
  decompose( LinB, Hb, Rb, Ib),
  decompose( LinC, Hc, Rc, Ic),
  arith_eval( Ia+Ib, Ic),
  arith_eval( Ra+Rb, Rc),
  add_linear_11h( Ha, Hb, Hc).

add_linear_11h( [],	  Ys, Ys).
add_linear_11h( [X*Kx|Xs], Ys, Zs) :-
  add_linear_11h( Ys, X, Kx, Xs, Zs).

  add_linear_11h( [],	    X, Kx, Xs, [X*Kx|Xs]).
  add_linear_11h( [Y*Ky|Ys], X, Kx, Xs, Zs) :-
     nf_ordering( X, Y, Rel),
     ( Rel = (=), arith_eval( Kx+Ky, Kz),
		( arith_eval(Kz=:=0) ->
		    add_linear_11h( Xs, Ys, Zs)
		;
		    Zs = [X*Kz|Ztail],
		    add_linear_11h( Xs, Ys, Ztail)
		)
     ; Rel = (<), Zs = [X*Kx|Ztail], add_linear_11h( Xs, Y, Ky, Ys, Ztail)
     ; Rel = (>), Zs = [Y*Ky|Ztail], add_linear_11h( Ys, X, Kx, Xs, Ztail)
     ).

mult_linear_factor( Lin, K, Mult) :-
  arith_eval( K=:=1 ),				% avoid copy
  !,
  Mult = Lin.
mult_linear_factor( Lin, K, Res) :-
  decompose( Lin, Hom, R, I),
  decompose( Res, Mult, Rk, Ik),
  arith_eval( I*K, Ik),
  arith_eval( R*K, Rk),
  mult_hom( Hom, K, Mult).

mult_hom( [],	     _, []).
mult_hom( [A*Fa|As], F, [A*Fan|Afs]) :-
  arith_eval( F*Fa, Fan),
  mult_hom( As, F, Afs).

/*
%
% slightly stabilizes clp(r) numerically
%
mult_hom( [],	     _, []).
mult_hom( [X*Kx|Xs], K, Res) :-
  arith_eval( K*Kx, C),
  ( arith_eval( C=:=0) ->
     mult_hom( Xs, K, Res)
  ;
     Res = [X*C|Tail],
     mult_hom( Xs, K, Tail)
  ).
*/

%
% Replace V in H by its new definition, Vh+Vi
%
nf_substitute( V, LinV, LinX, LinX1) :-
  delete_factor( V, LinX, LinW, K),
  add_linear_f1( LinV, K, LinW, LinX1).


delete_factor( Vid, Lin, Res, Coeff) :-
  decompose( Lin, Hom, R, I),
  decompose( Res, Hdel, R, I),
  delete_factor_hom( Vid, Hom, Hdel, Coeff).
/**/
%
% Makes no use of the nf_ordering and is faster ...
% Depends of course on the price of nf_ordering/3
%
delete_factor_hom( Vid, [Car|Cdr], RCdr, RKoeff) :-
  Car = Var*Koeff,
  compare( R, Var, Vid),
  ( R = (=), RCdr = Cdr, RKoeff=Koeff
  ; R = (<), RCdr = [Car|RCdr1],
	   delete_factor_hom( Vid, Cdr, RCdr1, RKoeff)
  ; R = (>), RCdr = [Car|RCdr1],
	   delete_factor_hom( Vid, Cdr, RCdr1, RKoeff)
  ).
/**/
/**
%
%
%
delete_factor_hom( Vid, [Car|Cdr], RCdr, RKoeff) :-
  Car = Var*Koeff,
  nf_ordering( Vid, Var, Rel),
  ( Rel= (=),
    RCdr = Cdr, RKoeff=Koeff
  ; Rel= (>),
    RCdr = [Car|RCdr1],
    delete_factor_hom( Vid, Cdr, RCdr1, RKoeff)
  ).
**/


% nf_coeff_of( Nf, X, Coeff)
% determine the coeff of variable X in Nf
% fails if X is not a member of the Nf
%
nf_coeff_of( Lin, Vid, Coeff) :-
  decompose( Lin, Hom, _, _),
  get_atts( Vid, order(OVid)),			% pulled out of loop
  nf_coeff_hom( Hom, OVid, Coeff), !.

nf_coeff_hom( [Var*K|Vs], Vid, Coeff) :-
  % nf_ordering( Vid, Var, Rel),
  get_atts( Var, order(OVar)),
  compare( Rel, Vid, OVar),
  ( Rel= (=), Coeff = K
  ; Rel= (>), nf_coeff_hom( Vs, Vid, Coeff)
  ).

nf_rhs_x( Lin, X, Rhs,K) :-
  decompose( Lin, Tail, R, I),
  get_atts( X, order(Ox)),			% pulled out of loop
  nf_coeff_hom( Tail, Ox, K),
  arith_eval( R+I, Rhs).			% late because X may not occur in H

%
% solve for New = Lin1
%
isolate( New, Lin, Lin1) :-
  delete_factor( New, Lin, Lin0, Coeff),
  arith_eval( -1/Coeff, K),
  mult_linear_factor( Lin0, K, Lin1).


indep( Lin, X) :-
  decompose( Lin, [Y*K], _, I),
  X == Y,
  arith_eval( K=:=1),
  arith_eval( I=:=0).

nf2sum( [],	I, I).
nf2sum( [X|Xs], I, Sum) :-
  ( arith_eval(I=:=0) ->
      X = Var*K,
      ( arith_eval( K=:=1) ->
	  hom2sum( Xs, Var, Sum)
      ; arith_eval( K=:= -1) ->
	  hom2sum( Xs, -Var, Sum)
      ;
	  hom2sum( Xs, K*Var, Sum)
      )
  ;
      hom2sum( [X|Xs], I, Sum)
  ).

hom2sum( [],	     Term,  Term).
hom2sum( [Var*K|Cs], Sofar, Term) :-
  ( arith_eval( K=:=1) ->
      Next = Sofar + Var
  ; arith_eval( K=:= -1) ->
      Next = Sofar - Var
  ; arith_eval( K < 0) ->
      arith_eval( -K, Ka),
      Next = Sofar - Ka*Var
  ;
      Next = Sofar + K*Var
  ),
  hom2sum( Cs, Next, Term).


