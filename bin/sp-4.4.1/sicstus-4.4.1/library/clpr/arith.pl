%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   arith.pl                                               %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% common code for R,Q, runtime predicates
%
% linearize evaluation, collect vars
%
% Todo: +) limited encoding length option
%	+) 2 stage compilation: a) linearization
%				b) specialization to R or Q
%
%

:- use_module( library(types), [illarg/3]).

l2conj( [],	true).
l2conj( [X|Xs], Conj) :-
  ( Xs = [], Conj = X
  ; Xs = [_|_], Conj = (X,Xc), l2conj( Xs, Xc)
  ).

% ----------------------------------------------------------------------

%
% float/1 coercion is allowed only at the outermost level in Q
%
compile_Q( Term, R, Code) :-
  linearize( Term, Res, Linear),
  specialize_Q( Linear, Code, Ct),
  ( Res = boolean,  Ct = []
  ; Res = float(R), Ct = []
  ; Res = rat(N,D), Ct = [ putq(D,N,R) ]
  ).

%
% assumes normalized params and puts a normalized result
%
compile_Qn( Term, R, Code) :-
  linearize( Term, Res, Linear),
  specialize_Qn( Linear, Code, Ct),
  ( Res = boolean,  Ct = []
  ; Res = float(R), Ct = []
  ; Res = rat(N,D), Ct = [ putq(D,N,R) ]
  ).


compile_case_signum_Qn( Term, Lt,Z,Gt, Code) :-
  linearize( Term, rat(N,_), Linear),
  specialize_Qn( Linear, Code,
      [
	  compare( Rel, N, 0),
	  ( Rel = (<), Lt
	  ; Rel = (=), Z
	  ; Rel = (>), Gt
	  )
      ]).


specialize_Qn( []) --> [].
specialize_Qn( [Op|Ops]) -->
  specialize_Qn( Op),
  specialize_Qn( Ops).
%
specialize_Qn( op_var(rat(N,D),Var))   --> [ Var=rat(N,D) ]. % <--- here is the difference ---
specialize_Qn( op_integer(rat(I,1),I)) --> [].
specialize_Qn( op_rat(rat(N,D),N,D))   --> [].
specialize_Qn( op_float(rat(N,D),X))   --> [], { float_rat( X, N,D) }.
specialize_Qn( apply(R,Func)) -->
  specialize_Q_fn( Func, R).


specialize_Q( []) --> [].
specialize_Q( [Op|Ops]) -->
  specialize_Q( Op),
  specialize_Q( Ops).
%
specialize_Q( op_var(rat(N,D),Var))   --> [ getq(Var,N,D) ].
specialize_Q( op_integer(rat(I,1),I)) --> [].
specialize_Q( op_rat(rat(N,D),N,D))   --> [], { D > 0 }.
specialize_Q( op_float(rat(N,D),X))   --> [], { float_rat( X, N,D) }.
specialize_Q( apply(R,Func)) -->
  specialize_Q_fn( Func, R).

specialize_Q_fn( +rat(N,D),			rat(N,D)) --> [].
specialize_Q_fn( numer(rat(N,_)),		rat(N,1)) --> [].
specialize_Q_fn( denom(rat(_,D)),		rat(D,1)) --> [].
specialize_Q_fn( -rat(N0,D),			rat(N,D)) --> [ N is -N0 ].
specialize_Q_fn( abs(rat(Nx,Dx)),		rat(N,D)) --> [ N is abs(Nx) ], {D=Dx}.
specialize_Q_fn( signum(rat(Nx,Dx)),		rat(N,D)) --> [ signumq( Nx,Dx, N,D) ].
specialize_Q_fn( floor(rat(Nx,Dx)),		rat(N,D)) --> [ floorq( Nx,Dx, N,D) ].
specialize_Q_fn( ceiling(rat(Nx,Dx)),		rat(N,D)) --> [ ceilingq( Nx,Dx, N,D) ].
specialize_Q_fn( truncate(rat(Nx,Dx)),		rat(N,D)) --> [ truncateq( Nx,Dx, N,D) ].
specialize_Q_fn( round(rat(Nx,Dx)),		rat(N,D)) --> [ roundq( Nx,Dx, N,D) ].
specialize_Q_fn( log(rat(Nx,Dx)),		rat(N,D)) --> [ logq( Nx,Dx, N,D) ].
specialize_Q_fn( exp(rat(Nx,Dx)),		rat(N,D)) --> [ expq( Nx,Dx, N,D) ].
specialize_Q_fn( sin(rat(Nx,Dx)),		rat(N,D)) --> [ sinq( Nx,Dx, N,D) ].
specialize_Q_fn( cos(rat(Nx,Dx)),		rat(N,D)) --> [ cosq( Nx,Dx, N,D) ].
specialize_Q_fn( tan(rat(Nx,Dx)),		rat(N,D)) --> [ tanq( Nx,Dx, N,D) ].
specialize_Q_fn( asin(rat(Nx,Dx)),		rat(N,D)) --> [ asinq( Nx,Dx, N,D) ].
specialize_Q_fn( acos(rat(Nx,Dx)),		rat(N,D)) --> [ acosq( Nx,Dx, N,D) ].
specialize_Q_fn( atan(rat(Nx,Dx)),		rat(N,D)) --> [ atanq( Nx,Dx, N,D) ].
specialize_Q_fn( float(rat(Nx,Dx)),		float(F)) --> [ rat_float( Nx,Dx, F) ].
%
specialize_Q_fn( rat(Nx,Dx)+rat(Ny,Dy), 	rat(N,D)) --> [ addq( Nx,Dx, Ny,Dy, N,D) ].
specialize_Q_fn( rat(Nx,Dx)-rat(Ny,Dy), 	rat(N,D)) --> [ subq( Nx,Dx, Ny,Dy, N,D) ].
specialize_Q_fn( rat(Nx,Dx)*rat(Ny,Dy), 	rat(N,D)) --> [ mulq( Nx,Dx, Ny,Dy, N,D) ].
specialize_Q_fn( rat(Nx,Dx)/rat(Ny,Dy), 	rat(N,D)) --> [ divq( Nx,Dx, Ny,Dy, N,D) ].
specialize_Q_fn( exp(rat(Nx,Dx),rat(Ny,Dy)),	rat(N,D)) --> [ expq( Nx,Dx, Ny,Dy, N,D) ].
specialize_Q_fn( min(rat(Nx,Dx),rat(Ny,Dy)),	rat(N,D)) --> [ minq( Nx,Dx, Ny,Dy, N,D) ].
specialize_Q_fn( max(rat(Nx,Dx),rat(Ny,Dy)),	rat(N,D)) --> [ maxq( Nx,Dx, Ny,Dy, N,D) ].
%
specialize_Q_fn( rat(Nx,Dx)  <	rat(Ny,Dy),	boolean)  --> [ comq( Nx,Dx, Ny,Dy, <) ].
specialize_Q_fn( rat(Nx,Dx)  >	rat(Ny,Dy),	boolean)  --> [ comq( Ny,Dy, Nx,Dx, <) ].
specialize_Q_fn( rat(Nx,Dx)  =< rat(Ny,Dy),	boolean)  --> [ comq( Nx,Dx, Ny,Dy, Rel), Rel \== (>) ].
specialize_Q_fn( rat(Nx,Dx)  >= rat(Ny,Dy),	boolean)  --> [ comq( Ny,Dy, Nx,Dx, Rel), Rel \== (>) ].
specialize_Q_fn( rat(Nx,Dx) =\= rat(Ny,Dy),	boolean)  --> [ comq( Nx,Dx, Ny,Dy, Rel), Rel \== (=) ].
specialize_Q_fn( rat(Nx,Dx) =:= rat(Ny,Dy),	boolean)  -->
  %
  % *normalized* rationals
  %
  ( {Nx = Ny} -> [] ; [ Nx = Ny ] ),
  ( {Dx = Dy} -> [] ; [ Dx = Dy ] ).

% ----------------------------------------------------------------------

compile_R( Term, R, Code) :-
  linearize( Term, Res, Linear),
  specialize_R( Linear, Code, Ct),
  ( Res == boolean ->
      Ct = [], R = boolean
  ; float(Res) ->
      Ct = [ R=Res ]
  ;
      Ct = [ R is Res ]
  ).

compile_case_signum_R( Term, Lt,Z,Gt, Code) :-
  eps( Eps, NegEps),
  linearize( Term, Res, Linear),
  specialize_R( Linear, Code,
    [
	Rv is Res,
	( Rv < NegEps -> Lt
	; Rv > Eps    -> Gt
	;		 Z
	)
    ]).

specialize_R( []) --> [].
specialize_R( [Op|Ops]) -->
  specialize_R( Op),
  specialize_R( Ops).
%
specialize_R( op_var(Var,Var)) --> [].
specialize_R( op_integer(R,I)) --> [], { R is float(I) }.
specialize_R( op_rat(R,N,D))   --> [], { rat_float( N,D, R) }.
specialize_R( op_float(F,F))   --> [].
specialize_R( apply(R,Func)) -->
  specialize_R_fn( Func, R).

specialize_R_fn( signum(X),		S) -->
  ( {var(X)} ->
      {Xe=X}
  ;
      [ Xe is X ]
  ),
  {
    eps( Eps, NegEps)
  },
  [
    ( Xe < NegEps -> S = -1.0
    ; Xe > Eps	  -> S =  1.0
    ;		     S =  0.0
    )
  ].

specialize_R_fn( +X,			X) --> [].
specialize_R_fn( -X,			-X) --> [].
specialize_R_fn( abs(X),		abs(X)) --> [].
specialize_R_fn( floor(X),		float(floor(/*float?*/X))) --> [].
specialize_R_fn( ceiling(X),		float(ceiling(/*float?*/X))) --> [].
specialize_R_fn( truncate(X),		float(truncate(/*float?*/X))) --> [].
specialize_R_fn( round(X),		float(round(/*float?*/X))) --> [].
specialize_R_fn( log(X),		log(X))  --> [].
specialize_R_fn( exp(X),		exp(X)) --> [].
specialize_R_fn( sin(X),		sin(X)) --> [].
specialize_R_fn( cos(X),		cos(X)) --> [].
specialize_R_fn( tan(X),		tan(X)) --> [].
specialize_R_fn( asin(X),		asin(X)) --> [].
specialize_R_fn( acos(X),		acos(X)) --> [].
specialize_R_fn( atan(X),		atan(X)) --> [].
specialize_R_fn( float(X),		float(X)) --> [].
%
specialize_R_fn( X+Y,			X+Y) --> [].
specialize_R_fn( X-Y,			X-Y) --> [].
specialize_R_fn( X*Y,			X*Y) --> [].
specialize_R_fn( X/Y,			X/Y) --> [].
specialize_R_fn( exp(X,Y),		exp(X,Y)) --> [].
specialize_R_fn( min(X,Y),		min(X,Y)) --> [].
specialize_R_fn( max(X,Y),		max(X,Y)) --> [].
/**/
%
% An absolute eps is of course not very meaningful.
% An eps scaled by the magnitude of the operands participating
% in the comparison is too expensive to support in Prolog on the
% other hand ...
%
%
%		 -eps	0  +eps
%   ---------------[----|----]----------------
%	     < 0		  > 0
%      <-----------]	     [----------->
%	    =< 0
%      <---------------------]
%				  >= 0
%		   [--------------------->
%
%
specialize_R_fn( X  <  Y, boolean)  -->
  {
    eps( Eps, NegEps)
  },
  ( {X==0} ->
      [ Y > Eps ]
  ; {Y==0} ->
      [ X < NegEps ]
  ;
      [ X-Y < NegEps ]
  ).
specialize_R_fn( X  >  Y, boolean)  --> specialize_R_fn( Y  < X, boolean).
specialize_R_fn( X  =< Y, boolean)  -->
  {
    eps( Eps, _)
  },
  [ X-Y < Eps ].
specialize_R_fn( X  >= Y, boolean)  --> specialize_R_fn( Y =< X, boolean).
specialize_R_fn( X =:= Y, boolean)  -->
  {
    eps( Eps, NegEps)
  },
  ( {X==0} ->
	[ Y >= NegEps, Y =< Eps ]
  ; {Y==0} ->
	[ X >= NegEps, X =< Eps ]
  ;
	[
	  Diff is X-Y,
	  Diff =< Eps,
	  Diff >= NegEps
	]
  ).
specialize_R_fn( X =\= Y, boolean)  -->
  {
    eps( Eps, NegEps)
  },
  [
    Diff is X-Y,
    ( Diff < NegEps -> true ; Diff > Eps )
  ].
/**/

/**
%
% b30427, pp.218
%
specialize_R_fn( X  >  Y, boolean)  --> specialize_R_fn( Y  < X, boolean).
specialize_R_fn( X  <  Y, boolean)  -->
  [ scaled_eps(X,Y,E), Y-X > E ].

specialize_R_fn( X  >= Y, boolean)  --> specialize_R_fn( Y =< X, boolean).
specialize_R_fn( X  =< Y, boolean)  -->
  [ scaled_eps(X,Y,E), X-Y =< E ].	% \+ >

specialize_R_fn( X =:= Y, boolean)  -->
  [ scaled_eps(X,Y,E), abs(X-Y) =< E ].

specialize_R_fn( X =\= Y, boolean)  -->
  [ scaled_eps(X,Y,E), abs(X-Y) > E ].


scaled_eps( X, Y, Eps) :-
  exponent( X, Ex),
  exponent( Y, Ey),
  arith_eps( E),
  Max is max(Ex,Ey),
  ( Max < 0 ->
     Eps is E/(1<<Max)
  ;
     Eps is E*(1<<Max)
  ).

exponent( X, E) :-
  A is abs(X),
  float_rat( A, N, D),
  E is msb(N+1)-msb(D).
**/

% ----------------------------------------------------------------------

linearize( Term, Res, Linear) :-
  linearize( Term, Res, Vs,[], Lin, []),
  keysort( Vs, Vss),
  ( Vss = [],	  Linear = Lin
  ; Vss = [V|Vt], join_vars( Vt, V, Linear, Lin)
  ).

%
% flatten the evaluation, collect variables, shared by Q,R,...
%
linearize( X,	     R, [X-R|Vs],Vs) --> {var(X)}, !,	[ ].
linearize( X,	     R, Vs,Vs) --> {integer(X)}, !,	[ op_integer(R,X) ].
linearize( X,	     R, Vs,Vs) --> {float(X)}, !,	[ op_float(R,X) ].
linearize( rat(N,D), R, Vs,Vs) --> !,			[ op_rat(R,N,D) ].
linearize( Term,     R, V0,V1) -->
  {
    functor( Term, N, A),
    functor( Skeleton, N, A)
  },
  linearize_args( A, Term, Skeleton, V0,V1),		[ apply(R,Skeleton) ].

linearize_args( 0, _, _, Vs,Vs) --> [].
linearize_args( N, T, S, V0,V2) -->
  {
    arg( N, T, Arg),
    arg( N, S, Res),
    N1 is N-1
  },
  linearize( Arg, Res, V0,V1),
  linearize_args( N1, T, S, V1,V2).

join_vars( [],	      Y-Ry) --> [ op_var(Ry,Y) ].
join_vars( [X-Rx|Xs], Y-Ry) -->
  ( {X==Y} ->
      {Rx=Ry},
      join_vars( Xs, Y-Ry)
  ;
      [ op_var(Ry,Y) ],
      join_vars( Xs, X-Rx)
  ).

% ---------------------------------- runtime system ---------------------------

%
% C candidate
%
limit_encoding_length( 0,D, _,	  0,D) :- !.		% msb ...
limit_encoding_length( N,D, Bits, Nl,Dl) :-
  Shift is min(max(msb(abs(N)),msb(D))-Bits,
	       min(msb(abs(N)),msb(D))),
  Shift > 0,
  !,
  Ns is N>>Shift,
  Ds is D>>Shift,
  Gcd is gcd(Ns,Ds),
  Nl is Ns//Gcd,
  Dl is Ds//Gcd.
limit_encoding_length( N,D, _, N,D).


%
% No longer backconvert to integer
%
% putq( 1, N, N) :- !.
putq( D, N, rat(N,D)).

getq( Exp,	N,D) :- var( Exp), !, illarg(var, getq(Exp,N,D), 1).
getq( I,	I,1) :- integer(I), !.
getq( F,	N,D) :- float( F), !, float_rat( F, N,D).
getq( rat(N,D), N,D) :-
  integer( N),
  integer( D),
  D > 0,
  1 =:= gcd(N,D).

%
% actually just a joke to have this stuff in Q ...
%
 expq( N,D, N1,D1) :- rat_float( N,D, X), F is	exp(X), float_rat( F, N1,D1).
 logq( N,D, N1,D1) :- rat_float( N,D, X), F is	log(X), float_rat( F, N1,D1).
 sinq( N,D, N1,D1) :- rat_float( N,D, X), F is	sin(X), float_rat( F, N1,D1).
 cosq( N,D, N1,D1) :- rat_float( N,D, X), F is	cos(X), float_rat( F, N1,D1).
 tanq( N,D, N1,D1) :- rat_float( N,D, X), F is	tan(X), float_rat( F, N1,D1).
asinq( N,D, N1,D1) :- rat_float( N,D, X), F is asin(X), float_rat( F, N1,D1).
acosq( N,D, N1,D1) :- rat_float( N,D, X), F is acos(X), float_rat( F, N1,D1).
atanq( N,D, N1,D1) :- rat_float( N,D, X), F is atan(X), float_rat( F, N1,D1).

%
% for integer powers we can do it in Q
%
expq( Nx,Dx, Ny,Dy, N,D) :-
  ( Dy =:= 1 ->
     ( Ny >= 0 ->
	powq( Ny, Nx,Dx, 1,1, N,D)
     ;
	Nabs is -Ny,
	powq( Nabs, Nx,Dx, 1,1, N1,D1),
	( N1 < 0 ->
	   N is -D1, D is -N1
	;
	   N = D1, D = N1
	)
     )
  ;
     rat_float( Nx,Dx, Fx),
     rat_float( Ny,Dy, Fy),
     F is exp(Fx,Fy),
     float_rat( F, N, D)
  ).

%
% positive integer powers of rational
%
powq( 0, _, _,	Nt,Dt, Nt,Dt) :- !.
powq( 1, Nx,Dx, Nt,Dt, Nr,Dr) :- !, mulq( Nx,Dx, Nt,Dt, Nr,Dr).
powq( N, Nx,Dx, Nt,Dt, Nr,Dr) :-
  N1 is N >> 1,
  ( N /\ 1 =:= 0 ->
      Nt1 = Nt, Dt1 = Dt
  ;
      mulq( Nx,Dx, Nt,Dt, Nt1,Dt1)
  ),
  mulq( Nx,Dx, Nx,Dx, Nxx,Dxx),
  powq( N1, Nxx,Dxx, Nt1,Dt1, Nr,Dr).


/*
%
% the choicepoint ruins the party ...
%
mulq( Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Na,Db),
  ( Gcd1 =:= 1 -> Na1=Na,Db1=Db; Na1 is Na//Gcd1,Db1 is Db//Gcd1 ),
  Gcd2 is gcd(Nb,Da),
  ( Gcd2 =:= 1 -> Nb1=Nb,Da1=Da; Nb1 is Nb//Gcd2,Da1 is Da//Gcd2 ),
  Nc is Na1 * Nb1,
  Dc is Da1 * Db1.
*/
mulq( Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Na,Db),
  Na1 is Na//Gcd1,
  Db1 is Db//Gcd1,
  Gcd2 is gcd(Nb,Da),
  Nb1 is Nb//Gcd2,
  Da1 is Da//Gcd2,
  Nc is Na1 * Nb1,
  Dc is Da1 * Db1.

/*
divq( Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Na,Nb),
  ( Gcd1 =:= 1 -> Na1=Na,Nb1=Nb; Na1 is Na//Gcd1,Nb1 is Nb//Gcd1 ),
  Gcd2 is gcd(Da,Db),
  ( Gcd2 =:= 1 -> Da1=Da,Db1=Db; Da1 is Da//Gcd2,Db1 is Db//Gcd2 ),
  ( Nb1 < 0 ->			% keep denom positive !!!
     Nc is -(Na1 * Db1),
     Dc is Da1 * (-Nb1)
  ;
     Nc is Na1 * Db1,
     Dc is Da1 * Nb1
  ).
*/
divq( Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Na,Nb),
  Na1 is Na//Gcd1,
  Nb1 is Nb//Gcd1,
  Gcd2 is gcd(Da,Db),
  Da1 is Da//Gcd2,
  Db1 is Db//Gcd2,
  ( Nb1 < 0 ->			% keep denom positive !!!
     Nc is -(Na1 * Db1),
     Dc is Da1 * (-Nb1)
  ;
     Nc is Na1 * Db1,
     Dc is Da1 * Nb1
  ).

%
% divq_11( Nb,Db, Nc,Dc) :- divq( 1,1, Nb,Db, Nc,Dc).
%
divq_11( Nb,Db, Nc,Dc) :-
  ( Nb < 0 ->			% keep denom positive !!!
     Nc is -Db,
     Dc is -Nb
  ;
     Nc is Db,
     Dc is Nb
  ).

'divq_-11'( Nb,Db, Nc,Dc) :-
  ( Nb < 0 ->			% keep denom positive !!!
     Nc is Db,
     Dc is -Nb
  ;
     Nc is -Db,
     Dc is Nb
  ).

/*
addq( Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Da,Db),
  ( Gcd1 =:= 1 -> 			% This is the case (for random input) with
					% probability 6/(pi**2).
     Nc is Na*Db + Nb*Da,
     Dc is Da*Db
  ;
     T is Na*(Db//Gcd1) + Nb*(Da//Gcd1),
     Gcd2 is gcd(T,Gcd1),
     Nc is T//Gcd2,
     Dc is (Da//Gcd1) * (Db//Gcd2)
  ).
*/
addq( Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Da,Db),
  T is Na*(Db//Gcd1) + Nb*(Da//Gcd1),
  Gcd2 is gcd(T,Gcd1),
  Nc is T//Gcd2,
  Dc is (Da//Gcd1) * (Db//Gcd2).

/*
subq( Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Da,Db),
  ( Gcd1 =:= 1 -> 			% This is the case (for random input) with
					% probability 6/(pi**2).
     Nc is Na*Db - Nb*Da,
     Dc is Da*Db
  ;
     T is Na*(Db//Gcd1) - Nb*(Da//Gcd1),
     Gcd2 is gcd(T,Gcd1),
     Nc is T//Gcd2,
     Dc is (Da//Gcd1) * (Db//Gcd2)
  ).
*/
subq( Na,Da, Nb,Db, Nc,Dc) :-
  Gcd1 is gcd(Da,Db),
  T is Na*(Db//Gcd1) - Nb*(Da//Gcd1),
  Gcd2 is gcd(T,Gcd1),
  Nc is T//Gcd2,
  Dc is (Da//Gcd1) * (Db//Gcd2).

comq( Na,Da, Nb,Db, S) :-	% todo: avoid multiplication by looking a signs first !!!
  Xa is Na * Db,
  Xb is Nb * Da,
  compare( S, Xa, Xb).

minq( Na,Da, Nb,Db, N,D) :-
  comq( Na,Da, Nb,Db, Rel),
  ( Rel = (=), N=Na, D=Da
  ; Rel = (<), N=Na, D=Da
  ; Rel = (>), N=Nb, D=Db
  ).

maxq( Na,Da, Nb,Db, N,D) :-
  comq( Na,Da, Nb,Db, Rel),
  ( Rel = (=), N=Nb, D=Db
  ; Rel = (<), N=Nb, D=Db
  ; Rel = (>), N=Na, D=Da
  ).

signumq( N,_, S,1) :-
  compare( Rel, N, 0),
  rel2sig( Rel, S).

rel2sig( <, -1).
rel2sig( >,  1).
rel2sig( =,  0).



% -----------------------------------------------------------------------------

truncateq( N,D, R,1) :-
  R is N // D.

%
% returns the greatest integral  value	less  than  or
% equal to x.  This corresponds to IEEE rounding toward nega-
% tive infinity
%
floorq( N,1, N,1) :- !.
floorq( N,D, R,1) :-
  ( N < 0 ->
     R is N // D - 1
  ;
     R is N // D
  ).

%
% returns the least  integral  value  greater  than  or
% equal  to x.	This corresponds to IEEE rounding toward posi-
% tive infinity
%
ceilingq( N,1, N,1) :- !.
ceilingq( N,D, R,1) :-
  ( N > 0 ->
     R is N // D + 1
  ;
     R is N // D
  ).

%
% rounding towards zero
%
roundq( N,D, R,1) :-
  % rat_float( N,D, F), 	% cheating, can do that in Q
  % R is integer(round(F)).
  I is N//D,
  subq( N,D, I,1, Rn,Rd),
  Rna is abs(Rn),
  ( comq( Rna,Rd, 1,2, <) ->
      R = I
  ; I >= 0 ->
      R is I+1
  ;
      R is I-1
  ).

% ------------------------------- rational -> float -------------------------------
%
% The problem here is that SICStus converts BIG fractions N/D into +-nan
% if it does not fit into a float
%
%   | ?- X is msb(integer(1.0e+308)).
%   X = 1023
%

rat_float( Nx,Dx, F) :-
  limit_encoding_length( Nx,Dx, 1023, Nxl,Dxl),
  F is Nxl / Dxl.

% ------------------------------- float -> rational -------------------------------

float_rat( F, N, D) :-
  float_rat( 100, F, F, 1,0,0,1, N0,D0), 	% at most 100 iterations
  ( D0 < 0 ->					% sign normalization
     D is -D0,
     N is -N0
  ;
     D = D0,
     N = N0
  ).

float_rat( 0, _, _, Na,_,Da,_, Na,Da) :- !.
float_rat( _, _, X, Na,_,Da,_, Na,Da) :-
  Da=\=0,			% [MC] SP4
  0.0 =:= abs(X-Na/Da),
  !.
float_rat( N, F, X, Na,Nb,Da,Db, Nar,Dar) :-
  I is integer(F),
  ( I =:= F ->					% guard against zero division
     Nar is Na*I+Nb,				% 1.0 -> 1/1 and not 0/1 (first iter.) !!!
     Dar is Da*I+Db
  ;
     Na1 is Na*I+Nb,
     Da1 is Da*I+Db,
     F1 is 1/(F-I),
     N1 is N-1,
     float_rat( N1, F1, X, Na1,Na,Da1,Da, Nar,Dar)
  ).

