%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   fourmotz.pl                                            %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% TODO	-) remove syntactic redundancy first ?!!
%	-) avoid the construction of the crossproduct list
%	+) consider strictness in crossproduct generation !!!
%

fm_elim( Vs, Target, Pivots) :-
  prefilter( Vs, Vsf),
  fm_elim_int( Vsf, Target, Pivots).

prefilter( [], []).
prefilter( [V|Vs], Res) :-
  ( get_atts( V, -target),
    occurs( V) ->
      Res = [V|Tail],
      put_atts( V, keep_indep),
      prefilter( Vs, Tail)
  ;
      prefilter( Vs, Res)
  ).

%
% the target variables are marked with an attribute, and we get a list
% of them as an argument too
%
fm_elim_int( [], _,	 Pivots) :-			% done
  unkeep( Pivots).
fm_elim_int( Vs, Target, Pivots) :-
  Vs = [_|_],
  ( best( Vs, Best, Rest) ->
      occurences( Best, Occ),
      elim_min( Best, Occ, Target, Pivots, NewPivots)
  ;						% give up
      NewPivots=Pivots, Rest = []
  ),
  fm_elim_int( Rest, Target, NewPivots).

%
% Find the variable with the smallest netto increase on the
% size of the ineq. system after its elimination
%
best( Vs, Best, Rest) :-
  findall( Delta-N, fm_cp_filter( Vs, Delta, N), Deltas),
  keysort( Deltas, [_-N|_]),
  select_nth( Vs, N, Best, Rest).

fm_cp_filter( Vs, Delta, N) :-
  length( Vs, Len),
  mem( Vs,X,Vst),
  get_atts( X, [-target,lin(Lin)]),
  indep( Lin, X),
  occurences( X, Occ),
  Occ = [_|_],
  % crossproduct( Occ, New, []),
  % length( New, CpLnew),
  cp_card( Occ, 0,Lnew),
  length( Occ, Locc),
  Delta is Lnew-Locc,
  length( Vst, Vstl),
  N is Len-Vstl.

mem( [X|Xs], X, Xs).
mem( [_|Ys], X, Xs) :- mem( Ys, X, Xs).

select_nth( List, N, Nth, Others) :-
  select_nth( List, 1,N, Nth, Others).

select_nth( [X|Xs], N,N, X, Xs) :- !.
select_nth( [Y|Ys], M,N, X, [Y|Xs]) :-
  M1 is M+1,
  select_nth( Ys, M1,N, X, Xs).

%
% fm_detach + reverse_pivot introduce indep t_none, which
% invalidates the invariants
%
elim_min( V, Occ, Target, Pivots, NewPivots) :-
  crossproduct( Occ, New, []),
  activate_crossproduct( New),
    reverse_pivot( Pivots),
    fm_detach( Occ),
    % length( Occ, Locc), length( New, Lnew), print( fm(-Locc,+Lnew)), nl,
    allvars( V, All),
    redundancy_vars( All),			% only for New \== []
    make_target_indep( Target, NewPivots),
    drop_dep( All).

%
% restore NF by reverse pivoting
%
reverse_pivot( []).
reverse_pivot( [I:D|Ps]) :-
  get_atts( D, type(Dt)),
  put_atts( D, -keep),				% no longer
  pivot( D, I, Dt),
  reverse_pivot( Ps).

unkeep( []).
unkeep( [_:D|Ps]) :-
  put_atts( D, -keep),
  drop_dep_one( D),
  unkeep( Ps).


%
% All we drop are bounds
%
fm_detach( []).
fm_detach( [V:_|Vs]) :-
  detach_bounds( V),
  fm_detach( Vs).

%
% Todo: maybe bulk_basis_add
%
activate_crossproduct( []).
activate_crossproduct( [lez(Strict,Lin)|News]) :-
  arith_eval( 0, Z),
  var_with_def_intern( t_u(Z), Var, Lin, Strict),
  basis_add( Var, _),
  activate_crossproduct( News).

% ------------------------------------------------------------------------------

crossproduct( [])     --> [].
crossproduct( [A|As]) -->
  crossproduct( As, A),
  crossproduct( As).

crossproduct( [],	 _)    --> [].
crossproduct( [B:Kb|Bs], A:Ka) -->
  {
    get_atts( A, [type(Ta),lin(LinA),strictness(Sa)]),
    get_atts( B, [type(Tb),lin(LinB),strictness(Sb)]),
    arith_eval( -Kb/Ka, K),
    add_linear_f1( LinA, K, LinB, Lin)
  },
  ( { arith_eval( K > 0) } ->			% signs were opposite
       { Strict is Sa \/ Sb },
       cross_lower( Ta, Tb, K, Lin, Strict),
       cross_upper( Ta, Tb, K, Lin, Strict)
  ;						% La =< A =< Ua -> -Ua =< -A =< -La
       {
	 flip( Ta, Taf),
	 flip_strict( Sa, Saf),
	 Strict is Saf \/ Sb
       },
       cross_lower( Taf, Tb, K, Lin, Strict),
       cross_upper( Taf, Tb, K, Lin, Strict)
  ),
  crossproduct( Bs, A:Ka).

cross_lower( Ta, Tb, K, Lin, Strict) -->
  {
    lower( Ta, La),
    lower( Tb, Lb),
    !,
    arith_eval(K*La+Lb,L),
    normalize_scalar( L, Ln),
    arith_eval( -1, Mone),
    add_linear_f1( Lin, Mone, Ln, Lhs),
    Sl is Strict >> 1				% normalize to upper bound
  },
  [ lez(Sl,Lhs) ].
cross_lower( _, _, _, _, _) --> [].

cross_upper( Ta, Tb, K, Lin, Strict) -->
  {
    upper( Ta, Ua),
    upper( Tb, Ub),
    !,
    arith_eval(-(K*Ua+Ub),U),
    normalize_scalar( U, Un),
    add_linear_11( Un, Lin, Lhs),
    Su is Strict /\ 0b01			% normalize to upper bound
  },
  [ lez(Su,Lhs) ].
cross_upper( _, _, _, _, _) --> [].

lower( t_l(L),	  L).
lower( t_lu(L,_), L).
lower( t_L(L),	  L).
lower( t_Lu(L,_), L).
lower( t_lU(L,_), L).

upper( t_u(U),	  U).
upper( t_lu(_,U), U).
upper( t_U(U),	  U).
upper( t_Lu(_,U), U).
upper( t_lU(_,U), U).

flip( t_l(X),	t_u(X)).
flip( t_u(X),	t_l(X)).
flip( t_lu(X,Y),t_lu(Y,X)).
flip( t_L(X),	t_u(X)).
flip( t_U(X),	t_l(X)).
flip( t_lU(X,Y),t_lu(Y,X)).
flip( t_Lu(X,Y),t_lu(Y,X)).

flip_strict( 0b00, 0b00).
flip_strict( 0b01, 0b10).
flip_strict( 0b10, 0b01).
flip_strict( 0b11, 0b11).

cp_card( [],	 Ci,Ci).
cp_card( [A|As], Ci,Co) :-
  cp_card( As, A, Ci,Cii),
  cp_card( As, Cii,Co).

cp_card( [],	    _,	  Ci,Ci).
cp_card( [B:Kb|Bs], A:Ka, Ci,Co) :-
  get_atts( A, type(Ta)),
  get_atts( B, type(Tb)),
  arith_eval( -Kb/Ka, K),
  ( arith_eval( K > 0)	->			% signs were opposite
       cp_card_lower( Ta, Tb, Ci,Cii),
       cp_card_upper( Ta, Tb, Cii,Ciii)
  ;
       flip( Ta, Taf),
       cp_card_lower( Taf, Tb, Ci,Cii),
       cp_card_upper( Taf, Tb, Cii,Ciii)
  ),
  cp_card( Bs, A:Ka, Ciii,Co).

cp_card_lower( Ta, Tb, Si,So) :-
  lower( Ta, _),
  lower( Tb, _),
  !,
  So is Si+1.
cp_card_lower( _, _, Si,Si).

cp_card_upper( Ta, Tb, Si,So) :-
  upper( Ta, _),
  upper( Tb, _),
  !,
  So is Si+1.
cp_card_upper( _, _, Si,Si).

% ------------------------------------------------------------------------------



occurences( V, Occ) :-
  allvars( V, All),
  occurences( All, V, Occ).

occurences( De,     _, []) :- var( De), !.
occurences( [D|De], V, Occ) :-
  ( get_atts( D, [lin(Lin),type(Type)]),
    occ_type_filter( Type),
    nf_coeff_of( Lin, V, K) ->
      Occ = [D:K|Occt],
      occurences( De, V, Occt)
  ;
      occurences( De, V, Occ)
  ).

occ_type_filter( t_l(_)).
occ_type_filter( t_u(_)).
occ_type_filter( t_lu(_,_)).
occ_type_filter( t_L(_)).
occ_type_filter( t_U(_)).
occ_type_filter( t_lU(_,_)).
occ_type_filter( t_Lu(_,_)).

%
% occurs( V) :-  occurences( V, Occ),  Occ = [_|_].
%
occurs( V) :-
  allvars( V, All),
  occurs( All, V).

occurs( De,	_) :- var( De), !, fail.
occurs( [D|De], V) :-
  ( get_atts( D, [lin(Lin),type(Type)]),
    occ_type_filter( Type),
    nf_coeff_of( Lin, V, _) ->
      true
  ;
      occurs( De, V)
  ).
