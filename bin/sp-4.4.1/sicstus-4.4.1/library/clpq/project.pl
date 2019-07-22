%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   project.pl                                             %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% Answer constraint projection
%

:- public project_attributes/2. 		% xref.pl

%
% interface predicate
%
% May be destructive (either acts on a copy or in a failure loop)
%
project_attributes( TargetVars, Cvas) :-
  sort( TargetVars, Tvs),		% duplicates ?
  sort( Cvas, Avs),			% duplicates ?
  mark_target( Tvs),
  project_nonlin( Tvs, Avs, NlReachable),
  ( Tvs == [] -> 
      drop_lin_atts( Avs)
  ;
      redundancy_vars( Avs),			% redundancy.pl
      make_target_indep( Tvs, Pivots),		
      mark_target( NlReachable),		% after make_indep to express priority
      drop_dep( Avs),
      fm_elim( Avs, Tvs, Pivots),
      impose_ordering( Avs)
  ).

mark_target( []).
mark_target( [V|Vs]) :-
  put_atts( V, target),
  mark_target( Vs).

mark_keep( []).
mark_keep( [V|Vs]) :-
  put_atts( V, keep),
  mark_keep( Vs).

%
% Collect the pivots in reverse order
% We have to protect the target variables pivot partners
% from redundancy eliminations triggered by fm_elim,
% in order to allow for reverse pivoting.
%
make_target_indep( Ts, Ps) :- make_target_indep( Ts, [], Ps).

make_target_indep( [],	   Ps, Ps).
make_target_indep( [T|Ts], Ps0,Pst) :-
  ( get_atts( T, [lin(Lin),type(Type)]),
    decompose( Lin, H, _, _),
    nontarget( H, Nt) ->
       Ps1 = [T:Nt|Ps0],
       put_atts( Nt, keep),
       pivot( T, Nt, Type)
  ;
       Ps1 = Ps0
  ),
  make_target_indep( Ts, Ps1,Pst).

nontarget( [V*_|Vs], Nt) :-
  ( get_atts( V, [-target,-keep_indep]) ->
      Nt = V
  ;
      nontarget( Vs, Nt)
  ).

drop_dep( Vs) :- var( Vs), !.
drop_dep( []).
drop_dep( [V|Vs]) :-
  drop_dep_one( V),
  drop_dep( Vs).

drop_dep_one( V) :-
  get_atts( V, [lin(Lin),type(t_none),-target,-keep,-nonzero]),
  \+ indep( Lin, V),
  !,
  put_atts( V, [-lin(_),-type(_),-class(_),-order(_),-strictness(_)]).
drop_dep_one( _).

drop_lin_atts( []).
drop_lin_atts( [V|Vs]) :-
  put_atts( V, [-lin(_),-type(_),-class(_),-order(_),-strictness(_)]),
  drop_lin_atts( Vs).

impose_ordering( Cvas) :-
  systems( Cvas, [], Sys),
  impose_ordering_sys( Sys).

impose_ordering_sys( []).
impose_ordering_sys( [S|Ss]) :-
  arrangement( S, Arr), 			% ordering.pl
  arrange( Arr, S),
  impose_ordering_sys( Ss).

arrange( [],  _).
arrange( Arr, S) :- Arr = [_|_],
  class_allvars( S, All),
  order( Arr, 1, N),
  order( All, N, _),
  renorm_all( All),
  arrange_pivot( All).

order( Xs, N, M) :- var(Xs), !, N=M.
order( [], N, N).
order( [X|Xs], N, M) :-
  ( get_atts( X, order(O)),
    var(O) ->
      O=N,
      N1 is N+1,
      order( Xs, N1, M)
  ;
      order( Xs, N, M)
  ).

renorm_all( Xs) :- var( Xs), !.
renorm_all( [X|Xs]) :-
  ( get_atts( X, lin(Lin)) ->
      renormalize( Lin, New),
      put_atts( X, lin(New)),
      renorm_all( Xs)
  ;
      renorm_all( Xs)
  ).

arrange_pivot( Xs) :- var( Xs), !.
arrange_pivot( [X|Xs]) :-
  ( get_atts( X, [lin(Lin),type(t_none)]),
    decompose( Lin, [Y*_|_], _, _),
    nf_ordering( Y, X, <) ->
      pivot( X, Y, t_none),
      arrange_pivot( Xs)
  ;
      arrange_pivot( Xs)
  ).

