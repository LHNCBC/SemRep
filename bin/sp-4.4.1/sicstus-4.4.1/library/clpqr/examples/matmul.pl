%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   matmul.pl                                              %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


matmul( [],    _, []).
matmul( [H|T], B, [H1|T1]) :-
  rowmul( B, H, H1),
  matmul( T, B, T1).

rowmul( [],    _,  []).
rowmul( [H|T], AV, [H1|T1]) :-
  vecmul( AV, H, 0, H1),
  rowmul( T, AV, T1).

/*
%
% eager
%
vecmul( [],	 [],	  S, S).
vecmul( [H1|T1], [H2|T2], In, Out) :-
  { Sofar=In+H1*H2 },
  vecmul( T1, T2, Sofar, Out).
*/

%
% lazy
%
vecmul( [],	 [],	  S0, S1) :- { S0=S1 }.
vecmul( [H1|T1], [H2|T2], In, Out) :-
  vecmul( T1, T2, In+H1*H2, Out).

inv_hilbert( N) :-
  hilbert( N, H),
  identity( N, U),
  statistics( runtime, _),
    matmul( H, Inv, U),
  statistics( runtime, [_,Lp_time]),
  format( "% took ~p msec~n", [Lp_time]),
  printnl( Inv).

printnl( []).
printnl( [H|T]) :- print( H), nl, printnl( T).

% ---------- simple matrix generator

mat( I, N, _, []) :- I > N, !.
mat( I, N, Fn, [Row|Rows]) :-
  mat_row( I, 1, N, Fn, Row),
  I1 is I+1,
  mat( I1, N, Fn, Rows).

mat_row( _, J, N, _, []) :- J > N, !.
mat_row( I, J, N, Fn, [Res|Es]) :-
  Call =.. [Fn,I,J,Res],
  call( Call),
  J1 is J+1,
  mat_row( I, J1, N, Fn, Es).

identity( N, Mat) :-
  mat( 1, N, ident, Mat).

ident( I, I, 1) :- !.
ident( _, _, 0).

caneghem( N, Mat) :-
  mat( 1, N, can, Mat).

can( I, J, El) :- can( I, J, 1, 101, El).

can( I, 1, Curr, Mod, El) :- !, El is (I*Curr) mod Mod.
can( I, J, Curr, Mod, El) :-
  J1 is J-1,
  C1 is (Curr*I) mod Mod,
  can( I, J1, C1, Mod, El).

hilbert( N, Mat) :-
  mat( 1, N, hilbert, Mat).

hilbert( I, J, 1/X) :- X is I+J-1.

