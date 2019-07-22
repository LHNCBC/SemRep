%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   squares.pl                                             %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


/*
This beautiful example of disequations at work is due
to [Colmerauer 90]. It addresses the task of tiling a rectangle
with squares of all-different, a priori unknown sizes. Here is a
translation of the original Prolog-III program to clp(q,r)

[Colmerauer 90]
	Colmerauer A.: An Introduction to Prolog III,
	Communications of the ACM, 33(7), 69-90, 1990.




| ?- length(L,9),filled_rectangle(A,L).

A = 33/32,
L = [15/32,9/16,1/4,7/32,1/8,7/16,1/32,5/16,9/32] ? ;

A = 69/61,
L = [33/61,36/61,28/61,5/61,2/61,9/61,25/61,7/61,16/61] ?
*/


rectangle( A, Rs) :-
  Rs = [X1,X2,X3,X4,X5,X6,X7,X8,X9],
  filled_rectangle( A, Rs).

filled_rectangle( A, C) :-
  { A >= 1 },
  distinct_squares( C), 			% also acts as generator [], [_], [_,_], ...
  filled_zone( [-1,A,1], _, C, []).

distinct_squares( []).
distinct_squares( [B|C]) :-
  { B > 0 },
  outof( C, B),
  distinct_squares( C).

outof( [],     _).
outof( [B1|C], B) :-
  { B =\= B1 },
  outof( C, B).

filled_zone( [V|L], [V|L], C0, C0) :-
  { V >= 0 }.
filled_zone( [V|L], L3, [B|C], C2) :-
  { V < 0 },
  placed_square( B, L, L1),
  filled_zone( L1, L2, C, C1),
  { Vb=V+B },
  filled_zone( [Vb,B|L2], L3, C1, C2).

placed_square( B, [H,H0,H1|L], L1) :-
  { B > H, H0=0, H2=H+H1 },
  placed_square( B, [H2|L], L1).
placed_square( B, [B,V|L], [X|L]) :-
  { X=V-B }.
placed_square( B, [H|L], [X,Y|L]) :-
  { B < H, X= -B, Y=H-B }.

%
% first nontrivial solution has 21 squares ...
%
perfect( S) :-
  { A = 1 },
  distinct_squares( S),
  length( S, Len), Len > 1,
  print( try(Len)), nl,
  flush_output,
  time( filled_zone( [-1,A,1], _, S, [])).

/*
try(2)
%%% Timing 00:00:00.030     0.030
try(3)
%%% Timing 00:00:00.070     0.070
try(4)
%%% Timing 00:00:00.270     0.270
try(5)
%%% Timing 00:00:01.060     1.060
try(6)
%%% Timing 00:00:04.470     4.470
try(7)
%%% Timing 00:00:19.960    19.960
try(8)
%%% Timing 00:01:33.380    93.380
try(9)
%%% Timing 00:07:27.380   447.380
try(10)
%%% Timing 00:37:03.770  2223.770
try(11)
%%% Timing 03:11:38.380 11498.380
try(12)
%%% Timing 16:18:43.110 58723.110
*/