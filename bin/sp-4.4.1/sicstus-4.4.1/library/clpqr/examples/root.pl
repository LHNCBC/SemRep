%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   root.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% perform N iterations of the sqrt(2) newton approx.
%
root( N, R) :-
  root( N, 1, R).

root( 0, S, R) :- !, S=R.
root( N, S, R) :-
  N1 is N-1,
  { S1 = S/2 + 1/S },
  root( N1, S1, R).

%
% print e with a precision of at least N digits after 2.
%
e( N) :-
  e( N, E),
  print_decimal( E, N).

e( N, E) :-
  { Err =:= exp(10,-(N+2)), Half =:= 1/2 },
  inv_e_series( Half, Half, 3, Err, Inv_E),
  { E =:= 1/Inv_E }.

inv_e_series( Term, S0, _, Err, Sum) :-
  { abs(Term) =< Err },
  !,
  S0 = Sum.
inv_e_series( Term, S0, N, Err, Sum) :-
  N1 is N+1,
  { Term1 =:= -Term/N, S1 =:= Term1+S0 },
  inv_e_series( Term1, S1, N1, Err, Sum).

%
% print Rat with a precision of N places after the decimal point
%
print_decimal( Rat, N) :-
  clpq:arith_eval( truncate( Rat), Int), Int=rat(I,1),
  clpq:arith_eval( Rat-Int, Rest),
  clpq:arith_eval( numer( Rest), rat(Num,1)),
  clpq:arith_eval( denom( Rest), rat(Den,1)),
  format( "~d.", [I]),
  wdig( 0, N, Num, Den),
  nl.

wdig( N, M, _, _) :- N>=M, !.
wdig( _, _, 0, _) :- !. 			% finite decimal expansion
wdig( I, N, A, B) :-
  I1 is I+1,
  D  is (10*A) //  B,
  A1 is (10*A) mod B,
  ( I mod 10 =:= 0 -> write(' ') ; true ),
  ( I mod 70 =:= 0 -> nl, write('  ') ; true),
  write( D),
  wdig( I1, N, A1, B).

%
% Collect n digits of the decimal expansion of a/b
% where a//b = 0
%
dig( 0, _, _, []) :- !.
dig( _, 0, _, []) :- !. 			% finite decimal expansion
dig( I, A, B, [D|Ds]) :-
  I1 is I-1,
  A10 is 10*A,
  D  is A10 //	B,
  A1 is A10 mod B,
  dig( I1, A1, B, Ds).

/*
| ?- time(expo(1000)).
2.
  7182818284 5904523536 0287471352 6624977572 4709369995 9574966967 6277240766
  3035354759 4571382178 5251664274 2746639193 2003059921 8174135966 2904357290
  0334295260 5956307381 3232862794 3490763233 8298807531 9525101901 1573834187
  9307021540 8914993488 4167509244 7614606680 8226480016 8477411853 7423454424
  3710753907 7744992069 5517027618 3860626133 1384583000 7520449338 2656029760
  6737113200 7093287091 2744374704 7230696977 2093101416 9283681902 5515108657
  4637721112 5238978442 5056953696 7707854499 6996794686 4454905987 9316368892
  3009879312 7736178215 4249992295 7635148220 8269895193 6680331825 2886939849
  6465105820 9392398294 8879332036 2509443117 3012381970 6841614039 7019837679
  3206832823 7646480429 5311802328 7825098194 5581530175 6717361332 0698112509
  9618188159 3041690351 5988885193 4580727386 6738589422 8792284998 9208680582
  5749279610 4841984443 6346324496 8487560233 6248270419 7862320900 2160990235
  3043699418 4914631409 3431738143 6405462531 5209618369 0888707016 7683964243
  7814059271 4563549061 3031072085 1038375051 0115747704 1718986106 8739696552
  1267154688 9570350354
%%% Timing 00:01:29.150    89.150
*/




