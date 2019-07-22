%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.2 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   printf.pl                                              %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% a mockup for clp(R) printf/2
%

:- module( printf, [printf/1,printf/2]).

printf( A) :- printf( A, []).

printf([], _).
/* obsolete as of sicstus3 with escape sequences
printf([0'\,Ch|Fmt],Args) :- !,
  printf_special(Ch),
  printf(Fmt,Args).
*/
printf([0'%|Spec],Args) :- !,
  argspec(Args,Restargs,Spec,Rest),
  printf(Rest,Restargs).
printf([Ch|Fmt],Args) :-
  put_code(Ch),
  printf(Fmt,Args).

printf_special( 0'n) :- !, nl.
printf_special( 0't) :- !, put_code( 9).
printf_special( 0'r) :- !, put_code(13).
printf_special( 0'b) :- !, put_code( 8).
printf_special( 0'f) :- !, put_code(12).
printf_special(   N) :-    put_code(N).

argspec(Ai,Ao) --> format_prefix( Prefix),
		   format_action( Prefix, Ai,Ao).

format_prefix( -Pref) --> "-", !, format_prefix_1(Pref).
format_prefix( +Pref) -->	  format_prefix_1(Pref).

format_prefix_1( two(0,N2)) --> ".", !, format_digits(N2).
format_prefix_1( two(N1,N2)) --> format_digits(N1), ".", !,
				 format_digits(N2).
format_prefix_1( one(N1))    --> format_digits(N1), !.
format_prefix_1( none)	     --> [].

format_digits( N) --> [D],
  {
    D >= "0",
    D =< "9",
    N0 is D-0'0
  },
  format_digits( N0,N).

format_digits( N0,N2) --> [D],
  {
    D >= "0",
    D =< "9",
    !,
    N1 is D-0'0 + N0*10
  },
  format_digits( N1,N2).
format_digits( N0,N0) --> [].

format_action( Prefix, Ai,Ao) --> [F],
  {
    format_action_1( F, Prefix, Ai, Ao), !
  }.
format_action( +none, [A|As], As) -->			% 0'% without further spec.
  {
    ( number(A) ->
       format( "~5g", [A])
    ;
       print(A)
    )
  }.

fmt_norm( rat(N,D), Norm) :- !, ( D=1 -> Norm=N ; Norm is N/D ).
fmt_norm( N,	    N).

format_action_1( 0'e, Prefix, [A|As], As) :- fmt_norm( A, An), action_e( Prefix, An).
format_action_1( 0'f, Prefix, [A|As], As) :- fmt_norm( A, An), action_f( Prefix, An).
format_action_1( 0'g, Prefix, [A|As], As) :- fmt_norm( A, An), action_g( Prefix, An).

format_action_1( 0'%, _,      A,      A)  :-  put_code(0'%).

%
% todo
%
format_action_1( 0'd,  Prefix, [A|As], As) :-
	fmt_norm( A, An),
	Ani is integer(An),
	action_d( Prefix, Ani).
format_action_1( 0'o, _Prefix, [A|As], As) :-  print(A).
format_action_1( 0'x, _Prefix, [A|As], As) :-  print(A).
format_action_1( 0'c, _Prefix, [A|As], As) :-  print(A).
format_action_1( 0'u, _Prefix, [A|As], As) :-  print(A).
format_action_1( 0's, _Prefix, [A|As], As) :-  print(A).

action_d( +one(A), X) :- format( "~|~t~d~*+", [X,A]).
action_d( +none,   X) :- format( "~d", [X]).
action_d( -one(A), X) :- format( "~|~d~t~*+", [X,A]).
action_d( -none,   X) :- format( "~d", [X]).

action_e( +two(A,B), X) :- format( "~|~t~*e~*+", [B,X,A]).
action_e( +one(B),   X) :- format( "~|~t~e~*+", [X,B]).
action_e( +none,     X) :- format( "~e", [X]).
action_e( -two(A,B), X) :- format( "~|~*e~t~*+", [B,X,A]).
action_e( -one(B),   X) :- format( "~|~e~t~*+", [X,B]).
action_e( -none,     X) :- format( "~e", [X]).

action_f( +two(A,B), X) :- format( "~|~t~*f~*+", [B,X,A]).
action_f( +one(B),   X) :- format( "~|~t~f~*+", [X,B]).
action_f( +none,     X) :- format( "~f", [X]).
action_f( -two(A,B), X) :- format( "~|~*f~t~*+", [B,X,A]).
action_f( -one(B),   X) :- format( "~|~f~t~*+", [X,B]).
action_f( -none,     X) :- format( "~f", [X]).

action_g( +two(A,B), X) :- format( "~|~t~*g~*+", [B,X,A]).
action_g( +one(B),   X) :- format( "~|~t~g~*+", [X,B]).
action_g( +none,     X) :- format( "~g", [X]).
action_g( -two(A,B), X) :- format( "~|~*g~t~*+", [B,X,A]).
action_g( -one(B),   X) :- format( "~|~g~t~*+", [X,B]).
action_g( -none,     X) :- format( "~g", [X]).








