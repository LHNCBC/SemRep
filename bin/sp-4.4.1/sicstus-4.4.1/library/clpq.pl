%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   clpq.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module( clpq, [
	                {}/1, 
			maximize/1,
			minimize/1, 
			inf/2, inf/4, sup/2, sup/4,
			bb_inf/3,
			bb_inf/5,
			ordering/1,
			entailed/1,
			dump/3, projecting_assert/1
		 ]).

:- multifile
	user:portray/1,
	user:portray_message/2.

%
user:portray( rat(A,B)) :-
	nonvar( A),				% during debugging ...
	nonvar( B),
        !,
	portray_rat( A, B).

% Must write a space before negative numbers if called e.g. in the
% context of writing F=rat(A,B).
%
portray_rat(A, B) :-
	(   A<0, B==1 -> write(' '), write(A)
	;   B==1 -> write(A)
	;   A<0 -> write(' '), write(A/B)
  	;   write(A/B)
  	).

%
% Don't report export of private predicates from clpq
%
%
user:portray_message( warning, import(_,_,clpq,private)) :-
    !.

this_linear_solver( clpq).

:- use_module( 'clpq/arith_q').

:- ensure_loaded( 
	[
	    'clpq/itf3',
	    'clpq/store'			% early because of macros
						% but after itf3
	]).

:- use_module( 'clpq/geler').
:- use_module( 'clpq/nfq').
:- use_module( 'clpq/class').

:- ensure_loaded( 
	[
	    'clpq/project',
	    'clpq/bv',
	    'clpq/ineq',
	    'clpq/redund',
	    'clpq/fourmotz',
	    'clpq/bb',
	    'clpq/dump'
	]).
