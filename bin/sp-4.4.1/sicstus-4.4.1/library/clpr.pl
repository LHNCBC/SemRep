%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   clpr.pl                                                %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module( clpr, [
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

%
% Don't report export of private predicates from clpr
%
:- multifile 
	user:portray_message/2.

%
user:portray_message( warning, import(_,_,clpr,private)) :-
    !.

this_linear_solver( clpr).

:- use_module( 'clpr/arith_r').

:- ensure_loaded( 
	[
	    'clpr/itf3',
	    'clpr/store'			% early because of macros
						% but after itf3
	]).

:- use_module( 'clpr/geler').
:- use_module( 'clpr/nfr').
:- use_module( 'clpr/class').

:- ensure_loaded( 
	[
	    'clpr/project',
	    'clpr/bv',
	    'clpr/ineq',
	    'clpr/redund',
	    'clpr/fourmotz',
	    'clpr/bb',
	    'clpr/dump'
	]).
