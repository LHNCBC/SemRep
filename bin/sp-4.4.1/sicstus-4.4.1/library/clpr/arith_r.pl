%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   arith_r.pl                                             %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module( arith_r, 
	[
	    arith_eval/1,	% [MC SP4]
	    arith_eval/2,	% [MC SP4]
	    case_signum/4,	% [MC SP4]
	 
	    arith_eps/1,
	    arith_normalize/2,
	    integerp/1,
	    integerp/2
	]).

%
% Modules receiving R expansion
%
arith_module( clpr).
arith_module( nfr).

%
goal_expansion(arith_eval(Term,Res), _Lay0, Module, Module:Expansion, []) :-
	arith_module( Module),
	compile_R( Term, Res, Code),
	l2conj( Code, Expansion).

goal_expansion(arith_eval(Rel), _Lay0, Module, Module:Expansion, []) :-
	arith_module( Module),
	compile_R( Rel, boolean, Code),
	l2conj( Code, Expansion).

goal_expansion(case_signum(Term,Lt,Z,Gt), _Lay0, Module, Module:Expansion, []) :-
	arith_module( Module),
	compile_case_signum_R( Term, Lt,Z,Gt, Code),
	l2conj( Code, Expansion).

% [MC SP4]
arith_eval(Term, Res) :-
	compile_R( Term, Res, Code),
	l2conj( Code, Expansion), !,
	call(Expansion).

arith_eval(Rel) :-
	compile_R( Rel, boolean, Code),
	l2conj( Code, Expansion), !,
	call(Expansion).

case_signum(Term,Lt,Z,Gt) :-
	compile_case_signum_R( Term, Lt,Z,Gt, Code),
	l2conj( Code, Expansion), !,
	call(Expansion).

:- ensure_loaded( arith).

%
% This the only place where this constant lives
%
arith_eps( 1.0e-10).				% for Monash #zero expansion 1.0e-12
eps( 1.0e-10, -1.0e-10).

arith_normalize( X,	   Norm) :- var(X), !, 
  raise_exception( instantiation_error(arith_normalize(X,Norm),1)).
arith_normalize( rat(N,D), Norm) :- rat_float( N,D, Norm).
arith_normalize( X,	   Norm) :- number(X),
  Norm is float(X).

integerp( X) :-
  floor(/*float?*/X)=:=X.

integerp( X, I) :-
  floor(/*float?*/X)=:=X,
  I is integer(X).
