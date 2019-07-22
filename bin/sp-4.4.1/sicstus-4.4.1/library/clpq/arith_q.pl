%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  clp(q,r)                                         version 1.3.3 %
%                                                                 %
%  (c) Copyright 1992,1993,1994,1995                              %
%  Austrian Research Institute for Artificial Intelligence (OFAI) %
%  Schottengasse 3                                                %
%  A-1010 Vienna, Austria                                         %
%                                                                 %
%  File:   arith_q.pl                                             %
%  Author: Christian Holzbaur           christian@ai.univie.ac.at %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module( arith_q, 
	[
	    arith_eval/1,	% [MC SP4]
	    arith_eval/2,	% [MC SP4]
	    case_signum/4,	% [MC SP4]

	    arith_eps/1,
	    arith_normalize/2,
	    integerp/1,
	    integerp/2,
						% Q specifics
	    acosq/4,
	    addq/6,
	    asinq/4,
	    atanq/4,
	    ceilingq/4,
	    comq/5,
	    cosq/4,
	    divq/6,
	    divq_11/4,
	    'divq_-11'/4,
	    expq/4,
	    expq/6,
	    floorq/4,
	    getq/3,
	    logq/4,
	    maxq/6,
	    minq/6,
	    mulq/6,
	    putq/3,
	    rat_float/3,
	    roundq/4,
	    signumq/4,
	    sinq/4,
	    subq/6,
	    tanq/4,
	    truncateq/4
	]).

%
% Modules receiving Q expansion
%
arith_module( clpq).
arith_module( nfq).

%
goal_expansion( putq(D,N,Res), _Lay0, Module, (Res = rat(N,D)), []) :- arith_module( Module).

goal_expansion( arith_eval(Term,Res), _Lay0, Module, Module:Expansion, []) :-
	arith_module( Module),
	compile_Qn( Term, Res, Code),
	l2conj( Code, Expansion).

goal_expansion(arith_eval(Rel), _Lay0, Module, Module:Expansion, []) :-
	arith_module( Module),
	compile_Qn( Rel, boolean, Code),
	l2conj( Code, Expansion).

goal_expansion(case_signum(Term,Lt,Z,Gt), _Lay0, Module, Module:Expansion, []) :-
	arith_module( Module),
	compile_case_signum_Qn( Term, Lt,Z,Gt, Code),
	l2conj( Code, Expansion).

goal_expansion(comq(Na,Da,Nb,_,S), _Lay0, arith_q, 0<Nb, []) :-
	Na==0, Da==1, S==(<).

goal_expansion(comq(Na,_,Nb,Db,S), _Lay0, arith_q, Na<0, []) :-
	Nb==0, Db==1, S==(<).

goal_expansion(divq(Na,Da,Nb,Db,Nc,Dc), _Lay0, arith_q, Exp, []) :-
	(   Na==1,Da==1    
	->  Exp = divq_11(Nb,Db,Nc,Dc)
	;   Na==(-1),Da==1
	->  Exp = 'divq_-11'(Nb,Db,Nc,Dc)
	).

% [MC SP4]
arith_eval(Term, Res) :-
	compile_Qn( Term, Res, Code),
	l2conj( Code, Expansion), !,
	call(Expansion).

arith_eval(Rel) :-
	compile_Qn( Rel, boolean, Code),
	l2conj( Code, Expansion), !,
	call(Expansion).

case_signum(Term,Lt,Z,Gt) :-
	compile_case_signum_Qn( Term, Lt,Z,Gt, Code),
	l2conj( Code, Expansion), !,
	call(Expansion).

:- ensure_loaded( arith).

arith_eps( 0).					% for Monash #zero expansion

arith_normalize( Const, Norm) :-
  getq( Const, N, D),
  putq( D, N, Norm).

integerp( rat(_,1)).

integerp( rat(I,1), I).

%---------------------------------------------------------------------------

/*
   1 addq(0, 1, 1, 1, _, _).
  10 comq(0, 1, _, _, <).
   1 comq(0, 1, _, _, _).
  16 comq(_, _, 0, 1, <).
   2 comq(_, _, 0, 1, _).
   1 comq(_, _, 1, 1000, <).
   6 comq(_, _, _, _, <).
   7 divq(-1, 1, _, _, _, _).
   1 divq(0, 1, 1, 1, _, _).
   1 divq(1, 1, 1000, 1, _, _).
   4 divq(1, 1, _, _, _, _).
   2 getq(_, -1, 1).
  13 getq(_, 0, 1).
   6 getq(_, 1, 1).
   1 mulq(0, 1, 1000, 1, _, _).
   1 putq(1, 0, _).
   1 putq(1, 1, _).
   1 putq(1000, 1, _).
*/

