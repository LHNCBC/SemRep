%% [PM] 4.1.3 obsolete code
end_of_file.

% Copyright (C) 1994, Swedish Institute of Computer Science.

% Provides compile time environment for fcompiling clpq/clpr

:- meta_predicate nfq:geler(?,:).
:- meta_predicate nfr:geler(?,:).
:- meta_predicate clpq:wait_linear(?,?,:).
:- meta_predicate clpr:wait_linear(?,?,:).

%
% Don't report export of private predicates from clpq
%
:- multifile
	user:portray_message/2.

%
user:portray_message( warning, import(_,_,From,private)) :-
	clpqr( From).

clpqr( clpq).
clpqr( clpr).

env_fcompile( Name, Arith) :-
	compile_time_env( Name, Arith, Module),
	fcompile( Module:Name).

compile_time_env(File, Arith, Module) :-
	file_mod(Arith, File, Module),
	load_expansions(Module, Arith).

load_expansions(user, _).
load_expansions(arith_q, _).
load_expansions(arith_r, _).
load_expansions(classq, _) :- [class].		% atts
load_expansions(classr, _) :- [class].		% atts
load_expansions(geler_q, _) :- [geler].		% atts
load_expansions(geler_r, _) :- [geler].		% atts
load_expansions(nfq, Arith) :- 
	nfq:[Arith].				% macros
load_expansions(nfr, Arith) :- 
	nfr:[Arith].				% macros
load_expansions(clpr, Arith) :- 
	clpr:[Arith],				% macros
	clpr:[itf3],				% atts
	clpr:[store].				% macros
load_expansions(clpq, Arith) :- 
	clpq:[Arith],				% macros
	clpq:[itf3],				% atts
	clpq:[store].				% macros

file_mod(arith_q, arith,     arith_q).
file_mod(arith_r, arith,     arith_r).
file_mod(arith_q, arith_q,   arith_q).
file_mod(arith_r, arith_r,   arith_r).
file_mod(arith_q, bb,        clpq).
file_mod(arith_r, bb,        clpr).
file_mod(arith_q, bv,        clpq).
file_mod(arith_r, bv,        clpr).
file_mod(arith_q, class,     classq).
file_mod(arith_r, class,     classr).
file_mod(_,       compenv,   user).
file_mod(arith_q, dump,      clpq).
file_mod(arith_r, dump,      clpr).
file_mod(arith_q, fourmotz,  clpq).
file_mod(arith_r, fourmotz,  clpr).
file_mod(arith_q, geler,     geler_q).
file_mod(arith_r, geler,     geler_r).
file_mod(arith_q, ineq,      clpq).
file_mod(arith_r, ineq,      clpr).
file_mod(arith_q, itf3,      clpq).
file_mod(arith_r, itf3,      clpr).
file_mod(arith_q, nf,        nfq).
file_mod(arith_r, nf,        nfr).
file_mod(arith_q, nfq,       nfq).
file_mod(arith_r, nfr,       nfr).
file_mod(arith_q, ordering,  classq).
file_mod(arith_r, ordering,  classr).
file_mod(arith_q, project,   clpq).
file_mod(arith_r, project,   clpr).
file_mod(arith_q, redund,    clpq).
file_mod(arith_r, redund,    clpr).
file_mod(arith_q, store,     clpq).
file_mod(arith_r, store,     clpr).

