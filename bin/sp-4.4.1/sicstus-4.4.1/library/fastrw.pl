/* Copyright (C) 1993, 2012 Swedish Institute of Computer Science */

%   File       : fastrw.pl
%   Author     : Mats Carlsson
%   Updated    : 3 September 1999
%   Purpose    : Fast term I/O

:- module(fastrw, [
	fast_read/1, fast_read/2,
	fast_write/1, fast_write/2,
	fast_buf_read/2,
	fast_buf_write/3
   ]).

:- use_module(library(types), [
	illarg/3
	]).

fast_write(Term) :-
	current_output(Stream),
	stream_code(Stream, Code),
	do_fast_write(Term, Code, fast_write(Term)).

fast_write(Stream, Term) :-
	stream_code(Stream, Code),
	do_fast_write(Term, Code, fast_write(Stream,Term)).

fast_buf_write(Term, Size, Addr) :-
	open_buf_write(Code),
	do_fast_write(Term, Code, fast_buf_write(Term,Size,Addr)),
	c_buffer_data(Code, Size, Addr).

% [PD] 4.0.2 (from MC)
% do_fast_write(Term, Code, Goal) :-
% 	numbervars(Term, 0, _),
% 	c_fast_write(Term, Code, RC),
% 	fastrw_check(RC, Goal),
% 	fail.
do_fast_write(Term, Code, Goal) :-
	prolog:term_variables_dfs(Term, Vars), % [PM] 4.3 does order matter?
	(   foreach(Var,Vars),
	    count(I,0,_)
	do  prolog:'$put_cva'(Var, [], []),
	    Var = '$VAR'(I)
	),
	c_fast_write(Term, Code, RC),
	fastrw_check(RC, Goal),
	fail.
do_fast_write(_, _, _).

fast_read(Term) :-
	current_input(Stream),
	stream_code(Stream, Code),
	do_fast_read(Term0, Code, fast_read(Term)),
	Term = Term0.

fast_read(Stream, Term) :-
	stream_code(Stream, Code),
	do_fast_read(Term0, Code, fast_read(Stream,Term)),
	Term = Term0.

fast_buf_read(Term, Addr) :-
	open_buf_read(Code, Addr),
	do_fast_read(Term0, Code, fast_buf_read(Term,Addr)),
	Term = Term0.

do_fast_read(Term, Code, Goal) :-
	c_fast_read(Term, Code, RC),
	fastrw_check(RC, Goal).

fastrw_check(0, _).
fastrw_check(-1, Goal) :-
	illarg(existence(byte,0,past_end_of_stream), Goal, 0).
fastrw_check(-2, Goal) :-
	illarg(consistency('expected version','gotten version',0), Goal, 0).
fastrw_check(-3, Goal) :-
	illarg(consistency('wellformed string','actual input',0), Goal, 0).
fastrw_check(-4, Goal) :-
	illarg(system('term refs out of sync'), Goal, 0).

%----------------------------------------------------------------

foreign(plc_fast_write, c_fast_write(+term, +address, [-integer])).
foreign(plc_fast_read, c_fast_read(+term, +address, [-integer])).
foreign(plc_open_buf_write, open_buf_write([-address])).
foreign(plc_open_buf_read, open_buf_read([-address], +integer/*+address*/)).
foreign(plc_buffer_data, c_buffer_data(+address, -integer, -integer/*-address*/)).

foreign_resource(fastrw,
		 [init(frw_init),deinit(frw_deinit),
		  plc_fast_write,plc_fast_read,
		  plc_open_buf_write,plc_open_buf_read,
		  plc_buffer_data]).


:- load_foreign_resource(library(system(fastrw))).
