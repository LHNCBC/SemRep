%% File generated from original SWI file


/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers and Jan Wielemaker
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2003-2004, K.U. Leuven

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/


:- module(chr,[
	chr_trace/0,
	chr_notrace/0,
	chr_leash/1,
	chr_flag/3,
	chr_show_store/1,
	find_chr_constraint/1
	]).

:- op(1180, xfx, ==>).
:- op(1180, xfx, <=>).
:- op(1150, fx, constraints).
:- op(1150, fx, handler).
:- op(1150, fx, rules).
:- op(1100, xfx, \).
:- op(1200, xfx, @).
:- op(1190, xfx, pragma).
:- op( 500, yfx, #).
:- op(1150, fx, chr_type).
:- op(1130, xfx, --->).
:- op(1150, fx, (?)).
:- public
	    % [PM] 4.2.3+ Called from toplev.pl
	    all_stores_goal/2.

%% [PM] 4.2.3+ Was pointlessly dynamic. chr_translated_program is used as blackboard key.
%% :- dynamic   chr_translated_program/1.
:- multifile user:file_search_path/2.
:- mode user:file_search_path(+,-). % [PM] 4.2.3+ suppresses SPIDER warning about missing cut in multifile clause.
user:file_search_path(chr, library(chr)).


:- use_module('chr/chr_translate').
:- use_module('chr/chr_runtime').
:- use_module('chr/chr_hashtable_store').
:- use_module('chr/hprolog').

:- public
       % [PM] 4.2.3+ called from generated code.
       chr:'$chr_module'/1.

:- multifile chr:'$chr_module'/1.

:- dynamic
	chr_term/2.			% File, Term

:- volatile chr_term/2.


%	chr_expandable(+Term)
%	
%	Succeeds if Term is a  rule  that   must  be  handled by the CHR
%	compiler. Ideally CHR definitions should be between
%
%		:- constraints ...
%		...
%		:- end_constraints.
%
%	As they are not we have to   use  some heuristics. We assume any
%	file is a CHR after we've seen :- constraints ... 

chr_expandable((:- constraints _)).
chr_expandable((constraints _)).
chr_expandable((:- chr_constraint _)).
chr_expandable((:- chr_type _)).
chr_expandable((chr_type _)).
chr_expandable(option(_, _)).
chr_expandable((:- chr_option(_, _))).
chr_expandable((handler _)).
chr_expandable((rules _)).
chr_expandable((_ <=> _)).
chr_expandable((_ @ _)).
chr_expandable((_ ==> _)).
chr_expandable((_ pragma _)).

%	chr_expand(+Term, -Expansion)
%	
%	Extract CHR declarations and rules from the file and run the
%	CHR compiler when reaching end-of-file.


% extra_declarations([(:-use_module(chr(chr_runtime)))
% 		     , (:- use_module(chr(hprolog),[chr_term_variables/2,chr_term_variables/3]))
%		     , (:-use_module(chr(hpattvars)))
%		     | Tail], Tail).		   
extra_declarations(List,List).

chr_expand(Term, []) :-
	chr_expandable(Term), !,
	prolog_load_context(source,File),
	assert(chr_term(File, Term)).
chr_expand(end_of_file, FinalProgram) :-
	extra_declarations(FinalProgram,Program),
	findall(T, retract(chr_term(File, T)), CHR0),
	CHR0 \== [],
	prolog_load_context(module, Module),
	add_debug_decl(CHR0, CHR1),
	add_optimise_decl(CHR1, CHR),
	catch(call_chr_translate(File,
			   [ (:- module(Module, []))
			   | CHR
			   ],
			   Program0),
		chr_error(Error),
		(	chr_compiler_errors:print_chr_error(Error),
			fail
		)
	),
	delete_header(Program0, Program).


delete_header([(:- module(_,_))|T0], T) :- !,
	delete_header(T0, T).
delete_header(L, L).

add_debug_decl(CHR, CHR) :-
	member(option(Name, _), CHR), Name == debug, !.
add_debug_decl(CHR, CHR) :-
	member((:- chr_option(Name, _)), CHR), Name == debug, !.
add_debug_decl(CHR, [(:- chr_option(debug, Debug))|CHR]) :-
	(   chr_current_prolog_flag(generate_debug_info, true)
	->  Debug = on
	;   Debug = off
	).


add_optimise_decl(CHR, CHR) :-
	\+(\+(memberchk((:- chr_option(optimize, _)), CHR))), !.
add_optimise_decl(CHR, [(:- chr_option(optimize, full))|CHR]) :-
	chr_current_prolog_flag(optimize, full), !.
add_optimise_decl(CHR, CHR).


%	call_chr_translate(+File, +In, -Out)
%	
%	The entire chr_translate/2 translation may fail, in which case we'd
%	better issue a warning  rather  than   simply  ignoring  the CHR
%	declarations.

call_chr_translate(_, In, _Out) :-
	( chr_translate(In, Out0) ->
	    nb_setval(chr_translated_program,Out0),
	    fail
	).
call_chr_translate(_, _In, Out) :-
	nb_current(chr_translated_program,Out), !,
	nb_delete(chr_translated_program).

call_chr_translate(File, _, []) :-
	print_message(error, chr(compilation_failed(File))).



:- use_module(library(types), [must_be/4]).

:- dynamic
	current_toplevel_show_store/1,
	current_generate_debug_info/1,
	current_optimize/1.

current_toplevel_show_store(on).

current_generate_debug_info(false).

current_optimize(off).

chr_current_prolog_flag(generate_debug_info, X) :-
	chr_flag(generate_debug_info, X, X).
chr_current_prolog_flag(optimize, X) :-
	chr_flag(optimize, X, X).


chr_flag(Flag, Old, New) :-
	Goal = chr_flag(Flag,Old,New),
	must_be(Flag, oneof([toplevel_show_store,generate_debug_info,optimize]), Goal, 1),
	chr_flag(Flag, Old, New, Goal).

chr_flag(toplevel_show_store, Old, New, Goal) :-
	clause(current_toplevel_show_store(Old), true, Ref),
	(   New==Old -> true
	;   must_be(New, oneof([on,off]), Goal, 3),
	    erase(Ref),
	    assertz(current_toplevel_show_store(New))
	).
chr_flag(generate_debug_info, Old, New, Goal) :-
	clause(current_generate_debug_info(Old), true, Ref),
	(   New==Old -> true
	;   must_be(New, oneof([false,true]), Goal, 3),
	    erase(Ref),
	    assertz(current_generate_debug_info(New))
	).
chr_flag(optimize, Old, New, Goal) :-
	clause(current_optimize(Old), true, Ref),
	(   New==Old -> true
	;   must_be(New, oneof([full,off]), Goal, 3),
	    erase(Ref),
	    assertz(current_optimize(New))
	).

all_stores_goal(Goal, _) :-
	chr_flag(toplevel_show_store, on, on), !,
	findall_chr_constraints(Cs),
	andify(Cs, Goal).
all_stores_goal(true, _).

andify([], true).
andify([X|L], Conj) :- andify(L, X, Conj).

andify([], X, X).
andify([Y|L], X, (X,Conj)) :- andify(L, Y, Conj).

:- multifile user:term_expansion/6.

user:term_expansion(In, _, Ids, Out, [], [chr|Ids]) :-
	nonvar(In),
	nonmember(chr, Ids),
	chr_expand(In, Out), !.

