/*  $Id$

    Part of CHR (Constraint Handling Rules)

    Author:        Tom Schrijvers
    E-mail:        Tom.Schrijvers@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
    Copyright (C): 2005, K.U. Leuven

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
:- module(chr_compiler_errors,
		[	
			chr_banner/0,
			chr_info/2,
			chr_warning/2,
			chr_error/2,
			print_chr_error/1,
			generate_format_rule/3
		]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% chr_warning(+Type,+FormattedLines)
% chr_info(+Type,+FormattedLines)
% chr_error(+Type,+FormattedLines)


chr_error(Type,Lines) :-
	throw(chr_error(error(Type,Lines))).

print_chr_error(error(Type,Lines)) :-
	print_chr_error(Type,Lines).

%% SICStus begin
chr_banner :-
	print_message(informational, chr_banner).

chr_info(Term,Lines) :-
	print_message(informational, chr(Term,Lines)).

chr_warning(Term,Lines) :-
	print_message(warning, chr(Term,Lines)).

print_chr_error(Term,Lines) :-
	print_message(error, chr(Term,Lines)).


:- multifile user:generate_message_hook/3.

user:generate_message_hook(chr_banner) --> !,
	generate_chr_banner.
user:generate_message_hook(chr(Term)) --> !,
	generate_chr_message(Term, []).
user:generate_message_hook(chr(Term,Lines)) --> !,
	generate_chr_message(Term, Lines).

generate_chr_banner -->
	['\tThe K.U.Leuven CHR System'-[], nl],
	['\t\tContributors:\tTom Schrijvers, Jon Sneyers, Bart Demoen,'-[], nl],
	['\t\t\t\tJan Wielemaker'-[], nl],
	['\t\tCopyright:\tK.U.Leuven, Belgium'-[], nl],
	['\t\tURL:\t\thttp://www.cs.kuleuven.be/~toms/CHR/'-[], nl].
	
generate_chr_message(deprecated(Term), Lines) --> !,
	['CHR: deprecated syntax "~q"'-[Term], nl],
	generate_chr_lines(Lines),
	['     Support for deprecated syntax will be discontinued in the near future!'-[], nl].
generate_chr_message(internal, Lines) --> !,
	['CHR: something unexpected happened in the CHR compiler'-[], nl],
	generate_chr_lines(Lines),
        ['     Your program may not have been compiled correctly!'-[], nl],
        ['     Please contact tom.schrijvers@cs.kuleuven.be'-[], nl].
generate_chr_message(unsupported_pragma(Pragma,Rule), Lines) --> !,
	['CHR: unsupported pragma ~w in '-[Pragma]],
	generate_format_rule(Rule), [nl],
	generate_chr_lines(Lines),
	['     Pragma is ignored!'-[], nl].
generate_chr_message(syntax(Term), Lines) --> !,
	['CHR: invalid syntax "~q"'-[Term], nl],
	generate_chr_lines(Lines).
generate_chr_message(debug_options, _) --> !, [
	'CHR Debugging options:'-[], nl,
	'    RET   creep            c      creep'-[],nl,
	'    s     skip             f      fail'-[],nl,
	'    g     ancestors        n      nodebug'-[],nl,
	'    a     abort            b      break'-[],nl,
	'    EOF   exit             e      exit'-[],nl,
	'    ?     help             h      help'-[],nl,nl].
generate_chr_message(ancestors(History,Depth), Lines) --> !,
	[ 'CHR Ancestors:'-[], nl ],
	ancestors(History, Depth),
	generate_chr_lines(Lines).
generate_chr_message(event(Port,Depth), Lines) -->
	[ 'CHR: '-[] ],
	event(Port, Depth), [nl],
	generate_chr_lines(Lines).
generate_chr_message(Term, Lines) -->
	{prolog_flag(debugger_print_options, Options)},
	['CHR: '-[], write_term(Term,Options), nl],
	generate_chr_lines(Lines).

generate_chr_lines([]) --> [].
generate_chr_lines([Line|Lines]) -->
	c(Line), [nl],
	generate_chr_lines(Lines).

c([]) --> [].
c([X|Xs]) --> [X], c(Xs).

ancestors([], _) --> [].
ancestors([Event|Events], Depth) -->
	['\t'-[]], event(Event, Depth), [nl],
	{NDepth is Depth - 1},
	ancestors(Events, NDepth).

event(Port, Depth) -->
	depth(Depth),
	port(Port).
event(apply(H1,H2,G,B), Depth) -->
	depth(Depth),
	[ 'Apply: '-[] ],
	rule(H1,H2,G,B).
event(try(H1,H2,G,B), Depth) -->
	depth(Depth),
	[ 'Try: '-[] ],
	rule(H1,H2,G,B).
event(insert(#(_,Susp)), Depth) -->
	depth(Depth),
	[ 'Insert: '-[] ],
	head(Susp).

port(call(Susp)) -->
	[ 'Call: '-[] ],
	head(Susp).
port(wake(Susp)) -->
	[ 'Wake: '-[] ],
	head(Susp).
port(exit(Susp)) -->
	[ 'Exit: '-[] ],
	head(Susp).
port(fail(Susp)) -->
	[ 'Fail: '-[] ],
	head(Susp).
port(redo(Susp)) -->
	[ 'Redo: '-[] ],
	head(Susp).
port(remove(Susp)) -->
	[ 'Remove: '-[] ],
	head(Susp).


depth(Depth) -->
	[ '~t(~D)~10| '-[Depth] ].

head(Susp) -->
	{ Susp =.. [_,ID,_,_,_,_|GoalArgs], Goal =.. GoalArgs
	},
	[ '~w # <~w>'-[Goal, ID] ].

heads([H]) --> !,
	head(H).
heads([H|T]) -->
	head(H),
	[ ', '-[] ],
	heads(T).

rule(H1, H2, G, B) -->
	rule_head(H1, H2),
	rule_body(G, B).

rule_head([], H2) --> !,
	heads(H2),
	[ ' ==> '-[] ].
rule_head(H1, []) --> !,
	heads(H1),
	[ ' <=> '-[] ].
rule_head(H1, H2) -->
	heads(H2), [ ' \\ '-[] ], heads(H1), [' <=> '-[]].


rule_body(true, B) --> !,
	[ '~w.'-[B] ].
rule_body(G, B) -->
	[ '~w | ~w.'-[G, B] ].

%% SICStus end

%% SWI begin
chr_banner :-
	chr_info(banner,
		 [['\tThe K.U.Leuven CHR System'-[]],
		  ['\t\tContributors:\tTom Schrijvers, Jon Sneyers, Bart Demoen,'-[]],
		  ['\t\t\t\tJan Wielemaker'-[]],
		  ['\t\tCopyright:\tK.U.Leuven, Belgium'-[]],
		  ['\t\tURL:\t\thttp://www.cs.kuleuven.be/~~toms/CHR/'-[]]]).

chr_info(Term,Lines) :-
	( \+verbosity_on ->
		true
	;
		long_line_with_equality_signs,
		format(user_error,'CHR compiler:\n',[]),	
		chr_print_lines(Lines),
		long_line_with_equality_signs
	).

verbosity_on :- prolog_flag(verbose,V), V == yes.

chr_warning(deprecated(Term),Lines) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: deprecated syntax      ~w.\n',[Term]),	
	format(user_error,'    `--> ',[]),
	chr_print_lines(Lines),
        format(user_error,'    Support for deprecated syntax will be discontinued in the near future!\n',[]),
	long_line_with_equality_signs.

chr_warning(internal,Lines) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: something unexpected happened in the CHR compiler.\n',[]),	
	format(user_error,'    `--> ',[]),
	chr_print_lines(Lines),
        format(user_error,'    Your program may not have been compiled correctly!\n',[]),
        format(user_error,'    Please contact tom.schrijvers@cs.kuleuven.be.\n',[]),
	long_line_with_equality_signs.

chr_warning(unsupported_pragma(Pragma,Rule),Lines) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING: unsupported pragma ~w in ',[Pragma]),
	generate_format_rule(Rule, Fmts, ['.\n'-[]]),
	chr_print_fmts(Fmts),
	format(user_error,'    `--> ',[]),
	chr_print_lines(Lines),
        format(user_error,'    Pragma is ignored!\n',[]),
	long_line_with_equality_signs.

chr_warning(_,Lines) :-
	long_line_with_equality_signs,
	format(user_error,'CHR compiler WARNING:\n',[]),	
	format(user_error,'    `--> ',[]),
	chr_print_lines(Lines),
	long_line_with_equality_signs.

print_chr_error(syntax(Term),Lines) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR: invalid syntax "~w".\n',[Term]),	
	format(user_error,'    `--> ',[]),
	chr_print_lines(Lines),
	long_line_with_equality_signs.
print_chr_error(internal,Lines) :- !,
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR: something unexpected happened in the CHR compiler.\n'),	
	format(user_error,'    `--> ',[]),
	chr_print_lines(Lines),
        format(user_error,'    Please contact tom.schrijvers@cs.kuleuven.be.\n'),
	long_line_with_equality_signs.
print_chr_error(_,Lines) :-
	long_line_with_equality_signs,
	format(user_error,'CHR compiler ERROR:\n',[]),	
	format(user_error,'    `--> ',[]),
	chr_print_lines(Lines),
	long_line_with_equality_signs.

chr_print_lines([]).
chr_print_lines([Line|Lines]) :-
	chr_print_fmts(Line),
	nl(user_error),
	chr_print_lines(Lines).

chr_print_fmts([]).
chr_print_fmts([Fmt-Params|Fmts]) :-
	format(user_error, Fmt, Params),
	chr_print_fmts(Fmts).

long_line_with_equality_signs :-
	format(user_error,'================================================================================\n',[]).
%% SWI end

generate_format_rule(pragma(_,_,_,MaybeName,N)) -->
	(   {MaybeName = yes(Name)} -> ['rule ~w'-[Name]]
	;                              ['rule number ~d'-[N]]
	).
