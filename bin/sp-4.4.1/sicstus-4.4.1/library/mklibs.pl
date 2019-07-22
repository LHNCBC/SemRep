% Build all the library object modules as hidden modules
:- public make/3.

% Return the line number of the first line of the term layout
condense_layout([FL0|_], FL) :- !, FL=FL0.
condense_layout(FL, FL).

:- multifile user:term_expansion/6.
user:term_expansion((:- module(M,E)), Lay0, Ids,
		      (:- module(M,E,[hidden(true)])), Lay1, Ids1) :-
	nonmember(mklibs, Ids), !,
        Ids1 = [mklibs|Ids],
        condense_layout(Lay0, Lay1).

:- dynamic use_error_message_hook/1.
:- volatile use_error_message_hook/1.


:- multifile user:portray_message/2.

user:portray_message(error, Message) :-
	use_error_message_hook(Options), !,
	handle_error_message(Message, Options).

user:portray_message(warning, Message) :-
        use_error_message_hook(Options), !,
	handle_warning_message(Message, Options).


% [PM] 4.1.3 this never had an effect in SP4. (library/1 is hardwired to _first_ look in $SP_LIBRARY_DIR)
% :- dynamic my_library_directory/1.
% :- multifile user:library_directory/1. 
% user:library_directory(DirSpec) :-
%         my_library_directory(DirSpec).
:- meta_predicate my_module(:).
my_module(M:Module) :- Module = M.

make(SymbolicPath, Rest, Options) :-
	prolog_flag(redefine_warnings, _, off),
        asserta(use_error_message_hook(Options)),
        % [PM] 4.1.3+
	% absolute_file_name(., A),
	% asserta(my_library_directory(A)),
        % [PM] 4.2.1 Make module explicit (and avoid SPIDER warning).
        my_module(M),
	compile(M:SymbolicPath),
        Tail = Rest,
        memberchk(output(Output), Options), % [PM] 4.1.3+
	save_files([SymbolicPath|Tail], Output).

%% [PM] 3.8.5
%% Options is a list of
%% on_foreign_resource_error(Opt) --
%%         existence error when loading foreign resource (e.g.,
%%         run-time linker error)
%% on_error(Opt) -- any error
%% Where Opt is
%%  warn -- this is the SICStus standard behaviour, print the error
%%          and go on.
%%  halt -- (the default) call halt(13) to exit with an error
%%          code. Useful to tell make that something went wrong. This
%%          is used for most libraries.
%% on_warning(Opt) -- any warning
%% Where Opt is
%%  warn -- this is the SICStus standard behaviour, print the warning
%%          and go on.
%%  halt -- (the default) call halt(12) to exit with an error
%%          code. Useful to tell make that something went wrong.

handle_error_message(existence_error(load_foreign_resource(_),_,_,_,_), Options) :-
	member(on_foreign_resource_error(warn), Options), !,
	fail.
handle_error_message(_Message, Options) :-
	member(on_error(warn), Options), !,
	fail.
handle_error_message(Message, _Options) :- 
	%% This is the default
	%% member(on_error(halt), _Options), 
	!,
        % [PM] 4.1.3+ ensure our portray_message/2 hooks fails
        retractall(use_error_message_hook(_)), 
        % [PM] 4.1.3+ Was bad and is no longer needed
	% abolish(user:portray_message/2),
	print_message(error, Message),
	halt(13).
handle_error_message(_Message, _Options) :-
	fail.

handle_warning_message(_Message, Options) :-
	member(on_warning(warn), Options), !,
	fail.
handle_warning_message(Message, Options) :-
   \+ \+ member(ignore_warning(Message), Options),
   !.
handle_warning_message(Message, _Options) :- 
	%% This is the default
	%% member(on_warning(halt), _Options), 
	!,
        % [PM] 4.1.3+ ensure our portray_message/2 hooks fails
        retractall(use_error_message_hook(_)), 
        % [PM] 4.1.3+ Was bad and is no longer needed
	% abolish(user:portray_message/2),
	print_message(error, Message),
	halt(12).
handle_warning_message(_Message, _Options) :-
	fail.
