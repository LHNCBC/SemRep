:- use_module(library(system), [
	environ/2
   ]).

:- use_module(library(file_systems), [
	file_exists/1
   ]).

:- use_module(library(lists), [
	append/2
   ]).

% The next two predicates are also defined in skr_lib(sicstus_utils).
concat_atoms([], '').
concat_atoms([H|T], Atom) :-
	concat_atoms_1(T, H, Atom).

concat_atoms_1([], H, H).
concat_atoms_1([Next|Rest], First, Atom) :-
	atom_concat(First, Next, Head),
	concat_atoms_1(Rest, Head, Atom).

% :- ensure_loaded('~/specialist/SKR/src.SICStus/lib/ctypes').
% :- ensure_loaded('~/specialist/SKR/src.SICStus/lib/sicstus_utils').

% :- op(700, fy, bcl).       % BASE CLAUSE LISTING: allows   | ?- bcl <predicate>.
% :- op(700, fy, bcc).       % BASE CLAUSE COUNT:   allows   | ?- bcc <predicate>. 
% :- op(700, fy, ls).        % LISTING:             allows   | ?- l <predicate>. 
% :- op(700, fy, u).         % run a Unix command
:- op(700, xfx, =?).       % testing equality

:- [messages].

:- [utils].

:- [startup].

% :- ensure_loaded(library(emacsdebug)).
% :- window_format(bindings,  _, [quoted(true),portrayed(true),max_depth(0)]).
% :- window_format(source,    _, [quoted(true),portrayed(true),max_depth(0)]).
% :- window_format(ancestors, _, [quoted(true),portrayed(true),max_depth(0)]).
% :- emacs_debugger(_, on).

:- [prolog:'patch14171.pl'].

%:- environ('SKR', SKR),
%   atom_concat(SKR, '/prolog/', SKRPrologUtilsDir),
%   environ('GWAH', GWAH),
%   atom_concat(GWAH, '/specialist/SKR/prolog/', HomePrologUtilsDir),
%   asserta(file_search_path(prolog_utils, SKRPrologUtilsDir)),
%   asserta(file_search_path(prolog_utils, HomePrologUtilsDir)).

% :- ensure_loaded(prolog_utils(compilation)).

:- init_application_environment.
