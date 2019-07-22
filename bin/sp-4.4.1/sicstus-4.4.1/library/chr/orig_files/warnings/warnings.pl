%% Attempt to generate every CHR warning ...
%% Bart Demoen
%% Tue Feb  7 09:22:02 CET 2006


:- use_module(library(chr)).

% deprecated: handler/1 declaration
% deprecated: rules/1 declaration
% deprecated: option
% chr_option(debug,on) inconsistent with current_prolog_flag(generate_debug_info,off (not for SICStus in principle)
% constraints --> chr_constraint
% Illegal type definition
% already_in_heads unsupported_pragma
% already_in_head(_) unsupported_pragma

% weird_program,'Heads will never match
% weird_program,'Guard will always fail
% weird_program,'All heads passive in

handler deprecated.

rules also_deprecated.

option(debug,on).

constraints foo/1, bla/0.

:- chr_type(foo = int).

foo(1) <=> true pragma already_in_heads.


foo(2) # Id <=> true pragma already_in_head(Id).


foo(_) <=> true.

bla, foo(_) <=> true.

bla <=> fail | true.


