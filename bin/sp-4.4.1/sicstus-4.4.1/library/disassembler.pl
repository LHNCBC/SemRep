/* Copyright(C) 1999, Swedish Institute of Computer Science */

%% This module should be considered undocumented and is subject to change without notice.
%%
:- module(disassembler, [disassemble/0,
                         disassemble/1,
                         disassemble_predicate/2,
                         disassemble_clause/3,
                         disassemble_clause/4,
                         pc_location/4,
                         pc_location/5,
                         pc_location/6]).

:- use_module(library(lists), [suffix/2]).
:- use_module(library(types), [must_be/4]).

:- meta_predicate disassemble(:).
:- meta_predicate disassemble_predicate(:,-).
:- meta_predicate disassemble_clause(:,+,+,-).
:- meta_predicate disassemble_clause(:,+,-).
:- meta_predicate must_be_pred_spec_tree(:,+,+).

%% disassemble(:Predspec).
disassemble(Predspec) :-
    Self = disassemble(Predspec),
    must_be_pred_spec_tree(Predspec, Self, 1),
    prolog:parse_functor_spec(Predspec, [user,follow], Self, MHeads),
    ( suffix(MHeads, [MFA|Remains]),            % BT
      disassemble_predicate(MFA, Disass),
      print_message(informational, Disass),
      Remains = [_|_], % fails for last predicate
      %% empty line between predicates
      print_message(informational, format('',[])),
      fail
    ; true
    ).

disassemble :-
    current_prolog_flag(typein_module, M),
    disassemble(M:_).

disassemble_predicate(MFA, Disass) :-
        prolog:disassemble_predicate(MFA, Disass).

disassemble_clause(MFA, ClauseNoInFile, File, Clause) :-
        prolog:disassemble_clause(MFA, ClauseNoInFile, File, Clause).

disassemble_clause(MFA, ClauseNo, Clause) :-
        prolog:disassemble_clause(MFA, ClauseNo, Clause).

pc_location(PC, MFA, File, ClauseNoInFile) :-
        prolog:pc_location(PC, MFA, File, ClauseNoInFile).

pc_location(PC, MFA, File, ClauseNoInFile, CallNo) :-
        prolog:pc_location(PC, MFA, File, ClauseNoInFile, CallNo).

pc_location(PC, MFA, File, ClauseNoInFile, LineNo, CallNo) :-
        prolog:pc_location(PC, MFA, File, ClauseNoInFile, LineNo, CallNo).

% Use this instead of must_be(Culprit, pred_spec_tree, Goal, ArgNo) when the Culprit is a meta argument in the caller.
must_be_pred_spec_tree(Culprit, Goal, ArgNo) :-
        % Trick SPIDER into not complaining about passing meta argument (Culprit) in non-meta position.
        Culprit1 = Culprit,
        must_be(Culprit1, pred_spec_tree, Goal, ArgNo).


:- multifile user:generate_message_hook/3.

user:generate_message_hook(predicate(_Module1,_Name,_Arity,_Addr1,MFA,_Module2,File,_Options,_PredType,_Addr2,Pred)) --> !,
        ['Predicate: ~q'-[MFA]],
        generate_message_file(File,File),
        [nl],
        generate_message_pred_info(Pred).

generate_message_pred_info('ENTER_COMPACTCODE'(Varcase,Lstcase,Othercase,Clauses)) --> !,
        generate_message_varcase(Varcase),
        generate_message_lstcase(Lstcase),
        generate_message_othercase(Othercase),
	(   foreach(Clause,Clauses)
	do  generate_message_clause(Clause)
	).
generate_message_pred_info('ENTER_COMPACTCODE_INDEXED'(Varcase,Lstcase,Othercase,Clauses)) --> !,
        generate_message_varcase(Varcase),
        generate_message_lstcase(Lstcase),
        generate_message_othercase(Othercase),
	(   foreach(Clause,Clauses)
	do  generate_message_clause(Clause)
	).

generate_message_varcase(varcase([L|Ls])) --> !,
        ['Varcase: ~w'-[[L|Ls]], nl].
generate_message_varcase(varcase(L)) -->
        ['Varcase: ~w'-[[L]], nl].

generate_message_lstcase(lstcase([])) --> !.
generate_message_lstcase(lstcase([L|Ls])) --> !,
        ['Lstcase: ~w'-[[L|Ls]], nl].
generate_message_lstcase(lstcase(L)) -->
        ['Lstcase: ~w'-[[L]], nl].

generate_message_othercase(othercase([])) --> !.
generate_message_othercase(othercase(switch(Hash, Default))) -->
        ['Switch: ~q, default: ~q'-[Hash,Default], nl].

generate_message_clause(clause(Bits,Includer,Included,Instructions)) -->
        ['clause indexed ~w'-[Bits]],
        generate_message_file(Includer, Included),
        [nl],
	(   foreach(L1=Instruction,Instructions)
	do  ['   ~q: ~q'-[L1, Instruction], nl]
	).

generate_message_file([], []) --> !.
generate_message_file(File, File) --> !,
        [' (~w)'-[File]].
generate_message_file(Includer, Included) --> !,
        [' (~w included by ~w)'-[Included,Includer]].

