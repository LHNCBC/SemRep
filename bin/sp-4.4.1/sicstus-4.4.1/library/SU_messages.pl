%%% -*- Mode: Prolog; Module: SU_messages; -*-
/* Copyright (C) 1995, Swedish Institute of Computer Science. */


:- module('SU_messages', []).

% For user extensions.  Undocumented.
:- multifile
	typename/3,
	domainname/3,
	operation/3,
	commandtype/3,
	contexttype/3,
	message/3,
	resource/3,
	errortype/3.


%------------------------------------------------------------------------
%------------------------ Message printing ------------------------------
%------------------------------------------------------------------------


error_msg(system_error(Message)) --> 
	['System error'-[],nl,
	 '~q'-[Message],nl].
error_msg(instantiation_error(Goal,ArgNo)) -->
	['Instantiation error'-[]],
	head(Goal, ArgNo),
	goal(Goal).
error_msg(uninstantiation_error(Goal,ArgNo,Culprit)) -->
	['Uninstantiation error'-[]],
	head(Goal, ArgNo),
        type(var, Culprit),
	goal(Goal).
error_msg(type_error(Goal,ArgNo,TypeName,Culprit)) -->
        { TypeName == variable, ! },
        % [PM] 4.3 This is really an uninstantiated_error (introduced in ISO Cor.2).
        ['Uninstantiation error'-[]],
        head(Goal, ArgNo),
        type(TypeName, Culprit),
        goal(Goal).
error_msg(type_error(Goal,ArgNo,TypeName,Culprit)) -->
	['Type error'-[]],
	head(Goal, ArgNo),
	type(TypeName, Culprit),
	goal(Goal).
error_msg(domain_error(Goal,ArgNo,DomainName,Culprit)) -->
	['Domain error'-[]],
	head(Goal, ArgNo),
	domain(DomainName, Culprit),
	goal(Goal).
error_msg(representation_error(Goal,ArgNo,Message)) -->
	['Representation error'-[]],
	head(Goal, ArgNo),
	message1(Message),
	goal(Goal).
error_msg(existence_error(Goal,ArgNo,ObjType,Culprit,_Message)) -->
	['Existence error'-[]],
	head(Goal, ArgNo),
	typename1(ObjType),
	[' '-[], write_term(Culprit),' does not exist'-[],nl],
	goal(Goal).
error_msg(consistency_error(Goal,Culprit1,Culprit2,Message)) -->
	['Consistency error: '-[],
	 write_term(Culprit1),
	 ' and '-[],
	 write_term(Culprit2),
	 ' are inconsistent'-[],
	 nl],
	message1(Message),
	goal(Goal).
error_msg(context_error(Goal,ContextType,CommandType)) -->
	['Context error: '-[]],
	commandtype1(CommandType),
	[' appeared '-[]],
	contexttype1(ContextType), [nl],
	goal(Goal).
error_msg(resource_error(Goal,Resource)) -->
	['Resource error: insufficient '-[]],
	resource1(Resource), [nl],
	goal(Goal).
error_msg(permission_error(Goal,Operation,ObjType,Culprit,Message)) -->
	['Permission error: cannot '-[]],
	operation1(Operation), [' '-[]],
	typename1(ObjType),
	[' '-[],write_term(Culprit),nl],
	message1(Message),
	goal(Goal).
error_msg(syntax_error(Goal,Line,Msg,Tokens,AfterError)) -->
	['Syntax error'-[]],
	head(Goal, 0),
	message1(Msg),
	['in line ~d'-[Line],nl],
	(   {Tokens = []} -> {true}
	;   {once(length(Tokens, Length)),
	     BeforeError is Length-AfterError},
	    tokens_to_format_commands(BeforeError, Tokens, Tokens1), [nl],
	    ['<<here>>'-[], nl],
	    tokens_to_format_commands(Tokens1), [nl]
	).
error_msg(evaluation_error(Goal,ArgNo,float_overflow,_Culprit)) --> !,
	['Evaluation error'-[]],
	head(Goal, ArgNo),
	['Floating-point overflow'-[],nl].
error_msg(evaluation_error(Goal,ArgNo,float_not_a_number,_Culprit)) --> !,
	['Evaluation error'-[]],
	head(Goal, ArgNo),
	['Not a number'-[],nl].
error_msg(evaluation_error(Goal,ArgNo,ErrorType,Culprit)) -->
	['Evaluation error'-[]],
	head(Goal, ArgNo),
	errortype1(ErrorType),
	[' at '-[],write_term(Culprit),nl].

domain(DomainName, Culprit) -->
	['expected '-[]],
	domainname1(DomainName),
	[', but found '-[],
	 write_term(Culprit),
	 nl].

domainname1(DomainName) -->
	domainname(DomainName), !.
domainname1(DomainName) -->
	['~q'-[DomainName]].

%% [PM] 4.0 I would like to get rid of the raw >(I) etc and replace it
%% with domain(integer, >(I)). This would make integer handling the
%% same as for float and number. The reason I keep the legacy format
%% is that I worry about the various special cases in intrins1.pl (for
%% translation to equivalent ISO exceptions etc)
%%
domainname(>(I)) --> !, domainname(domain(integer,>(I))). % ['an integer greater than ~q'-[I]].
domainname(>=(I)) --> !, domainname(domain(integer,>=(I))). % ['an integer not less than ~q'-[I]].
domainname(<(I)) --> !, domainname(domain(integer,<(I))). % ['an integer less than ~q'-[I]].
domainname(=<(I)) --> !, domainname(domain(integer,=<(I))). % ['an integer not greater than ~q'-[I]].
domainname(=:=(I)) --> !, domainname(domain(integer,=:=(I))). % ['an integer equal to ~q'-[I]].
domainname(=\=(I)) --> !, domainname(domain(integer,=\=(I))). % ['an integer not equal to ~q'-[I]].
domainname(between(I,J)) --> !, {integer(I)}, !,
	['an integer in ~q'-[[I,J]]].
domainname(between(I,J)) --> !, ['a float in ~q'-[[I,J]]].
domainname(domain(ExpType,ExpDomain)) --> !, typename1(ExpType), [' '-[]], rangename(ExpDomain). % [PM] 4.0
domainname(abs_ge(I)) --> !, !,
	['a float with absolute value not less than ~q'-[I]].
domainname(ge(I)) --> !, ['a float not less than ~q'-[I]].
domainname(gt(I)) --> !, ['a float greater than ~q'-[I]].
domainname(noneof(L)) -->
	{foreach(A,L) do atom(A)}, !,
	['an atom not in ~q'-[L]].
domainname(noneof(L)) --> !,
	['an term not in ~q'-[L]].
domainname(oneof(L)) -->
	{foreach(A,L) do atom(A)}, !,
	['an atom in ~q'-[L]].
domainname(oneof(L)) --> !,
	['a term in ~q'-[L]].
domainname(non_empty_list) --> !,
	['a nonempty list'-[]].

rangename(>(I)) --> ['greater than ~q'-[I]].
rangename(>=(I)) --> ['not less than ~q'-[I]].
rangename(<(I)) --> ['less than ~q'-[I]].
rangename(=<(I)) --> ['not greater than ~q'-[I]].
rangename(=:=(I)) --> ['equal to ~q'-[I]].
rangename(=\=(I)) --> ['not equal to ~q'-[I]].
rangename(between(I,J)) --> ['in ~q'-[[I,J]]].

tokens_to_format_commands(Tokens) -->
	(   foreach(Head,Tokens)
	do  token_to_format_commands(Head),
	    [' '-[]]
	).

tokens_to_format_commands(N, S0, S) -->
	(   for(_,1,N),
	    fromto(S0,[Head|Tail],Tail,S)
	do  token_to_format_commands(Head),
	    [' '-[]]
	).

token_to_format_commands(atom(X)) --> !, ['~q'-[X]].
token_to_format_commands(var(_,X,_)) --> !, ['~s'-[X]].
token_to_format_commands(number(X)) --> !, ['~w'-[X]].
token_to_format_commands(string(X)) --> !, {double_quotes(X, Y)}, ['"~s"'-[Y]].
token_to_format_commands(X-_) --> !, token_to_format_commands(X).
token_to_format_commands(Err:X) --> !, ['<~w:~w>'-[Err,X]].
token_to_format_commands(X) --> ['~w'-[X]].

double_quotes([], []).
double_quotes([0'"|Xs], [0'",0'"|Ys]) :- !,  %" 
	double_quotes(Xs, Ys).
double_quotes([X|Xs], [X|Ys]) :-
  	double_quotes(Xs, Ys).


head(Goal, ArgNo) -->
	head(Goal, user, ArgNo).

head(Goal, _, _) --> {var(Goal)}, !, [nl].
head($@(Goal,_), Module, ArgNo) --> !, % [MC] 4.1.2
	head(Goal, Module, ArgNo).
head(0, _, _) --> !, [nl].
head((:-Directive), Module, ArgNo) --> !,
        head(Directive, Module, ArgNo).
head(Module:Goal, _, ArgNo) --> {atom(Module)}, !,
	head(Goal, Module, ArgNo).
head(Goal, Module, ArgNo) --> [X,nl],
	{goal_functor(Goal, Name, Arity)},
        % [PM] 4.2.1 Use explicit module (instead of implied
        % 'SU_messages'). Do not use Module:Goal since
        % predicate_property/2 will fail for builtins if Module is
        % undefined.
	{   predicate_property(user:Goal,built_in)
	->  Spec = Name/Arity
	;   Spec = Module:Name/Arity
	},
	{   integer(ArgNo), 0 < ArgNo, ArgNo =< Arity
	->  X = ' in argument ~d of ~q'-[ArgNo,Spec]
	;   X = ' in ~q'-[Spec]
	}.

% [PM] 4.3 used to recurse down (',')/2 but that only leads to things like call((true,42)) saying "Type error in 42/0...".
% goal_functor((_,Goal), Name, Arity) :- !,
% 	goal_functor(Goal, Name, Arity).
goal_functor(Goal, Name, Arity) :-
	functor(Goal, Name, Arity).

type(TypeName, Culprit) -->
	['expected '-[]],
	typename1(TypeName),
	[', but found '-[],
	 write_term(Culprit),
	 nl].


typename1(TypeName) -->
	typename(TypeName), !.
typename1(TypeName) -->
	['~q'-[TypeName]].


typename(X) --> { var(X), !, fail }.
typename(proper_list) --> !,
	['proper list'-[]].
typename(proper_list(Type)) --> !,
	['proper list of '-[]],
	typename1(Type).
typename(list) --> !,
	['list'-[]].
typename(list(Type)) --> !,
	['list of '-[]],
	typename1(Type).
typename(var_or(Type)) --> !,
	['var or '-[]],
	typename1(Type).
typename(foreign_resource) --> !,
	['foreign_resource declaration for'-[]].
typename(integer) --> !,           % [PM] 4.0 
	['an integer'-[]].
typename(float) --> !,             % [PM] 4.0 
	['a float'-[]].
typename(number) --> !,            % [PM] 4.0 
	['a number'-[]].
typename(var) --> !,
	['a variable'-[]].
typename(past_end_of_stream) --> !,
	['past end of'-[]].

message1(0) --> !.
message1('') --> !.
message1(X) -->
	message(X), !, [nl].
message1(Message) -->
	['~w'-[Message],nl].

message(in_character_code) --> !,
	['~w'-['invalid in-character code']].
message(character_code) --> !,
	['~w'-['invalid character code']].
message(character) --> !,
	['~w'-['invalid character']].
message(max_arity) --> !,
	['~w'-['arity too large']].
message(max_integer) --> !,
	['~w'-['integer too large']].
message(max_atom_length) --> !,
	['~w'-['atom too long']].
message(max_path_length) --> !,
	['~w'-['path name too long']].
message(mis_encoded_string) --> !,
	['~w'-['misencoded string']].
message(nofit(X)) --> !,
	['does not fit in ~w'-[X]].
message(bound(What,N)) --> !,
	['Compiled representation of clause has more than ~d ~a variables'-[N,What]].
message(min_clpfd_integer) --> !, % [MC] SPRM 13682
	['CLPFD integer underflow'-[]].
message(max_clpfd_integer) --> !, % [MC] SPRM 13682
	['CLPFD integer overflow'-[]].

goal(0) --> !.
goal($@(Goal,_)) --> !, % [MC] 4.1.2
	goal(Goal).
goal(Goal) --> ['goal:  '-[],
                write_term(Goal),
                % [PM] 4.3.3 xref do_query/3 (enabling bind_command_variables/1 there would require something like legacy_numbervars here).
                % write_term(Goal,[quoted(true), legacy_numbervars(true)]),
                nl].

beta_msg -->
        ['~w'-['This is a BETA RELEASE!']],
        [nl],
        ['~w'-['It should not be used for critical work, and performance may be degraded.']],
        [nl].

termination_date_msg(YYYYMMDD) -->
        ['This version expires on ~w'-[YYYYMMDD]],
        [nl].

commandtype1(0) --> !.
commandtype1(CommandType) -->
	commandtype(CommandType), !.
commandtype1(CommandType) -->
	['~q'-[CommandType]].

commandtype(clause_or_directive) --> !,
	[' clause or directive'-[]].

contexttype1(0) --> !.
contexttype1(ContextType) -->
	contexttype(ContextType), !.
contexttype1(ContextType) -->
	['in ~q'-[ContextType]].

contexttype(not(file_load)) --> !,
	[' when not loading file(s)'-[]].
contexttype(not(first)) --> !,
	[' not first in file being loaded'-[]].

resource1(0) --> !.
resource1(Resource) -->
	resource(Resource), !.
resource1(Resource) -->
	['~q'-[Resource]].

operation1(0) --> !.
operation1(Operation) -->
	operation(Operation), !.
operation1(Operation) -->
	['~a'-[Operation]].

errortype1(0) --> !.
errortype1(ErrorType) -->
	errortype(ErrorType), !.
errortype1(float_not_a_number) --> !,
	['Not a number'-[]].
errortype1(ErrorType) -->
	['~w'-[ErrorType]].

other_msg(statistics(Props)) -->
        % [PM] 4.3 We should change this so that it does not fail for unknown added statistics.
	{keysort(Props, [agc_count-AgcCount,
			 agc_freed-AgcFreed,
			 agc_nbfreed-AgcNbfreed,
			 agc_time-AgcTime,
			 atoms_nbfree-AtomsNbfree,
			 atoms_nbused-AtomsNbused,
			 atoms_used-AtomsUsed,
			 choice_free-ChoiceFree,
			 choice_used-ChoiceUsed,
			 dcgc_count-DcgcCount,
			 dcgc_time-DcgcTime,
			 defrag_count-DefragCount,
			 defrag_time-DefragTime,
			 dpgc_count-DpgcCount,
			 dpgc_time-DpgcTime,
			 gc_count-GcCount,
			 gc_freed-GcFreed,
			 gc_time-GcTime,
			 global_stack_free-GlobalFree,
			 global_stack_used-GlobalUsed,
                         jit_count-JitCount,
                         jit_time-JitTime,
			 local_stack_free-LocalFree,
			 local_stack_used-LocalUsed,
			 memory_buckets-Buckets,
			 memory_culprit-Culprit,
			 memory_free-MemoryFree,
			 memory_used-MemoryUsed,
			 runtime-[RunTime,_],
			 ss_choice-SSChoice,
			 ss_global-SSGlobal,
			 ss_local-SSLocal,
			 ss_time-SSTime,
			 total_runtime-[TotalTime,_],
			 trail_free-TrailFree,
			 trail_used-TrailUsed,
			 walltime-[WallTime,_]])},
	['memory (total) ~t~d~28| bytes'-[MemoryTotal],nl,
	 '   global stack ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	 [GlobalTotal,GlobalUsed,GlobalFree],nl,
	 '   local stack ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	 [LocalTotal,LocalUsed,LocalFree],nl,
	 '   trail stack ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	 [TrailTotal,TrailUsed,TrailFree],nl,
	 '   control stack ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	 [ChoiceTotal,ChoiceUsed,ChoiceFree],nl,
	 '   program space ~t~d~28| bytes:~t~d~18+ in use, ~t~d~18+ free'-
	 [ProgramTotal,ProgramUsed,ProgramFree],nl,
	 '   program space breakdown:'-[],nl],
	{   foreach(Key-Val,Buckets),
	    foreach(Val-Key,Buckets1),
	    fromto(0,U1,U2,ProgramUsed),
	    fromto(0,B1,B2,TotalBuckets)
	do  B2 is B1+Val,
	    (Key==stack -> U2 = U1 ; U2 is U1+Val)
	},
	{ProgramFree is MemoryUsed + MemoryFree - TotalBuckets},
	{MemoryTotal is MemoryUsed+MemoryFree},
	{GlobalTotal is GlobalUsed+GlobalFree},
	{LocalTotal is LocalUsed+LocalFree},
	{TrailTotal is TrailUsed+TrailFree},
	{ChoiceTotal is ChoiceUsed+ChoiceFree},
	{ProgramTotal is ProgramUsed+ProgramFree},
	{keysort(Buckets1, Buckets2)},
	memory_buckets(Buckets2),
	['    ~d atoms (~d bytes) in use, ~d free'-[AtomsNbused,AtomsUsed,AtomsNbfree],nl],
	(   {Culprit=='(none)'} -> ['    No memory resource errors'-[],nl]
	;                          ['    The most recent memory resource error involved area "~w"'-[Culprit],nl]
	),
	{SSTimeSec is SSTime/1000},
	{GcTimeSec is GcTime/1000},
	{AgcTimeSec is AgcTime/1000},
	{DefragTimeSec is DefragTime/1000},
	{DcgcTimeSec is DcgcTime/1000},
	{DpgcTimeSec is DpgcTime/1000},
	{RunTimeSec is RunTime/1000},
	{TotalTimeSec is TotalTime/1000},
	{WallTimeSec is WallTime/1000},
	[' '-[],nl,
	 '~t~3f~12| sec. for ~d global, ~d local, and ~d choice stack overflows'-
	 [SSTimeSec,SSGlobal,SSLocal,SSChoice],nl,
	 '~t~3f~12| sec. for ~d garbage collections which collected ~d bytes'-
	 [GcTimeSec,GcCount,GcFreed],nl,
	 '~t~3f~12| sec. for ~d atom garbage collections which collected ~d atoms (~d bytes)'-
	 [AgcTimeSec,AgcCount,AgcNbfreed,AgcFreed],nl,
	 '~t~3f~12| sec. for ~d defragmentations'-
	 [DefragTimeSec,DefragCount],nl,
	 '~t~3f~12| sec. for ~d dead clause reclamations'-
	 [DcgcTimeSec,DcgcCount],nl,
	 '~t~3f~12| sec. for ~d dead predicate reclamations'-
	 [DpgcTimeSec,DpgcCount],nl
        ],
        ( { JitCount > 0 } ->
          { JitTimeSec is JitTime/1000.0 },
          [        '~t~3f~12| sec. for JIT-compiling ~d predicates'-
                   [JitTimeSec,JitCount],nl
          ]
        ; []
        ),
        [
	 '~t~3f~12| sec. runtime'-[RunTimeSec],nl,
	 '    ========'-[],nl,
	 '~t~3f~12| sec. total runtime'-[TotalTimeSec],nl,
	 '~t~3f~12| sec. elapsed time'-[WallTimeSec],nl].
other_msg(help) -->
	['This is the default help message.'-[],nl,
	 'You can design your own by defining user:user_help/0.'-[],nl].
other_msg(generated(File,Time)) -->
	['~w generated, ~d msec'-[File,Time],nl].
other_msg(import(Pred,To,From,What)) -->
	['predicate ~q imported by ~w from ~w is ~a'-[Pred,To,From,What],nl].
other_msg(no_match(Doing)) -->
	['~q - no matching predicate'-[Doing],nl].
other_msg(not_module(M)) -->
	['~w is not a current module'-[M],nl].
other_msg(not_module_file(F)) -->
	['~w is not a module-file'-[F],nl].
other_msg(make_library_index(Dir)) -->
	['cannot make index file for ~w'-[Dir],nl].
other_msg(close(Stream)) -->
	['~q was closed while loading'-[Stream],nl].
other_msg(ignore_clause(Type,Pred)) -->
	['ignoring ~a clauses of ~q'-[Type,Pred],nl].
other_msg(load_context(LoadM,CompileM)) -->
	['This file, loaded into module ~w,'-[LoadM],nl,
	 '          is compiled in context of module ~w'-[CompileM],nl].
other_msg(reload(File,OldM,NewM)) -->
	['~w is previously loaded into module ~w'-[File,OldM],nl,
	 '          now it is reloaded into module ~w'-[NewM],nl].
other_msg(singletons(Vas)) -->
        % [PM] 4.3 Vas is a list of atoms nowadays (not '$VAR'(Atom)), so write without quoting.
        % ['~q - singleton variables'-[Vas],nl].
        ['~w - singleton variables'-[Vas],nl].
other_msg(free_variables(Vas)) -->
        % [PM] 4.3 Vas is a list of atoms nowadays (not '$VAR'(Atom)), so write without quoting.
        % ['~q treated as local in do-loop but also used outside'-[Vas],nl],
        % ['suggest renaming or adding ~q'-[param(Vas)],nl].
        ['~w treated as local in do-loop but also used outside'-[Vas],nl],
        ['suggest renaming or adding ~w'-[param(Vas)],nl].

other_msg(singletons(Vas,Pred)) -->
        % from bootstrap compiler, Vas is a list of atoms (even before 4.3)        
        % [PM] 4.3 Vas is a list of atoms (even before 4.3) so write without quoting.
        % ['~q - singleton variables in ~q'-[Vas,Pred],nl].
        ['~w'-[Vas], ' - singleton variables in ~q'-[Pred],nl].
other_msg(plwam_context(From,To,File)) -->
	['Approximate lines: ~d-~d, file: ~w'-[From,To,File],nl].
other_msg(clauses_not_together(Pred)) -->
	['clauses for ~q are not together'-[Pred],nl].
other_msg(redefine) -->
	['Do you really want to redefine it? '-[],nl].
other_msg(not_redefined(Type,Mod)) -->
	['The ~w ~q is NOT being redefined.'-[Type,Mod],nl].
other_msg(redefined(Type,Mod,[],File)) -->
        !,
	['The ~w ~q is being redefined from multifile to non-multifile.'-[Type,Mod],nl,
	 '    New file: ~w'-[File],nl].
other_msg(redefined(Type,Mod,OldFile,File)) -->
	['The ~w ~q is being redefined.'-[Type,Mod],nl,
	 '    Old file: ~w'-[OldFile],nl,
	 '    New file: ~w'-[File],nl].
other_msg(redefine_procedure_help) -->
	['       y    redefine this procedure'-[],nl,
	 '       n    don\'t redefine this procedure'-[],nl,
	 '       p    redefine this procedure and don\'t ask again'-[],nl,
	 '       s    don\'t redefine this procedure and don\'t ask again'-[],nl,
	 '       a    abort'-[],nl,
	 '       b    break'-[],nl,
	 '       ?    print this information'-[],nl,nl].
other_msg(ask_redefine_imported(Pred,ThisMod,ThisMod)) -->
        !,
	['NAME CLASH: ~q is already defined in module ~q'-
	       [Pred,ThisMod],nl].
other_msg(ask_redefine_imported(Pred,ThisMod,OldMod)) -->
	['NAME CLASH: ~q is already imported into module ~q from module ~q'-
	       [Pred,ThisMod,OldMod],nl].
other_msg(redefined_imported(Pred,ThisMod,ThisMod,NewMod)) -->
        !,
	['The predicate ~q, already defined in module ~q, is being imported from module ~q'-
	       [Pred,ThisMod,NewMod],nl].
other_msg(redefined_imported(Pred,ThisMod,OldMod,ThisMod)) -->
        !,
	['The predicate ~q, already imported into module ~q from module ~q, is being redefined'-
	       [Pred,ThisMod,OldMod],nl].
other_msg(redefined_imported(Pred,ThisMod,OldMod,NewMod)) -->
	['The predicate ~q, already imported into module ~q from module ~q, is being imported from module ~q'-
	       [Pred,ThisMod,OldMod,NewMod],nl].
other_msg(not_redefined_imported(Pred,ThisMod,ThisMod)) -->
        !,
	['The predicate ~q, already defined in module ~q, is NOT being redefined'-
	       [Pred,ThisMod],nl].
other_msg(not_redefined_imported(Pred,ThisMod,OldMod)) -->
	['The predicate ~q, already imported into module ~q from module ~q, is NOT being redefined'-
	       [Pred,ThisMod,OldMod],nl].
other_msg(override(NewMod)) -->
	['Do you really want to override this definition with the one in ~w?'-
	       [NewMod],nl,nl].
other_msg(mismatched_conditional(Conditional)) -->
        % [PM] 4.3 The Conditional does not contain '$VAR'(Atom) terms, so no need for legacy numbervars
	['Mismatched conditional ~q'-
	       [Conditional],nl].
other_msg(unterminated_conditionals(Stack)) -->
	['Missing endif directive, nesting level ~d'-
	       [Depth],nl],
        { once(length(Stack, Depth)) }.
other_msg(abort(_)) -->
	['Execution aborted'-[],nl].
other_msg(halt) --> []. % can be intercepted
other_msg(break(I)) -->
	['Break level ~d'-[I],nl].
other_msg(break) -->
	['End break'-[],nl].
other_msg(prompt(G,B,M,S,A,P)) -->
	{prepend_item(M, user, M, [], P4),
	 prepend_item(B, 0, B, P4, P3),
	 prepend_item(P, off, profile, P3, P2),
	 prepend_item(S, off, source_info, P2, P1),
	 prepend_item(A, off, advice, P1, P0),
	 prepend_item(G, off, G, P0, Prompt)},
	(   {Prompt = []} -> []
	;   ['~q'-[Prompt],nl]
	).
other_msg(malformed_clause(Clause)) -->
        % [PM] 4.3 The Clause may have legacy '$VAR'(Atom) sub-terms
        % ['~q - illegal clause'-[Clause],nl].
        [write_term(Clause, [quoted(true),
                             % numbervars(true), numbervars_if_name_(legacy)
                             legacy_numbervars(true)
                            ]),
         ' - illegal clause'-[],nl].
other_msg(declaration(Decl,What)) -->
        % [PM] 4.3 The Decl may have legacy '$VAR'(Atom) sub-terms
        % ['~q - declaration ~a'-[Decl,What],nl].
        [write_term(Decl, [quoted(true),
                           % numbervars(true), numbervars_if_name_(legacy)
                           legacy_numbervars(true)
                          ]),
         ' - declaration ~a'-[What],nl].
other_msg(invalid_expression(Type,Expr)) -->
        % [PM] 4.3 The Expr does not contain '$VAR'(Atom) terms, so no need for legacy numbervars
	['invalid ~a in arithmetic expression: ~q'-[Type,Expr],nl].
other_msg(loading(_Present,user)) --> [].
other_msg(loading(Depth,Present,AbsoluteFileName)) -->
	['~*c~a ~w...'-[Depth,0' ,Present,AbsoluteFileName],nl].
other_msg(loaded(Depth,Past,AbsoluteFileName,'$none',Msec,Bytes)) -->
        !,
 	['~*c~w ~a, ~d msec ~d bytes'-[Depth,0' ,AbsoluteFileName,Past,Msec,Bytes],nl].
other_msg(loaded(Depth,Past,AbsoluteFileName,Module,Msec,Bytes)) -->
	['~*c~w ~a in module ~w, ~d msec ~d bytes'-
	       [Depth,0' ,Past,AbsoluteFileName,Module,Msec,Bytes],nl].
other_msg(imported(Depth,Exporter,Importer)) -->
	['~*cmodule ~w imported into ~w'-[Depth,0' ,Exporter,Importer],nl].
other_msg(foreign_resource(Depth,Past,Resource,Importer)) -->
        (   { Past == loaded } -> [] % [PM] 3.9.1 'loaded' is silent in preference to new 'loading' msg
        ;   ['~*c~w foreign resource ~w in module ~w'-[Depth,0' ,Past,Resource,Importer],nl]
        ).
other_msg(created(AbsoluteFileName,T)) -->
	['~w created in ~d msec'-[AbsoluteFileName,T],nl].
other_msg(not_created(AbsoluteFileName)) -->
	['~w NOT created'-[AbsoluteFileName],nl].
other_msg(restored(AbsoluteFileName,T,S)) -->
	['~w restored in ~d msec ~d bytes'-[AbsoluteFileName,T,S],nl].
other_msg(not_loaded(Pred,What)) -->
	['~q - NOT ~a'-[Pred,What],nl].
other_msg(failed(Goal)) -->
        % [PM] 4.3 The Goal may have legacy '$VAR'(Atom) sub-terms
        % ['~q - goal failed'-[Goal],nl].
        [write_term(Goal,[quoted(true),
                          % numbervars(true), numbervars_if_name_(legacy)
                          legacy_numbervars(true)
                         ]),
         ' - goal failed'-[],nl].
other_msg(wrong_option) --> % not used
	['Option not applicable at this port'-[],nl].
other_msg(wrong_inv_no(Command)) -->
        % [PM] 4.3 Not used?
	['~q - wrong invocation number'-[Command],nl].
other_msg(wrong_command(Command, Port)) -->
        % [PM] 4.3 The Command does not contain '$VAR'(Atom) terms, so no need for legacy numbervars
	['~q - wrong command at ~q port'-[Command,Port],nl].
other_msg(breakpoints([])) -->
        !,
	['There are no breakpoints'-[],nl].
other_msg(breakpoints([X|Xs])) -->
	['Breakpoints:'-[],nl],
        % [PM] 4.3 The Conds will contain plain '$VAR'(Integer) terms, so no need for legacy numbervars
	(   foreach(BPoint,[X|Xs])
	do  {breakpoint_info(BPoint, BID, Mark, St, Func, Conds)},
	    ['    ~t~d~7+ ~a~a ~q'-[BID,Mark,St,Func]],
	    (   {Conds = []} -> []
	    ;   [' if ~q'-[Conds]]
	    ),
	    [nl]
	).
other_msg(breakp(no,BreakPointText,Ref)) -->
        !,
	['There is no ~a ~q'-[BreakPointText,Ref],nl].
other_msg(breakp(bp(_Type,plain(MFunc),BID),add,already)) -->
        !,
	['There is already a plain spypoint on ~w, (BID=~d)'-[MFunc,BID],nl].
other_msg(breakp(bp(Type,WFunc,BID),add,ok)) -->
        !,
	{breakpoint_type_text(Type, TypeTxt)},
	breakpoint_functor_text(WFunc, TypeTxt),
	[' added, BID=~d'-[BID],nl].
other_msg(breakp(bp(Type,WFunc,BID),What,How)) -->
        !,
	{breakpoint_type_text(Type, TypeTxt)},
	breakpoint_functor_text(WFunc, TypeTxt),
	{breakpoint_already_last_texts(How, Already, Last)},
	[', BID=~d, ~a~ad~a'-[BID,Already,What,Last],nl].
other_msg(breakp(all,Type,What)) --> 
	{breakpoint_type_text(Type, TypeTxt)},
	['All ~as ~ad'-[TypeTxt,What],nl].
other_msg(breakp(bp(_Type,WFunc,_BID),compiled_inline)) -->
	{arg(1, WFunc, MFunc)},
	['Predicate ~q compiled inline, breakable only in interpreted code'-
	       [MFunc],nl].
other_msg(trace(Goal)) -->
	write_item(Goal), [nl].
other_msg(trace_help) -->
	['Please enter a valid trace command (''h'' for help).'-[],nl].
other_msg(trace_command) -->
	['Invalid trace command'-[],nl].
other_msg(trace_command(TC)) -->
	['Incorrect trace command ~a'-[TC],nl].
other_msg(ancestors([])) -->
        !,
	['There are no ancestors'-[],nl].
other_msg(ancestors([X|Xs])) -->
	['Ancestors:'-[],nl],
        % [PM] 4.3 Does not contain '$VAR'(Atom) terms, so no need for legacy numbervars
	list_items([X|Xs]).
other_msg(backtrace([])) -->
        !,
	['There is no backtrace'-[],nl].
other_msg(backtrace([X|Xs])) -->
	['Backtrace:'-[],nl],
        % [PM] 4.3 Does not contain '$VAR'(Atom) terms, so no need for legacy numbervars
	list_items([X|Xs]).
other_msg(bgoal(G)) -->
        % [PM] 4.3 Unused?
	['Blocked goal:'-[],nl],
	list_items([bgoal(G)]).
other_msg(blocked([])) -->
        !,
	['There are no blocked goals'-[],nl].
other_msg(blocked([X|Xs])) -->
	['Blocked goals:'-[],nl],
        % [PM] 4.3 Does not contain '$VAR'(Atom) terms, so no need for legacy numbervars
	list_items([X|Xs]).
other_msg(bindings(L)) -->
	{prolog_flag(source_info, SrcFlag)},
        % [PM] 4.3 Does not contain '$VAR'(Atom) terms, so no need for legacy numbervars
	print_bindings(SrcFlag, L).
other_msg(debugging_options) -->
	debugging_options.
other_msg(inst_cond_spy(Module, Functor, Name)) -->
	['Placing spypoint on ~q with conditions: '-[Module:Functor/Name],nl].
other_msg(whereis(file(File),Pred)) -->
        !,
	['~q is defined in the file ~w'-[Pred,File],nl].
other_msg(whereis(built_in,Pred)) -->
        !,
	['~q is a built-in predicate'-[Pred],nl].
other_msg(whereis(dynamic,Pred)) -->
        !,
	['~q has been defined dynamically'-[Pred],nl].
other_msg(whereis(undefined,Pred)) -->
	['~q is undefined'-[Pred],nl].
other_msg(undef(Pred)) -->
	['The predicate ~q is undefined'-[Pred],nl].
other_msg(leash([])) -->
        !,
	['No leashing'-[],nl].
other_msg(leash([X|Xs])) -->
	['Using leashing stopping at ~w ports'-[[X|Xs]],nl].
other_msg(unknown(trace)) -->
        !,
	['Undefined predicates will trap to the debugger (trace)'-[],nl].
other_msg(unknown(fail)) -->
        !,
	['Undefined predicates will just fail (fail)'-[],nl].
other_msg(unknown(error)) -->
	['Undefined predicates will raise an exception (error)'-[],nl].
other_msg(debug(debug)) -->
        !,
	['The debugger will first leap -- showing spypoints (debug)'-[],nl].
other_msg(debug(trace)) -->
        !,
	['The debugger will first creep -- showing everything (trace)'-[],nl].
other_msg(debug(off)) -->
        !,
	['The debugger is switched off'-[],nl].
other_msg(debug(zip)) -->
	['The debugger will first zip -- showing spypoints (zip)'-[],nl].
other_msg(auto_bindings(What)) -->
	['Prolog bindings are switched ~w'-[What],nl].
other_msg(ignored(Term)) -->
	['Ignoring ~w'-[Term],nl].
other_msg(no_license_file) -->
	['License file not found'-[],nl].
other_msg(no_site_fact) -->
	['No site fact in the license file'-[],nl].
other_msg(no_product_fact(Product)) -->
	['You have no license for: ~w'-[Product],nl].
other_msg(bad_license_code(Fact)) -->
	['The code for the license is incorrect: ~q'-[Fact],nl].
other_msg(expired_license(Fact)) -->
	['The license has expired: ~q'-[Fact],nl].
other_msg(version(Version,Site)) -->
	['~w'-[Version],nl,
	 'Licensed to ~w'-[Site],nl].
other_msg(version(Version)) -->
	['~w'-[Version],nl].
other_msg(version_addon(Version)) -->
        ['~w'-[Version],nl].
other_msg(sicstus(_Major,_Minor,_Revision,Beta,_Extra)) -->
        { Beta > 0 },
        !,
        beta_msg.
other_msg(sicstus(_Major,_Minor,_Revision,_Beta,_Extra)) -->
        [].
other_msg(termination_date(YYYYMMDD)) -->
        { YYYYMMDD > 0 },
        !,
        termination_date_msg(YYYYMMDD).
other_msg(termination_date(_YYYYMMDD)) -->
        [].
other_msg(blame_on(Ancestor)) -->
	goal(Ancestor).
%% ^C handling
other_msg(interrupt_options) --> [
	nl,
	'Prolog interrupt options:'-[],nl,
	'    a        abort           - cause abort'-[],nl,
	'    b        break           - cause break'-[],nl,
	'    c        continue        - do nothing'-[],nl,
	'    e        exit            - cause exit'-[],nl,
	'    d        debug           - start leaping'-[],nl,
	'    z        zip             - start zipping'-[],nl,
	'    t        trace           - start creeping'-[],nl,
	'    h        help            - get this list'-[],nl,nl].
other_msg(interruption) -->
	[nl,'Prolog interruption (h for help)? '-[],nl].
%% Emacs interface & source positions
other_msg(emacs_command(Command,Args)) -->
	display_emacs_command(Command,Args).
other_msg(source_pos(SrcInfo,Port,Cmd)) -->
	show_source_pos(SrcInfo, Port, Cmd).
other_msg(no_source_pos) -->
	no_source_pos.
%% Top-level
other_msg(solutions(Bs, VariableNames)) -->
        % [PM] 4.3 Move the common VariableNames into the bindings elements.
        {
          (  foreach(B, Bs),
             foreach(B1, Bs1),
             param([VariableNames])
        do
           ( nonvar(B), B=binding(Tag,Val,Sel) ->
             B1 = binding(Tag,Val,Sel,VariableNames)
           ; nonvar(B), B=constraint(G,Sel) ->
             B1 = constraint(G,Sel,VariableNames)
           ; B1 = B
           )
          )        
        },
        other_msg(solutions(Bs1)).

other_msg(solutions([])) -->
        !,
	[nl, 'true'-[],nl].
other_msg(solutions([B|Bs])) -->
	[nl],
	list_items_sep([B|Bs], ',').
other_msg(bindings_help) -->
	['Top-level options:'-[], nl,
         '   RET y     no more choices'-[],nl,
	 '     ; n     more choices'-[],nl,
	 '       b     break'-[],nl,
	 '       <     reset printdepth'-[],nl,
	 '       < <n> set printdepth'-[],nl,
	 '       ^     reset subterm'-[],nl,
	 '       ^ <n> set subterm'-[],nl,
	 '     ? h     print this information'-[],nl,nl].
other_msg(profile(Data)) -->
	profile(Data).
other_msg(coverage(Data)) -->
	coverage(Data).
other_msg(ide_version_error(IDEVers,MinAllowed,MaxAllowed)) -->
        { ( ground(IDEVers), ( IDEVers = (IDEMin,IDEMax) -> true; IDEMin = IDEVers, IDEMax = IDEVers ), integer(IDEMin), integer(IDEMax) ->
            ( IDEMax < MinAllowed ->
              How = 'too old'
            ; IDEMin > MaxAllowed ->
              How = 'too new'
            ; otherwise ->
              How = 'incompatible'
            )
          ; otherwise ->
            How = 'incompatible'
          )
        },
        ['IDE is ~w. IDE-version (~q) is not in supported range [~d..~d]'-[How, IDEVers,MinAllowed,MaxAllowed],nl].
other_msg(yes) -->
	['yes'-[],nl].
other_msg(no) -->
	[nl,'no'-[],nl].
other_msg(empty) --> [].
other_msg(format(Fmt,Args)) -->
	[Fmt-Args,nl].

print_bindings(emacs, L) --> !,
	{Buf = '*Prolog Bindings*'},
	display_emacs_head(c, [Buf]), [nl],	% create_buffer
	list_items(L),
	['\n'-[],nl],		% force empty line
	display_emacs_tail.
print_bindings(_, L) -->
	list_items(L).

% Display in non-increasing order
memory_buckets([]) --> [].
memory_buckets([0-_|Buckets]) --> !,
	 memory_buckets(Buckets).
memory_buckets([_-stack|Buckets]) --> !,
	 memory_buckets(Buckets).
memory_buckets([Val-Key|Buckets]) -->
	 memory_buckets(Buckets),
	 ['~12|~w ~t~d~46| bytes'-[Key,Val],nl].

show_source_pos(SrcInfo, Port, Cmd) -->
	{prolog_flag(source_info, SrcFlag)},
	show_source_pos(SrcFlag, SrcInfo, Port, Cmd).

no_source_pos -->
	{prolog_flag(source_info, SrcFlag)},
	remove_source_pos(SrcFlag).

remove_source_pos(off) --> [].
remove_source_pos(on) --> [].
remove_source_pos(emacs) -->
	display_emacs_command(u, []).  % unshow

show_source_pos(on, SrcInfo, _, _) --> !,
	print_source_pos(SrcInfo).
show_source_pos(emacs, SrcInfo, Port, ask) --> !,
	emacs_source_pos(SrcInfo, Port).
show_source_pos(_, _, _, _) --> [].

print_source_pos([]) --> [].
print_source_pos(fileref(File,Line)) -->
	['in scope of a goal at line ~d in ~w'-[Line,File],nl].
print_source_pos(clauseref(File,MFunc,Nth,CallNo,_)) -->
	(   {File == []}
	->  ['in scope of body subgoal ~d of clause ~d of ~q'-
	           [CallNo,Nth,MFunc],nl]
	;   ['in scope of body subgoal ~d of clause ~d of ~q in ~w'-
	           [CallNo,Nth,MFunc,File],nl]
	).

emacs_source_pos([], _Port) --> [].
emacs_source_pos(fileref(File, Line), Port) -->
        display_emacs_command(f, [Port, Line, File]). % file_show
emacs_source_pos(clauseref(_,PredRef,_,_,Line), Port) -->
	{Buf = '*Prolog Source*'},
	display_emacs_head(c, [Buf]), [nl],	% create_buffer
	['~@'-[listing(PredRef)]],
	display_emacs_tail, [nl],
	display_emacs_head(b, [Port,Line,Buf]),	% buffer_show
	display_emacs_tail.

display_emacs_command(Command, Args) -->
	display_emacs_head(Command, Args),
	display_emacs_tail.

display_emacs_head(Command, Args) -->
	['$([{~w'-[Command]],
	(   foreach(Arg,Args)
	do  [',~w'-[Arg]]
	).

display_emacs_tail -->
	[nl,'$)]}'-[],nl].


% When Property \== Default IL is Item prepended to IL0, which is a comma
% separated list of items
prepend_item(Property, Default, _Item, IL0, IL) :-
	Property == Default, !,
	IL = IL0.
prepend_item(_, _, Item, IL0, IL) :-
	(   IL0 = [] -> IL = Item
	;   IL = (Item,IL0)
	).

breakpoint_info(bp(_Type,WFunc,BID,Conds,Status),
		BID, Mark, St, Func, Conds) :-
	functor(WFunc, Kind, _), kind_mark(Kind, Mark),
	status_mark(Status, St),
	numbervars(Conds, 0, _),
	(   WFunc = generic -> Func = generic
	;   arg(1, WFunc, Func)
	).

kind_mark(plain, '+').
kind_mark(conditional, '*').
kind_mark(generic, '#').
kind_mark(none, ' ').

status_mark(off, 'D'). % breakpoint disabled
status_mark(on, ' ').  

breakpoint_type_text(debugger, spypoint).
breakpoint_type_text(advice, 'advice point').
breakpoint_type_text(all, breakpoint).

breakpoint_functor_text(plain(MFunc), Type) -->
	['Plain ~a for ~q'-[Type,MFunc]].
breakpoint_functor_text(conditional(MFunc), Type) -->
	['Conditional ~a for ~q'-[Type,MFunc]].
breakpoint_functor_text(generic, Type) --> ['Generic ~a'-[Type]].

breakpoint_already_last_texts(already, 'already ', '').
breakpoint_already_last_texts(last, '', ' (last)').
breakpoint_already_last_texts(ok, '', '').


%--------- default printing predicates

list_items(L) -->
	list_items_sep(L, '').

list_items_sep([], _) --> [].
list_items_sep([G|Gs], Sep) -->
	write_item(G),
	(   {Gs = []} -> [nl]
	;   sep_line(Sep),
	    list_items_sep(Gs, Sep)
	).

sep_line('') --> !, [nl].
sep_line(Sep) --> ['~a'-Sep, nl].

% First three clauses for messages 'trace', 'ancestors', 'backtrace', 
% 'bgoal', 'blocked'; last two clauses for 'solutions'.
% [PM] 4.3 Does not contain '$VAR'(Atom) terms, so no need for legacy numbervars
write_item(goal(Method,Kind,FInv,Inv,Depth,Port,Sel,SelGoal)) -->
	{kind_mark(Kind, Mark)},
	port_info_text(Port, Pport, Nondet),
	{integer(Inv), FInv=:=Inv -> At = '@' ; At = ''},
	['~N~a~a ~t~w~w~9| ~t~w~16|~a'-[Nondet,Mark,At,Inv,Depth,Pport]],
	print_sel_list(Sel),
	write_goal(Method, SelGoal).
write_item(bgoal(I,V,G)) -->
	{prolog_flag(debugger_print_options, Options)},
	['~w (~p): '-[I,V], write_term(G,Options)].
write_item('') --> [].
write_item(binding(Var,Val,Sel)) -->
        write_item(binding(Var,Val,Sel,[])).
write_item(binding(Var,Val,Sel,VariableNames)) -->
        ['~s = '-[Var]],
        print_sel_list(Sel),
        {   current_op(Prio, xfx, =)
        ->  RPrio is Prio-1
        ;   RPrio = 699
        },
        {
           prolog_flag(toplevel_print_options, Options),
           ( VariableNames == [] ->
             AllOptions = [priority(RPrio)|Options]
           ; otherwise ->
             AllOptions = [priority(RPrio),variable_names(VariableNames)|Options]
           )
        },
        [write_term(Val, AllOptions)].
write_item(constraint(G,Sel)) -->
        write_item(constraint(G,Sel,[])).
write_item(constraint(G,Sel,VariableNames)) -->
	print_sel_list(Sel),
        {
           prolog_flag(toplevel_print_options, Options),
           ( VariableNames == [] ->
             AllOptions = Options
           ; otherwise ->
             AllOptions = [variable_names(VariableNames)|Options]
           )
         },
	[write_term(G, AllOptions)].

print_sel_list(Ss) -->
	(   foreach(S,Ss)
	do  ['^~w '-[S]]
	).

port_info_text(ancestor, ' Ancestor: ', ' ') --> []. 
port_info_text(block, ' Block: ', ' ') --> []. 
port_info_text(unblock, ' Unblock: ', ' ') --> [].
port_info_text(call, ' Call: ', ' ') --> [].
port_info_text(exit(Port), Pport, Nondet) -->
	port_info_text(Port, Pport, Nondet).
port_info_text(det, ' Exit: ', ' ') --> [].
port_info_text(nondet, ' Exit: ', '?') --> [].
port_info_text(redo, ' Redo: ', ' ') --> [].
port_info_text(fail, ' Fail: ', ' ') --> [].
port_info_text(exception(_), ' Exception: ', ' ') --> [].
port_info_text(void, ' ', ' ') --> [].

% [PM] 4.3 Argument not contain '$VAR'(Atom) terms, so no need for legacy numbervars
write_goal(display, Goal) --> [write_term(Goal, [ignore_ops(true)])].
write_goal(print, Goal) --> [write_term(Goal, Options)],
	{prolog_flag(debugger_print_options, Options)}.
write_goal(write_term(Options), Goal) --> [write_term(Goal, Options)].
write_goal(write, Goal) --> ['~q'-[Goal]].


%-------------------------help-------------------------------------------
debugging_options --> [
	'Debugging options:'-[], nl,
	'    RET   creep            c      creep'-[],nl,
	'    l     leap             z      zip'-[],nl,
	'    s     skip             s <i>  skip i'-[],nl,
	'    o     out              o <n>  out n'-[],nl,
	'    q     q-skip           q <i>  q-skip i'-[],nl,
	'    r     retry            r <i>  retry i'-[],nl,
	'    f     fail             f <i>  fail i'-[],nl,
	'    j<p>  jump to port     j<p><i>jump to port i'-[],nl,
	'    d     display          w      write'-[],nl,
	'    p     print            p <n>  print partial'-[],nl,
	'    g     ancestors        g <n>  ancestors n'-[],nl,
	'    t     backtrace        t <n>  backtrace n'-[],nl,
	'    [     frame up         ]      frame down'-[],nl,
	'    [ <i> frame i          ] <i>  frame i   '-[],nl,
	'    v     variables        v <i>  variables i'-[],nl,
	'    &     blocked goals    & <n>  nth blocked goal'-[],nl,
	'    n     nodebug          =      debugging'-[],nl,
	'    +     spy this         *      spy conditionally'-[],nl,
	'    -     nospy this       \\ <i>  remove brkpoint'-[],nl,
	'    D <i> disable brkpoint E <i>  enable brkpoint'-[],nl,
	'    a     abort            b      break'-[],nl,
	'    @     command          u      unify'-[],nl,
	'    e     raise exception  .      find this'-[],nl,
	'    <     reset printdepth < <n>  set printdepth'-[],nl,
	'    ^     reset subterm    ^ <n>  set subterm'-[],nl,
	'    ?     help             h      help'-[],nl,nl].



%------------------------------------------------------------------------
%-------------------------- User input handling -------------------------
%------------------------------------------------------------------------

/*
   There is an exact copy of the following clauses in msgs.pl for
   safety.
*/

:- multifile query_class/5.
% query_class(+QueryClass, -Prompt, -InputMethod, -MapMethod, -FailureMode)
query_class(QueryClass, Prompt, line, char(Pairs), help_query) :-
	query_abbreviation(QueryClass, Prompt, Pairs), !.
query_class(next_solution, ' ? ', line, next_solution, help) :- !.
query_class(debugger, ' ? ', line, debugger, help_query) :- !.
query_class(spypoint_cond, 'conditions: ', T, M, F) :- !, def_params(T, M, F).
query_class(dbg_goal_term, 'Goal term: ',  T, M, F) :- !, def_params(T, M, F).
query_class(dbg_exception_term, 'Exception term: ',
	                                   T, M, F) :- !, def_params(T, M, F).
query_class(dbg_extend_term, '(Show,Command,Mode): ',
	                                   T, M, F) :- !, def_params(T, M, F).
query_class(dbg_command, '| :- ', (T-Vs)^term(T,Opts), =, help_query) :- !,
	Opts = [consume_layout(true),syntax_errors(dec10),variable_names(Vs)].
query_class(query, '| ?- ', (T-Vs)^term(T,Opts), =, help_query) :- !,
	Opts = [consume_layout(true),syntax_errors(dec10),variable_names(Vs)].
query_class(clause, '| ', (T-Vs/Ss)^term(T,Opts), =, help_query) :- !,
	Opts = [consume_layout(true),syntax_errors(dec10),variable_names(Vs),singletons(Ss)].
query_class(lbp_goal, '', (T-Vs)^term(T,Opts), =, help_query) :- !,
	Opts = [consume_layout(true),syntax_errors(error),variable_names(Vs)].

def_params(term([consume_layout(true),syntax_errors(dec10)]), =, help_query).

:- multifile query_abbreviation/3.
% query_abbreviation(+QueryClass, -Prompt, -Pairs)
query_abbreviation(yes_or_no, ' (y or n) ', [yes-[-1,0'y,0'Y], no-"nN"]) :- !.
query_abbreviation(yes_no_proceed, ' (y, n, p, s, a, b, or ?) ',
		   [yes-[-1,0'y,0'Y],
		    no-"nN",
		    proceed-"pP",
		    suppress-"sS",
		    '$backtrace'-"tT", % undocumented
		    break-"b",
		    abort-"aA"]) :- !.
query_abbreviation(interrupt, '',
		   [abort-"aA",
		    break-"bB",
		    continue-"cC",
		    exit-[-1,0'e,0'E],
		    debug(debug)-"dD",
		    debug(trace)-"t",
		    debug(zip)-"zZ",
		    '$backtrace'-"T", % undocumented
		    empty-[0'\n]]) :- !.


:- multifile query_input/3.
% query_input(+InputMethod, +Prompt, -RawInput)
query_input(line, Prompt, Line) :- !,
	write(user_error, Prompt),
	flush_output(user_error),
	read_line(user_input, Line).
query_input(term(Opts), Prompt, Term) :- !,
	prompt(OldPrompt, Prompt),
	call_cleanup(read_term(user_input, Term, Opts),
		     prompt(_, OldPrompt)).
query_input(FullInput0^term(Term,Opts), Prompt, FullInput) :- !,
	prompt(OldPrompt, Prompt),
	call_cleanup(read_term(user_input, Term, Opts),
		     prompt(_, OldPrompt)),
	FullInput = FullInput0.


:- multifile query_map/4.
% query_map(+MapMethod, +RawInput, -Result, -Answer)
query_map(next_solution, Line, Result, Answer) :- !,
	remove_nonlayout(Line, Char, Chars),
	query_map_next_solution(Char, Chars, Result, Answer).
query_map(char(Pairs), Line, Result, Answer) :- !,
	pairs_to_answer(Pairs, Line, Result, Answer).
query_map(debugger, Line, Result, Answer) :- !,
	parse_dbg_command(Line, Result, Answer).
query_map(=, Term0, Result, Term) :- !,
	Term = Term0,
	Result = success.

query_map_next_solution(Char, _Chars, success, Answer) :-
	member(Answer-Abbrev, [yes-";nN", no-[-1,0'\n,0'y,0'Y], break-"bB", '$backtrace'-"tT"]),
	member(Char, Abbrev), !.
query_map_next_solution(0'<, Chars, success, printdepth(Arg)) :-
	parse_int_list(IntL, Chars, []),
	type_check(nonneg_arg(10), IntL, Arg), !.
query_map_next_solution(0'^, Chars, success, subterm(Arg)) :-
	parse_int_list(IntL, Chars, []),
	type_check(int_list, IntL, Arg), !.
query_map_next_solution(_, _, failure, _).

/*
   The following clauses only have a simplified copy in msgs.pl.
*/

%--- Map abbreviation to answer -----------------------------------------
pairs_to_answer(Pairs, Line, Result, Answer) :-
	first_nonlayout_char(Line, C),
	member(Answer-Abrv, Pairs),
	member(C,Abrv), !,
	Result = success.
pairs_to_answer(_, _, failure, _).

first_nonlayout_char(Line, C) :-
	remove_nonlayout(Line, C, _).

remove_nonlayout([C0|Chars], First, Rest) :-
	(   white_space(C0) -> remove_nonlayout(Chars, First, Rest)
	;   First = C0,
	    Rest = Chars
	).
remove_nonlayout([], 0'\n, []).
remove_nonlayout(end_of_file, -1, []).

%--- Map trace command to answer ----------------------------------------
% Also used by Suite/debugger.pl
parse_int_list([Arg|Args]) -->
	parse_int(Arg), !,
	parse_int_list(Args).
parse_int_list([]) --> all_whitespace.

parse_int(Arg) -->
	[C],
        (   {digit(C, D)} ->
	    parse_int(D, Arg)
	;   {C =:= 0'-} ->
	    parse_int(0, Arg1),
	    {Arg is -Arg1}
	;   {white_space(C)} ->
	    parse_int(Arg)
	).

parse_int(Arg0, Arg) -->
	[C],
	{digit(C, D)}, !,
	{Arg1 is Arg0*10 + D},
	parse_int(Arg1, Arg).
parse_int(Arg, Arg) --> [].

all_whitespace -->
	[C], !,
	{white_space(C)},
	all_whitespace.
all_whitespace --> [].

digit(C, D) :-
	C >= 0'0, C =< 0'9,
	D is C - 0'0.

white_space(127) :- !.  % DEL
white_space(C) :- C =< 0' .


% These are the basic_trace_commands in traceui.pl
standard_dbg_command(-1,   _, abort,                 no_arg).
standard_dbg_command(0'a,  _, abort,                 no_arg).
standard_dbg_command(0'c,  _, creep,                 no_arg).
standard_dbg_command(0'\n, _, creep,                 no_arg).
standard_dbg_command(0'l,  _, leap,                  no_arg).
standard_dbg_command(0'z,  _, zip,                   no_arg).
standard_dbg_command(0'n,  _, nodebug,               no_arg).
standard_dbg_command(0'r,  A, retry(A),              pos_arg).
standard_dbg_command(0'f,  A, fail(A),               pos_arg).
standard_dbg_command(0's,  A, skip(A),               pos_arg).
standard_dbg_command(0'q,  A, qskip(A),              pos_arg).
standard_dbg_command(0'd,  _, display,               no_arg).
standard_dbg_command(0'w,  _, write,                 no_arg).
standard_dbg_command(0'\\, A, remove_breakpoint(A),  pos_arg).
standard_dbg_command(0'D,  A, disable_breakpoint(A), pos_arg).
% These are also simple to parse, but more complex to process later
standard_dbg_command(0'\t, _, emacs_lbp,             no_arg). %!!! doesn't work now % [PM] read emacs/console Goal
standard_dbg_command(0'u,  _, unify,                 no_arg). %!!!
standard_dbg_command(0'e,  _, exception,             no_arg). %!!!
standard_dbg_command(0'x,  _, extend,                no_arg). %!!!
standard_dbg_command(0'o,  A, out(A),                int_arg).
standard_dbg_command(0'g,  A, ancestors(A),          pos_arg(-1)).
standard_dbg_command(0't,  A, backtrace(A),          pos_arg(-1)).
standard_dbg_command(0'v,  A, variables(A),          pos_arg(-1)).
standard_dbg_command(0'[,  A, up(A),                 pos_arg(-1)).
standard_dbg_command(0'],  A, down(A),               pos_arg(-1)).
standard_dbg_command(0'&,  A, blocked_goals(A),      pos_arg).
standard_dbg_command(0'p,  A, print(A),              nonneg_arg(none)).
standard_dbg_command(0'+,  _, spy,                   no_arg).
standard_dbg_command(0'-,  _, nospy,                 no_arg).
standard_dbg_command(0'*,  _, conditional_spy,       no_arg).
standard_dbg_command(0'E,  A, enable_breakpoint(A),  pos_arg).
standard_dbg_command(0'.,  _, find_this,             no_arg).
standard_dbg_command(0'=,  _, debugging,             no_arg).
standard_dbg_command(0'b,  _, break,                 no_arg).
standard_dbg_command(0'T,  _, '$backtrace',          no_arg). % undocumented
standard_dbg_command(0'@,  _, command,               no_arg). %!!!
standard_dbg_command(0'<,  A, set_printdepth(debugger,A), nonneg_arg(10)).
standard_dbg_command(0'^,  A, set_subterm(A),        int_list).
standard_dbg_command(0'?,  _, help,                  no_arg).
standard_dbg_command(0'h,  _, help,                  no_arg).


default_or_int_with_cond([], Default, _, Default).
default_or_int_with_cond([Int], _, Cond, Arg) :-
	holds(Cond, Int), !,
	Arg = Int.

holds(gt(I0), I) :- I > I0.
holds(ge(I0), I) :- I >= I0.
holds(nocond, _).

type_check(no_arg, [], _).
type_check(int_list, IntL, IntL).
type_check(int_arg, IntL, Arg) :-
	default_or_int_with_cond(IntL, 1, nocond, Arg).
type_check(pos_arg, IntL, Arg) :-
	default_or_int_with_cond(IntL, none, gt(0), Arg).
type_check(pos_arg(Default), IntL, Arg) :-
	default_or_int_with_cond(IntL, Default, gt(0), Arg).
type_check(nonneg_arg(Default), IntL, Arg) :-
	default_or_int_with_cond(IntL, Default, ge(0), Arg).

parse_dbg_command(Line, Result, Answer) :-
	remove_nonlayout(Line, Char, LineEnd),
	parse_dbg_command(Char, LineEnd, Result0, Answer0),
	(   Result0 = failure(Warning)
	->  Answer = unknown([Char|LineEnd],Warning),
	    Result = success
	;   Answer = Answer0,
	    Result = Result0
	).

parse_dbg_command(Char, Line, Result, Answer) :-
	standard_dbg_command(Char, Arg, Answer, Type), !,
	parse_std_args(Line, Type, Arg, Result, Answer).
parse_dbg_command(0'j, Line, Result, Answer) :- !,
	parse_jump_command(Line, Result, Answer).
parse_dbg_command(0':, _Line, success, command) :- !. % emacs support
parse_dbg_command(_, _, failure(trace_command), _).


% parse_std_args(+Line, +Type, -Arg, -Result, +Command)
parse_std_args(Line, Type, Arg, Result, _) :-
	parse_int_list(IntL, Line, []),
	type_check(Type, IntL, Arg), !,
	Result = success.
parse_std_args(_, _, _, Result, Command) :-
	functor(Command, CommandName, _),
	Result = failure(trace_command(CommandName)).

parse_jump_command([Char|Line], Result, Answer) :-
	parse_jump_character(Char, JumpCommand), !,
	parse_std_args(Line, pos_arg, Arg, Result, jump),
	Answer = jump(JumpCommand, Arg).
parse_jump_command(_,failure(trace_command(jump)),_).

parse_jump_character(0'c, jump_retry).
parse_jump_character(0'f, jump_fail).
parse_jump_character(0'e, jump_reexit).
parse_jump_character(0'r, jump_redo).

% [4.2] profiling, coverage analysis

profile(Data) -->
	(   foreach(Caller-counter(CalleeCounts,Insns,Choices,Calls),Data),
	    foreach(Caller-counter(Insns,Choices,Calls),CalleeCountsL),
	    foreach(Key-Caller,KL1),
	    fromto(Pairs1,Pairs2,Pairs4,[]),
	    fromto(Down1,Down2,Down4,[]),
	    fromto(Up1,Up2,Up4,[])
	do  {Key is -Insns},
	    (   foreach(Callee-TagC,CalleeCounts),
		fromto(Pairs2,[(Caller-Callee)-TagC|Pairs3],Pairs3,Pairs4),
		fromto(Down2,[Caller-Callee|Down3],Down3,Down4),
		fromto(Up2,[Callee-Caller|Up3],Up3,Up4),
		param(Caller)
	    do  []
	    )
	),
	{keysort(KL1, KL2)},
	{keysort(Pairs1, Pair2Inta)},
	{prolog:keyclumped(Pair2Inta, Pair2Intb)},
	{   foreach(Pair-TagInts,Pair2Intb),
	    foreach(Pair-TagTotal,Pair2Int)
	do  (   foreach(TX,TagInts),
		fromto(det(0),S0,S,TagTotal)
	    do  profile_add(S0, TX, S)
	    )
	},
	{keysort(CalleeCountsL, Callee2Counts)},
	{sort(Down1, Down5)},
	{prolog:keyclumped(Down5, DownG)},
	{sort(Up1, Up5)},
	{prolog:keyclumped(Up5, UpG)},
	['~i~tinsns~12|~ttry/retry~24|~tcalled~36|~t~44|name'-[0],nl],
	(   foreach(_-Self,KL2),
	    param(Pair2Int,Callee2Counts,DownG,UpG)
	do  ['~i~`-t~64|'-[0],nl],
	    {memberchk(Self-Callers, UpG) -> true ; Callers = []},
	    {memberchk(Self-Callees, DownG) -> true ; Callees = []},
	    {memberchk(Self-counter(Insns1,Choices1,Calls3), Callee2Counts)},
	    (   foreach(Caller1,Callers),
		param(Self,Pair2Int,Calls3)
	    do  {memberchk((Caller1-Self)-TagD, Pair2Int)},
		profile_line(TagD, Calls3, Caller1)
	    ),
	    profile_line(Calls3, Insns1, Choices1, Self),
	    (   foreach(Callee1,Callees),
		param(Self,Pair2Int,Callee2Counts)
	    do  {memberchk((Self-Callee1)-TagE, Pair2Int)},
		{   memberchk(Callee1-counter(_,_,Calls4), Callee2Counts) -> true
		;   Calls4 = [0] % SPRM 13268, best effort
		},
		profile_line(TagE, Calls4, Callee1)
	    )
	).

profile_add(det(X), det(Y), det(Z)) :- !,
	Z is X+Y.
profile_add(det(X), nondet(Y), nondet(Z)) :- !,
	Z is X+Y.
profile_add(nondet(X), det(Y), nondet(Z)) :- !,
	Z is X+Y.
profile_add(nondet(X), nondet(Y), nondet(Z)) :- !,
	Z is X+Y.

profile_line(det(Part),TTotal,Callee) -->
	{arg(1, TTotal, Total)},
	['~t~d~36|/~d~t~48|~q'-[Part,Total,Callee],nl].
profile_line(nondet(Part),TTotal,Callee) -->
	{arg(1, TTotal, Total)},
	['~t*~d~36|/~d~t~48|~q'-[Part,Total,Callee],nl].

profile_line(det(Calls), Insns, Choices, Self) -->
	    ['~t~d~12|~t~d~24|~t~d~36|~t~44|~q'-[Insns,Choices,Calls,Self],nl].
profile_line(nondet(Calls), Insns, Choices, Self) -->
	    ['~t~d~12|~t~d~24|~t*~d~36|~t~44|~q'-[Insns,Choices,Calls,Self],nl].

coverage(Data0) -->
	{sort(Data0, Data)},
	(   foreach(counter(File1,Pred1,ClauseNo1,Line1)-Count1,Data),
	    foreach(File1-counter(Pred1,ClauseNo1,Line1,Count1),KL1)
	do  []
	),
	{prolog:keyclumped(KL1, KL2)},
	(   foreach(File2-Clump2,KL2)
	do  [File2-[],nl],
	    (   foreach(counter(Pred2,ClauseNo2,Line2,Count2),Clump2),
		foreach(Pred2-counter(ClauseNo2,Line2,Count2),KL3)
	    do  []
	    ),
	    {prolog:keyclumped(KL3, KL4)},
	    (   foreach(Pred3-Clump3,KL4)
	    do  ['    ~q'-[Pred3],nl],
		(   foreach(counter(ClauseNo3,Line3,Count3),Clump3),
		    foreach(ClauseNo3-counter(Line3,Count3),KL5)
		do  []
		),
		{prolog:keyclumped(KL5, KL6)},
		(   foreach(ClauseNo4-Clump4,KL6)
		do  ['        clause ~d'-[ClauseNo4],nl],
		    (   foreach(counter(Line4,Count4),Clump4)
		    do  coverage_line(Count4, Line4)
		    )
		)
	    )
	).

coverage_line(det(Count), Line) -->
	['~t~d~12|:~t~d~24|'-[Line,Count],nl].
coverage_line(nondet(Count), Line) -->
	['~t~d~12|:~t*~d~24|'-[Line,Count],nl].

% [PM] 4.3 Add these last in case there are errors while compiling this file (causing generate_message//1 to be called...
:- multifile generate_message/3.
generate_message(Message) --> error_msg(Message), !.
generate_message(Message) --> other_msg(Message), !.
