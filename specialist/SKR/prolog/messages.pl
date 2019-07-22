%%% Code provided by Per Mildner via e-mail 07/09/2009
%%% to store and re-play warning and error messages
%%% generated during compilation.
%%% Updated in e-mail 01/17/2011 to change arity of prompt(...) term from 5 to 6.

:- dynamic collecting_messages/0.
:- dynamic collected/3.

%% initially true
collecting_messages.

:- multifile user:generate_message_hook/3.

% user:generate_message_hook(A,B,C) :-
% 	format(user_output, 'GMH A: ~q~n', [A]),
% 	'SU_messages':generate_message(A,B,C),
%  	format(user_output, 'GMH B: ~q~n', [B]),
% 	format(user_output, 'GMH C: ~q~n~n', [C]).

:- multifile user:message_hook/3.

% user:generate_message_hook(solutions(BindingsList), MessageLines, []) :-
%  	user:generate_message_hook(bindings(BindingsList), MessageLines, []).

% BindingsList is a list of binding/3 terms:
% binding(VarName, Value, SubtermSelector)
% VarName is a list of ASCII codes,
% Value is an arbitrary term, and
% SubtermSelector (irrelevant for our purposes here) is a (usually empty) list of integers.
% E.g.,
% bindings( [ binding("Input", [1,2,3], []),
%	      binding("Accumulator", 0, []),
%	      binding("Output",_, []) ] )
% pad_bindings_list/3 transforms the above list of bindings/3 terms into
% bindings( [ binding("Input      ", [1,2,3], []),
%	      binding("Accumulator", 0, []),
%	      binding("Output     ",_, []) ] )
% by adding enough spaces to the end of the names of all the variables
% (other than the longest) so that all the variables names have the same length.
% As a result, the "="s in the variable bindings are all vertically aligned,
% which results in far more readable output.


user:generate_message_hook(bindings(BindingsList), MessageLines, []) :-
	BindingsList = [_|_],
	% Ensure that BindingsList is a list of binding/3 terms
 	(  foreach(BindingsTerm, BindingsList)
 	do BindingsTerm = binding(_,_,_)
  	),
	% determine the length of the longest variable name in the bindings list
	max_bindings_variable_length(BindingsList, MaxLen),
	% pad all the variables with enough blank spaces so that
	% all the bindings will align on the "=" in the "*Prolog Bindings*" buffer.
	pad_bindings_list(BindingsList, MaxLen, PaddedBindingsList),
	'SU_messages':other_msg(bindings(PaddedBindingsList), MessageLines, []).

% Another way to do this would involve modifying the Control-Args terms
% by inserting blank spaces in them instead. This could be done as follows:
% 	'SU_messages':other_msg(bindings(BindingsList), OrigMessageLines, []),
% 	pad_message_lines(OrigMessageLines, MaxLen, MessageLines).

% pad_message_lines(OrigMessageLines, MaxLen, MessageLines) :-
% 	(  foreach(X, OrigMessageLines),
% 	   foreach(Y, MessageLines),
% 	   param(MaxLen)
% 	do ( X = '~s = '-[List] ->
% 	     length(List, Length),
% 	     Padding is MaxLen - Length,
% 	     Y = '~s~*c = '-[List,Padding,32]
% 	   ; Y = X
% 	   )
% 	).
% 
% Original Control-Args term:
% '~s = '-[[82,101,115,116,80,104,114,97,115,101,115]]
% i.e.,
% '~s = '-["RestPhrases"]
% Padded Control-Args term:
% '~s~*c = '-[[82,101,115,116,80,104,114,97,115,101,115],N,32]
% i.e.,
% '~s~*c = '-["RestPhrases",N,32]

%% Records messages for later re-play
user:message_hook(Severity, Message, Lines) :-
	collecting_messages,
	memberchk(Severity, [warning, error]),
	assertz(collected(Severity, Message, Lines)),
	%% also do default processing
	fail.
  
%% This will be called by top-level on each prompt
%% Arity-5 version of prompt(...)
user:message_hook(informational,prompt(_,_,_,_,_),_) :-
	%% Ensure we only print collected
	%% messages at first prompt.
	collecting_messages,
	retractall(collecting_messages),
	print_collected_messages,
	%% let default processing handle the prompt
	fail.

%% This will be called by top-level on each prompt
%% Arity-6 version of prompt(...)
user:message_hook(informational,prompt(_,_,_,_,_,_),_) :-
	%% Ensure we only print collected
	%% messages at first prompt.
	collecting_messages,
	retractall(collecting_messages),
	print_collected_messages,
	%% let default processing handle the prompt
	fail.

print_collected_messages :-
	( collected(_, _, _) -> % >= 1 messages
	  format(user_output, '~N### COLLECTED ERRORS~n',[]),
	  ( collected(S, M, L),
	    print_collected_message(S, M, L),
	    nl(user_error),
	    fail
	  ; retractall(collected(_, _, _))
	  )
	; true
	).

print_collected_message(Severity, _Message, Lines) :-
	print_message_lines(user_error, Severity, Lines).


% determine the length of the longest variable name in the bindings list
max_bindings_variable_length(BindingsList, MaxLen) :-
	(  foreach(Line, BindingsList),
	   fromto(0, In, Out, MaxLen)
	do Line = binding(VariableName,_VariableBinding,_SubtermSelector),
	   length(VariableName, VariableNameLength),
	   Out is max(In, VariableNameLength)
	).

pad_bindings_list(BindingsList, MaxLen, PaddedBindingsList) :-
	(  foreach(BindingTerm, BindingsList),
	   foreach(PaddedBindingTerm, PaddedBindingsList),
	   param(MaxLen)
	do pad_binding_term(BindingTerm, MaxLen, PaddedBindingTerm)
	).

pad_binding_term(binding(VarName,       Binding, ArgSel),
		 MaxLen,
		 binding(PaddedVarName, Binding, ArgSel)) :-
	length(VarName, VarLen),
	PaddingLength is MaxLen - VarLen,
	% Create a list of length PaddingLength, all of whose elements are 32
	(  for(_, 1, PaddingLength),
	   foreach(32, BlanksList)
	do true
	),
	append(VarName, BlanksList, PaddedVarName).
	
	

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Allow non-interactive tracing of specific predicates
% This can be done simply via the advanced debugging options, e.g.,
% spy(qp_lookup:assembledefns_4/4, -[print,proceed,debug]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
