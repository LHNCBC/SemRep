%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%% SEMREP %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

g    :-
	( current_predicate(go_4, usemrep:_P) ->
	  go_4(interactive, _Predications, _InputFile, OutputFile)
	; go(interactive, _Predications, _InputFile, OutputFile)
        ),
	( var(OutputFile) ->
	  true
	; concat_atom(['cat ', OutputFile], UnixCommand),
	  format('~n~n~*c~n', [80,36]),
	  format('OUTPUT FILE: ~w~n', [OutputFile]),
	  format('~*c~n~n', [80,36]),
	  u UnixCommand
	).


% This is an undocumented feature of Quintus provided by Mats Carlson.
% It has the effect of running the Prolog predicate shutdown_semrep/0
% when Prolog exits.

:- '$Quintus: hook'(assert, exit, permanent, shutdown_semrep).

shutdown_semrep :-
	% only call shutdown_semrep if user is currently running semrep
	( current_module(tagger_access03),
	  write('CALLING stop_tagger_access'),
	  tagger_access06:stop_tagger_access,
	  nl,
	  fail
	; current_module(skr),
	  write('CALLING stop_metamap'),
	  skr:stop_metamap,
	  nl,
	  fail
	; current_module(db_access06),
	  write('CALLING stop_db_access'),
	  db_access06:stop_db_access,
	  nl
	; true
        ).
