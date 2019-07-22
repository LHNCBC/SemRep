%% patch for SPRM 14171
%% compile me in the 'prolog' module, thus:
%% | ?- [prolog:'patch14171.pl'].

% This is also used by spider.pl
show_variables1(InvSt, Bindings) :-
	inv_st_info(pc, InvSt, PC),
	compound(PC),
	decomp_pc(PC, Ref, [], Sels), !,
	'$instance_defmap'(Ref, Map, Map),
	'$instance'(Head, Body, Ref),
	term_variables_dfs((Head:-Body), Vas), % [PM] 4.3 Is DFS order really important?
	set_subterm(Sels, Body, Focus),	       % [MC] 4.3.1 Body was (Head:-Body)
	term_variables_unordered(Focus, FVas),
	inv_st_goal_mod(InvSt, Goal, Module),
	(   foreach(Name,Map),
	    foreach(Var,Vas),
	    fromto(Bindings1,B1,B2,[]),
	    param(FVas)
	do  (   atom(Name),
		member(FV, FVas),
		FV==Var ->
		B1 = [Name=Var|B2]
	    ;   B1 = B2
	    )
	),
	(Focus = Goal -> true ; Focus = Module:Goal),
	sort(Bindings1, Bindings).
show_variables1(InvSt, Bindings) :-
	inv_st_info(pc, InvSt, PC),
	integer(PC),
	PC=\=0,
	inv_st_goal_mod(InvSt, Goal, Module),
	dbgvas_common(PC, LeafStretch, LeafSite, SiteClauses, ClauseCallers, WAM), !,
	%% SiteClauses is list of pairs Site-Disjunct containing site
	%% ClauseCallers is list of pairs Disjunct-Site calling disjunct
	%% Gather info from (leaf) goal
	dbgvas_call_info(LeafStretch, LeafSite, SiteClauses, ClauseCallers, WAM,
			 Module:Goal,
			 Bindings1, Bindings2),
	%% Gather info from parent chain
	retry_chpt(call_chpt, InvSt, Chpt), 
	(   fromto(LeafSite,Site1,Site2,-1),
	    fromto(LeafStretch,Stretch1,Stretch2,-1),
	    fromto(Bindings2,Bindings3,Bindings4,[]),
	    param(SiteClauses,ClauseCallers,WAM,Chpt)
	do  dbgvas_stretch_info(Stretch1, Site1, ClauseCallers, WAM, Chpt,
				Bindings3, Bindings4),
	    (   member(Stretch1-Site2,ClauseCallers),
		member(Site2-Stretch2,SiteClauses) -> true
	    ;   Site2 = -1,
		Stretch2 = -1
	    )
	),
	sort(Bindings1, Bindings).
show_variables1(_, []).
