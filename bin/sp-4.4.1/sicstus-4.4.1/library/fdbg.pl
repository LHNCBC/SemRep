/* Copyright(C) 2001, Swedish Institute of Computer Science */

%   File       : fdbg.pl
%   Authors    : David Hanak, Tamas Szeredi
%   Updated    : 9 October 2001
%   Purpose    : Finite domain debugger

:- module(fdbg, [
        % predicates to be called from toplevel
	fdbg_on/0,
	fdbg_on/1,
	fdbg_off/0,
        % predicates to be called from Prolog code
	fdbg_assign_name/2,
	fdbg_current_name/2,
	fdbg_get_name/2,
	fdbg_show/2,
	fdbg_guard/3,
	fdbg_label_show/3,
	fdbg_annotate/3,
	fdbg_annotate/4,
	fdbg_legend/1,
	fdbg_legend/2,
	fdbg_transform_actions/3
		]).

:- meta_predicate
	fdbg_on(:),
	fdbg_guard(3,?,?).

:- use_module(library(clpfd)).

:- use_module(library(lists), [
	is_list/1,
	keyclumped/2,
	nth1/3,
	select/3
	]).

:- use_module(library(sockets), [
	%% socket/2,
	%% socket_connect/3,
	%% socket_buffering/4,
	%% current_host/1
        socket_client_open/3,
        current_host/1
	]).

:- use_module(library(codesio), [
	read_from_codes/2
	]).

:- use_module(library(avl), [
	empty_avl/1,
	avl_store/4,
        avl_member/3,
        avl_fetch/3
	]).

:- use_module(library(types), [
        must_be/4
	]).

:- use_module(library(atts)).

% For a component of a selector the # prefix operator indicates that the 
% given number refers to the n-th element of a list rather than the n-th
% argument of a term.
:- op(400, fy, #).

% The attribute storing the name of the variable
:- attribute name/1.

% Required multifile predicates:
:- multifile
	user:portray/1,
	user:portray_message/2,
	user:debugger_command_hook/2.

% Provided multifile predicates:
:- multifile
	fdvar_portray/3,
	legend_portray/3.

% [PM] 4.2.1 missing meta declarations.
:- meta_predicate
        handle_options(:),
        succeed(0).

%% Blackboard entries:
%%   installed_breakpoints - list of main advice point IDs
%%   fdbg_logfile          - the stream used for logging
%%   constraint_hook       - the hook(s) to be called on dispatch_global/4 exit
%%   labeling_hook         - the hook(s) to be called on labeling events
%%   fdvar_counter         - the number of the next auto-named variable
%%
%% Options:
%%
%%   Output redirection:
%%
%%   * file(Filename, Mode) - Tells FDBG to set the stream alias 'fdbg_output'
%%	to the file called Filename opened in mode Mode.  Mode can either
%%	be 'write' or 'append'.  The file specified is opened on a call to
%%	fdbg_on/1 and is closed on a call to fdbg_off/0.  The built-in 
%%	visualizer dumps it's output to this stream and it's advisable for any
%%	user written visualizers to do the same.
%%   * socket(Host, Port) - Same as the file option, but it tells FDBG to 
%%      set the stream alias 'fdbg_output' to the _socket_ connected to
%%      Host on port Port.  The specified socket is created on a call to
%%      fdbg_on/1 and is closed on a call to fdbg_off/0.
%%   * stream(StreamOrAlias) - Tells FDBG to set the stream alias 'fdbg_output'
%%      to the given stream or stream alias.
%%
%%     If none of the three output-specification options (file, socket or
%%     stream) is given in the option list, then the default behaviour is to
%%     set the 'fdbg_output' alias to the current output stream.
%%
%%   Global constraint events:
%%
%%   * constraint_hook(Goal) - Tells FDBG to extend Goal with two (additional) 
%%	arguments and call it on the exit port of the global constraint 
%%	dispatcher.  The two arguments are the following:
%%	  - Constraint: the constraint that was handled by the dispatcher
%%	  - Actions:    the action list returned by the dispatcher
%%	Goal should normally be a visualizer, either built-in (see fdbg_show/3)
%%	or user defined.  Several 'constraint_hook' options may appear in the
%%	option list, they will be called in their order of appearence.
%%   * no_constraint_hook - Tells FDBG not to call any visulaizer for global
%%     constraint events.
%%
%%     If none of these options is given then the default constraint
%%     visualizer, fdbg_show/3 is used.
%%
%%   Labeling events:
%%
%%   * labeling_hook(Goal) - Tells FDBG to extend Goal with three (additional)
%%      arguments and call it on any of the following labeling events:
%%        - start:      labeling has just started for a variable
%%        - fail:       labeling has just failed for a variable
%%        - step(Step): variable has been constrained in a labeling step,
%%                       which is described by the compound term Step
%%      The three arguments are the following:
%%        - Event: one of the atoms above
%%        - ID:    identifies the labeling session, i.e. binds step and
%%                 fail events to the corresponding start event
%%        - Var:   the variable being the subject of labeling
%%   * no_labeling_hook - Tells FDBG not to call any visulaizer for labeling
%%      events.
%%
%%     If none of these options is given then the default labeling
%%     visualizer, fdbg_label_show/3 is used.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                  fdbg_on                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_on(+Options):  Installs advice points required to debug constraint
%%   variables using the specified Options (see Options description).  Can be
%%   called safely several times - installs itself only once.
fdbg_on(_) :-
	debugger_is_on, !,
	print_message(warning, fdbg(state(already,on))).
fdbg_on(Options) :-
	handle_options(Options),
	% To create Byrd-box for '$fd_eval_indexical'/3.
 	add_breakpoint([pred(clpfd:'$fd_eval_indexical'/3), advice, port(call)]-
		       [command(proceed)],
 		       BID14),
	% To handle indexicals
	add_breakpoint([goal(clpfd:'$fd_eval_indexical'(_,Actions,Indexical)),
			advice, port(exit)]-
		       [true(fdbg:dispatch_hook_raw(Indexical,Actions)),
			command(proceed)],
		       BID15),
	% To create Byrd-box for dispatch_global_fast/5.
 	add_breakpoint([pred(clpfd:dispatch_global_fast/5), advice, port(call)]-
		       [command(proceed)],
 		       BID1),
	% To handle global constraints
	add_breakpoint([goal(clpfd:dispatch_global_fast(_,_,_,Actions,Global)),
			advice, port(exit)]-
		       [true(fdbg:dispatch_hook_raw(Global,Actions)),
			command(proceed)],
		       BID2),
	% To handle in_set/2 [MC]
	add_breakpoint([goal(clpfd:prune_and_propagate(Var,Set)),
			advice, port(call)]-
		       [true(fdbg:aux_dispatch_hook(Var in_set Set)),
			command(proceed)],
		       BID3),
	add_breakpoint([goal(clpfd:prune_and_propagate_chk(Var,Set)),
			advice, port(call)]-
		       [true(fdbg:aux_dispatch_hook(Var in_set Set)),
			command(proceed)],
		       BID4),
	% To handle in/2 [MC]
	add_breakpoint([goal(clpfd:propagate_interval(Var,Min,Max)),
			advice, port(call)]-
		       [true(fdbg:aux_dispatch_hook(Var in Min..Max)),
			command(proceed)],
		       BID5),
	add_breakpoint([goal(clpfd:propagate_interval_chk(Var,Min,Max)),
			advice, port(call)]-
		       [true(fdbg:aux_dispatch_hook(Var in Min..Max)),
			command(proceed)],
		       BID6),
	% To handle domain/[2,3] [MC]
	add_breakpoint([goal(clpfd:domain(List,Set)), advice, port(call)]-
		       [true(fdbg:aux_dispatch_hook(domain(List,Set))),
			command(proceed)],
		       BID7),
	% To handle Var=Other formed unifications
	add_breakpoint([goal(clpfd:verify_attributes(Var,Other,_Goals)),
			advice, port(call)]-
		       [true(fdbg:aux_dispatch_hook(Var=Other)),
			command(proceed)],
		       BID8),
	% To handle labeling steps
	add_breakpoint([goal(clpfd:fdbg_start_labeling(X, Var)), advice]-
		       [port(Port),
			true(fdbg:labeling_hook(Port, Var)),
			command(proceed)],
		       BID9),
	add_breakpoint([goal(clpfd:fdbg_labeling_step(Var, Step)),
			advice, port(call)]-
		       [true(fdbg:labeling_hook(step(Step), Var)),
			command(proceed)],
		       BID10),
	add_breakpoint([goal(clpfd:labeling_singleton(Var, X, Mode)),
			advice, port(call)]-
		       [true(fdbg:labeling_hook(step('$labeling_step'(Var, =, X, Mode)),
						Var)),
			command(proceed)],
		       BID11),
	add_breakpoint([goal(clpfd:labeling_max(Var, Max, Mode)),
			advice, port(call)]-
		       [true(fdbg:labeling_hook(step('$labeling_step'(Var, =<, Max, Mode)),
						Var)),
			command(proceed)],
		       BID12),
	add_breakpoint([goal(clpfd:labeling_min(Var, Min, Mode)),
			advice, port(call)]-
		       [true(fdbg:labeling_hook(step('$labeling_step'(Var, >=, Min, Mode)),
						Var)),
			command(proceed)],
		       BID13),
	bb_put(installed_breakpoints, [BID1, BID2, BID3, BID4, BID5,
				       BID6, BID7, BID8, BID9, BID10,
				       BID11, BID12, BID13, BID14, BID15]),
	bb_put(fdvar_counter, 1),
	prolog:'$module_hiding'(clpfd, _, off), % [MC] 4.3.1, advices require it
	print_message(informational, fdbg(state(on))).

%% fdbg_on:  Calls fdbg_on/1 with a default option list.
fdbg_on :-
	fdbg_on([%% [MC] file('fdbg.log', write),
		 %% [MC] constraint_hook(fdbg:fdbg_show),
		 %% [MC] labeling_hook(fdbg:fdbg_label_show)
		 ]).

%% debugger_is_on: succeed if fdbg is currently turned on
debugger_is_on :-
	bb_get(installed_breakpoints, _).

%% [PM] 4.1.3 SPIDER/xref
:- public aux_dispatch_hook/1, labeling_hook/2.

%% [MC] simulate an action list for =/2
aux_dispatch_hook(X=Y) :-
	Goal = (X=Y),
	dispatch_hook(Goal, [exit,Goal]).
%% [MC] simulate an action list for in_set/2
aux_dispatch_hook(Var in_set Set) :-
	Goal = (Var in_set Set),
	dispatch_hook(Goal, [exit,Goal]).
%% [MC] simulate an action list for in/2
aux_dispatch_hook(Var in Min..Max) :-
	Goal = (Var in Min..Max),
	(   Min..Max == (inf..sup) -> true
	;   dispatch_hook(Goal, [exit,Goal])
	).
%% [MC] simulate an action list for domain/[2,3]
aux_dispatch_hook(domain(List,Set)) :-
	(   foreach(X,List),
	    foreach((X in_set Set),InList),
	    param(Set)
	do  true
	),
	dispatch_hook(domain(List,Set), [exit|InList]).

% Called from breakpoint conditions (see fdbg_on/1)
:- public dispatch_hook_raw/2.

dispatch_hook_raw(Frob, Actions) :-
	clpfd:propagator_goal(Frob, [Constraint], []),
	dispatch_hook(Constraint, Actions).

%% dispatch_hook(+Constraint, +Actions):  Call visualizers to display
%%   Constraint, the global constraint that has been waken right now.
dispatch_hook(Constraint, Actions) :-
	bb_get(constraint_hook, Goals),
	(   clpfd:fdpred_exported(Constraint, Sugared) -> true
	;   Sugared = Constraint
	),
	call_goals_with_args(Goals, [Sugared,Actions]),
        flush_output(fdbg_output). % [PM] 4.2.1 can throw but can not fail!

%% labeling_hook(+Port, +Var):  Calls the labeling visualizers to handle
%%   the labeling event regarding Var.
labeling_hook(Port, Var) :-
	bb_get(labeling_hook, Goals),
	labeling_inv_and_action(Port, Inv, Event),
	call_goals_with_args(Goals, [Event, Inv, Var]),
        flush_output(fdbg_output). % [PM] 4.2.1 can throw but can not fail!

%% labeling_inv_and_action(+Port, -InvNum, -Event):  Translates the
%%   Byrd-box port of the labeling predicate to an invocation number InvNum
%%   identifying the labeling event, and a compound term describing the
%%   event.
labeling_inv_and_action(call, I, start) :-
	execution_state(inv(I)).
labeling_inv_and_action(fail, I, fail) :-
	execution_state(inv(I)).
labeling_inv_and_action(step(S), I, step(S)) :-
	get_pred_inv(clpfd:fdbg_start_labeling/2, I).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 fdbg_off                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_off:  Uninstalls advice points required to debug constraint variables
%%   and closes the logfile if necessary.
fdbg_off :-
	bb_delete(installed_breakpoints, BIDS), !,
	remove_breakpoints(BIDS),
	bb_put(fdvar_counter, 1),
	succeed(bb_delete(constraint_hook, _)), % These bb entries 
	succeed(bb_delete(labeling_hook, _)),   % might not exist
	bb_delete(fdbg_logfile, Stream/Action),
        %% [PM] 4.0 now done in close/1 and remove_stream_alias/2 below
	%% %% temporary hack
	%% stream_code(Stream, Code),
	%% prolog:erase_stream_properties(Code),
	(   Action == close -> close(Stream) % [PM] 4.0 will remove fdbg_output alias
	;   prolog:remove_stream_alias(Stream, fdbg_output) % [PM] 4.0 just remove the alias
	),
	prolog:'$module_hiding'(clpfd, _, on), % [MC] 4.3.1, see fdbg_on
	print_message(informational, fdbg(state(off))).
fdbg_off.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              fdbg_assign_name                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_assign_name(+Term, ?Name):  Assign the atom Name to Term, and also 
%%   assign a derived name to each subterm of Term which is a variable.  
%%   If Name is a variable, then use a default (generated) name, and return
%%   it in Name.
fdbg_assign_name(Term, Name) :-
	atom(Name), !,
	(   ground(Term) -> true
	;   setup_names(Term, Name-[])
	),
	(   var(Term) -> true
	;   set_name(Term, Name)
	).
fdbg_assign_name(Term, Name) :-
	var(Name),
	get_and_increase_counter(I),
	namesel_to_name('fdvar'-[I], Name),
	set_name(Term, Name).

%% setup_names(+Term, +NameSel):  Assign default names to all variables
%%   in Term, starting naming from NameSel.
setup_names(Var, NameSel) :-
	var(Var), !,
	namesel_to_name(NameSel, Name),
	set_name(Var, Name).
setup_names(List, NameSel) :-
	is_list(List), !,
	setup_list_names(List, NameSel).
setup_names(Compound, NameSel) :-
	compound(Compound), !,
	Compound =.. [_|Args],
	setup_list_names(Args, NameSel).
setup_names(_, _).

%% setup_list_names(+List, +NameSel):  Assign default names to all variables
%%   in List, starting naming from NameSel.
setup_list_names(List, Name-Sel) :-
	(   foreach(T,List),
	    count(I,1,_),
	    param([Name,Sel])
	do  append(Sel, [I], Sel1),
	    setup_names(T, Name-Sel1)
	).

%% set_name(+Term, +Name): Assign the name Name to Term.
set_name(Term, Name) :-
	(   var(Term) -> put_atts(Term, name(Name))
	;   true
	),
	get_nametree(Names0),
	avl_store(Name, Names0, Term, Names),
	put_nametree(Names).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                             fdbg_current_name                             %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_current_name(?Term, ?Name):  Returns the Term registered under Name in
%%   the name-term store, or enumerates all Name-Term pairs.
fdbg_current_name(Term, Name) :-
	get_nametree(Names),
	(   atom(Name) -> avl_fetch(Name, Names, Term)
	;   var(Name)  -> avl_member(Name, Names, Term)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               fdbg_get_name                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_get_name(+Term, -Name):  If Term has a name, then returns it in Name.

%!! There is some problem with the whole issue of naming, because
%% named variables can get instantiated to a term which is the same as
%% some other named ground term, in which case using this call to query
%% the name of a term might find the wrong name first!
fdbg_get_name(Term, Name) :-
	var(Term), !,
	get_atts(Term, name(Name)).
fdbg_get_name(Term, Name) :-
	get_nametree(Names),
	avl_member(Name, Names, Term), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 fdbg_show                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_show(+Constraint, +Actions):  The default builtin visualizer (See
%%   documentation for details.)
fdbg_show(Constraint, Actions) :-
	fdbg_annotate(Constraint, Actions, AnnotC, CVars),
	print(fdbg_output, AnnotC),
	nl(fdbg_output),
	fdbg_legend(CVars, Actions),
	nl(fdbg_output).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                fdbg_guard                                 %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_guard(+Goal, +Constraint, +Actions):  
%%   Another visualizer which does no
%%   input but notifies the user by calling Goal, if a solution is lost
%%   through domain narrowings.  (See documentation for details.)
fdbg_guard(Goal, Constraint, Actions) :-
	fdbg_get_name(Vars-Values, fdbg_guard),
	fdbg_annotate(Vars, Actions, Annotated, _),
	check_domains_for_range(Annotated, Values, BadBefore, BadAfter),
	(   BadBefore \== [] -> true
	;   BadAfter == [] -> true
	;   call(Goal, BadAfter, Constraint, Actions) -> true
	;   true
	).

check_domains_for_range([], [], [], []).
check_domains_for_range([Var|Vars], [Value|Values], Before0, After0) :-
 	Var = fdvar(_,V,VSetNew), !,
	fd_set(V, VSetOld),
	(   fdset_member(Value, VSetOld) -> Before0 = Before
	;   Before0 = [V-Value|Before]
	),
	(   fdset_member(Value, VSetNew) -> After0 = After
	;   After0 = [V-Value|After]
	),
	check_domains_for_range(Vars, Values, Before, After).
check_domains_for_range([V|Vars], [Value|Values], Before0, After0) :-
	(   V=:=Value -> Before0 = Before, After0 = After
	;   Before0 = [V-Value|Before], After0 = [V-Value|After]
	),
	check_domains_for_range(Vars, Values, Before, After).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              fdbg_label_show                              %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_label_show(+Event, +LabelID, +Variable):  The default builtin labeling
%%   tracer.  (See documentation for details.)
fdbg_label_show(start, I, Var) :-
	fdbg_annotate(Var, AVar, _),
	(   AVar = fdvar(Name, _, Set)
	->  fdset_to_range(Set, Range),
	    format(fdbg_output, 'Labeling [~p, <~p>]: starting in range ~p.~n',
		   [I, Name, Range])
	;   format(fdbg_output, 'Labeling [~p, <>]: starting.~n', [I])
	).
fdbg_label_show(fail, I, Var) :-
	(   var(Var)
	->  lookup_or_set_name(Var, Name),
	    format(fdbg_output, 'Labeling [~p, <~p>]: failed.~n~n', [I, Name])
	;   format(fdbg_output, 'Labeling [~p, <>]: failed.~n~n', [I])
	).
fdbg_label_show(step(Step), I, Var) :-
	(   var(Var)
	->  lookup_or_set_name(Var, Name),
	    format(fdbg_output, 'Labeling [~p, <~p>]: ~p~n~n', [I, Name, Step])
	;   format(fdbg_output, 'Labeling [~p, <>]: ~p~n~n', [I, Step])
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                               fdbg_annotate                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_annotate(+Term0, -Term, -Variables):  Replace constraint variables in
%%   Term0 with a compound term containing its name (looked up from the
%%   name-term store), its current fdset, and the variable itself
%%   (fdvar(Name, Var, FDSet)), and give back the result in Term.  Also
%%   collect these compound terms into the list Vars.
fdbg_annotate(Term0, Term, Vars) :-
	fdbg_annotate(Term0, [], _, Term, Vars, Vars, []).

%% fdbg_annotate(+Term0, +Actions, -Term, -Variables):  Replace constraint
%%   variables in Term0 with a compound term containing its name (looked up
%%   from the name-term store), an fdset representing its set after
%%   narrowing with Actions, and the variable itself (fdvar(Name, Var,
%%   FDSetAfter)), and give back the result in Term.  Also collect these
%%   compound terms into the list Vars.
fdbg_annotate(Term0, Actions, Term, Vars) :-
	fdbg_annotate(Term0, Actions, _, Term, Vars, Vars, []).

%% fdbg_annotate(+Term0, +Actions0, -Actions, -Term, +Vars, +Vars0, -Vars1):
%%   This predicate handles the recursion of annotating all subterms of the
%%   given Term.
fdbg_annotate(Atom, Actions, Actions, Term, _, Vars0, Vars1) :-
	atomic(Atom), !,
	Term = Atom,
	Vars1 = Vars0.
fdbg_annotate(Var, Actions0, Actions, Term, Vars, Vars0, Vars1) :-
	var(Var), !,
	(   find_varterm(Var, Vars, Term)
	->  Actions = Actions0,
	    Vars1 = Vars0
	;   fd_set(Var, Set0),
	    get_fdset_incl_actions(Actions0, Actions, Var, Found, Set0, Set),
	    fdbg_annotate_var(Found, Var, Set, Term, Vars0, Vars1)
	).
fdbg_annotate(Compound, Actions0, Actions, Term, Vars, Vars0, Vars1) :-
	% compound(Compound),
	Compound =.. [TermName|Args0],
	annotate_list(Args0, Actions0, Actions, Args, Vars, Vars0, Vars1),
	Term =.. [TermName|Args].

%% find_varterm(+V, +Vs, -Term):  The variable V appears in the second argument
%%   of some element in Vs, which is a list of fdvar/3 compounds.  The term
%%   where is V is found is returned in Term.
find_varterm(V, [fdvar(N,V1,S)|_], Term) :-
	V == V1, !,
	Term = fdvar(N,V,S).
find_varterm(V, [_|Vs], Term) :-
	nonvar(Vs),
	find_varterm(V, Vs, Term).

%% fdbg_annotate_var(+Found, +Var, +Set, -Term, +Vars0, -Vars):  If Var
%%    has a name or is a finite domain variable, or Found is 'found',
%%    then it is replaced by the appropriate fdvar/3 compound.
% fdbg_annotate_var(not_found, Var, Set, Term, Vars0, Vars) :- TENTATIVE
% 	fdset_interval(Set, inf, sup),
% 	\+ fdbg_get_name(Var, _), !,
% 	Term = Var,
% 	Vars = Vars0.
fdbg_annotate_var(_, Var, Set, Term, Vars0, Vars) :-
	lookup_or_set_name(Var, Name),
	Term = fdvar(Name, Var, Set),
	Vars0 = [Term|Vars].

%% annotate_list(+Terms0, +Actions0, -Actions, -Terms, +Vars, +Vars0, -Vars1):
%%   Calls fdbg_annotate/7 for each element of Terms0 and collects the
%%   results in Terms.
annotate_list([T0|T0s], Actions0, Actions, [T1|T1s], Vars, Vars0, Vars2) :-
	fdbg_annotate(T0, Actions0, Actions1, T1, Vars, Vars0, Vars1),
	annotate_list(T0s, Actions1, Actions, T1s, Vars, Vars1, Vars2).
annotate_list([], Actions, Actions, [], _, Vars1, Vars1).


%% get_fdset_incl_actions(+Actions0, -Actions, +Var, -Found, +Set0, -Set):
%%   Apply all narrowing actions appearing in Actions0 concerning Var on Set0,
%%   the result is stored in Set, and the remaining actions are returned in
%%   Actions.
get_fdset_incl_actions([], [], _, not_found, Set, Set).
get_fdset_incl_actions([X in Dom|Actions0], Actions, V, Found, Set0, Set) :-
	V == X, !,
	Found = found,
	range_to_fdset(Dom, Set1),
	fdset_intersection(Set0, Set1, Set2),
	get_fdset_incl_actions(Actions0, Actions, V, _, Set2, Set).
get_fdset_incl_actions([X in_set Set1|Actions0], Actions, V, Found, Set0, Set) :-
	V == X, !,
	Found = found,
	fdset_intersection(Set0, Set1, Set2),
	get_fdset_incl_actions(Actions0, Actions, V, _, Set2, Set).
get_fdset_incl_actions([A = B|Actions0], Actions, V, Found, Set0, Set) :-
	same_pair(A-B, V, Other), !,
	Found = found,
	(   var(Other)		       % If the other side is a variable, then
	->  Actions = [A = B|Actions1] % we might still need this action later.
	;   Actions = Actions1
	),
	fd_set(Other, OSet),
	fdset_intersection(Set0, OSet, Set1),
	get_fdset_incl_actions(Actions0, Actions1, V, _, Set1, Set).
get_fdset_incl_actions([A|Actions0], [A|Actions], V, Found, Set0, Set) :-
	get_fdset_incl_actions(Actions0, Actions, V, Found, Set0, Set).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                fdbg_legend                                %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_legend(+Vars):  Prints a legend of Vars, which is a list of fdvar/3
%%   compound terms.
fdbg_legend(Vars0) :-
	(   foreach(FDV,Vars0),
	    foreach(V-FDV,KL1)
	do  FDV = fdvar(_,V,_)
	),
	keysort(KL1, KL2),
	keyclumped(KL2, KL3),
	(   foreach(_-[Var|_],KL3),
	    foreach(Var,Vars)
	do  true
	),
	do_legend(Vars).

%% fdbg_legend(+Vars, +Actions):  Prints a legend of Vars, and some
%%   conclusions regarding the constraint (exiting, failing, etc.) based on
%%   Actions.
fdbg_legend(Vars, Actions0) :-
	fdbg_transform_actions(Actions0, Vars, Actions),
	fdbg_legend(Vars),
	(   actions_fail(Actions, Fail)
	->  fail_notify(Fail)
	;   actions_notify(Actions)
	).

%% actions_fail(+Actions, -Fail):  Succeeds if Actions action list
%%   contains a fail/0 or a fail/1 term.  Fail is the first matching term.
actions_fail(Actions, Fail) :-
	member(fail, Actions), !,
	Fail = fail.
actions_fail(Actions, fail(Reason)) :-
	member(fail(Reason), Actions).

%% fail_notify(+Fail):  Prints a message about constraint failure.
fail_notify(fail) :-
	format(fdbg_output, '    Constraint failed.~n', []).
fail_notify(fail(Reason)) :-
	format(fdbg_output, '    Constraint failed because ~p is not true.~n',
	       [Reason]).

%% actions_notify(+Actions):  Print some conclusions regarding the
%%   constraint (exiting, calls, etc.) based on the transformed actions
%%   list, Actions.  Note: doesn't handle fail actions.
actions_notify(Actions0) :-
	(   select(exit, Actions0, Actions1)  
	->  write(fdbg_output, '    Constraint exited.'),
	    nl(fdbg_output)
	;   Actions0 = Actions1
	),
	collect_arguments(Actions1, call, Calls, Actions2),
	(   Calls == [] -> true
	;   format(fdbg_output, '    Calling goals: ~p~n', [Calls])
	),
	collect_terms_with_functors(Actions2, fdvar/3, Vars, Actions),
	(   Vars == [] -> true
	;   format(fdbg_output, '  Other variables modified by the actions:~n', []),
	    do_legend(Vars)
	),
	(   Actions == [] -> true
	;   fdbg_annotate(Actions, AnnActions, _),
	    format(fdbg_output, '    Unrecognized actions: ~p~n', [AnnActions])
	).

%% do_legend(+Vars):  Prints a legend of Vars.
do_legend(Vars) :-
	(   foreach(fdvar(Name,Var,Set),Vars)
	do  write(fdbg_output, '    '),
	    do_legend_entry(Name, Var, Set),
	    nl(fdbg_output)
	).

%% do_legend_entry(+Name, +Variable, +Set):  Prints a line of the legend,
%%   calling the user hook, if possible.
do_legend_entry(Name, Variable, Set) :-
	current_output(OOut),
	set_output(fdbg_output),
	call_cleanup(legend_portray(Name, Variable, Set),
		     set_output(OOut)), !.
do_legend_entry(Name, Variable, Set) :-
	fd_set(Variable, Set0),
	print_legend_entry(Name, Set0, Set).

%% print_legend_entry(+Name, +SetBefore, +SetAfter):  Prints a line of the
%%   legend, showing changes in the domain by the current constraint, if
%%   any.
print_legend_entry(Name, Set, Set) :- !,
	fdset_to_range(Set, Range),
	format(fdbg_output, '~p = ~p', [Name, Range]).
print_legend_entry(Name, Set0, Set) :-
	fdset_to_range(Set0, Range0),
	fdset_to_range(Set, Range),
	format(fdbg_output, '~p = ~p -> ~p', [Name, Range0, Range]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                          fdbg_transform_actions                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% fdbg_transform_actions(+Actions0, +Vars, -TransformedActions):  Remove
%%   all actions concerning variables in Vars from Actions0, remove
%%   multiple exit and/or fail commands, and substitute all other
%%   narrowings with an fdvar/3 compound term per variable.
fdbg_transform_actions(Actions0, Vars, Actions) :-
	duplicate_equals(Actions0, Actions1),
	filter_actions(Vars, Actions1, Actions2),
	simplify_actions(Actions2, Actions3),
	sort(Actions3, Actions).

%% duplicate_equals(+Actions0, -Actions):  Each A=B term in Actions0 is
%%   doubled if both A and B are variables, or reversed (B=A) if only B is
%%   a variable.  Other elements are left unchanged.  The result is stored
%%   in Actions.
duplicate_equals([], []).
duplicate_equals([A=B|Actions0], Actions) :-
	var(A),
	var(B), !,
	Actions = [A=B,B=A|Actions1],
	duplicate_equals(Actions0, Actions1).
duplicate_equals([A=B|Actions0], Actions) :-
	var(B), !,
	Actions = [B=A|Actions1],
	duplicate_equals(Actions0, Actions1).
duplicate_equals([Action|Actions0], [Action|Actions]) :-
	duplicate_equals(Actions0, Actions).

%% filter_actions(+Vars, +Actions0, -Actions):  Actions is the list of
%%   actions from Actions0 in which variables from Vars doesn't appear, and
%%   also add 'fail' to action list if one of the variables has an empty
%%   domain.
filter_actions([], Actions, Actions).
filter_actions([Var|Vars], Actions0, Actions) :-
	remove_var_from_actions(Actions0, Var, Actions1),
	filter_actions(Vars, Actions1, Actions).

remove_var_from_actions(Actions0, FDVar, Actions) :-
	FDVar = fdvar(_,Var,Set),
	remove_var_from_actions0(Actions0, Var, Actions1),
	(   empty_fdset(Set)
	->  Actions = [fail(FDVar in_set Set)|Actions1]
	;   Actions = Actions1
	).

%% remove_var_from_actions0(+Actions0, +Var, +Actions):  Actions is the list
%%   of actions from Actions0 in which Var is not the first argument.
remove_var_from_actions0([], _, []).
remove_var_from_actions0([Action|Actions0], Var, Actions) :-
	narrowing_action(Action, V), V == Var, !,
	remove_var_from_actions0(Actions0, Var, Actions).
remove_var_from_actions0([Action|Actions0], Var, [Action|Actions]) :-
	remove_var_from_actions0(Actions0, Var, Actions).

% Not needed any more, because min/max/minmax does not occur any more, so
% narrowing_action/2 can be used instead.
%
% %% var_in_action(+Action, +Var):  Succeeds if Var is the first argument of
% %%   a narrowing action, Action.
% var_in_action(Action, Var) :-
% 	Action =.. [Op, X|_],
% 	member(Op, [in, in_set, =, min, max, minmax]),
% 	X == Var.

%% simplify_actions(+Actions0, -Actions):  Modify and simplify action list.
simplify_actions([], []).
simplify_actions([Action|Actions0], Actions) :-
	narrowing_action(Action, _),
	ground(Action), !,
	(   call(Action)
	->  Actions = Actions1
	;   Actions = [fail(Action)|Actions1]
	),
	simplify_actions(Actions0, Actions1).
%simplify_actions(Actions0, Actions) :- % not good, because a choicepoint is
%	Actions0 = [Action|_],          % made (indifferentiable from clause 1)
simplify_actions([Action|As0], Actions) :-
	Actions0 = [Action|As0],
	narrowing_action(Action, V), !,
	fdbg_annotate(V, Actions0, Var, _),
	Actions = [Var|Actions2],
	remove_var_from_actions(Actions0, Var, Actions1),
	simplify_actions(Actions1, Actions2).
simplify_actions([Action|Actions0], [Action|Actions]) :-
	simplify_actions(Actions0, Actions).

%% narrowing_action(+Action, -Var):  Succeeds if Action is a valid
%%   narrowing action, Var is the affected variable.
narrowing_action(V in _, V).
narrowing_action(V in_set _, V).
narrowing_action(V = _, V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                           debugger_command_hook                           %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% user:debugger_command_hook(+DCommand,?Actions): This predicate is
%%   called for each debugger command SICStus Prolog reads in. The first
%%   argument is the abstract format of the debugger command DCommand, as
%%   returned by the query facility. If it succeeds, Actions is taken as
%%   the list of actions to be done for the given debugger command. If it
%%   fails, the debugger command is interpreted in the standard way.

%% The first clause here overrides the default handling of the & debugger
%% command (when fdbg is loaded), so that the annotated form of FD variables
%% is also printed when listing the variables with blocked goals.
%%
%% The second clause defines the 'A' debugger command to annotate and
%% print the current goal, and print a legend of the variables appearing
%% in it.  If a selector is specified then the subterm specified by it in
%% the current goal is assumed to be an action list, and is taken into
%% account when displaying the legend.  For example at the exit port of
%% dispatch_global/4 the debugger command "A [4]" would use the action
%% list returned by the current constraint (in the fourth argument).
%%
%% The third clause defines 'W', which can be used to add a variable to
%% the list of named terms from inside the debugger.  For example:
%%     7      15 Call: bar(4, [A,B,C]) ? W foo=[2,#2]
%% This would give the name 'foo' to B. (B is the second element
%% of the second argument of the current goal (bar)).
user:debugger_command_hook(blocked_goals(Arg), []) :- !,
	prolog:what_blocked_goals(BGs),
	(   Arg = none
	->  list_blocked(BGs, 0, Goals),
	    print_message(help, blocked(Goals))
	;   nth1(Arg, BGs, Var-Goal)
	->  add_annotation(Var, PVar),
	    print_message(help, bgoal(Arg, PVar, Goal))
	;   print_message(warning, trace_command(blocked_goals)),
	    fail
	).
user:debugger_command_hook(unknown([0'A|Codes],_), []) :-
	debugger_is_on, !,
	execution_state(break_level(0), goal(Module:Goal)),
	prolog:module_qualified_goal(Goal, Module, MGoal),
	(   get_actions(Codes, MGoal, Actions)
        ->  nl, fdbg_show(MGoal, Actions)
        ;   print_message(warning, fdbg(invalid_dbg_command(annotate)))
        ).
user:debugger_command_hook(unknown([0'W|Codes],_), []) :- !,
	execution_state(break_level(0), goal(Module:Goal)),
	prolog:module_qualified_goal(Goal, Module, MGoal),
	append(Codes, [0'.], CodesDot),
	(   protected_read_from_codes(CodesDot, Name = Pos),
	    pick_arg(Pos, MGoal, Term)
	->  fdbg_assign_name(Term, Name)
	;   print_message(warning, fdbg(invalid_dbg_command(name)))
	).

%% list_blocked(+BlockedGoals, +Index, -GoalTerms):  Copied from traceui.pl
%%   but in case of FD variables it inserts the annotated form of the
%%   variable beside the variable in GoalTerms.
list_blocked([], _, []).
list_blocked([V-G|Gs], I0, [bgoal(I, PV, G)|Goals]) :-
	I is I0+1,
	add_annotation(V, PV),
	list_blocked(Gs, I, Goals).

%% add_annotation(+Var, -PrintedVar):  If Var is an FD variable or it has a
%%   name, then PrintedVar is of the form "AnnotatedVar=Var", otherwise it
%%   returns Var by itself.
add_annotation(Var, PrintedVar) :-
	fdbg_annotate(Var,AnnVar,_),
	AnnVar \== Var, !,
	PrintedVar = (AnnVar=Var).
add_annotation(Var, Var).

%% get_actions(+Codes, +Goal, -Actions): If Codes is not empty, then parse
%%   it as a selector and return in Actions the subterm of Goal pointed to
%%   by the given selector, otherwise return an empty list.
get_actions([], _, []) :- !.
get_actions(Codes, Goal, Actions) :-
	  append(Codes, [0'.], CodesDot),
	  protected_read_from_codes(CodesDot, Pos),
	  pick_arg(Pos, Goal, Actions).

%% protected_read_from_codes(+Codes, -Term):  Call read_from_codes/2, and if
%%   there is a syntax error then silently fail.
protected_read_from_codes(Codes, Term) :-
        %!! Is it OK to assume the functor syntax_error/5?
	on_exception(error(_ISO,syntax_error(_,_,_,_,_)),  %  [PM] 4.1.0 ISO-ized SPRM 11617
		     read_from_codes(Codes, Term),
		     fail).

%!! Some better error reporting should be done for 'A' and 'W' in the case
%%  that this predicate fails, because the current error message "invalid
%%  arguments" is a bit misleading.

%% pick_arg(+Selector, +Term, -Var):  Var is the subterm of Term pointed to
%%   by Selector.
pick_arg([], V, V).
pick_arg([P|Ps], T, V) :-
	nonvar(T),
	(   P = #N -> safe_nth(N, T, SubT)     % either list...
	;   T =.. [_|Args], nth1(P, Args, SubT) % or compound
	),
	pick_arg(Ps, SubT, V).

%% safe_nth(+N, +List, -Element):  Element is the Nth element of List, but
%%   an open end is not substituted with an empty [_|_] compound.
safe_nth(_, X, _) :- var(X), !, fail.
safe_nth(1, [H|_], V) :- !, V = H.
safe_nth(N, [_|T], V) :- N > 1, N1 is N-1, safe_nth(N1, T, V).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         name handling predicates                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% lookup_or_set_name(+Term, -Name): Return in Name the name avliated
%%   with Term, or if it does not have a name, assign a new one to it
%%   and return it in Name.
lookup_or_set_name(Term, Name) :-
	fdbg_get_name(Term, Name), !.
lookup_or_set_name(Term, Name) :-
	fdbg_assign_name(Term, Name).


%% get_nametree(-Names):  Names is the tree storing the names of nonvar terms.
get_nametree(Names) :-
	empty_avl(Empty),
	create_or_get_private(fdbg, Empty, Names).

%% put_nametree(+Names):  Make Names the current name-tree.
put_nametree(Names) :-
	create_or_put_private(fdbg, Names).


%% namesel_to_name(+Basename-Selector, -Name):  Convert the
%% Basename-Selector pair: an atom and a selector to another atom, Name.
namesel_to_name(Name0-Sel0, Name) :-
	namesel_to_name(Sel0, Name0, Name).

%% namesel_to_name(+SelectorList, +Name0, -Name):  Generate an atom, Name
%%   by appending SelectorList separated by underscores to Name0.
namesel_to_name([], Name, Name).
namesel_to_name([I|Is], Name0, Name) :-
	number_codes(I, Ic),
	atom_codes(Ia, [0'_|Ic]),
	atom_concat(Name0, Ia, Name1),
	namesel_to_name(Is, Name1, Name).


%% get_and_increase_counter(-Counter):  Return the next free value of the
%%   fdvar counter.
get_and_increase_counter(I) :-
	(   bb_get(fdvar_counter, I) -> true
	;   I = 1
	),
	I1 is I+1,
	bb_put(fdvar_counter, I1).

%% Backtrackable version of the above predicate (using the global private
%% for storing the counter).
%get_and_increase_counter(I) :-
%	create_or_get_private(noname_counter, 1, I),
%	I1 is I+1,
%	create_or_put_private(noname_counter, I1).


%% verify_attribute(-Var, +Other, -Goals): If Var has a name/1 attribute
%%   and Other is a variable without this attribute then copy it from Var
%%   to Other.
verify_attributes(Var, Other, []) :-
	get_atts(Var, name(Name)),
	var(Other),
	get_atts(Other, -name(_)), !,
	put_atts(Other, name(Name)).
verify_attributes(_, _, []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                         miscellanious predicates                          %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% call_goals_with_args(+Goals, +Args):  Goals is a list of module
%%   specified goals.  Each goal is extended with Args as an argument list
%%   and then it is called.
call_goals_with_args([], _).
call_goals_with_args([Goal|Goals], Args) :-
	Metacall =.. [call,Goal|Args],
	succeed(Metacall),
	call_goals_with_args(Goals, Args).

%% suceed(+Goal):  Calls Goal and cuts off multiple choices.  Succeeds even
%%   if Goal fails.
succeed(Goal) :- call(Goal), !.
succeed(_).

%% same_pair(+Pair, +Var, -Other):  Succeeds if Var appears on either side
%%   of Pair, and A-B-like compound.  Other is the other half of Pair.
same_pair(A-B, V, Other) :-
	A == V, !,
	Other = B.
same_pair(A-B, V, Other) :-
	B == V, !,
	Other = A.

%% get_pred_inv(+Pred, -Inv):  Inv is the invocation number of predicate
%%   Pred.
get_pred_inv(Pred, I) :-
	execution_state(inv(I0)),
	get_pred_inv(I0, Pred, I).

%% get_pred_inv(+Inv0, +Pred, -Inv):  Inv is the invocation number of
%%   predicate matching Pred, Inv must be smaller than or equal to Inv0.
get_pred_inv(0, _, '?') :- !.
get_pred_inv(I0, Pred, I) :-
	execution_state(inv(I0), pred(Pred)), !,
	I = I0.
get_pred_inv(I0, Pred, I) :-
	I1 is I0 - 1,
	get_pred_inv(I1, Pred, I).

%% collect_arguments(+List0, +Name, -Args, -List):  Each term with a
%%   functor Name/1 is removed from the list List0, their arguments are
%%   concatenated into the list Args.  List is a list of the remaining
%%   elements.
collect_arguments([], _, [], []).
collect_arguments([H|List0], Name, Args, List) :-
	(   H =.. [Name, Arg]
	->  Args = [Arg|Args1],
	    List = List1
	;   Args = Args1,
	    List = [H|List1]
	),
	collect_arguments(List0, Name, Args1, List1).

%% collect_terms_with_functors(+List0, +Functor, -Terms, -List):  Each term
%%   with a functor Functor is removed from the list List0 and gets
%%   collected into the list Terms.  List is a list of the remaining
%%   elements.
collect_terms_with_functors([], _, [], []).
collect_terms_with_functors([Term|List0], Name/Arity, Terms, List) :-
	(   functor(Term, Name, Arity)
	->  Terms = [Term|Terms1],
	    List = List1
	;   Terms = Terms1,
	    List = [Term|List1]
	),
	collect_terms_with_functors(List0, Name/Arity, Terms1, List1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        option handling predicates                         %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% handle_options(+Options):  Handle and store options appearing in
%%   Options, which is either a list of options or a single option.
handle_options(Module:Options) :-
	must_be(Options, proper_list(callable), fdbg_on(Module:Options), 1),
	%% Handle 'file' option
	handle_file_option(Options, Options2),
	%% Handle 'constraint_hook' and 'labeling_hook' option(s)
	(   select(no_constraint_hook, Options2, Options3) -> true
	;   memberchk(constraint_hook(_), Options2)
	->  handle_hooks(constraint_hook, Module, Options2, Options3)
	;   handle_hooks(constraint_hook, Module,
			 [constraint_hook(fdbg:fdbg_show)], _),
	    Options3 = Options2
	),
	(   select(no_labeling_hook, Options3, Options4) -> true
	;   memberchk(labeling_hook(_), Options3)
	->  handle_hooks(labeling_hook, Module, Options3, Options4)
	;   handle_hooks(labeling_hook, Module,
			 [labeling_hook(fdbg:fdbg_label_show)], _),
	    Options4 = Options3
	),
	%% Signal other unhandled options
	(   Options4 == [] -> true
	;   print_message(warning, fdbg(unhandled_options(Options4)))
	).

%% handle_file_option(+Options1, -Options2):  Select and process the first
%%   option concerning the output redirection from the option list Options1,
%%   and return the rest of the options in Options2.
handle_file_option(Options1, Options2) :-
	(   select(file(temp(Temp), Mode), Options1, Options2)
	->  open(temp(Temp), Mode, Stream, [if_exists(generate_unique_name)]),
	    bb_put(fdbg_logfile, Stream/close)
	;   select(file(Filename, Mode), Options1, Options2)
	->  open(Filename, Mode, Stream),
	    bb_put(fdbg_logfile, Stream/close)
	;   select(socket(Host, Port), Options1, Options2)
	->  fdbg_socket_open(Host, Port, Stream),
	    bb_put(fdbg_logfile, Stream/close)
	;   select(stream(Stream), Options1, Options2)
	->  bb_put(fdbg_logfile, Stream/keep_open)
	;   Options1 = Options2,
	    current_output(Stream),
	    bb_put(fdbg_logfile, Stream/keep_open)
	),
        %% [PM] 4.0 use new, semi-sane, interface to alias feature
        prolog:add_stream_alias(Stream, fdbg_output),
	%% %% temporary hack [MC]
	%% stream_code(Stream, Code),
	%% prolog:assert_stream_properties([fdbg_output], [], Code),
        true.

%% handle_hooks(+OptionName, +Module, +Options0, -Options):  OptionName
%%   options are taken from the options list Options0, the rest of the
%%   options are returned in Options.  Then, each option taken is module
%%   expanded and their list is stored under OptionName.
handle_hooks(OptName, Module, Options0, Options) :-
	collect_arguments(Options0, OptName, Goals0, Options),
	(   foreach(MG,Goals0),
	    foreach(M:G,Goals),
	    param(Module)
	do  (MG = M:G -> true ; M=Module, G=MG)
	),
	bb_put(fdbg:OptName, Goals).

%% SzT 2001.03.07
%% fdbg_socket_open(+Host, +Port, -Stream):  Open a socket connection to Host:Port
%%   and return the resulting Stream.  If Host is the atom 'current_host' then
%%   connect to a server on the local machine.

fdbg_socket_open(Host0, Port, Stream) :-
	get_host(Host0, Host),
	%% socket('AF_INET', Socket),
	%% socket_connect(Socket, 'AF_INET'(Host, Port), Stream),
	%% no_buffering(Stream).

        %% [PM] 4.0 FIXME: add autoflush options or ensure all writes do flush
        socket_client_open(Host:Port, Stream, [type(text)]),
	format(user_error, 'Socket stream: ~p, Host: ~p:~p~n', [Stream, Host, Port]).

%% SzT 2001.03.07
%% get_host(+Host0, -Host):  If Host is the atom 'current_host' then replace
%%   it with the name of the current machine.
get_host(H0, H) :-
	(   var(H0)
	;   H0 == current_host
	), !,
	current_host(H).
get_host(H, H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        private handling predicates                        %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% create_or_get_private(+PrivName, +DefValue, -Value):  The value stored in
%%   the global private field of the debugger under the name PrivName is Value.
%%   If no entry exists yet under that name then a new one is created and is
%%   initialized with value DefValue.
create_or_get_private(Name, DefValue, Value) :-
	get_global_private(Name, Mut),
	(    mutable(Mut)
	->   get_mutable(Value, Mut)
	;    Value = DefValue, create_mutable(Value, Mut)
	).

%% create_or_put_private(+PrivName, +Value):  Value is stored in the global
%%   private field of the debugger under the name PrivName.  If it already
%%   exists its value is overwritten.
create_or_put_private(Name, Value) :-
	get_global_private(Name, Mut),
	(    mutable(Mut)
	->   update_mutable(Value, Mut)
	;    create_mutable(Value, Mut)
	).

%% get_global_private(+Name, -Value):  If the entry with functor Name/1 
%%   already exists in the (open ended) list stored in the global private
%%   field of the debugger, then its argument is unified with Value.
%%   Otherwise a new entry is created with the given Name and its 
%%   (uninstantiated) argument is unified with Value.
get_global_private(Name, Value) :-
	execution_state(break_level(0), private(Priv)),
	Mem =.. [Name, Value],
	memberchk(Mem, Priv).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                                 portrays                                  %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Portrayal of the fdvar/3 compound
user:portray(fdvar(Name, Var, Set)) :-
	fdvar_portray(Name, Var, Set), !.
user:portray(fdvar(Name, _, _)) :- !,
	format('<~p>', [Name]).

%% Portrayal of the printing of a labeling step
user:portray('$labeling_step'(Var, Rel, X, Mode)) :- !,
	(   var(Var)
	->  lookup_or_set_name(Var, Name),
	    format('~p: <~p> ~p ~p', [Mode, Name, Rel, X])
	;   format('~p: ~p ~p ~p', [Mode, Var, Rel, X])
	).

%% Suppress printing messages for the new advice points added in fdbg_on
user:portray_message(informational,
		     breakp(bp(advice,conditional(Pred),_),_,_)) :- 
	member(Pred, [clpfd:'$fd_eval_indexical'/3,
		      clpfd:dispatch_global_fast/5,
		      clpfd:prune_and_propagate/2,
		      clpfd:prune_and_propagate_chk/2,
		      clpfd:propagate_interval/3,
		      clpfd:propagate_interval_chk/3,
		      clpfd:domain/2,
		      clpfd:verify_attributes/3,
		      clpfd:fdbg_start_labeling/2,
		      clpfd:fdbg_labeling_step/2,
		      clpfd:labeling_singleton/3,
		      clpfd:labeling_min/3,
		      clpfd:labeling_max/3]), !.

%% FDBG messages 
:- multifile user:generate_message_hook/3.

user:generate_message_hook(fdbg(state(OnOff))) --> !,
	['The clp(fd) debugger is switched ~w'-[OnOff], nl].
user:generate_message_hook(fdbg(state(When, State))) --> !,
	['The clp(fd) debugger is ~w switched ~w!'-[When, State], nl].
user:generate_message_hook(fdbg(unhandled_options(Options))) --> !,
	['There were some unhandled options: ~p'-[Options], nl].
user:generate_message_hook(fdbg(file_mode(Mode))) --> !,
	['~q is not a valid logfile open mode'-[Mode], nl].
user:generate_message_hook(fdbg(invalid_dbg_command(Command))) --> !,
	['Invalid argument, usage: '-[]],
	dbg_command_usage(Command).

dbg_command_usage(annotate) -->
	['"A [<selector>]"   (e.g. "A [1,#3,2]")'-[], nl].
dbg_command_usage(name) -->
	['"W <name>=<selector>"   (e.g. "W foo=[2,#2]")'-[], nl].


% This turns debug mode on, in which arithmetical constraints are
% transformed to global constraints, and the action lists of
% built-in global constraints only contain documented items.
:- initialization
	fd_flag(debug, _, on).
