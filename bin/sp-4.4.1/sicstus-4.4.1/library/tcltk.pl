/* Copyright(C) 1995, Swedish Institute of Computer Science */

%   File       : tcltk.pl
%   Author     : Stefan Andersson
%   Purpose    : Tcl/Tk interface

:- module(tcltk, [
	tcl_new/1,
	tcl_delete/1,
	tcl_eval/3,
	tcl_event/3,
	tk_new/2,
	tk_main_window/2,
	tk_destroy_window/1,
	tk_make_window_exist/1,
	tk_num_main_windows/1,
	tk_do_one_event/0,
	tk_do_one_event/1,
	tk_next_event/2,
	tk_next_event/3,
	tk_main_loop/0,
	tk_terminal/5
	       ]).
:- use_module(library(types), [
	must_be/4,
	illarg/3
	]).


% -----------------------------------------------------------------------

%% called from tcl.c
call_from_tcl(StreamCode, Interp) :-
	stream_code(Stream, StreamCode),
	read_term(Stream, Goal, [variable_names(Vars)]),
	once(user:Goal),			% default module "user"
	(   foreach(Var=Val,Vars),
	    param(Interp)
	do  '$tcl_add_result'(Interp, Var, Val)
	).

% -----------------------------------------------------------------------

%% called from tcl.c
read_sc(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	read(Stream, Term).

write_sc(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	write(Stream, Term),
	flush_output(Stream).


writeq_sc(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	writeq(Stream, Term),
	flush_output(Stream).

write_canonical_sc(StreamCode, Term) :-
	stream_code(Stream, StreamCode),
	write_canonical(Stream, Term),
	flush_output(Stream).

format_sc(StreamCode, Fmt, Args) :-
	stream_code(Stream, StreamCode),
	format(Stream, Fmt, user:Args), % default module "user"
	flush_output(Stream).

%% [PM] 4.1.3 SPIDER/xref
%% These are all called from tcl.c
:- public call_from_tcl/2, read_sc/2, write_sc/2, writeq_sc/2, write_canonical_sc/2, format_sc/3.

% -----------------------------------------------------------------------
% Tk

tk_new(Options, Interp) :-
	ErrGoal = tk_new(Options, Interp),
	must_be(Options, proper_list, ErrGoal, 1),
	(   foreach(Option,Options),
	    fromto('',App0,App1,AppName),
	    fromto('',Disp0,Disp1,Display),
	    fromto(0,Flags0,Flags1,Flags),
	    param(ErrGoal)
	do  (   callable(Option),
		tk_option(Option, App0, App1, Disp0, Disp1, Flags0, Flags1) -> true
	    ;   illarg(domain(term,tk_option), ErrGoal, 1)
	    )
	),
	tcl_new(Interp),
	'$tk_new'(Interp, AppName, Display, Flags).

tk_option(top_level_events, App0, App, Disp0, Disp, Flags0, Flags) :-
	App=App0, Disp=Disp0,
	Flags is Flags0 \/ 0x0001.   % PROLOG_TCL_NEW_FLAG_TOP_LEVEL_EVENTS
tk_option(disable_aqua_hack, App0, App, Disp0, Disp, Flags0, Flags) :- % [PM] 3.10.1
	App=App0, Disp=Disp0,
	Flags is Flags0 \/ 0x0002.   % PROLOG_TCL_NEW_FLAG_DISABLE_AQUA_HACK
tk_option(enable_aqua_hack, App0, App, Disp0, Disp, Flags0, Flags) :- % [PM] 3.10.1
	App=App0, Disp=Disp0,
	Flags is Flags0 \/ 0x0004.  % PROLOG_TCL_NEW_FLAG_ENABLE_AQUA_HACK
tk_option(name(Name), _, App, Disp0, Disp, Flags0, Flags) :-
	App=Name, Disp=Disp0, Flags=Flags0.
tk_option(display(Display), App0, App, _, Disp, Flags0, Flags) :-
	App=App0, Disp=Display, Flags=Flags0.

% -----------------------------------------------------------------------
% Handle one event

% [PM] 4.3 FIXME: continue getting rid of all hardwired magic numbers. Note that
% the DoOneEvent flags got twice as large in Tcl 7.5 (Tk 4.1) and this
% code is older than this. In particular, all event flags are > 1, which
% this code obviously does not know about.


tk_do_one_event :-
        is_option(tk_dont_wait, PROLOG_TCL_DONT_WAIT),
        is_option(tk_all_events, PROLOG_TCL_ALL_EVENTS),
        Flags is PROLOG_TCL_ALL_EVENTS \/ PROLOG_TCL_DONT_WAIT,
	'$tk_do_one_event'(Flags, 0, 1).

tk_do_one_event(Options) :-
	tk_do_one_event1(Options).

tk_do_one_event1(BitMask) :- integer(BitMask), !,
	'$tk_do_one_event'(0, BitMask, 1).
tk_do_one_event1(Options) :-
	ErrGoal = tk_do_one_event(Options),
	must_be(Options, proper_list, ErrGoal, 1),
	(   foreach(Option,Options),
	    fromto(0,Opt1,Opt3,Opt),
	    param(ErrGoal)
	do  (   nonvar(Option),
		is_option(Option, Opt2) -> Opt3 is Opt1 \/ Opt2
	    ;   illarg(domain(term,tk_event_option), ErrGoal, 1)
	    )
	),
	'$tk_do_one_event'(Opt, 0, 1).



% [PM] 4.3 see tk.c. No longer depend on the values used in tcl.h/tk.h.
is_option(tk_dont_wait,     0x0002). % PROLOG_TCL_DONT_WAIT
is_option(tk_x_events,      Bits) :-  is_option(tk_window_events, Bits).
is_option(tk_window_events, 0x0004). % PROLOG_TCL_WINDOW_EVENTS
is_option(tk_file_events,   0x0008). % PROLOG_TCL_FILE_EVENTS
is_option(tk_timer_events,  0x0010). % PROLOG_TCL_TIMER_EVENTS
is_option(tk_idle_events,   0x0020). % PROLOG_TCL_IDLE_EVENTS
is_option(tk_all_events,    0x0001). % PROLOG_TCL_ALL_EVENTS



% -----------------------------------------------------------------------
% Process all windows-events until there is a Prolog event 

tk_next_event(Interp, Event) :-
        is_option(tk_all_events, PROLOG_TCL_ALL_EVENTS),
	'$tk_do_one_event'(Interp, PROLOG_TCL_ALL_EVENTS, 0, Event0),
	tk_num_main_windows(WindowsLeft),
        BitMask = 0,
	tk_next_event_cont(WindowsLeft, Interp, PROLOG_TCL_ALL_EVENTS, BitMask, Event0, Event).

tk_next_event(BitMask, Interp, Event) :-
	integer(BitMask), !,
        % [PM] 4.3 Presumably this tries to clear the TCL_DONT_WAIT bit, and nothing else.
	BitMask1 is BitMask /\ 0xfd,			% always wait
        Prolog_flags = 0,
	tk_next_event(Prolog_flags, BitMask1, Interp, Event).
tk_next_event(Options, Interp, Event) :-
        must_be(Options, proper_list, tk_next_event(Options,Interp,Event), 1),
	(   foreach(Option,Options),
	    fromto(0,Opt1,Opt3,Opt)
	do  nonvar(Option),
            is_option(Option, Opt2),
	    Opt3 is Opt1 \/ Opt2
	), !,
        Prolog_flags = Opt,
        BitMask = 0,
	tk_next_event(Prolog_flags, BitMask, Interp, Event).
tk_next_event(Options, Interp, Event) :-
	illarg(domain(list(atom),tk_next_event_options), tk_next_event(Options,Interp,Event), 1).

tk_next_event(Prolog_flags, BitMask, Interp, Event) :-
        '$tk_do_one_event'(Interp, Prolog_flags, BitMask, Event0),
        tk_num_main_windows(WindowsLeft),
        tk_next_event_cont(WindowsLeft, Interp, Prolog_flags, BitMask, Event0, Event).


tk_next_event_cont(0, _Interp, _Prolog_flags, _BitMask, _Event0, Event) :- !, Event=[].
tk_next_event_cont(WindowsLeft, Interp, Prolog_flags, BitMask, Event0, Event) :-
        WindowsLeft > 0,
	(   Event0 = [] ->				% no Prolog event
	    tk_next_event(Prolog_flags, BitMask, Interp, Event)
	;   Event = Event0
	).


% -----------------------------------------------------------------------
% Defines TkMainLoop in Prolog to enable Prolog control-C
% interrupt while executing Tk

tk_main_loop :-
	tk_num_main_windows(N),
	(   N > 0 ->
            is_option(tk_all_events, PROLOG_TCL_ALL_EVENTS),
	    '$tk_do_one_event'(PROLOG_TCL_ALL_EVENTS, 0, _),
	    tk_main_loop
        ;   true
	).


% -----------------------------------------------------------------------

/* Error printing */

:- multifile user:generate_message_hook/3.

user:generate_message_hook(tcl_error(Goal,Message)) --> !,
	generate_message_hook(Goal, Message, 'TCL').
user:generate_message_hook(tk_error(Goal,Message)) --> !,
	generate_message_hook(Goal, Message, 'TK').

% should be combinaed with a portray/1 that recognizes
% and prints "strings"
generate_message_hook(Goal, Message, Package) -->
	[Package-[], nl,
	 'goal:  '-[], write_term(Goal), nl,
	 '~s'-[Message], nl].


% -----------------------------------------------------------------------

tk_terminal(Interp, TextWidget, InStream, OutStream, ErrStream) :-
	absolute_file_name(library('tkterm.tcl'),TkTermSrc),
	'$tk_term'(Interp, TkTermSrc, TextWidget, InCode, OutCode, ErrCode),
        stream_code(InStream, InCode),
        stream_code(OutStream, OutCode),
        stream_code(ErrStream, ErrCode).


% -----------------------------------------------------------------------

foreign(tcl_new, tcl_new(-term)).
foreign(tcl_delete_interp, tcl_delete(+term)).
foreign(tcl_eval, tcl_eval(+term,+term,-term)).
foreign(tcl_event, tcl_event(+term,+term,-term)).
foreign(tcl_add_result, '$tcl_add_result'(+term,+term,+term)).

foreign(tk_new, '$tk_new'(+term,+string,+string,+integer)).
foreign(tk_destroy_window, tk_destroy_window(+term)).
foreign(tk_make_window_exist, tk_make_window_exist(+term)).
foreign(tk_main_window, tk_main_window(+term,-term)).
foreign(tk_do_one_event2, '$tk_do_one_event'(+integer,+integer,[-integer])).
foreign(tk_do_one_event4, '$tk_do_one_event'(+term,+integer,+integer,-term)).
foreign(tk_num_main_windows, tk_num_main_windows(-term)).
foreign(tk_term, '$tk_term'(+term,+string,+codes,-address('SP_stream'),-address('SP_stream'),-address('SP_stream'))).

foreign_resource(tcltk, [
	init(tk_initialize),
	deinit(tk_deinitialize),
	tcl_new,
	tcl_delete_interp,
	tcl_eval,
	tcl_event,
	tcl_add_result,
	tk_new,
	tk_destroy_window,
	tk_make_window_exist,
	tk_main_window,
	tk_do_one_event2,
	tk_do_one_event4,
	tk_num_main_windows,
	tk_term
			    ]).	

:- load_foreign_resource(library(system(tcltk))).
