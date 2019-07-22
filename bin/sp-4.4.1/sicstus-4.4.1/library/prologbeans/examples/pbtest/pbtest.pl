%%% -*- Mode: Prolog; Module: pbtest; -*-
%%% pbtest.pl
%%% Copyright (c) 2003 SICS AB. All rights reserved.
%%% -----------------------------------------------------------------
%%%
%%% Author  : Joakim Eriksson
%%% Purpose : PrologBeans example
%%%

:- module(pbtest,[main/0,main/1,my_predicate/2]).

:- use_module(library(prologbeans)).
:- use_module(library(lists),[reverse/2]).
%% [PD] 4.0.2 For testing attributed vars
:- use_module(library(clpfd)).
:- use_module(library(system), [sleep/1]).

%% Register acceptable queries and start the server (using default port)
main(Port) :-
    register_query(evaluate(C,P), my_predicate(C, P)),
    register_query(reverse(C,P), lists:reverse(C, P)),
    register_query('devel\xf6\pers'(Dev), developers(Dev)), % 0xf6 = o umlaut
    register_query(send_receive(L1,L2), identity(L1,L2)),
    register_query(newVar(X,N,M), X in N..M), %% [PD] 4.0.2 attributed vars
    register_query(logging, start_logging),
    register_query(shutdown, shutdown_server),
    register_query(sleep(S), my_sleep(S)),
    register_event_listener(server_started, server_started_listener),
    register_event_listener(server_shutdown, server_shutdown_listener),
    register_event_listener(session_started(SessionID), session_started_listener(SessionID)),
    register_event_listener(session_ended(SessionID), session_ended_listener(SessionID)),
    %% Note: By specifying an uninstantiated port we will let the OS assign an unused port.
    %%       The port used will be obtained by server_started_listener below.
    start([port(Port)]),
    halt.

main :-
    main(_Port).


%% Event listener callbacks
server_started_listener :-
    get_server_property(port(Port)),
%    format(user_error, 'port:~w\n', [Port]),
    format(user_error, 'port:~w~n', [Port]), % [PD]
    flush_output(user_error).

server_shutdown_listener :-
   format(user_error, '~Npbtest.pl: Shutdown server~n', []),
   flush_output(user_error).

session_started_listener(SessionID) :-
   format(user_error, '~Npbtest.pl: Starting session ~q~n', [SessionID]),
   flush_output(user_error).
session_ended_listener(SessionID) :-
   format(user_error, '~Npbtest.pl: Ending session ~q~n', [SessionID]),
   flush_output(user_error).


%% In this case we know that we have received a list of characters
%% that needs to be converted into an expression!
my_predicate(CharCodes, P) :-
    prologbeans:pb_read_term_from_codes(CharCodes, X, []),
    P is X.

%% [PM] 4.1.3 for testing timeout behavior
my_sleep(Seconds) :-
        sleep(Seconds).
        

% get all the answers from the "developer" predicate
developers(Dev) :-
    findall(D, developer(D), Dev).

identity(L,L).

shutdown_server :-
    format(user_error, '~Npbtest.pl: shutting down~n', []),
    shutdown(now).

start_logging :-
    format(user_error, '~Npbtest.pl: starting logging~n', []),
    retractall(prologbeans:logging_),
    asserta(prologbeans:logging_).

%% The developers
developer('Joakim').
developer('Niclas').
developer('Per').
%% [PD] 4.0 Now ISO syntax
developer('\345\\344\\366\\305\\304\\326\'). % [PD] 3.12.1 Test non-ASCII characters
