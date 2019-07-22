%%% Copyright (c) 2003 SICS AB. All rights reserved.
%%% -----------------------------------------------------------------
%%%
%%% PROLOGBEANS
%%%
%%% Original Authors  : Joakim Eriksson, Niclas Finne
%%%
%%% A prologbeans server that is intended to be used for communication
%%% between Java (or other applications) and a prolog application.
%%% The prolog application needs to start up the server and
%%% register acceptable queries with associated goals to call.
%%%

:- module(prologbeans, [start/0,start/1, shutdown/0, shutdown/1,
			register_query/2, register_query/3, unregister_query/1,
                        register_event_listener/2,
                        register_event_listener/3,
                        unregister_event_listener/1,
			get_server_property/1,
			session_remove/1, session_get/4,
			session_put/3,
                        
                        %% [PM] 4.2.1 NOTE: The predicates prefixed with pb2_ are not part of the official API and will change without notice.
                        pb2_register_query/3,
                        pb2_unregister_query/1,
                        
                        pb2_once_peer/5,
                        pb2_call_peer/5,

                        pb2_call_peer_async/2,
                        pb2_call_peer_async_more/1,
                        pb2_call_peer_async_result/4,
                        pb2_call_peer_async_done/1,
                        

                        pb2_call_peer_first/5,
                        pb2_call_peer_more/4,
                        pb2_call_peer_done/1,
                        
                        pb2_findall_peer/6,
                        pb2_client_open/3,
                        pb2_client_close/1
		       ]).

:- use_module(library(lists), [
        select/4
        ]).
:- use_module(library(codesio), [
	read_term_from_codes/3
	]).
:- use_module(library(system), [
        now/1
        ]).


:- use_module(library(fastrw), [
        fast_read/2,
        fast_write/2
        ]).

:- use_module(library(sockets), [
        socket_client_open/3
        ]).

:- use_module(library(prologbeansserver), [
        init/0,
        new_thread/3,
        run/1,
        open_socket/4,
        stop/0,
        new_stream/2,
        has_open_connections/0,
        pb_input_stream/2,
        pb_output_stream/2,
        close_stream/3
        ]).


/*- [PM] 4.2.1 now uses callback/2

% for xref; these are metacalled from prologbeansserver
:- public
        connection/2,
        connection_closed/1,
        data_received/1,
        resume/0.
*/

:- meta_predicate register_query(+,0).
:- meta_predicate register_query(+,0,+).
:- meta_predicate register_event_listener(+,0).
:- meta_predicate register_event_listener(+,0,-).

% [PM] 4.3 Tell SPIDER about arguments that are really meta-closures but
%      where we can not use proper meta_predicate declarations.
:- call_query(0,+,-) is pseudo_meta_predicate.
:- call_event_robust(+,0) is pseudo_meta_predicate.
:- call_event_failure(+,0) is pseudo_meta_predicate.
:- call_event_error(+, +, 0) is pseudo_meta_predicate.

% [PM] 4.2.1 These are all (re)set by start/1 so should be volatile.
:- dynamic session_info/2.
:- volatile session_info/2.
:- dynamic session_data/2.
:- volatile session_data/2.
:- dynamic session_timeout/1.
:- volatile session_timeout/1.
:- dynamic session_gc_timeout/1.
:- volatile session_gc_timeout/1.
:- dynamic session_last_gc/1.
:- volatile session_last_gc/1.
:- dynamic accepted_host/1.
:- volatile accepted_host/1.
:- dynamic shutdown_mode/1.
:- volatile shutdown_mode/1.
:- dynamic server_port/1.
:- volatile server_port/1.

% [PM] 4.2.1 These could make sense to persist in a saved state, so tell SPIDER that they are intentionally not volatile.
:- query_db/3 is not_volatile.
:- event_listener_db/2 is not_volatile.
:- dynamic query_db/3. 
:- dynamic event_listener_db/2.

:- dynamic logging_/0.
:- volatile logging_/0.
logging :- logging_, !.

:- meta_predicate(log(+,:)).
log(Fmt, Args) :-
        logging,
        !,
        format(user_error, Fmt, Args).
log(_Fmt, _Args).


pb_read_term_from_codes(Chars, Term, Options) :-
    read_term_from_codes(Chars, Term, Options).


%% ------------------------------------------------------
%% start-up predicates
%% ------------------------------------------------------
start :-
    start([]).

start(Options) :-
    init,
    retractall(accepted_host(_)),
    get_option(accepted_hosts(Hosts), Options, accepted_hosts(['127.0.0.1'])),
    set_hosts(Hosts),
    get_option(session_timeout(Timeout), Options, session_timeout(0)),
    session_set_timeout(Timeout),
    get_option(session_gc_timeout(GCTimeout), Options, session_gc_timeout(0)),
    session_set_gc_timeout(GCTimeout),
    get_option(port(Port), Options, port(8066)),
    retractall(server_port(_)),
    %% [PM] 4.2.1 FIXME: port(none) is not documented. For use with initial_streams/1.
    ( Port == none ->
      % If no port then there is nothing to do unless some (initial_streams/1-) stream is open
      shutdown(no_connections)
    ; otherwise ->
      set_socket_options(SocketOptions, Options),
      open_socket(Port, SocketOptions)
    ),
    assert(server_port(Port)), % [PM] 4.2.1 may be 'none'
    server_thread_name(Thread),
    new_thread(Thread, callback, []),
    call_server_listeners(server_started),
    %% [PM] 4.2.1 initial_streams/1 is not documented. For debugging.
    get_option(initial_streams(InitialStreams), Options, initial_streams(InitialStreams)),
    ( foreach(SS, InitialStreams),
      foreach(Thread-SS, InitialThreadStreams),
      param(Thread)
    do
      true
    ),
    run(InitialThreadStreams),
    call_server_listeners(server_shutdown).

server_thread_name(prologbeans).

open_socket(Port, SocketOptions) :-
    server_thread_name(Thread),
    StreamOptions = [], % I.e. [type(binary)]
    open_socket(Port, Thread, SocketOptions, StreamOptions).

shutdown :-
    shutdown(now).

shutdown(now) :-
    stop.
shutdown(no_sessions) :-
    assert(shutdown_mode(no_sessions)).
shutdown(no_connections) :-
    assert(shutdown_mode(no_connections)).

set_hosts([]).
set_hosts([Host|Hosts]) :-
    assert(accepted_host(Host)),
    set_hosts(Hosts).

get_option(Option, OptionList, DefaultValue) :-
    (  memberchk(Option, OptionList) ->
	true
    ;  otherwise ->
	Option = DefaultValue
    ).

%% [PD] 4.0.0beta1 If no port is given when calling prologbeans:start/1,
%%                 set the reuse adress bit.
set_socket_options(SocketOptions, Options) :-
    memberchk(port(_), Options), !,
    SocketOptions = [].
set_socket_options([reuseaddr(true)], _Options).

%% ------------------------------------------------------
%% 'Hooks' for the communication server
%% ------------------------------------------------------


%% handle callback from prologbeansserver
callback(initial_state, _CallbackOptions, _InState, OutState) :-
    initial_state(OutState).
callback(connection(Host, PBStream), _CallbackOptions, InState, OutState) :-
    connection(Host, PBStream, InState, OutState).
callback(connection_closed(PBStream), _CallbackOptions, InState, OutState) :-
    connection_closed(PBStream, InState, OutState).
callback(data_received(PBStream), CallbackOptions, InState, OutState) :-
    data_received(PBStream, CallbackOptions, InState, OutState).
callback(resume, _Options, InState, OutState) :-
    resume(InState, OutState).

initial_state(State) :-
    State = prologbeans_state([]).

connection(Host, PBStream, InState, OutState) :-
    accepted_host(Host),!,
    new_stream(PBStream, prologbeans),
    OutState = InState.
connection(Host, _, InState, OutState) :-
    print_message(error, prologbeans(denied(Host))),
    OutState = InState.

connection_closed(PBStream, InState, OutState) :-
    session_remove(PBStream),
    OutState = InState.

%%% [PD] 3.12.0
%%% Catch all exceptions from fast_read/2 and close the stream in both
%%% directions. There are no recoverable errors from fast_read/2.
%%% We do not want to catch any exceptions from handle_input/2.

data_received(PBStream, CallbackOptions, InState, OutState) :-
    pb_input_stream(PBStream,IStream),
    catch( ( receive_term(IStream, Term), T=success ),
	  error(A,B),  % [PM] 4.1.0 Only ISO exceptions (SPRM 11617)
	  T=exception),
    ( T==success ->
	handle_input(Term, PBStream, CallbackOptions, InState, OutState)
    ; otherwise ->
        log('data_recived exception: ~q', [error(A,B)]),
        close_stream(PBStream, InState, OutState)
    ).


%% ------------------------------------------------------
%% Query handling
%% ------------------------------------------------------
handle_input(Msg, PBStream, CallbackOptions, InState, OutState) :-
    pb2_known_message(Msg),
    !,
    pb2_handle_input(Msg, PBStream, CallbackOptions, InState, OutState).
handle_input(end_of_file, _PBStream, _CallbackOptions, InState, OutState) :- !,
    % [PM] 4.3.1 This happens when PBStream is a text stream.
    OutState = InState.
handle_input(query(Query, Bindings), PBStream, CallbackOptions, InState, OutState) :- !,
    get_session(PBStream, Session),
    handle_input(query(Query, Bindings, Session), PBStream, CallbackOptions, InState, OutState).
handle_input(query(Query, Bindings, Session), PBStream, _CallbackOptions, InState, OutState) :- !,
    now(CurrentTime),
    session_set_last_access(Session, CurrentTime),
    handle_query(Query, Bindings, Session, PBStream, InState, OutState).
handle_input(end_session(Session), PBStream, _CallbackOptions, InState, OutState) :- !,
    session_remove(Session),
    send_result(PBStream, session_removed(Session)),
    OutState = InState.
handle_input(_Query, PBStream, _CallbackOptions, InState, OutState) :-
    unknown_query_reply(Reply),
    send_result(PBStream, Reply),
    OutState = InState.

%% The term to send back on unknown query
unknown_query_reply(error('unknown query')).

%% The term to send back on syntax error (from parsing the query template)
syntax_error_reply(error('syntax error')).

%% The term to send back when the result would contain cyclic terms
cyclic_result_reply(error('cyclic result')).


%% A simple way of getting a "unique"? session ID of a Stream...
get_session(Stream, Stream).

prepare_vars([], _, []).
prepare_vars([VarBinding|Vars], Bindings, Unbounds) :-
    memberchk(VarBinding, Bindings), !,
    prepare_vars(Vars, Bindings, Unbounds).
prepare_vars([VarBinding|Vars], Bindings, [VarBinding|Unbounds]) :-
    prepare_vars(Vars, Bindings, Unbounds).


handle_query(Query, Bindings, Session, PBStream, InState, OutState) :-
    % [PM] 4.2.1 FIXME: use parse_query/4.
    on_exception(Error, pb_read_term_from_codes(Query, Term,
						[variable_names(Variables)]),
                        syntax_error(Error)), !,
    prepare_vars(Variables, Bindings, Unbound),
    query(Term, Session, Unbound, PBStream),
    OutState = InState.
handle_query(_, _, _, PBStream, InState, OutState) :-
        syntax_error_reply(Reply),
        send_result(PBStream, Reply),
        OutState = InState.

:- syntax_error/1 is failing.
syntax_error(Error) :-
	print_message(error, prologbeans(read)),
	print_message(error, Error),
	fail.


:- mode parse_query(+,+,-,-).
%% Read and instantiate term from Template and Bindings, return bindings for unbound variables.
%% Writes an error message and fails on syntax error
parse_query(Template, Bindings, QueryTerm, Unbound) :-
    on_exception(Error, pb_read_term_from_codes(Template, Term,
                                                [variable_names(Variables)]),
                        syntax_error(Error)), !,
    prepare_vars(Variables, Bindings, Unbound),
    QueryTerm = Term.


query(Query, Session, Variables, PBStream) :-
    query_db(Query, Goal, Session), !,
    % Note: Goal is module prefixed
    call_query(Goal, Variables, Result),
    send_result(PBStream, Result).
query(_Query, _, _, PBStream) :-
        unknown_query_reply(Reply),
        send_result(PBStream, Reply).


call_query(Goal, Preliminary, Result) :-
    on_exception(Error, (call(Goal), Result = Preliminary),
		 Result = error(Error)), !.
call_query(_, _, no).

% Write term to (binary or text) PBStream. Writes an error message and sends an error if term is cyclic.
send_result(PBStream, Term) :-
    acyclic_term(Term), !,
    pb_send_term(PBStream, Term).
send_result(PBStream, Term) :-
    print_message(error, prologbeans(cyclic(PBStream,Term))),
    cyclic_result_reply(Reply),
    pb_send_term(PBStream, Reply).

% Write term to (binary or text) PBStream. Caller ensures Term is acyclic
pb_send_term(PBStream, Term) :- % < & > 
    pb_output_stream(PBStream, OStream),
    send_term(OStream, Term).

% Write term to (binary or text) Stream. Caller ensures Term is acyclic
send_term(OStream, Term) :-
    ( logging -> log('Call: ~q~n', [send_term(OStream, Term)]) ; true),
    ( stream_property(OStream, type(binary)) ->
      fast_write(OStream, Term)
    ; otherwise ->
      write_canonical(OStream, Term),
      write(OStream, ' .\n')
    ),
    flush_output(OStream).


%% ------------------------------------------------------
%% Query registry/unregistering
%% ------------------------------------------------------
register_query(Query, Goal) :-
    nonvar(Query),
    retractall(query_db(Query, _, _)),
    assert(query_db(Query, Goal, _)).

register_query(Query, Goal, SessionVar) :-
    nonvar(Query),
    retractall(query_db(Query, _, _)),
    assert(query_db(Query, Goal, SessionVar)).

unregister_query(Query) :-
    nonvar(Query),
    retractall(query_db(Query, _, _)).


%% ------------------------------------------------------
%% Event listener registration
%% ------------------------------------------------------

%% Register Goal as a handler for Event.
register_event_listener(Event, Goal, Id) :-
   registrable_event(Event), !,
   %% functor(F,A,Event),
   %% functor(F,A,Template),
   %% retractall(event_listener_db(Template, _, _)),
   %% event listeners will be called last-registered-first. This is
   %% intentionally not documented.
   asserta(event_listener_db(Event, Goal), Ref),
   Id = Ref.
register_event_listener(Event, Goal, Id) :-
   ErrGoal = register_event_listener(Event, Goal, Id),
   illarg_event(ErrGoal, 1, Event).

:- illarg_event/3 is throwing.
illarg_event(ErrGoal, ArgNo, Event) :-
   prolog:illarg(domain(term,'valid event'), ErrGoal, ArgNo, Event).


register_event_listener(Event, Goal) :-
   register_event_listener(Event, Goal, _Id).

%% it is an error to unregister an event twice
unregister_event_listener(Id) :-
   erase(Id).


call_event_listeners(Event) :-
   event_listener_db(Event, Goal), % BT
   call_event_robust(Event, Goal),
   fail.
call_event_listeners(_Event).   


call_event_robust(Event, Goal) :-
   % [PM] 4.2.1 FIXME: SPIDER will, rightly, complain about non-meta arg Goal. We need a declaration meta_predicate event_listener_db(+,-(0)) for a proper fix.
   on_exception(Exception,
		call(Goal),
		call_event_error(Exception, Event, Goal)),
   !,
   true.
call_event_robust(Event, Goal) :-
   call_event_failure(Event, Goal).

call_event_failure(Event, Goal) :-
    print_message(warning, prologbeans(handler_failure(Goal,Event))).

call_event_error(Exception, Event, Goal) :-
    print_message(error, prologbeans(handler_error(Goal,Event,Exception))).

%% matches valid events
registrable_event(Event) :- var(Event), !, fail.
registrable_event(server_started).
registrable_event(server_shutdown).
registrable_event(session_started(_SessionID)).
registrable_event(session_ended(_SessionID)).

call_server_listeners(Event) :-
   call_event_listeners(Event).

call_session_listeners(Event) :-
   call_event_listeners(Event).

%% ------------------------------------------------------
%% Session handling
%% ------------------------------------------------------

session_remove(SessionID) :-
    nonvar(SessionID),
    session_info(SessionID, _),!,
    call_session_listeners(session_ended(SessionID)),
    retractall(session_info(SessionID, _LastAccess)),
    retractall(session_data(SessionID, _Data)).
session_remove(SessionID) :-
    nonvar(SessionID).

session_get(SessionID, Name, _DefaultValue, Value) :-
    nonvar(SessionID),
    session_data(SessionID, List),
    memberchk(Name-Value, List), !.
session_get(_SessionID, _Name, Value, Value).

session_put(SessionID, Name, Value) :-
    nonvar(SessionID),
    session_data(SessionID, List),!,
    update_list(Name, Value, List, NewList),
    retractall(session_data(SessionID, _)),
    assert(session_data(SessionID, NewList)).
session_put(SessionID, Name, Value) :-
    nonvar(SessionID),
    assert(session_data(SessionID, [Name-Value])).

update_list(Name, Value, List, NewList) :-
    select(Name-_, List, Name-Value, NewList), !.
update_list(Name, Value, List, [Name-Value | List]).

%% ---------------------------------------------------------
%% Server properties (more properties will be added in future versions)
%% ---------------------------------------------------------
get_server_property(port(Port)) :-
    server_port(Port1),
    !,
    Port = Port1.

%% ---------------------------------------------------------
%% Session timeout handling
%% ---------------------------------------------------------

session_set_last_access(SessionID, Time) :-
    nonvar(SessionID),
    retract(session_info(SessionID, _)),!,
    assert(session_info(SessionID, Time)).
session_set_last_access(SessionID, Time) :-
    nonvar(SessionID),
    assert(session_info(SessionID, Time)),
    call_session_listeners(session_started(SessionID)).

session_set_timeout(TimeoutSeconds) :-
    integer(TimeoutSeconds),
    retractall(session_timeout(_)),
    (  TimeoutSeconds > 0 ->
	assert(session_timeout(TimeoutSeconds))
    ;  otherwise ->
	true
    ).

session_set_gc_timeout(TimeoutSeconds) :-
    integer(TimeoutSeconds),
    retractall(session_gc_timeout(_)),
    (  TimeoutSeconds > 0 ->
	assert(session_gc_timeout(TimeoutSeconds))
    ;  otherwise ->
	true
    ).

session_get_last_garbage_collect(LastTime) :-
    session_last_gc(LastTime), !.
session_get_last_garbage_collect(0).

session_set_last_garbage_collect(LastTime) :-
    retractall(session_last_gc(_)),
    assert(session_last_gc(LastTime)).

session_maybe_garbage_collect :-
    session_gc_timeout(GCTimeout),
    session_timeout(_),
    session_get_last_garbage_collect(LastGarbage),
    now(CurrentTime),
    LastGarbage + GCTimeout < CurrentTime, !,
    session_garbage_collect(CurrentTime).
session_maybe_garbage_collect.

% unreachable
% session_garbage_collect :-
%     now(CurrentTime),
%     session_garbage_collect(CurrentTime).

session_garbage_collect(CurrentTime) :-
    session_timeout(TimeoutSeconds), !,
    OldestTime is CurrentTime - TimeoutSeconds,
    findall(Session, (session_info(Session, LastAccess),
			 LastAccess < OldestTime),
	    SessionsToRemove),
    session_gc(SessionsToRemove),
    session_set_last_garbage_collect(CurrentTime).
session_garbage_collect(CurrentTime) :-
    session_set_last_garbage_collect(CurrentTime).

session_gc([Session|Ss]) :-
    session_remove(Session),
    session_gc(Ss).
session_gc([]).


%% ---------------------------------------------------------
%% Timeout and shutdown handling
%% ---------------------------------------------------------

resume(InState, OutState) :-
    session_maybe_garbage_collect,
    check_shutdown,
    OutState = InState.

check_shutdown :-
    shutdown_mode(Mode), !,
    check_shutdown(Mode).
check_shutdown.

check_shutdown(no_sessions) :-
    (  session_info(_, _) ->
	true
    ;  otherwise ->
	stop
    ).
check_shutdown(no_connections) :-
    (  has_open_connections ->
	true
    ;  otherwise ->
	stop
    ).


%% ---------------------------------------------------------
%% PB2 [PM] 4.2.1
%% ---------------------------------------------------------

:- dynamic /* not volatile */ pb2_registered_query_/3.

:- meta_predicate(pb2_register_query(+,0,+)).

%% Register a Query, the corresponding Goal and any Options
%% Options is used for passing in information to and from the Goal.
%% Valid options include (input/output mode indicated with +/-, respectively):
%% state(+In,-Out) where the Goal is supposed to bind Out to the new state on success
%% peer(-Peer) where Peer can be used by by Goal for communicating with the peer process.
%% result(-Result) where the goal should bind Result which will be returned to the caller.
pb2_register_query(Query, Goal, Options) :-
        callable(Query), !,
        pb2_unregister_query(Query),
        assert(pb2_registered_query_(Query, Goal, Options)).
pb2_register_query(Query, Goal, Options) :-
        pb2_error(pb2_register_query(Query, Goal, Options)).

:- mode pb2_unregister_query(+).
%% Unregister a query. Does nothing if there is no registered query matching Query.
pb2_unregister_query(Query) :-
        callable(Query), !,
        functor(Query, F, A),
        functor(Template, F, A),
        retractall(pb2_registered_query_(Template, _, _)).
pb2_unregister_query(Query) :-
        pb2_error(pb2_unregister_query(Query)).

:- pb2_error/1 is throwing.
pb2_error(Error) :-
        throw(pb2server_error(Error)).

:- mode pb2_known_message(+).
pb2_known_message(Msg) :- var(Msg), !,
        fail.
pb2_known_message(Msg) :-
        pb2_known_query_message(Msg), !.
pb2_known_message(Msg) :-
        pb2_known_result_message(Msg), !.
pb2_known_message(done).
pb2_known_message(more).


%% Messages corresponding to a PB2 query
pb2_known_query_message('call'(_)).
pb2_known_query_message('call'(_,_)).
pb2_known_query_message('once'(_)).
pb2_known_query_message('once'(_,_)).

%% Messages corresponding to result from a PB2 query
pb2_known_result_message('[]').
pb2_known_result_message([_|_]).
pb2_known_result_message('no').
pb2_known_result_message('error'(_)).

%% Continuation corresponding to a received result-message.
pb2_result_message_continuation(Msg, Continuation) :-
        Msg = '[]',
        Continuation = result(Msg).
pb2_result_message_continuation(Msg, Continuation) :-
        Msg = [_|_],
        Continuation = result(Msg).
pb2_result_message_continuation(Msg, Continuation) :-
        Msg = 'no',
        Continuation = no.
pb2_result_message_continuation(Msg, Continuation) :-
        Msg = 'error'(_),
        Continuation = Msg.

pb2_handle_input(Msg, PBStream, _CallbackOptions, PBInState, PBOutState) :-
    PBInState = prologbeans_state(QInStates),
    PBOutState = prologbeans_state(QOutStates),
    pb2_stream_key(PBStream, QKey),
    pb2_q_state(QInStates, QKey, InQState, QOutStates, OutQState),
    pb2_state_components(PB2InState, PBStream, InQState),
    pb2_dispatch_message(Msg, PB2InState, PB2OutState),
    pb2_state_components(PB2OutState, PBStream, OutQState).


pb2_q_state([], Key, InQState, OutStates, OutQState) :-
    pb2_default_q_state(Key,InQState),
    OutStates = [Key-OutQState].

pb2_q_state([K-KState|InStates], Key, InQState, OutStates, OutQState) :-
    K == Key, !,
    InQState = KState,
    OutStates = [Key-OutQState|InStates].

pb2_q_state([KKState|InStates], Key, InQState, OutStates, OutQState) :-
    % KKState = K-_, K \== Key,
    OutStates = [KKState|OutStates1],
    pb2_q_state(InStates, Key, InQState, OutStates1, OutQState).

pb2_default_q_state(_Key, OutQState) :-
    OutQState = [].


%% A unique key identifying this connection.
:- mode pb2_stream_key(+, -).
pb2_stream_key(PBStream, QKey) :-
    % for now. FIXME: use something atomic, e.g. stream id
    QKey = PBStream.


pb2_dispatch_message(Msg, PB2InState, PB2OutState) :-
        call_message_description(Msg, Description), % call/[1,2]
        !,
        pb2_safe_call_hook(Description, PB2InState, State1, R), % BT
        pb2_write_call_result(R, State1),
        ( R = result(_) ->
          % produced a solution, let client ask for more
          pb2_read_and_dispatch_after_call(State1, State2, Continuation1),
          ( Continuation1 = 'more' ->
             % Had a solution, backtrack for more.
            fail
          ; Continuation1 = 'done' ->
            !,
            true
          )
        ; otherwise ->
          % invariant R in [no, error(_), unknown(_)]
          !, % [PM] 4.2.1 redundant, for detcheck
          State2 = State1
        ),
        PB2OutState = State2.
pb2_dispatch_message(Msg, PB2InState, PB2OutState) :-
        once_message_description(Msg, Description), % once/[1,2]
        pb2_safe_once_hook(Description, PB2InState, State1, R),
        pb2_write_call_result(R, State1),
        PB2OutState = State1.

call_message_description('call'(Template, Bindings), template_query(Template, Bindings)).
call_message_description('call'(Term), term_query(Term)).

once_message_description('once'(Template, Bindings), template_query(Template, Bindings)).
once_message_description('once'(Term), term_query(Term)).

:- pb2_call_hook(+, +, -, -) is nondet.
:- mode pb2_call_hook(+, +, -, -).
%% Succeeds with Result one of explicit_result(R), implicit_result, unknown_query
%% Passes on non-success and non-determinism from query hook.
pb2_call_hook(Query, InState, OutState, Result) :-
        pb2_registered_query_(Query, Goal, QueryParams), !,
        ( memberchk(state(QInState, QOutState), QueryParams) ->
          pb2_state_state(InState, QInState)
        ; otherwise ->
          pb2_state_state(InState, QInState),
          QOutState = QInState
        ),
        /*-
        ( memberchk(in_state(QInState), QueryParams) ->
          pb2_state_state(InState, QInState)
        ; true
        ),
        ( memberchk(out_state(QOutState), QueryParams) ->
          true
        ; otherwise ->
          pb2_state_state(InState, QInState), % redundant if in_state/1 option
          QOutState = QInState
        ),
        */
        ( memberchk(peer(Peer), QueryParams) ->
          pb2_state_peer(InState, Peer)
        ; true
        ),
        %% ( memberchk(determinate(false),QueryParams) ->
          call(Goal),
        %%   true
        %% ; otherwise ->
        %%   once(Goal)
        %% ),
        pb2_update_state_state(InState, QOutState, OutState),
        ( memberchk(result(R), QueryParams) ->
          Result = explicit_result(R)
        ; otherwise ->
          Result = implicit_result
        ).
pb2_call_hook(_Query, InState, OutState, Result) :-
        OutState = InState,
        Result = unknown_query.

%% Translates result from pb2_call_hook/4 for a template_query/2.
%% Succeeds with result(Bindings) or error(...)
template_query_result(explicit_result(R), _ResultBindings, Result) :-
        Result = result([result=R]).
template_query_result(implicit_result, ResultBindings, Result) :-
        Result = result(ResultBindings).
template_query_result(unknown_query, _ResultBindings, Result) :-
        unknown_query_reply(Result).

%% Translates result from pb2_call_hook/4 for a term_query/1.
%% Succeeds with result(Bindings) or error(...)
term_query_result(explicit_result(R), _Query, Result) :-
        Result = result([result=R]).
term_query_result(implicit_result, Query, Result) :-
        Result = result([result=Query]).
term_query_result(unknown_query, _Query, Result) :-
        unknown_query_reply(Result).



:- pb2_safe_call_hook1(+, +, -, -) is nondet.
%% Passes on failure from query hook.
pb2_safe_call_hook1(Description, InState, OutState, Result) :-
        Description = template_query(Template, Bindings),
        ( parse_query(Template, Bindings, Query, Unbound) ->
          ResultBindings = Unbound,
          E = error(_,_),
          catch(( pb2_call_hook(Query, InState, OutState, HookResult),
                  template_query_result(HookResult, ResultBindings, Result)
                ), E, (Result=error(E), OutState=InState))
        ; otherwise ->
          OutState = InState,
          syntax_error_reply(Result)
        ).
pb2_safe_call_hook1(Description, InState, OutState, Result) :-
        Description = term_query(Query),
        E = error(_,_),
        catch(( pb2_call_hook(Query, InState, OutState, HookResult),
                term_query_result(HookResult, Query, Result)
              ), E, (Result=error(E), OutState=InState)).



:- pb2_safe_call_hook(+, +, -, -) is nondet.
pb2_safe_call_hook(Description, InState, OutState, Result) :-
        ( pb2_safe_call_hook1(Description, InState, OutState, Result)
        ; OutState = InState,
          Result = no % Result = failure(Query)
        ).

pb2_safe_once_hook(Description, InState, OutState, Result) :-
        ( pb2_safe_call_hook1(Description, InState, OutState, Result1) ->
          Result = Result1
        ; OutState = InState,
          Result = no % Result = failure(Query)
        ).

pb2_state_components(pb2_state(PBStream, QState), PBStream, QState).

pb2_state_message_stream(pb2_state(PBStream, _), In) :-
    pb_input_stream(PBStream, In).

pb2_state_reply_stream(pb2_state(PBStream, _), Out) :-
        pb_output_stream(PBStream, Out).

pb2_state_state(pb2_state(_,QState), QState).

pb2_update_state_state(pb2_state(PBStream,_QState), QState, pb2_state(PBStream, QState)).

pb2_state_peer(State, Peer) :-
        State = pb2_state(PBStream, _),
        pb_input_stream(PBStream, In),
        pb_output_stream(PBStream, Out),
        pb2_get_stream_id(In, InId),
        pb2_get_stream_id(Out, OutId),
        Peer = pb2_peer(InId, OutId, State).



pb2_write_call_result(result(Bindings), State) :-
    % Bindings is a list of atom=term
    pb2_write_server_message(Bindings, State).
pb2_write_call_result(no, State) :-
    pb2_write_server_message(no, State).
pb2_write_call_result(error(E), State) :-
    pb2_write_server_message(error(E), State).
pb2_write_call_result(unknown(_G), State) :-
        unknown_query_reply(Reply),
        pb2_write_server_message(Reply, State).


pb2_write_server_message(Msg, State) :-
        pb2_state_reply_stream(State, S),
        pb2_write_stream_message(S, Msg).

%% FIXME: What to do if cyclic?
%% FIXME: What to do if error?
pb2_write_stream_message(S, Msg) :-
    acyclic_term(Msg), !,
    send_term(S, Msg).

%% FIXME: What to do if error?
pb2_read_stream_message(S, Msg) :-
    receive_term(S, Msg).

%% Receive a term from the (binary or text) stream.
%% FIXME: What to do if error?
receive_term(S, Term) :-
        ( logging -> log('Call: ~q~n', [receive_term1(S, Term)]) ; true),
        receive_term1(S, Term1),
        ( logging -> log('Called: ~q~n', [receive_term1(S, Term1)]) ; true),
        Term = Term1.


receive_term1(S, Term) :-
    stream_property(S, type(binary)), !,
    fast_read(S, Term).
receive_term1(S, Term) :-
    read(S, Term).



pb2_get_stream_id(S, Id) :- S = closure(_), !,
        Id = 'unavailable'.
pb2_get_stream_id(S, Id) :-
        stream_property(S, id(Id1)),
        !, % [PM] 4.2.1 redundant, for detcheck.
        Id = Id1.


pb2_read_and_dispatch_after_call(InState, OutState, Continuation) :-
        pb2_read_next_client_message(InState, Msg),
        pb2_read_and_dispatch_after_call1(Msg, InState, OutState, Continuation).

pb2_read_and_dispatch_after_call1(Msg, InState, OutState, Continuation) :-
        pb2_known_query_message(Msg), % call or once
        !,
        pb2_dispatch_message(Msg, InState, State1),
        pb2_read_and_dispatch_after_call(State1, OutState, Continuation).
pb2_read_and_dispatch_after_call1(Msg, InState, OutState, Continuation) :-
        Msg = 'more',
        !,
        OutState = InState,
        Continuation = 'more'.
pb2_read_and_dispatch_after_call1(Msg, InState, OutState, Continuation) :-
        Msg = 'done',
        !,
        OutState = InState,
        Continuation = 'done'.
pb2_read_and_dispatch_after_call1(Msg, _InState, _OutState, _Continuation) :-
        pb2_known_message(Msg),
        !,
        pb2_invalid_message_context(Msg).
pb2_read_and_dispatch_after_call1(Msg, _InState, _OutState, _Continuation) :-
        pb2_invalid_message(Msg).

:- pb2_invalid_message_context/1 is throwing.
pb2_invalid_message_context(Msg) :-
        pb2_throw_error(invalid_message_context(Msg)).

:- pb2_invalid_message/1 is throwing.
pb2_invalid_message(Msg) :-
        pb2_throw_error(invalid_message(Msg)).

:- pb2_throw_error/1 is throwing.
pb2_throw_error(Description) :-
        throw(prologbeans_exception(Description)).

% Get the next message from the client
pb2_read_next_client_message(State, Message) :-
    pb2_state_message_stream(State, S),
    pb2_read_stream_message(S, Message).


pb2_peer_in_state(Peer, QInState, InState) :-
        pb2_verify_peer_streams(Peer),
        pb2_peer_state(Peer, State),
        pb2_update_state_state(State, QInState, InState).

pb2_peer_state(pb2_peer(_,_,State), State).

pb2_verify_peer_streams(Peer) :-
        Peer = pb2_peer(InId, OutId, State),
        State = pb2_state(PBStream, _),
        pb_input_stream(PBStream, In),
        pb_output_stream(PBStream, Out),
        pb2_get_stream_id(In, InId),
        pb2_get_stream_id(Out, OutId).


pb2_peer_message_stream(pb2_peer(_, OutId, State), S) :-
        State = pb2_state(PBStream, _),
        pb_output_stream(PBStream, Out),
        pb2_get_stream_id(Out, OutId),
        S = Out.


pb2_once_peer(Query, Peer, QInState, QOutState, Result) :-
        pb2_write_peer_callback_message('once'(Query), Peer),
        pb2_peer_in_state(Peer, QInState, InState1),
        pb2_read_and_dispatch_after_once_peer(InState1, OutState1, Continuation),
        pb2_state_state(OutState1, QOutState1),
        QOutState = QOutState1,
        ( Continuation = result(_) ->
          Result = Continuation
        ; Continuation = no ->
          Result = Continuation
        ; Continuation = error(_) ->
          Result = Continuation
        ).

pb2_call_peer_async(Query, Peer) :-
        pb2_write_peer_callback_message('call'(Query), Peer).

pb2_call_peer_async_more(Peer) :-
        pb2_write_peer_callback_message('more', Peer).

pb2_call_peer_async_done(Peer) :-
        pb2_write_peer_callback_message('done', Peer).

pb2_call_peer_done(Peer) :-
        pb2_call_peer_async_done(Peer).

pb2_call_peer_first(Query, Peer, QInState, QOutState, Result) :-
        pb2_call_peer_async(Query, Peer),
        pb2_call_peer_async_result(Peer, QInState, QOutState, Result).

%% FIXME: Improve or remove this since it is too easy to get communication out of sync? 
:- pb2_call_peer(+, +, +, -, -) is nondet.
%% Do not use this.
pb2_call_peer(Query, Peer, QInState, QOutState, Result) :-
        ( pb2_call_peer_first(Query, Peer, QInState, QOutState1, Result1)
        ; repeat,
          pb2_call_peer_more(Peer, QInState, QOutState1, Result1)
        ),
        ( Result1 = result(_) ->
          % query must be closed
          call_cleanup(( Succeeded=yes
                       ;
                         Succeeded=no),
                       ( Succeeded == yes ->
                         pb2_call_peer_done(Peer)
                       ; % Succeeded == no ->
                         % No cleanup on failure, it is handled by repeat above
                         true
                       )),
          Succeeded == yes
        ; otherwise ->
          % query is done
          !
        ),
        QOutState = QOutState1,
        Result = Result1.

pb2_call_peer_more(Peer, QInState, QOutState, Result) :-
        pb2_call_peer_async_more(Peer),
        pb2_call_peer_async_result(Peer, QInState, QOutState, Result).


pb2_call_peer_async_result(Peer, QInState, QOutState, Result) :-
        pb2_peer_in_state(Peer, QInState, InState1),
        pb2_read_and_dispatch_after_call_peer(InState1, OutState1, Continuation),
        pb2_state_state(OutState1, QOutState1),
        QOutState = QOutState1,
        ( Continuation = result(_) ->
          Result = Continuation
        ; Continuation = no ->
          Result = Continuation
        ; Continuation = error(_) ->
          Result = Continuation
        ).


pb2_findall_peer(Query, Peer, QInState, QOutState, Results, E) :-
        pb2_call_peer_async(Query, Peer),
        pb2_findall_peer1(Peer, QInState, QOutState, Results, E).

pb2_findall_peer1(Peer, QInState, QOutState, Results, E) :-
        pb2_call_peer_async_result(Peer, QInState, QState1, Result),
        pb2_findall_peer1(Result, Peer, QState1, QOutState, Results, E).

pb2_findall_peer1(Msg, Peer, QInState, QOutState, Results, E) :- Msg = result(R),
        Results = [R|Results1],
        pb2_call_peer_async_more(Peer),
        pb2_findall_peer1(Peer, QInState, QOutState, Results1, E).
pb2_findall_peer1(Msg, _Peer, QInState, QOutState, Results, E) :-  Msg = no,
        Results = [],
        E = success,
        QOutState = QInState.
pb2_findall_peer1(Msg, _Peer, QInState, QOutState,  Results, E) :- Msg = error(_),
        Results = [],
        E = Msg,
        QOutState = QInState.




pb2_read_and_dispatch_after_call_peer(InState, OutState, Continuation) :-
        pb2_read_and_dispatch_after_once_peer(InState, OutState, Continuation).


pb2_read_and_dispatch_after_once_peer(InState, OutState, Continuation) :-
        pb2_read_next_client_message(InState, Msg),
        pb2_read_and_dispatch_after_once_peer1(Msg, InState, OutState, Continuation).

pb2_read_and_dispatch_after_once_peer1(Msg, InState, OutState, Continuation) :-
        pb2_known_query_message(Msg),
        !,
        pb2_dispatch_message(Msg, InState, State1),
        pb2_read_and_dispatch_after_once_peer(State1, OutState, Continuation).
pb2_read_and_dispatch_after_once_peer1(Msg, InState, OutState, Continuation) :-
        pb2_known_result_message(Msg), % [], [V=T,...], error(_), no
        !,
        OutState = InState,
        pb2_result_message_continuation(Msg, Continuation).
pb2_read_and_dispatch_after_once_peer1(Msg, _InState, _OutState, _Continuation) :-
        pb2_known_message(Msg),
        !,
        pb2_invalid_message_context(Msg).
pb2_read_and_dispatch_after_once_peer1(Msg, _InState, _OutState, _Continuation) :-
        pb2_invalid_message(Msg).


pb2_write_peer_callback_message(Msg, Peer) :-
        pb2_peer_message_stream(Peer, S),
        pb2_write_stream_message(S, Msg).

:- mode pb2_client_open(+, -, +).
%% Connect to a prolog beans server. The first argument can be either
%% . an address argument suitable as first argument to sockets:socket_client_open/3, or
%% . a term, streams(InStream,OutStream), where InStream and OutStream should be
%%   already opened streams that will be used for communicating with the server.
pb2_client_open(streams(In,Out), Peer, Options) :- !,
        pb2_client_stream_open(In, Out, Peer, Options).
pb2_client_open(SocketAddr, Peer, Options) :-
        pb2_client_open_socket_client_open_options(Options, CSOOptions, OtherOptions),
        socket_client_open(SocketAddr, SocketStream, OtherOptions),
        In = SocketStream,
        Out = SocketStream,
        pb2_client_stream_open(In, Out, Peer, CSOOptions).

%% Close the streams associated with a peer, previously created with pb2_client_open/3.
pb2_client_close(Peer) :-
        pb2_peer_state(Peer, State),
        pb2_state_components(State, PBStream, _),
        pb_input_stream(PBStream, In),
        pb_output_stream(PBStream, Out),
        close(Out, [direction(output)]),
        close(In, [direction(input)]).




pb2_client_stream_open(In, Out, Peer, _Options) :-
        prologbeansserver:pb_opaque_stream(In,Out,PBStream),
        State = pb2_state(PBStream,X),
        X = [],
        pb2_state_peer(State, Peer).


pb2_client_open_socket_client_open_options(Options, CSOOptions, OtherOptions) :-
         CSOOptionPatterns = [], % no options defined, yet.
         filter(Options, CSOOptionPatterns, CSOOptions, OtherOptions).


%% Split Items into those matching Patters and those not matching. Order is preserved.
filter(Items, Patterns, Matches, NonMatches) :-
        Patterns == [], !,
        Matches = Items,
        NonMatches = [].
filter(Items, Patterns, Matches, NonMatches) :-
        ( foreach(Item, Items),
           fromto(FirstMatches, InMatches, OutMatches, []),
           fromto(FirstNonMatches, InNonMatches, OutNonMatches, []),
           param([Patterns])
         do
           ( \+ memberchk(Item, Patterns) ->
             InMatches = OutMatches,
             InNonMatches = [Item|OutNonMatches]
           ; otherwise ->
             InMatches = [Item|OutMatches],
             InNonMatches = OutNonMatches
           )
         ),
         Matches = FirstMatches,
         NonMatches = FirstNonMatches.


