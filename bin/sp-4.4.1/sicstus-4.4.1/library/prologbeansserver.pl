%%% Copyright (c) 2003, 2011 SICS AB. All rights reserved.
%%% -----------------------------------------------------------------
%%%
%%% PROLOGBEANSSERVER
%%% an implementation of a simple Internet server listening to one
%%% or several ports. Currently used for the PrologBeans module/library.
%%% Note: This is an internal module and may change without notice.
%%%
%%% Original Authors : Joakim Eriksson, Niclas Finne
%%%

:- module(prologbeansserver, [
	init/0,
	new_thread/1, % [PM] 4.2.1 legacy, do not use
        new_thread/3, % [PM] 4.2.1
	run/0,
        run/1,
	open_socket/3,
        open_socket/4,
	stop/0,
	new_stream/2,
	has_open_connections/0,
	pb_input_stream/2,
	pb_output_stream/2,
	close_stream/1, % [PM] 4.2.1 legacy, do not use.
        close_stream/3 % [PM] 4.2.1
	]).

:- use_module(library(sockets),[
	socket_server_open/3,
	socket_server_close/1,
	socket_select/7,
	socket_server_accept/4
        ]).

:- meta_predicate(new_thread(+,4,+)).
:- meta_predicate(new_thread1(+,4,+)).


:- dynamic is_running/0.	% is_running
:- volatile is_running/0.

:- dynamic current_timeout_/1.	% current_timeout_(<seconds>:<microseconds>)
:- volatile current_timeout_/1.

:- dynamic socket/3.		% socket(<socket>,<handler>,<stream options>)
:- volatile socket/3.

:- dynamic stream/2.		% stream(<stream>,<handler>)
:- volatile stream/2.

% [PM] 4.2.1 It makes (marginal) sense to register thread and handler before
% saving a state so the following two dynamic predicates should not be volatile.
:- dynamic /* not volatile */ thread/1.		% thread(<handler>)
%% [PM] 4.2.1 registered_handler_(+Thread, -4, -Options).
:- dynamic /* not volatile */ registered_handler_/3.




:- meta_predicate my_module(:).
% [PM] 4.2.1 Obtain module context of caller.
my_module(M:Module) :-
    Module = M.

%% [PD] Quintus does not have bidirectional streams. Combine the output stream
%%      and the input stream into an opaque entity. Do this for both SICStus
%%      and Quintus and use the predicates below to extract the necessary
%%      stream.
%%      Within this module care has to be taken whenever handling streams.
%%      Sometimes it is an opaque stream entity and sometimes a real stream.

%% 
pb_input_stream(PBStream, InputStream) :-
    pb_opaque_stream(InputStream, _, PBStream).

pb_output_stream(PBStream, OutputStream) :-
    pb_opaque_stream(_, OutputStream, PBStream).

pb_opaque_stream(InputStream,OutputStream,PBStream) :-
    PBStream = '$pbstream'(InputStream,OutputStream).

open_socket(P, H, SocketOptions) :-
    StreamOptions = [], % the default, i.e. [type(binary)],
    open_socket(P, H, SocketOptions, StreamOptions).

open_socket(P, H, SocketOptions, StreamOptions) :-
    open_socket1(P, Socket, SocketOptions),
    assert(socket(Socket,H,StreamOptions)).

open_socket1(P, Socket, SocketOptions) :-
    socket_server_open(P, Socket, SocketOptions).

current_sockets(HSs) :-
    findall(H-S, socket(S,H,_), HSs).

do_close_server_socket(H, Socket) :-
    on_exception(error(_ISO,Error),
		 socket_server_close(Socket),
		 print_message(error, prologbeans(close_socket_h(H,Error)))).


close_socket(H-Socket) :-
    do_close_server_socket(H, Socket),
    retractall(socket(Socket,_,_)).

close_sockets :-
    current_sockets(HSs),
    close_sockets(HSs).

close_sockets([]).
close_sockets([HS|HSs]) :-
    close_socket(HS),
    close_sockets(HSs).

set_timeout(off) :- !,
    retractall(current_timeout_(_)),
    assert(current_timeout_(off)).
set_timeout(T) :-
    retractall(current_timeout_(_)),
    assert(current_timeout_(T)).

init :-
    set_timeout(10:0).



stop :-
    retractall(is_running).

:- mode current_timeout(-).
current_timeout(T) :-
        current_timeout_(T1), !,
        T = T1.

run :-
    run([]).

run(InitialStreams) :-
    %% Make sure the server has been initialized
    current_timeout(_), !,
    assert(is_running),
    % [PM] 4.2.1 allow priming with already open streams, e.g. for testing.
    ( foreach(H-SS, InitialStreams)
    do
      ( SS = (In,Out) ->
        pb_opaque_stream(In, Out, PBStream)
      ; SS = BidiStream ->
        pb_opaque_stream(BidiStream, BidiStream, PBStream)
      ),
      new_stream(PBStream, H)
    ),
    loop([]).
run(_InitialStreams) :-
    throw(error(system_error,system_error('server not initialized'))).


loop(InStates) :-
    is_running,!,
    listen_robust(InStates, States),
    loop(States).
loop(_InStates) :-
    close_sockets,
    close_streams.

listen_robust(InStates, OutStates) :-
    on_exception(E,listen(InStates, OutStates1),listen_error(E, InStates, OutStates1)),
    !,
    OutStates = OutStates1.
listen_robust(InStates, OutStates) :-
    print_message(warning, prologbeans(listen)),
    OutStates = InStates.


listen(InStates, OutStates) :-
    current_timeout(To),
    current_sockets(HSs),
    current_input_streams(Ss),
    socket_select(HSs, Cs, Ss, RSs, [], _, To),
    s_handle_connections(Cs, InStates, States1),
    s_handle_inputs(RSs, States1, States2),
    get_threads(Threads),
    handle_threads(Threads, States2, OutStates).


s_handle_connections([], InStates, OutStates) :-
    OutStates = InStates.
s_handle_connections([H-ServerSocket|Ss], InStates, OutStates) :-
    socket(ServerSocket, H, StreamOptions), !,
    socket_server_accept(ServerSocket, Client, SocketStream, StreamOptions),
    pb_opaque_stream(SocketStream, SocketStream, PBStream),
    thread_state(InStates, H, InThreadState, States1, OutThreadState), 
    handle_connection(H-connection(Client,PBStream), InThreadState, OutThreadState),
    s_handle_connections(Ss, States1, OutStates).


handle_connection(H-connection(A,PBStream), InThreadState, OutThreadState) :-
    call_handler_robust(H, connection(A, PBStream), InThreadState, ThreadState),
    maybe_close_new_connection(PBStream),
    OutThreadState = ThreadState.


% Closes the new connection if the handler did not add it as a new stream
maybe_close_new_connection(PBStream) :-
    stream(PBStream,_), !.
maybe_close_new_connection(PBStream) :-
    on_exception(Error,
		 close_stream_internal(PBStream, [force(true)]),
		 print_message(error, prologbeans(close_stream_internal(PBStream,Error)))).

has_open_connections :-
    stream(_,_),
    !.

new_stream(PBStream,H) :-
    %% [PM] 4.2.1 FIXME: guard against duplicate entries, invalid arguments, ...?
    assert(stream(PBStream,H)).

:- mode stream_handler(+, -).
stream_handler(PBStream,H) :-
    stream(PBStream, H1),
    !,
    H = H1.

current_streams(Ss) :-
    findall(PBStream, stream(PBStream,_), Ss).

%% List of IS-PBStream, suitable for socket_select.
current_input_streams(Ss) :-
    findall(PBStream-IS, ( stream(PBStream,_), pb_input_stream(PBStream,IS) ), Ss).

%% Obsolete. Use close_stream/4 instead.
close_stream(PBStream) :-
    stream(PBStream, H), !,
    default_thread_state(H, InThreadState),
    close_stream(PBStream, InThreadState, _OutThreadState).
close_stream(_).


close_stream(PBStream, InThreadState, OutThreadState) :-
    nonvar(PBStream),
    stream(PBStream, H), !,
    %% [PM] 4.0.5 SPRM 11252 The exported stream closer (called by
    %% prologbeans) should notify handler.
    call_handler_robust(H, connection_closed(PBStream), InThreadState, ThreadState1),
    close_stream_internal(PBStream),
    OutThreadState = ThreadState1.
close_stream(_, InThreadState, OutThreadState) :-
    OutThreadState = InThreadState.

close_stream_internal(PBStream) :-
    close_stream_internal(PBStream, []).

close_stream_internal(PBStream, Options) :-
    nonvar(PBStream),
    pb_input_stream(PBStream,IS),
    do_close_stream(IS, Options),
    retractall(stream(PBStream,_)).

do_close_stream(IS, Options) :-
    on_exception(error(_ISO,Error),
		 close(IS, Options),
		 print_message(error, prologbeans(close_stream_internal(IS,Error)))).

close_streams :-
    current_streams(Ss),
    close_streams(Ss).

close_streams([]).
close_streams([PBStream|Ss]) :-
    %% would be faster to simply close stream and retractall(stream(_,_))...
    close_stream_internal(PBStream),
    close_streams(Ss).


s_handle_inputs([], InStates, OutStates) :-
    OutStates = InStates.
s_handle_inputs([PBStream-_InputStream|RSs], InStates, OutStates) :-
    % pb_opaque_stream(SocketStream,SocketStream,PBStream),
    handle_input(PBStream, InStates, States1),
    s_handle_inputs(RSs, States1, OutStates).

handle_input(PBStream, InStates, OutStates) :-
    stream_handler(PBStream, H), !,
    handle_input(PBStream, H, InStates, OutStates).
handle_input(PBStream, InStates, OutStates) :-
    print_message(error, prologbeans(no_handler(PBStream))),
    close_stream_internal(PBStream),
    OutStates = InStates.

handle_input(PBStream, H, InStates, OutStates) :-
        thread_state(InStates, H, InThreadState, OutStates, OutThreadState),
        (  check_end_of_stream(PBStream) ->
           close_stream(PBStream, InThreadState, OutThreadState)
        ; otherwise ->
           % [PM] 4.2.1 handler should be prepared to handle (doing
           % nothing) the case when reading from PBStream returns
           % EOF. This is because (at least for text streams) EOF
           % detection is not reliable until a term is actually read.
           call_handler_robust(H, data_received(PBStream), InThreadState, OutThreadState)
        ).

check_end_of_stream(PBStream) :-
    pb_input_stream(PBStream,IS),
    %% [PM] 4.0.5 SPRM 11252 at_end_of_stream will not report EOF
    %% for connection reset (e.g. other end closed (abortively?)).
    %%
    %% To quote our own docs: "The design of
    %% @code{at_end_of_stream/[0,1]} makes it inherently
    %% unreliable. It is present only for ISO standards
    %% compliance."
    %% at_end_of_stream(IS)

    %% [PM] 4.0.5 Treat all errors as EOF
    %% [PM] 4.1.0 Never blindly gobble exceptions, only error/2 terms
    on_exception(error(_ISO,_E),peek_eof(IS),true).

%% Succeeds if peeking ahead produces an EOF. Works for both binary and text streams.
peek_eof(Stream) :-
    stream_property(Stream, type(binary)), !,
    peek_byte(Stream, -1).
peek_eof(Stream) :-
    % stream_property(Stream, type(text)),
    peek_code(Stream, -1).



new_thread(H, MHandler, Options) :-
    atom(H),
    MHandler = M:Handler,
    atom(M),
    callable(Handler), % Not a strong check
    new_thread1(H, MHandler, Options).

new_thread1(H, Handler, Options) :-
    remove_thread(H),
    assert(thread(H)),
    assert(registered_handler_(H, Handler, Options)).

%% [PM] 4.2.1 Obsolete. Use new_thread/3 instead.
new_thread(H) :-
    nonvar(H),
    thread(H),!.
new_thread(H) :-
    % [PM] 4.2.1 legacy argument check is weaker than new_thread/3.
    nonvar(H),
    new_thread1(H, legacy_handler, [thread(H)]).

remove_thread(H) :-
    nonvar(H),
    retractall(thread(H)),
    retractall(registered_handler_(H, _, _)).

get_threads(Hs) :-
    findall(H, thread(H), Hs).

% This will also filter away states for non-existing threads.
handle_threads([], _InStates, OutStates) :-
    OutStates = [].
handle_threads([H|Hs], InStates, OutStates) :-
    ( memberchk(H-InThreadState, InStates) ->
      true
    ; otherwise ->
      default_thread_state(H, InThreadState)
    ),
    OutStates = [H-OutThreadState|OutStates1],
    call_handler_robust(H, resume, InThreadState, OutThreadState),
    handle_threads(Hs, InStates, OutStates1).

legacy_handler(G, _Options, _InState, OutState) :- G = initial_state,
    OutState = [].
legacy_handler(G, Options, InState, OutState) :- G = connection(_, _),
    memberchk(thread(H), Options),
    once(H:G),
    OutState = InState.
legacy_handler(G, Options, InState, OutState) :- G = connection_closed(_),
    memberchk(thread(H), Options),
    once(H:G),
    OutState = InState.
legacy_handler(G, Options, InState, OutState) :- G = data_received(_),
    memberchk(thread(H), Options),
    once(H:G),
    OutState = InState.
legacy_handler(G, Options, InState, OutState) :- G = resume,
    memberchk(thread(H), Options),
    once(H:G),
    OutState = InState.



/*-
:- meta_predicate(register_handler(:,1,+)).
:- meta_predicate(unregister_handler(:)).

register_handler(Template1, Closure, Options) :-
    parse_template(Template1, Thread, Template),
    callable(Closure),
    !,
    unregister_handler1(Template, Thread),
    assert(registered_handler_(Template, Thread, Closure, Options)).

parse_template(M1:Template1, Thread, Template) :-
    get_module(Template1, M1, Template2, _M2),
    callable(Template2),
    atom(M1),
    Thread = M1, % not M2
    Template = Template2.

unregister_handler(Template1) :-
    parse_template(Template1, Thread, Template),
    !,
    unregister_handler1(Template, Thread).

unregister_handler1(Template1, Thread) :-
    functor(Template1, F, A),
    functor(Template, F, A),
    retractall(registered_handler_(Template, Thread, _, _)).

get_module(Term1, Mod1, Term2, Mod2) :- var(Term1), !,
        Term2 = Term1,
        Mod2 = Mod1.
get_module(Mod1:Term1, _, Term2, Mod2) :- 
        !,
        get_module(Term1, Mod1, Term2, Mod2).
get_module(Term, Mod, Term, Mod).
*/

call_handler_robust(Thread, G, InThreadState, OutThreadState) :-
    registered_handler_(Thread, Handler, Options),
    on_exception(E, call(Handler, G, Options, InThreadState, ThreadState1), handler_error(Thread,G,E, InThreadState, ThreadState1)),
    !,
    OutThreadState = ThreadState1.
call_handler_robust(Thread, G, InThreadState, OutThreadState) :-
    print_message(warning, prologbeans(handler_failure(Thread,G))),
    OutThreadState = InThreadState.

handler_error(H, G, E, InThreadState, OutThreadState) :-
    print_message(error, prologbeans(handler_error(H,G,E))),
    OutThreadState = InThreadState.

listen_error(system_blocked(M), _InStates, _OutStates) :- !, % [PM] 4.2.1 not used (was it ever?)
    throw(system_blocked(M)).
listen_error(error(_ISO,existence_error(_,1,stream,SocketStream,0)), InStates, OutStates) :- !, % [PM] 4.1.0 ISO-ized (SPRM 11617)
    %% stream no longer exists
    print_message(error, prologbeans(no_stream(SocketStream))),
    %% notify stream handler about the closed stream
    %% [PM] 4.2.1 This never worked in SP4 since the passed stream was SocketStream rather than the opaque stream.
    %%            I have updated the code to find the corresponding PBStream but I am not sure we should handle
    %%            the case when someone else closes the stream. Also, there is the (unlikely, but still) possibility
    %%            that a stream with the same address exists now, which we would then close without permission.
    %%            If we really want to do this we should consider using the id/1 stream property to robustly
    %%            keep track of our streams.
    (
       % [PM] 4.2.1 Disabled for now, see above
       % stream(PBStream,H), % [PM] 4.2.1 BT to find the corresponding opaque stream
       % % we only listen on input streams
       % pb_input_stream(PBStream, SocketStream) ->
       % thread_state(InStates, H, InThreadState, OutStates, OutThreadState), 
       % close_stream(PBStream, H, InThreadState, OutThreadState)
       % ; otherwise ->
      OutStates = InStates
    ).

listen_error(E, InStates, OutStates) :-
    print_message(error, prologbeans(listen(E))),
    OutStates = InStates.

thread_state([], H, InThreadState, OutStates, OutThreadState) :-
    default_thread_state(H,InThreadState),
    OutStates = [H-OutThreadState].

thread_state([K-KState|InStates], H, InThreadState, OutStates, OutThreadState) :-
    K == H, !,
    InThreadState = KState,
    OutStates = [H-OutThreadState|InStates].

thread_state([KKState|InStates], H, InThreadState, OutStates, OutThreadState) :-
    % KKState = K-_, K \== H,
    OutStates = [KKState|OutStates1],
    thread_state(InStates, H, InThreadState, OutStates1, OutThreadState).


default_thread_state(Thread, OutThreadState) :-
    InThreadState = [],
    call_handler_robust(Thread, initial_state, InThreadState, OutThreadState).



/* ----------------------------------------------------------------------
    Messages
   ---------------------------------------------------------------------- */

:- multifile user:generate_message_hook/3.
%%% if SICSTUS
/* SPRM 11714
:- dynamic   user:generate_message_hook/3.
*/
%%% endif SICSTUS

user:generate_message_hook(prologbeans(What)) --> !,
    pbmessage(What).

pbmessage(close_socket_h(H,Error)) -->
	['[PBEANS] - could not close socket for handler ~q: ~q'-[H,Error],nl].
pbmessage(close_socket_c(S,Error)) -->
	['[PBEANS] - could not close socket connection ~q: ~q'-[S,Error],nl].
pbmessage(no_handler(S)) -->
	['[PBEANS] - no stream handler found for ~q'-[S],nl].
pbmessage(handler_failure(H,G)) -->
	['[PBEANS] - ~q failed to handle ~q'-[H,G],nl].
pbmessage(handler_error(H,G,E)) -->
	['[PBEANS] - ~q could not handle ~q: ~q'-[H,G,E],nl].
pbmessage(no_stream(S)) -->
	['[PBEANS] - stream ~q no longer exists (closing)'-[S],nl].
pbmessage(close_stream_internal(S,Error)) -->
	['[PBEANS] - could not close stream ~q: ~q'-[S,Error],nl].
pbmessage(listen) -->
	['[PBEANS] - listen failed'-[],nl].
pbmessage(listen(Error)) -->
	['[PBEANS] - listen error: ~q'-[Error],nl].
pbmessage(denied(Host)) -->
	['[PBEANS] - denied connection from ~w'-[Host],nl].
pbmessage(read) -->
	['[PBEANS] - could not read data'-[],nl].
pbmessage(cyclic(Stream,Result)) -->
        {
          my_module(M),
          % [PM] 4.2.1 used to do, in effect, print(Result), which is a bad idea when Result is cyclic...
          G = M:write_term(Result, [quoted(true), max_depth(10)])
        },
	['[PBEANS] - cyclic result to ~q: ~@'-[Stream,G],nl].
