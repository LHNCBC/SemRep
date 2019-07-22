/* Copyright (C) 1990 Swedish Institute of Computer Science */
/* Modified 2000 by Malcolm Ryan to include the 'quit' message */

:- module(linda, [
	linda/0,
	linda/1
   ]).
:- meta_predicate
   %% [PM] 3.9.1 linda/1 takes goals as args so should always have been meta
   %%            Meta even more important now with the accept_hook.
   linda(:).

:- use_module(library(fastrw), [
	fast_read/1,
	fast_write/1
   ]).

:- use_module(library(sockets), [
	socket_server_open/2,
	socket_server_accept/4,
        socket_server_close/1,
	socket_select/7,
	current_host/1
   ]).

%%:- use_module(library(sockets), [
%%	socket/2,
%%	socket_bind/2,
%%	%% socket_buffering/4, [PM] 3.8.7 socket_select is fixed to work with default buffering
%%	socket_listen/2, 
%%	socket_select/5
%%   ]).
:- use_module(library(lists), [
	delete/3
   ]).

:- dynamic ts/1.
:- volatile ts/1.
:- dynamic tuple/1.
:- volatile tuple/1.
:- dynamic waiting_list/5.
:- volatile waiting_list/5.
:- dynamic linda_trace/0.
:- volatile linda_trace/0.
:- dynamic quit/0.
:- volatile quit/0.
:- dynamic server_accept_hook_dispatch/2. % [PM] 3.9.1

:- meta_predicate cond_to_client(+, 0, +).


linda(Options0) :-
   get_module(Options0, M0,Options1),
   nonvar(Options1),
   ( Options1 = _-_ -> % [PM] 3.9.1 traditional interface (Host:Port)-Goal)
       Options = [Options1]
   ; otherwise ->
       Options = Options1
   ),
   
   get_opt((Host:Port)-Goal0, Options, _-true),
   get_module(Goal0, M0, M,Goal1),
   Goal = M:Goal1,
   
   %% get_opt(accept_hook(AcceptHook0), Options, accept_hook(linda:server_accept_hook_default)),
   %% get_module(AcceptHook0, M0, MAccept,AcceptHook),
   %% set_server_accept_hook(MAccept, AcceptHook),
   get_opt(accept_hook(Client,Stream,AcceptHook0), Options, accept_hook(_,_,true)),
   get_module(AcceptHook0, M0, MAccept,AcceptHook),
   set_server_accept_hook(Client,Stream, MAccept:AcceptHook),

   create_server_socket(Socket, Host, Port),
   once(Goal),
   server1(Socket,[]).

linda :-
   linda([Addr-(format(user_error,'Server address is ~q~n',[Addr]), flush_output(user_error))]).


get_module(Goal0, M,Goal) :-
   prolog_flag(typein_module, M0),
   get_module(Goal0, M0, M,Goal).


get_module(Goal0, M0, M,Goal) :- var(Goal0), !, % probably an error
   Goal = Goal0,
   M = M0.
get_module(M0:Goal0, _M0, M,Goal) :- !,
   get_module(Goal0, M0, M,Goal).
get_module(Goal0, M0, M,Goal) :-
   M = M0,
   Goal = Goal0.


get_opt(Pattern, Options, _Default) :-
   member(Pattern, Options), !.
get_opt(Pattern, _Options, Default) :-
   Pattern = Default.


%% [PM] 3.9.1 accept all clients by default
server_accept_hook_default(_Client, _Stream).

%% dynamic
server_accept_hook_dispatch(Client, Stream) :-
   server_accept_hook_default(Client, Stream).

server_accept_hook(Client,Stream) :-
   server_accept_hook_dispatch(Client, Stream).

%% set_server_accept_hook(M, Pred) :-
%%    Goal =.. [Pred,Client,Stream], % Pred(Client,Stream)
%%    retractall(server_accept_hook_dispatch(_,_)),
%%    assert((server_accept_hook_dispatch(Client, Stream) :-
%%              M:Goal
%%           )).
set_server_accept_hook(Client,Stream,Goal) :-
   retractall(server_accept_hook_dispatch(_,_)),
   assert((server_accept_hook_dispatch(Client, Stream) :- Goal, !)).

server1(Socket, Streams) :- quit, Socket \== 'closed_socket', !,
        % [PM] 4.2 Stop accepting new connections but continue to process existing clients indefinitely SPRM 12089
	format(user_error,'---------- Closing listening socket~n',[]), flush_output(user_error),
        % [PM] 4.2 Close server socket so we can re-use it later. SPRM 12089.
        socket_server_close(Socket),
        server1('closed_socket', Streams).
server1(_Socket, []) :- quit, !,
        % Shutdown and no remaining clients. We are done.
        % _Socket == 'closed_socket' here
	retractall(quit).
server1(Socket, Streams0) :-
	wait_for_arrival(Socket,Streams0,ReadableStreams,Streams1),
	(   foreach(InStream,ReadableStreams),
	    fromto(Streams1,SS0,SS1,Streams2)
	do  server_one_stream(InStream,SS0,SS1)
	), !,
	server1(Socket, Streams2).

/* Waits for either one or all of the following:
	1) A connection is done to 'Socket'
        2) It is possible to read from a stream
*/

wait_for_arrival(Socket, Streams0, ReadableStreams, Streams) :-
        Timeout = 'off',
        % [PM] 4.2 continue processing clients even if we initated shutdown by closing the listening port. SPRM 12089
        ( Socket == 'closed_socket' ->
            Sockets = []
        ; otherwise ->
            Sockets = [Socket]
        ),
        trace_linda(before,socket_select(Sockets, Connecting, Streams0,ReadableStreams, [],WR, Timeout),Ti),
	socket_select(Sockets, Connecting, Streams0,ReadableStreams, [],WR, Timeout),
        trace_linda(after,socket_select(Sockets, Connecting, Streams0,ReadableStreams, [],WR, Timeout),Ti),
        ( Connecting = [ServerSocket|_] ->
           StreamOptions = [type(binary)],      % [PM] 4.0 FIXME: for now we cannot accept non-binary protocols
           socket_server_accept(ServerSocket, Client, Stream, StreamOptions),
           ( server_accept_hook(Client, Stream) ->
              format(user_error,'---------- Opened ~p ~q~n',[Client, Stream]), flush_output(user_error),
              Streams = [Stream|Streams0]
           ; otherwise ->
              format(user_error,'---------- Rejected ~p~n',[Client]), flush_output,
              close(Stream),
              Streams = Streams0
           )
        ; Connecting = [] ->
           Streams = Streams0
        ).

%% wait_for_arrival(Socket, Streams0, ReadableStreams, Streams) :-
%%         %% socket_select(Socket, NewStream, off, Streams0, ReadableStreams),
%%         %% new_stream_in_list(NewStream, Streams0, Streams).
%%         %% [PM] 3.9.1 Now get the address of the client. Suggested by Malcolm Ryan
%%         socket_select([server-Socket], NewTermStreams, off, Streams0, ReadableStreams),
%%         new_streams_in_list(NewTermStreams, Streams0, Streams).
%%
%% new_streams_in_list([], Streams, Streams).
%% new_streams_in_list([_Term-connection(Client,Stream) | NewTermStreams], 
%%         StreamsIn, StreamsOut) :-
%%    ( server_accept_hook(Client, Stream) ->
%%        format(user_error,'---------- Opened ~p ~q~n',[Client, Stream]),
%%        StreamsOut = [Stream | StreamsOut1]
%%    ; otherwise ->
%%        format(user_error,'---------- Rejected ~p~n',[Client]),
%%        close(Stream),
%%        StreamsOut = StreamsOut1
%%    ),
%%    new_streams_in_list(NewTermStreams, StreamsIn, StreamsOut1).

create_server_socket(Socket, Host, Port) :-
        ( var(Host) ->
           current_host(Host)
        ;  true
        ),
        socket_server_open(Port, Socket).

%% create_server_socket(Socket, Host, Port) :-
%%         socket('AF_INET', Socket),
%%         socket_bind(Socket, 'AF_INET'(Host,Port)),
%%         socket_listen(Socket, 5).

server_one_stream(InStream,SS0,SS1) :-
	set_input(InStream),
	set_output(InStream),
        %% [PM] 4.1.2 SPRM 11384, 11775. Make it more robust against misbehaving clients.
        %%      Note that the server side is still not immune against client crashes.
        catch(server_one_stream1(InStream,SS0,SS1),
              %% Treat connection reset as EOF
              error(system_error,system_error('SPIO_E_NET_CONNRESET')),
              server_one_stream(-1,InStream,SS0,SS1)).



server_one_stream1(InStream,SS0,SS1) :-
	get_byte(Protocol),     % [PM] 4.0 FIXME: Should change to text stream if protocol is 0'p
	server_one_stream(Protocol,InStream,SS0,SS1).

server_one_stream(-1,S,SS0,SS) :- /* end of file, that is, a broken connection*/ !,
	set_input(user_input),
	set_output(user_output),
        close(S),
	format(user_error,'---------- Closed ~q~n',[S]),
	delete(SS0,S,SS).
server_one_stream(Protocol,_Stream,SS,SS) :-
	get_protocol_byte(Protocol, Request),
	trace_linda(before,Request,Ti),
	perform_request(Request,Protocol),
	trace_linda(after,Request,Ti).


get_protocol_byte(0'f, Byte) :-
	get_byte(Byte).
get_protocol_byte(0'p, Byte) :-
	get_code(Byte).

%----------------------------------------
perform_request(0's,Protocol) :-  /* rd_noblock */ !,
	get_input(Protocol,Tuple),
	cond_to_client(Protocol,tuple(Tuple), Tuple).
perform_request(0'r,Protocol) :-  /* rd */ !,
	get_input(Protocol,Tuple),
	to_client_blocking(rd,Protocol,Tuple).
perform_request(0'R,Protocol) :- /* rd, conjunction */ !,
	get_input(Protocol,Tlist),
	(   member(T,Tlist),tuple(T) ->
	    to_client(Protocol,T)
	;   time_stamp(TimeStamp),
	    current_output(Stream),
	    assert_waiting_lists(Tlist,Protocol,TimeStamp,Stream,rd)
	).
perform_request(0'i,Protocol) :-  /* in */ !,
	get_input(Protocol,Tuple),
	to_client_blocking(in,Protocol,Tuple).
perform_request(0'I,Protocol) :- /* in, conjunction */ !,
	get_input(Protocol,Tlist),
	(   member(T,Tlist),tuple(T) ->
	    retract(tuple(T)),
	    to_client(Protocol,T)
	;   time_stamp(TimeStamp),
	    current_output(Stream),
	    assert_waiting_lists(Tlist,Protocol,TimeStamp,Stream,in)
	).
perform_request(0'j,Protocol) :-  /* in_noblock */ !,
	get_input(Protocol,Tuple),
	cond_to_client(Protocol,retract(tuple(Tuple)), Tuple).
perform_request(0'o,Protocol) :- /* out */ !,
	get_input(Protocol,Tuple),
	perform_out(Tuple).
perform_request(0'p,Protocol) :-  /* ping */ !,
	get_input(Protocol,In),
	ping_answer(In,Out),
	to_client(Protocol,Out).
perform_request(0'b,Protocol) :- /* bagof_rd_noblock */ !,
	get_input(Protocol,b(Template,Tuple,Bag)),
	tupleGoal(Tuple,Goal),
	(bagof(Template,Goal,Bag) ; Bag = []),
	to_client(Protocol,Bag).
perform_request(0'c,Protocol) :- /* call */ !,
	get_input(Protocol,Goal),
	cond_to_client(Protocol,call(Goal), Goal).
perform_request(0't,Protocol) :- /* trace */ !,
	get_input(Protocol,Q),
	trace_answer(Q,Repl),
	to_client(Protocol,Repl).
perform_request(0'q,Protocol) :- /* quit */ !,
	get_input(Protocol,_),
        ( quit ->
            % [PM] 4.2 We are already being shut down
            format(user_error,'---------- Received redundant Shutdown message. Waiting for Clients to close\n',[])
        ; otherwise ->
            format(user_error,'---------- Received Shutdown message. Waiting for Clients to close\n',[]),
            assert(quit)
        ).
perform_request(R, _Protocol) :-
	current_input(Stream),
	format(user_error,'*** Unknown request from ~q: ~q~n',[Stream,R]).

ping_answer(ping,pong) :- !.
ping_answer(X, illegal(X)).

%-----------------------------------------------------------------------------
perform_out(Tuple) :-
	retract(waiting_list(Tuple,Protocol,TS,WaitingStream,Op)), !,
	remove_disjunction(TS),
	to_clientS(Protocol,WaitingStream,Tuple),
	(   Op = rd ->
	    perform_out(Tuple)
	;   true
	).
perform_out(Tuple) :-
	assertz(tuple(Tuple)).

remove_disjunction(TS) :- TS>0, !,
	retractall(waiting_list(_,_,TS,_,_)).
remove_disjunction(_).	

%-----------------------------------------------------------------------------
trace_answer(Q,on) :- var(Q), linda_trace, !.
trace_answer(Q,off) :- var(Q), !.
trace_answer(on,on) :- linda_trace, !.
trace_answer(on,on) :- !, assert(linda_trace).
trace_answer(_,off) :- retractall(linda_trace).

%-----------------------------------------------------------------------------
time_stamp(T) :- retract(ts(T)), !, T1 is T+1, assert(ts(T1)).
time_stamp(1) :- assert(ts(2)).

assert_waiting_lists([],_Protocol,_TimeStamp,_Stream,_Op).
assert_waiting_lists([T|Ts],Protocol,TimeStamp,Stream,Op) :-
	assert_one_waiting(T,Protocol,TimeStamp,Stream,Op),
	assert_waiting_lists(Ts,Protocol,TimeStamp,Stream,Op).

assert_one_waiting(Tuple,Protocol,TimeStamp,Stream,Op) :-
	assertz(waiting_list(Tuple,Protocol,TimeStamp,Stream,Op)).

%-----------------------------------------------------------------------------
to_client_blocking(rd,Protocol,Tuple) :- 
	tuple(Tuple), !,
	to_client(Protocol,Tuple).
to_client_blocking(in,Protocol,Tuple) :- 
	retract(tuple(Tuple)), !,
	to_client(Protocol,Tuple).
to_client_blocking(Op,Protocol,Tuple) :- 
	current_output(Stream), 
	assert_one_waiting(Tuple,Protocol,0,Stream,Op).
%-----------------------------------------------------------------------------
tupleGoal(T,G) :- var(T), !, G = tuple(T).
tupleGoal(T,G) :- tupleGoal1(T,G).
tupleGoal1(V^T,VG) :- !, VG = V^G, tupleGoal1(T,G).
tupleGoal1(T,tuple(T)).

%-----------------------------------------------------------------------------
get_input(0'f, Data) :- fast_read(Data).
get_input(0'p, Data) :- read(Data).
	
put_output(0'f, Data):- fast_write(Data).
put_output(0'p, Data):- write_canonical(Data), write('.'), nl.

%-----------------------------------------------------------------------------
to_client(Protocol,C,Data) :- 
	put_output_byte(Protocol, C),
	put_output(Protocol,Data),
	flush_output.
to_client(Protocol,Data) :- 
	put_output(Protocol,Data),
	flush_output.

put_output_byte(0'f, C) :-
	put_byte(C).
put_output_byte(0'p, C) :-
	put_code(C).

to_clientS(Protocol,Stream,Data) :- 
	current_output(S),
	set_output(Stream),
	put_output(Protocol,Data),
	flush_output,
	set_output(S).
%-----------------------------------------------------------------------------
cond_to_client(Protocol,Cond, Data) :-
	call(Cond), !,
	to_client(Protocol,0's, Data).
cond_to_client(Protocol,_Cond,_Data) :-
	put_output_byte(Protocol, 0'f),
	flush_output.

%-----------------------------------------------------------------------------
trace_linda(before,R,Ti) :-
	linda_trace, !,
	write(user_error, '['),
        ( integer(R) ->
           put_code(user_error, R)
        ; writeq(user_error, R)
        ),
	write(user_error,'  '),
	flush_output(user_error),
	statistics(runtime,[Ti,_]).
trace_linda(after,R,Ti) :-
	linda_trace, !,
	statistics(runtime,[Tf,_]),
        ( integer(R) ->
           put_code(user_error, R)
        ; writeq(user_error, R)
        ),
	T is Tf-Ti,
	format(user_error,'(~w ms)]~n',[T]),
        flush_output(user_error).
trace_linda(_,_,0).
