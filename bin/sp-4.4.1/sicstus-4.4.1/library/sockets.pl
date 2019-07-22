/* -*- Mode:Prolog; -*-
   Copyright (C) 2005 Swedish Institute of Computer Science. */

:- module(sockets, [
	current_host/1,

%% [PM] 4.0 Should be replaced with non-determinate hostname_address/3
%%	hostname_address/2

	socket_client_open/3,
	socket_server_open/2,
	socket_server_open/3,
        socket_server_close/1,
	socket_server_accept/4,
	socket_select/7

		   ]).
:- use_module(library(types), [
	illarg/3,
        illarg/4,
	must_be/4
	]).


%@  @cindex sockets
%@  @cindex process communication
%@  @cindex communication, process
%@  This library package defines a number of predicates for
%@  communicating over sockets.

%@  
%@  To create a (bi-directional) stream connected to a remote server,
%@  use @code{socket_client_open/3}.

%@  
%@  To open a port for remote clients to connect to, use
%@  @code{socket_server_open/[2,3]} and to open a stream to a connecting
%@  client, use @code{socket_server_accept/4}.

%@  
%@  To be able to multiplex input and output from several streams (not
%@  necesessarily socket streams) and incoming connections, use
%@  @code{socket_select/7}.

%@  
%@  @cindex address, socket
%@  @cindex socket address

%@  When opening a client or server socket a @dfn{socket address}
%@  needs to be specified. The address specifies the address family
%@  and family-specific information. The following formats are
%@  supported for socket addresses:

%@  @table @code
%@  @item inet(@var{Nodename},@var{Servname})
%@  @itemx @var{Nodename}:@var{Servname}
%@  @itemx @var{Servname}
%@  This specifies the address for and ordinary internet
%@  socket (@code{AF_INET} or @code{AF_INET6}).

%@  @var{Nodename} is the internet address of the remote host, as an
%@  atom, something like @code{'www.sics.se'} or
%@  @code{'193.10.64.51'}. The empty nodename @code{''} (the default), 
%@  has special meaning, see the documentation for
%@  @code{socket_client_open/3} and @code{socket_server_open/[2,3]}.

%@  @var{Servname} is either a port number as an atom of decimal
%@  digits or as an integer, e.g.@: @code{'80'}, or @code{80};
%@  alternatively some @dfn{well known port names} can be used, e.g.@:
%@  @code{'http'}. The set of well known port names is OS specific,
%@  portable code should use integer port numbers.

%@  @var{Servname} can also be a variable when opening a server socket
%@  with @code{socket_server_open/[2,3]}. In this case a available
%@  port is assigned automatically and Servname is bound to it.
%@  

%@  @item unix(@var{Path}) @since{release 4.0.3}

%@  A Unix domain (@code{AF_UNIX}) socket is opened at the specified
%@  file system location. This is only supported on Unix-like
%@  platforms.

%@  @var{Path} is a file-name and is passed to
%@  @code{absolute_file_name/2}. There may be platform-specific
%@  restrictions on the length of the resulting pathname and the file
%@  system containing it.

%@  @end table

%@  
%@  All streams below can be read from as well as written to.  All I/O
%@  predicates operating on streams can be used, for example
%@  @code{get_code/2}, @code{get_byte/2},
%@  @code{read/2}, @code{write/2}, @code{format/3},
%@  @code{current_stream/3},
%@  etc.  The predicates that create streams take options similar to
%@  @code{open/4}, e.g.@: to specify whether the stream is binary
%@  (the default) or text.
%@  
%@  @table @code


%@  @item socket_client_open(@var{+Addr}, @var{-Stream}, @var{+Options})
%@  @PLXindex {socket_client_open/3 (sockets)}
%@  Creates a stream @var{Stream} connected to address @var{Addr}.

%@  See above for the allowed address formats. If the nodename is
%@  empty (@code{''}) then a connection is made to the local machine.


%% %@  @var{Addr} can be:
%% %@  @table @code
%% %@  @item @var{Host}:@var{Port}
%%
%% %@  Connect to the machine with address @var{Host} (a host name or
%% %@  host address) at port @var{Port} (a port number or service name).
%% %@  The @var{Host} should be an atom, e.g.@: @code{'www.sics.se'}. The
%% %@  @var{Port} is either a port number as an integer or atom, e.g.@:
%% %@  @code{80}, or @code{'80'}; alternatively some @dfn{well known
%% %@  port names} can be used, e.g.@: @code{'http'}. The set of well
%% %@  known port names is OS specific, portable code should use integer
%% %@  port numbers.
%% %@  @end table

%@  
%@  The stream is created using options from @var{Options}. Supported
%@  options include:
%@  @table @code
%@  @item type(binary)
%@  Create a binary stream (the default).
%@  @item type(text)
%@  Create a text stream. The default encoding is Latin 1.
%@  @item eof_action(@var{Action})
%@  end of file action, as for @code{open/4}.
%@  @c @item flush(auto)
%@  @c As for open/4.
%@  @item encoding(@var{ENCODING}) @since{release 4.1}
%@  As for open/4. Implies @code{type(text)}.
%@  @item eol(@var{Eol}) @since{release 4.1}
%@  As for open/4. Implies @code{type(text)}.
%@  @end table
%@  
%@  To create a binary stream to some web server @code{www.sics.se}, you
%@  would do e.g.@:
%@  @example
%@  @group
%@  | ?- socket_client_open('www.sics.se':80, Stream, [type(binary)]).
%@  @end group
%@  @end example
%@  
%@  @noindent or, to make a text (Latin 1)
%@  stream to a @code{daytime} service in Hong Kong you could do:
%@  
%@  @example
%@  @group
%@  | ?- socket_client_open('stdtime.gov.hk':daytime, S, [type(text)]),
%@       read_line(S, L),
%@       format('~s', [L]).
%@  @end group
%@  @end example
%@  
%@  See the source code for @code{library('linda/client')} for a
%@  simple client.
%@  
socket_client_open(Addr1, Stream, Options) :-
   Goal = socket_client_open(Addr1, Stream, Options),
   ArgNo = 3,
   OptionClass = socket_stream_open,
   socket_option_bits([direction(readwrite)|Options], OptionClass, OptionBits0, Options1, Goal,ArgNo),
   socket_option_encoding(Options1, Encoding, OptionBits0,OptionBits1, Goal,ArgNo),
   ( canonical_address(Addr1, Addr),
     ( Addr = unix(Path) ->
        'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_UNIX'(FamilyBitsUnix),
        OptionBits2 is OptionBits1 \/ FamilyBitsUnix,
        Servname = 'AF_UNIX',
        % [PM] 4.3 no_normalize_ is overkill since AF_UNIX is (currently) unavailable on Windows.
        prolog:absolute_file_name(Path, Nodename, [no_normalize_(true)], Goal, 1) % Addr is #1 not ArgNo
     ;  Addr = inet(Nodename,Servname) ->
        'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_INET'(FamilyBitsInet),
        OptionBits2 is OptionBits1 \/ FamilyBitsInet,
        atom(Nodename)
     ) ->
     true
   ; otherwise ->
     illarg(domain(term,socket_address), Goal, 1) % Addr is #1, not ArgNo
   ),
   prolog:'$socket_open'(Nodename, Servname, OptionBits2, Encoding, RawStream),
   stream_code(Stream, RawStream).

%% Filter out the options that correspond to flag-bits.
socket_option_bits(Options, OptionClass, OptionBits, Options1, Goal,ArgNo) :-
   must_be(Options, proper_list(nonvar), Goal,ArgNo),
   socket_option_bits1(Options, OptionClass, 0,OptionBits, Options1, Goal,ArgNo).

socket_option_bits1([], _OptionClass, OptionBits0,OptionBits, Options1, _Goal,_ArgNo) :-
   OptionBits = OptionBits0,
   Options1 = [].
socket_option_bits1([Option|Options], OptionClass, OptionBits0,OptionBits, Options1, Goal,ArgNo) :-
   ( socket_option_bit(Option, OptionClass, OptionBit, Goal,ArgNo) ->
      OptionBits1 is OptionBits0 \/ OptionBit,
      Options1 = Options2
   ; otherwise ->
     OptionBits1 = OptionBits0,
     Options1 = [Option|Options2]
   ),
   socket_option_bits1(Options, OptionClass, OptionBits1,OptionBits, Options2, Goal,ArgNo).

% Caller ensures Option is nonvar
socket_option_bit(type(X), OptionClass, OptionBit, Goal,ArgNo) :-
   ( atom(X), mode_type_bit(X, OptionClass, OptionBit0) ->
      OptionBit = OptionBit0
   ; mode_type_bit(_, OptionClass, _OptionBit0) ->
      findall(T, mode_type_bit(T, OptionClass, _), Ts),
      must_be(X, oneof(Ts), Goal,ArgNo)
   ).
socket_option_bit(eof_action(X), OptionClass, OptionBit, Goal,ArgNo) :-
   ( atom(X), mode_eof_action_bit(X, OptionClass, OptionBit0) ->
      OptionBit = OptionBit0
   ; mode_eof_action_bit(_, OptionClass, _OptionBit0) ->
      findall(T, mode_eof_action_bit(T, OptionClass, _), Ts),
      must_be(X, oneof(Ts), Goal,ArgNo)
   ).
socket_option_bit(direction(X), OptionClass, OptionBit, Goal,ArgNo) :-
   ( atom(X), mode_direction_bit(X, OptionClass, OptionBit0) ->
      OptionBit = OptionBit0
   ; mode_direction_bit(_, OptionClass, _OptionBit0) ->
      findall(T, mode_direction_bit(T, OptionClass, _), Ts),
      must_be(X, oneof(Ts), Goal,ArgNo)
   ).
socket_option_bit(flush(X), OptionClass, OptionBit, Goal,ArgNo) :-
   ( atom(X), mode_flush_bit(X, OptionClass, OptionBit0) ->
      OptionBit = OptionBit0
   ; mode_flush_bit(_, OptionClass, _OptionBit0) ->
      findall(T, mode_flush_bit(T, OptionClass, _), Ts),
      must_be(X, oneof(Ts), Goal,ArgNo)
   ).
%% socket_option_bit(address_family(X), OptionClass, OptionBits, Goal,ArgNo) :-
%%    ( atom(X), address_family_bits(X, OptionClass, OptionBits0) ->
%%       OptionBits = OptionBits0
%%    ; address_family_bits(_, OptionClass, OptionBits0) ->
%%       findall(T, address_family_bits(T, OptionClass, _), Ts),
%%       must_be(X, oneof(Ts), Goal,ArgNo)
%%    ).

socket_option_bit(loopback(Bool),   socket_server_open, Bits, Goal,ArgNo) :-
        bool_select(Bool, 0x0001, Bits, Goal,ArgNo).        % PROLOG_SOCKET_LISTEN_OPTION_LOOPBACK
socket_option_bit(numeric_nodename(Bool), socket_server_open, Bits, Goal,ArgNo) :-
        bool_select(Bool, 0x0002, Bits, Goal,ArgNo).        % PROLOG_SOCKET_LISTEN_OPTION_NUMERIC_NODENAME
socket_option_bit(numeric_servname(Bool), socket_server_open, Bits, Goal,ArgNo) :-
        bool_select(Bool, 0x0004, Bits, Goal,ArgNo).        % PROLOG_SOCKET_LISTEN_OPTION_NUMERIC_SERVNAME
socket_option_bit(reuseaddr(Bool), socket_server_open, Bits, Goal,ArgNo) :-
        bool_select(Bool, 0x0008, Bits, Goal,ArgNo).        % PROLOG_SOCKET_LISTEN_OPTION_REUSEADDR

bool_select(Bool, TrueBits, Bits, Goal,ArgNo) :-
        FalseBits = 0,
        bool_select(Bool, TrueBits, FalseBits, Bits, Goal,ArgNo).

bool_select(Bool, _TrueBits, _FalseBits, _Bits, Goal,ArgNo) :- var(Bool), !,
        illarg(var,Goal,ArgNo).
bool_select(true, TrueBits, _FalseBits, Bits, _Goal,_ArgNo) :- !,
        Bits = TrueBits.
bool_select(false, _TrueBits, FalseBits, Bits, _Goal,_ArgNo) :- !,
        Bits = FalseBits.
bool_select(Bool, _TrueBits, _FalseBits, _Bits, Goal,ArgNo) :- !,
        must_be(Bool, boolean, Goal,ArgNo).




mode_direction_bit(read,      socket_stream_open, 0x0002). % read modeRead
mode_direction_bit(write,     socket_stream_open, 0x0004). % write modeWrite
mode_direction_bit(readwrite, socket_stream_open, 0x0006). % readwrite modeRead\/modeWrite

%% mode_bit(modeTTY, _OptionClass, 0x0008).

mode_eof_action_bit(eof,      socket_stream_open, 0x0200). % eof_action(eof) modeEofOnEof
mode_eof_action_bit(reset,    socket_stream_open, 0x0400). % eof_action(reset) modeResetOnEof

mode_type_bit(binary, socket_stream_open,         0x0800). % type(binary) modeBinary
mode_type_bit(text,   socket_stream_open,         0x4000). % type(text) modeText

mode_flush_bit(auto,   socket_stream_open,        0x8000). % flush(auto) modeAutoFlush


%% address_family_bits(Family, socket_stream_open, Bits) :-
%%    address_family_bits(Family, Bits).
%% address_family_bits(Family, socket_server_open, Bits) :-
%%    address_family_bits(Family, Bits).

%% address_family_bits(inet, Bits) :- 'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_INET'(Bits).
%% address_family_bits(inet4, Bits) :- 'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_INET4'(Bits).
%% address_family_bits(inet6, Bits) :- 'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_INET6'(Bits).
%% address_family_bits(unix, Bits) :- 'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_UNIX'(Bits).

'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_INET'( 0x000000).
%% 'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_INET4'(0x100000). % modePrivate1
%% 'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_INET6'(0x200000). % modePrivate2
'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_UNIX'( 0x300000). % (modePrivate1 | modePrivate2)
%% 'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_MASK'( 0x300000). % (modePrivate1 | modePrivate2)


%@  
%@  @item socket_server_open(@var{?Addr}, @var{-ServerSocket}, @var{+Options}) @since{release 4.0.3}
%@  @PLXindex {socket_server_open/[2,3] (sockets)}
%@  Create a server socket @var{ServerSocket} that listens on address @var{Addr}.

%@  See above for the allowed address formats. If the nodename is
%@  empty (@code{''}) then any remote client machine is allowed to
%@  connect unless the option @code{loopback(true)} is also specified.

%@  @var{Addr}
%@  can specify an internet address where the port is a variable in
%@  which case a free port number is used and @var{Port} is bound to
%@  it. The common case is that Addr is a numeric port number or a
%@  variable that becomes bound to a free port number.

%@
%@  The created server socket should be closed with
%@  @code{socket_server_close/1} eventually. Incoming connection can
%@  be accepted
%@  with @code{socket_server_accept/4} and waited for with
%@  @code{socket_select/7}.
%@  See the source code for @code{library('linda/server')} for a
%@  simple server that uses this predicate.
%@  
%@  @var{Options} is a list of options, currently 
%@  @table @code
%@  @item reuseaddr(Bool) @since{release 4.0.3}
%@  @var{Bool} is either @code{true} or @code{false} (the default). If
%@  @code{true} then allow reuse of local addresses. For internet
%@  sockets this corresponds to the @code{SO_REUSEADDR} socket
%@  option. For unix domain sockets this means that the file will be
%@  deleted, if present, before opening.

%@  @item numeric_nodename(Bool) @since{release 4.0.3}

%@  @var{Bool} is either @code{true} or @code{false} (the default). If
%@  @code{true} then the nodename of an internet address will be
%@  treated as a numerical address and no name lookup will be
%@  performed.

%@  @item numeric_servname(Bool) @since{release 4.0.3}

%@  @var{Bool} is either @code{true} or @code{false} (the default). If
%@  @code{true} then the servname of an internet address will be
%@  treated as a numerical port number and no lookup of @use{well
%@  known port names} will be performed.

%@  @item loopback(Bool) @since{release 4.0.3}

%@  @var{Bool} is either @code{true} or @code{false} (the default). If
%@  @code{true} then the nodename will be ignored and the socket will
%@  only listen to connection from the loopback device, i.e.@: the
%@  local machine.

%@  @end table


%@  @item socket_server_open(@var{?Port}, @var{-ServerSocket})
%% %@  @PLXindex {socket_server_open/2 (sockets)}
%@  The same as
%@  @code{socket_server_open(@var{Port}, @var{ServerSocket}, [])}.

%% %@  Create a server socket @var{ServerSocket} that listens on port
%% %@  @var{Port}. Port can be either an integer port number or an atomic
%% %@  service name, see @code{socket_client_open/3} for details. Port can
%% %@  also be a variable in which case a free port number is used and
%% %@  @var{Port} is bound to it.
%% %@  The created server socket should be closed with
%% %@  @code{socket_server_close/1} eventually. Incoming connection can
%% %@  be accepted
%% %@  with @code{socket_server_accept/4} and waited for with
%% %@  @code{socket_select/7}.
%% %@  See the source code for @code{library('linda/server')} for a
%% %@  simple server that uses this predicate.
socket_server_open(Addr, Socket) :-
   Options = [],
   Goal = socket_server_open(Addr, Socket),
   ArgNo = 0,
   socket_server1(Addr, Socket, Options, Goal,ArgNo).

socket_server_open(Addr, Socket, Options) :-
   Goal = socket_server_open(Addr, Socket, Options),
   ArgNo = 3,
   socket_server1(Addr, Socket, Options, Goal,ArgNo).

%% socket_server_open(Nodename, Servname, Socket, Options) :-
%%    Goal = socket_server_open(Nodename, Servname, Socket, Options),
%%    ArgNo = 4,
%%    socket_server1(Nodename, Servname, Socket, Options, Goal,ArgNo).

%% %% Can pass Nodename:Servename instead of just Servname
%% socket_server_parse_servname(Addr, Servname, Nodename) :-
%%         nonvar(Addr),
%%         Addr = Nodename1:Servname1,
%%         !,
%%         Servname = Servname1,
%%         Nodename = Nodename1.
%% socket_server_parse_servname(Addr, Servname, Nodename) :-
%%         Servname = Addr,
%%         Nodename = ''.

canonical_address(Addr1, Addr) :- var(Addr1), !, % unbound port
        Nodename = '',
        Servname = Addr1,
        Addr = inet(Nodename, Servname).
canonical_address(Addr1, Addr) :- atomic(Addr1), !,
        %% [PM] 4.0.3+ SPRM 10872
        Nodename = '',
        Servname = Addr1,
        Addr = inet(Nodename, Servname).
canonical_address((Nodename:Servname), inet(Nodename,Servname)). % legacy (support indefinitely!)
canonical_address(inet(Nodename,Servname), inet(Nodename,Servname)).
%% not yet implemented
%% canonical_address(inet4(Nodename,Servname), inet4(Nodename,Servname)).
%% canonical_address(inet6(Nodename,Servname), inet6(Nodename,Servname)).
canonical_address(unix(Path), unix(Path)).

socket_server1(Addr0, Socket, Options, Goal,ArgNo) :-
   OptionClass = socket_server_open,
   socket_option_bits(Options, OptionClass, OptionBits1, Options1, Goal,ArgNo),
   %% [PM] 4.1 there should be no unhandled options left
   ( Options1 = [Opt|_] ->
      illarg(domain(term, socket_server_option), Goal,ArgNo, Opt)
   ; true
   ),
   ( canonical_address(Addr0, Addr),
     ( Addr = unix(Path) ->
        'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_UNIX'(FamilyBitsUnix),
        OptionBits2 is OptionBits1 \/ FamilyBitsUnix,
        Servname = 'AF_UNIX',
        % [PM] 4.3 no_normalize_ is overkill since AF_UNIX is (currently) unavailable on Windows. (but, in the future, normalize may make sense on OS X for instance).
        prolog:absolute_file_name(Path, Nodename, [no_normalize_(true)], Goal, 1) % The pathname arg is #1 not ArgNo
     ; Addr = inet(Nodename, Servname) ->
	atom(Nodename), % [PM] 4.3.2 otherwise fail into domain error (prolog_socket_listen_fail_on_interrupt() expects atom)
        'PROLOG_SOCKET_OPEN_OPTION_ADDRESS_FAMILY_INET'(FamilyBitsInet),
        OptionBits2 is OptionBits1 \/ FamilyBitsInet
     ) ->
        true
   ; otherwise ->
        illarg(domain(term,socket_address), Goal, 1) % Addr is #1, not ArgNo
   ),
   prolog:'$socket_listen'(Nodename, Servname, OptionBits2, Options1, RawSocket),
   socket_raw(Socket, RawSocket).



%@  @item socket_server_accept(@var{+ServerSocket}, @var{-Client}, @var{-Stream}, @var{+StreamOptions})
%@  @PLXindex {socket_server_accept/4 (sockets)}
%@  The first connection to socket @var{ServerSocket} is extracted,
%@  blocking if necessary.  The stream @var{Stream} is created on this
%@  connection using @var{StreamOptions} as for
%@  @code{socket_client_open/3}. @var{Client} will be unified with an atom containing
%@  the numerical Internet host address of the connecting client.

%@  Note that the stream will be @code{type(binary)} unless
%@  @code{type(text)} is specified either explicitly or implicitly
%@  with @code{encoding/1} or other text-only options.
socket_server_accept(Socket, ClientNodename, Stream, Options) :-
   Goal = socket_server_accept(Socket, ClientNodename, Stream, Options),
   ArgNo = 4,
   socket_option_bits(Options, socket_server_accept, AcceptOptionBits, Options1, Goal,ArgNo),
   socket_option_bits(Options1, socket_stream_open, OpenOptionBits, Options2, Goal,ArgNo),
   OptionBits0 is AcceptOptionBits \/ OpenOptionBits,
   socket_option_encoding(Options2, Encoding, OptionBits0,OptionBits, Goal,ArgNo),
   socket_raw(Socket, RawSocket),
   prolog:'$socket_accept'(RawSocket, OptionBits, Encoding, RawNodename, _RawServname, RawStream),
   ClientNodename = RawNodename,
   stream_code(Stream, RawStream).


% default is modeBinary
add_default_open_option_mode_bits(OpenOptionBits0, OpenOptionBits) :-
   mode_type_bit(binary, socket_stream_open,         ModeBinary),
   OpenOptionBits0 /\ ModeBinary =\= 0, !,
   OpenOptionBits = OpenOptionBits0.
add_default_open_option_mode_bits(OpenOptionBits0, OpenOptionBits) :-
   mode_type_bit(text, socket_stream_open,         ModeText),
   OpenOptionBits0 /\ ModeText =\= 0, !,
   OpenOptionBits = OpenOptionBits0.
add_default_open_option_mode_bits(OpenOptionBits0, OpenOptionBits) :-
   mode_type_bit(binary, socket_stream_open,         ModeBinary), !,
   OpenOptionBits is OpenOptionBits0 \/ ModeBinary.

socket_option_encoding(Options1, Encoding, OptionBits0,OptionBits1, Goal,ArgNo) :-
        %% [] means "not specified", it does not mean "empty list" in this context.
        Encoding0 = [],
        socket_option_encoding(Options1, Encoding0,Encoding, OptionBits0,OptionBits1, Goal,ArgNo).


socket_option_encoding([], Encoding0,Encoding, OptionBits0,OptionBits, _Goal,_ArgNo) :-
        add_default_open_option_mode_bits(OptionBits0, OptionBits),
        Encoding = Encoding0.
socket_option_encoding([Opt|Options0], Encoding0,Encoding, OptionBits0,OptionBits, Goal,ArgNo) :- !,
        merge_encoding_option(Opt, Encoding0,Encoding1, OptionBits0,OptionBits1, Goal, ArgNo),
        socket_option_encoding(Options0, Encoding1,Encoding, OptionBits1,OptionBits, Goal,ArgNo).

merge_encoding_option(Opt, _Encoding0, _Encoding1, _OptionBits0,_OptionBits, Goal,ArgNo) :- var(Opt), !,
       illarg(var, Goal,ArgNo).                                                       
merge_encoding_option(Opt, Encoding0, Encoding1, OptionBits0,OptionBits, Goal,ArgNo) :-
        Opt = encoding(Enc), !,
        must_be(Enc, atom, Goal,ArgNo),
        mode_type_bit(binary, socket_stream_open, ModeBinary),
        ( OptionBits0 /\ ModeBinary =\= 0 ->
          illarg(consistency(type(binary),Opt,''), Goal,ArgNo)
        ; true
        ),
        %% encoding/1 option implies text (and would have aleready raised an error if explicit binary)
        mode_type_bit(text, socket_stream_open, ModeText),
        OptionBits is OptionBits0 \/ ModeText,
        ( Encoding0 = encoding(_Enc,BOM,EOL) ->
          Encoding1 = encoding(Enc,BOM,EOL)
        ; Encoding1 = encoding(Enc,default,default)
        ).
merge_encoding_option(Opt, Encoding0, Encoding1, OptionBits0,OptionBits, Goal,ArgNo) :-
        Opt = eol(EOL), !,
        must_be(EOL, oneof([lf,crlf,auto,default]), Goal,ArgNo),
        mode_type_bit(binary, socket_stream_open, ModeBinary),
        ( OptionBits0 /\ ModeBinary =\= 0 ->
          illarg(consistency(type(binary),Opt,''), Goal,ArgNo)
        ; true
        ),
        %% eol/1 option implies text (and would have aleready raised an error if explicit binary)
        mode_type_bit(text, socket_stream_open, ModeText),
        OptionBits is OptionBits0 \/ ModeText,
        ( Encoding0 = encoding(Enc,BOM,_EOL) ->
          Encoding1 = encoding(Enc,BOM,EOL)
        ; Encoding1 = encoding('ISO-8859-1',default,EOL)
        ).
merge_encoding_option(Opt, _Encoding0, _Encoding1, _OptionBits0,_OptionBits, Goal,ArgNo) :-
         illarg(domain(term, socket_option), Goal,ArgNo, Opt).


%@  @item socket_server_close(@var{+ServerSocket})
%@  @PLXindex {socket_server_close/1 (sockets)}
%@  Close the server socket @var{ServerSocket} and stop listening on
%@  its port.
socket_server_close(Socket) :-
   socket_raw(Socket, RawSocket),
   prolog:'$socket_close'(RawSocket).


%@  @item socket_select(@var{+ServerSockets},@var{-SReady}, @var{+ReadStreams},@var{-RReady}, @var{+WriteStreams},@var{-WReady}, @var{+Timeout})
%@  @PLXindex {socket_select/7 (sockets)}
%@  Check for server sockets with incoming connections (i.e.@: ready
%@  for @code{socket_server_accept/4}), streams on @var{ReadStreams}
%@  ready for input, and streams on @var{WriteStreams}
%@  ready for output. The streams can be any kind of streams, they need
%@  not be socket streams. The ready server sockets are returned (in the same
%@  order) in @var{SReady}, the ready input streams in @var{RReady},
%@  and the ready output streams in @var{WReady}.
%@  
%@  An input (output) stream is ready for input (output) when an
%@  @dfn{item} can be read (written) without blocking. An item
%@  is a character for text streams and a byte for binary streams.
%@  Note that a stream is considered ready for I/O if the
%@  corresponding I/O operation will raise an error (such as if the
%@  stream is past end of stream).
%@  
%@  Each entry in the input lists @var{ServerSockets},
%@  @var{ReadStreams}, and @var{WriteStreams} can be either a server
%@  socket or stream respectively or a term
%@  @code{@var{Term}-@var{Entry}} where @var{Entry} is the server
%@  socket or stream and @var{Term} is some arbitrary term used for
%@  book-keeping. If an entry is associated with a term in this way
%@  then so will the corresponding ready entry.
%@  
%@  If @var{TimeOut} is instantiated to @code{off}, the predicate
%@  waits until something is available.  If @var{TimeOut} is a nonzero
%@  number (integer or floating point), then the predicate waits at
%@  most that number of seconds before returning. For backward
%@  compatibility, if @var{TimeOut} is @var{S:U} the predicate waits
%@  at most @var{S} seconds and @var{U} microseconds. If there is a
%@  timeout, all ready lists are unified with @code{[]}.
%@  
%@  See the source code for @code{library('linda/server')} for a
%@  simple server that uses this predicate.
socket_select(ListenSockets,LReady, ReadStreams,RReady, WriteStreams,WReady, Timeout) :-
        prolog:socket_select(ListenSockets,LReady, ReadStreams,RReady, WriteStreams,WReady, Timeout).

socket_raw('$socket'(Raw), Raw).

%@  @item current_host(@var{?HostName})
%@  @PLXindex {current_host/1 (sockets)}
%@  @var{HostName} is unified with the fully qualified name of the
%@  machine that the process is executing on. The call will also succeed if
%@  @var{HostName} is instantiated to the unqualified name of the
%@  machine in lower case. @strong{Please note:} this predicate will
%@  fail if there are errors, e.g.@: if no domain has been
%@  configured. 

current_host(Host) :-
  prolog:'$hostname'(HostName, Domain),
  ( Host = HostName ->
     true
  ; atom(Host) ->
     atom_concat(Host, Domain, HostName),
     ! % [PM] 4.2.1 redundant, for detcheck.
  ).

%@  @end table

