
/****************************************************************************
*
*                          PUBLIC DOMAIN NOTICE                         
*         Lister Hill National Center for Biomedical Communications
*                      National Library of Medicine
*                      National Institues of Health
*           United States Department of Health and Human Services
*                                                                         
*  This software is a United States Government Work under the terms of the
*  United States Copyright Act. It was written as part of the authors'
*  official duties as United States Government employees and contractors
*  and thus cannot be copyrighted. This software is freely available
*  to the public for use. The National Library of Medicine and the
*  United States Government have not placed any restriction on its
*  use or reproduction.
*                                                                        
*  Although all reasonable efforts have been taken to ensure the accuracy 
*  and reliability of the software and data, the National Library of Medicine
*  and the United States Government do not and cannot warrant the performance
*  or results that may be obtained by using this software or data.
*  The National Library of Medicine and the U.S. Government disclaim all
*  warranties, expressed or implied, including warranties of performance,
*  merchantability or fitness for any particular purpose.
*                                                                         
*  For full details, please see the MetaMap Terms & Conditions, available at
*  https://metamap.nlm.nih.gov/MMTnCs.shtml.
*
***************************************************************************/

:- module(skr_tcp, [
	establish_tcp_connection/4
    ]).

:- use_module(skr(skr_utilities), [
	fatal_error/2
    ]).

:- use_module(library(sockets), [
	socket_client_open/3
	% tcp_connect/2,
	% tcp_input_stream/2,
	% tcp_output_stream/2,
	% tcp_shutdown/1	
   ]).

establish_tcp_connection(ServerName, ServerHost, Port, Stream) :-
	test_tcp_connect(ServerName, ServerHost, Port, Stream).
	% test_tcp_input_stream(Socket,  Port, ServerAddress, StreamIn),
	% test_tcp_output_stream(Socket, Port, ServerAddress, StreamOut).

test_tcp_connect(ServerName, ServerHost, Port, Stream) :-
	on_exception(ExceptionCode,
		     socket_client_open(inet(ServerHost,Port), Stream, [type(text)]),
		     signal_tcp_error(socket_client_open,
				      ServerName, ServerHost, Port, ExceptionCode)).

signal_tcp_error(Predicate, ServerName, Host, Port, ExceptionCode) :-
	fatal_error('Calling ~w for ~w Server on host ~w and port ~w:~n~w~n',
		    [Predicate, ServerName, Host, Port, ExceptionCode]).
