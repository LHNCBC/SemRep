
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

:- module(server_choice, [
	get_server_streams/1,
	get_server_stream/2
    ]).

:- use_module(skr(skr), [
	stop_and_halt/0
   ]).

:- use_module(skr(skr_utilities), [
	ensure_number/2,
	send_message/2
   ]).

:- use_module(skr_lib(nls_strings), [
	atom_codes_list/2,
	split_string_completely/3,
	trim_and_compress_whitespace/2
   ]).

:- use_module(skr_lib(nls_system), [
	control_option/1,
	control_value/2
   ]).

:- use_module(skr_lib(skr_tcp), [
	establish_tcp_connection/4
   ]).

:- use_module(library(random), [
	random_member/2
   ]).

:- use_module(library(system), [
	environ/2
   ]).

get_server_streams(TaggerServerStream-WSDServerStream) :-
	get_server_stream('TAGGER',  TaggerServerStream),
	get_server_stream('WSD',     WSDServerStream).
	
% ServerType is    'WSD',              'TAGGER',              or 'LEXICON'.
% ControlValue is  'WSD_SERVER',       'TAGGER_SERVER',       or 'LEXICON_SERVER'.
% HostsEnvVar is   'WSD_SERVER_HOSTS', 'TAGGER_SERVER_HOSTS', or 'LEXICON_SERVER_HOSTS'.
% PortEnvVar is    'WSD_SERVER_PORT', ' TAGGER_SERVER_PORT',  or 'LEXICON_SERVER_PORT'.

% This code simply avoids duplicating the same code for each of the various servers.

get_server_stream('TAGGER', ServerStream) :-
	( control_option(no_tagging) ->
	  ServerStream = ''
	; get_server_stream_1('TAGGER', ServerStream)
	).
get_server_stream('WSD', ServerStream) :-
	( \+ control_option(word_sense_disambiguation) ->
	  ServerStream = ''
	; get_server_stream_1('WSD', ServerStream)
	).

get_server_stream_1(ServerType, ServerStream) :-
	atom_codes(ServerType, ServerTypeCodes),
	get_control_value(ServerTypeCodes, ControlValueCodes),
	atom_codes(ControlValue, ControlValueCodes),
	get_hosts_env_var(ControlValueCodes, HostsEnvVar),
	get_port_env_var(ControlValueCodes, PortEnvVar),
	get_server_stream_aux(ServerType, ControlValue, HostsEnvVar, PortEnvVar, ServerStream).

get_control_value(ServerTypeCodes, ControlValueCodes) :-
	append(ServerTypeCodes, "_SERVER", ControlValueCodes).

get_hosts_env_var(ServerTypeCodes, HostsEnvVar) :-
	append(ServerTypeCodes, "_HOSTS", HostsEnvVarCodes),
	atom_codes(HostsEnvVar, HostsEnvVarCodes).

get_port_env_var(ServerTypeCodes, PortEnvVar) :-
	append(ServerTypeCodes, "_PORT", PortEnvVarCodes),
	atom_codes(PortEnvVar, PortEnvVarCodes).

get_server_stream_aux(ServerType, ControlValue, HostsEnvVar, PortEnvVar, ServerStream) :-
        environ(PortEnvVar, ServerPortAtom),
	ensure_number(ServerPortAtom, ServerPort),
	( control_value(ControlValue, ChosenServerHost) ->
	  UserChoice = ChosenServerHost,
	  ServerHosts = [ChosenServerHost],
          ServerMessage = '***USER SPECIFIED*** '
	; UserChoice is 0,
	  environ(HostsEnvVar, ServerHostsEnv),
	  atom_codes(ServerHostsEnv, ServerHostsChars0),
	  trim_and_compress_whitespace(ServerHostsChars0, ServerHostsChars),
	  split_string_completely(ServerHostsChars, " ", ServerHostsStrings),
	  atom_codes_list(ServerHosts, ServerHostsStrings),
          ServerMessage = ''
	),
	repeat,
	    random_member(ChosenServerHost, ServerHosts),
	    establish_tcp_connection(ServerType, ChosenServerHost, ServerPort, ServerStream),
	    conditionally_announce_server_connection(ServerType, ServerMessage,
						     ChosenServerHost, ServerStream),
	!.
get_server_stream_aux(ServerType, ControlValue, _HostsEnvVar, _PortEnvVar, _ServerStream) :-
	( control_value(ControlValue, UserChoice) ->
	  send_message('~nCould not set ~w Server hosts and port for ~q.~nAborting.~n',
		       [ServerType,UserChoice])
	; send_message('~nCould not set ~w Server hosts and port.~nAborting.~n',
		       [ServerType])
	),
	stop_and_halt.

conditionally_announce_server_connection(ServerType, ServerMessage, ServerHost, ServerStream) :-
	( \+ control_option(silent) ->
	  send_message('Established connection ~w to ~w Server on ~w~w.~n',
		       [ServerStream,ServerType,ServerMessage,ServerHost])
	; true
	).


