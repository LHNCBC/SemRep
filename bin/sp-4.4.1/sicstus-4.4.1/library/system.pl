/* -*- Mode: Prolog; -*-
   Copyright(C) 1995-2006, Swedish Institute of Computer Science */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   File   : SYSTEM.PL							     %
%   Authors : Mats Carlsson, Stefan Andersson				     %
%   Updated: 11 May 1996						     %
%   Purpose: Operating system utilities					     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(system, [
	environ/2,
	environ/3, % [PM] 4.1
	now/1,
	datime/1,
	datime/2,
	sleep/1

% Use sockets:current_host/1
%	host_name/1,

% Never did make much sense, use sockets:current_host/1 instead
%	host_id/1,

% [PM] 4.0 now in library(file_systems)
%	make_directory/1,
% [PM] 4.0 now in library(file_systems) as create_directory/[1,2]
%	working_directory/2,

%% [PM] 4.0 instead use open(temp(foo), write, S, [if_exists(generate_unique_name)]) 
%%	mktemp/2,
%%	tmpnam/1,

%% [PM] 4.0 replaced by library(process)
%	system/0,
%	system/1,
%	system/2,
%	shell/0,
%	shell/1,
%	shell/2,
%	exec/3,
%       popen/3,
%	pid/1,
%	kill/2,
%	wait/2,

		  ]).

:- use_module(library(types), [must_be/4]).


%@  This package contains utilities for invoking services from the operating
%@  system that does not fit elsewhere.
%@  
%@ @c   Certain predicates described below take names of files or
%@ @c   directories as arguments.  These must be given as atoms, and
%@ @c   the predicates below will not call @code{absolute_file_name/3} on
%@ @c   them.
%@ @c   
%@ @c   Some predicates are described as invoking the default shell.
%@ @c   Specifically this means invoking @file{/bin/sh} on UNIX platforms. Under
%@ @c   MSDOS, Windows and OS/2, the command interpreter given by the
%@ @c   environment variable @env{COMSPEC} is invoked.
%@  
%@  Exported predicates:
%@  
%@  @table @code

%@  @item now(@var{-When})
%@  @PLXindex {now/1 (system)}
%@  Unifies the current date and time as a UNIX timestamp with
%@  @var{When}.

now(When) :-
        prolog:current_time(When).

%@  @item datime(@var{-Datime})
%@  @PLXindex {datime/[1,2] (system)}
%@  Unifies @var{Datime} with the current date and time as a
%@  @code{datime/6} record of the form
%@  @code{datime(@var{Year},@var{Month},@var{Day},@var{Hour},@var{Min},@var{Sec})}.
%@  All fields are integers.
%@  @item datime(@var{+When},@var{-Datime})
%@  @item datime(@var{-When},@var{+Datime})
%@  Convert a time stamp, as obtained by @code{now/1}, to a
%@  @code{datime/6} record. Can be used in both directions.

datime(Datime) :-
        prolog:datime(Datime).

datime(When, Datime) :- ground(Datime), !,      % [PM] 4.0 prefer mktime since many Datime map to the same When
        prolog:mktime(Datime, When).
datime(When, Datime) :- nonvar(When), !,
        prolog:datime(When, Datime).

%@  @item sleep(@var{+Seconds})
%@  @PLXindex {sleep/1 (system)}
%@  Puts the SICStus Prolog process asleep for @var{Second} seconds, where
%@  @var{Seconds} should be a non-negative number.
%@  @c [PM] 4.0 now interruptible sleep using SPIO
%@  @c Under UNIX, the @code{usleep}
%@  @c function will be used if @var{Seconds} is less than one, and @code{sleep}
%@  @c otherwise.  Under MSDOS, Windows or OS/2, the @code{Sleep} function will be used.

sleep(Seconds) :-
   Goal = sleep(Seconds),
   prolog:sleep(Seconds, Goal).


:- environ(?, ?) is nondet.
%@  @item environ(@var{?Var}, @var{?Value})
%@  @PLXindex {environ/[2,3] (system)}

%@  @var{Var} is the name of a system property or an environment
%@  variable, and @var{Value} is its value.  Both are atoms.  Can be
%@  used to enumerate all current system properties and environment
%@  variables.

%@
%@  The same as @code{environ(@var{Var}, @var{Value}, merged)}.

environ(Var, Value) :-
        %% Currently environ/3 only reports error if Option is wrong which it is not
        environ(Var, Value, merged).


:- environ(?, ?, +) is nondet.
%@  @item environ(@var{?Var}, @var{?Value}, @var{+Source}) @since{release 4.1}
%@  @var{Var} is the name of an environment variable or system property, and @var{Value} is its
%@  value.  Both are atoms.  Can be used to enumerate all current
%@  environment variables and system properties.
%@  
%@  @var{Source} is one of @code{properties}, in which case only system properties are enumerated;
%@  @code{environment}, in which case only environment variables are enumerated; and
%@  @code{merged}, in which case both environment variables and system
%@  properties are enumerated. When @var{Source} is @code{merged} and
%@  an environment variable and a system property have equivalent names,
%@  the value of the system property is returned.
%@  
%@  On UNIX-like platforms, two names are equivalent if and only if they
%@  are identical.  On Windows-like platforms, a case insensitive
%@  comparison is used.
%@  
%@  
%@  @xref{System Properties and Environment Variables}, for
%@  more information.
%@  
environ(Var, Value, Option) :-
        Goal = environ(Var, Value, Option),
        must_be(Option, oneof([merged,properties,environment]), Goal,3),
        prolog:environ(Var, Value, Option).

:- if(false). /* [PM] 4.1 gone forever (setenv is unsafe) */
%--------------------------------------------------------------------------
%   setenv(+Var, +Value)
%   Var is the name of a UNIX environment variable, and Value is its value.
%   Both are atoms.  Sets the value of the environment variable Var in
%   the current process and in child processes spawned after the call
%   to setenv/2.
%   Note: Will leak memory on most platforms due to general losynes of
%   environment variable API.

setenv(Var, Value) :-
	%% Goal = setenv(Var, Value),
	%% must_be(Var, atom, Goal, 1),
	%% must_be(Value, atom, Goal, 2),
   c_setenv(Var, Value, Res),
   Res == 0.                     % FIXME: Error handling?

:- endif.

%% [PM] 4.1.3 SPIDER/xref
:- public set_system_property/2.

%%   [PM] 4.1 not public yet.
%%   set_system_property(+Var, +Value)
%%   Sets the value of the system property Var to Value, both should
%%   be atoms.
set_system_property(Var, Value) :-
	Goal = set_system_property(Var, Value),
	must_be(Var, atom, Goal, 1),
	must_be(Value, atom, Goal, 2),
        prolog:set_system_property(Var, Value).

%-------------------------------------------------------------------------

%@  @end table
